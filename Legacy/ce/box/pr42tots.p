/* ---------------------------------------------- ce/box/pr42tots.p 02/96 JLF */
/* copy of com for 2 sheet boxes                                              */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-rels AS INT NO-UNDO.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

DEF SHARED BUFFER xest FOR est.
DEF SHARED BUFFER xef  FOR ef.
DEF SHARED BUFFER xeb  FOR eb.
DEF SHARED BUFFER xop  FOR est-op.

{ce/print4.i SHARED SHARED}

DEF SHARED VAR qty AS INT NO-UNDO.
DEF SHARED VAR v-do-gsa LIKE do-gsa.

DEF VAR fg-wt$ AS DEC NO-UNDO.
DEF VAR fg-wt% AS DEC NO-UNDO.
DEF VAR op-tot-t LIKE op-tot NO-UNDO.
DEF VAR qm AS DEC NO-UNDO.
DEF VAR ld-fg-rate AS DEC NO-UNDO.
DEF VAR ld-dm-mrkp AS DEC NO-UNDO.
DEF VAR ld-pmrkp AS DEC EXTENT 3 NO-UNDO.

DEF BUFFER reftable-fm FOR reftable.
DEF BUFFER reftable-fold-pct FOR reftable.
def buffer bf-set for eb.

DO TRANSACTION:
  {sys/inc/cerun.i F}
  {sys/inc/cematl.i}
END.

do-gsa = v-do-gsa.

find first ce-ctrl where ce-ctrl.company = cocode and
                         ce-ctrl.loc     = locode no-lock no-error.

output to value(outfile1) append .

fg-wt = 0.
for each car break by car.id:    
      z = tt-blk.
      find first xeb where xeb.company  = xest.company and
                           xeb.est-no   = xest.est-no and
                           xeb.form-no  = car.snum   and
                           xeb.blank-no = car.bnum no-lock no-error.
                           
      find first carrier where carrier.company = cocode and carrier.loc = locode
      and carrier.carrier = car.carrier no-lock no-error.
      if avail carrier then
      find first carr-mtx
          where carr-mtx.company  eq cocode
            and carr-mtx.loc      eq locode
            and carr-mtx.carrier  eq carrier.carrier
            and carr-mtx.del-zone eq car.dscr
          no-lock no-error.
      yyy = 0.  /* use for rate per cwt */

      if xeb.fr-out-c ne 0 then
        yyy = (xeb.fr-out-c * (car.qty / 100)).
      else
      
      if xeb.fr-out-m ne 0 then
        yyy = (xeb.fr-out-m * (z / 1000)).
      
      else
      if avail carr-mtx then do:
        if carrier.chg-method eq "W" then
        do i = 1 to 10:
          yyy = carr-mtx.rate[i] * car.qty / 100.
          if carr-mtx.weight[i] ge car.qty then leave.
        end.
        
        else
        if carrier.chg-method eq "P" then
        do i = 1 to 10:
          yyy = carr-mtx.rate[i] * p-qty.
          if carr-mtx.weight[i] ge p-qty then leave.
        end.
        
        else
        do i = 1 to 10:
          fr-tot = carr-mtx.rate[i] * car.msf.
          if carr-mtx.weight[i] ge car.msf then leave.
        end.
      
        if yyy lt carr-mtx.min-rate then yyy = carr-mtx.min-rate.
      
        yyy = yyy + (carr-mtx.min-rate * (ip-rels - 1)).
      end.
      
      ASSIGN
         car.cost = car.cost + yyy
         fg-wt  = fg-wt + car.qty
         fr-tot = fr-tot + yyy.

      find first blk where blk.id = car.id no-error.

      ASSIGN
         ld-fg-rate = IF blk.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate
         blk.sell = blk.sell + yyy /* use sell for freight costs for now */
         blk.lab  = blk.lab  + ((car.qty / 100) * ld-fg-rate)
         blk.cost = blk.cost + ((car.qty / 100) * ld-fg-rate)
         fg-wt$   = fg-wt$ + (car.qty / 100 * ld-fg-rate).
      
      /* if set is unitized freight is charged only once not per form */
      FIND FIRST bf-set WHERE bf-set.company = xest.company
                       AND bf-set.est-no = xest.est-no
                       AND bf-set.form-no = 0
                       NO-LOCK NO-ERROR.                          
      if avail bf-set /*and bf-set.set-is-assembled*/ and bf-set.pur-man then leave.   
end.

if fg-wt$ gt 0 then put "Finished Goods Handling" fg-wt$ to 80 skip.

op-tot[5] = op-tot[5] + fg-wt$ .

put "TOTAL  OPERATIONS        " op-tot[3] format ">>>>9.99" to 58
    op-tot[4] format ">>>>>9.99" to 68
    op-tot[5] format ">>>>,>>9.99" to 80
    skip(1).

IF cerunf EQ "HOP" THEN DO:
  FOR EACH brd
      WHERE CAN-FIND(FIRST item
                     WHERE item.company EQ xest.company
                       AND item.i-no    EQ brd.i-no
                       AND CAN-DO("B,P,R,1,2,3,4",item.mat-type)):
    ACCUM brd.qty (TOTAL).
    ACCUM brd.qty-mr + brd.qty-wst (TOTAL).
  END.
  PUT "Total Waste Percentage"
      (ACCUM TOTAL brd.qty-mr + brd.qty-wst) / (ACCUM TOTAL brd.qty) * 100
                                  FORMAT ">>,>>9.99" TO 80
      SKIP(1).
END.

/* mat */
   do i = 1 to 6:
      ctrl[9] = ce-ctrl.mat-pct[i] / 100.
      if ce-ctrl.mat-cost[i] gt dm-tot[5]  then leave.
   end.
/* lab */
   do i = 1 to 6:
      ctrl[10] = ce-ctrl.lab-pct[i] / 100.
      if ce-ctrl.lab-cost[i] gt op-tot[5]  then leave.
   end.

   DO TRANSACTION:
     xest.gsa-mat = ctrl[9] * 100.
   END.

   assign gsa-mat = ctrl[9]  * 100     
          gsa-lab = ctrl[10] * 100
          gsa-com = ce-ctrl.comm-mrkup
          gsa-war = ce-ctrl.whse-mrkup.

   FIND FIRST reftable-fm NO-LOCK
       WHERE reftable-fm.reftable EQ "gsa-fm"
         AND reftable-fm.company  EQ xest.company
         AND reftable-fm.loc      EQ ""
         AND reftable-fm.code     EQ xest.est-no
       NO-ERROR.

  IF AVAIL reftable-fm THEN
     gsa-fm = reftable-fm.val[1].
  ELSE
  DO:
        gsa-fm = ce-ctrl.fold-pct.
  END.

   output close.
   run ce/gsa.p (ip-rowid, qtty[vmcl], rels[vmcl]).
   session:set-wait-state("general").
   output to value(outfile1) append .
   ASSIGN
      ctrl[9]  = gsa-mat / 100
      ctrl[10] = gsa-lab / 100
      ctrl[1]  = gsa-war / 100
      ctrl[19] = gsa-fm / 100.

   find first xeb
       where xeb.company = xest.company
         AND xeb.est-no    eq xest.est-no
         and xeb.form-no ne 0
       no-error.

   qm = tt-blk / 1000.
       
   IF cerunf EQ "Dee" THEN DO:
     FOR EACH est-prep NO-LOCK
         WHERE est-prep.company EQ xest.company
           AND est-prep.est-no  EQ xest.est-no
           AND est-prep.code    NE "":

       IF est-prep.ml THEN
         ld-pmrkp[1] = ld-pmrkp[1] +
                       ((est-prep.cost * est-prep.qty) *
                        (IF est-prep.amtz NE 0 THEN est-prep.amtz / 100 ELSE 1)).
       ELSE
         ld-pmrkp[2] = ld-pmrkp[2] +
                       ((est-prep.cost * est-prep.qty) *
                        (IF est-prep.amtz NE 0 THEN est-prep.amtz / 100 ELSE 1)).
     END.

     ASSIGN
      ld-pmrkp[1] = tprep-mat - ld-pmrkp[1]
      ld-pmrkp[2] = tprep-lab - ld-pmrkp[2]
      ld-pmrkp[3] = ld-pmrkp[1] + ld-pmrkp[2]
      tprep-mat   = tprep-mat - ld-pmrkp[1]
      tprep-lab   = tprep-lab - ld-pmrkp[2].
   END.

   fac-tot = dm-tot[5] + op-tot[5] +
             tprep-mat + tprep-lab + mis-tot[1] + mis-tot[3].

   DO TRANSACTION:
   IF xest.gsa-mat EQ 0 THEN xest.costBoard = 0.
   IF cematl-log THEN ld-dm-mrkp = dm-tot[5] * cematl-dec / 100.

   ASSIGN
      xxx = dm-tot[5] - xest.costBoard + tprep-mat + mis-tot[1]
      ctrl2[9] = xxx * ctrl[9]
      xxx = op-tot[5] + tprep-lab + mis-tot[3]
      ctrl2[10] = xxx * ctrl[10]
      xest.costBoard = xest.costBoard * xest.gsa-mat / 100.

   END.

   ASSIGN
     ctrl2[1] = (fac-tot + xest.costBoard + ctrl2[9] + ctrl2[10] + ld-dm-mrkp) *
                ctrl[1]
     ctrl2[13] = (fac-tot + xest.costBoard + ctrl2[9] + ctrl2[10] + ld-dm-mrkp) *
                ctrl[19]
     tt-tot = dm-tot[5] + op-tot[5] + ctrl2[1] + ctrl2[13] +
              tprep-mat + tprep-lab + mis-tot[1] + mis-tot[3] +
              xest.costBoard + ctrl2[9] + ctrl2[10] + ld-dm-mrkp.

   if xeb.chg-method eq "P" and ctrl[6] ne 0 then fac-tot = fac-tot + fr-tot.

   ASSIGN
      ctrl2[4] = 0
      ctrl2[5] = 0.

   if ctrl[4] gt 0 then 
      ctrl2[4] = (fac-tot + xest.costBoard + ctrl2[9] + ctrl2[10] + ld-dm-mrkp) * 
                ctrl[4].
   tt-tot = tt-tot + ctrl2[4].

   if ctrl[11] gt 0 then do:             /* set spec#2 */
      if ctrl[11] le 1
      then ctrl2[11] = fac-tot * ctrl[11].
      else ctrl2[11] = ctrl[11].
   end.
   if ctrl[12] gt 0 then do:             /* set spec#3 */
      if ctrl[12] le 1
      then ctrl2[12] = fac-tot * ctrl[12].
      else ctrl2[12] = ctrl[12].
   end.

   for each blk:
      ASSIGN
        blk.cost = blk.cost + blk.sell
        blk.sell = 0.
   end.

   for each blk:
      find first xeb where xeb.company  = xest.company and
                           xeb.est-no   = xest.est-no and
                           xeb.form-no  = blk.snum   and
                           xeb.blank-no = blk.bnum no-lock no-error.

      ASSIGN
         blk.fact = blk.cost
         xxx = blk.sell /* xxx = 0 if freight already included! */
         /* add material gsa amount to blk.lab */
         blk.sell = ((blk.cost - blk.lab) * ctrl[9])
         /* set blk.lab to labor gsa amount */
         blk.lab  = blk.lab * ctrl[10]
         /* add gsa's to blk.cost */
         blk.cost = blk.cost + blk.lab + blk.sell
         yyy = blk.cost. /* yyy = total cost of blk */

      if ctrl[1] ne 0 then  /* warehousing % */
         blk.cost = blk.cost + (yyy * ctrl[1]).

      blk.cost = blk.cost + xxx. /* add freight if not already done */
      if ctrl[4] ne 0 then  /* special markup % */
         blk.cost = blk.cost + (yyy * ctrl[4]).
   end.

   find first xeb NO-LOCK
       where xeb.company = xest.company
         AND xeb.est-no    eq xest.est-no
         and xeb.form-no ne 0
       no-error.

   if not vmclean then do:
      put
   "*** T O T A L S                           Cost/M     MR $      Run $  Total Cost" skip
      "Direct Material"  dm-tot[5] / qm to 50
                         dm-tot[3] format ">>>>9.99" to 58
                         dm-tot[5] to 80 skip.

      IF cerunf NE "Dee" THEN
        PUT "Direct Labor" op-tot[5] / qm to 50
                           op-tot[3] FORMAT ">>>>9.99" to 58
                           op-tot[4] FORMAT ">>>>>9.99" to 68
                           op-tot[5] TO 80 SKIP.

      IF tprep-mat NE 0 THEN DO: PUT
      "Prep.  Material" tprep-mat / qm to 50 tprep-mat TO 80  SKIP. 
      lin-count = lin-count + 1. END.
      IF tprep-lab NE 0 THEN DO: PUT
      "Prep.  Labor   " tprep-lab / qm to 50 tprep-lab TO 80  SKIP.
      lin-count = lin-count + 1. END.
      IF mis-tot[1] NE 0 THEN DO: PUT
      "Misc.  Material" mis-tot[2] to 50 mis-tot[1] TO 80 SKIP.
      lin-count = lin-count + 1. END.
      IF mis-tot[3] NE 0 THEN DO: PUT
      "Misc.  Labor   " mis-tot[4] to 50 mis-tot[3] TO 80 SKIP.
      lin-count = lin-count + 6. END.

      IF cerunf EQ "Dee" THEN DO:
        ASSIGN
         op-tot-t[1] = op-tot[1]
         op-tot-t[2] = op-tot[2]
         op-tot-t[3] = op-tot[3]
         op-tot-t[4] = op-tot[4]
         op-tot-t[5] = op-tot[5]

         op-tot[5]   = op-tot[5] - op-tot[6] - op-tot[7]
         op-tot[3]   = ROUND(op-tot[5] * (op-tot-t[3] / op-tot-t[5]),2)
         op-tot[4]   = ROUND(op-tot[5] * (op-tot-t[4] / op-tot-t[5]),2)
         fac-tot     = fac-tot   - op-tot[7].

        IF op-tot[3] EQ ? THEN op-tot[3] = 0.
        IF op-tot[4] EQ ? THEN op-tot[4] = 0.

        PUT "Direct Labor"
            op-tot[5] / qm                 to 50
            op-tot[3] FORMAT ">>>>9.99"    to 58
            op-tot[4] FORMAT ">>>>>>9.99"  to 68
            op-tot[5] FORMAT ">>>>,>>9.99" TO 80 SKIP.

        ASSIGN
         op-tot[3]   = ROUND(op-tot[6] * (op-tot-t[3] / op-tot-t[5]),2)
         op-tot[4]   = ROUND(op-tot[6] * (op-tot-t[4] / op-tot-t[5]),2).

        IF op-tot[3] EQ ? THEN op-tot[3] = 0.
        IF op-tot[4] EQ ? THEN op-tot[4] = 0.

        PUT "Variable Overhead"
            op-tot[6] / qm                 to 50
            op-tot[3] FORMAT ">>>>9.99"    to 58
            op-tot[4] FORMAT ">>>>>>9.99"  to 68
            op-tot[6] FORMAT ">>>>,>>9.99" TO 80 skip.
      END.

      if xeb.chg-method eq "P" and ctrl[6] ne 0 and fr-tot ne 0 then
         put "Freight"       fr-tot / qm to 50 fr-tot to 80 skip.

      PUT "DIRECT FACTORY COST" fac-tot / qm to 50 fac-tot to 80 skip.

      fac-tot2 = fac-tot.

      IF cerunf EQ "Dee" THEN DO:
        ASSIGN
         op-tot[3] = ROUND(op-tot[7] * (op-tot-t[3] / op-tot-t[5]),2)
         op-tot[4] = ROUND(op-tot[7] * (op-tot-t[4] / op-tot-t[5]),2).

        IF op-tot[3] EQ ? THEN op-tot[3] = 0.
        IF op-tot[4] EQ ? THEN op-tot[4] = 0.

        put "Fixed Overhead"
            op-tot[7] / qm                 to 50
            op-tot[3] FORMAT ">>>>9.99"    to 58
            op-tot[4] FORMAT ">>>>>>9.99"  to 68
            op-tot[7] FORMAT ">>>>,>>9.99" TO 80 SKIP.

        fac-tot2 = fac-tot2 + op-tot[7].

        PUT "Prep Markup"
            ld-pmrkp[3] / qm                 TO 50
            ld-pmrkp[3] FORMAT ">>>>,>>9.99" TO 80 SKIP.

        fac-tot2 = fac-tot2 + ld-pmrkp[3].
      END.

      if (ctrl[13] = 1 OR cerunf EQ "Dee") and ctrl[4] ne 0 then do:
         fac-tot2 = fac-tot2 + ctrl2[4].
         if ctrl[4] gt 0 then put
         ce-ctrl.spec-l[1] space(1).
         if ctrl[4] le 1 then
         put string(ce-ctrl.spec-%[1] * 100,">>9.99") + "%" to 30.
         put ctrl2[4] / qm to 50 ctrl2[4] to 80 skip.
      end.
      if (ctrl[14] = 1 OR cerunf EQ "Dee") and ctrl[11] ne 0 then do:
         fac-tot2 = fac-tot2 + ctrl2[11].
         if ctrl[11] gt 0 then put
         ce-ctrl.spec-l[2] space(1).
         if ctrl[11] le 1 then
         put string(ce-ctrl.spec-%[2] * 100,">>9.99") + "%" to 30.
         put ctrl2[11] / qm to 50 ctrl2[11] to 80 skip.
      end.
      if (ctrl[15] = 1 OR cerunf EQ "Dee") and ctrl[12] ne 0 then do:
         fac-tot2 = fac-tot2 + ctrl2[12].
         if ctrl[12] gt 0  then put ce-ctrl.spec-l[3] space(1).
         if ctrl[12] le 1 then
         put string(ce-ctrl.spec-%[3] * 100,">>9.99") + "%" to 30.
         put ctrl2[12] / qm to 50 ctrl2[12] to 80 skip.
      end.

      IF ctrl[16] NE 0 OR cerunf EQ "Dee" THEN DO:
        IF xest.costBoard NE 0 THEN
          PUT "GS&A Board"
              STRING(xest.gsa-mat,">>9.99") + "%" TO 30
              xest.costBoard / qm                   TO 50
              xest.costBoard                        TO 80 SKIP.

        IF ctrl2[9] NE 0 THEN
          PUT "GS&A Material"
              STRING(ctrl[9] * 100,">>9.99")  + "%"  TO 30
              ctrl2[9] / qm                          TO 50
              ctrl2[9]                               TO 80 SKIP.

        IF ctrl2[10] NE 0 THEN
          PUT "GS&A Labor"
              STRING(ctrl[10] * 100,">>9.99") + "%"  TO 30
              ctrl2[10] / qm                         TO 50
              ctrl2[10]                              TO 80 SKIP.

        IF ld-dm-mrkp NE 0 THEN
          PUT "Direct Material Markup"
              STRING(cematl-dec,">>9.99") + "%"      TO 30
              ld-dm-mrkp / qm                        TO 50
              ld-dm-mrkp                             TO 80 SKIP.

        fac-tot2 = fac-tot2 + xest.costBoard + ctrl2[9] + ctrl2[10] + ld-dm-mrkp.
      END.
     
      if cerunf NE "Dee" AND ctrl[18] gt 0 and ctrl2[18] ne 0 then do:   /* Royalty */
         if ctrl2[18] le 1 then do:
            put "Royalty" string(ctrl2[18] * 100,">>9.99") + "%" to 30
                                     (ctrl2[18] * fac-tot) to 80 skip.
            fac-tot2 = fac-tot2 + (fac-tot * ctrl2[18]).
         end.
         else do:
            put "Royalty" ctrl2[18] / qm to 50 ctrl2[18] to 80 skip.
            fac-tot2 = fac-tot2 + ctrl2[18].
         end.
      end.
          
      IF cerunf EQ "Dee" THEN
        PUT "TOTAL CONTRIBUTION"        FORMAT "x(19)"
            (fac-tot2 - fac-tot) / qm   TO 50
            (fac-tot2 - fac-tot)        TO 80 SKIP.
      ELSE
        PUT "TOTAL FACTORY COST"        FORMAT "x(19)"
            fac-tot2 / qm               TO 50
            fac-tot2                    TO 80 SKIP.

      ASSIGN
       ord-cost = fac-tot2
       tt-tot   = fac-tot2.

      IF cerunf NE "Dee" THEN DO:
        if ctrl2[1] ne 0 THEN DO:
           put "Warehousing" string(ctrl[1] * 100)  + "%" to 30
               ctrl2[1] / qm to 50
               ctrl2[1]      to 80 skip.

           tt-tot = tt-tot + ctrl2[1].
        END.

        IF ctrl2[13] NE 0 THEN DO:
        
           PUT "Folding" string(ctrl[19] * 100)  + "%" to 30
               ctrl2[13] / qm to 50
               ctrl2[13]      to 80 skip.

           tt-tot = tt-tot + ctrl2[13].
        END.

        if xeb.chg-method eq "P" and ctrl[6] eq 0 and fr-tot ne 0 then do:
           put "Freight"       fr-tot / qm to 50 fr-tot to 80 skip.
           tt-tot = tt-tot + fr-tot.
        end.

        if ctrl[13] = 0 and ctrl[4] ne 0 then do:
           tt-tot = tt-tot + ctrl2[4].
           if ctrl[4] gt 0 then put
           ce-ctrl.spec-l[1] space(1).
           if ctrl[4] le 1 then
           put string(ce-ctrl.spec-%[1] * 100,">>9.99") + "%" to 30.
           put ctrl2[4] / qm to 50 ctrl2[4] to 80 skip.
        end.
        if ctrl[14] = 0 and ctrl[11] ne 0 then do:
           tt-tot = tt-tot + ctrl2[11].
           if ctrl[11] gt 0 then put
           ce-ctrl.spec-l[2] space(1).
           if ctrl[11] le 1 then
           put string(ce-ctrl.spec-%[2] * 100,">>9.99") + "%" to 30.
           put ctrl2[11] / qm to 50 ctrl2[11] to 80 skip.
        end.
        if ctrl[15] = 0 and ctrl[12] ne 0 then do:
           tt-tot = tt-tot + ctrl2[12].
           if ctrl[12] gt 0  then put ce-ctrl.spec-l[3] space(1).
           if ctrl[12] le 1 then
           put string(ce-ctrl.spec-%[3] * 100,">>9.99") + "%" to 30.
           put ctrl2[12] / qm to 50 ctrl2[12] to 80 skip.
        end.

        IF ctrl[16] EQ 0 THEN DO:
           IF xest.costBoard NE 0 THEN
            PUT "GS&A Board"
                STRING(xest.gsa-mat,">>9.99") + "%" TO 30
                xest.costBoard / qm                   TO 50
                xest.costBoard                        TO 80 SKIP.
 
          IF ctrl2[9] NE 0 THEN
            PUT "GS&A Material"
                STRING(ctrl[9] * 100,">>9.99")  + "%"  TO 30
                ctrl2[9] / qm                          TO 50
                ctrl2[9]                               TO 80 SKIP.

          IF ctrl2[10] NE 0 THEN
            PUT "GS&A Labor"
                STRING(ctrl[10] * 100,">>9.99") + "%"  TO 30
                ctrl2[10] / qm                         TO 50
                ctrl2[10]                              TO 80 SKIP.
 
          IF ld-dm-mrkp NE 0 THEN
            PUT "Direct Material Markup"
                STRING(cematl-dec,">>9.99") + "%"      TO 30
                ld-dm-mrkp / qm                        TO 50
                ld-dm-mrkp                             TO 80 SKIP.

          tt-tot = tt-tot + xest.costBoard + ctrl2[9] + ctrl2[10] + ld-dm-mrkp. 
        END.
          
        if ctrl[18] eq 0 and ctrl2[18] ne 0 then do:   /* Royalty */
           if ctrl2[18] le 1 then do:
              put "Royalty" string(ctrl2[18] * 100,">>9.99") + "%" to 30
                  (ctrl2[18] * fac-tot2) / qm to 50
                  (ctrl2[18] * fac-tot2)   to 80 skip.
              tt-tot = tt-tot + (fac-tot2 * ctrl2[18]).
           end.
           else do:
              put "Royalty" ctrl2[18] / qm to 50 ctrl2[18] to 80 skip.
              tt-tot = tt-tot + ctrl2[18].
           end.
        end.
      END.
   end.

   else DO TRANSACTION:
     assign
      vmcl-desc = "Prep.  Material"
      vmcl-cost = tprep-mat / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     assign
      vmcl-desc = "Prep.  Labor"
      vmcl-cost = tprep-lab / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     assign
      vmcl-desc = "Misc.  Material"
      vmcl-cost = mis-tot[2].
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     assign
      vmcl-desc = "Misc.  Labor"
      vmcl-cost = mis-tot[4].
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     op-tot[5] = op-tot[5] - op-tot[6] - op-tot[7].
     fac-tot   = fac-tot   - op-tot[7].

     assign
      vmcl-desc = "Direct Material"
      vmcl-cost = dm-tot[5] / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     assign
      vmcl-desc = "Direct Labor"
      vmcl-cost = op-tot[5] / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     assign
      vmcl-desc = "Variable Overhead"
      vmcl-cost = op-tot[6] / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     if xeb.chg-method eq "P" and ctrl[6] ne 0 and fr-tot ne 0 then do:
       assign
        vmcl-desc = "Freight"
        vmcl-cost = fr-tot / qm.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     assign
      vmcl-desc = "DIRECT FACTORY COST"
      vmcl-cost = fac-tot / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     fac-tot2 = fac-tot.

     assign
      vmcl-desc = "Fixed Overhead"
      vmcl-cost = op-tot[7] / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     fac-tot2 = fac-tot2 + op-tot[7].
     if ctrl[13] ne 0 then fac-tot2 = fac-tot2 + ctrl2[4].
     if ctrl[14] ne 0 then fac-tot2 = fac-tot2 + ctrl2[11].
     if ctrl[15] ne 0 then fac-tot2 = fac-tot2 + ctrl2[12].
     if ctrl[18] ne 0 then fac-tot2 = fac-tot2 + ctrl2[18].

     if ctrl[13] = 1 then do:
       assign
        vmcl-desc = ce-ctrl.spec-l[1]
        vmcl-cost = ctrl2[4] / qm.

       if ctrl[4] le 1 then
         vmcl-desc = vmcl-desc + " - " +
                     trim(string(ce-ctrl.spec-%[1] * 100,">>9.99%")).

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     if ctrl[14] = 1 then do:
       assign
        vmcl-desc = ce-ctrl.spec-l[2]
        vmcl-cost = ctrl2[11] / qm.

       if ctrl[11] le 1 then
         vmcl-desc = vmcl-desc + " - " +
                     trim(string(ce-ctrl.spec-%[2] * 100,">>9.99%")).

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     if ctrl[15] = 1 then do:
       assign
        vmcl-desc = ce-ctrl.spec-l[3]
        vmcl-cost = ctrl2[12] / qm.

       if ctrl[12] le 1 then
         vmcl-desc = vmcl-desc + " - " +
                     trim(string(ce-ctrl.spec-%[3] * 100,">>9.99%")).

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     IF ctrl[16] NE 0 THEN DO:
       ASSIGN
        vmcl-desc = "GS&A Board"
        vmcl-cost = xest.costBoard / qm
        fac-tot2  = fac-tot2 + xest.costBoard.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsabrd".

       ASSIGN
        vmcl-desc = "    GS&A Board %"
        vmcl-cost = xest.gsa-mat.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsabrd".

       ASSIGN
        vmcl-desc = "GS&A Material"
        vmcl-cost = ctrl2[9] / qm
        fac-tot2  = fac-tot2 + ctrl2[9].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsamat".

       ASSIGN
        vmcl-desc = "    GS&A Material %"
        vmcl-cost = ctrl[9] * 100.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsamat".

       ASSIGN
        vmcl-desc = "GS&A Labor"
        vmcl-cost = ctrl2[10] / qm
        fac-tot2  = fac-tot2 + ctrl2[10].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsalab".

       ASSIGN
        vmcl-desc = "    GS&A Labor %"
        vmcl-cost = ctrl[10] * 100.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsalab".

       IF ld-dm-mrkp NE 0 THEN DO:
         ASSIGN
          vmcl-desc = "Direct Material Markup"
          vmcl-cost = ld-dm-mrkp / qm
          fac-tot   = fac-tot + ld-dm-mrkp.

         {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
         mclean.rec-type = "dm-mrkp".

         ASSIGN
          vmcl-desc = "    Direct Material Markup %"
          vmcl-cost = cematl-dec.

         {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
         mclean.rec-type = "dm-mrkp".
       END.
     END.
     
     if ctrl[18] gt 0 then do:   /* Royalty */
       vmcl-desc = "Royalty".

       if ctrl[18] le 1 then
         assign
          vmcl-desc = vmcl-desc + " - " +
                      trim(string(ctrl2[18] * 100,">>9.99%"))
          vmcl-cost = (ctrl2[18] * fac-tot).

       else vmcl-cost = ctrl2[18].

       assign
        fac-tot2  = fac-tot2 + vmcl-cost
        vmcl-cost = vmcl-cost / qm.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     assign
      vmcl-desc = "TOTAL FACTORY COST"
      vmcl-cost = fac-tot2 / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     assign
      ord-cost = fac-tot2
      tt-tot   = fac-tot2.

     assign
      vmcl-desc = "Warehousing"
      vmcl-cost = ctrl2[1] / qm
      tt-tot    = tt-tot + ctrl2[1].

     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     mclean.rec-type = "warehousing".
       
     assign
      vmcl-desc = "    Warehousing %"
      vmcl-cost = ctrl[1] * 100.

     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     mclean.rec-type = "warehousing".

     assign
      vmcl-desc = "Folding"
      vmcl-cost = ctrl2[13] / qm
      tt-tot    = tt-tot + ctrl2[13].

     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     mclean.rec-type = "folding".
       
     assign
      vmcl-desc = "    Folding %"
      vmcl-cost = ctrl[19] * 100.

     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     mclean.rec-type = "folding".
     
     if xeb.chg-method eq "P" and ctrl[6] eq 0 and fr-tot ne 0 then do:
       assign
        vmcl-desc = "Freight"
        vmcl-cost = fr-tot / qm
        tt-tot    = tt-tot + fr-tot.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     if ctrl[13] = 0 then do:
       vmcl-desc = ce-ctrl.spec-l[1].

       if ctrl[4] le 1 then
         assign
          vmcl-desc = vmcl-desc + " - " +
                      trim(string(ce-ctrl.spec-%[1] * 100,">>9.99%"))
          ctrl2[4]  = fac-tot2 * ctrl[4].
       else ctrl2[4] = ctrl[4].

       assign
        vmcl-cost = ctrl2[4] / qm
        tt-tot    = tt-tot + ctrl2[4].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     if ctrl[14] = 0 then do:             /* set spec#2 */
       vmcl-desc = ce-ctrl.spec-l[2].

       if ctrl[11] le 1 then
         assign
          vmcl-desc = vmcl-desc + " - " +
                      trim(string(ce-ctrl.spec-%[2] * 100,">>9.99%"))
          ctrl2[11]  = fac-tot2 * ctrl[11].
       else ctrl2[11] = ctrl[11].

       assign
        vmcl-cost = ctrl2[11] / qm
        tt-tot    = tt-tot + ctrl2[11].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     if ctrl[15] = 0 then do:             /* set spec#3 */
       vmcl-desc = ce-ctrl.spec-l[3].

       if ctrl[12] le 1 then
         assign
          vmcl-desc = vmcl-desc + " - " +
                      trim(string(ce-ctrl.spec-%[3] * 100,">>9.99%"))
          ctrl2[12]  = fac-tot2 * ctrl[12].
       else ctrl2[12] = ctrl[12].

       assign
        vmcl-cost = ctrl2[12] / qm
        tt-tot    = tt-tot + ctrl2[12].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     IF ctrl[16] EQ 0 THEN DO:
       ASSIGN
        vmcl-desc = "GS&A Board"
        vmcl-cost = xest.costBoard / qm
        tt-tot    = tt-tot + xest.costBoard.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsabrd".

       ASSIGN
        vmcl-desc = "    GS&A Board %"
        vmcl-cost = xest.gsa-mat.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsabrd".

       ASSIGN
        vmcl-desc = "GS&A Material"
        vmcl-cost = ctrl2[9] / qm
        tt-tot    = tt-tot + ctrl2[9].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsamat".

       ASSIGN
        vmcl-desc = "    GS&A Material %"
        vmcl-cost = ctrl[9] * 100.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsamat".

       ASSIGN
        vmcl-desc = "GS&A Labor"
        vmcl-cost = ctrl2[10] / qm
        tt-tot    = tt-tot + ctrl2[10].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsalab".

       ASSIGN
        vmcl-desc = "    GS&A Labor %"
        vmcl-cost = ctrl[10] * 100.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsalab".

       IF ld-dm-mrkp NE 0 THEN DO:
         ASSIGN
          vmcl-desc = "Direct Material Markup"
          vmcl-cost = ld-dm-mrkp / qm
          tt-tot    = tt-tot + ld-dm-mrkp.

         {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
         mclean.rec-type = "dm-mrkp".

         ASSIGN
          vmcl-desc = "    Direct Material Markup %"
          vmcl-cost = cematl-dec.

         {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
         mclean.rec-type = "dm-mrkp".
       END.
     END.

     if ctrl[18] = 0 then do:   /* Royalty */
       vmcl-desc = "Royalty".

       if ctrl[18] le 1 then
         assign
          vmcl-desc = vmcl-desc + " - " +
                      trim(string(ctrl2[18] * 100,">>9.99%"))
          vmcl-cost = (ctrl2[18] * fac-tot).

       else vmcl-cost = ctrl2[18].

       assign
        tt-tot    = tt-tot + vmcl-cost
        vmcl-cost = vmcl-cost / qm.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     assign
      vmcl-desc = "FULL COST"
      vmcl-cost = tt-tot / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
   end.

   IF cerunf EQ "Dee" THEN
     ASSIGN
      tprep-mat = tprep-mat + ld-pmrkp[1]
      tprep-lab = tprep-lab + ld-pmrkp[2].

/* end ---------------------------------- copr. 1996  advanced software, inc. */
