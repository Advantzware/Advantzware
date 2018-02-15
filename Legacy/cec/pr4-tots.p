/* -------------------------------------------------- cec/pr4-tots.p 1/94 cd  */
/*
{sys/inc/var.i shared}
{sys/form/s-top.f}
*/
def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def var xxx as dec no-undo.
def shared var qty as INT NO-UNDO .
def var j as int no-undo.
def var i as int no-undo.
def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

{cec/print4.i shared shared}

def shared var v-form-no like ef.form-no no-undo.

def var t-cov as de  NO-UNDO.
def var qm as de  NO-UNDO.
def var fr-blk as de NO-UNDO.
def var v-msf as dec NO-UNDO.
def var v-pct as dec NO-UNDO.
DEF VAR v-cewhspct AS LOG NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
DEF VAR ll-gsa-pct AS LOG NO-UNDO.


{cec/msfcalc.i}

{cec/rollfac.i}

DO TRANSACTION:
  {sys/inc/cewhschg.i}
  v-cewhspct = NOT cewhschg-cha BEGINS "$".
END.

ll-gsa-pct = CAN-FIND(FIRST sys-ctrl
                      WHERE sys-ctrl.company EQ cocode
                        AND sys-ctrl.name    EQ "CEGSA"
                        AND sys-ctrl.int-fld EQ 0).

FIND FIRST xeb
    WHERE xeb.company EQ xest.company
      AND xeb.est-no  EQ xest.est-no
      AND xeb.style   GT ""
    NO-LOCK NO-ERROR.

   output to value(outfile1) append.

   qm =  qty / 1000 * v-sqft-fac.

   find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.
   
   find first style where style.company = cocode and style.style = xeb.style
   no-lock no-error.

   {est/calcpcts.i xest}
   IF calcpcts.val[1] EQ 0 THEN calcpcts.val[2] = 0.

   assign
   xxx       = dm-tot[5] - calcpcts.val[2] + tprep-mat + mis-tot[1]
   ctrl2[9]  = xxx * ctrl[9]
   xxx       = op-tot[5] + tprep-lab + mis-tot[3]
   ctrl2[10] = xxx * ctrl[10]
   fac-tot   = dm-tot[5] + op-tot[5] +
               tprep-mat + tprep-lab + mis-tot[1] + mis-tot[3].

   calcpcts.val[2] = calcpcts.val[2] * calcpcts.val[1] / 100.
   FIND CURRENT calcpcts NO-LOCK NO-ERROR.

   IF v-cewhspct THEN
     ctrl2[1] = (fac-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10]) * ctrl[1].

   assign
   ctrl2[13] = (fac-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10]) * ctrl[19]

   tt-tot    = dm-tot[5] + op-tot[5] + ctrl2[1] + ctrl2[19] +
               tprep-mat + tprep-lab + mis-tot[1] + mis-tot[3] +
               calcpcts.val[2] + ctrl2[9] + ctrl2[10]
   ctrl2[4]  = 0
   ctrl2[5]  = 0
   ctrl2[11] = 0
   ctrl2[12] = 0.

   if xeb.chg-method eq "P" and ctrl[6] ne 0 then fac-tot = fac-tot + fr-tot.

   /* SET PRINTED TOTALS for Misc. Charges */
   /* spec #1 */
   if ctrl[4]  > 0 and ctrl[4] <= 1
      then ctrl2[4]  = fac-tot * ctrl[4].
      else if ctrl[4] > 1 then ctrl2[4] = ctrl[4].
   /* spec #2 */
   if ctrl[11] > 0 then do:
      if ctrl[11] <= 1
      then ctrl2[11] = fac-tot * ctrl[11].
      else ctrl2[11] = ctrl[11].
   end.
   /* spec #3 */
   if ctrl[12] > 0 then do:
      if ctrl[12] <= 1
      then ctrl2[12] = fac-tot * ctrl[12].
      else ctrl2[12] = ctrl[12].
   end.
   
   /* Royalties or Style Markup */
   if style.royalty ne 0                        and
      (ctrl[18] eq 1 or ce-ctrl.sell-by eq "B") then do:
      
      if style.royalty le 1 then ctrl2[18] = fac-tot * ctrl[18].
                            else ctrl2[18] = style.royalty.
   end.
   
   if ce-ctrl.sell-by eq "B" then do:
     find first est-op
         where est-op.company = xest.company 
           and est-op.est-no eq xest.est-no
           and est-op.qty eq v-op-qty
           and est-op.s-num eq xef.form-no
           and est-op.line  ge 500
         no-lock no-error.

     RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

     v-msf = if avail est-op then est-op.num-sh
             else qty / (xeb.num-up * v-n-out).
             
     {sys/inc/roundup.i v-msf}
     
     assign
      v-msf = if v-corr then
                round(((xef.gsh-len * xef.gsh-wid) * .007) * v-msf,0)
              else
                round(((xef.gsh-len * xef.gsh-wid) / 144) * v-msf,0)
      v-msf = v-msf / 1000.
               
     {cec/sqftmrkp.i v-msf v-pct}
   end.

   if not vmclean then do:
      assign
       v-fac-hdr = "Cost/M" + (if v-rollfac then "SF" else "")
       v-fac-hdr = fill(" ",8 - length(trim(v-fac-hdr))) + trim(v-fac-hdr).

      put
      "*** T O T A L S" space(25) v-fac-hdr format "x(8)" space(5)
      "SU $      Run $  Total Cost" skip

      "Direct Material" dm-tot[5] / qm                 to 48
                        dm-tot[3] format ">>>>9.99"    to 57
                        dm-tot[5] format ">>>>,>>9.99" to 80 skip.
      put
      "Direct Labor" op-tot[5] / qm       to 48
                     op-tot[3] format ">>>>9.99"    to 57
                     op-tot[4] format ">>>>>>9.99"  to 68
                     op-tot[5] format ">>>>,>>9.99" to 80 skip.

      if tprep-mat ne 0 then put
         "Prep.  Material" tprep-mat / qm to 48 tprep-mat to 80  skip.
      if tprep-lab ne 0 then put
         "Prep.  Labor   " tprep-lab / qm to 48 tprep-lab to 80  skip.
      if mis-tot[1] ne 0 then put
      "Misc.  Material" mis-tot[2] to 48 mis-tot[1] to 80 skip.
      if mis-tot[3] ne 0 then put
      "Misc.  Labor   " mis-tot[4] to 48 mis-tot[3] to 80 skip.

      if xeb.chg-method eq "P" and ctrl[6] ne 0 and fr-tot ne 0 then
        put "Freight"  fr-tot / qm to 48 fr-tot to 80 skip.

      put "DIRECT FACTORY COST" fac-tot / qm to 48
                          fac-tot format ">>>>,>>9.99" to 80 skip.

      if ctrl[13] ne 0 then fac-tot = fac-tot + ctrl2[4].
      if ctrl[14] ne 0 then fac-tot = fac-tot + ctrl2[11].
      if ctrl[15] ne 0 then fac-tot = fac-tot + ctrl2[12].
/*      if ctrl[18] ne 0 and ce-ctrl.sell-by ne "B" then    26340 - double counted since done below */
/*                            fac-tot = fac-tot + ctrl2[18].*/

      if ctrl[13] = 1 and ctrl[4] ne 0 then do:
         if ctrl[4] > 0 then put ce-ctrl.spec-l[1].
         if ctrl[4] <= 1 then
         put string(ce-ctrl.spec-%[1] * 100,"->>9.99") + "%" to 30.
         put ctrl2[4] / qm to 48 ctrl2[4] to 80 skip.
      end.
      if ctrl[14] = 1 and ctrl[11] ne 0 then do:
         if ctrl[11] > 0 then put ce-ctrl.spec-l[2] space(1).
         if ctrl[11] <= 1 then
         put string(ce-ctrl.spec-%[2] * 100,"->>9.99") + "%" to 30.
         put ctrl2[11] / qm to 48 ctrl2[11] to 80 skip.
      end.
      if ctrl[15] = 1 and ctrl[12] ne 0 then do:
         if ctrl[12] > 0  then put ce-ctrl.spec-l[3] space(1).
         if ctrl[12] <= 1 then
         put string(ce-ctrl.spec-%[3] * 100,"->>9.99") + "%" to 30.
         put ctrl2[12] / qm to 48 ctrl2[12] to 80 skip.
      end.

      IF ctrl[16] NE 0 THEN DO:
        IF calcpcts.val[2] NE 0 THEN DO:
          PUT "GS&A Board".
          IF ll-gsa-pct THEN
            PUT STRING(calcpcts.val[1],"->>9.99") + "%" TO 30.
          PUT calcpcts.val[2] / qm TO 48 calcpcts.val[2] TO 80 SKIP.
        END.

        IF ctrl2[9] NE 0 THEN DO:
          PUT "GS&A Material".          
          IF ll-gsa-pct THEN
            PUT STRING(ctrl[9] * 100,"->>9.99") + "%" TO 30.
          PUT ctrl2[9] / qm TO 48 ctrl2[9] TO 80 SKIP.
        END.

        IF ctrl2[10] ne 0 THEN DO:
          PUT "GS&A Labor".
          IF ll-gsa-pct THEN
            PUT STRING(ctrl[10] * 100,"->>9.99") + "%" TO 30.
          PUT ctrl2[10] / qm TO 48 ctrl2[10] TO 80 SKIP.
        END.

        fac-tot = fac-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10].
      END.

      if ctrl[18] gt 0 and ctrl2[18] ne 0 and
         ce-ctrl.sell-by ne "B"           then do:   /* Royalty */
         
         if ctrl2[18] le 1 then do:
            put "Royalty" string(ctrl2[18] * 100,"->>9.99") + "%" to 30
                                     (ctrl2[18] * fac-tot) to 80 skip.
            fac-tot = fac-tot + (fac-tot * ctrl2[18]).
         end.
         else do:
            put "Royalty" ctrl2[18] / qm to 48 ctrl2[18] to 80 skip.
            fac-tot = fac-tot + ctrl2[18].
         end.
      end.

      put "TOTAL FACTORY COST" fac-tot / qm to 48
                               fac-tot format ">>>>,>>9.99" to 80 skip.

      assign
       ord-cost = fac-tot
       tt-tot   = fac-tot
       fac-tot2 = fac-tot.
      
      if ctrl2[1] ne 0 then do:
         put "Warehousing" +
             if v-cewhspct then (string(ctrl[1] * 100,"->>9.99") + "%") else "" to 30
             ctrl2[1] to 80 skip.
         tt-tot = tt-tot + ctrl2[1].
      end.

      if ctrl2[13] ne 0 then do:
         put "Broker Comm" +
             IF v-cewhspct THEN (string(ctrl[19] * 100,"->>9.99") + "%") ELSE "" to 30
             ctrl2[13] to 80 skip.
         tt-tot = tt-tot + ctrl2[13].
      end.

      if xeb.chg-method eq "P" and ctrl[6] eq 0 and fr-tot ne 0 then do:
         put "Freight" fr-tot / qm to 48 fr-tot to 80 skip.
         tt-tot = tt-tot + fr-tot.
      end.

      if ctrl[13] = 0 and ctrl[4] ne 0 then do:
         if ctrl[4] <= 1 then ctrl2[4]  = fac-tot * ctrl[4].
         else if ctrl[4] > 1 then ctrl2[4] = ctrl[4].
         put ce-ctrl.spec-l[1].
         if ctrl[4] <= 1 then
            put string(ce-ctrl.spec-%[1] * 100,"->>9.99") + "%" to 30.
         put ctrl2[4] / qm to 48 ctrl2[4] to 80 skip.
         tt-tot = tt-tot + ctrl2[4].
      end.
      if ctrl[14] = 0 and ctrl[11] ne 0 then do:             /* set spec#2 */
         if ctrl[11] <= 1 then ctrl2[11] = fac-tot * ctrl[11].
         else ctrl2[11] = ctrl[11].
         put ce-ctrl.spec-l[2].
         if ctrl[11] <= 1
            then put string(ce-ctrl.spec-%[2] * 100,"->>9.99") + "%" to 30.
         put ctrl2[11] / qm to 48 ctrl2[11] to 80 skip.
         tt-tot = tt-tot + ctrl2[11].
      end.
      if ctrl[15] = 0 and ctrl[12] ne 0 then do:             /* set spec#3 */
         if ctrl[12] <= 1 then ctrl2[12] = fac-tot * ctrl[12].
         else ctrl2[12] = ctrl[12].
         put ce-ctrl.spec-l[3].
         if ctrl[12] <= 1
            then put string(ce-ctrl.spec-%[3] * 100,"->>9.99") + "%" to 30.
         put ctrl2[12] / qm to 48 ctrl2[12] to 80 skip.
         tt-tot = tt-tot + ctrl2[12].
      end.

      IF ctrl[16] EQ 0 THEN DO:
        IF calcpcts.val[2] NE 0 THEN DO:
          PUT "GS&A Board".
          IF ll-gsa-pct THEN
            PUT STRING(calcpcts.val[1],"->>9.99") + "%" TO 30.
          PUT calcpcts.val[2] / qm TO 48 calcpcts.val[2] TO 80 SKIP.
        END.

        IF ctrl2[9] NE 0 THEN DO:
          PUT "GS&A Material".
          IF ll-gsa-pct THEN
            PUT STRING(ctrl[9] * 100,"->>9.99") + "%" TO 30.
          PUT ctrl2[9] / qm TO 48 ctrl2[9] TO 80 SKIP.
        END.

        IF ctrl2[10] ne 0 THEN DO:
          PUT "GS&A Labor".
          IF ll-gsa-pct THEN
            PUT STRING(ctrl[10] * 100,"->>9.99") + "%" TO 30.
          PUT ctrl2[10] / qm TO 48 ctrl2[10] TO 80 SKIP.
        END.

        tt-tot = tt-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10].
      END.

      if (ctrl[18] eq 0 or ce-ctrl.sell-by eq "B") and
         ctrl2[18] ne 0                            then do:   /* Royalty */
         
         if ce-ctrl.sell-by eq "B" then
            put "Style Markup".
         else
            put "Royalty".
           
         if ctrl2[18] le 1 then do:
            put string(ctrl2[18] * 100,"->>9.99") + "%" to 30
                (ctrl2[18] * fac-tot) / qm to 48
                (ctrl2[18] * fac-tot)      to 80 skip.
            tt-tot = tt-tot + (fac-tot * ctrl2[18]).
         end.
         
         else do:
            put ctrl2[18] / qm to 48 ctrl2[18] to 80 skip.
            tt-tot = tt-tot + ctrl2[18].
         end.
      end.
      
      if v-pct ne 0 then do:
         put "Board Markup" string(v-pct,"->>9.99") + "%" to 30
             (fac-tot * v-pct / 100) / qm to 48
             (fac-tot * v-pct / 100)      to 80 skip.
         tt-tot = tt-tot + (fac-tot * v-pct / 100).
      end.
   end.

   else do:
     assign
      vmcl-desc = "Prep.  Material"
      vmcl-cost = tprep-mat / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 0}

     assign
      vmcl-desc = "Prep.  Labor"
      vmcl-cost = tprep-lab / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 1}

     assign
      vmcl-desc = "Misc.  Material"
      vmcl-cost = mis-tot[1] / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 2}

     assign
      vmcl-desc = "Misc.  Labor"
      vmcl-cost = mis-tot[3] / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 3}

     ASSIGN
        op-tot[5] = op-tot[5] - op-tot[6] - op-tot[7]
        fac-tot   = fac-tot   - op-tot[7].

     assign
      vmcl-desc = "Direct Material"
      vmcl-cost = dm-tot[5] / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 4}

     assign
      vmcl-desc = "Direct Labor"
      vmcl-cost = op-tot[5] / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 5}

     assign
      vmcl-desc = "Variable Overhead"
      vmcl-cost = op-tot[6] / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 6}

     if xeb.chg-method eq "P" and ctrl[6] ne 0 and fr-tot ne 0 then do:
       assign
        vmcl-desc = "Freight"
        vmcl-cost = fr-tot / qm.

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 7}
     end.

     assign
      vmcl-desc = "DIRECT FACTORY COST"
      vmcl-cost = fac-tot / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 8}

     fac-tot2 = fac-tot.

     assign
      vmcl-desc = "Fixed Overhead"
      vmcl-cost = op-tot[7] / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 9}

     fac-tot2 = fac-tot2 + op-tot[7].
     if ctrl[13] ne 0 then fac-tot2 = fac-tot2 + ctrl2[4].
     if ctrl[14] ne 0 then fac-tot2 = fac-tot2 + ctrl2[11].
     if ctrl[15] ne 0 then fac-tot2 = fac-tot2 + ctrl2[12].
     if ctrl[18] ne 0 and ce-ctrl.sell-by ne "B" then
                           fac-tot2 = fac-tot2 + ctrl2[18].

     if ctrl[13] = 1 then do:
       assign
        vmcl-desc = ce-ctrl.spec-l[1]
        vmcl-cost = ctrl2[4] / qm.

       if ctrl[4] <= 1 then
         vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                     string(ce-ctrl.spec-%[1] * 100,"->>9.99%").

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 10}
     end.

     if ctrl[14] = 1 then do:
       assign
        vmcl-desc = ce-ctrl.spec-l[2]
        vmcl-cost = ctrl2[11] / qm.

       if ctrl[11] <= 1 then
         vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                     string(ce-ctrl.spec-%[2] * 100,"->>9.99%").

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 11}
     end.

     if ctrl[15] = 1 then do:
       assign
        vmcl-desc = ce-ctrl.spec-l[3]
        vmcl-cost = ctrl2[12] / qm.

       if ctrl[12] <= 1 then
         vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                     string(ce-ctrl.spec-%[3] * 100,"->>9.99%").

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 12}
     end.

     IF ctrl[16] NE 0 THEN DO:
       ASSIGN
        vmcl-desc = "GS&A Board"
        vmcl-cost = calcpcts.val[2] / qm
        fac-tot2  = fac-tot2 + calcpcts.val[2].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 13}

       IF ll-gsa-pct AND calcpcts.val[2] NE 0 THEN DO:
         mclean.rec-type = "gsabrd".

         ASSIGN
          vmcl-desc = "    GS&A Board %"
          vmcl-cost = calcpcts.val[1].

         {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 14}
         mclean.rec-type = "gsabrd".
       END.

       ASSIGN
        vmcl-desc = "GS&A Material"
        vmcl-cost = ctrl2[9] / qm
        fac-tot2  = fac-tot2 + ctrl2[9].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 15}

       IF ll-gsa-pct AND ctrl2[9] NE 0 THEN DO:
         mclean.rec-type = "gsamat".

         ASSIGN
          vmcl-desc = "    GS&A Material %"
          vmcl-cost = ctrl[9] * 100.

         {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 16}
         mclean.rec-type = "gsamat".
       END.

       ASSIGN
        vmcl-desc = "GS&A Labor"
        vmcl-cost = ctrl2[10] / qm
        fac-tot2  = fac-tot2 + ctrl2[10].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 17}

       IF ll-gsa-pct AND ctrl2[10] NE 0 THEN DO:
         mclean.rec-type = "gsalab".

         ASSIGN
          vmcl-desc = "    GS&A Labor %"
          vmcl-cost = ctrl[10] * 100.

         {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 18}
         mclean.rec-type = "gsalab".
       END.
     END.

     if ctrl[18] gt 0 and ce-ctrl.sell-by ne "B" then do:   /* Royalty */
       vmcl-desc = "Royalty".

       if ctrl2[18] le 1 then
         assign
          vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                      string(ctrl2[18] * 100,"->>9.99%")
          vmcl-cost = (ctrl2[18] * fac-tot).

       else vmcl-cost = ctrl2[18].

       assign
        /*fac-tot2  = fac-tot2 + vmcl-cost - 26340 - double counted since already included above*/
        vmcl-cost = vmcl-cost / qm.

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 19}
     end.

     assign
      vmcl-desc = "TOTAL FACTORY COST"
      vmcl-cost = fac-tot2 / qm.
     {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 20}

     assign
      ord-cost = fac-tot2
      tt-tot   = fac-tot2.

     if ctrl2[1] ne 0 or not v-cewhspct then do:
       assign
        vmcl-desc = "Warehousing"
        vmcl-cost = ctrl2[1] / qm
        tt-tot    = tt-tot + ctrl2[1].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 21}
       
       if v-cewhspct then do:
         assign
          mclean.rec-type = "warehousing"
          vmcl-desc       = "    Warehousing %"
          vmcl-cost       = ctrl[1] * 100.

         {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 22}
         mclean.rec-type = "warehousing".
       end.
     end.

     if ctrl2[13] ne 0 then do:
       assign
        vmcl-desc = "Broker Comm"
        vmcl-cost = ctrl2[13] / qm
        tt-tot    = tt-tot + ctrl2[13].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 23}
       
       assign
          mclean.rec-type = "broker"
          vmcl-desc       = "    Broker Comm %"
          vmcl-cost       = ctrl[19] * 100.

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 24}
       mclean.rec-type = "broker".
     end.

     if xeb.chg-method eq "P" and ctrl[6] eq 0 and fr-tot ne 0 then do:
       assign
        vmcl-desc = "Freight"
        vmcl-cost = fr-tot / qm
        tt-tot    = tt-tot + fr-tot.

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 25}
     end.

     if ctrl[13] = 0 then do:
       vmcl-desc = ce-ctrl.spec-l[1].

       if ctrl[4] <= 1 then
         assign
          vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                      string(ce-ctrl.spec-%[1] * 100,"->>9.99%")
          ctrl2[4]  = fac-tot2 * ctrl[4].
       else ctrl2[4] = ctrl[4].

       assign
        vmcl-cost = ctrl2[4] / qm
        tt-tot    = tt-tot + ctrl2[4].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 26}
     end.

     if ctrl[14] = 0 then do:             /* set spec#2 */
       vmcl-desc = ce-ctrl.spec-l[2].

       if ctrl[11] <= 1 then
         assign
          vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                      string(ce-ctrl.spec-%[2] * 100,"->>9.99%")
          ctrl2[11]  = fac-tot2 * ctrl[11].
       else ctrl2[11] = ctrl[11].

       assign
        vmcl-cost = ctrl2[11] / qm
        tt-tot    = tt-tot + ctrl2[11].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 27}
     end.

     if ctrl[15] = 0 then do:             /* set spec#3 */
       vmcl-desc = ce-ctrl.spec-l[3].

       if ctrl[12] <= 1 then
         assign
          vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                      string(ce-ctrl.spec-%[3] * 100,"->>9.99%")
          ctrl2[12]  = fac-tot2 * ctrl[12].
       else ctrl2[12] = ctrl[12].

       assign
        vmcl-cost = ctrl2[12] / qm
        tt-tot    = tt-tot + ctrl2[12].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 28}
     end.

     IF ctrl[16] EQ 0 THEN DO:
       ASSIGN
        vmcl-desc = "GS&A Board"
        vmcl-cost = calcpcts.val[2] / qm
        tt-tot    = tt-tot + calcpcts.val[2].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 29}

       IF ll-gsa-pct AND calcpcts.val[2] NE 0 THEN DO:
         mclean.rec-type = "gsabrd".

         ASSIGN
          vmcl-desc = "    GS&A Board %"
          vmcl-cost = calcpcts.val[1].

         {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 30}
         mclean.rec-type = "gsabrd".
       END.

       ASSIGN
        vmcl-desc = "GS&A Material"
        vmcl-cost = ctrl2[9] / qm
        tt-tot    = tt-tot + ctrl2[9].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 31}

       IF ll-gsa-pct AND ctrl2[9] NE 0 THEN DO:
         mclean.rec-type = "gsamat".

         ASSIGN
          vmcl-desc = "    GS&A Material %"
          vmcl-cost = ctrl[9] * 100.

         {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 32}
         mclean.rec-type = "gsamat".
       END.

       ASSIGN
        vmcl-desc = "GS&A Labor"
        vmcl-cost = ctrl2[10] / qm
        tt-tot    = tt-tot + ctrl2[10].

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 33}

       IF ll-gsa-pct AND ctrl2[10] NE 0 THEN DO:
         mclean.rec-type = "gsalab".

         ASSIGN
          vmcl-desc = "    GS&A Labor %"
          vmcl-cost = ctrl[10] * 100.

         {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 34}
         mclean.rec-type = "gsalab".
       END.
     END.
     
     if ctrl[18] eq 0 or ce-ctrl.sell-by eq "B" then do:   /* Royalty */
       vmcl-desc = if ce-ctrl.sell-by eq "B" then "Style Markup"
                                             else "Royalty".
                                             
       if ctrl2[18] le 1 then
         assign
          vmcl-desc = vmcl-desc + fill(" ",22 - length(trim(vmcl-desc))) +
                      string(ctrl2[18] * 100,"->>9.99%")
          vmcl-cost = (ctrl2[18] * fac-tot).

       else vmcl-cost = ctrl2[18].

       assign
        tt-tot    = tt-tot + vmcl-cost
        vmcl-cost = vmcl-cost / qm.

       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 35}
     end.
     
     if v-pct ne 0 then do:
       assign
        vmcl-desc = "Board Markup"
        tt-tot    = tt-tot + (fac-tot * v-pct / 100)
        vmcl-cost = (fac-tot * v-pct / 100) / qm.
         
       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 36}
       mclean.rec-type = "boardm".
       
       assign
        vmcl-desc = "    Board Markup %"
        vmcl-cost = v-pct.
         
       {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 37}
       mclean.rec-type = "boardm".
     end.
   end.

   output close.

   run cec/pr4-mis2.p.  /* billable charge */

/* end ---------------------------------- copr. 1993  advanced software, inc. */
