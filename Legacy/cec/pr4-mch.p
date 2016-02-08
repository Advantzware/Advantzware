/* -------------------------------------------------- cec/pr4-mch.p 03/97 JLF */
def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def shared var k as int no-undo.
def var xxx as int no-undo.
def shared var qty as INT NO-UNDO .
def var zzz as int no-undo.
def var i as int no-undo.

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

{cec/print4.i shared shared}
{cec/print42.i shared}

def var v-on-f as DEC NO-UNDO.
def var v-factor as dec NO-UNDO.
def var v-lab-fac as dec extent 2 NO-UNDO.
def var v-fix-fac as dec extent 2 NO-UNDO.
def var v-var-fac as dec extent 2 NO-UNDO.
def var v-num-up as int NO-UNDO.
def var v-msf as dec no-undo.
def var v-on-s as DEC NO-UNDO.
DEF VAR v-len LIKE xef.gsh-len NO-UNDO.
DEF VAR ll-unitize AS LOG NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
DEF VAR li-die AS INT NO-UNDO.
DEF VAR ld-fg-rate AS DEC NO-UNDO.
DEF VAR ld-hand-pct AS DEC NO-UNDO.
DEF VAR opmr LIKE est-op.op-mr NO-UNDO.
DEF VAR opsp AS DEC NO-UNDO.

DEF SHARED WORKFILE w-form
    FIELD form-no LIKE xef.form-no
    FIELD min-msf AS   LOG.

{cec/msfcalc.i}

{cec/rollfac.i}

{sys/inc/ceprice.i}

opsplit$ = 0.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

   assign
    v-fac-hdr = if vmclean then
                  ("Run$/M" + if v-rollfac then "SF" else "")
                else "Run $"
    v-fac-hdr = fill(" ",8 - length(trim(v-fac-hdr))) + trim(v-fac-hdr).

    RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).
    v-on-s = xeb.num-up * v-n-out.

   put skip(1) "Machine Desc     Out     SU     Run Speed    Rate    SU $"
       space(3) v-fac-hdr format "x(8)" space(2) "Total Cost".

   /* machines */
   for each est-op where est-op.company = xest.company 
                     and est-op.est-no  = xest.est-no
                     and est-op.qty   eq v-op-qty 
                     and est-op.line gt 500
       by est-op.b-num by est-op.d-seq by est-op.op-pass
       with frame ae down no-labels no-box:

      find first w-form where w-form.form-no eq est-op.s-num no-error.
      if not avail w-form then do:
        create w-form.
        w-form.form-no = est-op.s-num.
      end.

      find first mach
          {sys/ref/machW.i}
            and mach.m-code eq est-op.m-code
          no-lock no-error.
      if not avail mach then next.

      RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f).
      
      IF est-op.dept EQ "DC" AND est-op.n-out GT 0 THEN DO:
        FIND FIRST ef-nsh OF xef
            WHERE ef-nsh.pass-no EQ est-op.op-pass
              AND ef-nsh.dept    EQ est-op.dept
            NO-LOCK NO-ERROR.
        IF AVAIL ef-nsh THEN DO:
            
          RUN cec/foamplus.p (ROWID(ef-nsh), OUTPUT li-die).
          
          v-on-f = v-on-f * (est-op.n-out + INT(li-die GT 0)).
        END.
      END.
      
      v-len = IF est-op.dept EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.

      ll-unitize = NO.
      FOR EACH mstd
          WHERE mstd.company eq mach.company
            AND mstd.loc     eq mach.loc
            AND mstd.m-code  eq mach.m-code
          NO-LOCK
          BREAK BY mstd.style DESC:
        IF LAST(mstd.style)        OR
           mstd.style EQ xeb.style THEN DO:
          ll-unitize = mstd.rs-x EQ 98 OR mstd.rs-y EQ 98.
          LEAVE.
        END.
      END.

      ASSIGN
       opmr = est-op.op-mr
       opsp = est-op.op-speed.

      IF opsp NE 0 THEN
        IF ll-unitize THEN oprun = p-qty / opsp.
        ELSE
          IF mach.therm AND (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
             oprun = (((est-op.num-sh * v-on-f) - est-op.op-waste) *
                     (v-len / 12)) / opsp.
          ELSE
            IF mach.p-type = "A" THEN
              oprun = xest.est-qty[1] / opsp.
            ELSE
              IF est-op.op-sb THEN
                 oprun = ((est-op.num-sh * v-on-f) - est-op.op-waste) / opsp.
              ELSE
                oprun = ((est-op.num-sh * v-on-s) - est-op.op-waste) / opsp.       
      ELSE oprun = 0.

      IF w-form.min-msf AND mach.dept[1] EQ "RC" THEN
        ASSIGN
         opmr$     = 0
         oprun$    = 0
         optot$    = 0
         v-lab-fac = 0
         v-var-fac = 0
         v-fix-fac = 0.

      ELSE DO:
        ASSIGN
         opmr$        = opmr  * est-op.op-rate[1]
         oprun$       = oprun * est-op.op-rate[2]
         optot$       = opmr$ + oprun$

         v-lab-fac[1] = opmr  * est-op.op-crew[1] * mach.lab-rate[lab-drate]
         v-lab-fac[2] = oprun * est-op.op-crew[2] * mach.lab-rate[lab-drate]

         v-var-fac[1] = opmr  * mach.mr-varoh
         v-var-fac[2] = oprun * mach.run-varoh

         v-fix-fac[1] = opmr  * mach.mr-fixoh
         v-fix-fac[2] = oprun * mach.run-fixoh.

        IF v-min-mchg THEN DO:
          IF ceprice-chr EQ "MR+Run" AND mach.mrk-rate GT optot$ THEN
            ASSIGN
             v-factor = (mach.mrk-rate / optot$)
             opmr$    = opmr$  * v-factor
             oprun$   = oprun$ * v-factor.

          ELSE
          IF ceprice-chr EQ "RunOnly" AND mach.mrk-rate GT oprun$ THEN
            oprun$ = mach.mrk-rate.

          ASSIGN
           v-factor     = optot$
           optot$       = oprun$ + opmr$
           v-factor     = optot$ / v-factor.

          IF v-factor EQ ? THEN v-factor = 0.

          ASSIGN
           v-lab-fac[1] = v-lab-fac[1] * v-factor
           v-lab-fac[2] = v-lab-fac[2] * v-factor
           v-var-fac[1] = v-var-fac[1] * v-factor
           v-var-fac[2] = v-var-fac[2] * v-factor
           v-fix-fac[1] = v-fix-fac[1] * v-factor
           v-fix-fac[2] = v-fix-fac[2] * v-factor.
        END.

        ASSIGN
         opsplit$[1] = opsplit$[1] + v-lab-fac[1] + v-lab-fac[2]
         opsplit$[2] = opsplit$[2] + v-var-fac[1] + v-var-fac[2]
         opsplit$[3] = opsplit$[3] + v-fix-fac[1] + v-fix-fac[2].
      END.

      IF est-op.m-code NE "" THEN DO:
        CREATE op.
        ASSIGN op.blank-no = est-op.b-num
               op.dept     = est-op.dept
               op.form-no  = est-op.s-num
               op.i-no     = xeb.stock-no
               op.line     = est-op.line
               op.m-code   = est-op.m-code
               op.mr-fixoh = mach.mr-fixoh
               op.mr-hr    = opmr
               op.mr-rate  = mach.mr-rate
               op.mr-varoh = mach.mr-varoh
               op.mr-waste = est-op.op-waste
               op.pass     = est-op.op-pass
               op.run-hr   = oprun
               op.wst-prct = est-op.op-spoil.

         IF est-op.op-sb THEN
           op.run-qty = est-op.num-sh * v-on-f.
         ELSE
           op.run-qty = est-op.num-sh * xeb.num-up * v-n-out.
               
         op.speed = IF ll-unitize THEN (op.run-qty / oprun)
                                  ELSE opsp.

        IF vmclean THEN oprun$ = oprun$ /
                                 (qtty[k] * xeb.yld-qty / 1000 * v-sqft-fac).
        /* task 09171202 */
        IF est-op.dept EQ "DC" AND est-op.n-out EQ 0 THEN
            v-on-s = xeb.num-up.      

        DISPLAY est-op.m-dscr           FORMAT "x(16)"
                est-op.n-out            FORMAT ">>>9"
                   v-on-s / v-on-f WHEN est-op.n-out EQ 0 @ est-op.n-out
                   1 WHEN NOT est-op.op-sb                @ est-op.n-out
                   1 WHEN mach.p-type = "A"               @ est-op.n-out
                opmr                    FORMAT ">>9.99"
                oprun                   FORMAT ">>>9.99"     TO 35
                opsp                    FORMAT ">>>>9"       TO 41
                est-op.op-rate[2]       FORMAT ">>>9.99"     TO 49
                    WHEN ce-ctrl.sho-labor EQ TRUE
                opmr$                   FORMAT ">>>>9.99"    TO 57
                oprun$                  FORMAT ">>>>>>9.99"  TO 68
                optot$                  FORMAT ">>>>,>>9.99" TO 80 SKIP
            WITH STREAM-IO.

        IF vmclean THEN oprun$ = oprun$ *
                                 (qtty[k] * xeb.yld-qty / 1000 * v-sqft-fac).

        IF AVAIL itemfg AND est-op.b-num NE 0 THEN op.i-name = itemfg.i-name.
        IF op.line GT 500 THEN op.line = op.line - 500.
      END.

      ASSIGN
      op-tot[1] = op-tot[1] + opmr
      op-tot[2] = op-tot[2] + oprun
      op-tot[3] = op-tot[3] + opmr$
      op-tot[4] = op-tot[4] + oprun$
      op-tot[5] = op-tot[5] + optot$
      op-tot[6] = op-tot[6] + v-var-fac[1] + v-var-fac[2]
      op-tot[7] = op-tot[7] + v-fix-fac[1] + v-fix-fac[2].
   END.

   ASSIGN
    ctrl2[2]    = 0
    ld-hand-pct = IF xeb.pur-man THEN hand-pct-f ELSE ctrl[2].

   IF ld-hand-pct NE 0 THEN
     ctrl2[2] = ctrl2[2] + ((dm-tot[5] + tprep-mat + mis-tot[1]) * ld-hand-pct).

   if ctrl2[2] + ctrl2[3] ne 0 then do:
      put
      "Raw Mat'l Handling" (ctrl2[2] + ctrl2[3]) to 80 skip.
      assign
       opsplit$[1] = opsplit$[1] + ctrl2[2] + ctrl2[3]
       op-tot[5]   = op-tot[5] + (ctrl2[2] + ctrl2[3]).
   end.
   
   find first carrier
       where carrier.company eq cocode
         and carrier.loc     eq locode
         and carrier.carrier eq xeb.carrier
       no-lock no-error.
   if avail carrier then
       find first carr-mtx
       where carr-mtx.company  eq cocode
         and carr-mtx.loc      eq locode
         and carr-mtx.carrier  eq carrier.carrier
         and carr-mtx.del-zone eq xeb.dest-code
         AND carr-mtx.del-zip EQ xeb.ship-zip
       no-lock no-error.
   IF NOT AVAIL carr-mtx THEN
   find first carr-mtx
       where carr-mtx.company  eq cocode
         and carr-mtx.loc      eq locode
         and carr-mtx.carrier  eq carrier.carrier
         and carr-mtx.del-zone eq xeb.dest-code
       no-lock no-error.

   assign
    v-msf = (if v-corr then ((xeb.t-sqin - xeb.t-win) * qty * .007)
                       else ((xeb.t-sqin - xeb.t-win) * qty / 144)) / 1000
    zzz   = xef.weight.
   
   if xef.medium ne "" then do:
      find first item {sys/look/itemW.i} and
                 item.i-no = xef.medium no-lock no-error.
      if avail item
      then zzz = zzz + (item.basis-w * (1 - (item.shrink / 100))).
   end.
   
   if xef.flute ne "" then do:
      find first item {sys/look/itemW.i} and
                 item.i-no = xef.flute no-lock no-error.
      if avail item
      then zzz = zzz + item.basis-w.
   end.
   
   /*if xef.lam-code ne "" then do:
      find first item {sys/look/itemW.i} and
                 item.i-no = xef.lam-code no-lock no-error.
      if avail item
      then zzz = zzz + item.basis-w.
   end.*/
   
   xxx = v-msf * zzz.                           /* total weight */
   
   find first item                              /* add cases */
       where item.company eq cocode
         and item.i-no    eq xeb.cas-no
         and xeb.cas-no   ne ""
       no-lock no-error.
   xxx = xxx + (c-qty * (if avail item then item.basis-w
                                       else ce-ctrl.def-cas-w)).
                                       
   find first item                              /* add pallets */
       where item.company eq cocode
         and item.i-no    eq xeb.tr-no
         and xeb.tr-no    ne ""
       no-lock no-error.

   ASSIGN
   xxx = xxx + (p-qty * (if avail item then item.basis-w
                                       else ce-ctrl.def-pal-w))
   fr-tot = 0.
   
   if xeb.fr-out-c ne 0 then
     fr-tot = xeb.fr-out-c * (xxx / 100).
   else
   if xeb.fr-out-m ne 0 then
     fr-tot = xeb.fr-out-m * (qtty[k] / 1000).
   else
   if avail carr-mtx then do:
     if carrier.chg-method eq "W" then
     do i = 1 to 10:
       fr-tot = carr-mtx.rate[i] * xxx / 100.
       if carr-mtx.weight[i] ge xxx then leave.
     end.
     
     else
     if carrier.chg-method eq "P" then
     do i = 1 to 10:
       fr-tot = carr-mtx.rate[i] * p-qty.
       if carr-mtx.weight[i] ge p-qty then leave.
     end.
     
     else do:
       find first item
           {sys/look/itemW.i}
             and item.i-no     eq xef.board
             and item.mat-type eq "B"
             and item.avg-w    gt 0
           no-lock no-error.
       v-msf = v-msf * if avail item then item.avg-w else 1.
      
       do i = 1 to 10:
         fr-tot = carr-mtx.rate[i] * v-msf.
         if carr-mtx.weight[i] ge v-msf then leave.
       end.
     end.
     
     if fr-tot lt carr-mtx.min-rate then fr-tot = carr-mtx.min-rate.
      
     fr-tot = fr-tot + (carr-mtx.min-rate * (rels[k] - 1)).
   end.

   ld-fg-rate = IF xeb.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate.

   IF fg-wt / 100 * ld-fg-rate NE 0 THEN DO:
      PUT "Finished Goods Handling" fg-wt / 100 * ld-fg-rate TO 80 SKIP.
      ASSIGN
       opsplit$[1] = opsplit$[1] + (fg-wt / 100 * ld-fg-rate)
       op-tot[5]   = op-tot[5] + (fg-wt / 100 * ld-fg-rate).
   END.

   if vmclean then op-tot[4] = op-tot[4] /
                               (qtty[k] * xeb.yld-qty / 1000 * v-sqft-fac).

   put "TOTAL  OPERATIONS        "
       op-tot[3]                format ">>>>9.99"    to 57
       op-tot[4]                format ">>>>>>9.99"  to 68
       op-tot[5]                format ">>>>,>>9.99" to 80 skip(1).

   if vmclean then op-tot[4] = op-tot[4] *
                               (qtty[k] * xeb.yld-qty / 1000 * v-sqft-fac).

/* end ---------------------------------- copr. 1997  advanced software, inc. */
