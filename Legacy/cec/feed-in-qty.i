/*logic from cec/pr4-mch.p*/
IF xest.est-type EQ 5 THEN
DO:
   ASSIGN
      v-on-s-2 = xeb.num-up * vn-out
      ll-unitize-2 = NO.

   FOR EACH mstd
       WHERE mstd.company eq mach.company
         AND mstd.loc     eq mach.loc
         AND mstd.m-code  eq mach.m-code
       NO-LOCK
       BREAK BY mstd.style DESC:
     IF LAST(mstd.style)        OR
        mstd.style EQ xeb.style THEN DO:
       ll-unitize-2 = mstd.rs-x EQ 98 OR mstd.rs-y EQ 98.
       LEAVE.
     END.
   END.

   RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f-2).

   IF est-op.dept EQ "DC" AND est-op.n-out GT 0 THEN DO:
      FIND FIRST ef-nsh OF xef
          WHERE ef-nsh.pass-no EQ est-op.op-pass
            AND ef-nsh.dept    EQ est-op.dept
          NO-LOCK NO-ERROR.
      IF AVAIL ef-nsh THEN DO:
         RUN cec/foamplus.p (ROWID(ef-nsh), OUTPUT li-die-2).
         v-on-f-2 = v-on-f-2 * (est-op.n-out + INT(li-die-2 GT 0)).
      END.
   END.

   IF ll-unitize-2 THEN
   DO:
      ASSIGN
         save-qty-2 = save-qty / xeb.yld-qty
         c-qty-2 = 0
         p-qty-2 = 0.

      find first sys-ctrl WHERE
           sys-ctrl.company eq xef.company AND
           sys-ctrl.name    eq "MSFCALC"
           no-lock no-error.

      v-corr-2 = (not avail sys-ctrl) or sys-ctrl.char-fld eq "Corrware".

      if xeb.cas-no ne "" then do:

         FIND FIRST ITEM WHERE
              ITEM.company EQ xef.company AND
              item.i-no EQ xef.board
              NO-LOCK NO-ERROR.
         
         b-wt-2 = IF AVAIL item THEN item.basis-w ELSE 0.

         RELEASE ITEM.

         find first item where 
              item.company = xeb.company and
              item.i-no = xeb.cas-no
              no-lock no-error.

         if xeb.cas-cnt ne 0 then
            c-qty-2 = save-qty-2 / xeb.cas-cnt.
         else
            c-qty-2 = if v-corr-2 then
                (((xeb.t-sqin - xeb.t-win) * save-qty-2 * .000007) * b-wt-2)
                / (if avail(item) then item.avg-w else 1)
              else
                (((xeb.t-sqin - xeb.t-win) * save-qty-2 / 144000)  * b-wt-2)
                / (if avail(item) then item.avg-w else 1).

         if c-qty-2 > int(c-qty-2) then c-qty-2 = integer(c-qty-2) + 1.
         else c-qty-2 = integer(c-qty-2).
      END.

      if xeb.tr-no ne "" then do:

         RELEASE ITEM.

         FIND FIRST ITEM WHERE
              ITEM.company EQ xef.company AND
              item.i-no EQ xef.board
              NO-LOCK NO-ERROR.
         
         b-wt-2 = IF AVAIL item THEN item.basis-w ELSE 0.

         RELEASE ITEM.
      
         find first item where 
              item.company = xeb.company and
              item.i-no = xeb.tr-no
              no-lock no-error.

         if xeb.cas-pal ne 0 then
            p-qty-2 = c-qty-2 / xeb.cas-pal.
         else
            p-qty-2 = if v-corr-2 then
              ((((xeb.t-sqin - xeb.t-win) * save-qty-2 * .000007) * b-wt-2) + c-qty-2)
              / (if avail(item) then item.avg-w else 1)
              else
              ((((xeb.t-sqin - xeb.t-win) * save-qty-2 / 144000)  * b-wt-2) + c-qty-2)
              / (if avail(item) then item.avg-w else 1).

         if p-qty-2 > integer(p-qty-2) then p-qty-2 = integer(p-qty-2) + 1.
         else p-qty-2 = integer(p-qty-2).
      END.
      
      qty = p-qty-2.
   END.
   ELSE IF mach.therm AND (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
        qty = (((est-op.num-sh * v-on-f-2) - est-op.op-waste)).
   ELSE
   IF est-op.op-sb THEN
      qty = ((est-op.num-sh * v-on-f-2) - est-op.op-waste).
   ELSE
      qty = ((est-op.num-sh * v-on-s-2) - est-op.op-waste).
END. /*xest.est-type EQ 5*/
/*logic from cec/box/pr42-mch.p*/
ELSE IF xest.est-type EQ 6 THEN
DO:
   ll-unitize-2 = NO.

   FOR EACH mstd
       WHERE mstd.company eq mach.company
         AND mstd.loc     eq mach.loc
         AND mstd.m-code  eq mach.m-code
       NO-LOCK
       BREAK BY mstd.style DESC:
     IF LAST(mstd.style)        OR
        mstd.style EQ xeb.style THEN DO:
       ll-unitize-2 = mstd.rs-x EQ 98 OR mstd.rs-y EQ 98.
       LEAVE.
     END.
   END.

   IF INDEX("AP",mach.p-type) GT 0 THEN
      ASSIGN
         v-num-up-2 = 1
         v-n-out-2  = 1
         v-on-s-2 = 1
         v-on-f-2 = 1.
   ELSE DO:
      RUN sys/inc/numup.p (xef.company,xef.est-no, xef.form-no, OUTPUT v-num-up-2).
      RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out-2).
      RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f-2).
   END.

   IF est-op.dept EQ "DC" AND est-op.n-out GT 0 THEN DO:
      FIND FIRST ef-nsh OF xef
          WHERE ef-nsh.pass-no EQ est-op.op-pass
            AND ef-nsh.dept    EQ est-op.dept
          NO-LOCK NO-ERROR.
      IF AVAIL ef-nsh THEN DO:
         RUN cec/foamplus.p (ROWID(ef-nsh), OUTPUT li-die-2).
         v-on-f-2 = v-on-f-2 * (est-op.n-out + INT(li-die-2 GT 0)).
      END.
   END.

   IF ll-unitize-2 THEN DO:

      EMPTY TEMP-TABLE cas-2.

      RUN cec\BOX\pr42-cas-tt.p(INPUT save-qty,
                                INPUT xef.form-no).

      find first b-eb-2 WHERE
           b-eb-2.company   eq est-op.company AND
           b-eb-2.est-no    eq est-op.est-no AND
           b-eb-2.form-no   eq est-op.s-num AND
           (b-eb-2.blank-no eq est-op.b-num or est-op.b-num eq 0)
           no-lock no-error.

      FOR EACH cas-2
          WHERE cas-2.snum EQ b-eb-2.form-no
            AND cas-2.bnum EQ b-eb-2.blank-no
            AND cas-2.typ  EQ 3
            NO-LOCK:
          ACCUMULATE cas-2.qty (TOTAL).
      END.
      qty = (ACCUM TOTAL cas-2.qty).
    END.
    ELSE
    IF mach.p-type EQ "P" THEN
    DO:
       v-parts-2 = 0.
       FOR EACH b-eb-2 fields(yld-qty) WHERE
           b-eb-2.company EQ xest.company AND
           b-eb-2.est-no  EQ xest.est-no AND
           b-eb-2.form-no NE 0
           NO-LOCK:
           v-parts-2 = v-parts-2 +
              (IF b-eb-2.yld-qty LT 0 THEN (-1 / b-eb-2.yld-qty) ELSE b-eb-2.yld-qty).
       END.
       qty = (est-op.num-sh - est-op.op-waste) * v-parts-2.
    END.
    ELSE
    IF mach.therm AND
       mach.p-type NE "A" AND
       (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
       qty = (((est-op.num-sh * v-on-f-2) - est-op.op-waste)).
    ELSE
    IF est-op.op-sb THEN
       qty = ((est-op.num-sh * v-on-f-2) - est-op.op-waste).
    ELSE
       qty = ((est-op.num-sh * v-num-up-2 * v-n-out-2) - est-op.op-waste).
    
END. /*xest.est-type EQ 6*/
