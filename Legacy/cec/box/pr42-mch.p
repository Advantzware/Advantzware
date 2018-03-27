/* --------------------------------------------- cec/box/pr42-mch.p 07/96 JLF */
/* Machine Operations for setup boxes                                         */
/* -------------------------------------------------------------------------- */

DEFINE SHARED VARIABLE cocode AS cha     NO-UNDO.
DEFINE SHARED VARIABLE locode AS cha     NO-UNDO.
DEFINE SHARED VARIABLE k      AS INTEGER NO-UNDO.
DEFINE        VARIABLE xxx    AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE qty    AS INTEGER NO-UNDO.
DEFINE        VARIABLE zzz    AS INTEGER NO-UNDO.
DEFINE        VARIABLE i      AS INTEGER NO-UNDO.
DEFINE        VARIABLE j      AS INTEGER NO-UNDO.

DEFINE SHARED BUFFER xest FOR est.
DEFINE SHARED BUFFER xef  FOR ef.
DEFINE SHARED BUFFER xeb  FOR eb.
DEFINE SHARED BUFFER xop  FOR est-op.

{cec/print4.i shared shared}
{cec/print42.i shared}

DEFINE BUFFER xcar   FOR car.
DEFINE BUFFER b-cost FOR reftable.
DEFINE BUFFER b-qty  FOR reftable.

DEFINE VARIABLE v-num-up    AS INTEGER NO-UNDO.
DEFINE VARIABLE v-num-in    LIKE est-op.num-sh NO-UNDO.
DEFINE VARIABLE vn-out      LIKE xef.n-out-l INIT 1 NO-UNDO.
DEFINE VARIABLE v-outw      LIKE xef.n-out NO-UNDO.
DEFINE VARIABLE v-outl      LIKE xef.n-out-l NO-UNDO.
DEFINE VARIABLE v-outf      AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-on-f      AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-on-l      AS DECIMAL NO-UNDO.
DEFINE VARIABLE sh-tmp      LIKE sh-len NO-UNDO.
DEFINE VARIABLE v-widp      AS LOG     NO-UNDO.
DEFINE VARIABLE v-factor    AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-lab-fac   AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-fix-fac   AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-var-fac   AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-yld       AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-msf       AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-rm$       AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-on-s      AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-len       LIKE xef.gsh-len NO-UNDO.
DEFINE VARIABLE ll-unitize  AS LOG     NO-UNDO.
DEFINE VARIABLE v-n-out     AS INTEGER NO-UNDO.
DEFINE VARIABLE li-die      AS INTEGER NO-UNDO.
DEFINE VARIABLE v-parts     AS DECIMAL NO-UNDO.
DEFINE VARIABLE ld-hand-pct AS DECIMAL NO-UNDO.
DEFINE VARIABLE opmr        LIKE est-op.op-mr NO-UNDO.
DEFINE VARIABLE opsp        AS DECIMAL NO-UNDO.

DEFINE TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty  AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

DEFINE SHARED WORKFILE w-form
    FIELD form-no LIKE xef.form-no
    FIELD min-msf AS   LOG.

{cec/msfcalc.i}

{sys/inc/ceprice.i}
{ce/fEstOpRecKey.i}

v-parts = 0.
FOR EACH eb FIELDS(quantityPerSet) NO-LOCK
    WHERE eb.company EQ xest.company
    AND eb.est-no  EQ xest.est-no
    AND eb.form-no NE 0:
    v-parts = v-parts +
        (IF eb.quantityPerSet LT 0 THEN (-1 / eb.quantityPerSet) ELSE eb.quantityPerSet).
END.

/* machines */
FOR EACH xef
    WHERE xef.company = xest.company
    AND xef.est-no EQ xest.est-no
    AND (xef.form-no EQ v-form-no OR (NOT vmclean2)):

    FIND FIRST w-form WHERE w-form.form-no EQ xef.form-no NO-ERROR.
    IF NOT AVAILABLE w-form THEN 
    DO:
        CREATE w-form.
        w-form.form-no = xef.form-no.
    END.

    FOR EACH est-op
        WHERE est-op.company EQ xest.company
        AND est-op.est-no  EQ xest.est-no
        AND est-op.qty     EQ v-op-qty
        AND est-op.s-num   EQ xef.form-no
        AND est-op.line    GT 500
        BY est-op.b-num BY est-op.d-seq BY est-op.op-pass
        WITH FRAME ae DOWN NO-LABELS NO-BOX  STREAM-IO:

        FIND FIRST mach
        {sys/look/machW.i}
          AND mach.m-code EQ est-op.m-code
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE mach THEN NEXT.

        FIND FIRST eb
            WHERE eb.company   EQ est-op.company
            AND eb.est-no    EQ est-op.est-no
            AND eb.form-no   EQ est-op.s-num
            AND (eb.blank-no EQ est-op.b-num OR est-op.b-num EQ 0)
            NO-LOCK NO-ERROR.

        IF INDEX("AP",mach.p-type) GT 0 THEN
            ASSIGN
                v-num-up = 1
                v-n-out  = 1
                v-on-s   = 1
                v-on-f   = 1.

        ELSE 
        DO:
            RUN sys/inc/numup.p (xef.company,xef.est-no, xef.form-no, OUTPUT v-num-up).
            RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).
            RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f).
        END.

        IF est-op.dept EQ "DC" AND est-op.n-out GT 0 THEN 
        DO:
            FIND FIRST ef-nsh OF xef
                WHERE ef-nsh.pass-no EQ est-op.op-pass
                AND ef-nsh.dept    EQ est-op.dept
                NO-LOCK NO-ERROR.
            IF AVAILABLE ef-nsh THEN 
            DO:
                RUN cec/foamplus.p (ROWID(ef-nsh), OUTPUT li-die).
                v-on-f = v-on-f * (est-op.n-out + INT(li-die GT 0)).
            END.
        END.

        ASSIGN
            v-on-s = v-num-up * v-n-out
            v-len  = IF est-op.dept EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.

        ll-unitize = NO.
        FOR EACH mstd
            WHERE mstd.company EQ mach.company
            AND mstd.loc     EQ mach.loc
            AND mstd.m-code  EQ mach.m-code
            NO-LOCK
            BREAK BY mstd.style DESCENDING:
            IF LAST(mstd.style)        OR
                mstd.style EQ eb.style THEN 
            DO:
                ll-unitize = mstd.rs-x EQ 98 OR mstd.rs-y EQ 98.
                LEAVE.
            END.
        END.

        ASSIGN
            opsp = est-op.op-speed
            opmr = est-op.op-mr.

        IF opsp NE 0 THEN
            IF ll-unitize THEN 
            DO:
                FOR EACH cas
                    WHERE cas.snum EQ eb.form-no
                    AND cas.bnum EQ eb.blank-no
                    AND cas.typ  EQ 3
                    NO-LOCK:
                    ACCUMULATE cas.qty (TOTAL).
                END.
                oprun = (ACCUM TOTAL cas.qty) / opsp.
            END.
            ELSE
                IF mach.p-type EQ "P" THEN
                    oprun = (est-op.num-sh - est-op.op-waste) * v-parts / opsp.
                ELSE
                    IF mach.therm                                  AND
                        mach.p-type NE "A"                          AND
                        (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
                        oprun = (((est-op.num-sh * v-on-f) - est-op.op-waste) *
                            (v-len / 12)) / opsp.
                    ELSE
                        IF est-op.op-sb THEN
                            oprun = ((est-op.num-sh * v-on-f) - est-op.op-waste) / opsp.
                        ELSE
                            oprun = ((est-op.num-sh * v-on-s) - est-op.op-waste) / opsp.
        ELSE oprun = 0.
        
        /*Run Qty Divisor 24462 (also undoes 19774)*/
        IF est-op.n_out_div GT 0 THEN 
            oprun = oprun / est-op.n_out_div.
        
        IF w-form.min-msf AND mach.dept[1] EQ "RC" THEN
            ASSIGN
                opmr$     = 0
                oprun$    = 0
                optot$    = 0
                v-lab-fac = 0
                v-var-fac = 0
                v-fix-fac = 0.

        ELSE 
        DO:
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

            IF v-min-mchg THEN 
            DO:
                IF ceprice-chr EQ "MR+Run" AND mach.mrk-rate GT optot$ THEN
                    ASSIGN
                        v-factor = (mach.mrk-rate / optot$)
                        opmr$    = opmr$  * v-factor
                        oprun$   = oprun$ * v-factor.

                ELSE
                    IF ceprice-chr EQ "RunOnly" AND mach.mrk-rate GT oprun$ THEN
                        oprun$ = mach.mrk-rate.

                ASSIGN
                    v-factor = optot$
                    optot$   = oprun$ + opmr$
                    v-factor = optot$ / v-factor.

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

            IF est-op.b-num NE 0 THEN 
            DO:
                FIND FIRST blk
                    WHERE blk.snum EQ est-op.s-num
                    AND blk.bnum EQ est-op.b-num
                    NO-ERROR.
                ASSIGN
                    blk.lab  = blk.lab  + optot$
                    blk.cost = blk.cost + optot$.

                FIND FIRST xjob
                    WHERE xjob.i-no EQ blk.id
                    AND xjob.qty  EQ blk.qreq.
                /*
                              and xjob.qty  eq qtty[vmcl].
                */
                ASSIGN
                    xjob.lab = xjob.lab + v-lab-fac[1] + v-lab-fac[2]
                    xjob.voh = xjob.voh + v-var-fac[1] + v-var-fac[2]
                    xjob.foh = xjob.foh + v-fix-fac[1] + v-fix-fac[2].
            END.

            ELSE
                FOR EACH blk WHERE blk.snum EQ est-op.s-num:
                    ASSIGN
                        blk.lab  = blk.lab  + (optot$ * blk.pct)
                        blk.cost = blk.cost + (optot$ * blk.pct).
                    FIND FIRST xjob
                        WHERE xjob.i-no EQ blk.id
                        AND xjob.qty  EQ blk.qreq.
                    /*
                                  and xjob.qty  eq qtty[vmcl].
                    */
                    ASSIGN
                        xjob.lab = xjob.lab + ((v-lab-fac[1] + v-lab-fac[2]) * blk.pct)
                        xjob.voh = xjob.voh + ((v-var-fac[1] + v-var-fac[2]) * blk.pct)
                        xjob.foh = xjob.foh + ((v-fix-fac[1] + v-fix-fac[2]) * blk.pct).
                END.
        END.

        IF est-op.m-code NE "" THEN 
        DO:
            CREATE op.
            ASSIGN
                op.blank-no  = est-op.b-num
                op.dept      = est-op.dept
                op.form-no   = est-op.s-num
                op.i-no      = eb.stock-no
                op.line      = est-op.line
                op.m-code    = est-op.m-code
                op.mr-fixoh  = mach.mr-fixoh
                op.mr-hr     = opmr
                op.mr-rate   = mach.mr-rate
                op.mr-varoh  = mach.mr-varoh
                op.mr-waste  = est-op.op-waste
                op.pass      = est-op.op-pass
                op.run-hr    = oprun
                op.run-fixoh = mach.run-fixoh
                op.run-rate  = mach.run-rate
                op.run-varoh = mach.run-varoh
                op.wst-prct  = est-op.op-spoil
                op.rec_key   = fEstOpRecKey(est-op.rec_key)
                .

            IF mach.p-type EQ "P" THEN 
            DO:
                op.run-qty = est-op.num-sh * v-parts.
                {sys/inc/roundup.i op.run-qty}
            END.
            ELSE
                IF est-op.op-sb THEN
                    op.run-qty = est-op.num-sh * v-on-f.
                ELSE
                    op.run-qty = est-op.num-sh * v-num-up * v-n-out.
            
            /*Run Qty Divisor 24462 (also undoes 19774)*/
            IF est-op.n_out_div GT 0 THEN 
                op.run-qty = op.run-qty / est-op.n_out_div.
            
            op.speed = IF ll-unitize THEN (op.run-qty / oprun)
            ELSE opsp.

            v-yld = IF eb.quantityPerSet LT 0 THEN -1 / eb.quantityPerSet ELSE eb.quantityPerSet.

            IF vmclean THEN oprun$ = oprun$ / ((qtty[vmcl] * v-yld) / 1000).

            DISPLAY STRING(est-op.s-num,"99") + "-" +
                string(est-op.b-num,"9") FORMAT "x(4)"
                est-op.m-dscr     FORMAT "x(13)"
                est-op.n-out      FORMAT ">>>9"
                v-on-s / v-on-f 
                WHEN est-op.n-out EQ 0 @ est-op.n-out
                1 
                WHEN NOT est-op.op-sb                @ est-op.n-out
                opmr              FORMAT ">>9.99"
                oprun             FORMAT ">>9.99"      TO 36
                opsp              FORMAT ">>>>9"       TO 42
                est-op.op-rate[2] FORMAT ">>>9.99"     TO 51
                opmr$             FORMAT ">>>>9.99"    TO 60
                oprun$            FORMAT ">>>>9.99"    TO 69
                optot$            FORMAT ">>>>>>9.99" TO 80 SKIP WITH STREAM-IO.

            IF vmclean THEN oprun$ = oprun$ * ((qtty[vmcl] * v-yld) / 1000).

            IF AVAILABLE itemfg AND est-op.b-num NE 0 THEN op.i-name = itemfg.i-name.
            IF op.line GT 500 THEN op.line = op.line - 500.
        END.

        ASSIGN
            op-tot[1] = op-tot[1] + opmr
            op-tot[2] = op-tot[2] + oprun
            op-tot[3] = op-tot[3] + opmr$
            op-tot[4] = op-tot[4] + oprun$
            op-tot[5] = op-tot[5] + optot$
            op-tot[8] = op-tot[8] + opmr * est-op.op-crew[1]
            op-tot[9] = op-tot[9] + oprun * est-op.op-crew[2]
            dTotalManHrs = dTotalManHrs + opmr * est-op.op-crew[1] + oprun * est-op.op-crew[2]
            .
        r-spoil = r-spoil + (est-op.num-sh * (est-op.op-spoil / 100)).

        ASSIGN
            op-tot[6] = op-tot[6] + v-var-fac[1] + v-var-fac[2]
            op-tot[7] = op-tot[7] + v-fix-fac[1] + v-fix-fac[2].
    END.
END.

ctrl2[2] = 0.

FOR EACH blk WHERE blk.snum EQ v-form-no OR NOT vmclean2:
    FIND FIRST xjob
        WHERE xjob.i-no EQ blk.id
        AND xjob.qty  EQ blk.qreq.

    ld-hand-pct = IF blk.pur-man THEN hand-pct-f ELSE ctrl[2].

    IF ld-hand-pct NE 0 THEN
        ASSIGN
            v-rm$    = xjob.mat * ld-hand-pct
            blk.cost = blk.cost + v-rm$
            blk.lab  = blk.lab  + v-rm$
            ctrl2[2] = ctrl2[2] + v-rm$
            xjob.lab = xjob.lab + v-rm$.
END.

IF ctrl2[2] + ctrl2[3] NE 0 THEN 
DO:
    PUT "Raw Mat'l Handling" (ctrl2[2] + ctrl2[3]) TO 80 SKIP.
    op-tot[5] = op-tot[5] + (ctrl2[2] + ctrl2[3]).
END.

fr-tot = 0.

FOR EACH xef
    WHERE xef.company = xest.company
    AND xef.est-no EQ xest.est-no
    AND (xef.form-no EQ v-form-no OR (NOT vmclean2)):

    RELEASE item.
    RELEASE e-item.

    IF xef.form-no EQ 1 THEN 
    DO:
        FIND FIRST xeb OF xef NO-LOCK.
        FIND FIRST item
        {sys/look/itemW.i}
          AND item.i-no EQ xeb.tr-no
            NO-LOCK NO-ERROR.
        IF AVAILABLE item THEN
            FIND FIRST e-item OF item
                NO-LOCK NO-ERROR.
    END.

    IF AVAILABLE e-item AND item.mat-type EQ "Z" THEN 
    DO:
        FIND FIRST xeb
            WHERE xeb.company  EQ xest.company
            AND xeb.est-no   EQ xest.est-no
            AND xeb.form-no  EQ 0
            AND xeb.blank-no EQ 0
            NO-LOCK.

        ASSIGN
            v-yld  = IF xeb.quantityPerSet LT 0 THEN -1 / xeb.quantityPerSet ELSE xeb.quantityPerSet
            tr-tot = ((xeb.len * xeb.wid * xeb.dep) *
               (qtty[vmcl] * v-yld)) /
               (item.case-l * item.case-w * item.case-d).

        EMPTY TEMP-TABLE tt-ei.
        CREATE tt-ei.
        DO j = 1 TO 10:
            ASSIGN
                tt-ei.run-qty[j]  = e-item.run-qty[j]
                tt-ei.run-cost[j] = e-item.run-cost[j].
        END.

        FIND FIRST b-qty WHERE
            b-qty.reftable = "blank-vend-qty" AND
            b-qty.company = e-item.company AND
            b-qty.CODE    = e-item.i-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE b-qty THEN
        DO:
            FIND FIRST b-cost WHERE
                b-cost.reftable = "blank-vend-cost" AND
                b-cost.company = e-item.company AND
                b-cost.CODE    = e-item.i-no
                NO-LOCK NO-ERROR.

            DO j = 1 TO 10:
                ASSIGN
                    tt-ei.run-qty[j + 10]  = b-qty.val[j]
                    tt-ei.run-cost[j + 10] = b-cost.val[j].
            END.
        END.


        DO j = 1 TO 20:
            IF tt-ei.run-qty[j] < tr-tot THEN NEXT.
            fr-tot = ROUND(tr-tot * tt-ei.run-cost[j],2).
            LEAVE.
        END.
    END.

    ELSE
        FOR EACH xeb
            WHERE xeb.company EQ xest.company
            AND xeb.est-no  EQ xest.est-no
            AND xeb.form-no EQ xef.form-no
            NO-LOCK:
        
            v-yld  = IF xeb.quantityPerSet LT 0 THEN -1 / xeb.quantityPerSet ELSE xeb.quantityPerSet.
    
            FIND FIRST carrier
                WHERE carrier.company EQ cocode
                AND carrier.loc     EQ locode
                AND carrier.carrier EQ xeb.carrier
                NO-LOCK NO-ERROR.
            IF AVAILABLE carrier THEN
                FIND FIRST carr-mtx 
                    WHERE carr-mtx.company  EQ cocode
                    AND carr-mtx.loc      EQ locode
                    AND carr-mtx.carrier  EQ carrier.carrier
                    AND carr-mtx.del-zone EQ xeb.dest-code
                    AND carr-mtx.del-zip EQ xeb.ship-zip
                    NO-LOCK NO-ERROR.
            IF NOT AVAILABLE carr-mtx THEN 
                FIND FIRST carr-mtx
                    WHERE carr-mtx.company  EQ cocode
                    AND carr-mtx.loc      EQ locode
                    AND carr-mtx.carrier  EQ carrier.carrier
                    AND carr-mtx.del-zone EQ xeb.dest-code
                    NO-LOCK NO-ERROR.
            FIND FIRST car WHERE car.id EQ xeb.part-no NO-ERROR.
            IF NOT AVAILABLE car THEN 
            DO:
                CREATE car.
                ASSIGN
                    car.carrier = carrier.carrier
                    car.dscr    = carr-mtx.del-zone
                    car.id      = xeb.part-no
                    car.snum    = xeb.form-no
                    car.bnum    = xeb.blank-no.
            END.
            v-msf = (IF v-corr THEN
                ((xeb.t-sqin - xeb.t-win) * qtty[vmcl] * v-yld * .007)
                ELSE
                ((xeb.t-sqin - xeb.t-win) * qtty[vmcl] * v-yld / 144))
                / 1000.
            /* now msf */
            zzz = xef.weight.
            IF xef.medium NE "" THEN 
            DO:
                FIND FIRST item
          {sys/look/itemW.i}
            AND item.i-no EQ xef.medium
                    NO-LOCK NO-ERROR.
                IF AVAILABLE item THEN
                    zzz = zzz + (item.basis-w * (1 - (item.shrink / 100))).
            END.
            IF xef.flute NE "" THEN 
            DO:
                FIND FIRST item
          {sys/look/itemW.i}
            AND item.i-no EQ xef.flute
                    NO-LOCK NO-ERROR.
                IF AVAILABLE item THEN
                    zzz = zzz + item.basis-w.
            END.
            /*if xef.lam-code ne "" then do:
              find first item
                  {sys/look/itemW.i}
                    and item.i-no eq xef.lam-code
                  no-lock no-error.
              if avail item then
                zzz = zzz + item.basis-w.
            end.*/
            ASSIGN
                car.qty = car.qty + (v-msf * zzz)    /* total weight */
                car.msf = car.msf + v-msf.

            /* add pallet & case for total weight */
            FOR EACH cas WHERE cas.id EQ xeb.part-no,
                FIRST item
                WHERE item.company EQ cocode
                AND item.i-no    EQ cas.ino
                AND index("DC",item.mat-type) GT 0
                NO-LOCK:
                car.qty = car.qty + (cas.qty * item.basis-w).
            END.
        END.

    IF CAN-FIND(FIRST cas WHERE cas.typ EQ 1 AND cas.snum EQ 0) THEN
        FOR EACH xeb
            WHERE xeb.company EQ xest.company
            AND xeb.est-no  EQ xest.est-no
            AND xeb.form-no EQ 0
            NO-LOCK:
 
            FIND FIRST car WHERE car.snum EQ 0 NO-ERROR.
            IF NOT AVAILABLE car THEN 
            DO:
                CREATE car.
                ASSIGN
                    car.carrier = carrier.carrier
                    car.dscr    = carr-mtx.del-zone
                    car.id      = xeb.part-no
                    car.snum    = xeb.form-no
                    car.bnum    = xeb.blank-no.
            END.

            FOR EACH xcar WHERE ROWID(xcar) NE ROWID(car):
                ASSIGN
                    car.qty = car.qty + xcar.qty
                    car.msf = car.msf + xcar.msf.
            END.

            LEAVE.
        END.
END.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
