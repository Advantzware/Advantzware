/* -------------------------------------------------- cec/pr4-mch.p 03/97 JLF */
DEFINE SHARED VARIABLE cocode AS cha     NO-UNDO.
DEFINE SHARED VARIABLE locode AS cha     NO-UNDO.
DEFINE SHARED VARIABLE k      AS INTEGER NO-UNDO.
DEFINE        VARIABLE xxx    AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE qty    AS INTEGER NO-UNDO .
DEFINE        VARIABLE zzz    AS INTEGER NO-UNDO.
DEFINE        VARIABLE i      AS INTEGER NO-UNDO.

DEFINE SHARED BUFFER xest FOR est.
DEFINE SHARED BUFFER xef  FOR ef.
DEFINE SHARED BUFFER xeb  FOR eb.

{cec/print4.i shared shared}
{cec/print42.i shared}

DEFINE VARIABLE v-on-f      AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-factor    AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-lab-fac   AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-fix-fac   AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-var-fac   AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-num-up    AS INTEGER NO-UNDO.
DEFINE VARIABLE v-msf       AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-on-s      AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-len       LIKE xef.gsh-len NO-UNDO.
DEFINE VARIABLE ll-unitize  AS LOG     NO-UNDO.
DEFINE VARIABLE v-n-out     AS INTEGER NO-UNDO.
DEFINE VARIABLE li-die      AS INTEGER NO-UNDO.
DEFINE VARIABLE ld-fg-rate  AS DECIMAL NO-UNDO.
DEFINE VARIABLE ld-hand-pct AS DECIMAL NO-UNDO.
DEFINE VARIABLE opmr        LIKE est-op.op-mr NO-UNDO.
DEFINE VARIABLE opsp        AS DECIMAL NO-UNDO.

DEFINE SHARED WORKFILE w-form
    FIELD form-no LIKE xef.form-no
    FIELD min-msf AS   LOG.

{cec/msfcalc.i}

{cec/rollfac.i}

{sys/inc/ceprice.i}
{ce/fEstOpRecKey.i}

opsplit$ = 0.

FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.

ASSIGN
    v-fac-hdr = IF vmclean THEN
                  ("Run$/M" + IF v-rollfac THEN "SF" ELSE "")
                ELSE "Run $"
    v-fac-hdr = FILL(" ",8 - length(TRIM(v-fac-hdr))) + trim(v-fac-hdr).

RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).
v-on-s = xeb.num-up * v-n-out.

PUT SKIP(1) "Machine Desc     Out     SU     Run Speed    Rate    SU $"
    SPACE(3) v-fac-hdr FORMAT "x(8)" SPACE(2) "Total Cost".

/* machines */
FOR EACH est-op WHERE est-op.company = xest.company 
    AND est-op.est-no  = xest.est-no
    AND est-op.qty   EQ v-op-qty 
    AND est-op.line GT 500
    BY est-op.b-num BY est-op.d-seq BY est-op.op-pass
    WITH FRAME ae DOWN NO-LABELS NO-BOX:

    FIND FIRST w-form WHERE w-form.form-no EQ est-op.s-num NO-ERROR.
    IF NOT AVAILABLE w-form THEN 
    DO:
        CREATE w-form.
        w-form.form-no = est-op.s-num.
    END.

    FIND FIRST mach
    {sys/ref/machW.i}
            AND mach.m-code EQ est-op.m-code
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE mach THEN NEXT.

    RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f).
      
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
      
    v-len = IF est-op.dept EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.

    ll-unitize = NO.
    FOR EACH mstd
        WHERE mstd.company EQ mach.company
        AND mstd.loc     EQ mach.loc
        AND mstd.m-code  EQ mach.m-code
        NO-LOCK
        BREAK BY mstd.style DESCENDING:
        IF LAST(mstd.style)        OR
            mstd.style EQ xeb.style THEN 
        DO:
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
                    oprun = qty / opsp.
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
    END.

    IF est-op.m-code NE "" THEN 
    DO:
        CREATE op.
        ASSIGN 
            op.blank-no  = est-op.b-num
            op.dept      = est-op.dept
            op.form-no   = est-op.s-num
            op.i-no      = xeb.stock-no
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

        IF est-op.op-sb THEN
            op.run-qty = est-op.num-sh * v-on-f.
        ELSE
            op.run-qty = est-op.num-sh * xeb.num-up * v-n-out.
        
        /*Run Qty Divisor 24462 (also undoes 19774) - REFACTOR why is op.run-qty not determined before oprun*/
        IF est-op.n_out_div GT 0 THEN 
            op.run-qty = op.run-qty / est-op.n_out_div.       
        
        op.speed = IF ll-unitize THEN (op.run-qty / oprun)
        ELSE opsp.

        IF vmclean THEN oprun$ = oprun$ /
                (qtty[k] * xeb.quantityPerSet / 1000 * v-sqft-fac).
        
        /* task 09171202 - Reversed with Ticket 26136 */
/*        IF est-op.dept EQ "DC" AND est-op.n-out EQ 0 THEN*/
/*            v-on-s = xeb.num-up.                         */

        DISPLAY est-op.m-dscr           FORMAT "x(16)"
            est-op.n-out            FORMAT ">>>9"
                v-on-s / v-on-f         WHEN est-op.n-out EQ 0 @ est-op.n-out
                1                       WHEN NOT est-op.op-sb  @ est-op.n-out
                1                       WHEN mach.p-type = "A" @ est-op.n-out
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
                (qtty[k] * xeb.quantityPerSet / 1000 * v-sqft-fac).

        IF AVAILABLE itemfg AND est-op.b-num NE 0 THEN op.i-name = itemfg.i-name.
        IF op.line GT 500 THEN op.line = op.line - 500.
    END.

    ASSIGN
        op-tot[1] = op-tot[1] + opmr
        op-tot[2] = op-tot[2] + oprun
        op-tot[3] = op-tot[3] + opmr$
        op-tot[4] = op-tot[4] + oprun$
        op-tot[5] = op-tot[5] + optot$
        op-tot[6] = op-tot[6] + v-var-fac[1] + v-var-fac[2]
        op-tot[7] = op-tot[7] + v-fix-fac[1] + v-fix-fac[2]
        op-tot[8] = op-tot[8] + opmr * est-op.op-crew[1]
        op-tot[9] = op-tot[9] + oprun * est-op.op-crew[2].
END.

ASSIGN
    ctrl2[2]    = 0
    ld-hand-pct = IF xeb.pur-man THEN hand-pct-f ELSE ctrl[2].

IF ld-hand-pct NE 0 THEN
    ctrl2[2] = ctrl2[2] + ((dm-tot[5] + tprep-mat + mis-tot[1]) * ld-hand-pct).

IF ctrl2[2] + ctrl2[3] NE 0 THEN 
DO:
    PUT
        "Raw Mat'l Handling" (ctrl2[2] + ctrl2[3]) TO 80 SKIP.
    ASSIGN
        opsplit$[1] = opsplit$[1] + ctrl2[2] + ctrl2[3]
        op-tot[5]   = op-tot[5] + (ctrl2[2] + ctrl2[3]).
END.
   
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

ASSIGN
    v-msf = (IF v-corr THEN ((xeb.t-sqin - xeb.t-win) * qty * .007)
                       ELSE ((xeb.t-sqin - xeb.t-win) * qty / 144)) / 1000
    zzz   = xef.weight.
   
IF xef.medium NE "" THEN 
DO:
    FIND FIRST item {sys/look/itemW.i} AND
                 item.i-no = xef.medium NO-LOCK NO-ERROR.
    IF AVAILABLE item
        THEN zzz = zzz + (item.basis-w * (1 - (item.shrink / 100))).
END.
   
IF xef.flute NE "" THEN 
DO:
    FIND FIRST item {sys/look/itemW.i} AND
                 item.i-no = xef.flute NO-LOCK NO-ERROR.
    IF AVAILABLE item
        THEN zzz = zzz + item.basis-w.
END.
   
/*if xef.lam-code ne "" then do:
   find first item {sys/look/itemW.i} and
              item.i-no = xef.lam-code no-lock no-error.
   if avail item
   then zzz = zzz + item.basis-w.
end.*/
   
xxx = v-msf * zzz.                           /* total weight */
   
FIND FIRST item                              /* add cases */
    WHERE item.company EQ cocode
    AND item.i-no    EQ xeb.cas-no
    AND xeb.cas-no   NE ""
    NO-LOCK NO-ERROR.
xxx = xxx + (c-qty * (IF AVAILABLE item THEN item.basis-w
ELSE ce-ctrl.def-cas-w)).
                                       
FIND FIRST item                              /* add pallets */
    WHERE item.company EQ cocode
    AND item.i-no    EQ xeb.tr-no
    AND xeb.tr-no    NE ""
    NO-LOCK NO-ERROR.

ASSIGN
    xxx    = xxx + (p-qty * (IF AVAILABLE item THEN item.basis-w
                                       ELSE ce-ctrl.def-pal-w))
    fr-tot = 0.
   
IF xeb.fr-out-c NE 0 THEN
    fr-tot = xeb.fr-out-c * (xxx / 100).
ELSE
    IF xeb.fr-out-m NE 0 THEN
        fr-tot = xeb.fr-out-m * (qtty[k] / 1000).
    ELSE
        IF AVAILABLE carr-mtx THEN 
        DO:
            IF carrier.chg-method EQ "W" THEN
            DO i = 1 TO 10:
                fr-tot = carr-mtx.rate[i] * xxx / 100.
                IF carr-mtx.weight[i] GE xxx THEN LEAVE.
            END.
     
            ELSE
                IF carrier.chg-method EQ "P" THEN
                DO i = 1 TO 10:
                    fr-tot = carr-mtx.rate[i] * p-qty.
                    IF carr-mtx.weight[i] GE p-qty THEN LEAVE.
                END.
     
                ELSE 
                DO:
                    FIND FIRST item
           {sys/look/itemW.i}
             AND item.i-no     EQ xef.board
             AND item.mat-type EQ "B"
             AND item.avg-w    GT 0
                        NO-LOCK NO-ERROR.
                    v-msf = v-msf * IF AVAILABLE item THEN item.avg-w ELSE 1.
      
                    DO i = 1 TO 10:
                        fr-tot = carr-mtx.rate[i] * v-msf.
                        IF carr-mtx.weight[i] GE v-msf THEN LEAVE.
                    END.
                END.
     
            IF fr-tot LT carr-mtx.min-rate THEN fr-tot = carr-mtx.min-rate.
      
            fr-tot = fr-tot + (carr-mtx.min-rate * (rels[k] - 1)).
        END.

ld-fg-rate = IF xeb.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate.

IF fg-wt / 100 * ld-fg-rate NE 0 THEN 
DO:
    PUT "Finished Goods Handling" fg-wt / 100 * ld-fg-rate TO 80 SKIP.
    ASSIGN
        opsplit$[1] = opsplit$[1] + (fg-wt / 100 * ld-fg-rate)
        op-tot[5]   = op-tot[5] + (fg-wt / 100 * ld-fg-rate).
END.

IF vmclean THEN op-tot[4] = op-tot[4] /
        (qtty[k] * xeb.quantityPerSet / 1000 * v-sqft-fac).

PUT "TOTAL  OPERATIONS        "
    op-tot[3]                FORMAT ">>>>9.99"    TO 57
    op-tot[4]                FORMAT ">>>>>>9.99"  TO 68
    op-tot[5]                FORMAT ">>>>,>>9.99" TO 80 SKIP(1).

IF vmclean THEN op-tot[4] = op-tot[4] *
        (qtty[k] * xeb.quantityPerSet / 1000 * v-sqft-fac).

/* end ---------------------------------- copr. 1997  advanced software, inc. */
