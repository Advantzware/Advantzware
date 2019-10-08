/* ----------------------------------------------- ce/tan/pr4-mch.p 01/98 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

DEFINE SHARED BUFFER xest FOR est.
DEFINE SHARED BUFFER xef  FOR ef.
DEFINE SHARED BUFFER xeb  FOR eb.
DEFINE SHARED BUFFER xop  FOR est-op.

{ce/print4.i shared shared}

DEFINE VARIABLE v-on-f    AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-factor  AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-lab-fac AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-fix-fac AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-var-fac AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-num-up  AS INTEGER NO-UNDO.
DEFINE VARIABLE v-len     LIKE xef.gsh-len NO-UNDO.

DEFINE BUFFER bf-eb FOR eb.


{sys/inc/ceprice.i}
{ce/fEstOpRecKey.i}

opsplit$ = 0.

FOR EACH est-op WHERE est-op.company = xest.company
    AND est-op.est-no = xest.est-no AND est-op.line GT 500
    BY est-op.d-seq BY est-op.b-num BY est-op.op-pass
    WITH FRAME ae DOWN NO-LABELS NO-BOX:
    FIND FIRST mach {sys/look/machW.i} AND mach.m-code = est-op.m-code
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE mach THEN NEXT.

    RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f).

    v-len = IF est-op.dept EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.

    IF est-op.op-speed NE 0 THEN
        IF mach.therm AND (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
            oprun = (((est-op.num-sh * v-on-f) - est-op.op-waste) *
                (v-len / 12)) / est-op.op-speed.
        ELSE IF est-op.op-sb = TRUE THEN
                oprun = ((est-op.num-sh * v-on-f) - est-op.op-waste) / est-op.op-speed.
            ELSE
                oprun = ((est-op.num-sh *
                    xeb.num-up * (IF xef.n-out   EQ 0 THEN 1 ELSE xef.n-out) *
                    (IF xef.n-out-l EQ 0 THEN 1 ELSE xef.n-out-l)) -
                    est-op.op-waste) / est-op.op-speed.
    ELSE oprun = 0.
    
    /*Run Qty Divisor 24462 (also undoes 19774)*/
    IF est-op.n_out_div GT 0 THEN 
        oprun = oprun / est-op.n_out_div.
        
    ASSIGN
        opmr$        = est-op.op-mr * est-op.op-rate[1]
        oprun$       = oprun        * est-op.op-rate[2]
        optot$       = opmr$ + oprun$

        v-lab-fac[1] = est-op.op-mr * est-op.op-crew[1] * mach.lab-rate[lab-drate]
        v-lab-fac[2] = oprun        * est-op.op-crew[2] * mach.lab-rate[lab-drate]

        v-var-fac[1] = est-op.op-mr * mach.mr-varoh
        v-var-fac[2] = oprun        * mach.run-varoh

        v-fix-fac[1] = est-op.op-mr * mach.mr-fixoh
        v-fix-fac[2] = oprun        * mach.run-fixoh.

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

    FIND FIRST bf-eb WHERE bf-eb.company = xest.company
        AND bf-eb.est-no    = xest.est-no   AND
        bf-eb.form-no  = est-op.s-num AND
        bf-eb.blank-no = est-op.b-num NO-LOCK NO-ERROR.
    CREATE op.
    ASSIGN 
        op.blank-no  = est-op.b-num
        op.dept      = est-op.dept
        op.form-no   = est-op.s-num
        op.i-no      = IF AVAILABLE bf-eb THEN bf-eb.stock-no ELSE ""
        op.line      = est-op.line
        op.m-code    = est-op.m-code
        op.mr-fixoh  = mach.mr-fixoh
        op.mr-hr     = est-op.op-mr
        op.mr-rate   = mach.mr-rate
        op.mr-varoh  = mach.mr-varoh
        op.mr-waste  = est-op.op-waste
        op.pass      = est-op.op-pass
        op.run-hr    = oprun
        op.run-fixoh = mach.run-fixoh
        op.run-rate  = mach.run-rate
        op.run-varoh = mach.run-varoh
        op.wst-prct  = est-op.op-spoil
        op.speed     = est-op.op-speed
        op.rec_key   = fEstOpRecKey(est-op.rec_key)
        .
    IF est-op.op-sb THEN
        op.run-qty = est-op.num-sh * v-on-f.
    ELSE
        op.run-qty = est-op.num-sh * xeb.num-up *
            (IF xef.n-out   EQ 0 THEN 1 ELSE xef.n-out) *
            (IF xef.n-out-l EQ 0 THEN 1 ELSE xef.n-out-l).
    
    /*Run Qty Divisor 24462 (also undoes 19774)*/
    IF est-op.n_out_div GT 0 THEN 
        op.run-qty = op.run-qty / est-op.n_out_div.
    
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ op.i-no
        NO-LOCK NO-ERROR.
    IF AVAILABLE itemfg AND est-op.b-num NE 0 THEN
        op.i-name = itemfg.i-name.
    IF op.line GE 500 THEN
        op.line = op.line - 500.

    IF est-op.m-code NE "" THEN
        DISPLAY
            STRING(est-op.b-num,"99") FORMAT "x(2)" WHEN est-op.b-num NE 0
            est-op.m-dscr   FORMAT "x(17)"
            est-op.op-mr    FORMAT ">>9.99"
            oprun           FORMAT ">>>9.99"     TO 35
            est-op.op-speed FORMAT ">>>>>9"       TO 42
            est-op.op-rate[2] FORMAT ">>9.99"      TO 49
            opmr$           FORMAT ">>>>9.99"    TO 58
            oprun$          FORMAT ">>>>>9.99"   TO 68
            optot$          FORMAT ">,>>>,>>9.99" TO 80 SKIP WITH STREAM-IO.

    op-tot[1] = op-tot[1] + est-op.op-mr.
    op-tot[2] = op-tot[2] + oprun.
    op-tot[3] = op-tot[3] + opmr$.
    op-tot[4] = op-tot[4] + oprun$.
    op-tot[5] = op-tot[5] + optot$.

    ASSIGN
        op-tot[6] = op-tot[6] + v-var-fac[1] + v-var-fac[2]
        op-tot[7] = op-tot[7] + v-fix-fac[1] + v-fix-fac[2]
        r-spoil   = r-spoil + (est-op.num-sh * (est-op.op-spoil / 100)).
END.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
