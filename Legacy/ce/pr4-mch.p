/* --------------------------------------------------- ce/pr4-mch.p 02/98 JLF */
/* Machine Operations                                                         */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

DEFINE SHARED     BUFFER xest FOR est.
DEFINE SHARED     BUFFER xef  FOR ef.
DEFINE SHARED     BUFFER xeb  FOR eb.

DEFINE NEW SHARED BUFFER xop  FOR est-op.

DEFINE SHARED VARIABLE qty AS INTEGER NO-UNDO.

{ce/print4.i shared shared}

DEFINE VARIABLE v-on-f        AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-factor      AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-lab-fac     AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-fix-fac     AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-var-fac     AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-num-up      AS INTEGER NO-UNDO.
DEFINE VARIABLE v-len         LIKE xef.gsh-len NO-UNDO.
DEFINE VARIABLE v-gu-out      AS INTEGER INIT 1 NO-UNDO.
DEFINE VARIABLE v-printed-lit AS LOG     NO-UNDO.

FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.

{sys/inc/ceprice.i}
{ce/fEstOpRecKey.i}

ASSIGN
    opsplit$      = 0
    v-printed-lit = CAN-FIND(FIRST prodl WHERE prodl.company EQ cocode AND
                     prodl.prolin EQ 'Printed' AND
                     prodl.procat EQ xeb.procat).

/* machines */
FOR EACH est-op WHERE est-op.company = xest.company 
    AND est-op.est-no EQ xest.est-no
    AND est-op.qty   EQ v-op-qty
    AND est-op.line  GT 500
    BREAK BY est-op.s-num
    BY est-op.b-num
    BY est-op.d-seq
    BY est-op.op-pass
    WITH FRAME ae DOWN NO-LABELS NO-BOX:

    FIND FIRST mach
    {sys/ref/machW.i}
          AND mach.m-code EQ est-op.m-code
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE mach THEN NEXT.

    RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f).

    v-len = IF est-op.dept EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.

    IF est-op.op-speed NE 0 THEN
    DO:
        IF mach.therm AND (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
            oprun = (((est-op.num-sh * v-on-f * v-gu-out) - est-op.op-waste) *
                (v-len / 12)) / est-op.op-speed.
        ELSE
            IF est-op.op-sb THEN
                oprun = ((est-op.num-sh * v-on-f * v-gu-out) - est-op.op-waste) / est-op.op-speed.
            ELSE
                IF NOT v-printed-lit OR v-gu-out EQ 1 THEN
                    oprun = ((est-op.num-sh * 
                        xeb.num-up * (IF xef.n-out   EQ 0 THEN 1 ELSE xef.n-out) *
                        (IF xef.n-out-l EQ 0 THEN 1 ELSE xef.n-out-l)) -
                        est-op.op-waste) / est-op.op-speed.
                ELSE
                    oprun = ((est-op.num-sh * v-gu-out) - est-op.op-waste) / est-op.op-speed.
    END.
    ELSE
        oprun = 0.
    
    /*Run Qty Divisor 24462 (also undoes 19774)*/
    IF est-op.n_out_div GT 0 THEN 
        oprun = oprun / est-op.n_out_div.
        
    IF v-printed-lit AND est-op.n-out GT 1 THEN
        v-gu-out = v-gu-out * est-op.n-out.

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

    IF est-op.m-code NE "" THEN 
    DO:
        FIND itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ xeb.stock-no
            NO-LOCK NO-ERROR.
        CREATE op.
        ASSIGN
            op.blank-no  = est-op.b-num
            op.dept      = est-op.dept
            op.form-no   = est-op.s-num
            op.i-no      = xeb.stock-no
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
        IF AVAILABLE itemfg AND est-op.b-num NE 0 THEN op.i-name = itemfg.i-name.
        IF op.line GE 500 THEN op.line = op.line - 500.
    END.
    
    /*Run Qty Divisor 24462 (also undoes 19774) REFACTOR - why isn't this done before oprun is calculated?*/
    IF est-op.n_out_div GT 0 THEN 
        op.run-qty = op.run-qty / est-op.n_out_div.
        
    IF est-op.m-code NE "" THEN
        DISPLAY est-op.m-dscr   FORMAT "x(18)"
            est-op.op-mr    FORMAT ">>>9.99"
            oprun           FORMAT ">>>>9.99"     TO 34
            est-op.op-speed FORMAT ">>>>>9"       TO 41
            est-op.op-rate[2] FORMAT ">>9.99"     TO 48 WHEN ce-ctrl.sho-labor
            opmr$          FORMAT ">>>>9.99"      TO 57
            oprun$          FORMAT ">>>>>>9.99"   TO 68
            optot$          FORMAT ">,>>>,>>9.99" TO 80 SKIP WITH STREAM-IO.

    ASSIGN
        op-tot[1] = op-tot[1] + est-op.op-mr
        op-tot[2] = op-tot[2] + oprun
        op-tot[3] = op-tot[3] + opmr$
        op-tot[4] = op-tot[4] + oprun$
        op-tot[5] = op-tot[5] + optot$
        op-tot[6] = op-tot[6] + v-var-fac[1] + v-var-fac[2]
        op-tot[7] = op-tot[7] + v-fix-fac[1] + v-fix-fac[2].
END.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
