/* ---------------------------------------------- ce/box/pr42-mch.p 07/96 JLF */
/* Machine Operations for setup boxes                                         */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

DEFINE SHARED     BUFFER xest FOR est.
DEFINE SHARED     BUFFER xef  FOR ef.
DEFINE SHARED     BUFFER xeb  FOR eb.
DEFINE NEW SHARED BUFFER xop  FOR est-op.

{ce/print4.i shared shared}

DEFINE SHARED VARIABLE qty           AS INTEGER NO-UNDO.

DEFINE        VARIABLE v-on-f        AS DECIMAL NO-UNDO.
DEFINE        VARIABLE v-factor      AS DECIMAL NO-UNDO.
DEFINE        VARIABLE v-lab-fac     AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE        VARIABLE v-fix-fac     AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE        VARIABLE v-var-fac     AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE        VARIABLE v-num-up      AS INTEGER NO-UNDO.
DEFINE        VARIABLE v-len         LIKE xef.gsh-len NO-UNDO.
DEFINE        VARIABLE li-die        AS INTEGER NO-UNDO.
DEFINE        VARIABLE v-gu-out      AS INTEGER INIT 1 NO-UNDO.
DEFINE        VARIABLE v-printed-lit AS LOG     NO-UNDO.

{sys/inc/ceprice.i}
{ce/fEstOpRecKey.i}

ASSIGN
    opsplit$      = 0
    v-printed-lit = CAN-FIND(FIRST prodl WHERE prodl.company EQ cocode AND
                     prodl.prolin EQ 'Printed' AND
                     prodl.procat EQ xeb.procat).

FOR EACH est-op
    WHERE est-op.company EQ xest.company
    AND est-op.est-no  EQ xest.est-no
    AND est-op.line    GT 500,

    FIRST mach
    {sys/look/machW.i}
         AND mach.m-code EQ est-op.m-code
    NO-LOCK,

    FIRST xef
    WHERE xef.company EQ est-op.company
    AND xef.est-no  EQ est-op.est-no
    AND xef.form-no EQ est-op.s-num
    NO-LOCK

    BREAK BY est-op.s-num BY est-op.b-num BY est-op.d-seq BY est-op.op-pass
    WITH FRAME ae DOWN NO-LABELS NO-BOX:

    FIND FIRST xeb
        WHERE xeb.company  EQ est-op.company
        AND xeb.est-no   EQ est-op.est-no
        AND xeb.form-no  EQ est-op.s-num
        AND xeb.blank-no EQ est-op.b-num
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE xeb THEN
        FIND FIRST xeb
            WHERE xeb.company  EQ est-op.company
            AND xeb.est-no   EQ est-op.est-no
            AND xeb.form-no  EQ est-op.s-num
            NO-LOCK.

    RUN sys/inc/numup.p (xef.company, xef.est-no, xef.form-no, OUTPUT v-num-up).

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

    IF est-op.op-speed NE 0 THEN
    DO:
        IF mach.therm AND (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
            oprun = (((est-op.num-sh * v-on-f * v-gu-out) - est-op.op-waste) *
                (v-len / 12)) / est-op.op-speed.
        ELSE
            IF est-op.op-sb = TRUE THEN
                oprun = ((est-op.num-sh * v-on-f * v-gu-out) - est-op.op-waste) /
                    est-op.op-speed.
            ELSE
                IF NOT v-printed-lit OR v-gu-out EQ 1 THEN
                    oprun = ((est-op.num-sh *
                        v-num-up *
                        (IF xef.n-out   EQ 0 THEN 1 ELSE xef.n-out) *
                        (IF xef.n-out-l EQ 0 THEN 1 ELSE xef.n-out-l)) -
                        est-op.op-waste) / est-op.op-speed.
                ELSE
                    oprun = ((est-op.num-sh * v-gu-out) - est-op.op-waste) / est-op.op-speed.
    END.
    ELSE oprun = 0.

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

    IF est-op.b-num NE 0 THEN 
    DO:
        FIND FIRST blk WHERE blk.snum = est-op.s-num AND
            blk.bnum = est-op.b-num NO-ERROR.
        blk.lab  = blk.lab  + optot$. 
        blk.cost = blk.cost + optot$.
        FIND FIRST xjob
            WHERE xjob.i-no EQ blk.id
            AND xjob.qty  EQ qtty[vmcl].
        ASSIGN
            xjob.lab = xjob.lab +
	    (est-op.op-mr * (est-op.op-crew[1] * mach.lab-rate[lab-drate])) +
	    (oprun        * (est-op.op-crew[2] * mach.lab-rate[lab-drate]))
            xjob.voh = xjob.voh + v-var-fac[1] + v-var-fac[2]
            xjob.foh = xjob.foh + v-fix-fac[1] + v-fix-fac[2].
    END.
    ELSE
        FOR EACH blk WHERE blk.snum = est-op.s-num:
            ASSIGN
                blk.lab  = blk.lab  + (optot$ * blk.pct)
                blk.cost = blk.cost + (optot$ * blk.pct).
            FIND FIRST xjob
                WHERE xjob.i-no EQ blk.id
                AND xjob.qty  EQ qtty[vmcl].
            ASSIGN
                xjob.lab = xjob.lab +
	   (((est-op.op-mr * (est-op.op-crew[1] * mach.lab-rate[lab-drate]))  +
	     (oprun        * (est-op.op-crew[2] * mach.lab-rate[lab-drate])))
	    * blk.pct)
                xjob.voh = xjob.voh + ((v-var-fac[1] + v-var-fac[2]) * blk.pct)
                xjob.foh = xjob.foh + ((v-fix-fac[1] + v-fix-fac[2]) * blk.pct).
        END.

    IF est-op.m-code NE "" THEN 
    DO:
        FIND itemfg WHERE itemfg.company = cocode AND
            itemfg.i-no    = xeb.stock-no
            NO-LOCK NO-ERROR.
        CREATE OP.
        ASSIGN 
            OP.blank-no  = est-op.b-num
            OP.dept      = est-op.dept
            OP.form-no   = est-op.s-num
            OP.i-no      = xeb.stock-no
            OP.line      = est-op.line
            OP.m-code    = est-op.m-code
            OP.mr-fixoh  = mach.mr-fixoh
            OP.mr-hr     = est-op.op-mr
            OP.mr-rate   = mach.mr-rate
            OP.mr-varoh  = mach.mr-varoh
            OP.mr-waste  = est-op.op-waste
            OP.pass      = est-op.op-pass
            OP.run-hr    = oprun
            op.run-fixoh = mach.run-fixoh
            OP.run-rate  = mach.run-rate
            OP.run-varoh = mach.run-varoh
            OP.wst-prct  = est-op.op-spoil
            OP.speed     = est-op.op-speed
            op.rec_key   = fEstOpRecKey(est-op.rec_key)
            .
        IF est-op.op-sb THEN
            OP.run-qty = est-op.num-sh * v-on-f.
        /*	 else*/
        OP.run-qty = est-op.num-sh * v-num-up *
            (IF xef.n-out   EQ 0 THEN 1 ELSE xef.n-out) *
            (IF xef.n-out-l EQ 0 THEN 1 ELSE xef.n-out-l).
        IF AVAILABLE itemfg AND est-op.b-num <> 0 THEN
            OP.i-name = itemfg.i-name.
        IF OP.line >= 500 THEN
            OP.line = OP.line - 500.
    END.

    IF est-op.m-code NE "" THEN DISPLAY
            STRING(est-op.s-num,"9") + "-" +
            string(est-op.b-num,"99") FORMAT "x(4)"
            est-op.m-dscr   FORMAT "x(17)"
            est-op.op-mr    FORMAT ">>9.99"
            oprun           FORMAT ">>9.99"  TO 35
            est-op.op-speed FORMAT ">>>>>9"   TO 42
            est-op.op-rate[2] FORMAT ">>9.99"  TO 50
            opmr$           FORMAT ">>>>9.99" TO 59
            oprun$          FORMAT ">>>>>9.99" TO 69
            optot$          FORMAT ">>>>,>>9.99" TO 80 SKIP WITH STREAM-IO.

    ASSIGN
        op-tot[1] = op-tot[1] + est-op.op-mr
        op-tot[2] = op-tot[2] + oprun
        op-tot[3] = op-tot[3] + opmr$
        op-tot[4] = op-tot[4] + oprun$
        op-tot[5] = op-tot[5] + optot$
        r-spoil   = r-spoil + (est-op.num-sh * (est-op.op-spoil / 100))
        op-tot[6] = op-tot[6] + v-var-fac[1] + v-var-fac[2]
        op-tot[7] = op-tot[7] + v-fix-fac[1] + v-fix-fac[2].
END.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
