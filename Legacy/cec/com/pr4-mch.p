/* ----------------------------------------------- ce/com/pr4-mch.p 10/98 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

DEFINE SHARED BUFFER xest FOR est.
DEFINE SHARED BUFFER xef  FOR ef.
DEFINE SHARED BUFFER xeb  FOR eb.
DEFINE SHARED BUFFER xop  FOR est-op.

{cec/print4.i shared shared}

DEFINE        VARIABLE v-on-f    AS DECIMAL NO-UNDO.
DEFINE        VARIABLE v-factor  AS DECIMAL NO-UNDO.
DEFINE        VARIABLE v-lab-fac AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE        VARIABLE v-fix-fac AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE        VARIABLE v-var-fac AS DECIMAL EXTENT 2 NO-UNDO.
DEFINE        VARIABLE v-num-up  AS INTEGER NO-UNDO.
DEFINE        VARIABLE v-len     LIKE xef.gsh-len NO-UNDO.
DEFINE        VARIABLE opmr      LIKE est-op.op-mr NO-UNDO.
DEFINE        VARIABLE opsp      AS DECIMAL NO-UNDO.

DEFINE SHARED VARIABLE v-summ    AS LOG     NO-UNDO.

DEFINE TEMP-TABLE w-op NO-UNDO
    FIELD m-code LIKE op.m-code
    FIELD mr-hr  LIKE op.mr-hr
    FIELD run-hr LIKE op.run-hr
    FIELD units  LIKE op.run-qty
    FIELD speed  AS DECIMAL
    FIELD rec-id AS RECID.

DEFINE BUFFER bf-eb FOR eb.


{sys/inc/ceprice.i}
{ce/fEstOpRecKey.i}

opsplit$ = 0.

FOR EACH op:
    DELETE op.
END.

FOR EACH est-op WHERE est-op.company = xest.company
    AND est-op.est-no EQ xest.est-no
    AND est-op.line GT 500,
      
    FIRST mach
    {sys/look/machW.i}
      AND mach.m-code EQ est-op.m-code
    NO-LOCK,

    FIRST xef
    WHERE xef.company = xest.company
    AND xef.est-no   EQ est-op.est-no
    AND xef.form-no EQ est-op.s-num
    NO-LOCK
    
    BREAK BY est-op.s-num
    BY est-op.b-num
    BY est-op.d-seq
    BY est-op.op-pass:

    FIND FIRST bf-eb
        WHERE bf-eb.company   EQ est-op.company
        AND bf-eb.est-no    EQ est-op.est-no
        AND bf-eb.form-no   EQ est-op.s-num
        AND (bf-eb.blank-no EQ est-op.b-num OR est-op.b-num EQ 0)
        NO-LOCK NO-ERROR.
      
    RUN sys/inc/numup.p (xef.company,xef.est-no, xef.form-no, OUTPUT v-num-up).
  
    RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f).

    v-len = IF est-op.dept EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.

    ASSIGN
        opsp = est-op.op-speed
        opmr = est-op.op-mr.
  
    IF opsp GT 0 THEN 
    DO:
        IF AVAILABLE mach THEN 
        DO:
            IF mach.therm AND (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
                oprun = ((est-op.num-sh * v-on-f) - est-op.op-waste) *
                    (v-len / 12).
            ELSE
                IF est-op.op-sb THEN
                    oprun = (est-op.num-sh * v-on-f) - est-op.op-waste.
                ELSE
                    oprun = (est-op.num-sh * v-num-up * v-on-f) - est-op.op-waste.

            oprun = oprun / opsp.
        END.       
    END.

    ELSE oprun = 0.

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
        IF AVAILABLE blk THEN 
        DO:
            FIND FIRST xjob
                WHERE xjob.i-no     EQ blk.id
                AND xjob.form-no  EQ blk.snum
                AND xjob.blank-no EQ blk.bnum
                NO-ERROR.
            blk.lab  = blk.lab  + optot$. 
            blk.cost = blk.cost + optot$.
            ASSIGN
                xjob.lab = xjob.lab +
                  (opmr * (est-op.op-crew[1] * mach.lab-rate[lab-drate])) +
                   (oprun        * (est-op.op-crew[2] * mach.lab-rate[lab-drate]))
                xjob.voh = xjob.voh + v-var-fac[1] + v-var-fac[2]
                xjob.foh = xjob.foh + v-fix-fac[1] + v-fix-fac[2].
        END.
    END.
    ELSE
        FOR EACH blk WHERE blk.snum EQ est-op.s-num:
            ASSIGN
                blk.lab  = blk.lab  + (optot$ * blk.pct)
                blk.cost = blk.cost + (optot$ * blk.pct).
            FIND FIRST xjob
                WHERE xjob.i-no     EQ blk.id
                AND xjob.form-no  EQ blk.snum
                AND xjob.blank-no EQ blk.bnum
                NO-ERROR.

            ASSIGN
                xjob.lab = xjob.lab +
      (((opmr * (est-op.op-crew[1] * mach.lab-rate[lab-drate])) +
      (oprun        * (est-op.op-crew[2] * mach.lab-rate[lab-drate])))
        * blk.pct)
                xjob.voh = xjob.voh + ((v-var-fac[1] + v-var-fac[2]) * blk.pct)
                xjob.foh = xjob.foh + ((v-fix-fac[1] + v-fix-fac[2]) * blk.pct).
        END.

    FIND FIRST bf-eb
        WHERE bf-eb.company = xest.company
        AND bf-eb.est-no    EQ xest.est-no
        AND bf-eb.form-no  EQ est-op.s-num
        AND bf-eb.blank-no EQ est-op.b-num
        NO-LOCK NO-ERROR.

    IF est-op.m-code NE "" THEN 
    DO:
        CREATE op.
        ASSIGN
            op.blank-no  = est-op.b-num
            op.dept      = est-op.dept
            op.form-no   = est-op.s-num
            op.i-no      = IF est-op.b-num NE 0 THEN bf-eb.stock-no ELSE ""
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
            op.speed     = opsp
            op.rec_key   = fEstOpRecKey(est-op.rec_key)
            .

        IF est-op.op-sb THEN
            op.run-qty = est-op.num-sh * v-on-f.
        ELSE
            op.run-qty = est-op.num-sh * v-num-up *
                (IF xef.n-out   EQ 0 THEN 1 ELSE xef.n-out) *
                (IF xef.n-out-l EQ 0 THEN 1 ELSE xef.n-out-l).

        FIND FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ op.i-no
            NO-LOCK NO-ERROR.
        IF AVAILABLE itemfg AND est-op.b-num NE 0 THEN op.i-name = itemfg.i-name.
        IF op.line >= 500 THEN op.line = op.line - 500.
    END.

    ASSIGN
        op-tot[1] = op-tot[1] + opmr
        op-tot[2] = op-tot[2] + oprun
        op-tot[3] = op-tot[3] + opmr$
        op-tot[4] = op-tot[4] + oprun$
        op-tot[5] = op-tot[5] + optot$
        op-tot[6] = op-tot[6] + v-var-fac[1] + v-var-fac[2]
        op-tot[7] = op-tot[7] + v-fix-fac[1] + v-fix-fac[2]
        r-spoil   = r-spoil + (est-op.num-sh * (est-op.op-spoil / 100))
        op-tot[8] = op-tot[8] + opmr * est-op.op-crew[1]
        op-tot[9] = op-tot[9] + oprun * est-op.op-crew[2].
END.

FOR EACH op:
    RELEASE w-op.
    IF v-summ THEN
        FIND FIRST w-op WHERE w-op.m-code EQ op.m-code NO-LOCK NO-ERROR.

    IF NOT AVAILABLE w-op THEN 
    DO:
        CREATE w-op.
        ASSIGN
            w-op.m-code = op.m-code
            w-op.rec-id = RECID(op).
    END.

    ASSIGN
        w-op.mr-hr  = w-op.mr-hr  + op.mr-hr
        w-op.run-hr = w-op.run-hr + op.run-hr
        w-op.units  = w-op.units  + op.run-qty
        w-op.speed  = w-op.speed  + (op.speed * op.run-qty).
END.

FOR EACH op,

    FIRST est-op
    WHERE est-op.company = xest.company
    AND est-op.est-no  EQ xest.est-no
    AND est-op.m-code EQ op.m-code
    AND est-op.s-num  EQ op.form-no
    AND est-op.b-num  EQ op.blank-no
    AND est-op.line   GT 500
    NO-LOCK,

    FIRST mach
    {sys/look/machW.i}
      AND mach.m-code EQ est-op.m-code
    NO-LOCK,
    
    FIRST w-op WHERE w-op.rec-id EQ recid(op)

    BREAK BY op.form-no BY op.blank-no BY est-op.d-seq BY op.pass

    WITH FRAME ae DOWN NO-LABELS NO-BOX:

    ASSIGN
        opmr$  = w-op.mr-hr  * est-op.op-rate[1]
        oprun$ = w-op.run-hr * est-op.op-rate[2].

    IF opmr$  = ? THEN opmr$  = 0.
    IF oprun$ = ? THEN oprun$ = 0.

    optot$ = opmr$ + oprun$.

    IF v-min-mchg AND mach.mrk-rate GT optot$ THEN
        ASSIGN
            v-factor = (mach.mrk-rate / optot$)

            opmr$    = opmr$  * v-factor
            oprun$   = oprun$ * v-factor
            optot$   = mach.mrk-rate.

    DISPLAY STRING(est-op.s-num,">9") + "-" +
        string(est-op.b-num,"99")          FORMAT "x(5)"
        WHEN NOT v-summ
        est-op.m-dscr                      FORMAT "x(16)"
        w-op.mr-hr                         FORMAT ">>9.99"
        w-op.run-hr                TO 35   FORMAT ">>9.99"
        op.speed                   TO 42   FORMAT ">>>>9"
        WHEN NOT v-summ
        w-op.speed / w-op.units
        WHEN v-summ              @ op.speed
        est-op.op-rate[2]          TO 50   FORMAT ">>9.99"
        opmr$                      TO 59   FORMAT ">>>>9.99"
        oprun$                     TO 69   FORMAT ">>>>>9.99"
        optot$                     TO 80   FORMAT ">>>>,>>9.99"
        SKIP
        WITH STREAM-IO.
END.

/* end ---------------------------------- copr. 1998  advanced software, inc. */

