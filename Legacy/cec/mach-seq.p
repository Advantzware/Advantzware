/* -------------------------------------------------- cec/mach-seq.p 4/92 cd  */
/* create machine routing sequence                                            */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER ipiForm AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER ipiQty LIKE est-op.qty NO-UNDO.
DEFINE INPUT PARAMETER iplBuildCombo AS LOG NO-UNDO.

{sys/inc/var.i shared}

DEFINE VARIABLE riTempRec AS RECID.

DEFINE SHARED BUFFER xest FOR est.
DEFINE SHARED BUFFER xef  FOR ef.
DEFINE SHARED BUFFER xeb  FOR eb.

DEFINE NEW SHARED VARIABLE qty     AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE maxco   AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-2     AS LOG     INIT NO.
DEFINE NEW SHARED VARIABLE ll-corr AS LOG     NO-UNDO.

DEFINE SHARED     VARIABLE xcal    AS de      NO-UNDO.
DEFINE SHARED     VARIABLE sh-wid  AS de      NO-UNDO.
DEFINE SHARED     VARIABLE sh-len  AS de      NO-UNDO.

{ce/mach-lst.i new}

DEFINE TEMP-TABLE w-routing NO-UNDO 
    FIELD m-code LIKE mach.m-code.

{est/d-machex.i NEW}
{est/d-machex2.i NEW}

{sys/inc/ceroute1a.i}

DEFINE VARIABLE iIndex                AS INTEGER   NO-UNDO.
DEFINE VARIABLE iIndexAlt               AS INTEGER   NO-UNDO.
DEFINE VARIABLE dMSF            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-run            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-on-f           AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-rc-sw          AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE v-def-r          AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE v-defr-valid     AS LOG       INIT YES.
DEFINE VARIABLE v-yld            AS DECIMAL.
DEFINE VARIABLE ll-foam          AS LOG       NO-UNDO.
DEFINE VARIABLE v-gsh-wid        LIKE xef.gsh-len NO-UNDO.
DEFINE VARIABLE v-gsh-len        LIKE xef.gsh-len NO-UNDO.
DEFINE VARIABLE ll-label         AS LOG       NO-UNDO.
DEFINE VARIABLE op-farmout       AS LOG       NO-UNDO.
DEFINE VARIABLE op-valid-mach    AS LOG       NO-UNDO.


DEFINE BUFFER m2-lst FOR m-lst.



&SCOPED-DEFINE where-machine                                                     ~
               WHERE mach.company EQ cocode                                  ~
                 AND mach.obsolete EQ YES                                       



SESSION:SET-WAIT-STATE("general").

{cec/msfcalc.i}
/*{sys/inc/cepanel.i}*/

{sys/inc/cercrout.i}

ASSIGN
    qty      = IF ipiQty EQ 0 THEN xest.est-qty[1] ELSE ipiQty
    ll-label = CAN-FIND(FIRST sys-ctrl
                    WHERE sys-ctrl.company EQ xest.company
                      AND sys-ctrl.name    EQ "MACHFILE"
                      AND sys-ctrl.log-fld).

blok:
FOR EACH xef
    WHERE xef.company  EQ xest.company
    AND xef.est-no   EQ xest.est-no
    AND (xef.form-no EQ ipiForm OR ipiForm EQ 0):

    ASSIGN
        xef.op-lock = NO
        ll-corr     = NO.

    RUN cec/isitfoam.p (ROWID(xef), OUTPUT ll-foam).

    FOR EACH xeb
        WHERE xeb.company EQ xest.company
        AND xeb.est-no  EQ xest.est-no
        AND xeb.form-no EQ xef.form-no
        AND xeb.pur-man EQ NO
        NO-LOCK,

        FIRST style
        {sys/ref/styleW.i}
        AND style.style EQ xeb.style
        NO-LOCK
    
        BREAK BY xeb.blank-no:

        FIND FIRST item
        {sys/look/itemivW.i}
          AND item.i-no EQ xeb.i-code[1]
            NO-LOCK NO-ERROR.
        IF AVAILABLE item THEN FIND FIRST e-item OF item NO-LOCK NO-ERROR.
        maxco = (xeb.i-col + xeb.i-coat ) / xeb.i-pass.
        {sys/inc/roundup.i maxco}
    
        ASSIGN
            iIndex     = 0
            v-yld = IF xest.form-qty EQ 1 THEN 1 ELSE
              (IF xeb.yld-qty LT 0 THEN -1 / xeb.yld-qty ELSE xeb.yld-qty)
            dMSF = ipiQty * v-yld * xeb.t-len * xeb.t-wid
            dMSF = (IF v-corr THEN (dMSF * .007) ELSE (dMSF / 144)) / 1000.

        {sys/inc/roundup.i dMSF}

        FIND FIRST routing-mtx
        {sys/ref/rout-mtxW.i}
          AND routing-mtx.msf GE int(dMSF)
            NO-LOCK NO-ERROR.

        IF AVAILABLE routing-mtx THEN 
        DO:
            DO iIndex = 1 TO 10:
                IF (routing-mtx.dim-type  EQ "BLANK" AND
                    routing-mtx.bl-len[iIndex] GE xeb.t-len)     OR
                    (routing-mtx.dim-type  EQ "SHEET" AND
                    routing-mtx.bl-len[iIndex] GE xef.nsh-len)   THEN LEAVE.
            END.
            iIndexAlt = iIndex.
            DO iIndex = 1 TO 10:
                IF (routing-mtx.dim-type  EQ "BLANK" AND
                    routing-mtx.bl-wid[iIndex] GE xeb.t-wid)     OR
                    (routing-mtx.dim-type  EQ "SHEET" AND
                    routing-mtx.bl-wid[iIndex] GE xef.nsh-wid)   THEN LEAVE.
            END.
            iIndex = ((iIndex - 1) * 10) + iIndexAlt.
        END.
  
        IF iIndex GE 1 AND iIndex LE 100 THEN
            FIND FIRST routing
            {sys/ref/routingW.i}
          AND routing.r-code EQ routing-mtx.r-code[iIndex]
                NO-LOCK NO-ERROR.

        IF ll-foam THEN
            RUN cec/foammach.p (IF AVAILABLE routing THEN RECID(routing) ELSE ?,
                OUTPUT v-def-r).

        xcal = xef.cal.
  
        IF AVAILABLE routing THEN 
        DO:
            EMPTY TEMP-TABLE w-routing.
  
            DO iIndex = 1 TO 10:
                IF routing.m-code[iIndex] NE "" THEN 
                DO:
                    FIND FIRST mach
                        {&where-machine}
                        AND mach.m-code EQ routing.m-code[iIndex]
                    NO-LOCK NO-ERROR.

                    IF NOT AVAILABLE mach                                     OR
                        (AVAILABLE style AND style.type EQ "F"                              AND
                        (mach.dept[1] EQ "RC" OR mach.dept[1] EQ "DC") AND
                        v-def-r)                                          THEN NEXT.

                    CREATE w-routing.
                    w-routing.m-code = routing.m-code[iIndex].
                    RELEASE w-routing.

                    CREATE tt-mach-route.
                    ASSIGN
                        tt-mach-route.m-code   = routing.m-code[iIndex]
                        tt-mach-route.form-no  = xef.form-no
                        tt-mach-route.blank-no = IF xest.est-type EQ 5 THEN 1
                                      ELSE IF AVAILABLE xeb AND mach.p-type EQ "B" THEN
                                           xeb.blank-no
                                      ELSE 0.

                    RELEASE tt-mach-route.
                END.
            END.

            FOR EACH w-routing:
                FIND FIRST mach
                    {&where-machine}
                    AND mach.m-code EQ w-routing.m-code
                NO-LOCK NO-ERROR.

                IF NOT AVAILABLE mach OR
                    (NOT FIRST(xeb.blank-no) AND mach.p-type NE "B") THEN NEXT.

                DO i = 1 TO IF mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
                    mach.dept[3] EQ "PR" OR mach.dept[3] EQ "PR" THEN
                    IF xeb.i-pass EQ 0 THEN 1 ELSE xeb.i-pass
                    ELSE
                    IF mach.dept[1] EQ "RC" THEN 2 ELSE 1:

                    v-on-f = IF i EQ 1 THEN xef.n-out ELSE xef.n-out-l.

                    DO WHILE TRUE:
                        IF mach.dept[1] EQ "RC" THEN RUN cec/rc-mach.p (BUFFER mach, v-on-f, YES).
            
                        ELSE 
                        DO: 
                            IF mach.p-type EQ "B" THEN
                                ASSIGN
                                    sh-len = xeb.t-wid
                                    sh-wid = xeb.t-len.
            
                            ELSE
                                IF xef.lam-dscr EQ "R" THEN
                                    ASSIGN
                                        sh-wid = xef.nsh-wid
                                        sh-len = xef.nsh-len.
                                ELSE
                                    ASSIGN
                                        sh-wid = xef.nsh-len
                                        sh-len = xef.nsh-wid.
            
                                    {cec/mach-seq.i sh-len sh-wid xcal &defr=defr}
                        END.
            
                        LEAVE.
                    END.

                    IF AVAILABLE mach                                         AND
                        (mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
                        mach.dept[3] EQ "PR" OR mach.dept[3] EQ "PR")     AND
                        (maxco GT mach.max-col OR
                        (AVAILABLE ITEM AND mach.pr-type NE item.press-type)) THEN                                       
                        RELEASE mach.

                    v-defr-valid = AVAILABLE mach.
      
                    IF NOT AVAILABLE mach THEN
                        FIND FIRST mach
                        {&where-machine}
                        AND mach.m-code EQ w-routing.m-code
                    NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE mach THEN LEAVE.

                    CREATE m-lst.
                    ASSIGN
                        m-lst.f-no       = xef.form-no
                        m-lst.seq        = (10 * mach.d-seq) + (i - 1)
                        m-lst.dept       = mach.dept[1]
                        m-lst.bl         = mach.p-type EQ "B"
                        m-lst.m-code     = mach.m-code
                        m-lst.dscr       = mach.m-dscr
                        m-lst.defr       = YES
                        m-lst.defr-valid = v-defr-valid.

                    IF m-lst.bl THEN m-lst.b-no = xeb.blank-no.

                    IF LOOKUP(mach.dept[1],"CR,RC") GT 0 THEN 
                    DO:
                        m-lst.n-out = IF i EQ 2 THEN xef.n-out-l ELSE xef.n-out.

                        IF cercrout                                           AND
                            xef.n-out LE 1 AND xef.gsh-wid LE xef.nsh-wid      AND
                            xef.n-out-l LE 1 AND xef.gsh-len LE xef.nsh-len    THEN
                            LEAVE.

                        ELSE
                            IF (xef.n-out LE 1 AND xef.gsh-wid LE xef.nsh-wid AND i EQ 1)   OR
                                (xef.n-out-l LE 1 AND xef.gsh-len LE xef.nsh-len AND i EQ 2) THEN
                            DO:
                                IF ceroute1-log EQ YES AND
                                    CAN-FIND(FIRST tt-mach-route WHERE
                                    tt-mach-route.m-code = mach.m-code AND
                                    tt-mach-route.form-no = xef.form-no AND
                                    tt-mach-route.blank-no = IF xest.est-type EQ 5 THEN 1
                                ELSE IF AVAILABLE xeb AND mach.p-type EQ "B" THEN
                                    xeb.blank-no
                                ELSE 0) AND 
                                    NOT CAN-FIND(FIRST m2-lst WHERE
                                    m2-lst.m-code EQ m-lst.m-code AND
                                    m2-lst.f-no EQ m-lst.f-no AND
                                    m2-lst.b-no EQ m-lst.b-no AND
                                    ROWID(m2-lst) NE ROWID(m-lst)) THEN
                                DO:
                                    CREATE tt-mach-exc.
                                    ASSIGN
                                        tt-mach-exc.form-no  = xef.form-no
                                        tt-mach-exc.blank-no = IF xest.est-type EQ 5 THEN 1 ELSE
                                         IF AVAILABLE xeb AND mach.p-type EQ "B" THEN xeb.blank-no ELSE 0
                                        tt-mach-exc.m-code   = mach.m-code
                                        tt-mach-exc.dept     = mach.dept[1]
                                        tt-mach-exc.reason   = IF (xef.n-out LE 1 AND xef.gsh-wid LE xef.nsh-wid AND i EQ 1) THEN
                                             "Net Sheet Width Larger Than Or Equal To Gross Sheet Width"
                                          ELSE
                                             "Net Sheet Length Larger Than Or Equal To Gross Sheet Length".
                                    RELEASE tt-mach-exc.
                                END.

                                DELETE m-lst.
                            END.
                            /*if i eq 1 condition is yes and i eq 2 condition is no, delete error reason*/
                            ELSE IF ceroute1-log EQ YES AND i EQ 2 AND
                                    (xef.n-out LE 1 AND xef.gsh-wid LE xef.nsh-wid AND i EQ 1) THEN
                                DO:
                                    FIND FIRST tt-mach-exc WHERE
                                        tt-mach-exc.m-code   = mach.m-code AND
                                        tt-mach-exc.form-no  = xef.form-no AND
                                        tt-mach-exc.blank-no = (IF xest.est-type EQ 5 THEN 1 ELSE
                                        IF AVAILABLE xeb AND mach.p-type EQ "B" THEN xeb.blank-no ELSE 0) AND
                                        tt-mach-exc.reason EQ "Net Sheet Width Larger Than Or Equal To Gross Sheet Width"
                                        NO-ERROR.

                                    IF AVAILABLE tt-mach-exc THEN
                                        DELETE tt-mach-exc.
                                END.
                    END.

                    IF ceroute1-log EQ YES AND AVAILABLE m-lst AND m-lst.defr-valid = NO AND
                        CAN-FIND(FIRST tt-mach-route WHERE
                        tt-mach-route.m-code = mach.m-code AND
                        tt-mach-route.form-no = xef.form-no AND
                        tt-mach-route.blank-no = IF xest.est-type EQ 5 THEN 1
                    ELSE IF AVAILABLE xeb AND mach.p-type EQ "B" THEN
                        xeb.blank-no
                    ELSE 0) THEN
                    DO:
                        FIND FIRST tt-mach-exc WHERE
                            tt-mach-exc.m-code EQ mach.m-code AND
                            tt-mach-exc.form-no EQ xef.form-no AND
                            tt-mach-exc.blank-no EQ IF xest.est-type EQ 5 THEN 1 ELSE
                            IF AVAILABLE xeb AND mach.p-type EQ "B" THEN xeb.blank-no ELSE 0
                            NO-ERROR.

                        IF NOT AVAILABLE tt-mach-exc THEN
                        DO:
                            CREATE tt-mach-exc.
                            ASSIGN
                                tt-mach-exc.form-no  = xef.form-no
                                tt-mach-exc.blank-no = IF xest.est-type EQ 5 THEN 1 ELSE
                                      IF AVAILABLE xeb AND mach.p-type EQ "B" THEN xeb.blank-no ELSE 0
                                tt-mach-exc.m-code   = mach.m-code
                                tt-mach-exc.dept     = mach.dept[1].
                        END.

                        tt-mach-exc.reason   = IF maxco GT mach.max-col THEN
                            "Max # of Colors Exceeded"
                            ELSE IF (AVAILABLE ITEM AND mach.pr-type NE item.press-type) THEN
                                           "Printer Type Mismatch"
                                       ELSE tt-mach-exc.reason.
                        RELEASE tt-mach-exc.
                    END.
                END.
            END.
  
            IF NOT ll-foam THEN 
            DO:
                ASSIGN
                    i = 0
                    j = 0.

                FOR EACH m-lst
                    WHERE m-lst.dept EQ "RC"
                    AND m-lst.f-no EQ xef.form-no:
                    i = i + 1.  
                END.
  
                IF i GT 2 THEN
                    FOR EACH m-lst
                        WHERE m-lst.dept EQ "RC"
                        AND m-lst.f-no EQ xef.form-no:
                        j = j + 1.
    
                        IF j NE 1 AND j NE i THEN DELETE m-lst.
                    END.
            END.
        END.
    END.

    RELEASE xeb.

    FIND FIRST xeb
        WHERE xeb.company EQ xef.company
        AND xeb.est-no  EQ xef.est-no
        AND xeb.form-no EQ xef.form-no
        NO-LOCK NO-ERROR.

    FIND FIRST style
      {sys/ref/styleW.i}
        AND style.style EQ xeb.style
        NO-LOCK NO-ERROR.

    IF xeb.pur-man THEN 
    DO:      /* Purchased FG */
        RELEASE mach.
        DO i = 1 TO 7:
            IF AVAILABLE style AND style.m-code[i] EQ "" THEN NEXT.
            IF AVAILABLE style THEN
                FIND FIRST mach
                {&where-machine}
                AND mach.m-code EQ style.m-code[i]
            NO-LOCK NO-ERROR.
            IF AVAILABLE mach AND (mach.dept[1] NE "FO") AND
                (mach.dept[2] NE "FO") AND
                (mach.dept[3] NE "FO") AND
                (mach.dept[4] NE "FO")
                THEN RELEASE mach.
      
            IF AVAILABLE mach THEN LEAVE.
        END.
        IF NOT AVAILABLE mach THEN
            FOR EACH mach
                {&where-machine}
          AND mach.dept[1] EQ "FO"
        NO-LOCK:
        LEAVE.
    END.
    IF AVAILABLE mach THEN 
    DO:
        CREATE m-lst.
        ASSIGN
            m-lst.f-no   = xef.form-no
            m-lst.seq    = 10 * mach.d-seq
            m-lst.dept   = "FO"
            m-lst.bl     = YES
            m-lst.m-code = mach.m-code.
    END.
END.

  ELSE DO:                     /* Manufactured FG */
FIND FIRST style
        {sys/ref/styleW.i}
          AND style.style EQ xeb.style
    NO-LOCK NO-ERROR.

IF xef.lam-dscr EQ "R"                         /*or
       (xef.lam-dscr ne "R" and xef.xgrain eq "S")*/ THEN
    ASSIGN
        sh-wid = xef.nsh-wid
        sh-len = xef.nsh-len.
ELSE
    ASSIGN
        sh-wid = xef.nsh-len
        sh-len = xef.nsh-wid.

IF NOT ll-foam THEN 
DO:
    /* CTS - added corrugator logic */
    /* need corrugator? */
    /* if xef.roll then */
    IF CAN-FIND(FIRST item-bom
        WHERE item-bom.company  EQ cocode
        AND item-bom.parent-i EQ xef.board) THEN 
    DO:
        /* find corrugator entered in style file, if any */
        IF AVAILABLE mach THEN RELEASE mach.
        DO i = 1 TO 7:
            IF AVAILABLE style AND style.m-code[i] = "" THEN NEXT.
            IF AVAILABLE style THEN
                FIND FIRST mach {&where-machine}            AND
                mach.m-code  = style.m-code[i] NO-LOCK NO-ERROR.

            IF AVAILABLE mach THEN
            DO:
          
                IF mach.p-type EQ "R" OR
                    (mach.p-type EQ "B" AND ll-label) THEN
                    ASSIGN
                        v-gsh-len = xef.gsh-wid
                        v-gsh-wid = xef.gsh-len.
                ELSE
                    ASSIGN
                        v-gsh-wid = xef.gsh-wid
                        v-gsh-len = xef.gsh-len.
            END.
            ELSE
                ASSIGN
                    v-gsh-wid = xef.gsh-wid
                    v-gsh-len = xef.gsh-len.

            IF AVAILABLE mach AND (mach.dept[1] NE "CR") AND
                (mach.dept[2] NE "CR") AND
                (mach.dept[3] NE "CR") AND
                (mach.dept[4] NE "CR")
                THEN RELEASE mach.

                {cec/mach-seq.i v-gsh-len v-gsh-wid xcal}
        END.
        IF NOT AVAILABLE mach THEN
            FOR EACH mach
                {&where-machine}
              and mach.dept[1] eq "CR"
            no-lock:

        IF mach.p-type EQ "R" OR
            (mach.p-type EQ "B" AND ll-label) THEN
            ASSIGN
                v-gsh-len = xef.gsh-wid
                v-gsh-wid = xef.gsh-len.
        ELSE
            ASSIGN
                v-gsh-wid = xef.gsh-wid
                v-gsh-len = xef.gsh-len.

           {cec/mach-seq.i v-gsh-len v-gsh-wid xcal}
    END.
    IF AVAILABLE mach THEN 
    DO:
        CREATE m-lst.
        ASSIGN
            m-lst.f-no   = xef.form-no
            m-lst.seq    = 10 * mach.d-seq
            m-lst.dept   = "CR"
            m-lst.bl     = NO
            m-lst.m-code = mach.m-code
            m-lst.n-out  = xef.n-out
            ll-corr      = YES.
    END.
END.
/* CTS end */

/* need sheeter? */
IF xef.roll AND NOT ll-corr THEN 
DO:
    /* find sheeter entered in style file, if any */
    IF AVAILABLE mach THEN RELEASE mach.
    DO i = 1 TO 7:
        IF AVAILABLE style AND style.m-code[i] = "" THEN NEXT.
        IF AVAILABLE style THEN
            FIND FIRST mach {&where-machine}            AND
            mach.m-code  = style.m-code[i] NO-LOCK NO-ERROR.
        IF AVAILABLE mach AND (mach.dept[1] NE "RS") AND
            (mach.dept[2] NE "RS") AND
            (mach.dept[3] NE "RS") AND
            (mach.dept[4] NE "RS")
            THEN RELEASE mach.
            {cec/mach-seq.i xef.gsh-len xef.gsh-wid xcal}
    END.
    IF NOT AVAILABLE mach THEN
        FOR EACH mach
            {&where-machine}
              and mach.dept[1] eq "RS"
            no-lock:
          {cec/mach-seq.i xef.gsh-len xef.gsh-wid xcal}
END.
IF AVAILABLE mach THEN 
DO:
    CREATE m-lst.
    ASSIGN 
        m-lst.f-no   = xef.form-no
        m-lst.seq    = 10 * mach.d-seq
        m-lst.dept   = "RS"
        m-lst.bl     = NO
        m-lst.m-code = mach.m-code.
END.
END.

/* find ink & coater */
IF xeb.i-pass > 0 THEN 
DO:
    FIND FIRST item
            {sys/look/itemivW.i}
              AND item.i-no EQ xeb.i-code[1]
        NO-LOCK NO-ERROR.
    IF AVAILABLE item THEN FIND FIRST e-item OF item NO-LOCK NO-ERROR.
    maxco = (xeb.i-col + xeb.i-coat ) / xeb.i-pass.
        {sys/inc/roundup.i maxco}

    IF xeb.i-coat > 0 THEN
    DO k = 1 TO xeb.i-coat:
        /* find coater entered in style file, if any */
        IF AVAILABLE mach THEN RELEASE mach.
        DO i = 1 TO 7:
            IF AVAILABLE style AND style.m-code[i] = "" THEN NEXT.
            IF AVAILABLE style THEN
                FOR EACH mach
                    {&where-machine}
                  and mach.m-code  eq style.m-code[i]
                  and mach.dept[1] eq "CT"
                no-lock:
                    {cec/mach-seq.i sh-len sh-wid xcal}
        END.
    END.
    /* find 1st valid machine in mach file */
    IF NOT AVAILABLE mach THEN
        FOR EACH mach
            {&where-machine}
                and mach.dept[1] eq "CT"
              no-lock:
            {cec/mach-seq.i sh-len sh-wid xcal}
END.
IF AVAILABLE mach THEN 
DO:
    CREATE m-lst.
    ASSIGN
        m-lst.f-no    = xef.form-no
        m-lst.seq     = (10 * mach.d-seq) + k
        m-lst.bl      = NO
        m-lst.dept    = "CT"
        m-lst.m-code  = mach.m-code
        m-lst.dscr    = mach.m-dscr
        m-lst.pass-no = k.

    maxco = xeb.i-col / xeb.i-pass.
            {sys/inc/roundup.i maxco}
END.
END. /* avail item... */

/* find press */
IF AVAILABLE item THEN k = 0.
prez: REPEAT:
    k = k + 1.
    /* find machine entered in style file, if any */
    IF k = 1 AND AVAILABLE mach THEN RELEASE mach.
    DO i = 1 TO 7:
        IF AVAILABLE style  AND style.m-code[i] = "" THEN NEXT.
        IF AVAILABLE style THEN
            FOR EACH mach
                {&where-machine}
                  and mach.m-code  eq style.m-code[i]
                  and mach.max-col ge maxco
                no-lock:
              {cec/mach-seq.i sh-len sh-wid xcal}
    END.
    IF (NOT AVAILABLE mach) OR
        (AVAILABLE item AND mach.pr-type NE item.press-type) THEN NEXT.
    IF AVAILABLE mach THEN LEAVE.
END.
/* find layout machine in mach file */
IF NOT AVAILABLE mach THEN 
DO:
    FIND FIRST mach {&where-machine} AND mach.m-code = xef.m-code
    NO-LOCK NO-ERROR.
    IF AVAILABLE mach AND mach.dept[1] NE "PR" THEN RELEASE mach.
END.
/* find 1st valid machine in mach file */
IF NOT AVAILABLE mach THEN
    FOR EACH mach
        {&where-machine}
                and mach.max-col ge maxco
              by mach.max-col:
IF AVAILABLE item AND mach.pr-type NE item.press-type THEN NEXT.
            {cec/mach-seq.i sh-len sh-wid xcal}
END.

IF AVAILABLE mach THEN 
DO:
    IF mach.p-type = "R" OR xef.roll = NO THEN 
    DO:
        FIND FIRST m-lst WHERE m-lst.dept = "RS" NO-ERROR.
        IF AVAILABLE m-lst THEN
        DO:
            DELETE m-lst.

            IF ceroute1-log EQ YES AND
                CAN-FIND(FIRST tt-mach-route WHERE
                tt-mach-route.m-code = mach.m-code AND
                tt-mach-route.form-no = xef.form-no AND
                tt-mach-route.blank-no = IF xest.est-type EQ 5 THEN 1
            ELSE IF AVAILABLE xeb AND mach.p-type EQ "B" THEN
                xeb.blank-no
            ELSE 0) THEN
            DO:
                CREATE tt-mach-exc.
                ASSIGN
                    tt-mach-exc.form-no  = xef.form-no
                    tt-mach-exc.blank-no = IF xest.est-type EQ 5 THEN 1 ELSE
                                           IF AVAILABLE xeb AND mach.p-type EQ "B" THEN xeb.blank-no ELSE 0
                    tt-mach-exc.m-code   = mach.m-code
                    tt-mach-exc.dept     = mach.dept[1]
                    tt-mach-exc.reason   = "Invalid Machine".
                RELEASE tt-mach-exc.
            END.
        END.
    END.
    IF k = 1 AND mach.coater = TRUE AND mach.max-color > maxco THEN 
    DO:
        FIND FIRST m-lst WHERE m-lst.seq > 30 AND m-lst.seq < 40 NO-ERROR.
        IF AVAILABLE m-lst THEN 
        DO:

            DELETE m-lst.
                
            ASSIGN
                k     = k - 1
                maxco = maxco + 1.
            NEXT prez.
        END.
    END.  
    CREATE m-lst.
    ASSIGN
        m-lst.f-no    = xef.form-no
        m-lst.seq     = (10 * mach.d-seq) + k
        m-lst.bl      = NO
        m-lst.dept    = "PR"
        m-lst.m-code  = mach.m-code
        m-lst.dscr    = mach.m-dscr
        m-lst.pass-no = k.
END.
IF k >= xeb.i-pass THEN LEAVE.
END. /* avail item... */
END. /* if i-pass > 0 */
      else do:
k = 1.
/* find machine entered in style file, if any */
DO i = 1 TO 7:
    IF AVAILABLE style THEN
        FIND FIRST mach {&where-machine} AND mach.m-code = style.m-code[i]
    NO-LOCK NO-ERROR.
    IF AVAILABLE mach THEN LEAVE.
END.
/* find 1st valid machine in mach file */
IF NOT AVAILABLE mach THEN
    FIND FIRST mach {&where-machine} AND mach.m-code = xef.m-code
NO-LOCK NO-ERROR.
IF AVAILABLE mach AND mach.dept[1] = "PR" THEN 
DO:
    IF mach.p-type = "R" OR xef.roll = NO THEN 
    DO:
        FIND FIRST m-lst WHERE m-lst.dept = "RS" NO-ERROR.
        IF AVAILABLE m-lst THEN DELETE m-lst.
    END.
    CREATE m-lst.
    ASSIGN
        m-lst.f-no    = xef.form-no
        m-lst.seq     = (10 * mach.d-seq) + k
        m-lst.bl      = NO
        m-lst.m-code  = mach.m-code
        m-lst.dscr    = mach.m-dscr
        m-lst.pass-no = k.
    DO i = 1 TO 4 :
        IF mach.dept[i] NE "" OR mach.dept[i] NE "PR" THEN 
        DO:
            m-lst.dept    = mach.dept[i].
            LEAVE.
        END.
    END.

    IF m-lst.dept EQ "" AND mach.dept[1] EQ "PR" THEN
        m-lst.dept = "PR".
END.
END.
END. /* NOT ll-foam */

RUN cec/mach-sq1.p.

IF NOT ll-foam THEN RUN cec/mach-sq4.p.

END.   /* Manufactured FG */
END. /* for each xef */

IF ceroute1-log EQ YES THEN
DO:
    RUN est/d-machex2.w(AVAILABLE routing, OUTPUT op-farmout, OUTPUT op-valid-mach).

    SESSION:SET-WAIT-STATE ("GENERAL").
    IF op-valid-mach OR NOT CAN-FIND(FIRST tt-mach-route) THEN
    DO:
        IF op-valid-mach AND (AVAILABLE xeb AND NOT xeb.pur-man OR NOT AVAILABLE xeb) THEN
        DO:
            FOR EACH m-lst WHERE
                NOT CAN-FIND(FIRST tt-mach-route WHERE
                tt-mach-route.m-code EQ m-lst.m-code AND
                tt-mach-route.form-no EQ m-lst.f-no AND
                ((tt-mach-route.blank-no EQ m-lst.b-no) OR
                (xest.est-type EQ 5 AND m-lst.b-no EQ 0))):

                DELETE m-lst.
            END.
        END.

        RUN cec/mach-sq2.p (AVAILABLE routing, iplBuildCombo).
        SESSION:SET-WAIT-STATE ("").
    END.
    ELSE IF op-farmout THEN
        DO:
            FOR EACH m-lst:
                DELETE m-lst.
            END.

            FOR EACH xef WHERE
                xef.company  EQ xest.company AND
                xef.est-no   EQ xest.est-no AND
                (xef.form-no EQ ipiForm OR ipiForm EQ 0)
                NO-LOCK:

                RELEASE xeb.

                FIND FIRST xeb WHERE
                    xeb.company EQ xef.company AND
                    xeb.est-no  EQ xef.est-no AND
                    xeb.form-no EQ xef.form-no
                    NO-LOCK NO-ERROR.

                FIND FIRST style
               {sys/ref/styleW.i} AND
               style.style EQ xeb.style
                    NO-LOCK NO-ERROR.

                RELEASE mach.
                DO i = 1 TO 7:
                    IF AVAILABLE style AND style.m-code[i] EQ "" THEN NEXT.
                    IF AVAILABLE style THEN
                        FIND FIRST mach
                    {&where-machine} AND
                        mach.m-code EQ style.m-code[i]
                    NO-LOCK NO-ERROR.
            
                    IF AVAILABLE mach AND
                        (mach.dept[1] NE "FO") AND
                        (mach.dept[2] NE "FO") AND
                        (mach.dept[3] NE "FO") AND
                        (mach.dept[4] NE "FO") THEN
                        RELEASE mach.
            
                    IF AVAILABLE mach THEN LEAVE.
                END.
                IF NOT AVAILABLE mach THEN
                    FOR EACH mach
                        {&where-machine} AND
                 mach.dept[1] EQ "FO"
                 NO-LOCK:
                LEAVE.
            END.

            IF AVAILABLE mach THEN 
            DO:
                CREATE m-lst.
                ASSIGN
                    m-lst.f-no   = xef.form-no
                    m-lst.seq    = 10 * mach.d-seq
                    m-lst.dept   = "FO"
                    m-lst.bl     = YES
                    m-lst.m-code = mach.m-code.
                RELEASE m-lst.
            END.
        END.

    RUN cec/mach-sq2.p (AVAILABLE routing, iplBuildCombo).
END.
END.
ELSE
   RUN cec/mach-sq2.p (AVAIL routing, iplBuildCombo).

SESSION:SET-WAIT-STATE ("").

IF ceroute1-log EQ NO THEN
    RUN est/d-machex.w.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
