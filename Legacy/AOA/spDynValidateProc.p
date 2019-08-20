/* spDynValidateProc.p - rstark - 2.27.2019 */

/* add dynamic validate procedures in alphabetical order */
/* 1. procedure has input of iphWidget AS HANDLE         */
/* 2. RUN dynValReturn (iphWidget, results of validate)  */

&Scoped-define defInputParam ~
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.~
~
    RUN spGetSessionParam ("Company", OUTPUT cCompany).

DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSessionParam AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSessionValue AS CHARACTER NO-UNDO.

/* **********************  Internal Functions  ************************ */

FUNCTION fErrorMsg RETURNS CHARACTER (iphWidget AS HANDLE):
    RETURN "Invalid Entry for " + iphWidget:LABEL + " " + iphWidget:SCREEN-VALUE.
END FUNCTION.

/* **********************  Internal Procedures  *********************** */

/* all validate procedures should run this procedure */
PROCEDURE dynValReturn:
    DEFINE INPUT PARAMETER iphWidget AS HANDLE  NO-UNDO.
    DEFINE INPUT PARAMETER iplReturn AS LOGICAL NO-UNDO.
    
    IF iplReturn THEN RETURN "".
    ELSE RETURN fErrorMsg (iphWidget).
END PROCEDURE.

/* create procedures in alphabetical order below here */
PROCEDURE dynValCompany:
    {&defInputParam}
    RUN dynValReturn (iphWidget,
        iphWidget:SCREEN-VALUE EQ CHR(32)  OR
        iphWidget:SCREEN-VALUE EQ CHR(254) OR
        CAN-FIND(FIRST company
                 WHERE company.company EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValCust:
    {&defInputParam}
    RUN dynValReturn (iphWidget,
        iphWidget:SCREEN-VALUE EQ CHR(32)  OR
        iphWidget:SCREEN-VALUE EQ CHR(254) OR
        CAN-FIND(FIRST cust
                 WHERE cust.company EQ cCompany
                   AND cust.cust-no EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValFGItem:
    {&defInputParam}
    RUN dynValReturn (iphWidget,
        iphWidget:SCREEN-VALUE EQ CHR(32)  OR
        iphWidget:SCREEN-VALUE EQ CHR(254) OR
        CAN-FIND(FIRST itemfg
                 WHERE itemfg.company EQ cCompany
                   AND itemfg.i-no    EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValLoc:
    {&defInputParam}
    RUN dynValReturn (iphWidget,
        iphWidget:SCREEN-VALUE EQ CHR(32)  OR
        iphWidget:SCREEN-VALUE EQ CHR(254) OR
        CAN-FIND(FIRST loc
                 WHERE loc.company EQ cCompany
                   AND loc.loc     EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValMachine:
    {&defInputParam}
    RUN dynValReturn (iphWidget,
        iphWidget:SCREEN-VALUE EQ CHR(32)  OR
        iphWidget:SCREEN-VALUE EQ CHR(254) OR
        CAN-FIND(FIRST mach
                 WHERE mach.company EQ cCompany
                   AND mach.m-code  EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValMat:
    {&defInputParam}
    RUN dynValReturn (iphWidget,
        iphWidget:SCREEN-VALUE EQ CHR(32)  OR
        iphWidget:SCREEN-VALUE EQ CHR(254) OR
        CAN-FIND(FIRST mat
                 WHERE mat.mat EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValPostDate:
    {&defInputParam}
    
    DEFINE VARIABLE dtDate  AS DATE    NO-UNDO.
    DEFINE VARIABLE iPeriod AS INTEGER NO-UNDO.
    DEFINE VARIABLE lValid  AS LOGICAL NO-UNDO.

    dtDate = DATE(iphWidget:SCREEN-VALUE) NO-ERROR.
    RUN sys/inc/valtrndt.p (
        cCompany,
        dtDate,
        OUTPUT iPeriod
        ) NO-ERROR.
    lValid = NOT ERROR-STATUS:ERROR.
    IF lValid THEN
    RUN spSetSessionParam ("Period", STRING(iPeriod)).
    FOR EACH period NO-LOCK
        WHERE period.company EQ cCompany
          AND period.pst     LE TODAY
          AND period.pend    GE TODAY
        BY period.pst
        :
        IF period.pst  GT dtDate OR
           period.pend LT dtDate THEN DO:
            MESSAGE
                "Date is NOT in Current Period."
            VIEW-AS ALERT-BOX ERROR.
            lValid = NO.
        END. /* if not in current period */
        LEAVE.
    END. /* each period */
    RUN dynValReturn (iphWidget,lValid).
END PROCEDURE.

PROCEDURE dynValPostInvDate:
    {&defInputParam}

    DEFINE VARIABLE cErrorMsg     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE hSecureHandle AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lSecure       AS LOGICAL   NO-UNDO.

    dtDate = DATE(iphWidget:SCREEN-VALUE) NO-ERROR.
    IF dtDate EQ ? THEN RETURN.
    FIND FIRST period NO-LOCK
         WHERE period.company EQ cCompany
           AND period.pst     LE dtDate
           AND period.pend    GE dtDate
           AND period.pnum    EQ MONTH(dtDate)
         NO-ERROR.
    IF NOT AVAILABLE period THEN
    ASSIGN
        cErrorMsg = "Can Not POST Out Of Period " + STRING(dtDate,"99/99/9999")
        cSessionParam = "PostOutOfPeriod"
        .
    ELSE
    IF period.pstat EQ NO THEN
    ASSIGN
        cErrorMsg = "Period for " + STRING(dtDate,"99/99/9999") + " is already closed"
        cSessionParam = "PostIntoClosedPeriod"
        .
    IF cErrorMsg NE "" THEN DO:
        RUN spGetSessionParam ("Secure", OUTPUT cSessionValue).
        IF CAN-DO(",NO",cSessionValue) THEN DO:
            MESSAGE
                cErrorMsg SKIP
                "Enter Security Password or Enter to Return"
            VIEW-AS ALERT-BOX ERROR.
            RUN sys/ref/d-passwd.w (1, OUTPUT lSecure).
            RUN spSetSessionParam ("Secure", STRING(lSecure)).
            IF lSecure THEN DO:
                RUN spGetSessionParam (cSessionParam + "-Handle", OUTPUT cSessionValue).
                ASSIGN
                    hSecureHandle = WIDGET-HANDLE(cSessionValue)
                    hSecureHandle:SCREEN-VALUE = STRING(lSecure)
                    .
            END. /* if secure */
            RUN spGetSessionParam ("Secure-Handle", OUTPUT cSessionValue).
            ASSIGN
                hSecureHandle = WIDGET-HANDLE(cSessionValue)
                hSecureHandle:SCREEN-VALUE = STRING(lSecure)
                .
        END. /* lsecure eq no */
    END. /* cerrormsg ne "" */
END PROCEDURE.

PROCEDURE dynValProCat:
    {&defInputParam}
    RUN dynValReturn (iphWidget,
        iphWidget:SCREEN-VALUE EQ CHR(32)  OR
        iphWidget:SCREEN-VALUE EQ CHR(254) OR
        CAN-FIND(FIRST procat
                 WHERE procat.company EQ cCompany
                   AND procat.procat  EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValRMItem:
    {&defInputParam}
    RUN dynValReturn (iphWidget,
        iphWidget:SCREEN-VALUE EQ CHR(32)  OR
        iphWidget:SCREEN-VALUE EQ CHR(254) OR
        CAN-FIND(FIRST item
                 WHERE item.company EQ cCompany
                   AND item.i-no    EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValSalesRep:
    {&defInputParam}
    RUN dynValReturn (iphWidget,
        iphWidget:SCREEN-VALUE EQ CHR(32)  OR
        iphWidget:SCREEN-VALUE EQ CHR(254) OR
        CAN-FIND(FIRST sman
                 WHERE sman.company EQ cCompany
                   AND sman.sman    EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValSecure:
        DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
        
        DEFINE VARIABLE hSecureHandle AS HANDLE  NO-UNDO.
        DEFINE VARIABLE lSecure       AS LOGICAL NO-UNDO.
        
        RUN spGetSessionParam ("Secure", OUTPUT cSessionValue).
        IF CAN-DO(",NO",cSessionValue) AND iphWidget:SCREEN-VALUE EQ "YES" THEN DO:
            RUN sys/ref/d-passwd.w (1, OUTPUT lSecure).
            RUN spSetSessionParam ("Secure", STRING(lSecure)).
            RUN spGetSessionParam ("Secure-Handle", OUTPUT cSessionValue).
            ASSIGN
                hSecureHandle = WIDGET-HANDLE(cSessionValue)
                hSecureHandle:SCREEN-VALUE = STRING(lSecure)
                iphWidget:SCREEN-VALUE = STRING(lSecure)
                .
        END. /* if no */
END PROCEDURE.

PROCEDURE dynValTime:
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.

    ASSIGN
        iphWidget:SCREEN-VALUE = iphWidget:SCREEN-VALUE + "0000"
        iphWidget:SCREEN-VALUE = REPLACE(iphWidget:SCREEN-VALUE," ","0")
        .
    RUN dynValReturn (iphWidget,
       (INTEGER(SUBSTR(iphWidget:SCREEN-VALUE,1,2)) EQ 24 AND
        INTEGER(SUBSTR(iphWidget:SCREEN-VALUE,4,2)) EQ 0) OR
       (INTEGER(SUBSTR(iphWidget:SCREEN-VALUE,1,2)) LE 23 AND
        INTEGER(SUBSTR(iphWidget:SCREEN-VALUE,4,2)) LE 59)
        ).
END PROCEDURE.

PROCEDURE dynValUser:
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    RUN dynValReturn (iphWidget,
        iphWidget:SCREEN-VALUE EQ CHR(32)  OR
        iphWidget:SCREEN-VALUE EQ CHR(254) OR
        CAN-FIND(FIRST users
                 WHERE users.user_id EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValVendor:
    {&defInputParam}
    RUN dynValReturn (iphWidget,
        iphWidget:SCREEN-VALUE EQ CHR(32)  OR
        iphWidget:SCREEN-VALUE EQ CHR(254) OR
        CAN-FIND(FIRST vend
                 WHERE vend.company EQ cCompany
                   AND vend.vend-no EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.
