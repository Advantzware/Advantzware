/* spDynValidateProc.p - rstark - 2.27.2019 */

/* add dynamic validate procedures in alphabetical order */
/* 1. procedure has input of iphWidget AS HANDLE         */
/* 2. RUN dynValReturn (iphWidget, results of validate)  */

&Scoped-define defInputParam ~
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.~
~
    RUN spGetSessionParam ("Company", OUTPUT cCompany).
&Scoped-define checkRange ~
    RUN dynValReturn (iphWidget,~
        iphWidget:SCREEN-VALUE EQ CHR(32)  OR~
        iphWidget:SCREEN-VALUE EQ CHR(254) OR

DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSessionParam AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSessionValue AS CHARACTER NO-UNDO.

/* **********************  Internal Functions  ************************ */

FUNCTION fErrorMsg RETURNS CHARACTER (iphWidget AS HANDLE):
    RETURN "Invalid Entry for " + iphWidget:LABEL + " " + iphWidget:SCREEN-VALUE.
END FUNCTION.

/* **********************  Internal Procedures  *********************** */

/* all validate procedures should run this procedure, else RETURN "" */
PROCEDURE dynValReturn:
    DEFINE INPUT PARAMETER iphWidget AS HANDLE  NO-UNDO.
    DEFINE INPUT PARAMETER iplReturn AS LOGICAL NO-UNDO.
    
    IF iplReturn THEN RETURN "".
    ELSE RETURN fErrorMsg (iphWidget).
END PROCEDURE.

/* create procedures in alphabetical order below here */
PROCEDURE dynValAuditTable:
    {&defInputParam}    
    DEFINE VARIABLE cFieldLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFields     AS CHARACTER NO-UNDO INITIAL "All,All".
    DEFINE VARIABLE hWidget     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lTables     AS LOGICAL   NO-UNDO.

    RUN pGetWidgetByName (iphWidget, "types", OUTPUT hWidget).
    lTables = CAN-DO("LOG,TRACK",hWidget:SCREEN-VALUE) EQ NO.
    RUN pGetWidgetByName (iphWidget, "fields", OUTPUT hWidget).
    IF lTables THEN
    FOR EACH ASI._file NO-LOCK
        WHERE ASI._file._Tbl-type EQ "T"
          AND (iphWidget:SCREEN-VALUE EQ "All"
           OR ASI._file._file-name EQ iphWidget:SCREEN-VALUE),
        EACH ASI._field OF ASI._file NO-LOCK
        BREAK BY ASI._field._field-name
        :
        IF FIRST-OF(ASI._field._field-name) THEN DO:
            IF CAN-FIND(FIRST AuditHdr
                        WHERE AuditHdr.AuditDB    EQ "ASI"
                          AND AuditHdr.AuditTable EQ ASI._file._file-name) THEN 
            ASSIGN
                cFieldLabel = IF ASI._field._Label NE ? THEN ASI._field._Label ELSE ""
                cFields     = cFields + ","
                            + cFieldLabel + " ("
                            + ASI._field._field-name  + "),"
                            + ASI._field._field-name
                            .
        END. /* if first-of */
    END. /* each _file */
    ASSIGN
        hWidget:LIST-ITEM-PAIRS = cFields
        hWidget:SCREEN-VALUE    = "All"
        .
    RETURN "".
END PROCEDURE.

PROCEDURE dynValAuditType:
    {&defInputParam}
    DEFINE VARIABLE cTableLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTables     AS CHARACTER NO-UNDO INITIAL "All,All".
    DEFINE VARIABLE hWidget     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lTables     AS LOGICAL   NO-UNDO.
    
    RUN pGetWidgetByName (iphWidget, "tables", OUTPUT hWidget).
    lTables = CAN-DO("LOG,TRACK",iphWidget:SCREEN-VALUE) EQ NO.
    IF lTables THEN
    FOR EACH ASI._file NO-LOCK
        WHERE ASI._file._Tbl-type EQ "T"
        :
        IF CAN-FIND(FIRST AuditHdr
                    WHERE AuditHdr.AuditDB    EQ "ASI"
                      AND AuditHdr.AuditTable EQ ASI._file._file-name) THEN 
        ASSIGN
            cTableLabel = IF ASI._file._file-label NE ? THEN ASI._file._file-label ELSE ""
            cTables     = cTables + ","
                        + cTableLabel + " ("
                        + ASI._file._file-name  + "),"
                        + ASI._file._file-name
                        .
    END. /* each _file */
    IF iphWidget:SCREEN-VALUE EQ "All" OR lTables EQ NO THEN
    FOR EACH prgrms NO-LOCK
        BY prgrms.mnemonic
        BY prgrms.prgTitle
        :
        IF CAN-FIND(FIRST AuditHdr
                    WHERE AuditHdr.AuditDB    EQ "ASI"
                      AND AuditHdr.AuditTable EQ prgrms.prgmname) THEN
        cTables = cTables + "," + "["
                  + prgrms.mnemonic + "] "
                  + prgrms.prgTitle
                  + " (" + prgrms.prgmname + "),"
                  + prgrms.prgmname
                  .
    END. /* each prgrms */
    ASSIGN
        hWidget:LIST-ITEM-PAIRS = cTables
        hWidget:SCREEN-VALUE    = "All"
        .
    RUN dynValAuditTable (hWidget).
    RETURN "".
END PROCEDURE.

PROCEDURE dynValBoxDesign:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST box-design-hdr
                 WHERE box-design-hdr.company EQ cCompany
                 AND box-design-hdr.design-no <> 0
                   AND box-design-hdr.design-no  EQ integer(iphWidget:SCREEN-VALUE))
        ).
END PROCEDURE.

PROCEDURE dynValCarrier:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST carrier
                 WHERE carrier.company EQ cCompany
                   AND carrier.carrier EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValCompany:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST company
                 WHERE company.company EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValCust:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST cust
                 WHERE cust.company EQ cCompany
                   AND cust.cust-no EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValCustype:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST custype
                 WHERE custype.company EQ cCompany
                   AND custype.custype EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValFGItem:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST itemfg
                 WHERE itemfg.company EQ cCompany
                   AND itemfg.i-no    EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValFlute:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST flute
                 WHERE flute.company EQ cCompany
                   AND flute.CODE    EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValLoc:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST loc
                 WHERE loc.company EQ cCompany
                   AND loc.loc     EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValMachine:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST mach
                 WHERE mach.company EQ cCompany
                   AND mach.m-code  EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValMat:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST mat
                 WHERE mat.mat EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValPrep:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST prep
                 WHERE prep.company EQ cCompany
                   AND prep.CODE    EQ iphWidget:SCREEN-VALUE)
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
    {&checkRange}
        CAN-FIND(FIRST procat
                 WHERE procat.company EQ cCompany
                   AND procat.procat  EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.


PROCEDURE dynValRMItem:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST item
                 WHERE item.company EQ cCompany
                   AND item.i-no    EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValSalesRep:
    {&defInputParam}
    {&checkRange}
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

PROCEDURE dynValScoreType:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST scoreType
                 WHERE scoreType.company   EQ cCompany
                   AND scoreType.scoreType EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValStackPatterns:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST stackPattern
                 WHERE  stackPattern.stackCode     EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValStyle:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST style
                 WHERE style.company EQ cCompany
                   AND style.style   EQ iphWidget:SCREEN-VALUE)
        ).
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
    
    {&checkRange}
        CAN-FIND(FIRST users
                 WHERE users.user_id EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE dynValVendor:
    {&defInputParam}
    {&checkRange}
        CAN-FIND(FIRST vend
                 WHERE vend.company EQ cCompany
                   AND vend.vend-no EQ iphWidget:SCREEN-VALUE)
        ).
END PROCEDURE.

PROCEDURE pGetWidgetByName PRIVATE:
    DEFINE INPUT  PARAMETER iphWidget AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcName   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ophWidget AS HANDLE    NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    
    ASSIGN
        hWidget = iphWidget:FRAME
        hWidget = hWidget:FIRST-CHILD
        hWidget = hWidget:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hWidget):
        IF hWidget:NAME NE ? AND hWidget:NAME EQ ipcName THEN
        LEAVE.
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */
    ophWidget = hWidget.
END PROCEDURE.
