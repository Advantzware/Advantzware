/*------------------------------------------------------------------------
  File:         AuditHist.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 6.17.2019
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttAuditHistory 
{AOA/tempTable/ttAuditHistory.i}

/* Local Variable Definitions ---                                       */

&Scoped-define subjectID 11
{AOA/includes/subjectID{&subjectID}Defs.i}

/* subject business logic */
/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cStartAfterValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartBeforeValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartDB          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartField       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartTable       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartType        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartUser        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndAfterValue    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndBeforeValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndDB            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndField         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndTable         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndType          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndUser          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndDateTime     AS DATETIME  NO-UNDO.
    DEFINE VARIABLE dtStartDateTime   AS DATETIME  NO-UNDO.
    DEFINE VARIABLE iCount            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lDelete           AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bAuditDtl FOR AuditDtl.
    
    /* subject business logic */
    ASSIGN
        cStartType        = IF cTypes  NE "ALL" THEN cTypes  ELSE CHR(32)
        cEndType          = IF cTypes  NE "ALL" THEN cTypes  ELSE CHR(254)
        cStartUser        = IF cUsers  NE "ALL" THEN cUsers  ELSE CHR(32)
        cEndUser          = IF cUsers  NE "ALL" THEN cUsers  ELSE CHR(254)
        cStartDB          = IF cDBs    NE "ALL" THEN cDBs    ELSE CHR(32)
        cEndDB            = IF cDBs    NE "ALL" THEN cDBs    ELSE CHR(254)
        cStartTable       = IF cTables NE "ALL" THEN cTables ELSE CHR(32)
        cEndTable         = IF cTables NE "ALL" THEN cTables ELSE CHR(254)
        cStartField       = IF cFields NE "ALL" THEN cFields ELSE CHR(32)
        cEndField         = IF cFields NE "ALL" THEN cFields ELSE CHR(254)
        cStartBeforeValue = IF cBeforeValueFilter NE "" THEN cBeforeValueFilter ELSE CHR(32)
        cEndBeforeValue   = IF cBeforeValueFilter NE "" THEN cBeforeValueFilter ELSE CHR(254)
        cStartAfterValue  = IF cAfterValueFilter  NE "" THEN cAfterValueFilter  ELSE CHR(32)
        cEndAfterValue    = IF cAfterValueFilter  NE "" THEN cAfterValueFilter  ELSE CHR(254)
        dtStartDateTime   = DATETIME(STRING(dtStartTransDate,"99/99/9999") + " 00:00:00") 
        dtEndDateTime     = DATETIME(STRING(dtEndTransDate,"99/99/9999")   + " 23:59:59")
        .
    FOR EACH AuditHdr
        WHERE AuditHdr.AuditDateTime GE dtStartDateTime
          AND AuditHdr.AuditDateTime LE dtEndDateTime
          AND AuditHdr.AuditType     GE cStartType
          AND AuditHdr.AuditType     LE cEndType
          AND AuditHdr.AuditUser     GE cStartUser
          AND AuditHdr.AuditUser     LE cEndUser
          AND AuditHdr.AuditDB       GE cStartDB
          AND AuditHdr.AuditDB       LE cEndDB
          AND AuditHdr.AuditTable    GE cStartTable
          AND AuditHdr.AuditTable    LE cEndTable,
        FIRST bAuditDtl OF AuditHdr NO-LOCK
        WHERE bAuditDtl.AuditField       GE cStartField
          AND bAuditDtl.AuditField       LE cEndField
          AND bAuditDtl.AuditBeforeValue GE cStartBeforeValue
          AND bAuditDtl.AuditBeforeValue LE cEndBeforeValue
          AND bAuditDtl.AuditAfterValue  GE cStartAfterValue
          AND bAuditDtl.AuditAfterValue  LE cEndAfterValue
        :
        iCount = iCount + 1.
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, ?).
        FIND FIRST AuditTbl NO-LOCK
             WHERE AuditTbl.AuditTable EQ AuditHdr.AuditTable
             NO-ERROR.
        lDelete = NOT AVAILABLE AuditTbl OR DATETIME(TODAY - AuditTbl.expireDays + 1,0) GT AuditHdr.AuditDateTime.
        IF CAN-FIND(FIRST AuditDtl OF AuditHdr
                    WHERE AuditDtl.AuditField       GE cStartField
                      AND AuditDtl.AuditField       LE cEndField
                      AND AuditDtl.AuditBeforeValue GE cStartBeforeValue
                      AND AuditDtl.AuditBeforeValue LE cEndBeforeValue
                      AND AuditDtl.AuditAfterValue  GE cStartAfterValue
                      AND AuditDtl.AuditAfterValue  LE cEndAfterValue) THEN
        FOR EACH AuditDtl OF AuditHdr
            WHERE AuditDtl.AuditField       GE cStartField
              AND AuditDtl.AuditField       LE cEndField
              AND AuditDtl.AuditBeforeValue GE cStartBeforeValue
              AND AuditDtl.AuditBeforeValue LE cEndBeforeValue
              AND AuditDtl.AuditAfterValue  GE cStartAfterValue
              AND AuditDtl.AuditAfterValue  LE cEndAfterValue
            :
            CREATE ttAuditHistory.
            ASSIGN 
                ttAuditHistory.AuditID          = AuditHdr.AuditID
                ttAuditHistory.AuditType        = AuditHdr.AuditType
                ttAuditHistory.AuditDateTime    = AuditHdr.AuditDateTime
                ttAuditHistory.AuditUser        = AuditHdr.AuditUser
                ttAuditHistory.AuditDB          = AuditHdr.AuditDB
                ttAuditHistory.AuditTable       = AuditHdr.AuditTable
                ttAuditHistory.AuditField       = AuditDtl.AuditField
                ttAuditHistory.AuditExtent      = AuditDtl.AuditExtent
                ttAuditHistory.AuditIdxField    = AuditDtl.AuditIdxField
                ttAuditHistory.AuditKey         = AuditHdr.AuditKey
                ttAuditHistory.AuditBeforeValue = AuditDtl.AuditBeforeValue
                ttAuditHistory.AuditAfterValue  = AuditDtl.AuditAfterValue
                .
            IF lPurge AND lDelete THEN
            DELETE AuditDtl.
        END. /* each auditdtl */
        ELSE DO: /* no audit detail records exist */
            CREATE ttAuditHistory.
            ASSIGN 
                ttAuditHistory.AuditID          = AuditHdr.AuditID
                ttAuditHistory.AuditType        = AuditHdr.AuditType
                ttAuditHistory.AuditDateTime    = AuditHdr.AuditDateTime
                ttAuditHistory.AuditUser        = AuditHdr.AuditUser
                ttAuditHistory.AuditDB          = AuditHdr.AuditDB
                ttAuditHistory.AuditTable       = AuditHdr.AuditTable
                ttAuditHistory.AuditKey         = AuditHdr.AuditKey
                .
        END. /* else */
        IF lPurge AND lDelete THEN DO:
            FIND FIRST AuditStack EXCLUSIVE-LOCK
                 WHERE AuditStack.AuditStackID EQ AuditHdr.AuditStackID
                 NO-ERROR.
            IF AVAILABLE AuditStack THEN
            DELETE AuditStack.
            DELETE AuditHdr.
        END. /* if lpurge */
    END. /* EACH AuditHdr */
END PROCEDURE.
