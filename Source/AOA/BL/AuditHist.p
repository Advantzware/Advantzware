/*------------------------------------------------------------------------
  File: AuditHist.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Audit History.rpa */
{AOA/tempTable/ttAuditHistory.i}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttAuditHistory.
{AOA/includes/pAuditHistory.i}

/* local variables */
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

DEFINE BUFFER bAuditDtl FOR AuditDtl.

/* subject business logic */
ASSIGN
    cStartType        = IF cType EQ "ALL" THEN cType ELSE CHR(32)
    cEndType          = IF cType EQ "ALL" THEN cType ELSE CHR(254)
    cStartUser        = IF cUser EQ "ALL" THEN cUser ELSE CHR(32)
    cEndUser          = IF cUser EQ "ALL" THEN cUser ELSE CHR(254)
    cStartDB          = IF cDB EQ "ALL" THEN cDB ELSE CHR(32)
    cEndDB            = IF cDB EQ "ALL" THEN cDB ELSE CHR(254)
    cStartTable       = IF cTable EQ "ALL" THEN cTable ELSE CHR(32)
    cEndTable         = IF cTable EQ "ALL" THEN cTable ELSE CHR(254)
    cStartField       = IF cField EQ "ALL" THEN cField ELSE CHR(32)
    cEndField         = IF cField EQ "ALL" THEN cField ELSE CHR(254)
    cStartBeforeValue = IF cBeforeValueFilter NE "" THEN cBeforeValueFilter ELSE CHR(32)
    cEndBeforeValue   = IF cBeforeValueFilter NE "" THEN cBeforeValueFilter ELSE CHR(254)
    cStartAfterValue  = IF cAfterValueFilter  NE "" THEN cAfterValueFilter  ELSE CHR(32)
    cEndAfterValue    = IF cAfterValueFilter  NE "" THEN cAfterValueFilter  ELSE CHR(254)
    dtStartDateTime   = DATETIME(STRING(dtStartDate,"99/99/9999") + " 00:00:00") 
    dtEndDateTime     = DATETIME(STRING(dtEndDate,"99/99/9999")   + " 23:59:59")
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
        IF lPurge THEN 
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
    IF lPurge THEN DO:
        FIND FIRST AuditStack EXCLUSIVE-LOCK
             WHERE AuditStack.AuditStackID EQ AuditHdr.AuditStackID
             NO-ERROR.
        IF AVAILABLE AuditStack THEN
        DELETE AuditStack.
        DELETE AuditHdr.
    END. /* if lpurge */
END. /* EACH AuditHdr */
