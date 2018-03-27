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
DEFINE VARIABLE dtStartDateTime AS DATETIME  NO-UNDO.
DEFINE VARIABLE dtEndDateTime   AS DATETIME  NO-UNDO.

DEFINE BUFFER bAuditDtl FOR AuditDtl.

/* subject business logic */
ASSIGN
    dtStartDateTime = DATETIME(STRING(dtStartDate,"99/99/9999") + " 00:00:00") 
    dtEndDateTime   = DATETIME(STRING(dtEndDate,"99/99/9999")   + " 23:59:59")
    .
FOR EACH AuditHdr
    WHERE  AuditHdr.AuditDateTime  GE dtStartDateTime
      AND  AuditHdr.AuditDateTime  LE dtEndDateTime
      AND (AuditHdr.AuditType      EQ cType  OR cType  EQ "ALL")
      AND (AuditHdr.AuditUser      EQ cUser  OR cUser  EQ "ALL")
      AND (AuditHdr.AuditDB        EQ cDB    OR cDB    EQ "ALL")
      AND (AuditHdr.AuditTable     EQ cTable OR cTable EQ "ALL"),
    FIRST  bAuditDtl OF AuditHdr NO-LOCK
    WHERE (bAuditDtl.AuditField  EQ cField   OR cField EQ "ALL")
      AND (bAuditDtl.AuditBeforeValue BEGINS cBeforeValueFilter
       OR  cBeforeValueFilter EQ "")
      AND (bAuditDtl.AuditAfterValue  BEGINS cAfterValueFilter
       OR  cAfterValueFilter  EQ "")
    :
    IF CAN-FIND(FIRST AuditDtl OF AuditHdr
                WHERE (AuditDtl.AuditField EQ cField OR cField EQ "ALL")
                  AND (AuditDtl.AuditBeforeValue   EQ cBeforeValueFilter
                   OR  cBeforeValueFilter  EQ "")
                  AND (AuditDtl.AuditAfterValue    EQ cAfterValueFilter
                   OR  cAfterValueFilter   EQ "")) THEN
    FOR EACH AuditDtl OF AuditHdr
        WHERE (AuditDtl.AuditField EQ cField OR cField EQ "ALL")
          AND (AuditDtl.AuditBeforeValue EQ cBeforeValueFilter
           OR  cBeforeValueFilter  EQ "")
          AND (AuditDtl.AuditAfterValue  EQ cAfterValueFilter
           OR  cAfterValueFilter   EQ "")
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
    IF lPurge THEN 
    DELETE AuditHdr.
END. /* EACH AuditHdr */
