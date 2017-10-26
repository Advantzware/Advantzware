/*------------------------------------------------------------------------
  File: auditQuery.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Audit Query.rpa */
{aoa/tempTable/ttAuditQuery.i}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttAuditQuery.
{aoa/includes/pAuditQuery.i}

/* local variables */
DEFINE VARIABLE dtStartDateTime AS DATETIME  NO-UNDO.
DEFINE VARIABLE dtEndDateTime   AS DATETIME  NO-UNDO.

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
      AND (AuditHdr.AuditTable     EQ cTable OR cTable EQ "ALL")
    :
    IF CAN-FIND(FIRST AuditDtl OF AuditHdr) THEN 
    FOR EACH AuditDtl OF AuditHdr
        WHERE  AuditDtl.AuditID    EQ AuditHdr.AuditID
          AND (AuditDtl.AuditField EQ cField OR cField EQ "ALL")
        :
        CREATE ttAuditQuery.
        ASSIGN 
            ttAuditQuery.AuditID          = AuditHdr.AuditID
            ttAuditQuery.AuditType        = AuditHdr.AuditType
            ttAuditQuery.AuditDateTime    = AuditHdr.AuditDateTime
            ttAuditQuery.AuditUser        = AuditHdr.AuditUser
            ttAuditQuery.AuditDB          = AuditHdr.AuditDB
            ttAuditQuery.AuditTable       = AuditHdr.AuditTable
            ttAuditQuery.AuditField       = AuditDtl.AuditField
            ttAuditQuery.AuditExtent      = AuditDtl.AuditExtent
            ttAuditQuery.AuditIdxField    = AuditDtl.AuditIdxField
            ttAuditQuery.AuditKey         = AuditHdr.AuditKey
            ttAuditQuery.AuditBeforeValue = AuditDtl.AuditBeforeValue
            ttAuditQuery.AuditAfterValue  = AuditDtl.AuditAfterValue
            .
        IF lPurge THEN 
        DELETE AuditDtl.
    END. /* each auditdtl */
    ELSE DO: /* no audit detail records exist */
        CREATE ttAuditQuery.
        ASSIGN 
            ttAuditQuery.AuditID          = AuditHdr.AuditID
            ttAuditQuery.AuditType        = AuditHdr.AuditType
            ttAuditQuery.AuditDateTime    = AuditHdr.AuditDateTime
            ttAuditQuery.AuditUser        = AuditHdr.AuditUser
            ttAuditQuery.AuditDB          = AuditHdr.AuditDB
            ttAuditQuery.AuditTable       = AuditHdr.AuditTable
            ttAuditQuery.AuditKey         = AuditHdr.AuditKey
            .
    END. /* else */
    IF lPurge THEN 
    DELETE AuditHdr.
END. /* EACH AuditHdr */
