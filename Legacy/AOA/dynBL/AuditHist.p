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
    DEFINE VARIABLE dtStartDateTime AS DATETIME  NO-UNDO.
    DEFINE VARIABLE dtEndDateTime   AS DATETIME  NO-UNDO.
    
    DEFINE BUFFER bAuditDtl FOR AuditDtl.
    
    /* subject business logic */
    ASSIGN
        dtStartDateTime = DATETIME(STRING(dtStartTransDate,"99/99/9999") + " 00:00:00") 
        dtEndDateTime   = DATETIME(STRING(dtEndTransDate,"99/99/9999")   + " 23:59:59")
        .
    FOR EACH AuditHdr
        WHERE  AuditHdr.AuditDateTime  GE dtStartDateTime
          AND  AuditHdr.AuditDateTime  LE dtEndDateTime
          AND (AuditHdr.AuditType      EQ cTypes  OR cTypes  EQ "ALL")
          AND (AuditHdr.AuditUser      EQ cUsers  OR cUsers  EQ "ALL")
          AND (AuditHdr.AuditDB        EQ cDBs    OR cDBs    EQ "ALL")
          AND (AuditHdr.AuditTable     EQ cTables OR cTables EQ "ALL"),
        FIRST  bAuditDtl OF AuditHdr NO-LOCK
        WHERE (bAuditDtl.AuditField  EQ cFields   OR cFields EQ "ALL")
          AND (bAuditDtl.AuditBeforeValue BEGINS cBeforeValueFilter
           OR  cBeforeValueFilter EQ "")
          AND (bAuditDtl.AuditAfterValue  BEGINS cAfterValueFilter
           OR  cAfterValueFilter  EQ "")
        :
        IF CAN-FIND(FIRST AuditDtl OF AuditHdr
                    WHERE (AuditDtl.AuditField EQ cFields OR cFields EQ "ALL")
                      AND (AuditDtl.AuditBeforeValue   EQ cBeforeValueFilter
                       OR  cBeforeValueFilter  EQ "")
                      AND (AuditDtl.AuditAfterValue    EQ cAfterValueFilter
                       OR  cAfterValueFilter   EQ "")) THEN
        FOR EACH AuditDtl OF AuditHdr
            WHERE (AuditDtl.AuditField EQ cFields OR cFields EQ "ALL")
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
        IF lPurge THEN DO:
            FIND FIRST AuditStack EXCLUSIVE-LOCK
                 WHERE AuditStack.AuditStackID EQ AuditHdr.AuditStackID
                 NO-ERROR.
            IF AVAILABLE AuditStack THEN
            DELETE AuditStack.
            DELETE AuditHdr.
        END. /* if lpurge */
    END. /* EACH AuditHdr */
END PROCEDURE.
