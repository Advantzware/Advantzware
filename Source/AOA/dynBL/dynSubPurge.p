/*------------------------------------------------------------------------
  File:         dynSubPurge.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 10.4.2019
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&Scoped-define backupDir dynSubject.bak

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttDynSubject
DEFINE TEMP-TABLE ttDynSubject NO-UNDO LIKE dynSubject
    FIELD dynStatus AS CHARACTER FORMAT "x(20)" LABEL "Status"
    .
/* Parameters Definitions ---                                           */

&Scoped-define subjectID 45
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

DEFINE STREAM sDynSubject.
DEFINE STREAM sDynSubTable.
DEFINE STREAM sDynSubWhere.
DEFINE STREAM sDynSubCol.
DEFINE STREAM sDynSubParam.
DEFINE STREAM sDynParamVal.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE BUFFER bDynSubject FOR dynSubject.

    IF lPurge THEN DO:
        OS-CREATE-DIR "{&backupDir}".
        OUTPUT STREAM sDynSubject  TO "{&backupDir}\dynSubject.d"       APPEND.
        OUTPUT STREAM sDynSubtable TO "{&backupDir}\dynSubjectTable.d"  APPEND.
        OUTPUT STREAM sDynSubWhere TO "{&backupDir}\dynSubjectWhere.d"  APPEND.
        OUTPUT STREAM sDynSubCol   TO "{&backupDir}\dynSubjectColumn.d" APPEND.
        OUTPUT STREAM sDynSubParam TO "{&backupDir}\dynSubjectParam.d"  APPEND.
        OUTPUT STREAM sDynParamVal TO "{&backupDir}\dynParamValue.d"    APPEND.
    END. /* if lpurge */
    FOR EACH bDynSubject NO-LOCK
        WHERE bDynSubject.subjectID GE 5000
          AND bDynSubject.lastRunDateTime LT DATETIME(TODAY - 90,0)
        :
        CREATE ttDynSubject.
        BUFFER-COPY bDynSubject TO ttDynSubject.
        IF bDynSubject.lastRunDateTime LT DATETIME(TODAY - 180,0) THEN DO:
            IF lPurge THEN
            ttDynSubject.dynStatus = "Deleted".
            ELSE
            ttDynSubject.dynStatus = "To Be Deleted".
        END. /* if datatime */
        ELSE IF bDynSubject.lastRunDateTime LT DATETIME(TODAY - 90,0) THEN
        ttDynSubject.dynStatus = "To Be Depreciated".
        IF ttDynSubject.dynStatus EQ "Deleted" THEN
        DO TRANSACTION:
            FIND FIRST dynSubject EXCLUSIVE-LOCK
                 WHERE ROWID(dynSubject) EQ ROWID(bDynSubject).
            FOR EACH dynSubjectTable EXCLUSIVE-LOCK OF dynSubject:
                EXPORT STREAM sDynSubtable dynSubjectTable.
                DELETE dynSubjectTable.
            END. /* each dynsubjectable */
            FOR EACH dynSubjectWhere EXCLUSIVE-LOCK OF dynSubject:
                EXPORT STREAM sDynSubWhere dynSubjectWhere.
                DELETE dynSubjectWhere.
            END. /* each dynsubjectable */
            FOR EACH dynSubjectColumn EXCLUSIVE-LOCK OF dynSubject:
                EXPORT STREAM sDynSubCol dynSubjectColumn.
                DELETE dynSubjectColumn.
            END. /* each dynsubjectable */
            FOR EACH dynSubjectParamSet EXCLUSIVE-LOCK OF dynSubject:
                EXPORT STREAM sDynSubParam dynSubjectParamSet.
                DELETE dynSubjectParamSet.
            END. /* each dynsubjectable */
            FOR EACH dynParamValue EXCLUSIVE-LOCK OF dynSubject:
                EXPORT STREAM sDynParamVal dynParamValue.
                DELETE dynParamValue.
            END. /* each dynsubjectable */
            EXPORT STREAM sDynSubject dynSubject.
            DELETE dynSubject.
        END. /* if deleted */
    END. /* each bdynsubject */
    IF lPurge THEN DO:
        OUTPUT STREAM sDynSubject  CLOSE. 
        OUTPUT STREAM sDynSubtable CLOSE. 
        OUTPUT STREAM sDynSubWhere CLOSE.
        OUTPUT STREAM sDynSubCol   CLOSE. 
        OUTPUT STREAM sDynSubParam CLOSE.
        OUTPUT STREAM sDynParamVal CLOSE.
    END. /* if lpurge */
END PROCEDURE.
