/*------------------------------------------------------------------------
  File:         dynPurge.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 1.16.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE iNumResults AS INTEGER NO-UNDO.

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttPurge
DEFINE TEMP-TABLE ttPurge NO-UNDO
    FIELD purgeType       AS CHARACTER FORMAT "x(20)" LABEL "Type"
    FIELD purgeDescrip    AS CHARACTER FORMAT "x(40)" LABEL "Title / Description"
    FIELD user-id         AS CHARACTER FORMAT "x(10)" LABEL "User ID"
    FIELD lastRunDateTime AS DATETIME                 LABEL "Last Run Date/Time"
    FIELD subjectID     LIKE dynSubject.subjectID
    FIELD paramValueID  LIKE dynParamValue.paramValueID
    FIELD taskID        LIKE Task.taskID 
    FIELD purgeStatus     AS CHARACTER FORMAT "x(20)" LABEL "Status"
    FIELD purgeRowID      AS ROWID
    .
/* Parameters Definitions ---                                           */

&Scoped-define subjectID 45
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

FUNCTION fPurgeStatus RETURNS CHARACTER
    (ipdtLastRunDateTime AS DATETIME, ipdtCompareDateTime AS DATETIME):

    RETURN IF ipdtLastRunDateTime LT ipdtCompareDateTime THEN "Deleted 180+ Days"
           ELSE "Warning 90+ Days".
END FUNCTION.

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE dtRunDateTime AS DATETIME NO-UNDO EXTENT 2.
    DEFINE VARIABLE iCount        AS INTEGER  NO-UNDO.

    ASSIGN
        dtRunDateTime[1] = DATETIME(TODAY - 90, 0)
        dtRunDateTime[2] = DATETIME(TODAY - 180, 0)
        .
    FOR EACH Task NO-LOCK
        WHERE Task.scheduled EQ NO
          AND Task.lastDate  LT TODAY - 90
        :
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iNumResults, ?).
        RUN pCreatettPurge (
            "Unscheduled Task",
            Task.taskName,
            Task.user-id,
            DATETIME(Task.lastDate, Task.lastTime * 1000),
            Task.subjectID,
            Task.paramValueID,
            Task.taskID,
            fPurgeStatus (DATETIME(Task.lastDate, Task.lastTime * 1000), dtRunDateTime[2]),
            ROWID(Task)
            ).
    END. /* each task */

    FOR EACH Task NO-LOCK
        WHERE Task.endDate LT TODAY - 90
        :
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iNumResults, ?).
        RUN pCreatettPurge (
            "Expired Task",
            Task.taskName,
            Task.user-id,
            DATETIME(Task.endDate, 86400000),
            Task.subjectID,
            Task.paramValueID,
            Task.taskID,
            fPurgeStatus (DATETIME(Task.endDate, 86400000), dtRunDateTime[2]),
            ROWID(Task)
            ).
    END. /* each task */

    FOR EACH dynParamValue NO-LOCK
        WHERE dynParamValue.user-id         NE "_default"
          AND dynParamvalue.paramValueID    GT 0
          AND dynParamValue.lastRunDateTime LT dtRunDateTime[1]
        :
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iNumResults, ?).
        RUN pCreatettPurge (
            "User Task",
            dynParamValue.paramDescription,
            dynParamValue.user-id,
            dynParamValue.lastRunDateTime,
            dynParamValue.subjectID,
            dynParamValue.paramValueID,
            0,
            fPurgeStatus (dynParamValue.lastRunDateTime, dtRunDateTime[2]),
            ROWID(dynParamValue)
            ).
    END. /* each dynparamvalue */

    FOR EACH taskResult NO-LOCK
        WHERE taskResult.fileDateTime LT dtRunDateTime[1]
        :
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iNumResults, ?).
        RUN pCreatettPurge (
            "Task Result",
            taskResult.folderFile,
            taskResult.user-id,
            taskResult.fileDateTime,
            0,
            0,
            0,
            fPurgeStatus (taskResult.fileDateTime, dtRunDateTime[2]),
            ROWID(taskResult)
            ).
    END. /* each dynparamvalue */
    
    IF lPurge THEN
    FOR EACH ttPurge
        WHERE ttPurge.purgeStatus BEGINS "Deleted"
        :
        iCount = iCount + 1.
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, iNumResults).
        CASE ttPurge.purgeType:
            WHEN "Expired Task" OR WHEN "Unscheduled Task" THEN
            DO TRANSACTION:
                FIND Task EXCLUSIVE-LOCK
                     WHERE ROWID(Task) EQ ttPurge.purgeRowID.
                DELETE Task.
            END.
            WHEN "Task Result" THEN
            DO TRANSACTION:
                FIND taskResult EXCLUSIVE-LOCK
                     WHERE ROWID(taskResult) EQ ttPurge.purgeRowID.
                DELETE taskResult.
            END.
            WHEN "User Task" THEN
            DO TRANSACTION:
                FIND dynParamValue EXCLUSIVE-LOCK
                     WHERE ROWID(dynParamValue) EQ ttPurge.purgeRowID.
                DELETE dynParamValue.
            END.
        END CASE.
    END. /* each ttpurge */

END PROCEDURE.

PROCEDURE pCreatettPurge:
    DEFINE INPUT PARAMETER ipcPurgeType        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPurgeDescrip     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtLastRunDateTime AS DATETIME  NO-UNDO.
    DEFINE INPUT PARAMETER ipiSubjectID        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiParamValueID     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiTaskID           AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcPurgeStatus      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iprRowID            AS ROWID     NO-UNDO.

    CREATE ttPurge.
    ASSIGN
        ttPurge.purgeType       = ipcPurgeType
        ttPurge.purgeDescrip    = ipcPurgeDescrip
        ttPurge.user-id         = ipcUserID
        ttPurge.lastRunDateTime = ipdtLastRunDateTime
        ttPurge.subjectID       = ipiSubjectID
        ttPurge.paramValueID    = ipiParamValueID
        ttPurge.taskID          = ipiTaskID
        ttPurge.purgeStatus     = ipcPurgeStatus
        ttPurge.purgeRowID      = iprRowID
        iNumResults             = iNumResults + 1
        .
END PROCEDURE.
