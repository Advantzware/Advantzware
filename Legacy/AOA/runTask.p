/* runTask.p - rstark - 12.14.2018 */

/* prowin.exe spawned by tasker.w monitor */

DEFINE VARIABLE cAppSrv     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJasperFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubject    AS CHARACTER NO-UNDO.
DEFINE VARIABLE dttDateTime AS DATETIME  NO-UNDO.
DEFINE VARIABLE hAppSrv     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hAppSrvBin  AS HANDLE    NO-UNDO.
DEFINE VARIABLE hJasper     AS HANDLE    NO-UNDO.
DEFINE VARIABLE iAuditID    AS INTEGER   NO-UNDO.
DEFINE VARIABLE rRowID      AS ROWID     NO-UNDO.
DEFINE VARIABLE hSession    AS HANDLE    NO-UNDO.

{AOA/includes/pCalcNextRun.i}

ASSIGN
    PROPATH = ENTRY(1,SESSION:PARAMETER,"+")
    rRowID  = TO-ROWID(ENTRY(2,SESSION:PARAMETER,"+"))
    .
RUN system\session.p PERSISTENT SET hSession.
SESSION:ADD-SUPER-PROCEDURE (hSession).

FIND FIRST config NO-LOCK.
FIND FIRST Task NO-LOCK WHERE ROWID(Task) EQ rRowID.

FIND FIRST user-print NO-LOCK
     WHERE user-print.company    EQ Task.company
       AND user-print.program-id EQ Task.programID
       AND user-print.user-id    EQ Task.user-id
       AND user-print.batch-seq  EQ Task.taskID
       AND user-print.prgmName   EQ ""
     NO-ERROR.
IF AVAILABLE user-print THEN DO:
    FIND FIRST prgrms NO-LOCK
         WHERE prgrms.prgmname EQ Task.programID
         NO-ERROR.
    IF AVAILABLE prgrms THEN DO:
        IF prgrms.module NE "" THEN DO:
            cAppSrv = "AOA\appServer\aoa" + prgrms.module.
            IF SEARCH(cAppSrv + ".r") NE ? OR
               SEARCH(cAppSrv + ".p") NE ? THEN DO:
                RUN AOA\appServer\aoaBin.p PERSISTENT SET hAppSrvBin.
                SESSION:ADD-SUPER-PROCEDURE (hAppSrvBin).
                RUN VALUE(cAppSrv + ".p") PERSISTENT SET hAppSrv.
                SESSION:ADD-SUPER-PROCEDURE (hAppSrv).                
                RUN AOA\aoaJasper.p PERSISTENT SET hJasper.
                SESSION:ADD-SUPER-PROCEDURE (hJasper).
                RUN spJasper (
                    Task.taskFormat,
                    ROWID(user-print),
                    hAppSrv,
                    hAppSrvBin,
                    OUTPUT cJasperFile
                    ).
                RUN pCreateAuditHdr.
                RUN spCreateAuditDtl (iAuditID, "programID", 0, cJasperFile, Task.programID,  NO).
                IF Task.recipients NE "" THEN DO:
                    IF AVAILABLE config THEN DO:
                        IF config.taskName THEN
                        cSubject = cSubject + Task.taskName + " ".
                        IF config.taskType THEN
                        cSubject = cSubject + Task.taskFormat + " ".
                        IF config.taskDate THEN
                        cSubject = cSubject + STRING(TODAY,"99/99/9999") + " ".
                        IF config.taskTime THEN
                        cSubject = cSubject + STRING(TIME,"HH:MM:SS am").
                        cSubject = TRIM(cSubject).
                    END. /* if avail */
                    ELSE
                    cSubject = "AOA Task Result".
                    CREATE taskEmail.
                    ASSIGN
                        taskEmail.subject    = cSubject
                        taskEmail.body       = IF AVAILABLE config AND config.emailBody NE "" THEN
                                               config.emailBody ELSE "AOA Task Result Attached"
                        taskEmail.attachment = cJasperFile
                        taskEmail.recipients = Task.recipients
                        taskEmail.mustExist  = YES
                        taskEmail.rec_key    = Task.rec_key
                        .
                END. /* if recipients */
                ELSE IF Task.runNow THEN DO:
                    IF AVAILABLE config AND config.cueCard THEN DO:
                        CREATE taskEmail.
                        ASSIGN
                            taskEmail.subject    = "Submitted Run Now Request"
                            taskEmail.body       = ""
                            taskEmail.attachment = cJasperFile
                            taskEmail.recipients = "Cue Card Message"
                            taskEmail.user-id    = Task.user-id
                            taskEmail.mustExist  = YES
                            taskEmail.rec_key    = Task.rec_key
                            .
                    END. /* if avail */
                END. /* else if runnow */
                RUN pCalcNextRun (YES).
            END. /* if search */
            ELSE DO:
                RUN pCreateAuditHdr.
                RUN spCreateAuditDtl (
                    iAuditID,
                    "module",
                    0,
                    "invalid prgrms module (" + prgrms.module + ")",
                    Task.programID,
                    NO
                    ).
            END. /* else search */
        END. /* if module */
        ELSE DO:
            RUN pCreateAuditHdr.
            RUN spCreateAuditDtl (
                iAuditID,
                "programID",
                0,
                "prgrms module blank",
                Task.programID,
                NO
                ).
        END. /* else module */
    END. /* if avail prgrms */
    ELSE DO:
        RUN pCreateAuditHdr.
        RUN spCreateAuditDtl (
            iAuditID,
            "programID",
            0,
            "prgrms record not found",
            Task.programID,
            NO
            ).
    END. /* else prgrms */
END. /* if avail user-print */
ELSE DO:
    RUN pCreateAuditHdr.
    RUN spCreateAuditDtl (
        iAuditID,
        "taskID",
        0,
        "user-print record not found",
        Task.company + "|" +
        Task.programID + "|" +
        Task.user-id + "|" +
        STRING(Task.taskID),
        NO
        ).
END. /* else user-print */

QUIT.

PROCEDURE pCreateAuditHdr:
    RUN spCreateAuditHdr (
        "TASK",
        "ASI",
        "Task",
        Task.rec_key,
        OUTPUT iAuditID
        ).
END PROCEDURE.
