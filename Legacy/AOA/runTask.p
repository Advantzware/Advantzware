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

FIND FIRST dynParamValue NO-LOCK
     WHERE dynParamValue.subjectID    EQ Task.subjectID
       AND dynParamValue.user-id      EQ Task.user-id
       AND dynParamValue.prgmName     EQ Task.prgmName
       AND dynParamValue.paramValueID EQ Task.paramValueID
     NO-ERROR.
IF AVAILABLE dynParamValue THEN DO:
    FIND FIRST dynSubject NO-LOCK
         WHERE dynSubject.subjectID EQ Task.subjectID
         NO-ERROR.
    IF AVAILABLE dynSubject THEN DO:
        FIND FIRST prgrms NO-LOCK
             WHERE prgrms.prgmName EQ Task.prgmName
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
                    RUN spJasperQuery (
                        Task.taskFormat,
                        ROWID(dynParamValue),
                        dynSubject.subjectName,
                        Task.user-id,
                        hAppSrvBin,
                        OUTPUT cJasperFile
                        ).
                    RUN pCreateAuditHdr.
                    RUN spCreateAuditDtl (iAuditID, "programID", 0, cJasperFile, Task.prgmName,  NO).
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
                        Task.prgmName,
                        NO
                        ).
                END. /* else search */
            END. /* if module */
            ELSE DO:
                RUN pCreateAuditHdr.
                RUN spCreateAuditDtl (
                    iAuditID,
                    "prgmName",
                    0,
                    "prgrms module blank",
                    Task.prgmName,
                    NO
                    ).
            END. /* else module */
        END. /* if avail prgrms */
        ELSE DO:
            RUN pCreateAuditHdr.
            RUN spCreateAuditDtl (
                iAuditID,
                "prgmName",
                0,
                "prgrms record not found",
                Task.prgmName,
                NO
                ).
        END. /* else prgrms */
    END. /* if avail dynsubject */
    ELSE DO:
        RUN pCreateAuditHdr.
        RUN spCreateAuditDtl (
            iAuditID,
            "subjectID",
            0,
            "dynSubject record not found",
            STRING(Task.subjectID),
            NO
            ).
    END. /* else dynsubject */
END. /* if avail dynParamValue */
ELSE DO:
    RUN pCreateAuditHdr.
    RUN spCreateAuditDtl (
        iAuditID,
        "taskID",
        0,
        "dynParamValue record not found",
        STRING(Task.subjectID) + "|" +
        Task.user-id + "|" +
        Task.prgmName + "|" +
        STRING(Task.paramValueID),
        NO
        ).
END. /* else dynParamValue */

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
