/* runTask.p - rstark - 12.14.2018 */

DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.

DEFINE VARIABLE cAppSrv     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJasperFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubject    AS CHARACTER NO-UNDO.
DEFINE VARIABLE dttDateTime AS DATETIME  NO-UNDO.
DEFINE VARIABLE hAppSrv     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hAppSrvBin  AS HANDLE    NO-UNDO.
DEFINE VARIABLE hJasper     AS HANDLE    NO-UNDO.
DEFINE VARIABLE iAuditID    AS INTEGER   NO-UNDO.

{AOA/includes/pCalcNextRun.i}

FIND FIRST emailConfig NO-LOCK
     WHERE emailConfig.configID EQ 1
       AND emailConfig.isActive EQ YES
     NO-ERROR.
FIND FIRST config NO-LOCK NO-ERROR.
FIND FIRST Task NO-LOCK WHERE ROWID(Task) EQ iprRowID.

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
        IF dynParamValue.module NE "" THEN DO:
            cAppSrv = "AOA/appServer/aoa" + dynParamValue.module.
            IF SEARCH(cAppSrv + ".r") NE ? OR
               SEARCH(cAppSrv + ".p") NE ? THEN DO:
                RUN AOA/appServer/aoaBin.p PERSISTENT SET hAppSrvBin.
                SESSION:ADD-SUPER-PROCEDURE (hAppSrvBin).
                RUN VALUE(cAppSrv + ".p") PERSISTENT SET hAppSrv.
                SESSION:ADD-SUPER-PROCEDURE (hAppSrv).                
                RUN AOA/spJasper.p PERSISTENT SET hJasper.
                SESSION:ADD-SUPER-PROCEDURE (hJasper).
                RUN spJasperQuery (
                    Task.taskFormat,
                    ROWID(dynParamValue),
                    dynSubject.subjectTitle,
                    Task.user-id,
                    hAppSrvBin,
                    Task.rec_key,
                    OUTPUT cJasperFile
                    ).
                IF cJasperFile NE "" THEN DO:
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
                        DO TRANSACTION:
                            CREATE taskEmail.
                            ASSIGN
                                taskEmail.subject    = cSubject
                                taskEmail.body       = IF AVAILABLE emailConfig AND emailConfig.body NE "" THEN
                                                       emailConfig.body ELSE "AOA Task Result Attached"
                                taskEmail.attachment = cJasperFile
                                taskEmail.recipients = Task.recipients
                                taskEmail.mustExist  = YES
                                taskEmail.rec_key    = Task.rec_key
                                .
                        END. /* do trans */
                    END. /* if recipients */
                    ELSE IF Task.runNow THEN DO:
                        IF AVAILABLE config AND config.cueCard THEN
                        DO TRANSACTION:
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
                END. /* if cjasperfile ne "" */
                ELSE DO:
                    RUN pCreateAuditHdr.
                    RUN spCreateAuditDtl (
                        iAuditID,
                        "subjectID",
                        0,
                        "No Data Exists",
                        STRING(Task.subjectID) + "|" +
                        Task.user-id + "|" +
                        Task.prgmName + "|" +
                        STRING(Task.paramValueID),
                        NO
                        ).
                END. /* else cjasperfile ne "" */
                RUN pCalcNextRun (YES).
            END. /* if search */
            ELSE DO:
                RUN pCreateAuditHdr.
                RUN spCreateAuditDtl (
                    iAuditID,
                    "module",
                    0,
                    "invalid module (" + dynParamValue.module + ")",
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
                "module is blank",
                Task.prgmName,
                NO
                ).
        END. /* else module */
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

IF VALID-HANDLE(hAppSrvBin) THEN
DELETE PROCEDURE hAppSrvBin.
IF VALID-HANDLE(hAppSrv) THEN
DELETE PROCEDURE hAppSrv.
IF VALID-HANDLE(hJasper) THEN
DELETE PROCEDURE hJasper.

PROCEDURE pCreateAuditHdr:
    RUN spCreateAuditHdr (
        "TASK",
        "ASI",
        "Task",
        Task.rec_key,
        OUTPUT iAuditID
        ).
END PROCEDURE.
