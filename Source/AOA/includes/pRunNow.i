/* pRunNow.i - rstark - 2.15.2019 */

PROCEDURE pRunNow:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Task Format, Title and Use Email Recipients
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTaskFormat AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTitle      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplRecipients AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE cCompany          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRecipients       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTaskerNotRunning AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iConfigID         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lSubmit           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lTaskerNotRunning AS LOGICAL   NO-UNDO.
    
    IF AVAILABLE {1}dynParamValue THEN DO:
        &IF DEFINED(silentSubmitted) EQ 0 &THEN
        RUN spGetSessionParam ("Company", OUTPUT cCompany).
        RUN sys/ref/nk1look.p (
            cCompany,"TaskerNotRunning","I",NO,NO,"","",
            OUTPUT cTaskerNotRunning,OUTPUT lTaskerNotRunning
            ).
        iConfigID = INTEGER(cTaskerNotRunning).
        IF CAN-FIND(FIRST emailConfig
                    WHERE emailConfig.configID EQ iConfigID
                      AND emailConfig.isActive EQ YES
                      AND emailConfig.notified EQ YES) THEN DO:
            MESSAGE 
                "Task Monitor Currently Not Running" SKIP(1)
                "OK to Submit Task?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE lSubmit.
            IF lSubmit EQ NO THEN RETURN.
        END. /* if notified */
        &ENDIF
        IF iplRecipients THEN
        RUN pGetRecipients (OUTPUT cRecipients).
        DO TRANSACTION:
            CREATE Task.
            ASSIGN
                Task.subjectID    = {1}dynParamValue.subjectID
                Task.user-id      = IF {1}dynParamValue.user-id NE "_default" THEN USERID("ASI")
                                    ELSE {1}dynParamValue.user-id
                Task.prgmName     = {1}dynParamValue.prgmName
                Task.paramValueID = {1}dynParamValue.paramValueID
                Task.module       = {1}dynParamValue.module
                Task.taskName     = "Run Now Task"
                Task.taskFormat   = ipcTaskFormat
                Task.runNow       = YES
                Task.recipients   = cRecipients
                Task.taskType     = "Jasper"
                .
            RELEASE Task.
        END. /* do trans */
        &IF DEFINED(silentSubmitted) EQ 0 &THEN
        MESSAGE
            "Task ~"" + ipcTitle + "~" has been submitted."
        VIEW-AS ALERT-BOX TITLE "Run Now".
        &ENDIF
    END. /* if avail */

END PROCEDURE.
