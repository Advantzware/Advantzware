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
    
    DEFINE VARIABLE cRecipients AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSubmit     AS LOGICAL   NO-UNDO.
    
    IF AVAILABLE {1}dynParamValue THEN DO:
        &IF DEFINED(silentSubmitted) EQ 0 &THEN
        FIND FIRST config NO-LOCK.
        IF config.taskerEmailSent THEN DO:
            MESSAGE 
                "Task Monitor Currently Not Running" SKIP(1)
                "OK to Submit Task?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE lSubmit.
            IF lSubmit EQ NO THEN RETURN.
        END. /* if tasker email sent */
        &ENDIF
        IF iplRecipients THEN
        RUN pGetRecipients (OUTPUT cRecipients).
        DO TRANSACTION:
            CREATE Task.
            ASSIGN
                Task.subjectID    = {1}dynParamValue.subjectID
                Task.user-id      = {1}dynParamValue.user-id
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
