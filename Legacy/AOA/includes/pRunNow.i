/* pRunNow.i - rstark - 2.15.2019 */

PROCEDURE pRunNow:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTaskFormat AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTitle      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplRecipients AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE cRecipients AS CHARACTER NO-UNDO.
    
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
    MESSAGE
        "Task ~"" + ipcTitle + "~" has been submitted."
    VIEW-AS ALERT-BOX TITLE "Run Now".

END PROCEDURE.
