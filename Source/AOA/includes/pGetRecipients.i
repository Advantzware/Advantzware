/* pGetRecipients.i - rstark - 3.15.2021 */

PROCEDURE pGetRecipients:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcRecipients AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    FIND FIRST dynValueParam NO-LOCK
         WHERE dynValueParam.subjectID    EQ {1}dynParamValue.subjectID
           AND dynValueParam.user-id      EQ {1}dynParamValue.user-id
           AND dynValueParam.prgmName     EQ {1}dynParamValue.prgmName
           AND dynValueParam.paramValueID EQ {1}dynParamValue.paramValueID
           AND dynValueParam.paramName    EQ "svRecipients"
         NO-ERROR.
    IF AVAILABLE dynValueParam THEN
    opcRecipients = dynValueParam.paramValue.
    IF opcRecipients NE "" THEN DO:
        MESSAGE
            "Recipients:" opcRecipients SKIP(1)
            "Email Results?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE lUseEmail AS LOGICAL.
        IF lUseEmail EQ NO THEN
        opcRecipients = "".
    END. /* if */

END PROCEDURE.
