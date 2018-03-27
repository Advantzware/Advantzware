/* crmProcs.i */

PROCEDURE pGetAuthToken:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  input company, output authoriaztion token
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAuthToken AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

    RUN sys/ref/nk1look.p (ipcCompany, "CRMAuthToken", "C", NO, NO, "", "",
                           OUTPUT opcAuthToken, OUTPUT lFound).
    RETURN opcAuthToken.
END PROCEDURE.

PROCEDURE pGetConnection:
    /*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  output web service connection
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcConnection AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

    RUN sys/ref/nk1look.p ("", "ASIHelpService", "C", NO, NO, "", "",
                           OUTPUT opcConnection, OUTPUT lFound).
    RETURN opcConnection.
END PROCEDURE.
