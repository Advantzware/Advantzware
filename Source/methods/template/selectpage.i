/* This include file should be added as a library in window type smart objects only */
/* This will block the user to switch between the tabs if an update is in progress */
/* A SCOPED-DEFINE variable with name EXCLUDE-select-page is to be define
   in the window object */
 
DEFINE VARIABLE lRecordUpdating AS LOGICAL NO-UNDO.

PROCEDURE SetUpdateEnd :
/*------------------------------------------------------------------------------
  Purpose: Procedure to let window object know a record update finished    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    lRecordUpdating = FALSE.
END PROCEDURE.

PROCEDURE SetUpdateBegin :
/*------------------------------------------------------------------------------
  Purpose: Procedure to let window object know a record update started 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    lRecordUpdating = TRUE.
END PROCEDURE.

&IF DEFINED(EXCLUDE-select-page) NE 0 &THEN
PROCEDURE select-page :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-page# AS INTEGER NO-UNDO.

    IF lRecordUpdating THEN DO:
        RUN displayMessage (
            INPUT 38 /* zMessageID */
            ).
        
        RETURN.
    END.

    /* Do not edit/remove the below code. This is calling standard procedure to
       select a page in the folder */ 
    RUN broker-select-page IN adm-broker-hdl (
        INPUT THIS-PROCEDURE,
        INPUT p-page#
        ) NO-ERROR.
END PROCEDURE.
&ENDIF
