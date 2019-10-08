DEFINE INPUT  PARAMETER ipiOutboundEventID  AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER iplcRequestData     AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER iplcResponseData    AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER ipcNotesMessage     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplSuccess          AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcMessage          AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcNotes AS LONGCHAR NO-UNDO.

FIND FIRST APIOutboundEvent EXCLUSIVE-LOCK
     WHERE APIOutboundEvent.apiOutboundEventID EQ ipiOutboundEventID
     NO-ERROR.    
IF AVAILABLE APIOutboundEvent THEN DO:
    ASSIGN
        lcNotes                       = APIOutboundEvent.notes
        APIOutboundEvent.requestData  = iplcRequestData
        APIOutboundEvent.responseData = iplcResponseData
        APIOutboundEvent.success      = iplSuccess
        APIOutboundEvent.errorMessage = ipcMessage
        APIOutboundEvent.notes        = lcNotes
                                      + "~n"
                                      + STRING(NOW, "99/99/9999 HH:MM:SS.SSS")
                                      + " - "
                                      + USERID("ASI")
                                      + " - "
                                      + ipcNotesMessage
        .
END.     
