DEFINE INPUT  PARAMETER ipiOutboundEventID AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcAPIID           AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcClientID        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTriggerID       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplcRequestData    AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER iplcReponseData    AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER ipcProgramName     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplSuccess         AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcMessage         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcDateTime        AS DATETIME  NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutboundEventID AS INTEGER   NO-UNDO.

DEFINE VARIABLE lcNotes        AS LONGCHAR  NO-UNDO.

FIND FIRST APIOutboundEvent EXCLUSIVE-LOCK
     WHERE APIOutboundEvent.apiOutboundEventID EQ ipiOutboundEventID
       AND APIOutboundEvent.apiOutboundEventID NE 0
     NO-ERROR.
IF AVAILABLE APIOutboundEvent THEN DO:
    ASSIGN
        lcNotes                          = APIOutboundEvent.notes
        APIOutboundEvent.responseData    = iplcReponseData
        APIOutboundEvent.callingProgram  = ipcProgramName
        APIOutboundEvent.success         = iplSuccess
        APIOutboundEvent.errorMessage    = ipcMessage
        APIOutboundEvent.requestDateTime = ipcDateTime
        APIOutboundEvent.notes           = lcNotes
                                         + "~n"
                                         + STRING(NOW, "99/99/9999 HH:MM:SS.SSS")
                                         + " - "
                                         + USERID("ASI")
                                         + " - "
                                         + "Subsequent Trigger"
                                         + " - "
                                         + STRING(iplSuccess, "SUCCESS/FAILURE")
                                         + " - "
                                         + ipcMessage
        .
END.     
ELSE DO:
    CREATE APIOutboundEvent.
    ASSIGN
        APIOutboundEvent.company         = ipcCompany
        APIOutboundEvent.apiID           = ipcAPIID
        APIOutboundEvent.clientID        = ipcClientID
        APIOutboundEvent.sourceTriggerID = ipcTriggerID
        APIOutboundEvent.requestData     = iplcRequestData
        APIOutboundEvent.responseData    = iplcReponseData
        APIOutboundEvent.callingProgram  = ipcProgramName
        APIOutboundEvent.success         = iplSuccess
        APIOutboundEvent.errorMessage    = ipcMessage
        APIOutboundEvent.requestDateTime = ipcDateTime
        APIOutboundEvent.notes           = STRING(ipcDateTime) 
                                         + " - " 
                                         + USERID("ASI") 
                                         + " - "
                                         + "Initial Trigger"
                                         + " - "
                                         + STRING(iplSuccess, "SUCCESS/FAILURE")
                                         + " - "
                                         + ipcMessage
        .
END.

opiOutboundEventID = APIOutboundEvent.apiOutboundEventID.
