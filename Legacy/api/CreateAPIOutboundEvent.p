DEFINE INPUT PARAMETER ipcAPIID        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplcRequestData AS LONGCHAR  NO-UNDO.
DEFINE INPUT PARAMETER iplcReponseData AS LONGCHAR  NO-UNDO.
DEFINE INPUT PARAMETER ipcProgramName  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplSuccess      AS LOGICAL   NO-UNDO.
DEFINE INPUT PARAMETER ipcMessage      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcDateTime     AS DATETIME  NO-UNDO.

CREATE APIOutboundEvent.
ASSIGN
    APIOutboundEvent.apiID           = ipcAPIID
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
