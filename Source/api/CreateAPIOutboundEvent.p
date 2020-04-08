DEFINE INPUT  PARAMETER iplReTrigger        AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipiOutboundEventID  AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcLocation         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcAPIID            AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcClientID         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTriggerID        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrimaryID        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcEventDescription AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplcRequestData     AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER iplcReponseData     AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER ipcProgramName      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplSuccess          AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcMessage          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcDateTime         AS DATETIME  NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutboundEventID  AS INTEGER   NO-UNDO.

DEFINE VARIABLE lcNotes        AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lSuccess       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdFileSysProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE gcRequestFile  AS CHARACTER NO-UNDO.

RUN system/FileSysProcs.p PERSISTENT SET hdFileSysProcs.

FIND FIRST APIOutboundEvent EXCLUSIVE-LOCK
     WHERE APIOutboundEvent.apiOutboundEventID EQ ipiOutboundEventID
     NO-ERROR.
IF NOT AVAILABLE APIOutboundEvent AND iplReTrigger THEN
    RETURN. /* Do not create APIOutboundEvent in case of invalid re-trigger event */
    
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
        APIOutboundEvent.company          = ipcCompany
        APIOutboundEvent.locationID       = ipcLocation
        APIOutboundEvent.apiID            = ipcAPIID
        APIOutboundEvent.clientID         = ipcClientID
        APIOutboundEvent.sourceTriggerID  = ipcTriggerID
        APIOutboundEvent.primaryID        = ipcPrimaryID
        APIOutboundEvent.eventDescription = ipcEventDescription
        APIOutboundEvent.requestData      = iplcRequestData
        APIOutboundEvent.responseData     = iplcReponseData
        APIOutboundEvent.callingProgram   = ipcProgramName
        APIOutboundEvent.success          = iplSuccess
        APIOutboundEvent.errorMessage     = ipcMessage
        APIOutboundEvent.requestDateTime  = ipcDateTime
        APIOutboundEvent.notes            = STRING(ipcDateTime) 
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

FIND FIRST APIOutbound NO-LOCK
     WHERE APIOutbound.company  EQ ipcCompany 
       AND APIOutbound.apiId    EQ ipcAPIID
       AND APIOutbound.clientID EQ ipcClientID
       AND APIOutbound.isActive
     NO-ERROR.
IF AVAILABLE APIOutbound AND APIOutbound.SaveFile THEN DO:
    RUN FileSys_CreateDirectory IN hdFileSysProcs (
        INPUT  APIOutbound.SaveFileFolder,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ) NO-ERROR.
    IF lSuccess THEN DO:    
        gcRequestFile = STRING(opiOutboundEventID) + "." + APIOutbound.requestDatatype.     
        COPY-LOB iplcRequestData TO FILE gcRequestFile.
        OS-COPY VALUE (gcRequestFile) VALUE (APIOutbound.saveFileFolder).
        OS-DELETE VALUE(gcRequestFile).
    END.    
END. 
IF VALID-HANDLE(hdFileSysProcs) THEN
    DELETE PROCEDURE hdFileSysProcs. 