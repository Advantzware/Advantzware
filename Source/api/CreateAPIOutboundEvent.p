USING System.SharedConfig.

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

DEFINE VARIABLE lcNotes              AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lSuccess             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage             AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcRequestFile        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAPIOutboundTestMode AS LOGICAL   NO-UNDO.

lAPIOutboundTestMode = LOGICAL(SharedConfig:Instance:GetValue("APIOutboundTestMode")) NO-ERROR.

FIND FIRST APIOutboundEvent EXCLUSIVE-LOCK
     WHERE APIOutboundEvent.apiOutboundEventID EQ ipiOutboundEventID
     NO-ERROR.
IF NOT AVAILABLE APIOutboundEvent AND iplReTrigger THEN
    RETURN. /* Do not create APIOutboundEvent in case of invalid re-trigger event */
    
IF NOT AVAILABLE APIOutboundEvent THEN DO:
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
        .    
END.
    
ASSIGN
    lcNotes                          = APIOutboundEvent.notes
    APIOutboundEvent.responseData    = iplcReponseData
    APIOutboundEvent.callingProgram  = ipcProgramName
    APIOutboundEvent.success         = iplSuccess
    APIOutboundEvent.errorMessage    = ipcMessage
    APIOutboundEvent.requestDateTime = ipcDateTime
    APIOutboundEvent.userField1      = SharedConfig:Instance:ConsumeValue("APIOutboundEvent_UserField1")
    APIOutboundEvent.userField2      = SharedConfig:Instance:ConsumeValue("APIOutboundEvent_UserField2")        
    APIOutboundEvent.notes           = lcNotes
                                     + (IF NEW APIOutboundEvent THEN
                                            ""
                                        ELSE
                                            "~n")
                                     + STRING(NOW, "99/99/9999 HH:MM:SS.SSS")
                                     + " - "
                                     + USERID("ASI")
                                     + " - "
                                     + (IF NEW APIOutboundEvent THEN
                                            "Initial Trigger"
                                        ELSE
                                            "Subsequent Trigger")
                                     + " - "
                                     + STRING(iplSuccess, "SUCCESS/FAILURE")
                                     + " - "
                                     + ipcMessage
    .

opiOutboundEventID = APIOutboundEvent.apiOutboundEventID.

