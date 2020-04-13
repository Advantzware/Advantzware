{api\ttArgs.i}

DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcLocation         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcAPIID            AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcClientID         AS CHARACTER NO-UNDO.  
DEFINE INPUT  PARAMETER ipcTriggerID        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableList        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcROWIDList        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrimaryID        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcEventDescription AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplReTrigger        AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opiOutboundEventID  AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.
    
DEFINE VARIABLE lcRequestData  AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcResponseData AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE cParentProgram AS CHARACTER NO-UNDO.  
DEFINE VARIABLE iCount         AS INTEGER   NO-UNDO.
    
EMPTY TEMP-TABLE ttArgs.

/* Start validation - The below validations are performed on required fields and any
   failure in the below validations will not be logged in APIOutboundEvent table */
IF ipcAPIID EQ "" THEN DO:
    ASSIGN
        opcMessage = "API ID cannot be empty"
        oplSuccess = FALSE
        .
    RETURN.
END.

IF ipcTableList EQ "" THEN DO:
    ASSIGN
        opcMessage = "Empty value passed in table list"
        oplSuccess = FALSE
        .
    RETURN.
END.

IF ipcROWIDList EQ "" THEN DO:
    ASSIGN
        opcMessage = "Empty value passed in ROW ID list"
        oplSuccess = FALSE
        .
    RETURN.
END.

IF NUM-ENTRIES(ipcTableList) NE NUM-ENTRIES(ipcROWIDList) THEN DO:
    ASSIGN
        opcMessage = "Mismatch of number of entries in table and rowid list"
        oplSuccess = FALSE
        .
    RETURN.
END.

IF ipcPrimaryID EQ "" THEN DO:
    ASSIGN
        opcMessage = "Empty value passed in Primary ID"
        oplSuccess = FALSE
        .
    RETURN.
END.
/* End validation */

DO iCount = 1 TO NUM-ENTRIES(ipcTableList):
    CREATE ttArgs.
    ASSIGN
        ttArgs.argType  = "ROWID"
        ttArgs.argKey   = ENTRY(iCount,ipcTableList)
        ttArgs.argValue = ENTRY(iCount,ipcROWIDList)
        .
END.

cParentProgram = IF NUM-ENTRIES(PROGRAM-NAME(2)," ") EQ 2 THEN 
                     ENTRY(2, PROGRAM-NAME(2), " ")
                 ELSE
                     PROGRAM-NAME(2).

/* Fetch request data from APIOutboundEvent record 
   in case the API is re-triggerred */
IF iplReTrigger THEN DO:
    FOR FIRST ttArgs 
        WHERE ttArgs.argType EQ "ROWID" 
          AND ttArgs.argKey  EQ "APIOutboundEvent"
          , 
          FIRST APIOutboundEvent EXCLUSIVE-LOCK
          WHERE ROWID(APIOutboundEvent) = TO-ROWID(ttArgs.argValue):
        ASSIGN 
            lcRequestData = APIOutboundEvent.RequestData
            oplSuccess    = TRUE
            .
    END.
    
    IF NOT oplSuccess THEN DO:
        opcMessage = "Re-trigger failed!".
        RETURN.
    END.
    
    FIND FIRST APIOutbound NO-LOCK
         WHERE APIOutbound.company  EQ APIOutboundEvent.company
           AND APIOutbound.apiID    EQ APIOutboundEvent.apiID
           AND APIOutbound.clientID EQ APIOutboundEvent.clientID
         NO-ERROR.         
    IF AVAILABLE APIOutbound AND
       NOT APIOutbound.Inactive THEN DO:
        
        /* Validate if location is API enabled (see APIEnabled toggle box in I-F-4 screen) */
        RUN ValidateLocation (
            INPUT  ipcCompany,
            INPUT  ipcLocation,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).        

        IF oplSuccess THEN
            /* If all good then make the API call */
            RUN api/CallOutBoundAPI.p (
                INPUT  APIOutbound.apiOutboundID,
                INPUT  lcRequestData,
                INPUT  cParentProgram,
                OUTPUT lcResponseData,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).

        RUN api/CreateAPIOutboundEvent.p (
            INPUT  iplReTrigger,                        /* Re-Trigger Event Flag - IF TRUE updates the existing APIOutboundEvent record */
            INPUT  APIOutboundEvent.apiOutboundEventID, /* apiOutboundEventID - Updates APIOutboundEvent record for the given ID. Pass ? for creating new event */
            INPUT  APIOutboundEvent.company,
            INPUT  ipcLocation,
            INPUT  APIOutboundEvent.apiID,
            INPUT  APIOutboundEvent.clientID,
            INPUT  APIOutboundEvent.sourceTriggerID,
            INPUT  APIOutboundEvent.primaryID,
            INPUT  APIOutboundEvent.eventDescription,
            INPUT  lcRequestData,
            INPUT  lcResponseData,
            INPUT  cParentProgram,
            INPUT  oplSuccess,
            INPUT  opcMessage,
            INPUT  NOW,
            OUTPUT opiOutboundEventID
            ).                  
    END.
    ELSE DO:
        ASSIGN
            opcMessage = "API Outbound configuration for Outbound Sequence ID [" 
                       + APIOutboundEvent.apiID
                       + "], is not available or inactive"
            oplSuccess = FALSE
            .    
    END.
    
    RETURN.
END.

/* If ipcClientID is sent as empty, call API request for all clients, else for the passed client ID */
FOR EACH APIOutbound NO-LOCK
   WHERE APIOutbound.company EQ ipcCompany
     AND APIOutbound.apiID   EQ ipcAPIID
     AND (IF ipcClientID EQ "" THEN
              TRUE
          ELSE
              APIOutbound.clientID EQ ipcClientID):

    IF APIOutbound.Inactive THEN
        NEXT.
        
    FIND FIRST APIOutboundTrigger NO-LOCK
         WHERE APIOutboundTrigger.apiOutboundID EQ APIOutbound.apiOutboundID
           AND APIOutboundTrigger.triggerID     EQ ipcTriggerID
           AND APIOutboundtrigger.Inactive      EQ FALSE
         NO-ERROR.
    IF NOT AVAILABLE APIOutboundTrigger THEN
        NEXT.

    RUN api/PrepareOutboundRequest.p (
        INPUT  TABLE ttArgs,
        INPUT  APIOutbound.apiOutboundID,
        INPUT  APIOutboundTrigger.apiOutboundTriggerID,    
        OUTPUT lcRequestData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).

    IF NOT oplSuccess THEN DO:
        RUN api/CreateAPIOutboundEvent.p (
            INPUT  iplReTrigger,                   /* Re-Trigger Event Flag - IF TRUE updates the existing APIOutboundEvent record for the given apiOutboundEventID */
            INPUT  ?,                              /* apiOutboundEventID - Updates APIOutboundEvent record for the given ID. Pass ? for creating new event */
            INPUT  APIOutbound.company,
            INPUT  ipcLocation,
            INPUT  APIOutbound.apiID,
            INPUT  APIOutbound.clientID,
            INPUT  APIOutboundTrigger.triggerID,
            INPUT  ipcPrimaryID,
            INPUT  ipcEventDescription,
            INPUT  lcRequestData,
            INPUT  "", /* Response Data */
            INPUT  cParentProgram,
            INPUT  oplSuccess,
            INPUT  opcMessage,
            INPUT  NOW,
            OUTPUT opiOutboundEventID
            ). 
             
        RETURN.
    END.
    
    /* Validate if location is API enabled (see APIEnabled toggle box in I-F-4 screen).
       Validate after preparing the request data, as this request will be available for re-triggering
       from API Outbound Events Viewer screen */
    RUN ValidateLocation (
        INPUT  ipcCompany,
        INPUT  ipcLocation,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ). 

    /* If all good then make the API call */
    IF oplSuccess AND APIOutbound.requestDataType NE "FTP" THEN
        RUN api/CallOutBoundAPI.p (
            INPUT  APIOutbound.apiOutboundID,
            INPUT  lcRequestData,
            INPUT  cParentProgram,
            OUTPUT lcResponseData,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).

    RUN api\CreateAPIOutboundEvent.p (
        INPUT  iplReTrigger,                /* Re-Trigger Event Flag - IF TRUE updates the existing APIOutboundEvent record */
        INPUT  ?,                           /* apiOutboundEventID - Updates APIOutboundEvent record for the given ID. Pass ? for creating new event */
        INPUT  APIOutbound.company,
        INPUT  APIOutbound.apiID,
        INPUT  ipcLocation,
        INPUT  APIOutbound.clientID,
        INPUT  APIOutboundTrigger.triggerID,
        INPUT  ipcPrimaryID,
        INPUT  ipcEventDescription,
        INPUT  lcRequestData,
        INPUT  lcResponseData,
        INPUT  cParentProgram,
        INPUT  oplSuccess,
        INPUT  opcMessage,
        INPUT  NOW,
        OUTPUT opiOutboundEventID
        ).          
END.     

PROCEDURE ValidateLocation:
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.
    
    FIND FIRST loc NO-LOCK
         WHERE loc.company EQ ipcCompany
           AND loc.loc     EQ ipcLocation
         NO-ERROR.
    IF NOT AVAILABLE loc OR NOT loc.isAPiEnabled THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "API Calls are not enabled for location '" 
                       + ipcLocation + "'"
            .
        RETURN.
    END.
    
    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.
