/*------------------------------------------------------------------------
    File        : api\inbound\APIRequestRouterAS.p
    Purpose     : Calls relatedrequest handler 

    Syntax      :

    Description : Calls related request handler

    Author(s)   : Vishnu Vellanki
    Created     : Tue July 01 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcRoute           AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcVerb            AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcUsername        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcPassword        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcRequestDataType AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplcRequestData    AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT PARAMETER oplcResponseData   AS LONGCHAR  NO-UNDO.

DEFINE VARIABLE lcResponseDataStructure AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessage                AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSuccess                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cRequestedBy            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRecordSource           AS CHARACTER NO-UNDO INITIAL "APP Server".
DEFINE VARIABLE cNotes                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE riAPIInboundEvent       AS ROWID     NO-UNDO.
DEFINE VARIABLE cErrorMessage           AS CHARACTER NO-UNDO.

/* User Validation */
RUN UserAuthenticationCheck (
    INPUT   ipcUsername,
    INPUT   ipcPassword,
    OUTPUT  lSuccess,
    OUTPUT  cMessage
    ) NO-ERROR.

IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN
        cErrorMessage    = ERROR-STATUS:GET-MESSAGE(1)
        cMessage         = "Internal Server Error at AppServer (#8)"
        oplcResponseData = '~{ "response_code": 500, "response_message":"' + cMessage + '"}'
        .
     
    RUN api\CreateAPIInboundEvent.p (
        INPUT  ipcRoute,
        INPUT  iplcRequestData,
        INPUT  oplcResponseData,
        INPUT  lSuccess,
        INPUT  cMessage + " " + cErrorMessage,
        INPUT  NOW,
        INPUT  cRequestedBy,
        INPUT  cRecordSource,
        INPUT  cNotes,
        INPUT  "", /* PayloadID */
        OUTPUT riAPIInboundEvent
        ) NO-ERROR.
 
    RETURN.
END.

/* If the User Validation fails, sends reason for failure in reponse and logs APIInboundEvent table */ 
IF NOT lSuccess THEN DO:
    oplcResponseData = '~{ "response_code": 401, "response_message":"' + cMessage + '"}'.
     
    RUN api\CreateAPIInboundEvent.p (
        INPUT  ipcRoute,
        INPUT  iplcRequestData,
        INPUT  oplcResponseData,
        INPUT  lSuccess,
        INPUT  cMessage + cErrorMessage,
        INPUT  NOW,
        INPUT  cRequestedBy,
        INPUT  cRecordSource,
        INPUT  cNotes,
        INPUT  "", /* PayloadID */
        OUTPUT riAPIInboundEvent
        ) NO-ERROR.
 
    RETURN.
END.

/* API PROCESS */
FIND APIInbound NO-LOCK
     WHERE APIInbound.apiRoute     EQ ipcRoute 
       AND APIInbound.requestVerb  EQ ipcVerb 
       AND APIInbound.isActive
     NO-ERROR.
IF NOT AVAILABLE APIInbound THEN DO:
    ASSIGN 
        lSuccess         = NO
        cMessage         = "Config for API route " + ipcRoute + " not Found!"
        oplcResponseData = '~{ "response_code": 404, "response_message":"' + cMessage + '"}'
        .
        
    RUN api\CreateAPIInboundEvent.p (
        INPUT  ipcRoute,
        INPUT  iplcRequestData,
        INPUT  oplcResponseData,
        INPUT  lSuccess,
        INPUT  cMessage,
        INPUT  NOW,
        INPUT  cRequestedBy,
        INPUT  cRecordSource,
        INPUT  cNotes,
        INPUT  "", /* PayloadID */
        OUTPUT riAPIInboundEvent
        ) NO-ERROR.
        
    RETURN.
END.

lcResponseDataStructure = APIInbound.responseData.

/* Verifying if the request handler in APIInbound configuration is available */
IF SEARCH(APIInbound.requestHandler) EQ ? AND
   SEARCH(REPLACE(APIInbound.requestHandler,".p",".r")) EQ ? THEN DO:
    ASSIGN 
        lSuccess         = NO
        cMessage         = "Handler for API route " + ipcRoute + " not Found!"
        oplcResponseData = '~{ "response_code": 404, "response_message":"' + cMessage + '"}'
        .
        
    RUN api\CreateAPIInboundEvent.p (
         INPUT  ipcRoute,
         INPUT  iplcRequestData,
         INPUT  oplcResponseData,
         INPUT  lSuccess,
         INPUT  cMessage,
         INPUT  NOW,
         INPUT  cRequestedBy,
         INPUT  cRecordSource,
         INPUT  cNotes,
         INPUT  "", /* PayloadID */
         OUTPUT riAPIInboundEvent
         ) NO-ERROR. 
                 
    RETURN.
END.

/* Run the request handler program from the API Inbound configuration */
RUN VALUE(APIInbound.requestHandler)(
    INPUT  ipcRoute,
    INPUT  ipcVerb,
    INPUT  ipcRequestDataType,
    INPUT  iplcRequestData,
    INPUT  lcResponseDataStructure,
    INPUT  cRequestedBy,
    INPUT  cRecordSource,
    INPUT  cNotes,
    INPUT  ipcUsername,
    OUTPUT oplcResponseData,
    OUTPUT lSuccess,
    OUTPUT cMessage
    ) NO-ERROR.

 IF ERROR-STATUS:ERROR THEN DO:    
    ASSIGN 
        cErrorMessage    = ERROR-STATUS:GET-MESSAGE(1)
        lSuccess         = NO
        cMessage         = "Internal Server Error at AppServer (#9)"
        oplcResponseData = '~{ "response_code": 500, "response_message":"' + cMessage + '"}'
        .
    
    RUN api\CreateAPIInboundEvent.p (
         INPUT  ipcRoute,
         INPUT  iplcRequestData,
         INPUT  oplcResponseData,
         INPUT  lSuccess,
         INPUT  cMessage + " " + cErrorMessage,
         INPUT  NOW,
         INPUT  cRequestedBy,
         INPUT  cRecordSource,
         INPUT  cNotes,
         INPUT  "", /* PayloadID */
         OUTPUT riAPIInboundEvent
         ) NO-ERROR.    
    
    RETURN.
 END.

/* This procedure checks whether username and password are valid or not */
PROCEDURE UserAuthenticationCheck:
    DEFINE INPUT   PARAMETER ipcUsername    AS CHARACTER NO-UNDO.
    DEFINE INPUT   PARAMETER ipcPassword    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT  PARAMETER oplSuccess     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT  PARAMETER opcMessage     AS CHARACTER NO-UNDO.
    
    /* Checks users table */
    FIND FIRST users NO-LOCK
         WHERE users.user_id EQ ipcUsername
         NO-ERROR.
    IF NOT AVAILABLE users THEN DO:
        ASSIGN 
            oplSuccess = NO
            opcMessage = "Authentication Failed (#1)"
            .
        
        RETURN.
    END.
    
    /* Checks _user table */
    FIND FIRST _user NO-LOCK
         WHERE _user._Userid EQ users.user_id NO-ERROR.
    IF NOT AVAILABLE _user THEN DO:
        ASSIGN 
            oplSuccess = NO
            opcMessage = "Authentication Failed (#2)"
            .
               
        RETURN.
    END.
	
    /* Checks password is correct or not */      
    IF ENCODE(ipcPassword) NE _user._password THEN DO:
        ASSIGN 
            oplSuccess = NO
            opcMessage = "Authentication Failed (#3)"
            .
        
        RETURN.        
    END.

    /* Checks user is locked or not */
    IF users.isLocked THEN DO:
        ASSIGN 
            oplSuccess = NO
            opcMessage = "The user is locked. Please contact the systems administrator."
            .
                        
        RETURN.        
    END. 

    ASSIGN
        oplSuccess = YES
        opcMessage = "Success"
        .

END PROCEDURE.

