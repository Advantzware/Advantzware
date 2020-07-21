/*------------------------------------------------------------------------
    File        : api\inbound\APIRequestRouterAS.p
    Purpose     : Calls relatedrequest handler 

    Syntax      :

    Description : Calls related request handler

    Author(s)   : Vishnu Vellanki
    Created     : Tue July 01 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcRoute             AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcVerb              AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcUsername          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcPassword          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcRequestDataType   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplcRequestData      AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER ipcRecordSource      AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplcResponseData     AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT PARAMETER opcAPIInboundEvent   AS CHARACTER NO-UNDO.

DEFINE VARIABLE cResponseDataStructure  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessage                AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSuccess                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cRequestedBy            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNotes                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErrorMessage           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPassword               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRetrigger              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iAPIInboundEventID      AS INTEGER   NO-UNDO INITIAL ?.
DEFINE VARIABLE cCompany                AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdSession AS HANDLE NO-UNDO.

RUN system/session.p  PERSISTENT SET hdSession.
SESSION:ADD-SUPER-PROCEDURE (hdSession).

/* When this procedure is called from within ASI application 
   ( for offline load of queued requests, when the AppServer is down )
   then, currently logged user's password is passed as input 
   which is already encoded, so no need to encode */
cPassword = IF ipcRecordSource EQ "offline" THEN
                ipcPassword
            ELSE
                ENCODE(ipcPassword).

/* API PROCESS */
FIND APIInbound NO-LOCK
     WHERE APIInbound.apiRoute     EQ ipcRoute 
       AND APIInbound.requestVerb  EQ ipcVerb 
       AND NOT APIInbound.Inactive
     NO-ERROR.
IF NOT AVAILABLE APIInbound THEN DO:
    ASSIGN 
        lSuccess         = NO
        cMessage         = "Config for API route " + ipcRoute + " not Found!"
        oplcResponseData = '~{ "response_code": 404, "response_message":"' + cMessage + '"}'
        .
        
    RUN api\CreateAPIInboundEvent.p (
        INPUT  lRetrigger,
        INPUT  iAPIInboundEventID,
        INPUT  cCompany,
        INPUT  ipcRoute,
        INPUT  iplcRequestData,
        INPUT  oplcResponseData,
        INPUT  lSuccess,
        INPUT  cMessage,
        INPUT  NOW,
        INPUT  cRequestedBy,
        INPUT  ipcRecordSource,
        INPUT  cNotes,
        INPUT  "", /* PayloadID */
        OUTPUT opcAPIInboundEvent
        ) NO-ERROR.
        
    RETURN.
END.

ASSIGN
    cResponseDataStructure = APIInbound.responseData
    cCompany               = APIInbound.company
    .
    
/* User Validation */                
RUN UserAuthenticationCheck (
    INPUT   ipcUsername,
    INPUT   cPassword,
    OUTPUT  lSuccess,
    OUTPUT  cMessage
    ) NO-ERROR.

IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN
        cErrorMessage    = ERROR-STATUS:GET-MESSAGE(1)
        cMessage         = "Internal Server Error at AppServer (#8) - " + cErrorMessage
        oplcResponseData = '~{ "response_code": 500, "response_message":"' + cMessage + '"}'
        .
       
    RUN api\CreateAPIInboundEvent.p (
        INPUT  lRetrigger,
        INPUT  iAPIInboundEventID,
        INPUT  cCompany,
        INPUT  ipcRoute,
        INPUT  iplcRequestData,
        INPUT  oplcResponseData,
        INPUT  lSuccess,
        INPUT  cMessage + " " + cErrorMessage,
        INPUT  NOW,
        INPUT  cRequestedBy,
        INPUT  ipcRecordSource,
        INPUT  cNotes,
        INPUT  "", /* PayloadID */
        OUTPUT opcAPIInboundEvent
        ) NO-ERROR.
 
    RETURN.
END.

/* If the User Validation fails, sends reason for failure in reponse and logs APIInboundEvent table */ 
IF NOT lSuccess THEN DO:
    oplcResponseData = '~{ "response_code": 401, "response_message":"' + cMessage + '"}'.
     
    RUN api\CreateAPIInboundEvent.p (
        INPUT  lRetrigger,
        INPUT  iAPIInboundEventID,
        INPUT  cCompany,
        INPUT  ipcRoute,
        INPUT  iplcRequestData,
        INPUT  oplcResponseData,
        INPUT  lSuccess,
        INPUT  cMessage + cErrorMessage,
        INPUT  NOW,
        INPUT  cRequestedBy,
        INPUT  ipcRecordSource,
        INPUT  cNotes,
        INPUT  "", /* PayloadID */
        OUTPUT opcAPIInboundEvent
        ) NO-ERROR.
 
    RETURN.
END.

/* Verifying if the request handler in APIInbound configuration is available */
IF SEARCH(APIInbound.requestHandler) EQ ? AND
   SEARCH(REPLACE(APIInbound.requestHandler,".p",".r")) EQ ? THEN DO:
    ASSIGN 
        lSuccess         = NO
        cMessage         = "Handler for API route " + ipcRoute + " not Found!"
        oplcResponseData = '~{ "response_code": 404, "response_message":"' + cMessage + '"}'
        .
        
    RUN api\CreateAPIInboundEvent.p (
        INPUT  lRetrigger,
        INPUT  iAPIInboundEventID,
        INPUT  cCompany,
        INPUT  ipcRoute,
        INPUT  iplcRequestData,
        INPUT  oplcResponseData,
        INPUT  lSuccess,
        INPUT  cMessage,
        INPUT  NOW,
        INPUT  cRequestedBy,
        INPUT  ipcRecordSource,
        INPUT  cNotes,
        INPUT  "", /* PayloadID */
        OUTPUT opcAPIInboundEvent
         ) NO-ERROR. 
                 
    RETURN.
END.

/* Set the user id of asi database to current user name */
SETUSERID(ipcUsername,ipcPassword,LDBNAME(1)).

/* Run the request handler program from the API Inbound configuration */
RUN VALUE(APIInbound.requestHandler)(
    INPUT  ipcRoute,
    INPUT  ipcVerb,
    INPUT  ipcRequestDataType,
    INPUT  iplcRequestData,
    INPUT  cResponseDataStructure,
    INPUT  cRequestedBy,
    INPUT  ipcRecordSource,
    INPUT  cNotes,
    INPUT  ipcUsername,
    OUTPUT oplcResponseData,
    OUTPUT lSuccess,
    OUTPUT cMessage,
    OUTPUT opcAPIInboundEvent
    ) NO-ERROR.

 IF ERROR-STATUS:ERROR THEN    
    ASSIGN 
        cErrorMessage    = ERROR-STATUS:GET-MESSAGE(1)
        lSuccess         = NO
        cMessage         = "Internal Server Error at AppServer (#9) - " + cErrorMessage
        oplcResponseData = '~{ "response_code": 500, "response_message":"' + cMessage + '"}'
        .
    
RUN api\CreateAPIInboundEvent.p (
    INPUT  lRetrigger,
    INPUT  iAPIInboundEventID,
    INPUT  cCompany,
    INPUT  ipcRoute,
    INPUT  iplcRequestData,
    INPUT  oplcResponseData,
    INPUT  lSuccess,
    INPUT  cMessage + " " + cErrorMessage,
    INPUT  NOW,
    INPUT  cRequestedBy,
    INPUT  ipcRecordSource,
    INPUT  cNotes,
    INPUT  "", /* PayloadID */
    OUTPUT opcAPIInboundEvent
    ) NO-ERROR.
     
SESSION:REMOVE-SUPER-PROCEDURE (hdSession).
DELETE PROCEDURE hdSession.

/* This procedure checks whether username and password are valid or not */
PROCEDURE UserAuthenticationCheck:
    DEFINE INPUT   PARAMETER ipcUsername    AS CHARACTER NO-UNDO.
    DEFINE INPUT   PARAMETER ipcPassword    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT  PARAMETER oplSuccess     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT  PARAMETER opcMessage     AS CHARACTER NO-UNDO.
    
    /* Checks users table */
    FIND FIRST ASI.users NO-LOCK
         WHERE ASI.users.user_id EQ ipcUsername
         NO-ERROR.
    IF NOT AVAILABLE ASI.users THEN DO:
        ASSIGN 
            oplSuccess = NO
            opcMessage = "Authentication Failed (#1) - user is not available"
            .
        
        RETURN.
    END.
    
    /* Checks _user table */
    FIND FIRST ASI._user NO-LOCK
         WHERE ASI._user._Userid EQ ASI.users.user_id NO-ERROR.
    IF NOT AVAILABLE ASI._user THEN DO:
        ASSIGN 
            oplSuccess = NO
            opcMessage = "Authentication Failed (#2) - Internal DB user is not available (_user)"
            .
               
        RETURN.
    END.
	
    /* Checks password is correct or not */      
    IF  ASI._user._password NE ipcPassword THEN DO:
        ASSIGN 
            oplSuccess = NO
            opcMessage = "Authentication Failed (#3) - Incorrect password"
            .
        
        RETURN.
    END.

    /* Checks user is locked or not */
    IF ASI.users.isLocked THEN DO:
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

