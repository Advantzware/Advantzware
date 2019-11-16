/*------------------------------------------------------------------------

  File: testers/APIInboundTester.p

  Description: API Inbound Tester (Wrapper for api\inbound\APIRequestRouterAS.p)

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Mithun Porandla

  Created: 10/17/2019

------------------------------------------------------------------------*/
DEFINE VARIABLE cRoute             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVerb              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUsername          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPassword          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRequestDataType   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRequestData      AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE cRecordSource      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcResponseData     AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE cAPIInboundEvent   AS CHARACTER NO-UNDO.

ASSIGN
    cRoute           = "/api/getinventory"
    cVerb            = "POST"
    cUsername        = "user1"
    cPassword        = "user1"
    cRequestDataType = "JSON"
    lcRequestData    = '~{"Requestedby":"Premier","Company":"001","WareHouseID":"BENPA","LocationID":"DEFBIN","InventoryStockID":"10X10X10       00231","PrimaryID":"10x10x10","ItemType":"FG"}'
    cRecordSource    = "NodeServer"
    .    

/* Request call to API */    
RUN api\inbound\APIRequestRouterAS.p (
    /* API Inbound Route */
    INPUT  cRoute,
    /* API Request Verb. Eg. POST, GET */
    INPUT  cVerb,
    /* ASI Username */
    INPUT  cUserName,
    /* Password */
    INPUT  cPassword,
    /* API Request Data Type. Eg. JSON, XML */
    INPUT  cRequestDataType,
    /* API Request Data. Actual request which should be passed 
    as input to api route */
    INPUT  lcRequestData,
    /* Source of the request. Requests sent from Node send "NodeServer"
    as record source. Offline Loader uses "Offline" as record source */
    INPUT  cRecordSource,     
    /* Response data after processing the request */
    OUTPUT lcResponseData,
    /* ROWID of the APIInboundEvent created */
    OUTPUT cAPIInboundEvent
    ) NO-ERROR.

FIND FIRST APIInboundEvent NO-LOCK
     WHERE ROWID(APIInboundEvent) EQ TO-ROWID(cAPIInboundEvent) 
     NO-ERROR.
     
MESSAGE "APIInboundEventID Created:" APIInboundEvent.APIInboundEventID SKIP
        "Response:" SKIP
        STRING(lcResponseData) VIEW-AS ALERT-BOX.   
