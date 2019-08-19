/*------------------------------------------------------------------------
    File        : api\inbound\handlers\CreateInventoryTransferRequestHandler.p
    Purpose     : Creates Inventory Transfer 

    Syntax      :

    Description : Creates Inventory Transfer 

    Author(s)   : Vishnu Vellanki
    Created     : Tue July 05 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcRoute                  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcVerb                   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcRequestDataType        AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iplcRequestData           AS LONGCHAR   NO-UNDO.
DEFINE INPUT  PARAMETER iplcResponseDataStructure AS LONGCHAR   NO-UNDO.
DEFINE INPUT  PARAMETER ipcRequestedBy            AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcRecordSource           AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcNotes                  AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER oplcResponseData          AS LONGCHAR   NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess                AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER  NO-UNDO.

DEFINE VARIABLE hdttRequestData         AS HANDLE     NO-UNDO. 
DEFINE VARIABLE hdJSONProcs             AS HANDLE     NO-UNDO.
DEFINE VARIABLE hdttBuffer              AS HANDLE     NO-UNDO.
DEFINE VARIABLE hdttQuery               AS HANDLE     NO-UNDO.
DEFINE VARIABLE iCount                  AS INTEGER    NO-UNDO.
DEFINE VARIABLE cCompany                AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cWareHouseID            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLocationID             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cInventoryStockIDTag    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrimaryID              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cItemType               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE riAPIInboundEvent       AS ROWID      NO-UNDO.

/* This code will be removed once userauthentication validation happens in APIRequestRouterAS.p
   and then UserName will come as input parameter from APIRequestRouterAS.p if user authentication is success */
DEFINE VARIABLE cUserName AS CHARACTER  NO-UNDO.
cUserName = "user1".

{api/inbound/ttRequest.i}

RUN api/JSONProcs.p PERSISTENT SET hdJSONProcs. 

/* Get request data fields in a temp-table */
RUN ReadRequestData IN hdJSONProcs (
    INPUT  iplcRequestData,
    OUTPUT oplSuccess,
    OUTPUT opcMessage,
    OUTPUT TABLE ttRequest
    ).
        
IF NOT oplSuccess THEN DO:   
    oplcResponseData  = '~{"response_code": 400,"response_message":"' + opcMessage + '"}'.
    /* Log the request to APIInboundEvent */
    RUN api\CreateAPIInboundEvent.p (
        INPUT ipcRoute,
        INPUT iplcRequestData,
        INPUT oplcResponseData,
        INPUT oplSuccess,
        INPUT opcMessage,
        INPUT NOW,
        INPUT ipcRequestedBy,
        INPUT ipcRecordSource,
        INPUT ipcNotes,
        INPUT  "", /* PayloadID */
        OUTPUT riAPIInboundEvent
        ).

   RETURN.
END.        
        
/* This will fetch fields from request data */
FOR EACH ttRequest:
    CASE ttRequest.FieldName:
        WHEN "Company" THEN
           cCompany = ttRequest.FieldValue.
        WHEN "WareHouseID" THEN
           cWareHouseID = ttRequest.FieldValue.
        WHEN "LocationID" THEN
           cLocationID = ttRequest.FieldValue.
        WHEN "InventoryStockIDTag" THEN
           cInventoryStockIDTag = ttRequest.FieldValue.
        WHEN "PrimaryID" THEN
           cPrimaryID = ttRequest.FieldValue.
        WHEN "ItemType" THEN
           cItemType = ttRequest.FieldValue.
        WHEN "Requester" THEN
           ipcRequestedBy = ttRequest.FieldValue.
        WHEN "RequesterNotes" THEN
           ipcNotes = ttRequest.FieldValue.
    END CASE.
END.

/* This is to fetch response data*/ 
RUN api\inbound\CreateInventoryTransfer.p (
    INPUT  cCompany, 
    INPUT  cWareHouseID,
    INPUT  cLocationID, 
    INPUT  cInventoryStockIDTag,
    INPUT  cPrimaryID,
    INPUT  cItemType,
    INPUT  cUserName, 
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ).

IF NOT oplSuccess THEN
DO:
   oplcResponseData  = '~{"response_code": 400,"response_message":"' + opcMessage + '"}'.   
   
   /* Log the request to APIInboundEvent */
   RUN api\CreateAPIInboundEvent.p (
       INPUT ipcRoute,
       INPUT iplcRequestData,
       INPUT oplcResponseData,
       INPUT oplSuccess,
       INPUT opcMessage,
       INPUT NOW,
       INPUT ipcRequestedBy,
       INPUT ipcRecordSource,
       INPUT ipcNotes,
       INPUT  "", /* PayloadID */
       OUTPUT riAPIInboundEvent
       ).
   
   RETURN.  
END.
 
ASSIGN
    oplcResponseData = '~{"response_code":200,"response_message":"Inventory Transfer Is Success"}'
    opcMessage = "Success"
    .

/* Log the request to APIInboundEvent */
RUN api\CreateAPIInboundEvent.p (
    INPUT ipcRoute,
    INPUT iplcRequestData,
    INPUT oplcResponseData,
    INPUT oplSuccess,
    INPUT opcMessage,
    INPUT NOW,
    INPUT ipcRequestedBy,
    INPUT ipcRecordSource,
    INPUT ipcNotes,
    INPUT  "", /* PayloadID */
    OUTPUT riAPIInboundEvent
    ).
    
DELETE PROCEDURE hdJSONProcs.

                                                      
