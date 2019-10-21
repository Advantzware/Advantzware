/*------------------------------------------------------------------------
    File        : api\inbound\handlers\CreateTagForPORequestHandler.p
    Purpose     : Process request data for creates tag for po 

    Syntax      :

    Description : Process request data for creates tag for po

    Author(s)   : Vishnu Vellanki
    Created     : Tue Oct 11 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcRoute                  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcVerb                   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcRequestDataType        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplcRequestData           AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER iplcResponseDataStructure AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER ipcRequestedBy            AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcRecordSource           AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcNotes                  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcUsername               AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplcResponseData          AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess                AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcAPIInboundEvent        AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdttRequestData     AS HANDLE     NO-UNDO. 
DEFINE VARIABLE hdJSONProcs         AS HANDLE     NO-UNDO.
DEFINE VARIABLE hdttBuffer          AS HANDLE     NO-UNDO.
DEFINE VARIABLE hdttQuery           AS HANDLE     NO-UNDO.
DEFINE VARIABLE iCount              AS INTEGER    NO-UNDO.
DEFINE VARIABLE cCompany            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iPONo               AS INTEGER    NO-UNDO.
DEFINE VARIABLE iPOLine             AS INTEGER    NO-UNDO.
DEFINE VARIABLE cPrimaryID          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cItemType           AS CHARACTER  NO-UNDO. 
DEFINE VARIABLE dQuantity           AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dQuantityPerSubUnit AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cStockIDAlias       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cInventoryStockID   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCreateReceipt      AS CHARACTER  NO-UNDO.

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
        OUTPUT opcAPIInboundEvent
        ).

   RETURN.
END.        
        
/* This will fetch fields from request data */
FOR EACH ttRequest:
    CASE ttRequest.FieldName:
        WHEN "Company" THEN
           cCompany = ttRequest.FieldValue.
        WHEN "PONo" THEN
           iPONo = INT(ttRequest.FieldValue).
        WHEN "POLine" THEN
           iPOLine = INT(ttRequest.FieldValue).
        WHEN "PrimaryID" THEN
           cPrimaryID = ttRequest.FieldValue.
        WHEN "ItemType" THEN
           cItemType = ttRequest.FieldValue.
        WHEN "Quantity" THEN
           dQuantity = INT(ttRequest.FieldValue).
        WHEN "QuantityPerSubUnit" THEN
           dQuantityPerSubUnit = INT(ttRequest.FieldValue).
        WHEN "StockIDAlias" THEN
           cStockIDAlias = ttRequest.FieldValue.
        WHEN "CreateReceipt" THEN
           cCreateReceipt = ttRequest.FieldValue.
        WHEN "Requester" THEN
           ipcRequestedBy = ttRequest.FieldValue.
        WHEN "RequesterNotes" THEN
           ipcNotes = ttRequest.FieldValue.
    END CASE.
END.

/* This is to fetch response data*/ 
RUN api\inbound\CreateTagForPO.p (
    INPUT  cCompany, 
    INPUT  iPONo,
    INPUT  iPOLine, 
    INPUT  cPrimaryID,
    INPUT  cItemType,
    INPUT  dQuantity,
    INPUT  dQuantityPerSubUnit,  
    INPUT  cStockIDAlias,
    INPUT  ipcUsername,
    INPUT  cCreateReceipt,
    OUTPUT cInventoryStockID,  
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ).

IF NOT oplSuccess THEN
DO:
   ASSIGN 
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
       OUTPUT opcAPIInboundEvent
       ).
   
   RETURN.  
END.
 
ASSIGN
    oplcResponseData = iplcResponseDataStructure
    oplcResponseData = REPLACE(oplcResponseData,"$InventoryStockID$",cInventoryStockID)
    opcMessage = "Success"
    oplcResponseData  = '~{"response_code": 200,"response_message":"' + opcMessage + '","response_data":[' + oplcResponseData + ']}'. 
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
    OUTPUT opcAPIInboundEvent
    ).
    
DELETE PROCEDURE hdJSONProcs.

                                                      
