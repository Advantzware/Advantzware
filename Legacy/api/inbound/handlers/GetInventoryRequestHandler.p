/*------------------------------------------------------------------------
    File        : api/inbound/handlers/GetInventoryRequestHandler.p
    Purpose     : Prepares Inventory response data for the given input data

    Syntax      :

    Description : Prepares Inventory response data for the given input data

    Author(s)   : Vishnu Vellanki
    Created     : Tue July 05 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
{api/inbound/ttfgbin.i}
{api/inbound/ttRequest.i}

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
DEFINE VARIABLE lcResponseData          AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE lcConcatResponseData    AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE cCompany                AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cWareHouseID            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLocationID             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cInventoryStockID       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrimaryID              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cItemType               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE riAPIInboundEvent       AS ROWID      NO-UNDO.

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
        INPUT  ipcRoute,
        INPUT  iplcRequestData,
        INPUT  oplcResponseData,
        INPUT  oplSuccess,
        INPUT  opcMessage,
        INPUT  NOW,
        INPUT  ipcRequestedBy,
        INPUT  ipcRecordSource,
        INPUT  ipcNotes,
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
        WHEN "InventoryStockID" THEN
           cInventoryStockID = ttRequest.FieldValue.
        WHEN "PrimaryID" THEN
           cPrimaryID = ttRequest.FieldValue.
        WHEN "ItemType" THEN
           cItemType = ttRequest.FieldValue.
        WHEN "Requestedby" THEN
           ipcRequestedBy = ttRequest.FieldValue.
    END CASE.
END.

/* This is to fetch response data*/
RUN api\inbound\GetInventoryDetails.p (
    INPUT  cCompany, 
    INPUT  cWareHouseID,
    INPUT  cLocationID, 
    INPUT  cInventoryStockID,
    INPUT  cPrimaryID,
    INPUT  cItemType,
    OUTPUT oplSuccess,
    OUTPUT opcMessage,
    OUTPUT TABLE ttfgbin
    ).
         
IF NOT oplSuccess THEN
DO:
   oplcResponseData  = '~{"response_code": 400,"response_message":"' + opcMessage + '"}'.   
   /* Log the request to APIInboundEvent */
   RUN api\CreateAPIInboundEvent.p (
       INPUT  ipcRoute,
       INPUT  iplcRequestData,
       INPUT  oplcResponseData,
       INPUT  oplSuccess,
       INPUT  opcMessage,
       INPUT  NOW,
       INPUT  ipcRequestedBy,
       INPUT  ipcRecordSource,
       INPUT  ipcNotes,
       INPUT  "", /* PayloadID */
       OUTPUT riAPIInboundEvent
       ).
   
   RETURN.  
END.

/* Prepares response using response data from API Inbound configuration*/
FOR EACH ttfgbin NO-LOCK:
    ASSIGN
        lcResponseData       = iplcResponseDataStructure
        lcResponseData       = REPLACE(lcResponseData, "fg-bin.tag", ttfgbin.InventoryStockID)
        lcResponseData       = REPLACE(lcResponseData, "fg-bin.i-no", ttfgbin.PrimaryID)
        lcResponseData       = REPLACE(lcResponseData, "loadtag.misc-char[1]", ttfgbin.StockIDAlias)
        lcResponseData       = REPLACE(lcResponseData, "fg-bin.qty", STRING(ttfgbin.Quantity))
        lcResponseData       = REPLACE(lcResponseData, "fg-bin.pur-uom", ttfgbin.QuantityUOM)
        lcResponseData       = REPLACE(lcResponseData, "fg-bin.cases-unit", STRING(ttfgbin.QuantityPerSubUnit))
        lcResponseData       = REPLACE(lcResponseData, "fg-bin.case-count", STRING(ttfgbin.QuantitySubUnitsPerUnit))
        lcResponseData       = REPLACE(lcResponseData, "fg-bin.partial-total", STRING(ttfgbin.QuantityPartial))
        lcConcatResponseData = lcConcatResponseData + "," + lcResponseData
        .
END.

ASSIGN
    lcConcatResponseData  = TRIM(lcConcatResponseData,",") 
    oplcResponseData = '~{"response_code":200,"response_message":"This is InventoryStock response message","response_data":[' + lcConcatResponseData + ']}'
    opcMessage = "Success"
    .

/* Log the request to APIInboundEvent */
RUN api\CreateAPIInboundEvent.p (
    INPUT  ipcRoute,
    INPUT  iplcRequestData,
    INPUT  oplcResponseData,
    INPUT  oplSuccess,
    INPUT  opcMessage,
    INPUT  NOW,
    INPUT  ipcRequestedBy,
    INPUT  ipcRecordSource,
    INPUT  ipcNotes,
    INPUT  "", /* PayloadID */
    OUTPUT riAPIInboundEvent
    ).
    
DELETE PROCEDURE hdJSONProcs.

                                                      
