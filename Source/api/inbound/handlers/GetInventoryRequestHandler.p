/*------------------------------------------------------------------------
    File        : api/inbound/handlers/GetInventoryRequestHandler.p
    Purpose     : Prepares Inventory response data for the given input data

    Syntax      :

    Description : Prepares Inventory response data for the given input data

    Author(s)   : Vishnu Vellanki
    Created     : Tue July 05 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
{api/inbound/ttItem.i}
{api/inbound/ttRequest.i}

DEFINE INPUT  PARAMETER ipcRoute                  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcVerb                   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcRequestDataType        AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iplcRequestData           AS LONGCHAR   NO-UNDO.
DEFINE INPUT  PARAMETER iplcResponseDataStructure AS LONGCHAR   NO-UNDO.
DEFINE INPUT  PARAMETER ipcRequestedBy            AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcRecordSource           AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcNotes                  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcUserName               AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER oplcResponseData          AS LONGCHAR   NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess                AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER opcAPIInboundEvent        AS CHARACTER  NO-UNDO.

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
        OUTPUT opcAPIInboundEvent
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
    OUTPUT TABLE ttItem
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
       OUTPUT opcAPIInboundEvent
       ).
   
   RETURN.  
END.

/* Prepares response using response data from API Inbound configuration*/
FOR EACH ttItem NO-LOCK:
    ASSIGN
        lcResponseData           = iplcResponseDataStructure
        ttItem.InventoryStockID  = REPLACE(ttItem.InventoryStockID,'"','\"')
        ttItem.PrimaryID         = REPLACE(ttItem.PrimaryID,'"','\"')
        ttItem.StockIDAlias      = REPLACE(ttItem.StockIDAlias,'"','\"')
        lcResponseData           = REPLACE(lcResponseData, "$WarehouseID$", ttItem.WarehouseID)
        lcResponseData           = REPLACE(lcResponseData, "$LocationID$", ttItem.LocationID)
        lcResponseData           = REPLACE(lcResponseData, "$PrimaryID$", ttItem.PrimaryID)
        lcResponseData           = REPLACE(lcResponseData, "$InventoryStockID$", ttItem.InventoryStockID)
        lcResponseData           = REPLACE(lcResponseData, "$Quantity$", STRING(ttItem.Quantity))
        lcResponseData           = REPLACE(lcResponseData, "$ItemType$", ttItem.ItemType)
        lcResponseData           = REPLACE(lcResponseData, "$StockIDAlias$", ttItem.StockIDAlias)
        lcResponseData           = REPLACE(lcResponseData, "$QuanityUOM$", ttItem.QuantityUOM)
        lcResponseData           = REPLACE(lcResponseData, "$QuantityPerSubUnit$", STRING(ttItem.QuantityPerSubUnit))
        lcResponseData           = REPLACE(lcResponseData, "$QuantitySubUnitsPerUnit$", STRING(ttItem.QuantitySubUnitsPerUnit))
        lcResponseData           = REPLACE(lcResponseData, "$QuantityPartial$", STRING(ttItem.QuantityPartial))
        lcConcatResponseData     = lcConcatResponseData + "," + lcResponseData
        .
END.

ASSIGN
    lcConcatResponseData  = IF lcConcatResponseData NE "" THEN
                                lcConcatResponseData  
                            ELSE
                                '~"No data"'
    opcMessage            = "Success"
    lcConcatResponseData  = TRIM(lcConcatResponseData,",") 
    oplcResponseData      = '~{"response_code":200,"response_message":"' + opcMessage + '","response_data":[' + lcConcatResponseData + ']}'
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
    OUTPUT opcAPIInboundEvent
    ).
  
DELETE PROCEDURE hdJSONProcs.

                                                      
