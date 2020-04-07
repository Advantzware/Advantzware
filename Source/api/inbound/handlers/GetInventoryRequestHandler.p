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
 
DEFINE VARIABLE hdJSONProcs             AS HANDLE    NO-UNDO.
DEFINE VARIABLE lcConcatResponseData    AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE cCompany                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWareHouseID            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocationID             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInventoryStockID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrimaryID              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemType               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobNo                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iJobNo2                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCustNo                 AS CHARACTER NO-UNDO.

/* The below code is added as APIInboundEvent.rec_key will be populated in the APIInboundEvent's
   create trigger, only if session.p is running persistently, else will be populated with empty value.
   ( refer methods/triggers/create.i ) */
DEFINE VARIABLE hdSession AS HANDLE NO-UNDO.
RUN system/session.p PERSISTENT SET hdSession.
SESSION:ADD-SUPER-PROCEDURE (hdSession).

RUN api/JSONProcs.p PERSISTENT SET hdJSONProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdJSONProcs). 

/* Get request data fields in a temp-table */
RUN ReadRequestData IN hdJSONProcs (
    INPUT  iplcRequestData,
    OUTPUT oplSuccess,
    OUTPUT opcMessage,
    OUTPUT TABLE ttRequest
    ).

IF NOT oplSuccess THEN DO:
    oplcResponseData  = '~{"response_code": 400,"response_message":"' + opcMessage + '"}'.
    RETURN.
END.

RUN pProcessInputs (
    OUTPUT oplSuccess,
    OUTPUT opcMessage,
    OUTPUT lcConcatResponseData
    ) NO-ERROR.

RUN JSON_EscapeExceptionalCharacters (
    INPUT-OUTPUT opcMessage
    ) NO-ERROR.
    
IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
    oplcResponseData  = '~{"response_code": 400,"response_message":"' + opcMessage + '"}'.      
ELSE
    ASSIGN
        oplcResponseData = '~{"response_code":200,"response_message":"' + opcMessage + '","response_data":[' + lcConcatResponseData + ']}'
        opcMessage = "Success"
        .
        
THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdJSONProcs).
DELETE PROCEDURE hdJSONProcs.

PROCEDURE pProcessInputs:
    DEFINE OUTPUT PARAMETER oplSuccess             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage             AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcConcatResponseData AS LONGCHAR  NO-UNDO.
     
    DEFINE VARIABLE lRecFound      AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE lcResponseData AS LONGCHAR NO-UNDO.
    
    /* Fetch Requestor */          
    RUN JSON_GetFieldValueByName (
        INPUT  "Requester",
        OUTPUT lRecFound,
        OUTPUT ipcRequestedBy
        ) NO-ERROR.  
        
    /* Get the Company */
    RUN JSON_GetFieldValueByName (
        INPUT  "Company", 
        OUTPUT lRecFound, 
        OUTPUT cCompany
        ) NO-ERROR.

    /* Get the WarehouseID */
    RUN JSON_GetFieldValueByName (
        INPUT  "WarehouseID", 
        OUTPUT lRecFound, 
        OUTPUT cWarehouseID
        ) NO-ERROR.
        
    /* Get the LocationID */
    RUN JSON_GetFieldValueByName (
        INPUT  "LocationID ", 
        OUTPUT lRecFound, 
        OUTPUT cLocationID
        ) NO-ERROR.
        
    /* Get the InventoryStockID */
    RUN JSON_GetFieldValueByName (
        INPUT  "InventoryStockID", 
        OUTPUT lRecFound, 
        OUTPUT cInventoryStockID
        ) NO-ERROR.

    /* Get the PrimaryID */
    RUN JSON_GetFieldValueByName (
        INPUT  "PrimaryID", 
        OUTPUT lRecFound, 
        OUTPUT cPrimaryID
        ) NO-ERROR.
        
    /* Get the ItemType */
    RUN JSON_GetFieldValueByName (
        INPUT  "ItemType", 
        OUTPUT lRecFound, 
        OUTPUT cItemType
        ) NO-ERROR.
        
    /* Get the Notes */
    RUN JSON_GetFieldValueByName (
        INPUT  "RequesterNotes", 
        OUTPUT lRecFound, 
        OUTPUT ipcNotes
        ) NO-ERROR.

    /* This is to fetch response data*/
    RUN api\inbound\GetInventoryDetails.p (
        INPUT  cCompany, 
        INPUT  cWareHouseID,
        INPUT  cLocationID, 
        INPUT  cInventoryStockID,
        INPUT  cPrimaryID,
        INPUT  cJobNo,
        INPUT  iJobNo2,
        INPUT  cCustNo,
        INPUT  cItemType,
        OUTPUT oplSuccess,
        OUTPUT opcMessage,
        OUTPUT TABLE ttItem
        ).
 
    IF NOT oplSuccess THEN
        RETURN.
    
    /* Prepares response using response data from API Inbound configuration*/
    FOR EACH ttItem NO-LOCK:
            lcResponseData = iplcResponseDataStructure.
            
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"WarehouseID",ttItem.WarehouseID) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"LocationID", ttItem.LocationID) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"PrimaryID", ttItem.PrimaryID) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"InventoryStockID", ttItem.InventoryStockID) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"Quantity",STRING(ttItem.Quantity,"->>>>>>>>9.9<<<<<")) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"ItemType", ttItem.ItemType) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"StockIDAlias", ttItem.tag) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"QuantityUOM", ttItem.QuantityUOM) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"QuantityPerSubUnit", STRING(ttItem.QuantityPerSubUnit)) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"QuantitySubUnitsPerUnit", STRING(ttItem.QuantitySubUnitsPerUnit)) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"QuantityPartial", STRING(ttItem.QuantityPartial)) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"Units", STRING(ttItem.Units)) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"JobNo", ttItem.JobNo) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"JobNo2", STRING(ttItem.JobNo2)) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"POID", STRING(ttItem.POID)) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"UnitLength",STRING(ttItem.UnitLength,"->>>>>>>>9.9<<<<<")) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"UnitHeight",STRING(ttItem.UnitHeight,"->>>>>>>>9.9<<<<<")) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"UnitWidth",STRING(ttItem.UnitWidth,"->>>>>>>>9.9<<<<<")) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"StackHeight",STRING(ttItem.StackHeight)) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"TagStatus",ttItem.TagStatus) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"StatusDescription",ttItem.StatusDescription) NO-ERROR. 
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"OnHold",STRING(ttItem.OnHold)) NO-ERROR.                       
            oplcConcatResponseData = oplcConcatResponseData + "," + lcResponseData.
    END.
    
    ASSIGN
        oplcConcatResponseData =  IF oplcConcatResponseData NE "" THEN
                                      oplcConcatResponseData  
                                  ELSE
                                      '~"No data"'
        opcMessage             = "Success"
        oplcConcatResponseData = TRIM(oplcConcatResponseData,",") 
        .
END PROCEDURE. 

SESSION:REMOVE-SUPER-PROCEDURE (hdSession).
DELETE PROCEDURE hdSession.
  
