/*------------------------------------------------------------------------
    File        : api\inbound\handlers\GetInventoryForBOLRequestHandler.p
    Purpose     : Gets Inventory For a BOL 

    Syntax      :

    Description : Gets Inventory For a BOL  

    Author(s)   : Vishnu Vellanki
    Created     : Fri Nov 29 07:33:22 EDT 2019
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
DEFINE INPUT  PARAMETER ipcUsername               AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER oplcResponseData          AS LONGCHAR   NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess                AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER opcAPIInboundEvent        AS CHARACTER  NO-UNDO.

DEFINE VARIABLE hdJSONProcs          AS HANDLE    NO-UNDO.
DEFINE VARIABLE cCompany             AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iBOLID               AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcConcatResponseData AS LONGCHAR  NO-UNDO.

{api/inbound/ttRequest.i}
{api/inbound/ttBOLLine.i}

oplSuccess = YES.

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

    /* Get the BOLID */
    RUN JSON_GetFieldValueByName (
        INPUT  "BOLID", 
        OUTPUT lRecFound, 
        OUTPUT iBOLID
        ) NO-ERROR.
        
    /* Get the Notes */
    RUN JSON_GetFieldValueByName (
        INPUT  "RequesterNotes", 
        OUTPUT lRecFound, 
        OUTPUT ipcNotes
        ) NO-ERROR.

    /* This is to fetch response data*/ 
    RUN api\inbound\GetInventoryForBOL.p (
        INPUT  cCompany, 
        INPUT  iBOLID,
        OUTPUT TABLE ttBOLLine,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
 
    IF NOT oplSuccess THEN
    DO:
        oplcResponseData  = '~{"response_code": 400,"response_message":"' + opcMessage + '"}'.   
        RETURN.  
    END.
    
    /* Prepares response using response data from API Inbound configuration*/
    FOR EACH ttBOLLine NO-LOCK:
        lcResponseData = iplcResponseDataStructure.
        
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"WarehouseID",ttBOLLine.WarehouseID) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"LocationID", ttBOLLine.LocationID) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"PrimaryID", ttBOLLine.PrimaryID) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"InventoryStockID", ttBOLLine.InventoryStockID) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"Quantity",STRING(ttBOLLine.Quantity,"->>>>>>>>9.9<<<<<")) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"ItemType", ttBOLLine.ItemType) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"StockIDAlias", ttBOLLine.StockIDAlias) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"QuantityUOM", ttBOLLine.QuantityUOM) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"QuantityPerSubUnit", STRING(ttBOLLine.QuantityPerSubUnit)) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"QuantitySubUnitsPerUnit", STRING(ttBOLLine.QuantitySubUnitsPerUnit)) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"QuantityPartial", STRING(ttBOLLine.QuantityPartial)) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"Units", STRING(ttBOLLine.Units)) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"JobNo", ttBOLLine.JobNo) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"JobNo2", STRING(ttBOLLine.JobNo2)) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"POID", STRING(ttBOLLine.POID)) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"UnitLength",STRING(ttBOLLine.UnitLength,"->>>>>>>>9.9<<<<<")) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"UnitHeight",STRING(ttBOLLine.UnitHeight,"->>>>>>>>9.9<<<<<")) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"UnitWidth",STRING(ttBOLLine.UnitWidth,"->>>>>>>>9.9<<<<<")) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"StackHeight",STRING(ttBOLLine.StackHeight)) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData,"InventoryStatus",STRING(ttBOLLine.InventoryStatus)) NO-ERROR.
        
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





          

