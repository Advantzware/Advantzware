/*------------------------------------------------------------------------
    File        : api\inbound\handlers\CreateInventoryReceiptRequestHandler.p
    Purpose     : Creates Inventory Receipt 

    Syntax      :

    Description : Creates Inventory Receipt  

    Author(s)   : Vishnu Vellanki
    Created     : Tue Oct 09 07:33:22 EDT 2019
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

DEFINE VARIABLE hdJSONProcs              AS HANDLE     NO-UNDO.
DEFINE VARIABLE cCompany                 AS CHARACTER  NO-UNDO. 
DEFINE VARIABLE cInventoryStockID        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dQuantity                AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cQuantityUOM             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iPONo                    AS INTEGER    NO-UNDO.
DEFINE VARIABLE iPOLine                  AS INTEGER    NO-UNDO.
DEFINE VARIABLE cJobID                   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cJobID2                  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iQuantityPerSubUnit      AS INTEGER    NO-UNDO.
DEFINE VARIABLE iQuantitySubUnitsPerUnit AS INTEGER    NO-UNDO.
DEFINE VARIABLE cWarehouseID             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLocationID              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSSPostFG                AS CHARACTER  NO-UNDO.
{api/inbound/ttRequest.i}

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

RUN pProcessInputs (
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ) NO-ERROR.

RUN JSON_EscapeExceptionalCharacters (
    INPUT-OUTPUT opcMessage
    ) NO-ERROR.
    
IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
    oplcResponseData  = '~{"response_code": 400,"response_message":"' + opcMessage + '"}'.      
ELSE
    ASSIGN
        oplcResponseData = '~{"response_code":200,"response_message":"Creation of Inventory Receipt is success"}'
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
    OUTPUT opcAPIInboundEvent
    ).
THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdJSONProcs).
DELETE PROCEDURE hdJSONProcs.

PROCEDURE pProcessInputs:
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL    NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE lRecFound            AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE iReceiptCounter     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iIndex               AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iReceiptsFieldOrder AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iTopLevelParent      AS INTEGER    NO-UNDO  INITIAL 0.

    /* Fetch Requestor */          
    RUN JSON_GetFieldValueByName (
        INPUT  "Requester",
        OUTPUT lRecFound,
        OUTPUT ipcRequestedBy
        ) NO-ERROR.  
        
    /* Get the count of Receipt records */
    RUN JSON_GetRecordCountByNameAndParent (
        INPUT  "Receipts", 
        INPUT  iTopLevelParent, 
        OUTPUT iReceiptCounter
        ) NO-ERROR.

    /* Browse through all the Receipt records */
    DO TRANSACTION ON ERROR UNDO, LEAVE:                  
        DO iIndex = 0 TO iReceiptCounter - 1:
            ASSIGN
                iReceiptsFieldOrder = 0
                cCompany             = ""
                cWareHouseID         = ""
                cLocationID          = ""
                cInventoryStockID    = ""
                ipcNotes             = ""
                .
            
            /* Fetch the Rceipts field order, which will be further used as 
               parent to fetch it's child records */    
            RUN JSON_GetFieldOrderByNameValueAndParent (
                INPUT  "Receipts", 
                INPUT  STRING(iIndex), 
                INPUT  iTopLevelParent, 
                OUTPUT lRecFound, 
                OUTPUT iReceiptsFieldOrder
                ) NO-ERROR.
    
            /* Fetch company code */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "Company", 
                INPUT  iReceiptsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cCompany
                ) NO-ERROR.
    
            /* Fetch warehouse ID */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "WareHouseID", 
                INPUT  iReceiptsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cWareHouseID
                ) NO-ERROR.
    
            /* Fetch location id */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "LocationID", 
                INPUT  iReceiptsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cLocationID
                ) NO-ERROR.
    
            /* Fetch inventory stock ID */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "InventoryStockID", 
                INPUT  iReceiptsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cInventoryStockID
                ) NO-ERROR.
                
            /* Fetch inventory quantity */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "Quantity", 
                INPUT  iReceiptsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT dQuantity
                ) NO-ERROR.
                
            /* Fetch inventory quantity UOM */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "QuantityUOM", 
                INPUT  iReceiptsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cQuantityUOM
                ) NO-ERROR.           

            /* Fetch inventory PO number */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "PONo", 
                INPUT  iReceiptsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT iPONo
                ) NO-ERROR.

            /* Fetch inventory PO Line */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "POLine", 
                INPUT  iReceiptsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT iPOLine
                ) NO-ERROR.

            /* Fetch inventory Job Number2 */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "JobID2", 
                INPUT  iReceiptsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cJobID2
                ) NO-ERROR.  
                                 
            /* Fetch inventory Job Number */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "JobID", 
                INPUT  iReceiptsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cJobID
                ) NO-ERROR.  

            /* Fetch inventory QuantityPerSubUnit */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "QuantityPerSubUnit", 
                INPUT  iReceiptsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT iQuantityPerSubUnit
                ) NO-ERROR.  
                
            /* Fetch inventory QuantitySubUnitsPerUnit */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "QuantitySubUnitsPerUnit", 
                INPUT  iReceiptsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT iQuantitySubUnitsPerUnit
                ) NO-ERROR.  
            
            /* Fetch inventory SSPostFG */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "SSPostFG", 
                INPUT  iReceiptsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cSSPostFG
                ) NO-ERROR. 
                                                 
            /* Fetch Requestor */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "RequesterNotes", 
                INPUT  iReceiptsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT ipcNotes
                ) NO-ERROR.
                 
            /* This is to fetch response data*/ 
            RUN api\inbound\CreateInventoryReceipt.p (
                INPUT  cCompany, 
                INPUT  cInventoryStockID,
                INPUT  dQuantity,
                INPUT  cQuantityUOM,
                INPUT  iPONo,
                INPUT  iPOLine,
                INPUT  cJobID,                  
                INPUT  cJobID2,                 
                INPUT  iQuantityPerSubUnit,     
                INPUT  iQuantitySubUnitsPerUnit,
                INPUT  cWarehouseID,            
                INPUT  cLocationID, 
                INPUT  cSSPostFG,            
                INPUT  ipcUsername,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                )NO-ERROR.
            IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
                UNDO, LEAVE.
        END.
    END.
END PROCEDURE.            

