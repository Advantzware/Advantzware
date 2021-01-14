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
DEFINE INPUT  PARAMETER ipcUsername               AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER oplcResponseData          AS LONGCHAR   NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess                AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER opcAPIInboundEvent        AS CHARACTER  NO-UNDO.

DEFINE VARIABLE hdJSONProcs             AS HANDLE     NO-UNDO.

/* Inventory transfer variables */
DEFINE VARIABLE cCompany                AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cWareHouseID            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLocationID             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cInventoryStockIDTag    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrimaryID              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cItemType               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE riRctd                  AS ROWID      NO-UNDO.

{api/inbound/ttRequest.i}

RUN api/JSONProcs.p PERSISTENT SET hdJSONProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdJSONProcs). 

/* Get request data fields in a temp-table */
RUN ReadRequestData (
    INPUT  iplcRequestData,
    OUTPUT oplSuccess,
    OUTPUT opcMessage,
    OUTPUT TABLE ttRequest
    ) NO-ERROR.
        
IF NOT oplSuccess THEN DO:   
    oplcResponseData  = '~{"response_code": 400,"response_message":"' + opcMessage + '"}'.
    RETURN.
END.        

RUN pProcessInputs (
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ) NO-ERROR.

IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
    oplcResponseData  = '~{"response_code": 400,"response_message":"' + opcMessage + '"}'.      
ELSE
    ASSIGN
        oplcResponseData = '~{"response_code":200,"response_message":"Inventory Transfer Is Success"}'
        opcMessage = "Success"
        .

THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdJSONProcs).
DELETE PROCEDURE hdJSONProcs.

PROCEDURE pProcessInputs:
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL    NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE lRecFound            AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE iTransferCounter     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iIndex               AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iTransfersFieldOrder AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iTopLevelParent      AS INTEGER    NO-UNDO  INITIAL 0.

    /* Fetch Requestor */          
    RUN JSON_GetFieldValueByName (
        INPUT  "Requester",
        OUTPUT lRecFound,
        OUTPUT ipcRequestedBy
        ) NO-ERROR.  
        
    /* Get the count of transfer records */
    RUN JSON_GetRecordCountByNameAndParent (
        INPUT  "Transfers", 
        INPUT  iTopLevelParent, 
        OUTPUT iTransferCounter
        ) NO-ERROR.

    /* Browse through all the transfer records */
    DO TRANSACTION ON ERROR UNDO, LEAVE:                  
        DO iIndex = 0 TO iTransferCounter - 1:
            ASSIGN
                iTransfersFieldOrder = 0
                cCompany             = ""
                cWareHouseID         = ""
                cLocationID          = ""
                cPrimaryID           = ""
                cItemType            = ""
                cInventoryStockIDTag = ""
                ipcNotes             = ""
                .
            
            /* Fetch the Transfers field order, which will be further used as 
               parent to fetch it's child records */    
            RUN JSON_GetFieldOrderByNameValueAndParent (
                INPUT  "Transfers", 
                INPUT  STRING(iIndex), 
                INPUT  iTopLevelParent, 
                OUTPUT lRecFound, 
                OUTPUT iTransfersFieldOrder
                ) NO-ERROR.
    
            /* Fetch company code */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "Company", 
                INPUT  iTransfersFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cCompany
                ) NO-ERROR.
    
            /* Fetch warehouse ID */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "WareHouseID", 
                INPUT  iTransfersFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cWareHouseID
                ) NO-ERROR.
    
            /* Fetch location id */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "LocationID", 
                INPUT  iTransfersFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cLocationID
                ) NO-ERROR.
    
            /* Fetch inventory stock ID tag */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "InventoryStockIDTag", 
                INPUT  iTransfersFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cInventoryStockIDTag
                ) NO-ERROR.
    
            /* Fetch Primary ID */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "PrimaryID", 
                INPUT  iTransfersFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cPrimaryID
                ) NO-ERROR.
                                
            /* Fetch item type */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "ItemType", 
                INPUT  iTransfersFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cItemType
                ) NO-ERROR.
        
            /* Fetch Requestor */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "RequesterNotes", 
                INPUT  iTransfersFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT ipcNotes
                ) NO-ERROR.
                 
            /* This is to fetch response data*/ 
            RUN api\inbound\CreateInventoryTransfer.p (
                INPUT  cCompany, 
                INPUT  cWareHouseID,
                INPUT  cLocationID, 
                INPUT  cInventoryStockIDTag,
                INPUT  cPrimaryID,
                INPUT  cItemType,
                INPUT  ipcUserName, 
                INPUT  TRUE, /* Post */
                OUTPUT riRctd,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ) NO-ERROR.
            
            IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
                UNDO, LEAVE.
        END.
    END.
END PROCEDURE.            
 
