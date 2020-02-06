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

DEFINE VARIABLE hdJSONProcs         AS HANDLE     NO-UNDO.
DEFINE VARIABLE cCompany            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iPONo               AS INTEGER    NO-UNDO.
DEFINE VARIABLE iPOLine             AS INTEGER    NO-UNDO.
DEFINE VARIABLE cPrimaryID          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cItemType           AS CHARACTER  NO-UNDO. 
DEFINE VARIABLE dQuantity           AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iQuantityPerSubUnit AS INTEGER    NO-UNDO.
DEFINE VARIABLE cTag                AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cInventoryStockID   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCreateReceipt      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cWarehouseID        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLocationID         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcTags              AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE cLoadtagFormat      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iTagCopies          AS INTEGER    NO-UNDO.

{api/inbound/ttRequest.i}
/* The below code is added as APIInboundEvent.rec_key will be populated in the APIInboundEvent's
   create trigger, only if session.p is running persistently, else will be populated with empty value.
   ( refer methods/triggers/create.i ) */

DEFINE VARIABLE hdSession AS HANDLE NO-UNDO.
RUN system/session.p PERSISTENT SET hdSession.
SESSION:ADD-SUPER-PROCEDURE (hdSession).

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
    OUTPUT opcMessage,
    OUTPUT lcTags
    ) NO-ERROR.

RUN JSON_EscapeExceptionalCharacters (
    INPUT-OUTPUT opcMessage
    ) NO-ERROR. 
        
IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN 
    oplcResponseData  = '~{"response_code": 400,"response_message":"' + opcMessage + '"}'. 
ELSE
    ASSIGN
        opcMessage = "Success"
        oplcResponseData  = '~{"response_code": 200,"response_message":"' + opcMessage + '","response_data":[~{"Tags":[' + lcTags + ']~}]~}'.
        .

THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdJSONProcs).
DELETE PROCEDURE hdJSONProcs.

PROCEDURE pProcessInputs:
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcTags   AS LONGCHAR  NO-UNDO.
    
    DEFINE VARIABLE lRecFound       AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE iTagCounter     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iIndex          AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iTagsFieldOrder AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iTopLevelParent AS INTEGER    NO-UNDO  INITIAL 0.

    /* Fetch Requestor */          
    RUN JSON_GetFieldValueByName (
        INPUT  "Requester",
        OUTPUT lRecFound,
        OUTPUT ipcRequestedBy
        ) NO-ERROR.  
        
    /* Get the count of Tag records */
    RUN JSON_GetRecordCountByNameAndParent (
        INPUT  "Tags", 
        INPUT  iTopLevelParent, 
        OUTPUT iTagCounter
        ) NO-ERROR.

    /* Browse through all the Tag records */
    DO TRANSACTION ON ERROR UNDO, LEAVE:                  
        DO iIndex = 0 TO iTagCounter - 1:
            ASSIGN
                iTagsFieldOrder = 0
                cCompany             = ""
                cWareHouseID         = ""
                cLocationID          = ""
                cInventoryStockID    = ""
                ipcNotes             = ""
                .
            
            /* Fetch the Rceipts field order, which will be further used as 
               parent to fetch it's child records */    
            RUN JSON_GetFieldOrderByNameValueAndParent (
                INPUT  "Tags", 
                INPUT  STRING(iIndex), 
                INPUT  iTopLevelParent, 
                OUTPUT lRecFound, 
                OUTPUT iTagsFieldOrder
                ) NO-ERROR.
    
            /* Fetch company code */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "Company", 
                INPUT  iTagsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cCompany
                ) NO-ERROR.
    
            /* Fetch warehouse ID */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "WareHouseID", 
                INPUT  iTagsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cWareHouseID
                ) NO-ERROR.
    
            /* Fetch location id */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "LocationID", 
                INPUT  iTagsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cLocationID
                ) NO-ERROR.
                
            /* Fetch inventory quantity */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "Quantity", 
                INPUT  iTagsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT dQuantity
                ) NO-ERROR.
                
            /* Fetch inventory PO number */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "PONo", 
                INPUT  iTagsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT iPONo
                ) NO-ERROR.

            /* Fetch inventory PO Line */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "POLine", 
                INPUT  iTagsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT iPOLine
                ) NO-ERROR.

            /* Fetch inventory QuantityPerSubUnit */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "QuantityPerSubUnit", 
                INPUT  iTagsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT iQuantityPerSubUnit
                ) NO-ERROR.  
            
            /* Fetch inventory ItemType */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "ItemType", 
                INPUT  iTagsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cItemType
                ) NO-ERROR.  
            
            /* Fetch inventory ItemType */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "PrimaryID", 
                INPUT  iTagsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cPrimaryID
                ) NO-ERROR.  
             
            /* Fetch Requestor */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "RequesterNotes", 
                INPUT  iTagsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT ipcNotes
                ) NO-ERROR.
            
            /* Fetch Receipt creation */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "CreateReceipt", 
                INPUT  iTagsFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cCreateReceipt
                ) NO-ERROR.   
                   
            /* This is to fetch response data*/
            RUN api\inbound\CreateTagForPO.p (
                INPUT  cCompany,
                INPUT  iPONo,
                INPUT  iPOLine,
                INPUT  cPrimaryID,
                INPUT  cItemType,
                INPUT  dQuantity,
                INPUT  iQuantityPerSubUnit,
                INPUT  cTag,
                INPUT  cWarehouseID,
                INPUT  cLocationID,
                INPUT  ipcUsername,
                INPUT  cCreateReceipt,
                OUTPUT cInventoryStockID,
                OUTPUT cLoadtagFormat,
                OUTPUT iTagCopies,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
          
           oplcResponseData = iplcResponseDataStructure.
           RUN JSON_UpdateFieldValue (INPUT-OUTPUT oplcResponseData, "PONo", STRING(iPONo)) NO-ERROR.
           RUN JSON_UpdateFieldValue (INPUT-OUTPUT oplcResponseData, "POLine", STRING(iPOLine)) NO-ERROR.
           RUN JSON_UpdateFieldValue (INPUT-OUTPUT oplcResponseData, "Item", cPrimaryID) NO-ERROR.
           RUN JSON_UpdateFieldValue (INPUT-OUTPUT oplcResponseData, "InventoryStockID", cInventoryStockID) NO-ERROR.
           RUN JSON_UpdateFieldValue (INPUT-OUTPUT oplcResponseData, "ReceiptCreation", cCreateReceipt) NO-ERROR.
           RUN JSON_UpdateFieldValue (INPUT-OUTPUT oplcResponseData, "LoadtagFormat", cLoadtagFormat) NO-ERROR.
           RUN JSON_UpdateFieldValue (INPUT-OUTPUT oplcResponseData, "TagCopies", STRING(iTagCopies)) NO-ERROR.
           oplcTags = IF oplcTags EQ "" THEN
                         oplcResponseData 
                      ELSE 
                         oplcTags + "," + oplcResponseData
                      .
     
            IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
                UNDO, LEAVE.
        END.
    END.
END PROCEDURE.            

SESSION:REMOVE-SUPER-PROCEDURE (hdSession).
DELETE PROCEDURE hdSession.
