/*------------------------------------------------------------------------
    File        : api\inbound\handlers\CreateVendorInvoiceRequestHandler.p
    Purpose     : Process request data for createvendorinvoice  

    Syntax      :

    Description : Process request data for createvendorinvoice  

    Author(s)   : Vishnu Vellanki
    Created     : Thu Jan 01 07:33:22 EDT 2020
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

DEFINE VARIABLE hdJSONProcs                               AS HANDLE    NO-UNDO.
DEFINE VARIABLE cCompany                                  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cVendorID                                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendorInvoiceNumber                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendorInvoiceDate                        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendorInvoiceDueDate                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE dVendorInvoiceDiscountPercentage          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dVendorInvoiceTotalAmount                 AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iVendorInvoiceLinePurchaseOrderNumber     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iVendorInvoiceLinePurchaseOrderLineNumber AS INTEGER   NO-UNDO.
DEFINE VARIABLE cVendorInvoiceLineAccountNumber           AS CHARACTER NO-UNDO.
DEFINE VARIABLE dVendorInvoiceLineQuantity                AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cVendorInvoiceLineQuantityUOM             AS CHARACTER NO-UNDO.
DEFINE VARIABLE dVendorInvoiceLinePrice                   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cVendorInvoiceLinePriceUOM                AS CHARACTER NO-UNDO.
DEFINE VARIABLE dVendorInvoiceLineSqFt                    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dVendorInvoiceLineAmount                  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cVendorInvoiceLineDescription             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRequesterNotes                           AS CHARACTER NO-UNDO.

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
        opcMessage = "Success"
        oplcResponseData = '~{"response_code": 200,"response_message":"' + opcMessage + '"}'.
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
    DEFINE OUTPUT PARAMETER oplSuccess      AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lRecFound               AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE iInvoiceCounter         AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iIndex                  AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iInvoicesFieldOrder     AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iTopLevelParent         AS INTEGER  NO-UNDO INITIAL 0.
    DEFINE VARIABLE iInvoiceLineCounter     AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iInvoiceLinesFieldOrder AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iIndex1                 AS INTEGER  NO-UNDO.
 
    /* Fetch Requestor */          
    RUN JSON_GetFieldValueByName (
        INPUT  "Requester",
        OUTPUT lRecFound,
        OUTPUT ipcRequestedBy
        ) NO-ERROR. 
        
    /* Fetch Requestor Notes*/          
    RUN JSON_GetFieldValueByName (
        INPUT  "RequesterNotes",
        OUTPUT lRecFound,
        OUTPUT cRequesterNotes
        ) NO-ERROR. 

    /* Get the count of Invoice records */
    RUN JSON_GetRecordCountByNameAndParent (
        INPUT  "Invoices", 
        INPUT  iTopLevelParent, 
        OUTPUT iInvoiceCounter
        ) NO-ERROR.

    /* Browse through all the Invoice records */
    DO TRANSACTION ON ERROR UNDO, LEAVE:                  
        DO iIndex = 0 TO iInvoiceCounter - 1:
            /* Fetch the Invoices field order, which will be further used as 
               parent to fetch it's child records */    
            RUN JSON_GetFieldOrderByNameValueAndParent (
                INPUT  "Invoices", 
                INPUT  STRING(iIndex), 
                INPUT  iTopLevelParent, 
                OUTPUT lRecFound, 
                OUTPUT iInvoicesFieldOrder
                ) NO-ERROR.

            /* Fetch company code */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "Company", 
                INPUT  iInvoicesFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cCompany
                ) NO-ERROR. 
                
            /* Fetch vendor ID */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "VendorID", 
                INPUT  iInvoicesFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cVendorID
                ) NO-ERROR. 
        
            /* Fetch vendor invoice number */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "VendorInvoiceNumber", 
                INPUT  iInvoicesFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cVendorInvoiceNumber
                ) NO-ERROR. 
        
            /* Fetch vendor invoice date */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "VendorInvoiceDate", 
                INPUT  iInvoicesFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cVendorInvoiceDate
                ) NO-ERROR.
        
            /* Fetch vendor invoice due date */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "VendorInvoiceDueDate", 
                INPUT  iInvoicesFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT cVendorInvoiceDueDate
                ) NO-ERROR.
        
            /* Fetch vendor invoice discount percentage */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "VendorInvoiceDiscountPercentage", 
                INPUT  iInvoicesFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT dVendorInvoiceDiscountPercentage
                ) NO-ERROR.
        
            /* Fetch vendor invoice total amount */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "VendorInvoiceTotalAmount", 
                INPUT  iInvoicesFieldOrder, 
                OUTPUT lRecFound, 
                OUTPUT dVendorInvoiceTotalAmount
                ) NO-ERROR.
            
            /* Get the count of invoice lines records */
            RUN JSON_GetRecordCountByNameAndParent (
                INPUT  "InvoiceLines", 
                INPUT  iInvoicesFieldOrder, 
                OUTPUT iInvoiceLineCounter
                ) NO-ERROR.
                
            /* Browse through all the InvoicLines records */
            DO iIndex1 = 0 TO iInvoiceLineCounter - 1:
                /* Fetch the Invoices field order, which will be further used as 
               parent to fetch it's child records */    
                RUN JSON_GetFieldOrderByNameValueAndParent (
                    INPUT  "InvoiceLines", 
                    INPUT  STRING(iIndex1), 
                    INPUT  iInvoicesFieldOrder, 
                    OUTPUT lRecFound, 
                    OUTPUT iInvoiceLinesFieldOrder
                    ) NO-ERROR.
                    
                /* Fetch vendor invoice line PO number */
                RUN JSON_GetFieldValueByNameAndParent (
                    INPUT  "VendorInvoiceLinePurchaseOrderNumber", 
                    INPUT  iInvoiceLinesFieldOrder, 
                    OUTPUT lRecFound, 
                    OUTPUT iVendorInvoiceLinePurchaseOrderNumber
                    ) NO-ERROR.

                /* Fetch vendor invoice line PO line number */
                RUN JSON_GetFieldValueByNameAndParent (
                    INPUT  "VendorInvoiceLinePurchaseOrderLineNumber", 
                    INPUT  iInvoiceLinesFieldOrder, 
                    OUTPUT lRecFound, 
                    OUTPUT iVendorInvoiceLinePurchaseOrderLineNumber
                    ) NO-ERROR.

                /* Fetch vendor invoice line account number */
                RUN JSON_GetFieldValueByNameAndParent (
                    INPUT  "VendorInvoiceLineAccountNumber", 
                    INPUT  iInvoiceLinesFieldOrder, 
                    OUTPUT lRecFound, 
                    OUTPUT cVendorInvoiceLineAccountNumber
                    ) NO-ERROR.
            
                /* Fetch vendore invoice line quantity */
                RUN JSON_GetFieldValueByNameAndParent (
                    INPUT  "VendorInvoiceLineQuantity", 
                    INPUT  iInvoiceLinesFieldOrder, 
                    OUTPUT lRecFound, 
                    OUTPUT dVendorInvoiceLineQuantity
                    ) NO-ERROR.
            
                /* Fetch vendore invoice line quantity UOM */
                RUN JSON_GetFieldValueByNameAndParent (
                    INPUT  "VendorInvoiceLineQuantityUOM", 
                    INPUT  iInvoiceLinesFieldOrder, 
                    OUTPUT lRecFound, 
                    OUTPUT cVendorInvoiceLineQuantityUOM
                    ) NO-ERROR.           
    
                /* Fetch vendor invoice line quantity price */
                RUN JSON_GetFieldValueByNameAndParent (
                    INPUT  "VendorInvoiceLinePrice", 
                    INPUT  iInvoiceLinesFieldOrder, 
                    OUTPUT lRecFound, 
                    OUTPUT dVendorInvoiceLinePrice
                    ) NO-ERROR.
    
                /* Fetch vendor invoice line quantity UOM */
                RUN JSON_GetFieldValueByNameAndParent (
                    INPUT  "VendorInvoiceLinePriceUOM", 
                    INPUT  iInvoiceLinesFieldOrder, 
                    OUTPUT lRecFound, 
                    OUTPUT cVendorInvoiceLinePriceUOM
                    ) NO-ERROR.
    
                /* Fetch vendor invoice line sqft */
                RUN JSON_GetFieldValueByNameAndParent (
                    INPUT  "VendorInvoiceLineSqFt", 
                    INPUT  iInvoiceLinesFieldOrder, 
                    OUTPUT lRecFound, 
                    OUTPUT dVendorInvoiceLineSqFt
                    ) NO-ERROR.  
                                     
                /* Fetch vendor invoice line amount */
                RUN JSON_GetFieldValueByNameAndParent (
                    INPUT  "VendorInvoiceLineAmount", 
                    INPUT  iInvoiceLinesFieldOrder, 
                    OUTPUT lRecFound, 
                    OUTPUT dVendorInvoiceLineAmount
                    ) NO-ERROR.  
    
                /* Fetch vendor invoice line description */
                RUN JSON_GetFieldValueByNameAndParent (
                    INPUT  "VendorInvoiceLineDescription", 
                    INPUT  iInvoiceLinesFieldOrder, 
                    OUTPUT lRecFound, 
                    OUTPUT cVendorInvoiceLineDescription
                    ) NO-ERROR.  
            
                /* This is to fetch response data*/ 
                RUN api\inbound\CreateVendorInvoice.p (
                    INPUT  cCompany, 
                    INPUT  cVendorID,
                    INPUT  cVendorInvoiceNumber,
                    INPUT  cVendorInvoiceDate,
                    INPUT  cVendorInvoiceDueDate,
                    INPUT  dVendorInvoiceDiscountPercentage,
                    INPUT  dVendorInvoiceTotalAmount,                  
                    INPUT  iVendorInvoiceLinePurchaseOrderNumber,                 
                    INPUT  iVendorInvoiceLinePurchaseOrderLineNumber,     
                    INPUT  cVendorInvoiceLineAccountNumber,
                    INPUT  dVendorInvoiceLineQuantity,            
                    INPUT  cVendorInvoiceLineQuantityUOM, 
                    INPUT  dVendorInvoiceLinePrice,            
                    INPUT  cVendorInvoiceLinePriceUOM,
                    INPUT  dVendorInvoiceLineSqFt,
                    INPUT  dVendorInvoiceLineAmount,
                    INPUT  cVendorInvoiceLineDescription,
                    OUTPUT oplSuccess,
                    OUTPUT opcMessage
                    )NO-ERROR.


            IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
                UNDO, LEAVE.
            END.
        END.
    END.
END PROCEDURE.            

