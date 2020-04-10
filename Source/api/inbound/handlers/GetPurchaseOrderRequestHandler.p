/*------------------------------------------------------------------------
    File        : api/inbound/handlers/GetPurchaseOrderRequestHandler.p
    Purpose     : Prepares and sends the purchase order details

    Syntax      :

    Description : Prepares and sends the purchase order details

    Author(s)   : Mithun Porandla
    Created     : Fri October 04 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
{api/inbound/ttRequest.i}
{api/ttArgs.i}

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

DEFINE VARIABLE cCompany             AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPOID                AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcConcatResponseData AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE hdJSONProcs          AS HANDLE    NO-UNDO.

RUN api/JSONProcs.p PERSISTENT SET hdJSONProcs. 

oplSuccess = TRUE.

/* Get request data fields in a temp-table */
RUN ReadRequestData IN hdJSONProcs (
    INPUT  iplcRequestData,
    OUTPUT oplSuccess,
    OUTPUT opcMessage,
    OUTPUT TABLE ttRequest
    ) NO-ERROR.

IF oplSuccess THEN DO:    
    /* This will fetch fields from request data */
    FOR EACH ttRequest:
        CASE ttRequest.FieldName:
            WHEN "Company" THEN
               cCompany = ttRequest.FieldValue.
            WHEN "poID" THEN
               iPOID = INTEGER(ttRequest.FieldValue).
        END CASE.
    END.
    
    /* Validate if Purchase Order Number is valid */
    FIND FIRST po-ord NO-LOCK
         WHERE po-ord.company EQ cCompany
           AND po-ord.po-no   EQ iPOID
         NO-ERROR.
    IF NOT AVAILABLE po-ord THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid PO Number"
            .
    END.
END.

IF oplSuccess THEN DO:
    CREATE ttArgs.
    ASSIGN
        ttArgs.argType  = "ROWID"
        ttArgs.argKey   = "po-ord"
        ttArgs.argValue = STRING(ROWID(po-ord))
        .    
                     
    FIND FIRST APIOutbound NO-LOCK
         WHERE APIOutbound.company  EQ cCompany
           AND APIOutbound.apiID    EQ "SendPurchaseOrder"
           AND APIOutbound.clientID EQ "Siggins"
           AND APIOutbound.inActive EQ FALSE
         NO-ERROR.
    IF NOT AVAILABLE APIOutbound THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Required config missing or inactive"
            .    
    END.
END.

IF oplSuccess THEN DO:
    FIND FIRST APIOutboundTrigger NO-LOCK
         WHERE APIOutboundTrigger.apiOutboundID EQ APIOutbound.apiOutboundID
           AND APIOutboundTrigger.triggerID     EQ "TriggerGetPurchaseOrder"
           AND APIOutboundTrigger.inActive      EQ FALSE
         NO-ERROR.
    IF NOT AVAILABLE APIOutboundTrigger THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Required config missing or inactive"
            .
    END.
END.

IF oplSuccess THEN DO:
    oplcResponseData = APIOutbound.requestData.
    
    RUN api/SendPurchaseOrder.p (
        INPUT TABLE ttArgs,
        INPUT APIOutbound.apiOutboundID,
        INPUT APIOutboundTrigger.apiOutboundTriggerID,
        INPUT APIOutbound.requesthandler,
        INPUT-OUTPUT oplcResponseData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).
END.

IF oplSuccess THEN
    ASSIGN
        opcMessage       = "Success"
        oplcResponseData = '~{"response_code":200,"response_message":"Success","response_data":[' + oplcResponseData + ']}'
        .
ELSE
    ASSIGN
        oplcResponseData = '~{"response_code": 400,"response_message":"' + opcMessage + '"}'
        .
