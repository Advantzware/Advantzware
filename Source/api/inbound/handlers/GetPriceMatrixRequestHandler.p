/*------------------------------------------------------------------------
    File        : api/inbound/handlers/GetPriceRequestHandler.p
    Purpose     : API request handler to parse and fetch the price and price UOM

    Syntax      :

    Description : API request handler to parse and fetch the price and price UOM

    Author(s)   : Porandla Mithun
    Created     : Mon Jul 06 07:33:22 EDT 2020
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

DEFINE VARIABLE hdJSONProcs   AS HANDLE  NO-UNDO.
DEFINE VARIABLE iResponseCode AS INTEGER NO-UNDO.
DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.

{api/inbound/ttRequest.i}
{system/ttPriceMatrix.i}

DEFINE TEMP-TABLE ttInputs NO-UNDO
    LIKE ttPriceMatrix.

RUN api/JSONProcs.p PERSISTENT SET hdJSONProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdJSONProcs).

RUN pPrepareInputs (
    OUTPUT cCompany,
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ) NO-ERROR.

IF oplSuccess THEN DO:
    RUN pProcessInputs (
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
END.

IF oplSuccess THEN DO:
    RUN pGenerateResponseData (
        INPUT  cCompany,
        INPUT  iplcResponseDataStructure,
        OUTPUT oplcResponseData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
END.

IF ERROR-STATUS:ERROR THEN
    opcMessage = ERROR-STATUS:GET-MESSAGE(1).

RUN JSON_EscapeExceptionalCharacters (
    INPUT-OUTPUT opcMessage
    ) NO-ERROR.

iResponseCode = IF oplSuccess THEN
                    200
                ELSE
                    400.

/* Generate the response data in fixed format of response_code, response_message and response_data */
RUN JSON_GetResponseData (
    INPUT  iResponseCode,      /* Value which goes into response_code tag */
    INPUT  opcMessage,         /* Value which goes into response_message tag */
    INPUT  oplcResponseData,   /* Value which goes into response_data tag */
    OUTPUT oplcResponseData
    ) NO-ERROR.

THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdJSONProcs).
DELETE PROCEDURE hdJSONProcs.

PROCEDURE pPrepareInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Procedure to fetch input data into temp-table ttPriceMatrix
 Notes:
------------------------------------------------------------------------------*/     
    DEFINE OUTPUT PARAMETER opcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cItemID      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustID      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToID    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantity    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityUOM AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTargetUOM   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lRecFound         AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lValidValue       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iCountsCounter    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iIndex            AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCountsFieldOrder AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTopLevelParent   AS INTEGER NO-UNDO  INITIAL 0.

    /* Get request data fields in a temp-table */
    RUN ReadRequestData IN hdJSONProcs (
        INPUT  iplcRequestData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage,
        OUTPUT TABLE ttRequest
        ).

    /* Fetch Requester */
    RUN JSON_GetFieldValueByName (
        INPUT  "Requester",
        OUTPUT lRecFound,
        OUTPUT ipcRequestedBy
        ) NO-ERROR.

    /* Fetch Requester Notes */
    RUN JSON_GetFieldValueByName (
        INPUT  "RequesterNotes",
        OUTPUT lRecFound,
        OUTPUT ipcNotes
        ) NO-ERROR.

    /* Fetch company code */
    RUN JSON_GetFieldValueByName (
        INPUT  "Company",
        OUTPUT lRecFound,
        OUTPUT opcCompany
        ) NO-ERROR.

    /* Get the count of Counts records */
    RUN JSON_GetRecordCountByNameAndParent (
        INPUT  "data",
        INPUT  iTopLevelParent,
        OUTPUT iCountsCounter
        ) NO-ERROR.
    IF iCountsCounter EQ 0 THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "No records found to process counts"
            .
        RETURN.
    END.

    /* Browse through all the Counts records */
    DO iIndex = 0 TO iCountsCounter - 1:
        ASSIGN
            iCountsFieldOrder   = 0
            cItemID             = ""
            cCustID             = ""
            cShipToID           = ""
            cQuantity           = ""
            cQuantityUOM        = ""
            cTargetUOM          = ""
            .

        /* Fetch the Rceipts field order, which will be further used as
           parent to fetch it's child records */
        RUN JSON_GetFieldOrderByNameValueAndParent (
            INPUT  "data",
            INPUT  STRING(iIndex),
            INPUT  iTopLevelParent,
            OUTPUT lRecFound,
            OUTPUT iCountsFieldOrder
            ) NO-ERROR.

        /* Fetch ItemID */
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "ItemID",
            INPUT  iCountsFieldOrder,
            OUTPUT lRecFound,
            OUTPUT cItemID
            ) NO-ERROR.

        /* Fetch Cusotmer ID */
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "CustomerID",
            INPUT  iCountsFieldOrder,
            OUTPUT lRecFound,
            OUTPUT cCustID
            ) NO-ERROR.

        /* Fetch shipto id */
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "ShipToID",
            INPUT  iCountsFieldOrder,
            OUTPUT lRecFound,
            OUTPUT cShipToID
            ) NO-ERROR.
        
        CREATE ttInputs.
        ASSIGN
            ttInputs.company  = opcCompany
            ttInputs.itemID   = cItemID
            ttInputs.custID   = cCustID
            ttInputs.shipToID = cShipToID
            .
    END.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE pProcessInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Procedure to process input data
 Notes:
------------------------------------------------------------------------------*/     
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.
    
    FOR EACH ttInputs:
        RUN api/inbound/GetPriceMatrix.p (
            INPUT        ttInputs.company,
            INPUT        ttInputs.custID,
            INPUT        ttInputs.shipToID,
            INPUT        ttInputs.itemID,
            INPUT-OUTPUT TABLE ttPriceMatrix,
            OUTPUT       oplSuccess,
            OUTPUT       opcMessage
            ) NO-ERROR.

        IF ERROR-STATUS:ERROR THEN
            ASSIGN
                oplSuccess = FALSE
                opcMessage = ERROR-STATUS:GET-MESSAGE(1)
                .

        IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
            LEAVE.
    END.
END PROCEDURE.

PROCEDURE pGenerateResponseData PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Procedure to generate response data
 Notes:
------------------------------------------------------------------------------*/     
    DEFINE INPUT  PARAMETER ipcCompany                AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplcResponseDataStructure AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcResponseData          AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess                AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lcPriceResponseData        AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatPriceResponseData  AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcMatrixResponseData       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatMatrixResponseData AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE iIndex                     AS INTEGER  NO-UNDO.
    
    DEFINE BUFFER bfPrice-APIInboundDetail  FOR APIInboundDetail.
    DEFINE BUFFER bfMatrix-APIInboundDetail FOR APIInboundDetail.
    DEFINE BUFFER bf-oe-prmtx               FOR oe-prmtx.
    
    FIND FIRST bfPrice-APIInboundDetail NO-LOCK
         WHERE bfPrice-APIInboundDetail.company    EQ ipcCompany
           AND bfPrice-APIInboundDetail.apiRoute   EQ ipcRoute
           AND bfPrice-APIInboundDetail.detailType EQ "Response"
           AND bfPrice-APIInboundDetail.detailID   EQ "price"
           AND bfPrice-APIInboundDetail.parentID   EQ ipcRoute
         NO-ERROR.

    FIND FIRST bfMatrix-APIInboundDetail NO-LOCK
         WHERE bfMatrix-APIInboundDetail.company    EQ ipcCompany
           AND bfMatrix-APIInboundDetail.apiRoute   EQ ipcRoute
           AND bfMatrix-APIInboundDetail.detailType EQ "Response"
           AND bfMatrix-APIInboundDetail.detailID   EQ "matrix"
           AND bfMatrix-APIInboundDetail.parentID   EQ "price"
         NO-ERROR.

    IF AVAILABLE bfPrice-APIInboundDetail THEN DO:       
        FOR EACH ttInputs:
            lcPriceResponseData = bfPrice-APIInboundDetail.data.
            IF AVAILABLE bfMatrix-APIInboundDetail THEN DO:
                lcConcatMatrixResponseData = "".
                FOR EACH ttPriceMatrix
                    WHERE ttPriceMatrix.company  EQ ttInputs.company
                      AND ttPriceMatrix.itemID   EQ ttInputs.itemID
                      AND ttPriceMatrix.custID   EQ ttInputs.custID
                      AND ttPriceMatrix.shiptoID EQ ttInputs.shiptoID:
                    lcMatrixResponseData = bfMatrix-APIInboundDetail.data.
                    
                    RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcMatrixResponseData, "Level", ttPriceMatrix.level) NO-ERROR.
                    RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcMatrixResponseData, "Quantity", TRIM(STRING(ttPriceMatrix.quantity, ">>>>>>>>9.99<<<<"))) NO-ERROR.
                    RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcMatrixResponseData, "QuantityUOM", ttPriceMatrix.quantityUOM) NO-ERROR.
                    RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcMatrixResponseData, "Price", TRIM(STRING(ttPriceMatrix.price, ">>>>>>>>9.99<<<<"))) NO-ERROR.
                    RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcMatrixResponseData, "PriceUOM", ttPriceMatrix.priceUOM) NO-ERROR.

                    lcConcatMatrixResponseData = lcConcatMatrixResponseData + "," + lcMatrixResponseData.                    
                END.
            END.
                
            lcConcatMatrixResponseData = TRIM(lcConcatMatrixResponseData, ",").
            
            lcPriceResponseData = REPLACE(lcPriceResponseData, "$matrix$", lcConcatMatrixResponseData).
            
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcPriceResponseData, "Company", ttInputs.company) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcPriceResponseData, "CustomerID", ttInputs.custID) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcPriceResponseData, "ShipToID", ttInputs.shipTOID) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcPriceResponseData, "ItemID", ttInputs.itemID) NO-ERROR.
            
            lcConcatPriceResponseData = lcConcatPriceResponseData + "," + lcPriceResponseData.
        END.
    END.

    lcConcatPriceResponseData = TRIM(lcConcatPriceResponseData, ",").
    
    oplcResponseData = REPLACE(iplcResponseDataStructure, "$price$", lcConcatPriceResponseData).

    ASSIGN        
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.
