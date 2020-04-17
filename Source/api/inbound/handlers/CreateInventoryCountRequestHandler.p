/*------------------------------------------------------------------------
    File        : api\inbound\handlers\CreateInventoryCountRequestHandler.p
    Purpose     : Creates and posts Inventory Count Transaction

    Syntax      :

    Description : Creates and posts Inventory Count Transaction

    Author(s)   : Porandla Mithun
    Created     : Wed Dec 04 07:33:22 EDT 2019
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

    DEFINE VARIABLE hdCommonProcs AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hdJSONProcs   AS HANDLE  NO-UNDO.
    DEFINE VARIABLE iResponseCode AS INTEGER NO-UNDO.
    
    {api/inbound/ttRequest.i}

    /* Temp-table to save the input data */
    DEFINE TEMP-TABLE ttCounts NO-UNDO
        FIELD company            AS CHARACTER
        FIELD itemID             AS CHARACTER
        FIELD itemType           AS CHARACTER
        FIELD custID             AS CHARACTER
        FIELD inventoryStockID   AS CHARACTER
        FIELD warehouseID        AS CHARACTER
        FIELD locationID         AS CHARACTER
        FIELD jobID              AS CHARACTER
        FIELD jobID2             AS INTEGER
        FIELD quantity           AS INTEGER
        FIELD quantityPerSubUnit AS INTEGER
        FIELD quantityPartial    AS INTEGER
        FIELD poID               AS INTEGER
        FIELD poLine             AS INTEGER
        FIELD post               AS LOGICAL
        FIELD sequenceID         AS INT64
        FIELD ZeroOutCount       AS LOGICAL
        .

    /* The below code is added as APIInboundEvent.rec_key will be populated in the APIInboundEvent's
       create trigger, only if session.p is running persistently, else will be populated with empty value.
       ( refer methods/triggers/create.i ) */

    /* This will eventually move to setsession approach */
    &SCOPED-DEFINE NEW NEW
    {methods/defines/globdefs.i}
    {methods/defines/hndldefs.i}

    DEFINE VARIABLE hdSession AS HANDLE NO-UNDO.
    DEFINE VARIABLE hdTags    AS HANDLE NO-UNDO.

    RUN nosweat/persist.p  PERSISTENT SET Persistent-Handle.
    RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

    RUN system/session.p  PERSISTENT SET hdSession.
    SESSION:ADD-SUPER-PROCEDURE (hdSession).

    RUN system/TagProcs.p PERSISTENT SET hdTags.
    SESSION:ADD-SUPER-PROCEDURE (hdTags).

    {sys/inc/var.i "NEW SHARED"}
    {sys/inc/varasgn.i}

    RUN api/JSONProcs.p PERSISTENT SET hdJSONProcs.
    THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdJSONProcs).

    RUN system/CommonProcs.p PERSISTENT SET hdCommonProcs.
    THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdCommonProcs).

    RUN pPrepareInputs (
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

    THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdCommonProcs).
    DELETE PROCEDURE hdCommonProcs.

    SESSION:REMOVE-SUPER-PROCEDURE (hdSession).
    DELETE PROCEDURE hdSession.

    SESSION:REMOVE-SUPER-PROCEDURE (hdTags).
    DELETE PROCEDURE hdTags.

    PROCEDURE pPrepareInputs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch input data into temp-table ttCounts
     Notes:
    ------------------------------------------------------------------------------*/     
        DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

        DEFINE VARIABLE cCompany            AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cInventoryStockID   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cItemID             AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cItemType           AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cCustID             AS CHARACTER NO-UNDo.
        DEFINE VARIABLE cWarehouseID        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cLocationID         AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cJobID              AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cJobID2             AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cQuantity           AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cQuantityPerSubUnit AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cQuantityPartial    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cPOID               AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cPOLine             AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cPost               AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cZeroOutCount       AS CHARACTER NO-UNDO.
        
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

        /* Get the count of Counts records */
        RUN JSON_GetRecordCountByNameAndParent (
            INPUT  "Counts",
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
                cCompany            = ""
                cInventoryStockID   = ""
                cItemID             = ""
                cItemType           = ""
                cCustID             = ""
                cWarehouseID        = ""
                cLocationID         = ""
                cJobID              = ""
                cJobID2             = ""
                cQuantity           = ""
                cQuantityPerSubUnit = ""
                cQuantityPartial    = ""
                cPOID               = ""
                cPost               = ""
                .

            /* Fetch the Rceipts field order, which will be further used as
               parent to fetch it's child records */
            RUN JSON_GetFieldOrderByNameValueAndParent (
                INPUT  "Counts",
                INPUT  STRING(iIndex),
                INPUT  iTopLevelParent,
                OUTPUT lRecFound,
                OUTPUT iCountsFieldOrder
                ) NO-ERROR.

            /* Fetch company code */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "Company",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cCompany
                ) NO-ERROR.

            /* Fetch ItemID */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "ItemID",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cItemID
                ) NO-ERROR.

            /* Fetch ItemID */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "ItemType",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cItemType
                ) NO-ERROR.

            /* Fetch Cusotmer ID */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "CustomerID",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cCustID
                ) NO-ERROR.

            /* Fetch warehouse id */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "WarehouseID",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cWareHouseID
                ) NO-ERROR.

            /* Fetch location id */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "LocationID",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cLocationID
                ) NO-ERROR.

            /* Fetch inventory stock ID */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "InventoryStockID",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cInventoryStockID
                ) NO-ERROR.

            /* Fetch inventory Job Number */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "JobID",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cJobID
                ) NO-ERROR.

            /* Fetch inventory Job Number2 */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "JobID2",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cJobID2
                ) NO-ERROR.

            /* Validate JobID2 */
            IF lRecFound THEN DO:
                RUN spCommon_ValidateValueByDataType (
                    INPUT  cJobID2,
                    INPUT  "INTEGER",
                    OUTPUT lValidValue
                    ) NO-ERROR.

                IF NOT lValidValue THEN DO:
                    ASSIGN
                        opcMessage = "Invalid JobID2"
                        oplSuccess = NO
                        .
                    RETURN.
                END.
            END.

            /* Fetch inventory quantity */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "Quantity",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cQuantity
                ) NO-ERROR.

            /* Validate Quantity */
            IF lRecFound THEN DO:
                RUN spCommon_ValidateValueByDataType (
                    INPUT  cQuantity,
                    INPUT  "INTEGER",
                    OUTPUT lValidValue
                    ) NO-ERROR.

                IF NOT lValidValue THEN DO:
                    ASSIGN
                        opcMessage = "Invalid Quantity"
                        oplSuccess = NO
                        .
                    RETURN.
                END.
            END.

            /* Fetch inventory QuantityPerSubUnit */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "QuantityPerSubUnit",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cQuantityPerSubUnit
                ) NO-ERROR.

            /* Validate QuantityPerSubUnit */
            IF lRecFound THEN DO:
                RUN spCommon_ValidateValueByDataType (
                    INPUT  cQuantityPerSubUnit,
                    INPUT  "INTEGER",
                    OUTPUT lValidValue
                    ) NO-ERROR.

                IF NOT lValidValue THEN DO:
                    ASSIGN
                        opcMessage = "Invalid QuantityPerSubUnit"
                        oplSuccess = NO
                        .
                    RETURN.
                END.
            END.

            /* Fetch inventory QuantityPartial */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "QuantityPartial",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cQuantityPartial
                ) NO-ERROR.

            /* Validate QuantityPartial */
            IF lRecFound THEN DO:
                RUN spCommon_ValidateValueByDataType (
                    INPUT  cQuantityPartial,
                    INPUT  "INTEGER",
                    OUTPUT lValidValue
                    ) NO-ERROR.

                IF NOT lValidValue THEN DO:
                    ASSIGN
                        opcMessage = "Invalid QuantityPartial"
                        oplSuccess = NO
                        .
                    RETURN.
                END.
            END.

            /* Fetch inventory PO number */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "POID",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cPOID
                ) NO-ERROR.

            /* Validate cPOID */
            IF lRecFound AND cPOID NE "" THEN DO:
                RUN spCommon_ValidateValueByDataType (
                    INPUT  cPOID,
                    INPUT  "INTEGER",
                    OUTPUT lValidValue
                    ) NO-ERROR.

                IF NOT lValidValue THEN DO:
                    ASSIGN
                        opcMessage = "Invalid POID"
                        oplSuccess = NO
                        .
                    RETURN.
                END.
            END.

            /* Fetch inventory PO Line */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "POLine",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cPOLine
                ) NO-ERROR.

            /* Validate cPOLine */
            IF lRecFound AND cPOLine NE "" THEN DO:
                RUN spCommon_ValidateValueByDataType (
                    INPUT  cPOLine,
                    INPUT  "INTEGER",
                    OUTPUT lValidValue
                    ) NO-ERROR.

                IF NOT lValidValue THEN DO:
                    ASSIGN
                        opcMessage = "Invalid PO Line"
                        oplSuccess = NO
                        .
                    RETURN.
                END.
            END.

            /* Fetch Post flag */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "PostCount",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cPost
                ) NO-ERROR.

            /* Validate Post flag */
            IF lRecFound AND cPost NE "" THEN DO:
                RUN spCommon_ValidateValueByDataType (
                    INPUT  cPost,
                    INPUT  "LOGICAL",
                    OUTPUT lValidValue
                    ) NO-ERROR.

                IF NOT lValidValue THEN DO:
                    ASSIGN
                        opcMessage = "Invalid Post flag"
                        oplSuccess = NO
                        .
                    RETURN.
                END.
            END.

            /* Fetch Zero Out flag */
            RUN JSON_GetFieldValueByNameAndParent (
                INPUT  "ZeroOutCount",
                INPUT  iCountsFieldOrder,
                OUTPUT lRecFound,
                OUTPUT cZeroOutCount
                ) NO-ERROR.

            /* Validate Zero Out Count flag */
            IF lRecFound AND cZeroOutCount NE "" THEN DO:
                RUN spCommon_ValidateValueByDataType (
                    INPUT  cZeroOutCount,
                    INPUT  "LOGICAL",
                    OUTPUT lValidValue
                    ) NO-ERROR.

                IF NOT lValidValue THEN DO:
                    ASSIGN
                        opcMessage = "Invalid Zero Out Count flag"
                        oplSuccess = NO
                        .
                    RETURN.
                END.
            END.
                            
            CREATE ttCounts.
            ASSIGN
                ttCounts.company            = cCompany
                ttCounts.itemID             = cItemID
                ttCounts.itemType           = cItemType
                ttCounts.custID             = cCustID
                ttCounts.inventoryStockID   = cInventoryStockID
                ttCounts.warehouseID        = cWareHouseID
                ttCounts.locationID         = cLocationID
                ttCounts.jobID              = cJobID
                ttCounts.jobID2             = INTEGER(cJobID2)
                ttCounts.quantity           = INTEGER(cQuantity)
                ttCounts.quantityPerSubUnit = INTEGER(cQuantityPerSubUnit)
                ttCounts.quantityPartial    = INTEGER(cQuantityPartial)
                ttCounts.poID               = INTEGER(cPOID)
                ttCounts.poLine             = INTEGER(cPOLine)
                ttCounts.post               = LOGICAL(cPost)
                ttCounts.ZeroOutCount       = LOGICAL(cZeroOutCount)
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

        DO TRANSACTION ON ERROR UNDO, LEAVE:
            FOR EACH ttCounts:
                ASSIGN
                    g_company = ttCounts.company
                    cocode    = g_company
                    .

                RUN api\inbound\CreateInventoryCount.p (
                    INPUT        ttCounts.company,
                    INPUT        ttCounts.warehouseID,
                    INPUT        ttCounts.locationID,
                    INPUT        ttCounts.custID,
                    INPUT        ttCounts.poID,
                    INPUT        ttCounts.poLine,
                    INPUT-OUTPUT ttCounts.jobID,
                    INPUT-OUTPUT ttCounts.jobID2,
                    INPUT        ttCounts.itemID,
                    INPUT        ttCounts.itemType,
                    INPUT        ttCounts.inventoryStockID,
                    INPUT        ttCounts.quantity,
                    INPUT        ttCounts.quantityPerSubUnit,
                    INPUT        ttCounts.quantityPartial,
                    INPUT        ttCounts.post,
                    INPUT        ttCounts.ZeroOutCount,
                    OUTPUT       ttCounts.sequenceID,
                    OUTPUT       oplSuccess,
                    OUTPUT       opcMessage
                    ) NO-ERROR.

                IF ERROR-STATUS:ERROR THEN
                    ASSIGN
                        oplSuccess = FALSE
                        opcMessage = ERROR-STATUS:GET-MESSAGE(1)
                        .

                IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
                    UNDO, LEAVE.
            END.
        END.
    END PROCEDURE.

    PROCEDURE pGenerateResponseData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to generate response data
     Notes:
    ------------------------------------------------------------------------------*/     
        DEFINE INPUT  PARAMETER iplcResponseDataStructure AS LONGCHAR NO-UNDO.
        DEFINE OUTPUT PARAMETER oplcResponseData          AS LONGCHAR NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess                AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER NO-UNDO.
        
        DEFINE VARIABLE lcResponseData AS LONGCHAR  NO-UNDO.
        DEFINE VARIABLE cStatus        AS CHARACTER NO-UNDO.
        
        FOR EACH ttCounts:
            ASSIGN
                lcResponseData = iplcResponseDataStructure
                cStatus        = "Count Inventory created with sequence number " + STRING(ttCounts.sequenceID) + "."
                cStatus        = IF ttCounts.post THEN
                                     cStatus + " Posting successfull"
                                 ELSE
                                     cStatus
                .
            
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData, "company", ttCounts.company) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData, "itemID", ttCounts.itemID) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData, "jobID", ttCounts.jobID) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData, "jobID2", STRING(ttCounts.jobID2)) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData, "warehouseID", ttCounts.warehouseID) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData, "locationID", ttCounts.location) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData, "inventoryStockID", ttCounts.inventoryStockID) NO-ERROR.
            RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData, "status", cStatus) NO-ERROR.
            
            oplcResponseData = oplcResponseData + "," + lcResponseData.
        END.

        oplcResponseData = TRIM(oplcResponseData, ",").
        
        ASSIGN        
            oplSuccess = TRUE
            opcMessage = "Success"
            .
    END PROCEDURE.
