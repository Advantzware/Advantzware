
/*------------------------------------------------------------------------
    File        : CreateInventoryAdjustmentRequestHandler.p
    Purpose     : Creates Inventory Adjustment Transaction

    Syntax      :

    Description : Creates Inventory Adjustment Transaction

    Author(s)   : Rahul Rawat
    Created     : Fri Jul 24 04:27:32 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
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
    DEFINE TEMP-TABLE ttAdjustments NO-UNDO
        FIELD company                 AS CHARACTER
        FIELD inventoryStockID        AS CHARACTER
        FIELD quantity                AS DECIMAL 
        FIELD quantityPerSubUnit      AS INTEGER
        FIELD quantitySubUnitsPerUnit AS INTEGER 
        FIELD warehouseID             AS CHARACTER
        FIELD locationID              AS CHARACTER        
        FIELD reasonCode              AS CHARACTER       
        FIELD sequenceID              AS INT64
        .

    /* The below code is added as APIInboundEvent.rec_key will be populated in the APIInboundEvent's
       create trigger, only if session.p is running persistently, else will be populated with empty value.
       ( refer methods/triggers/create.i ) */

    /* This will eventually move to setsession approach */
    &SCOPED-DEFINE NEW NEW
    {methods/defines/globdefs.i}
    {methods/defines/hndldefs.i}

    DEFINE VARIABLE hdSession AS HANDLE NO-UNDO.

    RUN nosweat/persist.p  PERSISTENT SET Persistent-Handle.
    RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

    RUN system/session.p  PERSISTENT SET hdSession.
    SESSION:ADD-SUPER-PROCEDURE (hdSession).

    {sys/inc/var.i "NEW SHARED"}
    {sys/inc/varasgn.i}

    RUN api/JSONProcs.p PERSISTENT SET hdJSONProcs.
    THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdJSONProcs).
    
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

    SESSION:REMOVE-SUPER-PROCEDURE (hdSession).
    DELETE PROCEDURE hdSession.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE pGenerateResponseData PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplcResponseDataStructure AS LONGCHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcResponseData          AS LONGCHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess                AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lcResponseData AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cStatus        AS CHARACTER NO-UNDO.
    
    FOR EACH ttAdjustments:
        ASSIGN
            lcResponseData = iplcResponseDataStructure
            cStatus        = "Adjustment created with sequence number " + STRING(ttAdjustments.sequenceID) + "."
            .
        
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData, "company", ttAdjustments.company) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData, "InventoryStockID", ttAdjustments.inventoryStockID) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData, "warehouseID", ttAdjustments.warehouseID) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData, "locationID", ttAdjustments.location) NO-ERROR.
        RUN JSON_UpdateFieldValue (INPUT-OUTPUT lcResponseData, "status", cStatus) NO-ERROR.
        
        oplcResponseData = oplcResponseData + "," + lcResponseData.
    END.
    
    oplcResponseData = TRIM(oplcResponseData, ",").
    
    ASSIGN        
        oplSuccess = TRUE
        opcMessage = "Success"
        .
    
END PROCEDURE.

PROCEDURE pPrepareInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cCompany                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInventoryStockID        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantity                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityPerSubUnit      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantitySubUnitsPerUnit AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWarehouseID             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocationID              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReasonCode              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSequenceID              AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lRecFound              AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lValidValue            AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iAdjustmentsCounter    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iIndex                 AS INTEGER NO-UNDO.
    DEFINE VARIABLE iAdjustmentsFieldOrder AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTopLevelParent        AS INTEGER NO-UNDO  INITIAL 0.
    
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
    
    /* Get the count of Adjustment records */
    RUN JSON_GetRecordCountByNameAndParent (
        INPUT  "Adjustments",
        INPUT  iTopLevelParent,
        OUTPUT iAdjustmentsCounter
        ) NO-ERROR.
    IF iAdjustmentsCounter EQ 0 THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "No records found to process Adjustments"
            .
        RETURN.
    END.        
    
     DO iIndex = 0 TO iAdjustmentsCounter - 1:
        ASSIGN
            iAdjustmentsFieldOrder   =  0
            cCompany                 = ""
            cInventoryStockID        = ""
            cQuantity                = ""
            cQuantityPerSubUnit      = ""
            cQuantitySubUnitsPerUnit = ""
            cWarehouseID             = ""
            cLocationID              = ""
            cReasonCode              = ""
            cSequenceID              = ""
            .
    
        /* Fetch the Adjustments field order, which will be further used as
           parent to fetch it's child records */
        RUN JSON_GetFieldOrderByNameValueAndParent (
            INPUT  "Adjustments",
            INPUT  STRING(iIndex),
            INPUT  iTopLevelParent,
            OUTPUT lRecFound,
            OUTPUT iAdjustmentsFieldOrder
            ) NO-ERROR.
    
        /* Fetch company code */
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "Company",
            INPUT  iAdjustmentsFieldOrder,
            OUTPUT lRecFound,
            OUTPUT cCompany
            ) NO-ERROR.
    
        /* Fetch warehouse id */
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "WarehouseID",
            INPUT  iAdjustmentsFieldOrder,
            OUTPUT lRecFound,
            OUTPUT cWareHouseID
            ) NO-ERROR.
    
        /* Fetch location id */
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "LocationID",
            INPUT  iAdjustmentsFieldOrder,
            OUTPUT lRecFound,
            OUTPUT cLocationID
            ) NO-ERROR.
                        
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "InventoryStockID",
            INPUT  iAdjustmentsFieldOrder,
            OUTPUT lRecFound,
            OUTPUT cInventoryStockID
            ) NO-ERROR.
            
        /* Fetch inventory quantity */
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "Quantity",
            INPUT  iAdjustmentsFieldOrder,
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
            INPUT  iAdjustmentsFieldOrder, 
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
    
        /* Fetch inventory QuantitySubUnitsPerUnit */
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "QuantitySubUnitsPerUnit", 
            INPUT  iAdjustmentsFieldOrder, 
            OUTPUT lRecFound, 
            OUTPUT cQuantitySubUnitsPerUnit
            ) NO-ERROR.  
        
        /* Validate QuantitySubUnitsPerUnit */
        IF lRecFound THEN DO:
            RUN spCommon_ValidateValueByDataType (
                INPUT  cQuantitySubUnitsPerUnit,
                INPUT  "INTEGER",
                OUTPUT lValidValue
                ) NO-ERROR.
    
            IF NOT lValidValue THEN DO:
                ASSIGN
                    opcMessage = "Invalid cQuantitySubUnitsPerUnit"
                    oplSuccess = NO
                    .
                RETURN.
            END.
        END.
        
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "ReasonCode",
            INPUT  iAdjustmentsFieldOrder,
            OUTPUT lRecFound,
            OUTPUT cReasonCode
            ) NO-ERROR.
            
        CREATE ttAdjustments.
        ASSIGN
            ttAdjustments.company                       = cCompany                
            ttAdjustments.inventoryStockID              = cInventoryStockID       
            ttAdjustments.quantity                      = INTEGER(cQuantity)              
            ttAdjustments.quantityPerSubUnit            = INTEGER(cQuantityPerSubUnit)    
            ttAdjustments.quantitySubUnitsPerUnit       = INTEGER(cQuantitySubUnitsPerUnit)
            ttAdjustments.warehouseID                   = cWarehouseID            
            ttAdjustments.locationID                    = cLocationID             
            ttAdjustments.reasonCode                    = cReasonCode             
            .
    END.
    
    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE pProcessInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

        DO TRANSACTION ON ERROR UNDO, LEAVE:
            FOR EACH ttAdjustments:
                
                RUN api\inbound\CreateInventoryAdjustment.p (
                    INPUT        ttAdjustments.company,                
                    INPUT        ttAdjustments.inventoryStockID ,      
                    INPUT        ttAdjustments.quantity,               
                    INPUT        ttAdjustments.quantityPerSubUnit,     
                    INPUT        ttAdjustments.quantitySubUnitsPerUnit,
                    INPUT-OUTPUT ttAdjustments.warehouseID,            
                    INPUT-OUTPUT ttAdjustments.locationID,             
                    INPUT        ttAdjustments.reasonCode,                             
                    OUTPUT       ttAdjustments.sequenceID,
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

