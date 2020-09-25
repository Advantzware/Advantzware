/*------------------------------------------------------------------------
    File        : api\inbound\ConsumeInventoryViaBOL.p
    Purpose     : Consume Inventory Via Bill Of Lading

    Syntax      :

    Description : Consume Inventory Via Bill Of Lading

    Author(s)   : Mithun Porandla
    Created     : Thu October 24 05:16:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcRoute                  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcVerb                   AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER iplcRequestData           AS LONGCHAR   NO-UNDO.
    DEFINE OUTPUT PARAMETER ipcRequestedBy            AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess                AS LOGICAL    NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER  NO-UNDO.

    /* BOL Header detail variables */
    DEFINE VARIABLE cCompany        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBOLID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocationID     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBOLDate        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCarrierID      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTrailerID      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFreightTerms   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFreightCost    AS CHARACTER NO-UNDO.

    /* BOL Line detail variables */
    DEFINE VARIABLE cItemID            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInventoryStockID  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityOfUnits   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityPerUnit   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityPartial   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPartialOrComplete AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lRecFound          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidValue        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSkipSelectBins    AS LOGICAL   No-UNDO.
    DEFINE VARIABLE iLineCounter       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndex             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLineFieldOrder    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hdJSONProcs        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdCommonProcs      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE riOeboll           AS ROWID     NO-UNDO.
    DEFINE VARIABLE riOebolh           AS ROWID     NO-UNDO.
    DEFINE VARIABLE riFgbin            AS ROWID     NO-UNDO.

    DEFINE VARIABLE iTopLevelParent    AS INTEGER   NO-UNDO INITIAL 0.
    DEFINE VARIABLE cValidFreightTerms AS CHARACTER NO-UNDO INITIAL "P,B,C,T".

    {methods/defines/globdefs.i}
    {methods/defines/hndldefs.i}
    
    DEFINE VARIABLE hdSession AS HANDLE NO-UNDO.
    DEFINE VARIABLE hdTags    AS HANDLE NO-UNDO.
        
    {sys/inc/var.i NEW SHARED}
    {sys/inc/varasgn.i}  
        
    {api/inbound/ttRequest.i}
    {oe/bolcheck.i NEW}
    {oe/closchk.i NEW}

    /* Temp-table to store the fg-bin buffer for the inputs */
    DEFINE TEMP-TABLE ttBin NO-UNDO
        FIELD company     AS CHARACTER
        FIELD BOLID       AS INTEGER
        FIELD itemID      AS CHARACTER
        FIELD tag         AS CHARACTER
        FIELD jobID       AS CHARACTER
        FIELD jobID2      AS INTEGER
        FIELD warehouseID AS CHARACTER
        FIELD locationID  AS CHARACTER
        FIELD customerID  AS CHARACTER
        FIELD fgbinRowId  AS ROWID
        FIELD oebollRowId AS ROWID
        FIELD processed   AS LOGICAL
        .
    
    /* Temp-table to store the input data */
    DEFINE TEMP-TABLE ttInputs NO-UNDO
        FIELD company            AS CHARACTER
        FIELD BOLID              AS INTEGER
        FIELD locationID         AS CHARACTER
        FIELD BOLDate            AS DATE
        FIELD carrierID          AS CHARACTER
        FIELD trailerID          AS CHARACTER
        FIELD freightTerms       AS CHARACTER
        FIELD freightCost        AS DECIMAL
        FIELD itemID             AS CHARACTER
        FIELD inventoryStockID   AS CHARACTER
        FIELD quantityOfUnits    AS DECIMAL
        FIELD quantityPerUnit    AS DECIMAL
        FIELD quantityPartial    AS DECIMAL
        FIELD quantityTotal      AS DECIMAL
        FIELD partialOrComplete  AS CHARACTER
        FIELD oldFreightCost     AS DECIMAL
        .

    RUN api/JSONProcs.p PERSISTENT SET hdJSONProcs.
    THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdJSONProcs).

    RUN system/CommonProcs.p PERSISTENT SET hdCommonProcs.
    THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdCommonProcs).    

    DEFINE NEW SHARED BUFFER xoe-ord FOR oe-ord.

    /* Get request data fields in a temp-table */
    RUN ReadRequestData (
        INPUT  iplcRequestData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage,
        OUTPUT TABLE ttRequest
        ).

    IF NOT oplSuccess THEN
        RETURN.

    /* Fetch requested by from request data */
    RUN JSON_GetFieldValueByName (
        INPUT  "RequestedBy", 
        OUTPUT lRecFound, 
        OUTPUT ipcRequestedBy
        ) NO-ERROR.

    /* Fetch company from request data */
    RUN JSON_GetFieldValueByName (
        INPUT  "Company", 
        OUTPUT lRecFound, 
        OUTPUT cCompany
        ) NO-ERROR.
    
    /* Company validation */
    IF NOT lRecFound OR
       NOT CAN-FIND(FIRST company NO-LOCK
                    WHERE company.company EQ cCompany) THEN DO:
        ASSIGN
            opcMessage = "Invalid Company"
            oplSuccess = NO
            .
        RETURN.
    END.
    
    ASSIGN
        g_company = cCompany
        cocode    = g_company
        .
        
    /* Fetch location from request data */
    RUN JSON_GetFieldValueByName (
        INPUT  "LocationID",
        OUTPUT lRecFound,
        OUTPUT cLocationID
        ) NO-ERROR.
    
    /* Location validation */
    IF NOT lRecFound OR
       NOT CAN-FIND(FIRST loc NO-LOCK
                    WHERE loc.company EQ cCompany
                      AND loc.loc     EQ cLocationID) THEN DO:
        ASSIGN
            opcMessage = "Invalid WareHouseID"
            oplSuccess = NO
            .
        RETURN.
    END.

    /* Fetch BOLID from request data */
    RUN JSON_GetFieldValueByName (
        INPUT  "BOLID",
        OUTPUT lRecFound,
        OUTPUT cBOLID
        ) NO-ERROR.
    
    /* Validate BOL ID */
    IF lRecFound THEN DO:
        RUN spCommon_ValidateValueByDataType (
            INPUT  cBOLID,
            INPUT  "INTEGER",
            OUTPUT lValidValue
            ) NO-ERROR.
            
        IF NOT lValidValue THEN DO:
            ASSIGN
                opcMessage = "Invalid BOLID"
                oplSuccess = NO
                .
            RETURN.    
        END.
    END.
    
    /* BOL ID validation */
    IF NOT lRecFound OR
       NOT CAN-FIND(FIRST oe-bolh NO-LOCK
                    WHERE oe-bolh.company EQ cCompany
                      AND oe-bolh.bol-no  EQ INTEGER(cBOLID)) THEN DO:
        ASSIGN
            opcMessage = "Invalid BOLID"
            oplSuccess = NO
            .
        RETURN.
    END.

    /* Fetch BOLDate from request data */
    RUN JSON_GetFieldValueByName (
        INPUT  "BOLDate",
        OUTPUT lRecFound,
        OUTPUT cBOLDate
        ) NO-ERROR.
    
    /* Validate BOLDate */
    IF lRecFound AND cBOLDate NE "" THEN DO:
        RUN spCommon_ValidateValueByDataType (
            INPUT  cBOLDate,
            INPUT  "DATE",
            OUTPUT lValidValue
            ) NO-ERROR.
            
        IF NOT lValidValue THEN DO:
            ASSIGN
                opcMessage = "Invalid BOLDate"
                oplSuccess = NO
                .
            RETURN.    
        END.
    END.
    
    /* Fetch carrier id from request data */
    RUN JSON_GetFieldValueByName (
        INPUT  "CarrierID",
        OUTPUT lRecFound,
        OUTPUT cCarrierID
        ) NO-ERROR.
    /* Carrier ID validation */
    IF lRecFound AND cCarrierID NE "" AND
       NOT CAN-FIND(FIRST carrier NO-LOCK
                    WHERE carrier.company EQ cCompany
                      AND carrier.loc     EQ cLocationID
                      AND carrier.carrier EQ cCarrierID) THEN DO:
        ASSIGN
            opcMessage = "Invalid CarrierID"
            oplSuccess = NO
            .
        RETURN.
    END.

    /* Fetch trailer id from request data */
    RUN JSON_GetFieldValueByName (
        INPUT  "TrailerID",
        OUTPUT lRecFound,
        OUTPUT cTrailerID
        ) NO-ERROR.
    
    /* Fetch freight terms from request data */
    RUN JSON_GetFieldValueByName (
        INPUT  "FreightTerms",
        OUTPUT lRecFound,
        OUTPUT cFreightTerms
        ) NO-ERROR.

    IF NOT lRecFound OR
       LOOKUP(cFreightTerms, cValidFreightTerms) LE 0 THEN DO:
        ASSIGN
            opcMessage = "Invalid Freight Terms"
            oplSuccess = NO
            .
        RETURN.
    END.

    /* Fetch freight cost from request data */
    RUN JSON_GetFieldValueByName (
        INPUT  "FreightCost",
        OUTPUT lRecFound,
        OUTPUT cFreightCost
        ) NO-ERROR.
    /* Validate Freight Cost */
    IF lRecFound AND cFreightCost NE "" THEN DO:
        RUN spCommon_ValidateValueByDataType (
            INPUT  cFreightCost,
            INPUT  "DECIMAL",
            OUTPUT lValidValue
            ) NO-ERROR.
            
        IF NOT lValidValue THEN DO:
            ASSIGN
                opcMessage = "Invalid Freight Cost"
                oplSuccess = NO
                .
            RETURN.    
        END.
    END.
    
    /* Get the count of Line detail records */
    RUN JSON_GetRecordCountByNameAndParent (
        INPUT  "LineDetails", 
        INPUT  iTopLevelParent, 
        OUTPUT iLineCounter
        ) NO-ERROR.
    
    IF iLineCounter EQ 0 THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Missing BOL lines in request"
            .
        RETURN.
    END.
    
    /* Browse through all the Line detail records */
    DO iIndex = 0 TO iLineCounter - 1:
        
        ASSIGN
            iLineFieldOrder    = 0
            cItemID            = ""
            cInventoryStockID  = ""
            cQuantityOfUnits   = ""
            cQuantityPerUnit   = ""
            cQuantityPartial   = ""
            cPartialOrComplete = ""
            .
        
        /* Fetch the Line details field order, which will be further used as 
           parent to fetch it's child records */    
        RUN JSON_GetFieldOrderByNameValueAndParent (
            INPUT  "LineDetails", 
            INPUT  STRING(iIndex), 
            INPUT  iTopLevelParent, 
            OUTPUT lRecFound, 
            OUTPUT iLineFieldOrder
            ) NO-ERROR.

        /* Fetch item number */
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "ItemID", 
            INPUT  iLineFieldOrder, 
            OUTPUT lRecFound, 
            OUTPUT cItemID
            ) NO-ERROR.

        /* Fetch inventory stock ID (tag) */            
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "InventoryStockID", 
            INPUT  iLineFieldOrder, 
            OUTPUT lRecFound, 
            OUTPUT cInventoryStockID
            ) NO-ERROR.

        /* Fetch Quantity of units */                
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "QuantityOfUnits", 
            INPUT  iLineFieldOrder, 
            OUTPUT lRecFound, 
            OUTPUT cQuantityOfUnits
            ) NO-ERROR.
        
        /* Validate Quantity of units */
        IF lRecFound AND
            cQuantityOfUnits NE "" THEN DO:
            RUN spCommon_ValidateValueByDataType (
                INPUT  cQuantityOfUnits,
                INPUT  "DECIMAL",
                OUTPUT lValidValue
                ) NO-ERROR.
                
            IF NOT lValidValue THEN DO:
                ASSIGN
                    opcMessage = "Invalid QuantityOfUnits for item " + cItemID
                    oplSuccess = NO
                    .
                RETURN.    
            END.
        END.
        
        /* Fetch QuantityPerUnit  */        
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "QuantityPerUnit", 
            INPUT  iLineFieldOrder, 
            OUTPUT lRecFound, 
            OUTPUT cQuantityPerUnit
            ) NO-ERROR.
        
        /* Validate QuantityPerUnit */
        IF lRecFound AND
            cQuantityPerUnit NE "" THEN DO:
            RUN spCommon_ValidateValueByDataType (
                INPUT  cQuantityPerUnit,
                INPUT  "DECIMAL",
                OUTPUT lValidValue
                ) NO-ERROR.
                
            IF NOT lValidValue THEN DO:
                ASSIGN
                    opcMessage = "Invalid QuantityPerUnit for item " + cItemID
                    oplSuccess = NO
                    .
                RETURN.    
            END.
        END.
        
        /* Fetch Quantity Partial */            
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "QuantityPartial", 
            INPUT  iLineFieldOrder, 
            OUTPUT lRecFound, 
            OUTPUT cQuantityPartial
            ) NO-ERROR.
        
        /* Validate Quantity Partial */
        IF lRecFound AND
            cQuantityPartial NE "" THEN DO:
            RUN spCommon_ValidateValueByDataType (
                INPUT  cQuantityPartial,
                INPUT  "DECIMAL",
                OUTPUT lValidValue
                ) NO-ERROR.
                
            IF NOT lValidValue THEN DO:
                ASSIGN
                    opcMessage = "Invalid QuantityPartial for item " + cItemID
                    oplSuccess = NO
                    .
                RETURN.    
            END.
        END.
        
        /* Fetch Partial Or Complete */            
        RUN JSON_GetFieldValueByNameAndParent (
            INPUT  "PartialOrComplete", 
            INPUT  iLineFieldOrder, 
            OUTPUT lRecFound, 
            OUTPUT cPartialOrComplete
            ) NO-ERROR.

        /* Store the input data into temp-table */
        CREATE ttInputs.
        ASSIGN
            ttInputs.company            = cCompany
            ttInputs.BOLID              = INTEGER(cBOLID)
            ttInputs.locationID         = cLocationID
            ttInputs.BOLDate            = DATE(cBOLDate)
            ttInputs.carrierID          = cCarrierID
            ttInputs.trailerID          = cTrailerID
            ttInputs.freightTerms       = cFreightTerms
            ttInputs.freightCost        = DECIMAL(cFreightCost)
            ttInputs.itemID             = cItemID
            ttInputs.inventoryStockID   = cInventoryStockID
            ttInputs.quantityOfUnits    = DECIMAL(cQuantityOfUnits)
            ttInputs.quantityPerUnit    = DECIMAL(cQuantityPerUnit)
            ttInputs.quantityPartial    = DECIMAL(cQuantityPartial)
            ttInputs.quantityTotal      = (ttInputs.quantityOfUnits * ttInputs.quantityPerUnit) + ttInputs.quantityPartial
            ttInputs.partialOrComplete  = cPartialOrComplete
            .
    END.
    
    /* Validate and return error if duplicate records exist for 
       BOLID, ItemID and InventoryStockID in ttInputs table */
    RUN ValidateDuplicateInputs (
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
    IF NOT oplSuccess THEN
        RETURN.
    
    RUN ValidateBOLQuantity (
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
    IF NOT oplSuccess THEN
        RETURN.
        
    FOR EACH ttInputs:
        /* Validate BOLID */
        RUN ValidateBOL (
            INPUT  ttInputs.Company,
            INPUT  ttInputs.BOLID,
            OUTPUT riOebolh,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.
        IF NOT oplSuccess THEN
            RETURN.
        
        /* Fetch the current freight cost from oe-bolh record and store in ttInputs table.
           This will be used to compare the old freight cost with new freight cost */
        RUN GetBOLHeaderFreightCost (
            INPUT  riOebolh,
            OUTPUT ttInputs.oldFreightCost
            ) NO-ERROR.
        
        /* Validate BOL Line with company, BOLID and itemID */
        RUN ValidateBOLLine (
            INPUT  ttInputs.company,
            INPUT  ttInputs.BOLID,
            INPUT  ttInputs.itemID,
            INPUT  ttInputs.inventoryStockID,
            OUTPUT riOeboll,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.
        IF NOT oplSuccess THEN
            RETURN.
        
        /* Validate inventoryStockID from fg-bin */
        RUN ValidateInventoryStockID (
            INPUT  riOeboll,
            INPUT  ttInputs.inventoryStockID,
            OUTPUT riFgbin,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.
        IF NOT oplSuccess THEN
            RETURN.

        /* Create ttBin records for further processing */    
        RUN SelectBinsTags (
            INPUT  riOeboll,
            INPUT  riFgbin,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.
        IF NOT oplSuccess THEN
            RETURN.
    END.
    
    DO TRANSACTION ON ERROR UNDO, LEAVE:
        FOR EACH ttBin
            WHERE ttBin.processed EQ FALSE:
    
            FIND FIRST ttInputs
                 WHERE ttInputs.BOLID            EQ ttBin.BOLID
                   AND ttInputs.itemID           EQ ttBin.itemID
                   AND ttInputs.inventoryStockID EQ ttBin.tag
                 NO-ERROR.
            IF AVAILABLE ttInputs THEN DO:
                /* Process the ttBin records for applying changes to oe-boll records */
                RUN ProcessBinsTags (
                    INPUT  ttBin.company,
                    INPUT  ttBin.BOLID,
                    INPUT  ttBin.itemID,
                    OUTPUT oplSuccess,
                    OUTPUT opcMessage
                    ) NO-ERROR.
                IF NOT oplSuccess THEN
                    UNDO, LEAVE.
            END.
        END.

        FOR EACH ttInputs
            BREAK BY ttInputs.BOLID:
    
            IF FIRST-OF(ttInputs.BOLID) THEN DO:
                /* Re-Calculate Freight cost after processing the ttBin records */
                RUN ReCalculateFreightCost (
                    INPUT  ttInputs.company,
                    INPUT  ttInputs.BOLID,
                    OUTPUT oplSuccess,
                    OUTPUT opcMessage
                    ) NO-ERROR.
                IF NOT oplSuccess THEN
                    opcMessage = "Unable to recalculate Freight cost for " + STRING(ttInputs.BOLID).
                
                /* Update the oe-bolh freight cost with the input freight cost and
                   update the oe-boll records freight cost */ 
                RUN UpdateBOLFreightCost (
                    INPUT  ttInputs.company,
                    INPUT  ttInputs.BOLID,
                    INPUT  ttInputs.freightCost,
                    INPUT  ttInputs.oldFreightCost,
                    OUTPUT oplSuccess,
                    OUTPUT opcMessage
                    ) NO-ERROR.
                IF NOT oplSuccess THEN
                    opcMessage = "Unable to update Freight cost for " + STRING(ttInputs.BOLID). 
                
                /* Update bol-date, carrier, trailer and freight terms before posting */
                FIND FIRST oe-bolh EXCLUSIVE-LOCK
                     WHERE oe-bolh.company EQ ttInputs.company
                       and oe-bolh.bol-no  EQ ttInputs.BOLID NO-ERROR.
                IF AVAILABLE oe-bolh THEN
                    ASSIGN
                        oe-bolh.bol-date = IF ttInputs.BOLDate NE ? THEN 
                                               ttInputs.BOLDate
                                           ELSE
                                               oe-bolh.bol-date
                        oe-bolh.carrier  = IF ttInputs.carrierID NE "" THEN
                                               ttInputs.carrierID
                                           ELSE
                                               oe-bolh.carrier
                        oe-bolh.trailer  = IF ttInputs.trailerID NE "" THEN
                                               ttInputs.trailerID
                                           ELSE
                                               oe-bolh.trailer
                        oe-bolh.frt-pay  = IF ttInputs.freightTerms NE "" THEN
                                               ttInputs.freightTerms
                                           ELSE
                                               oe-bolh.frt-pay
                        NO-ERROR.

                RELEASE oe-bolh.
                
                RUN PostBOL (
                    INPUT  ttInputs.company,
                    INPUT  ttInputs.BOLID,
                    OUTPUT oplSuccess,
                    OUTPUT opcMessage
                    ) NO-ERROR.
                IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
                    UNDO, LEAVE.
                
                /* Update the oe-bolh.user-id and oe-boll.printed */
                FIND FIRST oe-bolh NO-LOCK
                     WHERE oe-bolh.company EQ ttInputs.company
                       and oe-bolh.bol-no  EQ ttInputs.BOLID NO-ERROR.
                IF AVAILABLE oe-bolh THEN DO:                    
                    FOR EACH oe-boll EXCLUSIVE-LOCK
                        WHERE oe-boll.company EQ oe-bolh.company
                          AND oe-boll.b-no    EQ oe-bolh.b-no:
                        oe-boll.printed = TRUE.
                    END.                
                END.

                RELEASE oe-boll.                
            END.
        END.
    END.

    EMPTY TEMP-TABLE ttInputs.
    EMPTY TEMP-TABLE ttBin.
    
    IF NOT oplSuccess THEN
        RETURN.
        
    ASSIGN
        opcMessage = "Inventory Consumed successfully"
        oplSuccess = TRUE
        .



/* **********************  Internal Procedures  *********************** */


    PROCEDURE ValidateBOL:
        /* Purpose: Validate if a given bol-no exist in oe-bolh table */
        
        DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipiBOLID   AS INTEGER   NO-UNDO.
        DEFINE OUTPUT PARAMETER opriOebolh AS ROWID     NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

        DEFINE BUFFER bf-oe-bolh FOR oe-bolh.

        FIND FIRST bf-oe-bolh NO-LOCK
             WHERE bf-oe-bolh.company EQ ipcCompany
               AND bf-oe-bolh.bol-no  EQ ipiBOLID
             NO-ERROR.
        IF NOT AVAILABLE bf-oe-bolh THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid BOL Number"
                .
            RETURN.
        END.

        IF bf-oe-bolh.posted THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "BOL Number " + STRING(bf-oe-bolh.bol-no) + " already posted"
                .
            RETURN.
        END.

        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .

        RELEASE bf-oe-bolh.
    END PROCEDURE.

    PROCEDURE GetBOLHeaderFreightCost:
        /* Purpose: Fetch the freight cost of a given oe-bolh records */
        
        DEFINE INPUT  PARAMETER ipriOebolh     AS ROWID   NO-UNDO.
        DEFINE OUTPUT PARAMETER opdFreightCost AS DECIMAL NO-UNDO.

        DEFINE BUFFER bf-oe-bolh FOR oe-bolh.

        FIND FIRST bf-oe-bolh NO-LOCK
             WHERE ROWID(bf-oe-bolh) EQ ipriOebolh NO-ERROR.
        IF AVAILABLE bf-oe-bolh THEN
            opdFreightCost = bf-oe-bolh.freight.

        RELEASE bf-oe-bolh.
    END PROCEDURE.

    PROCEDURE ValidateBOLLine:
        /* Purpose: Validate if a given item exist for the bol-no in oe-boll table */
    
        DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipiBOLID   AS INTEGER   NO-UNDO.
        DEFINE INPUT  PARAMETER ipcItemID  AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipcTag     AS CHARACTER NO-UNDO.
        DEFINE OUTPUT PARAMETER oprioeBoll AS ROWID     NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

        DEFINE BUFFER bf-oe-boll FOR oe-boll.

        FIND FIRST bf-oe-boll NO-LOCK
             WHERE bf-oe-boll.company EQ ipcCompany
               AND bf-oe-boll.bol-no  EQ ipiBOLID
               AND bf-oe-boll.i-no    EQ ipcItemID
               AND bf-oe-boll.tag     EQ ipcTag
             NO-ERROR.
        IF NOT AVAILABLE bf-oe-boll THEN DO:
            FIND FIRST bf-oe-boll NO-LOCK
                 WHERE bf-oe-boll.company EQ ipcCompany
                   AND bf-oe-boll.bol-no  EQ ipiBOLID
                   AND bf-oe-boll.i-no    EQ ipcItemID
                 NO-ERROR.
            IF NOT AVAILABLE bf-oe-boll THEN DO:
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = "Invalid BOL Line"
                    .
            END.
        END.

        ASSIGN
            opriOeboll = ROWID(bf-oe-boll)
            oplSuccess = TRUE
            opcMessage = "Success"
            .

        RELEASE bf-oe-boll.
    END PROCEDURE.

    PROCEDURE ValidateBOLQuantity:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to validate if at least one of the input line quantity is 
              not zero
     Notes:
    ------------------------------------------------------------------------------*/
        DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
        
        DEFINE BUFFER bf-ttInputs FOR ttInputs.
        
        FOR EACH bf-ttInputs
            BREAK BY bf-ttInputs.BOLID:
            IF FIRST-OF(bf-ttInputs.BOLID) THEN DO:
                IF NOT CAN-FIND(FIRST ttInputs
                                WHERE ttInputs.company       EQ bf-ttInputs.company
                                  AND ttInputs.BOLID         EQ bf-ttInputs.BOLID
                                  AND ttInputs.quantityTotal NE 0) THEN DO:
                    ASSIGN
                        oplSuccess = FALSE
                        opcMessage = "Quantity is zero for all lines for BOL # " + STRING(bf-ttInputs.BOLID)
                        .
                    RETURN.                    
                END.
            END.
        END.

        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .
    END PROCEDURE.

    PROCEDURE ValidateInventoryStockID:
        /* Purpose: Validate if a given tag exist in fg-bin table */
        
        DEFINE INPUT  PARAMETER ipriOeboll AS ROWID     NO-UNDO.
        DEFINE INPUT  PARAMETER ipcTag     AS CHARACTER NO-UNDO.
        DEFINE OUTPUT PARAMETER opriFgbin  AS ROWID     NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

        DEFINE BUFFER bf-oe-boll FOR oe-boll.
        DEFINE BUFFER bf-fg-bin  FOR fg-bin.

        FIND FIRST bf-oe-boll NO-LOCK
             WHERE ROWID(bf-oe-boll) EQ ipriOeboll
             NO-ERROR.
        IF NOT AVAILABLE bf-oe-boll THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid BOL Line"
                .
            RETURN.
        END.

        IF ipcTag EQ "" THEN DO:
            ASSIGN
                oplSuccess = TRUE
                opcMessage = "Success"
                .
            RETURN.
        END.

        FIND FIRST bf-fg-bin NO-LOCK
             WHERE bf-fg-bin.company EQ bf-oe-boll.company
               AND bf-fg-bin.i-no    EQ bf-oe-boll.i-no
               AND bf-fg-bin.tag     EQ ipcTag
               AND bf-fg-bin.qty     GT 0
               AND bf-fg-bin.cust-no EQ ""
            NO-ERROR.
        IF NOT AVAILABLE bf-fg-bin THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid Inventory Stock ID"
                .
            RETURN.
        END.

        IF NOT ((TRIM(bf-fg-bin.job-no) EQ "" OR
           NOT CAN-FIND(
               FIRST job
               WHERE job.company EQ bf-fg-bin.company
                 AND job.job-no  EQ bf-fg-bin.job-no
                 AND job.job-no2 EQ bf-fg-bin.job-no2
                 AND job.stat    EQ "H")
                 )) THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Job not available on the FG Bin with the item " + bf-oe-boll.i-no
                .
            RETURN.
        END.

        ASSIGN
            opriFgbin  = ROWID(bf-fg-bin)
            oplSuccess = TRUE
            opcMessage = "Success"
            .

        RELEASE bf-oe-boll.
        RELEASE bf-fg-bin.
    END PROCEDURE.

    PROCEDURE ValidateOrderQuantityWithInputs:
        /* Purpose: Validate if input quantity for an item excceds the oe-ordl quantity */

        DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipiBOLID   AS INTEGER   NO-UNDO.
        DEFINE INPUT  PARAMETER ipcItemID  AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipriOeboll AS ROWID     NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

        DEFINE BUFFER bf-ttInputs FOR ttInputs.
        DEFINE BUFFER bf-oe-boll  FOR oe-boll.
        DEFINE BUFFER bf-oe-ord   FOR oe-ord.
        DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.        

        DEFINE VARIABLE dTotalQuantity AS DECIMAL NO-UNDO.

        FIND FIRST bf-oe-boll NO-LOCK
             WHERE ROWID(bf-oe-boll) EQ ipriOeboll
             NO-ERROR.
        IF NOT AVAILABLE bf-oe-boll THEN
            RETURN.

        FOR EACH bf-ttInputs
            WHERE bf-ttInputs.company EQ ipcCompany
              AND bf-ttInputs.BOLID   EQ ipiBOLID
              AND bf-ttInputs.itemID  EQ ipcItemID:
            dTotalQuantity = dTotalQuantity + bf-ttInputs.quantityTotal.
        END.
          
        FIND FIRST bf-oe-ord
             WHERE bf-oe-ord.company EQ bf-oe-boll.company
               AND bf-oe-ord.ord-no  EQ INT(bf-oe-boll.ord-no) 
             NO-LOCK NO-ERROR.
    
        FIND FIRST bf-oe-ordl
            WHERE bf-oe-ordl.company EQ bf-oe-boll.company
              AND bf-oe-ordl.ord-no  EQ INT(bf-oe-boll.ord-no) 
              AND bf-oe-ordl.line    EQ bf-oe-boll.line
             NO-LOCK NO-ERROR.
        IF AVAILABLE bf-oe-ord AND
           AVAILABLE bf-oe-ordl AND
           dTotalQuantity GT bf-oe-ordl.qty * (1 + (bf-oe-ordl.over-pct / 100)) THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Qty Shipped (" + STRING(dTotalQuantity) 
                           + ") for item " + bf-oe-boll.i-no + " will exceed Qty Ordered + Allowable Overrun"
                .
            RETURN.
        END.
        
        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .
        
        RELEASE bf-ttInputs.
        RELEASE bf-oe-boll.
        RELEASE bf-oe-ord.
        RELEASE bf-oe-ordl.
    END PROCEDURE.

    PROCEDURE ValidateFGBinQuantityWithInputs:
        /* Purpose: Validate if input quantity for an item exceeds the fg-bin quantity */

        DEFINE INPUT  PARAMETER ipdQuantity AS INTEGER   NO-UNDO.
        DEFINE INPUT  PARAMETER ipriFgbin   AS ROWID     NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.

        DEFINE BUFFER bf-fg-bin  FOR fg-bin.

        FIND FIRST bf-fg-bin NO-LOCK
             WHERE ROWID(bf-fg-bin) EQ ipriFgbin
             NO-ERROR.
        IF NOT AVAILABLE bf-fg-bin THEN DO:
            ASSIGN
                oplSuccess = TRUE
                opcMessage = "Success"
                .
            RETURN.
        END.
        
        IF ipdQuantity GT bf-fg-bin.qty THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Input Quantity for  (" + STRING(ipdQuantity) 
                           + ") for item " + bf-fg-bin.i-no + " will exceed Quatity available for tag "
                           + bf-fg-bin.tag
                .
            RETURN.
        END.
        
        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .
        
        RELEASE bf-fg-bin.
    END PROCEDURE.
    
    PROCEDURE SelectBinsTags:
        /* Purpose: Procedure to create the ttBin records */
            
        DEFINE INPUT  PARAMETER ipriOeboll AS ROWID     NO-UNDO.
        DEFINE INPUT  PARAMETER ipriFgBin  AS ROWID     NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

        DEFINE BUFFER bf-oe-boll FOR oe-boll.
        DEFINE BUFFER bf-fg-bin  FOR fg-bin.

        FIND FIRST bf-oe-boll NO-LOCK
             WHERE ROWID(bf-oe-boll) EQ ipriOeboll
             NO-ERROR.
        IF NOT AVAILABLE bf-oe-boll THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid BOL Line"
                .
            RETURN.
        END.

        FIND FIRST bf-fg-bin NO-LOCK
             WHERE ROWID(bf-fg-bin) EQ ipriFgbin
             NO-ERROR.
        
        /* Create ttBin records */
        RUN CreateBinsTags (
            INPUT  ROWID(bf-fg-bin),
            INPUT  ROWID(bf-oe-boll),
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.
        IF NOT oplSuccess THEN
            RETURN.

        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .

        RELEASE bf-fg-bin.
        RELEASE bf-oe-boll.
    END PROCEDURE.

    PROCEDURE CreateBinsTags:
        /* Purpose: Creates ttBin records */
        
        DEFINE INPUT  PARAMETER ipriFgbin  AS ROWID     NO-UNDO.
        DEFINE INPUT  PARAMETER ipriOeboll AS ROWID     NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

        DEFINE VARIABLE lItemType AS LOGICAL NO-UNDO.

        DEFINE BUFFER bf-fg-bin   FOR fg-bin.
        DEFINE BUFFER bf-oe-boll  FOR oe-boll.

        FIND FIRST bf-fg-bin NO-LOCK
             WHERE ROWID(bf-fg-bin) EQ ipriFgbin
             NO-ERROR.

        FIND FIRST bf-oe-boll NO-LOCK
             WHERE ROWID(bf-oe-boll) EQ ipriOeboll
             NO-ERROR.

        CREATE ttBin.
        ASSIGN
            ttBin.company     = bf-oe-boll.company
            ttBin.BOLID       = bf-oe-boll.bol-no
            ttBin.itemID      = bf-oe-boll.i-no
            ttBin.fgbinRowID  = ipriFgbin
            ttBin.oebollRowID = ipriOeboll
            ttBin.processed   = FALSE
            .               

        IF AVAILABLE bf-fg-bin THEN
            ASSIGN
                ttBin.tag         = bf-fg-bin.tag
                ttBin.jobID       = bf-fg-bin.job-no
                ttBin.jobID2      = bf-fg-bin.job-no2
                ttBin.warehouseID = bf-fg-bin.loc
                ttBin.locationID  = bf-fg-bin.loc-bin
                ttBin.customerID  = bf-fg-bin.cust-no
                .
                        
        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .

        RELEASE bf-fg-bin.
        RELEASE bf-oe-boll.
    END PROCEDURE.

    PROCEDURE ValidateDuplicateInputs:
        /* Purpose: Validates duplicate records in ttInputs for BOLID, ItemID and inventoryStockID */
        DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

        DEFINE BUFFER bf-ttInputs1 FOR ttInputs.
        DEFINE BUFFER bf-ttInputs2 FOR ttInputs.

        FOR EACH bf-ttInputs1:
            FIND FIRST bf-ttInputs2
                 WHERE bf-ttInputs2.company          EQ bf-ttInputs1.company
                   AND bf-ttInputs2.BOLID            EQ bf-ttInputs1.BOLID
                   AND bf-ttInputs2.itemID           EQ bf-ttInputs1.itemID
                   AND bf-ttInputs2.inventoryStockID EQ bf-ttInputs1.inventoryStockID
                   AND ROWID(bf-ttInputs2)           NE ROWID(bf-ttInputs1)
                 NO-ERROR.
            IF AVAILABLE bf-ttInputs2 THEN DO:
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = "Duplicate InventoryStockID " + bf-ttInputs2.inventoryStockID
                               + " for BOLID " + STRING(bf-ttInputs2.BOLID)
                    .
                RETURN.
            END.
        END.

        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .

        RELEASE bf-ttInputs1.
        RELEASE bf-ttInputs2.
    END PROCEDURE.

    PROCEDURE ProcessBinsTags:
        /* Purpose: Process the ttBin records to apply tag quantity to oe-boll records */
        DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipiBOLID    AS INTEGER   NO-UNDO.
        DEFINE INPUT  PARAMETER ipcItemID   AS CHARACTER NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.

        DEFINE BUFFER bf-oerel          FOR oe-rel.
        DEFINE BUFFER bf-oe-boll        FOR oe-boll.
        DEFINE BUFFER bf-create-oe-boll FOR oe-boll.
        DEFINE BUFFER bf-calc-qty-boll  FOR oe-boll.
        DEFINE BUFFER bf-oe-bolh        FOR oe-bolh.
        DEFINE BUFFER bf-ttBin          FOR ttBin.
        DEFINE BUFFER bf-fg-bin         FOR fg-bin.
        DEFINE BUFFER bf-itemfg         FOR itemfg.
        DEFINE BUFFER bf-oe-ordl        FOR oe-ordl.
        DEFINE BUFFER bf-ttInputs       FOR ttInputs.

        FOR EACH bf-ttBin
            WHERE bf-ttBin.company EQ ipcCompany
              AND bf-ttBin.BOLID   EQ ipiBOLID
              AND bf-ttBin.itemID  EQ ipcItemID
            BREAK BY bf-ttBin.itemID:

            FIND FIRST bf-oe-boll NO-LOCK
                 WHERE ROWID(bf-oe-boll) EQ bf-ttBin.oebollRowID
                 NO-ERROR.
            IF NOT AVAILABLE bf-oe-boll THEN DO:
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = "Error finding BOL Line record"
                    .
                RETURN.
            END.

            FIND FIRST bf-oe-bolh NO-LOCK
                 WHERE bf-oe-bolh.b-no EQ bf-oe-boll.b-no NO-ERROR.
            IF NOT AVAILABLE bf-oe-bolh THEN DO:
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = "Error finding BOL Header record"
                    .
                RETURN.
            END.

            FIND FIRST bf-ttInputs
                 WHERE bf-ttInputs.company          EQ bf-ttBin.company
                   AND bf-ttInputs.BOLID            EQ bf-ttBin.BOLID
                   AND bf-ttInputs.itemID           EQ bf-ttBin.itemID
                   AND bf-ttInputs.inventoryStockID EQ bf-ttBin.tag
                 NO-ERROR.
            IF NOT AVAILABLE bf-ttInputs THEN DO:
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = "Error finding input record"
                    .
                RETURN.
            END.

            IF CAN-FIND(
                FIRST oe-boll
                WHERE oe-boll.company  EQ bf-oe-boll.company
                  AND oe-boll.b-no     EQ bf-oe-boll.b-no
                  AND oe-boll.ord-no   EQ bf-oe-boll.ord-no
                  AND oe-boll.i-no     EQ bf-oe-boll.i-no
                  AND oe-boll.line     EQ bf-oe-boll.line
                  AND oe-boll.rel-no   EQ bf-oe-boll.rel-no
                  AND oe-boll.b-ord-no EQ bf-oe-boll.b-ord-no
                  AND oe-boll.po-no    EQ bf-oe-boll.po-no
                  AND oe-boll.job-no   EQ bf-ttBin.jobID
                  AND oe-boll.job-no2  EQ bf-ttBin.jobID2
                  AND oe-boll.loc      EQ bf-ttBin.warehouseID
                  AND oe-boll.loc-bin  EQ bf-ttBin.locationID
                  AND oe-boll.tag      EQ bf-ttBin.tag
                  AND oe-boll.cust-no  EQ bf-ttBin.customerID
                  AND ROWID(oe-boll)   NE ROWID(bf-oe-boll)
                USE-INDEX b-no) THEN
                NEXT.
            
            /* fg-bin should not be found for the records that were sent without a tag */
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE ROWID(bf-fg-bin) EQ bf-ttBin.fgbinRowId
                NO-ERROR.
            IF NOT AVAILABLE bf-fg-bin THEN DO:         
                FIND CURRENT bf-oe-boll EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE bf-oe-boll THEN 
                    ASSIGN
                        bf-oe-boll.qty-case = bf-ttInputs.quantityPerUnit
                        bf-oe-boll.partial  = bf-ttInputs.quantityPartial
                        bf-oe-boll.cases    = bf-ttInputs.quantityOfUnits
                        bf-oe-boll.qty      = bf-ttInputs.quantityTotal
                        bf-oe-boll.p-c      = FALSE /* Always set as partial. The validtaion oe/oe-bolpc.p is going to correct this field */
                        bf-oe-boll.deleted  = NO
                        bf-oe-boll.posted   = NO
                        bf-oe-boll.printed  = NO
                        bf-oe-boll.cases    = IF bf-oe-boll.qty-case NE 0 THEN
                                                  TRUNC((bf-oe-boll.qty - bf-oe-boll.partial) / bf-oe-boll.qty-case, 0)
                                              ELSE
                                                  0
                        NO-ERROR.
            END.
            
            FOR FIRST bf-fg-bin NO-LOCK
                WHERE ROWID(bf-fg-bin) EQ bf-ttBin.fgbinRowId,
                FIRST bf-itemfg NO-LOCK
                WHERE bf-itemfg.company EQ bf-fg-bin.company
                  AND bf-itemfg.i-no    EQ bf-fg-bin.i-no:

               /* Update the existing oe-boll records if it's first of the item number
                  in ttBin records, else create new oe-boll record */
                IF FIRST-OF(bf-ttBin.itemID) THEN
                    FIND FIRST bf-create-oe-boll EXCLUSIVE-LOCK
                         WHERE ROWID(bf-create-oe-boll) EQ ROWID(bf-oe-boll) NO-ERROR.
                
                IF NOT AVAILABLE bf-create-oe-boll THEN
                    FIND FIRST bf-create-oe-boll EXCLUSIVE-LOCK
                         WHERE ROWID(bf-create-oe-boll) EQ ROWID(bf-oe-boll) 
                           AND bf-create-oe-boll.tag    EQ bf-ttBin.tag
                         NO-ERROR.
                
                IF NOT AVAILABLE bf-create-oe-boll THEN DO:
                    CREATE bf-create-oe-boll.
                    ASSIGN
                        bf-create-oe-boll.company = bf-oe-bolh.company
                        bf-create-oe-boll.loc     = bf-oe-bolh.loc
                        bf-create-oe-boll.bol-no  = bf-oe-bolh.bol-no
                        bf-create-oe-boll.ord-no  = bf-oe-bolh.ord-no
                        bf-create-oe-boll.po-no   = bf-oe-bolh.po-no
                        bf-create-oe-boll.r-no    = bf-oe-bolh.r-no
                        bf-create-oe-boll.b-no    = bf-oe-bolh.b-no
                        .

                    BUFFER-COPY bf-oe-boll EXCEPT rec_key TO bf-create-oe-boll.
                END.

                ASSIGN
                    bf-create-oe-boll.lot-no   = bf-oe-boll.lot-no
                    bf-create-oe-boll.job-no   = bf-fg-bin.job-no
                    bf-create-oe-boll.job-no2  = bf-fg-bin.job-no2
                    bf-create-oe-boll.loc      = bf-fg-bin.loc
                    bf-create-oe-boll.loc-bin  = bf-fg-bin.loc-bin
                    bf-create-oe-boll.tag      = bf-fg-bin.tag
                    bf-create-oe-boll.cust-no  = bf-fg-bin.cust-no
                    bf-create-oe-boll.p-c      = FALSE
                    bf-create-oe-boll.deleted  = NO
                    bf-create-oe-boll.posted   = NO
                    bf-create-oe-boll.printed  = NO
                    bf-create-oe-boll.qty-case = bf-ttInputs.quantityPerUnit
                    bf-create-oe-boll.partial  = bf-ttInputs.quantityPartial
                    bf-create-oe-boll.cases    = bf-ttInputs.quantityOfUnits
                    bf-create-oe-boll.qty      = bf-ttInputs.quantityTotal
                    bf-create-oe-boll.weight   = bf-create-oe-boll.qty / 100 * bf-itemfg.weight-100
                    .
                
                /* Important: Release the bf-create-oe-boll here. If not released this will 
                   update the same bf-create-oe-boll record for all the ttBin records */
                RELEASE bf-create-oe-boll.
            END.
            
            /* To catch multiple tags selected, need to examine all other lines */
            RUN oe/oe-bolpc.p (
                INPUT ROWID(bf-oe-boll), 
                INPUT "" /* Type. Send "ALL" to calculate for all line with same item */
                ) NO-ERROR.

            bf-ttBin.processed = TRUE.

        END.

        ASSIGN
            oplSuccess         = TRUE
            opcMessage         = "Success"
            .

        RELEASE bf-oerel.
        RELEASE bf-oe-boll.
        RELEASE bf-create-oe-boll.
        RELEASE bf-calc-qty-boll.
        RELEASE bf-oe-bolh.
        RELEASE bf-ttBin.
        RELEASE bf-fg-bin.
        RELEASE bf-itemfg.
        RELEASE bf-oe-ordl.
        RELEASE bf-ttInputs.
    END PROCEDURE.

    PROCEDURE ReCalculateFreightCost:
        /* Purpose: Copied from d-selbin.w to re-calculate the freight cost and pallet count
                    in oe-bolh and oe-boll records */
                    
        DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipiBOLID       AS INTEGER   NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess     AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.

        DEFINE VARIABLE dTotFreight AS DECIMAL NO-UNDO.

        DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
        DEFINE BUFFER bf-oe-boll FOR oe-boll.

        FIND FIRST bf-oe-bolh EXCLUSIVE-LOCK
             WHERE bf-oe-bolh.company EQ ipcCompany
               AND bf-oe-bolh.bol-no  EQ ipiBOLID
             NO-ERROR.
        IF NOT AVAILABLE bf-oe-bolh THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Error finding BOL Header record for updating freight cost"
                .
            RETURN.
        END.

        bf-oe-bolh.tot-pallets = 0.

        /* Obtain the total freight for all lines on BOL */
        FOR EACH bf-oe-boll
            WHERE bf-oe-boll.company EQ bf-oe-bolh.company
              AND bf-oe-boll.b-no    EQ bf-oe-bolh.b-no:

            RUN oe/pallcalc.p (
                INPUT  ROWID(bf-oe-boll), 
                OUTPUT bf-oe-boll.tot-pallets
                ).

            bf-oe-bolh.tot-pallets = bf-oe-bolh.tot-pallets + bf-oe-boll.tot-pallets.
        END.

        RUN oe/calcBolFrt.p (
            INPUT  ROWID(bf-oe-bolh),
            OUTPUT dTotFreight
            ).

        bf-oe-bolh.freight = dTotFreight.

        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .

        RELEASE bf-oe-bolh.
        RELEASE bf-oe-boll.
    END PROCEDURE.

    PROCEDURE UpdateBOLFreightCost:
        /* Purpose: Logic to apply the new freight cost to oe-bolh and oe-boll records */
        
        DEFINE INPUT  PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipiBOLID          AS INTEGER   NO-UNDO.
        DEFINE INPUT  PARAMETER ipdNewFreightCost AS DECIMAL   NO-UNDO.
        DEFINE INPUT  PARAMETER ipdOldFreightCost AS DECIMAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess        AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage        AS CHARACTER NO-UNDO.

        DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
        DEFINE BUFFER bf-oe-boll FOR oe-boll.
        
        IF ipdNewFreightCost LE 0 THEN
            RETURN.
            
        FIND FIRST bf-oe-bolh EXCLUSIVE-LOCK
             WHERE bf-oe-bolh.company EQ ipcCompany
               AND bf-oe-bolh.bol-no  EQ ipiBOLID
             NO-ERROR.
        IF NOT AVAILABLE bf-oe-bolh THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Error finding BOL Header record for updating freight cost"
                .
            RETURN.
        END.

        bf-oe-bolh.freight = ipdNewFreightCost.

        RUN oe/bolfrteq.p (
            BUFFER bf-oe-bolh,
            INPUT  ipdNewFreightCost,
            INPUT  ipdOldFreightCost
            ) NO-ERROR.

        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .

        RELEASE bf-oe-bolh.
        RELEASE bf-oe-boll.
    END PROCEDURE.    
    
    PROCEDURE PostBOL:
        DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipiBOLID    AS INTEGER   NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
        
        DEFINE VARIABLE hdOerelReCalc AS HANDLE    NO-UNDO.
        DEFINE VARIABLE dOut          AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE cTerm         AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lReportKey10  AS LOGICAL   NO-UNDO.
        
        DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
        DEFINE BUFFER bf-oe-boll FOR oe-boll.
        DEFINE BUFFER bf-cust    FOR cust.
        DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
        DEFINE BUFFER bf-oe-rel  FOR oe-rel.
        
        FIND FIRST bf-oe-bolh EXCLUSIVE-LOCK
             WHERE bf-oe-bolh.company EQ ipcCompany
               AND bf-oe-bolh.bol-no  EQ ipiBOLID
             NO-ERROR.
        IF NOT AVAILABLE bf-oe-bolh THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid BOL Number"
                .
            RETURN.
        END.

        IF bf-oe-bolh.posted THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "BOL Number " + STRING(bf-oe-bolh.bol-no) + " already posted"
                .
            RETURN.
        END.

        cTerm = STRING(YEAR(TODAY),"9999")
              + STRING(MONTH(TODAY),"99")
              + STRING(DAY(TODAY),"99")
              + STRING(TIME,"99999")
              + STRING(PROGRAM-NAME(1),"X(40)")
              + STRING(USERID("ASI"),"X(40)").

        FIND FIRST sys-ctrl-shipto NO-LOCK
             WHERE sys-ctrl-shipto.company      EQ bf-oe-bolh.company 
               AND sys-ctrl-shipto.name         EQ "BOLFMT" 
               AND sys-ctrl-shipto.cust-vend    EQ YES 
               AND sys-ctrl-shipto.cust-vend-no EQ bf-oe-bolh.cust-no 
               AND sys-ctrl-shipto.ship-id      EQ bf-oe-bolh.ship-id
             NO-ERROR.
        IF NOT AVAIL sys-ctrl-shipto THEN
            FIND FIRST sys-ctrl-shipto NO-LOCK
                 WHERE sys-ctrl-shipto.company      EQ bf-oe-bolh.company 
                   AND sys-ctrl-shipto.name         EQ "BOLFMT" 
                   AND sys-ctrl-shipto.cust-vend    EQ YES 
                   AND sys-ctrl-shipto.cust-vend-no EQ bf-oe-bolh.cust-no 
                   AND sys-ctrl-shipto.ship-id      EQ ''
                 NO-ERROR.

        ASSIGN
            lReportKey10        = bf-oe-bolh.printed
            bf-oe-bolh.printed  = YES
            bf-oe-bolh.prt-date = TODAY
            bf-oe-bolh.prt-time = TIME
            .

        IF NOT CAN-FIND(FIRST report 
                        WHERE report.term-id EQ cTerm 
                          AND report.rec-id  EQ RECID(bf-oe-bolh)) THEN DO:
            CREATE report.
            ASSIGN 
                report.term-id  = cTerm
                report.key-01   = bf-oe-bolh.cust-no
                report.key-02   = bf-oe-bolh.ship-id
                report.rec-id   = RECID(bf-oe-bolh)
                report.key-09   = STRING(bf-oe-bolh.printed,"REVISED/ORIGINAL")
                report.key-10   = STRING(lReportKey10)
                report.key-03   = IF AVAIL sys-ctrl-shipto AND  NOT sys-ctrl-shipto.log-fld THEN 
                                      "C" /*commercial invoice only*/
                                  ELSE IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN 
                                      "B" /*commercial invoice and bol both*/
                                  ELSE 
                                      "N" /*BOL only*/ 
                report.key-04   = IF AVAIL sys-ctrl-shipto THEN 
                                      sys-ctrl-shipto.char-fld 
                                  ELSE 
                                      "".
        END.

        IF bf-oe-bolh.stat EQ "H" THEN DO:  
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "BOL " + STRING(bf-oe-bolh.bol-no) + " is on HOLD status, cannot post"
                .
            RETURN.
        END.

        FOR EACH bf-oe-boll NO-LOCK
            WHERE bf-oe-boll.company EQ bf-oe-bolh.company
              AND bf-oe-boll.bol-no  EQ bf-oe-bolh.bol-no: 
            FIND FIRST bf-cust NO-LOCK
                 WHERE bf-cust.company EQ bf-oe-bolh.company
                   AND bf-cust.cust-no EQ bf-oe-bolh.cust-no 
                 NO-ERROR. 
            IF AVAILABLE bf-cust AND 
               bf-cust.ACTIVE EQ "X" AND 
               bf-oe-bolh.ship-id EQ bf-oe-boll.loc THEN DO:
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = "BOL " + STRING(bf-oe-bolh.bol-no) + ". Cannot transfer to the same location"
                    .
                RETURN.
            END.
        END.

        RUN sbo/oerel-recalc-act.p PERSISTENT SET hdOerelReCalc NO-ERROR.
  
        FOR EACH bf-oe-boll NO-LOCK 
            WHERE bf-oe-boll.b-no EQ bf-oe-bolh.b-no,
            EACH bf-oe-ordl NO-LOCK
            WHERE bf-oe-ordl.company EQ bf-oe-boll.company
               AND bf-oe-ordl.ord-no EQ bf-oe-boll.ord-no
                AND bf-oe-ordl.line  EQ bf-oe-boll.line:
            FOR EACH bf-oe-rel 
                WHERE bf-oe-rel.company EQ bf-oe-ordl.company
                  AND bf-oe-rel.ord-no  EQ bf-oe-ordl.ord-no
                  AND bf-oe-rel.i-no    EQ bf-oe-ordl.i-no
                  AND bf-oe-rel.line    EQ bf-oe-ordl.line
                  AND bf-oe-rel.stat    EQ "P"
                  AND bf-oe-rel.link-no GT 0 
                  AND bf-oe-rel.rel-no  GT 0:
                IF VALID-HANDLE(hdOerelReCalc) THEN 
                    RUN recalc-act-qty IN hdOerelReCalc (
                        INPUT  ROWID(bf-oe-rel), 
                        OUTPUT dOut
                        ) NO-ERROR.
            END.
        END.

        FOR EACH bf-oe-boll NO-LOCK 
            WHERE bf-oe-boll.b-no EQ bf-oe-bolh.b-no:
    
            RUN oe/bol-pre-post.p (
                INPUT ROWID(bf-oe-boll), 
                INPUT cTerm,    /* Report ID */ 
                INPUT NO        /* Show msg */
                ) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = "Error while pre posting BOL"
                    .
                RETURN.
            END.
        END.
            
        RUN oe/oe-bolp3.p (
            INPUT cTerm,
            INPUT TODAY
            ) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Error while posting BOL"
                .
            RETURN.
        END.

        /* Close transfer order here */
        RUN oe/closchk.p (
            INPUT 0
            ) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Error while BOL Close check"
                .
            RETURN.                   
        END.

        RELEASE bf-oe-bolh.

        FIND FIRST bf-oe-bolh EXCLUSIVE-LOCK
             WHERE bf-oe-bolh.company EQ ipcCompany
               AND bf-oe-bolh.bol-no  EQ ipiBOLID
             NO-ERROR.
        IF AVAILABLE bf-oe-bolh THEN DO:                
            FOR EACH bf-oe-boll NO-LOCK 
                WHERE bf-oe-boll.b-no EQ bf-oe-bolh.b-no:
                
                FIND FIRST bf-oe-ordl NO-LOCK
                     WHERE bf-oe-ordl.company EQ bf-oe-boll.company
                       AND bf-oe-ordl.ord-no  EQ bf-oe-boll.ord-no
                       AND bf-oe-ordl.line    EQ bf-oe-boll.line 
                     NO-ERROR.
                IF AVAILABLE bf-oe-ordl THEN DO:               
                    RUN oe/cleanrel.p (
                        INPUT ROWID(bf-oe-ordl)
                        ) NO-ERROR.                       
                    IF ERROR-STATUS:ERROR THEN DO:
                        ASSIGN
                            oplSuccess = FALSE
                            opcMessage = "Error while cleaning release"
                            .
                        RETURN.         
                    END.
                END.
            END.
        END.

        FOR EACH w-ord:
            RUN oe/close.p (
                INPUT w-ord.rec-id, 
                INPUT YES
                )NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = "Error while closing BOL - "
                    .
                RETURN.                    
            END.  
        END.

        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .
            
        IF VALID-HANDLE(hdOerelReCalc) THEN
            DELETE OBJECT hdOerelReCalc.
        
        RELEASE bf-oe-bolh.
        RELEASE bf-oe-boll.
        RELEASE bf-cust. 
        RELEASE bf-oe-ordl.
        RELEASE bf-oe-rel.
    END PROCEDURE.
    
    THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdJSONProcs).
    DELETE PROCEDURE hdJSONProcs.
    
    THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdCommonProcs).
    DELETE PROCEDURE hdCommonProcs.
    
