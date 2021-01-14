/*------------------------------------------------------------------------
    File        : api\inbound\CreateInventoryCount.p
    Purpose     : Creates and posts Inventory Count Transaction

    Syntax      :

    Description : Creates and posts Inventory Count Transaction

    Author(s)   : Porandla Mithun
    Created     : Wed Dec 04 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany            AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcWarehouseID        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcLocationID         AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcCustID             AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiPOID               AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiPOLine             AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcJobID             AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiJobID2            AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcItemID             AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcItemType           AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcInventoryStockID   AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiQuantity           AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiQuantityPerSubUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiQuantityPartial    AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER iplPost               AS LOGICAL   NO-UNDO.
    DEFINE INPUT        PARAMETER iplZeroOutCount       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opisequenceID         AS INT64     NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess            AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage            AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cTransactionTypeCount AS CHARACTER NO-UNDO INITIAL "C".
    DEFINE VARIABLE cDBNameASI            AS CHARACTER NO-UNDO INITIAL "ASI".
    DEFINE VARIABLE cJobID                AS CHARACTER NO-UNDO.

    /* This temp-table definition is required as is, and is used in existing
       posting program fg-cpost.i */
    DEFINE TEMP-TABLE w-fg-rctd NO-UNDO
        LIKE fg-rctd.

    /* The below include files consist of multiple temp-table definitions
       which are being used in existing posting programs */

    {sys/inc/VAR.i NEW SHARED}
    {oe/invwork.i NEW}
    {fg/fullset.i NEW}

    ASSIGN
        cocode = ipcCompany
        locode = ipcWarehouseID
        cJobID = iopcJobID
        .

    {Inventory/ttInventory.i "NEW SHARED"}

    DEFINE VARIABLE hdInventoryProcs AS HANDLE NO-UNDO.
    DEFINE VARIABLE hdJobProcs       AS HANDLE NO-UNDO.
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.

    /* Formats Job number */
    iopcJobID = DYNAMIC-FUNCTION (
               "fAddSpacesToString" IN hdJobProcs , iopcJobID , 6 , TRUE
               ). 
    
    /* Input validation */
    RUN pValidateInputs (
        INPUT        ipcCompany,
        INPUT        ipcWarehouseID,
        INPUT        ipcLocationID,
        INPUT        ipcCustID,
        INPUT-OUTPUT ipiPoID,
        INPUT-OUTPUT ipiPoLine,
        INPUT-OUTPUT iopcJobID,
        INPUT-OUTPUT iopiJobID2,
        INPUT        ipcItemID,
        INPUT        ipcItemType,
        INPUT        ipcInventoryStockID,
        INPUT        ipiQuantity,
        INPUT        ipiQuantityPerSubUnit,
        INPUT        ipiQuantityPartial,
        INPUT        iplPost,
        OUTPUT       oplSuccess,
        OUTPUT       opcMessage
        ) NO-ERROR.

    /* Creates fg-rctd of rita-code "C" and posts */
    IF oplSuccess AND NOT ERROR-STATUS:ERROR THEN
        RUN pCreateAndPostInventoryCount (
            INPUT  ipcCompany,
            INPUT  ipcWarehouseID,
            INPUT  ipcLocationID,
            INPUT  ipcCustID,
            INPUT  ipiPoID,
            INPUT  ipiPoLine,
            INPUT  iopcJobID,
            INPUT  iopiJobID2,
            INPUT  ipcItemID,
            INPUT  ipcItemType,
            INPUT  ipcInventoryStockID,
            INPUT  ipiQuantity,
            INPUT  ipiQuantityPerSubUnit,
            INPUT  ipiQuantityPartial,
            INPUT  iplPost,
            OUTPUT opiSequenceID,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.
  
    IF ERROR-STATUS:ERROR THEN
        opcMessage = ERROR-STATUS:GET-MESSAGE(1).
        
    iopcJobID = cJobID.
    
    THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdInventoryProcs).
    DELETE PROCEDURE hdInventoryProcs.
    DELETE PROCEDURE hdJobProcs.

    PROCEDURE pValidateInputs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to validate inputs
     Notes:
    ------------------------------------------------------------------------------*/     
        DEFINE INPUT        PARAMETER ipcCompany            AS CHARACTER NO-UNDO.
        DEFINE INPUT        PARAMETER ipcWarehouseID        AS CHARACTER NO-UNDO.
        DEFINE INPUT        PARAMETER ipcLocationID         AS CHARACTER NO-UNDO.
        DEFINE INPUT        PARAMETER ipcCustID             AS CHARACTER NO-UNDO.
        DEFINE INPUT-OUTPUT PARAMETER iopiPOID              AS INTEGER   NO-UNDO.
        DEFINE INPUT-OUTPUT PARAMETER iopiPOLine            AS INTEGER   NO-UNDO.
        DEFINE INPUT-OUTPUT PARAMETER iopcJobID             AS CHARACTER NO-UNDO.
        DEFINE INPUT-OUTPUT PARAMETER iopiJobID2            AS INTEGER   NO-UNDO.
        DEFINE INPUT        PARAMETER ipcItemID             AS CHARACTER NO-UNDO.
        DEFINE INPUT        PARAMETER ipcItemType           AS CHARACTER NO-UNDO.
        DEFINE INPUT        PARAMETER ipcInventoryStockID   AS CHARACTER NO-UNDO.
        DEFINE INPUT        PARAMETER ipiQuantity           AS INTEGER   NO-UNDO.
        DEFINE INPUT        PARAMETER ipiQuantityPerSubUnit AS INTEGER   NO-UNDO.
        DEFINE INPUT        PARAMETER ipiQuantityPartial    AS INTEGER   NO-UNDO.
        DEFINE INPUT        PARAMETER iplpost               AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT       PARAMETER oplSuccess            AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT       PARAMETER opcMessage            AS CHARACTER NO-UNDO.

        DEFINE VARIABLE lActive       AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cItemTypeList AS CHARACTER NO-UNDO INITIAL "FG,RM".
        DEFINE VARIABLE lItemType     AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cItemTypeFG   AS CHARACTER NO-UNDO INITIAL "FG".

        /* Company validation */
        IF ipcCompany EQ "" THEN DO:
            ASSIGN
                opcMessage = "Empty Company"
                oplSuccess = NO
                .
            RETURN.
        END.

        IF NOT CAN-FIND(FIRST company NO-LOCK
                        WHERE company.company EQ ipcCompany) THEN DO:
            ASSIGN
                opcMessage = "Invalid Company"
                oplSuccess = NO
                .
            RETURN.
        END.

        /* Warehouse validation */
        IF ipcWarehouseID EQ "" THEN DO:
            ASSIGN
                opcMessage = "Empty wareHouseID"
                oplSuccess = NO
                .
            RETURN.
        END.

        RUN ValidateLoc IN hdInventoryProcs (
            INPUT  ipcCompany,
            INPUT  ipcWarehouseID,
            OUTPUT lActive
            ).

        IF NOT lActive THEN DO:
            ASSIGN
                opcMessage = "Invalid WareHouseID"
                oplSuccess = NO
                .
            RETURN.
        END.

        /* Location validation */
        IF ipcLocationID EQ "" THEN DO:
            ASSIGN
                opcMessage = "Empty locationID"
                oplSuccess = NO
                .
            RETURN.
        END.

        RUN ValidateBin IN hdInventoryProcs (
            INPUT  ipcCompany,
            INPUT  ipcWarehouseID,
            INPUT  ipcLocationID,
            OUTPUT lActive
            ) NO-ERROR.

        IF NOT lActive THEN DO:
            ASSIGN
                opcMessage = "Invalid LocationID"
                oplSuccess = NO
                .
            RETURN.
        END.

        IF ipcItemType EQ "" THEN
            ipcItemType = gcItemTypeFG.

        IF LOOKUP(ipcItemType, cItemTypeList) EQ 0 THEN DO:
            ASSIGN
                opcMessage = "Invalid ItemType"
                oplSuccess = NO
                .
            RETURN.
        END.

        RUN pValidateItemAndTag (
            INPUT  ipcCompany,
            INPUT  ipcItemID,
            INPUT  ipcInventoryStockID,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.

        IF ERROR-STATUS:ERROR THEN
            opcMessage = ERROR-STATUS:GET-MESSAGE(1).

        IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
            RETURN.
        
        IF iopcJobID NE "" THEN DO:
            RUN pValidateJob (
                INPUT  ipcCompany,
                INPUT  iopcJobID,
                INPUT  iopiJobID2,
                INPUT  ipcItemID,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ) NO-ERROR.

            IF ERROR-STATUS:ERROR THEN
                opcMessage = ERROR-STATUS:GET-MESSAGE(1).

            IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
                RETURN.
        END.

        IF iopiPOID NE 0 THEN DO:
            RUN pValidatePOAndPOLine (
                INPUT  ipcCompany,
                INPUT  iopiPOID,
                INPUT  iopiPOLine,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ) NO-ERROR.

            IF ERROR-STATUS:ERROR THEN
                opcMessage = ERROR-STATUS:GET-MESSAGE(1).

            IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
                RETURN.
        END.

        IF ipcCustID NE "" THEN DO:
            RUN ValidateCust IN hdInventoryProcs (
                INPUT  ipcCompany,
                INPUT  ipcCustID,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ) NO-ERROR.

            IF ERROR-STATUS:ERROR THEN
                opcMessage = ERROR-STATUS:GET-MESSAGE(1).

            IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
                RETURN.
        END.
        
        IF ipiQuantity LT 0 THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Quantity cannot be less than zero"
                .
            RETURN.        
        END.

        IF ipiQuantityPerSubUnit LT 0 THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "QuantityPerSubUnit cannot be less than zero"
                .
            RETURN.        
        END.

        IF ipiQuantityPartial LT 0 THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "QuantityPartial cannot be less than zero"
                .
            RETURN.        
        END.
        
        lItemType = ipcItemType NE cItemTypeFG.
        
        FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company   EQ ipcCompany
               AND loadtag.item-type EQ lItemType
               AND loadtag.tag-no    EQ ipcInventoryStockID
             NO-ERROR.
        IF AVAILABLE loadtag THEN
            ASSIGN
                iopiPOID =   IF iopiPOID EQ 0 THEN
                                 loadtag.po-no
                             ELSE
                                 iopiPOID
                iopiPOLine = IF ipiPOLine EQ 0 THEN
                                 loadtag.line
                             ELSE
                                 ipiPOLine
                iopcJobID =  IF iopcJobID EQ "" THEN
                                 loadtag.job-no
                             ELSE
                                 iopcJobID
                iopiJobID2 = IF iopiJobID2 EQ 0 THEN
                                 loadtag.job-no2
                             ELSE
                                 iopiJobID2
                cJobID     = IF cJobID NE "" THEN
                                cJobID
                             ELSE
                                 iopcJobID
                .
        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .
            
        
    END PROCEDURE.

    PROCEDURE pValidateItemAndTag PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to validate itemID and inventoryStockID
     Notes:
    ------------------------------------------------------------------------------*/     
        DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipcItemID           AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

        DEFINE VARIABLE lValidTag AS LOGICAL NO-UNDO.

        DEFINE BUFFER bf-itemfg  FOR itemfg.
        DEFINE BUFFER bf-fg-rctd FOR fg-rctd.

        IF ipcItemID EQ "" THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Empty ItemID"
                .
            RETURN.
        END.

        IF ipcInventoryStockID EQ "" THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Empty InventoryStockID"
                .
            RETURN.
        END.

        IF NOT CAN-FIND(FIRST bf-itemfg
                        WHERE bf-itemfg.company EQ ipcCompany
                          AND bf-itemfg.i-no    EQ ipcItemID) THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid ItemID"
                .
            RETURN.
        END.

        RUN ValidateLoadTag IN hdInventoryProcs (
            INPUT  ipcCompany,
            INPUT  ipcItemType,
            INPUT  ipcInventoryStockID,
            OUTPUT lValidTag,
            OUTPUT opcMessage
            ) NO-ERROR.

        IF NOT lValidTag THEN
            RETURN.

        RELEASE bf-itemfg.
        RELEASE bf-fg-rctd.

        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .
    END PROCEDURE.

    PROCEDURE pValidatePOAndPOLine PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to validate PO and PO Line
     Notes:
    ------------------------------------------------------------------------------*/     
        DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipiPOID    AS INTEGER   NO-UNDO.
        DEFINE INPUT  PARAMETER ipiPOLine  AS INTEGER   NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

        RUN ValidatePO IN hdInventoryProcs (
            INPUT  ipcCompany,
            INPUT  ipiPOID,
            OUTPUT oplSuccess
            ) NO-ERROR.
        IF NOT oplSuccess THEN
            RETURN.

        RUN ValidatePOLine IN hdInventoryProcs (
            INPUT  ipcCompany,
            INPUT  ipiPOID,
            INPUT  ipiPOLine,
            OUTPUT oplSuccess
            ) NO-ERROR.
        IF NOT oplSuccess THEN
            RETURN.

        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .
    END PROCEDURE.

    PROCEDURE pValidateJob PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to validate job
     Notes:
    ------------------------------------------------------------------------------*/    
        DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipcJobID   AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipiJobID2  AS INTEGER   NO-UNDO.
        DEFINE INPUT  PARAMETER ipcItemID  AS CHARACTER NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

        DEFINE BUFFER bf-job-hdr FOR job-hdr.

        FIND FIRST bf-job-hdr NO-LOCK
             WHERE bf-job-hdr.company EQ ipcCompany
               AND bf-job-hdr.job-no  EQ ipcJobID
               AND bf-job-hdr.job-no2 EQ ipiJobID2
               AND bf-job-hdr.i-no    EQ ipcItemID
             NO-ERROR.
        IF NOT AVAILABLE bf-job-hdr THEN DO:
            FIND FIRST bf-job-hdr NO-LOCK
                 WHERE bf-job-hdr.company EQ ipcCompany
                   AND bf-job-hdr.job-no  EQ ipcJobID
                 NO-ERROR.
            IF AVAILABLE bf-job-hdr THEN
                FIND FIRST reftable NO-LOCK
                     WHERE reftable.reftable EQ "jc/jc-calc.p"
                       AND reftable.company  EQ ipcCompany
                       AND reftable.loc      EQ ""
                       AND reftable.code     EQ STRING(bf-job-hdr.job,"999999999")
                       AND reftable.code2    EQ ipcItemID
                     NO-ERROR.
            IF NOT AVAILABLE reftable THEN DO:
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = "Item# - " + ipcItemID + " is not on Job# - "
                               + ipcJobID + "-" + STRING(ipiJobID2)
                    .
                RETURN.
            END.
        END.

        IF NOT AVAILABLE bf-job-hdr THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid Job#"
                .
            RETURN.
        END.

        RELEASE bf-job-hdr.

        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .
    END PROCEDURE.

    PROCEDURE pCreateAndPostInventoryCount PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Creates new fg-rctd record of rita-code "C"
     Notes:
    ------------------------------------------------------------------------------*/
        DEFINE INPUT  PARAMETER ipcCompany            AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipcWarehouseID        AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipcLocationID         AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipcCustID             AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipiPOID               AS INTEGER   NO-UNDO.
        DEFINE INPUT  PARAMETER ipiPOLine             AS INTEGER   NO-UNDO.
        DEFINE INPUT  PARAMETER ipcJobID              AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipiJobID2             AS INTEGER   NO-UNDO.
        DEFINE INPUT  PARAMETER ipcItemID             AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipcItemType           AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipcInventoryStockID   AS CHARACTER NO-UNDO.
        DEFINE INPUT  PARAMETER ipiQuantity           AS INTEGER   NO-UNDO.
        DEFINE INPUT  PARAMETER ipiQuantityPerSubUnit AS INTEGER   NO-UNDO.
        DEFINE INPUT  PARAMETER ipiQuantityPartial    AS INTEGER   NO-UNDO.
        DEFINE INPUT  PARAMETER iplpost               AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opisequenceID         AS INT64     NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess            AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage            AS CHARACTER NO-UNDO.

        DEFINE VARIABLE iRNo AS INT64 NO-UNDO.
        DEFINE VARIABLE rifgrctd AS ROWID NO-UNDO.
        
        DEFINE BUFFER bf-fg-rctd  FOR fg-rctd.
        DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
        DEFINE BUFFER bf-itemfg   FOR itemfg.
        DEFINE BUFFER bf-fg-bin   FOR fg-bin.
        
        oplSuccess = TRUE.
        
        /* Retrieving last record of fg-rctd table - USE-INDEX fg-rctd is required to enforce the query to fetch latest r-no*/
        FIND LAST bf-fg-rctd NO-LOCK
             USE-INDEX fg-rctd NO-ERROR.
        IF AVAILABLE bf-fg-rctd AND bf-fg-rctd.r-no GT iRNo THEN
            iRNo = bf-fg-rctd.r-no.

        /* Retrieving last record of fg-rcpth table - USE-INDEX fg-rctd is required to enforce the query to fetch latest r-no*/
        /* In some cases r-no from fg-rcpth is greater than r-no from fg-rctd - so this additional find and the logic is required*/
        FIND LAST bf-fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAILABLE bf-fg-rcpth AND bf-fg-rcpth.r-no GT iRNo THEN
            iRNo = bf-fg-rcpth.r-no.

        CREATE bf-fg-rctd.
        ASSIGN
            bf-fg-rctd.r-no         = iRNo + 1
            bf-fg-rctd.rct-date     = TODAY
            bf-fg-rctd.trans-time   = TIME
            bf-fg-rctd.company      = ipcCompany
            bf-fg-rctd.rita-code    = cTransactionTypeCount
            bf-fg-rctd.qty          = ipiQuantity
            bf-fg-rctd.i-no         = ipcItemID
            bf-fg-rctd.job-no       = ipcJobID
            bf-fg-rctd.job-no2      = ipiJobID2
            bf-fg-rctd.s-num        = 0                      /* Assign sheet# to 0. Existing logic from b-phys.w */
            bf-fg-rctd.cases        = ipiQuantity
            bf-fg-rctd.qty-case     = ipiQuantityPerSubUnit
            bf-fg-rctd.partial      = ipiQuantityPartial
            bf-fg-rctd.t-qty        = (ipiQuantity * ipiQuantityPerSubUnit) + ipiQuantityPartial
            bf-fg-rctd.units-pallet = 1
            bf-fg-rctd.cases-unit   = 1
            bf-fg-rctd.loc          = ipcWarehouseID
            bf-fg-rctd.loc-bin      = ipcLocationID
            bf-fg-rctd.tag          = ipcInventoryStockID
            bf-fg-rctd.cust-no      = ipcCustID
            bf-fg-rctd.created-by   = USERID(cDBNameASI)
            bf-fg-rctd.updated-by   = USERID(cDBNameASI)
            bf-fg-rctd.po-no        = TRIM(STRING(ipiPOID,">>>>>>>>>>"))
            bf-fg-rctd.po-line      = ipiPOLine
            opisequenceID           = bf-fg-rctd.r-no
            .
        
        rifgrctd = ROWID(bf-fg-rctd).

        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company = ipcCompany
               AND bf-itemfg.i-no    = ipcItemID
             NO-ERROR.
        IF AVAILABLE bf-itemfg THEN
            ASSIGN
                bf-fg-rctd.i-name     = bf-itemfg.i-name
                bf-fg-rctd.pur-uom    = bf-itemfg.prod-uom
                bf-fg-rctd.cost-uom   = bf-itemfg.prod-uom
                .

        FIND FIRST bf-fg-bin
             WHERE bf-fg-bin.company EQ bf-fg-rctd.company
               AND bf-fg-bin.i-no    EQ bf-fg-rctd.i-no
               AND bf-fg-bin.job-no  EQ bf-fg-rctd.job-no
               AND bf-fg-bin.job-no2 EQ bf-fg-rctd.job-no2
               AND bf-fg-bin.loc     EQ bf-fg-rctd.loc
               AND bf-fg-bin.loc-bin EQ bf-fg-rctd.loc-bin
               AND bf-fg-bin.tag     EQ bf-fg-rctd.tag
               AND bf-fg-bin.cust-no EQ bf-fg-rctd.cust-no
             NO-LOCK NO-ERROR.
        IF AVAILABLE bf-fg-bin THEN
            ASSIGN
               bf-fg-rctd.ext-cost = bf-fg-rctd.t-qty / (IF bf-fg-bin.pur-uom EQ "M" THEN 1000 ELSE 1) * bf-fg-bin.std-tot-cost
               bf-fg-rctd.cost     = bf-fg-rctd.ext-cost / bf-fg-rctd.t-qty
               bf-fg-rctd.cost-uom = bf-fg-bin.pur-uom.
        ELSE
            RUN fg/fgrctdcst.p (
                INPUT  ROWID(bf-fg-rctd),
                INPUT  bf-fg-rctd.t-qty,
                OUTPUT bf-fg-rctd.cost,
                OUTPUT bf-fg-rctd.cost-uom,
                OUTPUT bf-fg-rctd.ext-cost
                ).

        /* Pur-uom blank was causing miscalculation during cost recalc ( Existing comment from b-phys.w) */
        IF bf-fg-rctd.pur-uom = "" THEN
            bf-fg-rctd.pur-uom = bf-fg-rctd.cost-uom.

        IF bf-fg-rctd.ext-cost EQ ? THEN
            bf-fg-rctd.ext-cost = 0.

        IF bf-fg-rctd.cost EQ ? THEN
            bf-fg-rctd.cost = 0.

        RELEASE bf-fg-rctd.
        RELEASE bf-fg-rcpth.
        RELEASE bf-itemfg.
        RELEASE bf-fg-bin.
        
        IF iplZeroOutCount THEN
            RUN pZeroOutCount (
                INPUT  rifgrctd,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ) NO-ERROR.
        
        IF NOT oplSuccess THEN
            RETURN.
                            
        IF iplPost THEN
            RUN pPostCount (
                INPUT  opiSequenceID,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ) NO-ERROR.

    END PROCEDURE.

    PROCEDURE pPostCount PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to Post Count transaction
     Notes:
    ------------------------------------------------------------------------------*/        
        DEFINE INPUT  PARAMETER ipiSequenceID AS INT64     NO-UNDO.
        DEFINE OUTPUT PARAMETER oplSuccess    AS LOGICAL   NO-UNDO.
        DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.

        DEFINE VARIABLE lCreatGlAccount AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE iRNo            AS INT64     NO-UNDO.
        DEFINE VARIABLE iAdjQty         AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iPeriod         AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iTrnNum         AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lRecFound       AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cRtnChar        AS CHARACTER NO-UNDO.

        /* The below variable are required to be defined with the below names,
           as these variables are being used in existing posting program fg-cpost.i */
        DEFINE VARIABLE begin_userid AS CHARACTER NO-UNDO.
        DEFINE VARIABLE end_userid   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE v-q-adj-ytd  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE v-qty-onh    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE v-post-date  AS DATE      NO-UNDO INITIAL TODAY.

        /* The below buffers are required to be defined with below names,
           as theser buffers are being used in existing posting program fg-cpost.i */
        DEFINE BUFFER b-fg-rctd   FOR fg-rctd.
        DEFINE BUFFER b-itemfg    FOR itemfg.
        DEFINE BUFFER b-fg-bin    FOR fg-bin.
        DEFINE BUFFER b2-fg-bin   FOR fg-bin.

        DEFINE BUFFER bf-fg-rctd  FOR fg-rctd.
        DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
        DEFINE BUFFER bf-itemfg   FOR itemfg.
        DEFINE BUFFER bf-period   FOR period.

        EMPTY TEMP-TABLE w-fg-rctd.

        /* The below variables need to be assigned with the current userid,
           as these are validated while posting in fg-cpost.i */
        ASSIGN
            begin_userid = USERID(cDBNameASI)
            end_userid   = USERID(cDBNameASI)
            .

        FIND FIRST fg-rctd NO-LOCK
             WHERE fg-rctd.r-no EQ ipiSequenceID
             NO-ERROR.
        IF NOT AVAILABLE fg-rctd THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Error creating fg-rctd record"
                .
            RETURN.
        END.

        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ cocode
               AND bf-itemfg.i-no    EQ fg-rctd.i-no
               AND bf-itemfg.isaset
               AND bf-itemfg.alloc
             NO-ERROR.
        IF AVAILABLE bf-itemfg THEN DO:
            /* Code to create temp-table tt-fg-set records for items of set type */
            RUN fg/fullset.p (
                INPUT ROWID(bf-itemfg)
                ) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = ERROR-STATUS:GET-MESSAGE(1)
                    .
                RETURN.
            END.
            
            /* For each of the tt-fg-set records, create temp-table w-fg-rctd record,
               which in turn will create fg-rctd records */
            FOR EACH tt-fg-set,
                FIRST b-itemfg
                WHERE b-itemfg.company EQ cocode
                  AND b-itemfg.i-no    EQ tt-fg-set.part-no
                NO-LOCK:

                iRNo = 1.

                /* Retrieving last record of fg-rctd table - USE-INDEX fg-rctd is required to enforce the query to fetch latest r-no*/
                FIND LAST bf-fg-rctd NO-LOCK
                     USE-INDEX fg-rctd NO-ERROR.
                IF AVAILABLE bf-fg-rctd AND bf-fg-rctd.r-no GT iRNo THEN
                    iRNo = bf-fg-rctd.r-no.

                /* Retrieving last record of fg-rcpth table - USE-INDEX fg-rctd is required to enforce the query to fetch latest r-no*/
                /* In some cases r-no from fg-rcpth is greater than r-no from fg-rctd - so this additional find and the logic is required*/
                FIND LAST bf-fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
                IF AVAILABLE bf-fg-rcpth AND bf-fg-rcpth.r-no GT iRNo THEN
                    iRNo = bf-fg-rcpth.r-no.

                CREATE w-fg-rctd.
                BUFFER-COPY fg-rctd TO w-fg-rctd
                ASSIGN
                    w-fg-rctd.i-no   = b-itemfg.i-no
                    w-fg-rctd.i-name = b-itemfg.i-name
                    w-fg-rctd.r-no   = iRNo
                    .

                FIND FIRST fg-bin
                    WHERE fg-bin.company EQ cocode
                      AND fg-bin.i-no    EQ fg-rctd.i-no
                      AND fg-bin.loc     EQ fg-rctd.loc
                      AND fg-bin.loc-bin EQ fg-rctd.loc-bin
                      AND fg-bin.tag     EQ fg-rctd.tag
                      AND fg-bin.job-no  EQ fg-rctd.job-no
                      AND fg-bin.job-no2 EQ fg-rctd.job-no2
                      AND fg-bin.cust-no EQ fg-rctd.cust-no
                    USE-INDEX co-ino NO-ERROR.
                iAdjQty = (IF AVAILABLE fg-bin THEN fg-bin.qty ELSE 0) * tt-fg-set.part-qty-dec.

                FIND FIRST fg-bin
                    WHERE fg-bin.company EQ cocode
                      AND fg-bin.i-no    EQ b-itemfg.i-no
                      AND fg-bin.loc     EQ fg-rctd.loc
                      AND fg-bin.loc-bin EQ fg-rctd.loc-bin
                      AND fg-bin.tag     EQ fg-rctd.tag
                      AND fg-bin.job-no  EQ fg-rctd.job-no
                      AND fg-bin.job-no2 EQ fg-rctd.job-no2
                      AND fg-bin.cust-no EQ fg-rctd.cust-no
                    USE-INDEX co-ino NO-ERROR.
                iAdjQty = (IF AVAILABLE fg-bin THEN fg-bin.qty ELSE 0) - iAdjQty.

                IF iAdjQty LT 0 THEN
                    iAdjQty = 0.

                w-fg-rctd.t-qty = (fg-rctd.t-qty * tt-fg-set.part-qty-dec) + iAdjQty.
            END.
        END.
        
        /* Call to create and post fg-rctd records created for set type items */
        {fg/fg-cpost.i w-}

        /* Call to post fg-rctd record created */
        {fg/fg-cpost.i}

        /* Code to fetch sys-ctrl configuration "AdjustGL" log field */
        RUN sys/ref/nk1look.p (
            INPUT  cocode,     /* Company       */
            INPUT  "AdjustGL", /* Sys-Ctrl Name */
            INPUT  "L",        /* Logical       */
            INPUT  NO,         /* Check by cust */
            INPUT  YES,        /* Use Cust      */
            INPUT  "",         /* Customer      */
            INPUT  "",         /* Ship-to       */
            OUTPUT cRtnChar,
            OUTPUT lRecFound
            ).
        IF lRecFound THEN
            lCreatGlAccount = LOGICAL(cRtnChar) NO-ERROR.

        /* Code to fetch the period */
        FIND FIRST bf-period NO-LOCK
             WHERE bf-period.company EQ cocode
               AND bf-period.pst     LE TODAY
               AND bf-period.pend    GE TODAY
            NO-ERROR.
        IF NOT AVAILABLE bf-period THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Unable to post tag " + fg-rctd.tag + "."
                           + " No Defined Period Exists for " + STRING(TODAY)
                .
            RETURN.
        END.

        iPeriod = bf-period.pnum.

        /* Creates GL records */
        IF lCreatGlAccount AND iPeriod GT 0 THEN DO:
            REPEAT:
                FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                     WHERE gl-ctrl.company EQ cocode
                     NO-ERROR NO-WAIT.
                IF AVAILABLE gl-ctrl THEN DO:
                    ASSIGN
                        iTrnNum       = gl-ctrl.trnum + 1
                        gl-ctrl.trnum = iTrnNum
                        .
                    FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.
                    LEAVE.
                END.
            END.

            FOR EACH work-job
               BREAK BY work-job.actnum:
               RUN spCreateGLHist(cocode,
                                  work-job.actnum,
                                  "OEINV",
                                  (IF work-job.fg THEN "ORDER ENTRY INVOICE FG"
                                                  ELSE "ORDER ENTRY INVOICE COGS"),
                                  TODAY,
                                  (IF work-job.fg THEN - work-job.amt
                                                  ELSE work-job.amt),
                                  iTrnNum,
                                  iPeriod,
                                  "A",
                                  TODAY,
                                  "",
                                  "FG").
                
            END.
        END.

        RELEASE bf-fg-rctd.
        RELEASE bf-fg-rcpth.
        RELEASE bf-itemfg.
        RELEASE bf-period.

        RELEASE b-fg-rctd.
        RELEASE b-itemfg.
        RELEASE b-fg-bin.
        RELEASE b2-fg-bin.

        ASSIGN
            oplSuccess = TRUE
            opcMessage = "Success"
            .
    END PROCEDURE.
    
PROCEDURE pZeroOutCount PRIVATE :
    /*------------------------------------------------------------------------------
      Purpose:   Count becomes zero if current loc/bin/custno is different 
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprifgrctd AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
     
    DEFINE VARIABLE iRNo AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-fg-rctd  FOR fg-rctd.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    
    oplSuccess = YES.
    
    /* Retrieving last record of fg-rctd table - USE-INDEX fg-rctd is required to enforce the query to fetch latest r-no*/
    FIND LAST bf-fg-rctd NO-LOCK
         USE-INDEX fg-rctd NO-ERROR.
    IF AVAILABLE bf-fg-rctd AND bf-fg-rctd.r-no GT iRNo THEN
        iRNo = bf-fg-rctd.r-no.
    
    /* Retrieving last record of fg-rcpth table - USE-INDEX fg-rctd is required to enforce the query to fetch latest r-no*/
    /* In some cases r-no from fg-rcpth is greater than r-no from fg-rctd - so this additional find and the logic is required*/
    FIND LAST bf-fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAILABLE bf-fg-rcpth AND bf-fg-rcpth.r-no GT iRNo THEN
        iRNo = bf-fg-rcpth.r-no.

    FIND FIRST fg-rctd NO-LOCK
         WHERE ROWID(fg-rctd) EQ iprifgrctd
         NO-ERROR.
    IF NOT AVAILABLE fg-rctd THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "FG receipt is not available"
            .
            
        RETURN.
    END.
    FOR EACH bf-fg-rctd 
        WHERE bf-fg-rctd.company  EQ fg-rctd.company           
          AND bf-fg-rctd.i-no     EQ fg-rctd.i-no
          AND bf-fg-rctd.tag      EQ fg-rctd.tag
          AND ROWID(bf-fg-rctd)   NE ROWID(fg-rctd): 
        DELETE bf-fg-rctd.
    END.

    FOR EACH fg-bin NO-LOCK
        WHERE fg-bin.company EQ fg-rctd.company  
          AND fg-bin.i-no    EQ fg-rctd.i-no
          AND fg-bin.job-no  EQ fg-rctd.job-no
          AND fg-bin.job-no2 EQ fg-rctd.job-no2 
          AND fg-bin.tag     EQ fg-rctd.tag
          AND fg-bin.qty     NE 0:

        IF fg-bin.loc     NE fg-rctd.loc     OR
           fg-bin.loc-bin NE fg-rctd.loc-bin OR
           fg-bin.cust-no NE fg-rctd.cust-no THEN DO:
            CREATE bf-fg-rctd.
            BUFFER-COPY fg-rctd EXCEPT fg-rctd.r-no fg-rctd.loc fg-rctd.loc-bin fg-rctd.cust-no TO bf-fg-rctd.
            ASSIGN 
                bf-fg-rctd.r-no       = iRNo
                bf-fg-rctd.loc        = fg-bin.loc
                bf-fg-rctd.loc-bin    = fg-bin.loc-bin
                bf-fg-rctd.cust-no    = fg-bin.cust-no
                bf-fg-rctd.cases      = 0
                bf-fg-rctd.qty-case   = 0
                bf-fg-rctd.cases-unit = 0
                bf-fg-rctd.partial    = 0
                bf-fg-rctd.t-qty      = 0
                iRNo                  = iRNo + 1
                .
        END.
    END.  /* for each fg-bin*/
    
    RELEASE bf-fg-rctd.
    RELEASE bf-fg-rcpth.
END PROCEDURE.
    
