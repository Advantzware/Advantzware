
/*------------------------------------------------------------------------
    File        : VendorCostProcs.p
    Purpose     : Deprecate e-item-vend, e-item, e-itemfg, e-itemfg-vend tables

    Syntax      :

    Description : Procedures and functions for processing of vendItemCost and related tables

    Author(s)   : BV
    Created     : Thu Sep 05 12:28:07 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{system\VendorCostProcs.i}

/*Constants*/
DEFINE VARIABLE gcItemTypeFG        AS CHARACTER NO-UNDO INITIAL "FG".
DEFINE VARIABLE gcItemTypeRM        AS CHARACTER NO-UNDO INITIAL "RM".
DEFINE VARIABLE gdQuantityOffset    AS DECIMAL   NO-UNDO INITIAL 0.000001.
DEFINE VARIABLE gdQuantityMax       AS DECIMAL   NO-UNDO INITIAL 9999999.999999.
DEFINE VARIABLE gcScopeAll          AS CHARACTER NO-UNDO INITIAL "All".
DEFINE VARIABLE gcScopeEffective    AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired".
DEFINE VARIABLE gcScopeEstimated    AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - Estimated Only".
DEFINE VARIABLE gcScopeNotEstimated AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - Not Estimated Only".
DEFINE VARIABLE gcScopeList         AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcScopeDefault      AS CHARACTER NO-UNDO.

/*Settings Variables*/
DEFINE VARIABLE glUseQtyFrom        AS LOGICAL   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION GetValidScopes RETURNS CHARACTER 
	(  ) FORWARD.



/* ***************************  Main Block  *************************** */
ASSIGN 
    gcScopeList    = gcScopeAll + "," + gcScopeEffective + "," + gcScopeEstimated + "," + gcScopeNotEstimated
    gcScopeDefault = gcScopeNotEstimated
    .

/* **********************  Internal Procedures  *********************** */

PROCEDURE BuildVendItemCosts:
    /*------------------------------------------------------------------------------
     Purpose:  Given company, item, and item properties and quantity, build
     the temp-table of vendITemCosts based on scope
     Notes:
     Syntax:
         RUN BuildVendItemCosts(ipcCompany, ipcItemID, ipcItemType, ipcScope, iplIncludeBlankVendor,
            ipdQuantity, ipcQuantityUOM, 
            ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM,
            ipdBasisWeight, ipcBasisWeightUOM, 
            OUTPUT TABLE ttVendItemCost,
            OUTPUT oplError, OUTPUT opcMessage).
            
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcScope AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeBlankVendor AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimWidth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimDepth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcDimUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdBasisWeight AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcBasisWeightUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttVendItemCost.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-vendItemCost FOR vendItemCost.
    
    DEFINE VARIABLE lIncludeNonEffective AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIncludeExpired      AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lEstimatedOnly       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lNotEstimatedOnly    AS LOGICAL NO-UNDO.
    
    
    IF LOOKUP(ipcScope,gcScopeList) EQ 0 THEN 
        ipcScope = gcScopeDefault.
    CASE ipcScope:
        WHEN gcScopeAll THEN  
            ASSIGN 
                lIncludeExpired      = YES
                lIncludeNonEffective = YES
                .
        WHEN gcScopeEffective THEN 
            ASSIGN
                lIncludeExpired      = NO
                lIncludeNonEffective = NO
                . 
        WHEN gcScopeEstimated THEN 
            ASSIGN 
                lIncludeExpired      = NO
                lIncludeNonEffective = NO 
                lEstimatedOnly       = YES 
                .
        WHEN gcScopeNotEstimated THEN 
            ASSIGN 
                lIncludeExpired      = NO 
                lIncludeNonEffective = NO 
                lEstimatedOnly       = NO
                lNotEstimatedOnly    = YES
                .
    END CASE.
    EMPTY TEMP-TABLE ttVendItemCost.
    FOR EACH bf-vendItemCost NO-LOCK
        WHERE bf-vendItemCost.company EQ ipcCompany
        AND bf-vendItemCost.itemID EQ ipcItemID
        AND bf-vendItemCost.itemType EQ ipcItemType
        AND (bf-vendItemCost.effectiveDate LE TODAY OR lIncludeNonEffective)
        AND (bf-vendItemCost.expirationDate GE TODAY OR bf-vendItemCost.expirationDate EQ ? OR bf-vendItemCost.expirationDate EQ 01/01/0001 OR lIncludeExpired)
        AND (lEstimatedOnly AND bf-vendItemCost.estimateNo NE "" OR NOT lEstimatedOnly)
        AND (lNotEstimatedOnly AND bf-vendItemCost.estimateNo EQ "" OR NOT lNotEstimatedOnly)
        AND (bf-vendItemCost.vendorID NE "" OR iplIncludeBlankVendor)
        :
        CREATE ttVendItemCost.
        BUFFER-COPY bf-vendItemCost TO ttVendItemCost.
        ASSIGN 
            ttVendItemCost.isExpired = bf-vendItemCost.expirationDate LT TODAY AND bf-vendItemCost.expirationDate NE ? AND bf-vendItemCost.expirationDate NE 01/01/0001
            ttVendItemCost.isNotEffective = bf-vendItemCost.effectiveDate GT TODAY
            ttVendItemCost.quantityTarget = ipdQuantity
            ttVendItemCost.quantityTargetInVendorUOM = ipdQuantity
            ttVendItemCost.quantityTargetUOM = ipcQuantityUOM
            ttVendItemCost.dimLengthInVendorDimUOM = ipdDimLength
            ttVendItemCost.dimWidthInVendorDimUOM = ipdDimWidth
            ttVendItemCost.dimDepthInVendorDimUOM = ipdDimDepth
            .
        IF ttVendItemCost.isExpired  THEN 
            ttVendItemCost.reasonNotValid = ttVendItemCost.reasonNotValid + "Expired,".
        IF ttVendItemCost.isNotEffective  THEN 
            ttVendItemCost.reasonNotValid = ttVendItemCost.reasonNotValid + "Not Yet Effective,".
        
        IF ttVendItemCost.quantityTargetUOM NE ttVendItemCost.vendorUOM THEN
            RUN pConvertQuantity(ipcCompany, ipdQuantity, ttVendItemCost.quantityTargetUOM, ttVendItemCost.vendorUOM, 
                ipdBasisWeight, ipcBasisWeightUOM, ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM,
                OUTPUT ttVendItemCost.quantityTargetInVendorUOM, OUTPUT oplError, INPUT-OUTPUT opcMessage).
        IF ipcDimUOM NE bf-vendItemCost.dimUOM THEN 
        DO:
            RUN pConvertDim(ipcCompany, ipdDimLength, ipcDimUOM, bf-vendItemCost.dimUOM, 
                OUTPUT ttVendItemCost.dimLengthInVendorDimUOM, OUTPUT oplError, OUTPUT opcMessage).
            IF NOT oplError THEN 
                RUN pConvertDim(ipcCompany, ipdDimWidth, ipcDimUOM, bf-vendItemCost.dimUOM, 
                    OUTPUT ttVendItemCost.dimWidthInVendorDimUOM, OUTPUT oplError, OUTPUT opcMessage).
            IF NOT oplError THEN 
                RUN pConvertDim(ipcCompany, ipdDimDepth, ipcDimUOM, bf-vendItemCost.dimUOM, 
                    OUTPUT ttVendItemCost.dimDepthInVendorDimUOM, OUTPUT oplError, OUTPUT opcMessage).
            IF oplError THEN RETURN.
        END.
        IF ttVendItemCost.quantityMaximumOrder NE 0 AND ttVendItemCost.quantityMaximumOrder LT ttVendItemCost.quantityTargetInVendorUOM THEN 
            ttVendItemCost.reasonNotValid = ttVendItemCost.reasonNotValid + 
                "Order Quantity of " + STRING(ttVendItemCost.quantityTargetInVendorUOM) + 
                " greater than maximum order quantity of " + STRING(ttVendItemCost.quantityMaximumOrder) + 
                " " + ttVendItemCost.vendorUOM + ",".                 
        IF ttVendItemCost.quantityMinimumOrder GT ttVendItemCost.quantityTargetInVendorUOM THEN 
            ttVendItemCost.reasonNotValid = ttVendItemCost.reasonNotValid + 
                "Order Quantity of " + STRING(ttVendItemCost.quantityTargetInVendorUOM) + 
                " less than minimum order quantity of " + STRING(ttVendItemCost.quantityMinimumOrder) + 
                " " + ttVendItemCost.vendorUOM + ",".
        IF ttVendItemCost.dimLengthInVendorDimUOM LT ttVendItemCost.dimLengthMinimum THEN 
            ttVendItemCost.reasonNotValid = ttVendItemCost.reasonNotValid + 
                "Item length of " + STRING(ttVendItemCost.dimLengthInVendorDimUOM) + 
                " less than minimum length of " + STRING(ttVendItemCost.dimLengthMinimum) + 
                " " + ttVendItemCost.dimUOM + ",".
        IF ttVendItemCost.dimLengthInVendorDimUOM GT ttVendItemCost.dimLengthMaximum THEN 
            ttVendItemCost.reasonNotValid = ttVendItemCost.reasonNotValid + 
                "Item length of " + STRING(ttVendItemCost.dimLengthInVendorDimUOM) + 
                " less than maximum length of " + STRING(ttVendItemCost.dimLengthMaximum) + 
                " " + ttVendItemCost.dimUOM + ",".
        IF ttVendItemCost.dimWidthInVendorDimUOM LT ttVendItemCost.dimWidthMinimum THEN 
            ttVendItemCost.reasonNotValid = ttVendItemCost.reasonNotValid + 
                "Item width of " + STRING(ttVendItemCost.dimWidthInVendorDimUOM) + 
                " less than minimum width of " + STRING(ttVendItemCost.dimWidthMinimum) + 
                " " + ttVendItemCost.dimUOM + ",".
        IF ttVendItemCost.dimWidthInVendorDimUOM GT ttVendItemCost.dimWidthMaximum THEN 
            ttVendItemCost.reasonNotValid = ttVendItemCost.reasonNotValid + 
                "Item width of " + STRING(ttVendItemCost.dimWidthInVendorDimUOM) + 
                " less than maximum width of " + STRING(ttVendItemCost.dimWidthMaximum) + 
                " " + ttVendItemCost.dimUOM + ",".
        ttVendItemCost.reasonNotValid = TRIM(TRIM(ttVendItemCost.reasonNotValid,",")).
        IF ttVendItemCost.reasonNotValid EQ "" THEN 
            ttVendItemCost.isValid = YES.
        IF ttVendItemCost.isValid THEN DO:
            RUN pGetVendorCosts(BUFFER bf-vendItemCost, ttVendItemCost.quantityTargetInVendorUOM, 
            ttVendItemCost.dimLengthInVendorDimUOM, ttVendItemCost.dimWidthInVendorDimUOM, ttVendItemCost.dimUOM, 
            OUTPUT ttVendItemCost.costPerVendorUOM, OUTPUT ttVendItemCost.costSetup, OUTPUT ttVendItemCost.costPerVendorUOMUpcharge, OUTPUT ttVendItemCost.costTotal,
            OUTPUT oplError, INPUT-OUTPUT opcMessage).
            IF oplError THEN 
                ASSIGN 
                ttVendItemCost.isValid = NO 
                ttVendItemCost.reasonNotValid = opcMessage
                .
        END.
    END.   

END PROCEDURE.

PROCEDURE GetVendorCostNextBreak:
    /*------------------------------------------------------------------------------
     Purpose: Given a Item ID Vendor and Quantity, retrieve the next price break quantity
     and costs
     Notes:
     Syntax:  
    RUN GetVendorCostNextBreak(ipcCompany, ipcItemID, ipcItemType, ipcVendorID, ipcCustomerID, ipcEstimateNo, ipiFormNo, ipiBlankNo, 
        ipdQuantity, ipcQuantityUOM, 
        ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM, ipdBasisWeight, ipcBasisWeightUOM, iplExactMatch,
        OUTPUT opdCostPerUOM, OUTPUT opdCostSetup, OUTPUT opcCostUOM, OUTPUT opdCostTotal, 
        OUTPUT opdQuantityNextPriceBreak,
        OUTPUT lError, OUTPUT cMessage).     
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendorID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimWidth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimDepth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcDimUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdBasisWeight AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcBasisWeightUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplExactMatch AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMNextPriceBreak AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostSetupNextPriceBreak AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotalNextPriceBreak AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityNextPriceBreak AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-vendItemCost FOR vendItemCost.
    DEFINE VARIABLE dQuantityInVendorUOM AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostPerUOMUpcharge  AS DECIMAL NO-UNDO.
    
    RUN pSetGlobalSettings(ipcCompany).
    RUN pGetVendItemCostBuffer(ipcCompany, ipcItemID, ipcItemType, ipcVendorID, ipcCustomerID, ipcEstimateNo, ipiFormNo, ipiBlankNo, iplExactMatch,
        BUFFER bf-vendItemCost, OUTPUT oplError, OUTPUT opcMessage).
    IF NOT oplError AND AVAILABLE bf-vendItemCost THEN 
    DO:
        ASSIGN 
            opcCostUOM = bf-vendItemCost.vendorUOM.
        IF opcCostUOM NE ipcQuantityUOM THEN 
        DO: 
            RUN pConvertQuantity(ipcCompany, ipdQuantity, ipcQuantityUOM, opcCostUOM, 
                ipdBasisWeight, ipcBasisWeightUOM, ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM,
                OUTPUT dQuantityInVendorUOM, OUTPUT oplError, INPUT-OUTPUT opcMessage).
        END.
        ELSE 
            dQuantityInVendorUOM = ipdQuantity. 
        
        RUN pGetCostsNextBreak(BUFFER bf-vendItemCost, dQuantityInVendorUOM, ipdDimLength, ipdDimWidth, ipcDimUOM,
            OUTPUT opdCostPerUOMNextPriceBreak, OUTPUT opdCostSetupNextPriceBreak, OUTPUT dCostPerUOMUpcharge, OUTPUT opdCostTotalNextPriceBreak, OUTPUT opdQuantityNextPriceBreak, 
            OUTPUT oplError, INPUT-OUTPUT opcMessage).
        opdCostPerUOMNextPriceBreak = opdCostPerUOMNextPriceBreak + dCostPerUOMUpcharge.
        
    END.
END PROCEDURE.

PROCEDURE GetVendorCost:
    /*------------------------------------------------------------------------------
     Purpose: Given a Item ID Vendor and Quantity, retrieve the effective 
     costs
     Notes:
     Syntax:  
    RUN GetVendorCost(ipcCompany, ipcItemID, ipcItemType, ipcVendorID, ipcCustomerID, ipcEstimateNo, ipiFormNo, ipiBlankNo, 
        ipdQuantity, ipcQuantityUOM, 
        ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM, ipdBasisWeight, ipcBasisWeightUOM, iplExactMatch,
        OUTPUT opdCostPerUOM, OUTPUT opdCostSetup, OUTPUT opcCostUOM, OUTPUT opdCostTotal, OUTPUT lError, OUTPUT cMessage).     
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendorID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimWidth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimDepth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcDimUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdBasisWeight AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcBasisWeightUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplExactMatch AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostSetup AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-vendItemCost FOR vendItemCost.
    DEFINE VARIABLE dQuantityInVendorUOM AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostPerUOMUpcharge  AS DECIMAL NO-UNDO.
    
    RUN pSetGlobalSettings(ipcCompany).
    RUN pGetVendItemCostBuffer(ipcCompany, ipcItemID, ipcItemType, ipcVendorID, ipcCustomerID, ipcEstimateNo, ipiFormNo, ipiBlankNo, iplExactMatch,
        BUFFER bf-vendItemCost, OUTPUT oplError, OUTPUT opcMessage).
    IF NOT oplError AND AVAILABLE bf-vendItemCost THEN 
    DO:
        ASSIGN 
            opcCostUOM = bf-vendItemCost.vendorUOM.
        IF opcCostUOM NE ipcQuantityUOM THEN 
        DO: 
            RUN pConvertQuantity(ipcCompany, ipdQuantity, ipcQuantityUOM, opcCostUOM, 
                ipdBasisWeight, ipcBasisWeightUOM, ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM,
                OUTPUT dQuantityInVendorUOM, OUTPUT oplError, INPUT-OUTPUT opcMessage).
        END.
        ELSE 
            dQuantityInVendorUOM = ipdQuantity. 
        
        RUN pGetVendorCosts(BUFFER bf-vendItemCost, dQuantityInVendorUOM, ipdDimLength, ipdDimWidth, ipcDimUOM, 
            OUTPUT opdCostPerUOM, OUTPUT opdCostSetup, OUTPUT dCostPerUOMUpcharge, OUTPUT opdCostTotal,
            OUTPUT oplError, INPUT-OUTPUT opcMessage).
        opdCostPerUOM = opdCostPerUOM + dCostPerUOMUpcharge.
        
    END.
END PROCEDURE.

PROCEDURE GetVendorItem:
    /*------------------------------------------------------------------------------
     Purpose: Given Vendor Matching information, find the vendor Item ID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendorID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplExactMatch AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcVendorItemID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-vendItemCost FOR vendItemCost.
    
    RUN pSetGlobalSettings(ipcCompany).
    RUN pGetVendItemCostBuffer(ipcCompany, ipcItemID, ipcItemType, ipcVendorID, ipcCustomerID, ipcEstimateNo, ipiFormNo, ipiBlankNo, iplExactMatch,
        BUFFER bf-vendItemCost, OUTPUT oplError, OUTPUT opcMessage).
    IF NOT oplError AND AVAILABLE bf-vendItemCost THEN 
        ASSIGN
            opcVendorItemID = bf-vendItemCost.vendorItemID 
            . 
            
END PROCEDURE.

PROCEDURE GetVendorSizes:
    /*------------------------------------------------------------------------------
     Purpose: Given Vendor Matching information, find the effective size limitations
     for the vendor item
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendorID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplExactMatch AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdMinLength AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdMaxLength AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdMinWidth AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdMaxWidth AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-vendItemCost FOR vendItemCost.
    
    RUN pSetGlobalSettings(ipcCompany).
    RUN pGetVendItemCostBuffer(ipcCompany, ipcItemID, ipcItemType, ipcVendorID, ipcCustomerID, ipcEstimateNo, ipiFormNo, ipiBlankNo, iplExactMatch,
        BUFFER bf-vendItemCost, OUTPUT oplError, OUTPUT opcMessage).
    IF NOT oplError AND AVAILABLE bf-vendItemCost THEN 
        ASSIGN
            opdMinLength = bf-vendItemCost.dimLengthMinimum
            opdMaxLength = bf-vendItemCost.dimLengthMaximum
            opdMinWidth  = bf-vendItemCost.dimWidthMinimum
            opdMaxWidth  = bf-vendItemCost.dimWidthMaximum 
            . 
            
END PROCEDURE.

PROCEDURE GetVendorUOM:
    /*------------------------------------------------------------------------------
     Purpose: Given Vendor Matching information, find the vendor UOM
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendorID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplExactMatch AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcVendorUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-vendItemCost FOR vendItemCost.
    
    RUN pSetGlobalSettings(ipcCompany).
    RUN pGetVendItemCostBuffer(ipcCompany, ipcItemID, ipcItemType, ipcVendorID, ipcCustomerID, ipcEstimateNo, ipiFormNo, ipiBlankNo, iplExactMatch,
        BUFFER bf-vendItemCost, OUTPUT oplError, OUTPUT opcMessage).
    IF NOT oplError AND AVAILABLE bf-vendItemCost THEN 
        ASSIGN
            opcVendorUOM = bf-vendItemCost.vendorUOM 
            . 
            
END PROCEDURE.

PROCEDURE GetDimCharge:
    /*------------------------------------------------------------------------------
     Purpose: To get the Dim charge   
     Parameters:  <none>
     Notes:       
     ------------------------------------------------------------------------------*/

    DEFINE INPUT        PARAMETER ipriID                 AS ROWID     NO-UNDO.
    DEFINE INPUT        PARAMETER ipdDimWidth            AS DECIMAL   NO-UNDO.
    DEFINE INPUT        PARAMETER ipdDimLength           AS DECIMAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdCostPerUOMUpcharge AS DECIMAL   NO-UNDO.  
    DEFINE OUTPUT       PARAMETER oplError               AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage             AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-vendItemCost FOR VendItemCost.
    
    FIND FIRST bf-vendItemCost NO-LOCK 
         WHERE ROWID(bf-vendItemCost) EQ ipriID
         NO-ERROR.
         
    IF NOT AVAILABLE bf-vendItemCost THEN DO:
        ASSIGN 
            oplError   = TRUE
            opcMessage = "Invalid vendItemCost record"
            .
        RETURN.
    END.
    
    RUN pGetUpchargeCostsForVendItemCost(
        BUFFER bf-vendItemCost, 
        INPUT  ipdDimLength, 
        INPUT  ipdDimWidth, 
        INPUT  bf-vendItemCost.dimUOM, 
        OUTPUT iopdCostPerUOMUpcharge, 
        OUTPUT oplError, 
        INPUT-OUTPUT opcMessage
        ).
        
    RELEASE bf-vendItemCost.    
    
END PROCEDURE.

PROCEDURE pConvertDim PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Converts a dimensional length to desired UOM
     Notes:
     Syntax:
         RUN pConvertDim(ipcCompany, ipdDimLength, ipcDimUOM, ipbf-vendItemCost.dimUOM, 
            OUTPUT dDimLengthInVendorDimUOM, OUTPUT oplError, OUTPUT iopcMessage).
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimInFromUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcFromUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcToUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDimInToUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcMessage AS CHARACTER NO-UNDO.

    /*Refactor - support for non IN uom as assumed now*/
    opdDimInToUOM = ipdDimInFromUOM.

END PROCEDURE.

PROCEDURE pConvertQuantity PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Converts given value in given uom to desired UOM, based on additional
     item specs
     Notes:
     Syntax:
         RUN pConvertQuantity(cCompany, dQtyInFromUOM, cFromUOM, cToUOM, 
            dBasisWeight, cBasisWeightUOM, dLength, dWidth, dDepth, cDimUOM,
            OUTPUT dQtyInToUOM, OUTPUT oplError, INPUT-OUTPUT iopcMessage).
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQtyInFromUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcFromUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcToUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdBasisWeight AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcBasisWeightUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimWidth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimDepth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcDimUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQtyInToUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dBasisWeightInLbsPerMSF AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dDimLengthInIN          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dDimWidthInIN           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dDimDepthInIN           AS DECIMAL NO-UNDO.

    /*Refactor handling of non-assumed UOMs of "IN" and "LBS/MSF" and handle error & message propagation*/
    ASSIGN 
        dBasisWeightInLBSPerMSF = ipdBasisWeight
        dDimLengthInIN          = ipdDimLength
        dDimWidthInIN           = ipdDimWidth
        dDimDepthInIN           = ipdDimDepth
        .

    RUN custom/convquom.p(ipcCompany, ipcFromUOM, ipcToUOM, 
        dBasisWeightInLBSPerMSF, dDimLengthInIN, dDimWidthInIN, dDimDepthInIN,
        ipdQtyInFromUOM, OUTPUT opdQtyInToUOM).      

END PROCEDURE.

PROCEDURE pGetCostLevel PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a VendItemCostID and Target Quantity, return the Level ID that matches it
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiVendItemCostID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityTarget AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiVendItemCostLevelID AS INT64 NO-UNDO.
    
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
    
    RUN pRecalculateFromAndToForLevels(ipiVendItemCostID).
    FIND FIRST bf-vendItemCostLevel NO-LOCK 
        WHERE bf-vendItemCostLevel.vendItemCostID EQ ipiVendItemCostID
        AND bf-vendItemCostLevel.quantityFrom LE ipdQuantityTarget
        AND bf-vendItemCostLevel.quantityTo GE ipdQuantityTarget
        NO-ERROR.
    IF AVAILABLE bf-vendItemCostLevel THEN 
        opiVendItemCostLevelID = bf-vendItemCostLevel.vendItemCostLevelID.
        
END PROCEDURE.

PROCEDURE pGetCostsForVendItemCost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a VendItemCostBuffer, and a quantity, get appropriate costs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-vendItemCost FOR vendItemCost.
    DEFINE INPUT PARAMETER ipdQuantityInVendorUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostSetup AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcMessage AS CHARACTER NO-UNDO.
 
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
    DEFINE VARIABLE iVendCostLevelID AS INT64 NO-UNDO.
    
    iopcMessage = TRIM(iopcMessage + " Quantity of " + STRING(ipdQuantityInVendorUOM)).
    
    RUN pGetCostLevel(ipbf-vendItemCost.vendItemCostID, ipdQuantityInVendorUOM, OUTPUT iVendCostLevelID).
    IF iVendCostLevelID NE 0 THEN 
        FIND FIRST bf-vendItemCostLevel NO-LOCK 
            WHERE bf-vendItemCostLevel.vendItemCostLevelID EQ iVendCostLevelID
            NO-ERROR.
    IF AVAILABLE bf-vendItemCostLevel THEN 
        ASSIGN
            opdCostPerUOM = bf-vendItemCostLevel.costPerUOM
            opdCostSetup  = bf-vendItemCostLevel.costSetup
            oplError      = NO
            iopcMessage   = iopcMessage + " in range." 
            .  
    ELSE 
        ASSIGN 
            oplError    = YES
            iopcMessage = iopcMessage + " not in range."
            .

END PROCEDURE.

PROCEDURE pGetCostsForVendItemCostNextBreak PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a VendItemCostBuffer, and a quantity, get appropriate costs for
     the next level up from the given quantity Level
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-vendItemCost FOR vendItemCost.
    DEFINE INPUT PARAMETER ipdQuantityInVendorUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMNextBreak AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostSetupNextBreak AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityNextBreak AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcMessage AS CHARACTER NO-UNDO.
 
    DEFINE VARIABLE iVendCostLevelID AS INT64 NO-UNDO.
    DEFINE BUFFER bf-vendItemCostLevel          FOR vendItemCostLevel.
    DEFINE BUFFER bfNextBreak-vendItemCostLevel FOR vendItemCostLevel.
    
    RUN pGetCostLevel(ipbf-vendItemCost.vendItemCostID, ipdQuantityInVendorUOM, OUTPUT iVendCostLevelID).
    IF iVendCostLevelID NE 0 THEN 
        FIND FIRST bf-vendItemCostLevel NO-LOCK 
            WHERE bf-vendItemCostLevel.vendItemCostLevelID EQ iVendCostLevelID
            NO-ERROR.
    IF AVAILABLE bf-vendItemCostLevel THEN 
    DO:
        FIND FIRST bfNextBreak-vendItemCostLevel NO-LOCK
            WHERE bfNextBreak-vendItemCostLevel.vendItemCostID EQ bf-vendItemCostLevel.vendItemCostID
            AND bfNextBreak-vendItemCostLevel.quantityFrom GE bf-vendItemCostLevel.quantityTo + gdQuantityOffset
            NO-ERROR.
        IF AVAILABLE bfNextBreak-vendItemCostLevel THEN 
            ASSIGN
                opdCostPerUOMNextBreak = bfNextBreak-vendItemCostLevel.costPerUOM
                opdCostSetupNextBreak  = bfNextBreak-vendItemCostLevel.costSetup
                opdQuantityNextBreak   = bfNextBreak-vendItemCostLevel.quantityFrom
                oplError               = NO
                iopcMessage            = iopcMessage + " Next Price Break Available" 
                .  
        ELSE 
            ASSIGN 
                oplError    = YES
                iopcMessage = iopcMessage + " No Price Break Available"
                .
    END. /*Avail Cost Level*/

END PROCEDURE.

PROCEDURE pGetCostsNextBreak PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Retrieve the vendor cost break level, given quantity already
     converted to the vendorUOM.
     Notes:
     Syntax: 
         RUN pGetCostsNextBreak(BUFFER ipbf-vendItemCost, dQuantityInVendorUOM, 
                OUTPUT opdCostPerUOM, OUTPUT opdCostSetup, OUTPUT opdCostPerUOMUpcharge, OUTPUT opdCostTotal,
                OUTPUT oplError, INPUT-OUTPUT iopcMessage).
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-vendItemCost FOR vendItemCost.
    DEFINE INPUT PARAMETER ipdQuantityInVendorUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimWidth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcDimUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMNextBreak AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostSetupNextBreak AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMUpcharge AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotalNextBreak AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityNextBreak AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcMessage AS CHARACTER NO-UNDO.


    RUN pGetCostsForVendItemCostNextBreak(BUFFER ipbf-vendItemCost, ipdQuantityInVendorUOM,
        OUTPUT opdCostPerUOMNextBreak, OUTPUT opdCostSetupNextBreak, OUTPUT opdQuantityNextBreak, 
        OUTPUT oplError, INPUT-OUTPUT iopcMessage).   
    IF NOT oplError THEN 
    DO:    
        RUN pGetUpchargeCostsForVendItemCost(BUFFER ipbf-vendItemCost, ipdDimLength, ipdDimWidth, ipcDimUOM, 
            OUTPUT opdCostPerUOMUpcharge, OUTPUT oplError, INPUT-OUTPUT iopcMessage).                       
        opdCostTotalNextBreak = (opdCostPerUOMNextBreak + opdCostPerUOMUpcharge) * opdQuantityNextBreak + opdCostSetupNextBreak. 
    END.
    
END PROCEDURE.

PROCEDURE pGetUpchargeCostsForVendItemCost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a vendItemCostBuffer, and dimensions, determine the upcharges if any
     Notes:
     Syntax: pGetUpchargeCostsForVendItemCost(BUFFER bf-vendItemCost, ipdDimLength, ipdDimWidth, ipcDimUOM, 
                OUTPUT opdCostPerUOMUpcharge, OUTPUT oplError, OUTPUT opcMessage).
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-vendItemCost FOR vendItemCost.
    DEFINE INPUT PARAMETER ipdDimLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimWidth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcDimUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMUpcharge AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dCostUpchargeOverLength  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostUpchargeOverWidth   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostUpchargeUnderLength AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostUpchargeUnderWidth  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dDimLengthInVendorDimUOM AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dDimWidthInVendorDimUOM  AS DECIMAL NO-UNDO.

    IF ipcDimUOM NE ipbf-vendItemCost.dimUOM THEN 
    DO:
        RUN pConvertDim(
            INPUT  ipbf-vendItemCost.company, 
            INPUT  ipdDimLength, 
            INPUT  ipcDimUOM,
            INPUT  ipbf-vendItemCost.dimUOM, 
            OUTPUT dDimLengthInVendorDimUOM, 
            OUTPUT oplError, 
            INPUT-OUTPUT iopcMessage
            ).
        IF NOT oplError THEN 
            RUN pConvertDim(
                INPUT  ipbf-vendItemCost.company, 
                INPUT  ipdDimWidth,
                INPUT  ipcDimUOM, 
                INPUT  ipbf-vendItemCost.dimUOM, 
                OUTPUT dDimWidthInVendorDimUOM, 
                OUTPUT oplError,
                INPUT-OUTPUT iopcMessage
                ).
        IF oplError THEN RETURN.
    END.
    ELSE 
        ASSIGN 
            dDimLengthInVendorDimUOM = ipdDimLength
            dDimWidthInVendorDimUOM  = ipdDimWidth
            .    
    
    IF ipdDimLength NE 0 AND ipbf-vendItemCost.dimLengthOver NE 0 THEN 
        dCostUpChargeOverLength = IF dDimLengthInVendorDimUOM GT ipbf-vendItemCost.dimLengthOver THEN ipbf-vendItemCost.dimLengthOverCharge ELSE 0.
    IF ipdDimWidth NE 0 AND ipbf-vendItemCost.dimWidthOver NE 0 THEN 
        dCostUpChargeOverWidth = IF dDimWidthInVendorDimUOM GT ipbf-vendItemCost.dimWidthOver THEN ipbf-vendItemCost.dimWidthOverCharge ELSE 0.
    IF ipdDimLength NE 0 AND ipbf-vendItemCost.dimLengthUnder NE 0 THEN 
        dCostUpChargeUnderLength = IF dDimLengthInVendorDimUOM GT ipbf-vendItemCost.dimLengthUnder THEN ipbf-vendItemCost.dimLengthUnderCharge ELSE 0.
    IF ipdDimWidth NE 0 AND ipbf-vendItemCost.dimWidthUnder NE 0 THEN 
        dCostUpChargeUnderWidth = IF dDimWidthInVendorDimUOM GT ipbf-vendItemCost.dimWidthUnder THEN ipbf-vendItemCost.dimWidthUnderCharge ELSE 0.
    opdCostPerUOMUpCharge = dCostUpChargeOverLength + dCostUpChargeOverWidth + dCostUpChargeUnderLength + dCostUpChargeUnderWidth.   
    IF opdCostPerUOMUpCharge NE 0 THEN 
        iopcMessage = iopcMessage + " Includes Dimension Upcharge".
        
END PROCEDURE.

PROCEDURE pGetVendItemCostBuffer PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns the effective VendItemCost Match given inputs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendorID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplExactMatch AS LOGICAL NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-vendItemCost FOR vendItemCost.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lIsRM           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMsgConstInputs AS CHARACTER NO-UNDO INITIAL " Inputs->".
    DEFINE VARIABLE cMsgConstUsing  AS CHARACTER NO-UNDO INITIAL " Using->".
    DEFINE VARIABLE cMsgConstComp   AS CHARACTER NO-UNDO INITIAL " Company:".
    DEFINE VARIABLE cMsgConstItem   AS CHARACTER NO-UNDO INITIAL " Item:".
    DEFINE VARIABLE cMsgConstVend   AS CHARACTER NO-UNDO INITIAL " Vend:".
    DEFINE VARIABLE cMsgConstCust   AS CHARACTER NO-UNDO INITIAL " Cust:". 
    DEFINE VARIABLE cMsgConstEst    AS CHARACTER NO-UNDO INITIAL " Est:". 
    DEFINE VARIABLE cMsgConstBlank  AS CHARACTER NO-UNDO INITIAL "[BLANK]".
    DEFINE VARIABLE cMsgComp        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgItem        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgVend        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgCust        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgEst         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgInputs      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgRMInputs    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgFGInputs    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsgUsing       AS CHARACTER NO-UNDO.
    
    &SCOPED-DEFINE RequiredCriteria WHERE opbf-vendItemCost.company EQ ipcCompany ~
                                AND opbf-vendItemCost.itemID EQ ipcItemID ~
                                AND opbf-vendItemCost.itemType EQ ipcItemType ~
                                AND opbf-vendItemCost.effectiveDate LE TODAY ~
                                AND (opbf-vendItemCost.expirationDate GE TODAY OR opbf-vendItemCost.expirationDate EQ ? OR opbf-vendItemCost.expirationDate EQ 01/01/0001) 

    ASSIGN 
        lIsRM        = ipcItemType EQ gcItemTypeRM
        cMsgComp     = cMsgConstComp + ipcCompany
        cMsgItem     = " " + ipcItemType + cMsgConstItem + ipcItemID
        cMsgVend     = cMsgConstVend + (IF ipcVendorID EQ "" THEN cMsgConstBlank ELSE ipcVendorID)
        cMsgCust     = cMsgConstCust + (IF ipcCustomerID EQ "" THEN cMsgConstBlank ELSE ipcCustomerID)
        cMsgEst      = cMsgConstEst + ipcEstimateNo + "-" + STRING(ipiFormNo,">>9") + "-" + STRING(ipiBlankNo, ">>9")
        cMsgRMInputs = cMsgConstInputs + cMsgComp + cMsgItem + cMsgVend 
        cMsgFGInputs = cMsgRMInputs + cMsgCust
        cMsgInputs   = cMsgFGInputs + cMsgEst
        .

    FIND FIRST opbf-vendItemCost NO-LOCK
        {&RequiredCriteria}
        AND opbf-vendItemCost.vendorID EQ ipcVendorID
        AND opbf-vendItemCost.customerID EQ ipcCustomerID
        AND opbf-vendItemCost.estimateNo EQ ipcEstimateNo
        AND opbf-vendItemCost.formNo EQ ipiFormNo
        AND opbf-vendItemCost.blankNo EQ ipiBlankNo
        NO-ERROR.
    IF NOT AVAILABLE opbf-vendItemCost THEN 
    DO:
        IF iplExactMatch THEN
            ASSIGN 
                oplError   = YES
                opcMessage = "Exact Match for effective and non-expired vendor cost not found." + cMsgInputs
                .
        ELSE 
        DO:  /*Fall back find options*/
            opcMessage = "Exact match not found but fallback effective and non-expired vendor cost found.".
            IF lIsRM THEN 
            DO: /*RM fall back options*/
                cMsgInputs = cMsgRMInputs.
                FIND FIRST opbf-vendItemCost NO-LOCK /*Match with blank customer*/
                    {&RequiredCriteria}
                    AND opbf-vendItemCost.vendorID EQ ipcVendorID
                    AND opbf-vendItemCost.customerID EQ ""
                    NO-ERROR.
                IF NOT AVAILABLE opbf-vendItemCost THEN 
                    FIND FIRST opbf-vendItemCost NO-LOCK /*Match with any customer - current functionality*/
                    {&RequiredCriteria}
                    AND opbf-vendItemCost.vendorID EQ ipcVendorID
                    NO-ERROR.
                IF NOT AVAILABLE opbf-vendItemCost THEN 
                    FIND FIRST opbf-vendItemCost NO-LOCK /*Match with blank vendor*/
                    {&RequiredCriteria}
                    AND opbf-vendItemCost.vendorID EQ ""
                    NO-ERROR.
                IF AVAILABLE opbf-vendItemCost THEN 
                    ASSIGN 
                        cMsgUsing = cMsgConstUsing + cMsgConstVend + (IF opbf-vendItemCost.vendorID EQ "" THEN cMsgConstBlank ELSE opbf-vendItemCost.vendorID)
                        cMsgUsing = cMsgUsing + cMsgConstCust + (IF opbf-vendItemCost.customerID EQ "" THEN cMsgConstBlank ELSE opbf-vendItemCost.customerID)
                        .
            END.
            ELSE 
            DO:  /*FG fall back options*/
                IF ipcEstimateNo NE "" THEN 
                DO:  /*Fall back options for Farm Tab in estimate*/
                    FIND FIRST opbf-vendItemCost NO-LOCK  /*Match with blank vendor*/
                        {&RequiredCriteria}
                        AND opbf-vendItemCost.estimateNo EQ ipcEstimateNo
                        AND opbf-vendItemCost.formNo EQ ipiFormNo
                        AND opbf-vendItemCost.blankNo EQ ipiBlankNo
                        AND opbf-vendItemCost.vendorID EQ ""
                        NO-ERROR.
                    IF AVAILABLE opbf-vendItemCost THEN 
                        cMsgUsing = cMsgConstUsing + cMsgConstVend + (IF opbf-vendItemCost.vendorID EQ "" THEN cMsgConstBlank ELSE opbf-vendItemCost.vendorID).
                END.
                ELSE 
                DO: /*Fall back options for PO*/
                    cMsgInputs = cMsgFGInputs.
                    FIND FIRST opbf-vendItemCost NO-LOCK /*Match with blank customer*/
                        {&RequiredCriteria}
                        AND opbf-vendItemCost.estimateNo EQ ""
                        AND opbf-vendItemCost.vendorID EQ ipcVendorID
                        AND opbf-vendItemCost.customerID EQ ""
                        NO-ERROR.
                    IF NOT AVAILABLE opbf-vendItemCost THEN 
                        FIND FIRST opbf-vendItemCost NO-LOCK /*Match with blank vendor blank customer*/
                        {&RequiredCriteria}
                        AND opbf-vendItemCost.estimateNo EQ ""
                        AND opbf-vendItemCost.vendorID EQ ""
                        AND opbf-vendItemCost.customerID EQ ""
                        NO-ERROR.
                    ASSIGN 
                        cMsgUsing = cMsgConstUsing + cMsgConstVend + (IF opbf-vendItemCost.vendorID EQ "" THEN cMsgConstBlank ELSE opbf-vendItemCost.vendorID)
                        cMsgUsing = cMsgUsing + cMsgConstCust + (IF opbf-vendItemCost.customerID EQ "" THEN cMsgConstBlank ELSE opbf-vendItemCost.customerID)
                        .
                END.    
            END.
            IF AVAILABLE opbf-vendItemCost THEN 
                ASSIGN 
                    oplError   = NO  
                    opcMessage = opcMessage + cMsgInputs + cMsgUsing
                    .
            ELSE 
                ASSIGN 
                    oplError   = YES
                    opcMessage = "No effective or non-expired vendor cost match found." + cMsgInputs
                    .    
        END.  /*Not iplExactMatch*/ 
    END. /*buffer not available*/
    ELSE /*buffer found*/
        ASSIGN 
            oplError   = NO
            opcMessage = "Exact Match for effective and non-expired vendor cost found." + cMsgInputs
            . 

END PROCEDURE.

PROCEDURE pGetVendorCosts PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Core Function for retrieving the vendor cost, given quantity already
     converted to the vendorUOM.
     Notes:
     Syntax: 
         RUN pGetVendorCosts(BUFFER ipbf-vendItemCost, dQuantityInVendorUOM, ipdDimLength, ipdDimWidth, ipdDimUOM, 
                OUTPUT opdCostPerUOM, OUTPUT opdCostSetup, OUTPUT opdCostPerUOMUpcharge, OUTPUT opdCostTotal,
                OUTPUT oplError, INPUT-OUTPUT iopcMessage).
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-vendItemCost FOR vendItemCost.
    DEFINE INPUT PARAMETER ipdQuantityInVendorUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimWidth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcDimUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostSetup AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMUpcharge AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcMessage AS CHARACTER NO-UNDO.


    RUN pGetCostsForVendItemCost(BUFFER ipbf-vendItemCost, ipdQuantityInVendorUOM,
        OUTPUT opdCostPerUOM, OUTPUT opdCostSetup,OUTPUT oplError, INPUT-OUTPUT iopcMessage).   
    IF NOT oplError THEN 
    DO:
        RUN pGetUpchargeCostsForVendItemCost(BUFFER ipbf-vendItemCost, ipdDimLength, ipdDimWidth, ipcDimUOM, 
            OUTPUT opdCostPerUOMUpcharge, OUTPUT oplError, INPUT-OUTPUT iopcMessage).             
        opdCostTotal = (opdCostPerUOM + opdCostPerUOMUpcharge) * ipdQuantityInVendorUOM + opdCostSetup. 
    END.
    
END PROCEDURE.

PROCEDURE pRecalculateFromAndToForLevels PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a vendItemCost buffer, recalculate all level from and to quantities
     based on base quantity
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiVendItemCostID AS INT64 NO-UNDO.
    
    DEFINE BUFFER bf-vendItemCost      FOR vendItemCost.
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
    DEFINE VARIABLE dQtyNext AS DECIMAL NO-UNDO.
    
    FIND FIRST bf-vendItemCost NO-LOCK
        WHERE bf-vendItemCost.vendItemCostID EQ ipiVendItemCostID
        NO-ERROR.
    IF NOT AVAILABLE bf-vendItemCost THEN RETURN.
    IF bf-vendItemCost.useQuantityFromBase THEN 
    DO:
        dQtyNext = IF bf-vendItemCost.quantityMaximumOrder NE 0 THEN bf-vendItemCost.quantityMaximumOrder ELSE gdQuantityMax.  
        FOR EACH bf-vendItemCostLevel EXCLUSIVE-LOCK
            WHERE bf-vendItemCostLevel.vendItemCostID EQ bf-vendItemCost.vendItemCostID
            BY bf-vendItemCostLevel.quantityBase DESCENDING:
            ASSIGN 
                bf-vendItemCostLevel.quantityTo   = dQtyNext
                bf-vendItemCostLevel.quantityFrom = bf-vendItemCostLevel.quantityBase
                dQtyNext                          = bf-vendItemCostLevel.quantityBase - gdQuantityOffset
                .
        END.
    END.
    ELSE 
    DO:
        FOR EACH bf-vendItemCostLevel EXCLUSIVE-LOCK
            WHERE bf-vendItemCostLevel.vendItemCostID EQ bf-vendItemCost.vendItemCostID
            BY bf-vendItemCostLevel.quantityBase:
            ASSIGN 
                bf-vendItemCostLevel.quantityFrom = dQtyNext
                bf-vendItemCostLevel.quantityTo   = bf-vendItemCostLevel.quantityBase
                .
            IF dQtyNext NE bf-vendItemCostLevel.quantityBase THEN 
                dQtyNext = bf-vendItemCostLevel.quantityBase + gdQuantityOffset.
        END.
    END.    
    RELEASE bf-vendItemCostLevel.
    
END PROCEDURE.

PROCEDURE pSetGlobalSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Sets the NK1 setting global variables that are pertinent to th
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (ipcCompany, "VendCostMatrix", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN glUseQtyFrom = cReturn EQ "YES".
    
END PROCEDURE.

PROCEDURE RecalculateFromAndTo:
    /*------------------------------------------------------------------------------
     Purpose: Public wrapper for processing the From/To ranges for a given
     vendItemCost record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiVendItemCostID AS INT64 NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-vendItemCost FOR vendItemCost.

    FIND FIRST bf-vendItemCost NO-LOCK
        WHERE bf-vendItemCost.vendItemCostID EQ ipiVendItemCostID
        NO-ERROR.
    IF AVAILABLE bf-vendItemCost THEN 
    DO: 
        RUN pRecalculateFromAndToForLevels(bf-vendItemCost.vendItemCostID).
        opcMessage = "Successfully recalculated all from and to quantities for vendor item cost".
    END.
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid VendItemCostID: " + STRING(ipiVendItemCostID)
            . 

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION GetValidScopes RETURNS CHARACTER 
	(  ):
/*------------------------------------------------------------------------------
 Purpose: returns the global property of valid scopes
 Notes:
------------------------------------------------------------------------------*/	
	RETURN gcScopeList.
		
END FUNCTION.

