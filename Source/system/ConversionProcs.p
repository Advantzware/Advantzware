
/*------------------------------------------------------------------------
    File        : ConversionProcs.p
    Purpose     : 

    Syntax      :

    Description : All UOM and Conversion Handling Procedures		

    Author(s)   : BV
    Created     : Thu Mar 12 22:26:33 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttUOM 
    FIELD uom                 AS CHARACTER CASE-SENSITIVE
    FIELD uomBase             AS CHARACTER CASE-SENSITIVE
    FIELD multiplierToBase    AS DECIMAL
    FIELD uomDescription      AS CHARACTER
    FIELD canUseOrderQuantity AS LOGICAL 
    FIELD canUsePOQuantity    AS LOGICAL
    FIELD canUseStockQuantity AS LOGICAL
    FIELD canUsePricePerUnit  AS LOGICAL
    FIELD canUseCostPerUnit   AS LOGICAL
    FIELD isBaseConverter     AS LOGICAL
    FIELD isOverridden        AS LOGICAL
    FIELD uomSource           AS CHARACTER 
    . 


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION Conv_IsEAUOM RETURNS LOGICAL 
    (ipcCompany AS CHARACTER,
    ipcItemID AS CHARACTER,
    ipcUOM AS CHARACTER) FORWARD.

FUNCTION fUseItemUOM RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */

PROCEDURE Conv_CalcExtendedValueForItem:
    /*------------------------------------------------------------------------------
     Purpose:  For an item, calculated Extended Value (unit in UOM x Value in UOM)
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdQty AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQtyUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdValue AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcValueUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdValueExtended AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dValueInQtyUOM AS DECIMAL NO-UNDO.

    IF CAN-DO("L,LOT",ipcValueUOM) THEN 
        opdValueExtended = ipdValue.
    ELSE 
    DO:
        RUN Conv_ValueFromUOMToUOMForItem(ipriItem, ipdValue, ipcValueUOM, ipcQtyUOM, OUTPUT dValueInQtyUOM, OUTPUT oplError, OUTPUT opcMessage).
        opdValueExtended = ipdQty * dValueInQtyUOM.
    END.

    
    
END PROCEDURE.

PROCEDURE Conv_CalcTotalPrice:
    /*------------------------------------------------------------------------------
     Purpose: Embeds the "switch" to use new itemUOM and returns the total price
     Notes:  deprecates need for oe/ordltot3.i and oe/ordltot.i
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQtyInEA AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdPricePerUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcPriceUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdDiscount AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCaseCountOverride AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTotalPrice AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dTotalPrice AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dPricePerEA AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
    
    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ ipcCompany
        AND itemfg.i-no EQ ipcItemID
        NO-ERROR.
        
    IF fUseItemUOM(ipcCompany) AND AVAILABLE itemfg THEN 
    DO:
        IF CAN-DO("CS,CAS", ipcPriceUOM) AND ipdCaseCountOverride NE 0 THEN 
            ASSIGN 
                dPricePerEA = ipdPricePerUOM / ipdCaseCountOverride      
                dTotalPrice = ipdQtyInEA * dPricePerEA
                .
        ELSE
            RUN Conv_CalcExtendedValueForItem(ROWID(itemfg), ipdQtyInEA, "EA", ipdPricePerUOM, ipcPriceUOM, OUTPUT dTotalPrice, OUTPUT lError, OUTPUT cMessage).
        IF lError OR dTotalPrice EQ 0 THEN 
            dTotalPrice = ipdPricePerUOM * ipdQtyInEA.
    END.        
    ELSE 
    DO:      
        CASE ipcPriceUOM:
            WHEN "L" OR 
            WHEN "LOT" THEN 
                dTotalPrice = ipdPricePerUOM.
            WHEN "CS" OR 
            WHEN "CAS" THEN 
                DO: 
                    dPricePerEA = IF ipdCaseCountOverride NE 0 THEN ipdPricePerUOM / ipdCaseCountOverride 
                    ELSE IF AVAILABLE itemfg AND itemfg.case-count NE 0 THEN ipdPricePerUOM / itemfg.case-count
                    ELSE ipdPricePerUOM.
                    dTotalPrice = ipdQtyInEA * dPricePerEA.
                END. 
            WHEN "C" THEN
                dTotalPrice =  ipdQtyInEA * ipdPricePerUOM / 100.
            WHEN "M" THEN
                dTotalPrice = ipdQtyInEA * ipdPricePerUOM / 1000.
            OTHERWISE 
            dTotalPrice = ipdQtyInEA * ipdPricePerUOM.
        END CASE.
    END.
    opdTotalPrice = ROUND(dTotalPrice * (1 - ipdDiscount / 100), 2).


END PROCEDURE.

PROCEDURE Conv_GetValidCostUOMs:
    /*------------------------------------------------------------------------------
     Purpose:  Return a comma separated list of valid UOMs for Cost - not item 
     specific
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcValidUOMs AS CHARACTER NO-UNDO.    
    
    RUN pBuildBaseUOMs.
    RUN pGetValidUoms("Cost", OUTPUT opcValidUOMs).
      
END PROCEDURE.

PROCEDURE Conv_GetValidCostUOMsForItem:
    /*------------------------------------------------------------------------------
     Purpose:  Given an item, return a comma separated list of valid UOMs for Cost
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItem AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER opcValidUOMs AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    RUN pGetValidUOMsForItem(ipriItem, "Cost", OUTPUT opcValidUOMs, OUTPUT oplError, OUTPUT opcMessage).

END PROCEDURE.

PROCEDURE Conv_GetValidPOQtyUOMs:
    /*------------------------------------------------------------------------------
     Purpose:  Return a comma separated list of valid UOMs for PO Qty - not item 
     specific
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcValidUOMs AS CHARACTER NO-UNDO.    
    
    RUN pBuildBaseUOMs.
    RUN pGetValidUoms("POQty", OUTPUT opcValidUOMs).
      
END PROCEDURE.

PROCEDURE Conv_GetValidPOQtyUOMsForItem:
    /*------------------------------------------------------------------------------
     Purpose:  Given an item, return a comma separated list of valid UOMs for PO Qty
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItem AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER opcValidUOMs AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    RUN pGetValidUOMsForItem(ipriItem, "POQty", OUTPUT opcValidUOMs, OUTPUT oplError, OUTPUT opcMessage).

END PROCEDURE.

PROCEDURE Conv_GetValidPriceUOMs:
    /*------------------------------------------------------------------------------
     Purpose:  Return a comma separated list of valid UOMs for Price - not item 
     specific
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcValidUOMs AS CHARACTER NO-UNDO.    
    
    RUN pBuildBaseUOMs.
    RUN pGetValidUoms("Price", OUTPUT opcValidUOMs).
      
END PROCEDURE.

PROCEDURE Conv_GetValidPriceUOMsForItem:
    /*------------------------------------------------------------------------------
     Purpose:  Given an item, return a comma separated list of valid UOMs for Price
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItem AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER opcValidUOMs AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    RUN pGetValidUOMsForItem(ipriItem, "Price", OUTPUT opcValidUOMs, OUTPUT oplError, OUTPUT opcMessage).

END PROCEDURE.

PROCEDURE Conv_GetValidOrderQtyUOMs:
    /*------------------------------------------------------------------------------
     Purpose:  Return a comma separated list of valid UOMs for OrderQty - not item 
     specific
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcValidUOMs AS CHARACTER NO-UNDO.    
    
    RUN pBuildBaseUOMs.
    RUN pGetValidUoms("OrderQty", OUTPUT opcValidUOMs).
      
END PROCEDURE.

PROCEDURE Conv_GetValidOrderQtyUOMsForItem:
    /*------------------------------------------------------------------------------
     Purpose:  Given an item, return a comma separated list of valid UOMs for OrderQty
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItem AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER opcValidUOMs AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    RUN pGetValidUOMsForItem(ipriItem, "OrderQty", OUTPUT opcValidUOMs, OUTPUT oplError, OUTPUT opcMessage).
      
END PROCEDURE.

PROCEDURE Conv_QtyToEA:
    /*------------------------------------------------------------------------------
     Purpose:  Converts Qty to EA, includes check for ItemUOM "switch" 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQtyInUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQtyUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCountOverride AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQtyInEA AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dMultiplier AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQtyInEA    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-itemfg FOR itemfg.
        
    lError = YES.
    IF fUseItemUom(ipcCompany) THEN 
    DO:
        FIND FIRST bf-itemfg NO-LOCK 
            WHERE bf-itemfg.company EQ ipcCompany
            AND bf-itemfg.i-no EQ ipcItemID
            NO-ERROR.
        IF AVAILABLE bf-itemfg THEN 
            RUN Conv_QuantityFromUOMToUOMForItem(ROWID(bf-itemfg), ipdQtyInUOM, ipcQtyUOM, "EA", OUTPUT dQtyInEA, OUTPUT lError, OUTPUT cMessage).        
    END. 
    IF lError THEN 
    DO:
        CASE ipcQtyUOM:
            WHEN "CS" THEN 
                dMultiplier = ipdCountOverride. 
            WHEN "PLT" THEN 
                dMultiplier = ipdCountOverride. /*refactor?*/
            WHEN "C" THEN 
                dMultiplier = 100.  /*vestige of old logic. refactor to not hardcode?*/
            OTHERWISE 
            DO:
                FIND FIRST uom NO-LOCK 
                    WHERE uom.uom EQ ipcQtyUOM NO-ERROR.
                IF AVAILABLE uom AND uom.mult NE 0 AND uom.Other EQ "EA" THEN
                    dMultiplier = uom.mult.
                ELSE 
                    dMultiplier = 1.
            END.
        END CASE.
        dQtyinEA =  ipdQtyInUOM * dMultiplier.
    END.
    opdQtyInEA = dQtyInEa.

END PROCEDURE.

PROCEDURE Conv_QuantityFromUOMtoUOM:
    /*------------------------------------------------------------------------------
     Purpose:  Given all inputs for an item, run a conversion of quantity from one
     value to another
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQtyInFromUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcFromUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcToUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdOverrideBasisWeightInLbsPerMSF AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdOverrideDimLengthInInches AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdOverrideDimWidthInInches AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdOverrideDimDepthInInches AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdOverrideCount AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQtyInToUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-itemfg FOR itemfg.
    DEFINE BUFFER bf-item FOR item.
    
    DEFINE VARIABLE dMultiplier AS DECIMAL NO-UNDO.
    
    RUN pGetBuffersByValues(ipcCompany, ipcItemID, ipcItemType, BUFFER bf-itemfg, BUFFER bf-item).
    
    IF AVAILABLE bf-itemfg THEN DO:
        RUN pBuildUOMs(ROWID(bf-itemfg)).
        RUN pBuildUOMsFromOverrides(ipcCompany, ipcItemType, 
                                    ipdOverrideBasisWeightInLbsPerMSF, "LB/MSF", 
                                    ipdOverrideDimLengthInInches, ipdOverrideDimWidthInInches, ipdOverrideDimDepthInInches, "IN",
                                    ipdOverrideCount).
    END.
    ELSE IF AVAILABLE bf-item THEN DO:
        RUN pBuildUOMs(ROWID(bf-item)).
        RUN pBuildUOMsFromOverrides(ipcCompany, ipcItemType, 
                                    ipdOverrideBasisWeightInLbsPerMSF, "LB/MSF", 
                                    ipdOverrideDimLengthInInches, ipdOverrideDimWidthInInches, ipdOverrideDimDepthInInches, "IN",
                                    ipdOverrideCount).
                                
    END.
    
    RUN pGetMultiplier(ipcFromUOM, ipcToUOM, OUTPUT dMultiplier, OUTPUT oplError, OUTPUT opcMessage).
    
    IF NOT oplError THEN 
    DO:
        IF dMultiplier NE 0 THEN 
            opdQtyInToUOM = ipdQtyInFromUOM * dMultiplier.
        ELSE 
            ASSIGN 
                oplError   = YES
                opcMessage = "Multiplier for conversion is 0"
                .
    END.
    
END PROCEDURE.

PROCEDURE Conv_QuantityFromUOMToUOMForItem:
    /*------------------------------------------------------------------------------
     Purpose: Given a RowID for an item (itemfg or oe-ordl) take a UOM value and convert
     to another UOM
     Notes:
     RUN Conv_ValueFromUOMToUOMForItem(ROWID(itemfg), dOldValue, cOldUOM, cNewUOM, OUTPUT dNewValue, OUTPUT lError, OUTPUT cMessage).
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdQtyInFromUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcFromUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcToUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQtyInToUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
       
    DEFINE VARIABLE dMultiplier AS DECIMAL NO-UNDO.
    
    RUN pBuildUOMs(ipriItem).
    
    RUN pGetMultiplier(ipcFromUOM, ipcToUOM, OUTPUT dMultiplier, OUTPUT oplError, OUTPUT opcMessage).
    
    IF NOT oplError THEN 
    DO:
        IF dMultiplier NE 0 THEN 
            opdQtyInToUOM = ipdQtyInFromUOM * dMultiplier.
        ELSE 
            ASSIGN 
                oplError   = YES
                opcMessage = "Multiplier for conversion is 0"
                .
    END.

END PROCEDURE.
PROCEDURE Conv_ValueFromUOMtoUOM:
    /*------------------------------------------------------------------------------
     Purpose:  Given all inputs for an item, run a conversion of quantity from one
     value to another
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdValueIn$PerFromUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcFromUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcToUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdOverrideBasisWeightInLbsPerMSF AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdOverrideDimLengthInInches AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdOverrideDimWidthInInches AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdOverrideDimDepthInInches AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdOverrideCount AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdValueIn$PerToUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-itemfg FOR itemfg.
    DEFINE BUFFER bf-item FOR item.
    
    DEFINE VARIABLE dMultiplier AS DECIMAL NO-UNDO.
    
    RUN pGetBuffersByValues(ipcCompany, ipcItemID, ipcItemType, BUFFER bf-itemfg, BUFFER bf-item).
    
    IF AVAILABLE bf-itemfg THEN DO:
        RUN pBuildUOMs(ROWID(bf-itemfg)).

    END.
    ELSE IF AVAILABLE bf-item THEN DO:
        RUN pBuildUOMs(ROWID(bf-item)).

    END.

    RUN pBuildUOMsFromOverrides(ipcCompany, ipcItemType, 
                                    ipdOverrideBasisWeightInLbsPerMSF, "LB/MSF", 
                                    ipdOverrideDimLengthInInches, ipdOverrideDimWidthInInches, ipdOverrideDimDepthInInches, "IN",
                                    ipdOverrideCount).

    RUN pGetMultiplier(ipcFromUOM, ipcToUOM, OUTPUT dMultiplier, OUTPUT oplError, OUTPUT opcMessage).
    
    IF NOT oplError THEN 
    DO:
        IF dMultiplier NE 0 THEN 
            opdValueIn$PerToUOM = ipdValueIn$PerFromUOM / dMultiplier.
        ELSE 
            ASSIGN 
                oplError   = YES
                opcMessage = "Multiplier for conversion is 0"
                .
    END.
    
END PROCEDURE.

PROCEDURE Conv_ValueFromUOMToUOMForItem:
    /*------------------------------------------------------------------------------
     Purpose: Given a RowID for an item (itemfg or oe-ordl) take a UOM value and convert
     to another UOM
     Notes:
     RUN Conv_ValueFromUOMToUOMForItem(ROWID(itemfg), dOldValue, cOldUOM, cNewUOM, OUTPUT dNewValue, OUTPUT lError, OUTPUT cMessage).
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdValueIn$PerFromUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcConvert$PerFromUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcConvert$PerToUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdValueIn$PerToUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
       
    DEFINE VARIABLE dMultiplier AS DECIMAL NO-UNDO.
    
    RUN pBuildUOMs(ipriItem).
    
    RUN pGetMultiplier(ipcConvert$PerFromUOM, ipcConvert$PerToUOM, OUTPUT dMultiplier, OUTPUT oplError, OUTPUT opcMessage).
    
    IF NOT oplError THEN 
    DO:
        IF dMultiplier NE 0 THEN 
            opdValueIn$PerToUOM = ipdValueIn$PerFromUOM / dMultiplier.
        ELSE 
            ASSIGN 
                oplError   = YES
                opcMessage = "Multiplier for conversion is 0"
                .
    END.

END PROCEDURE.

PROCEDURE pAddUOM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds a UOM and conditionally override matching UOM that already exists
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplOverride AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcUOMBase AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUOMDesc AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdMultiplier AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcSource AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPurposes AS CHARACTER NO-UNDO.

    FIND FIRST ttUOM EXCLUSIVE-LOCK 
        WHERE ttUOM.uom EQ ipcUOM
        AND NOT ttUOM.isOverridden
        NO-ERROR.
    IF AVAILABLE ttUOM THEN 
        ttUOM.isOverridden = iplOverride.
        
    CREATE ttUOM.
    ASSIGN 
        ttUOM.uom                 = ipcUOM
        ttUOM.uomBase             = ipcUOMBase
        ttUOM.uomDescription      = ipcUOMDesc
        ttUOM.multiplierToBase    = ipdMultiplier
        ttUOM.uomSource           = ipcSource
        ttUOM.canUseCostPerUnit   = CAN-DO(ipcPurposes,"Cost")
        ttUOM.canUseOrderQuantity = CAN-DO(ipcPurposes,"OrderQty")
        ttUOM.canUsePOQuantity    = CAN-DO(ipcPurposes,"POQty")
        ttUOM.canUsePricePerUnit  = CAN-DO(ipcPurposes,"Price")
        ttUOM.canUseStockQuantity = CAN-DO(ipcPurposes,"Stock")
        ttUOM.isOverridden        = NOT iplOverride
            
        .
END PROCEDURE.

PROCEDURE pAddUOMsFromItemUOM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a company, item and item type, add active UOMs to the ttUOM table.
     This will override any ttUOMs that have a matching UOM on hard-coded list and uom table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplOverride AS LOGICAL NO-UNDO.

    DEFINE VARIABLE cPurposes AS CHARACTER NO-UNDO.
    FOR EACH itemUoM NO-LOCK 
        WHERE itemUoM.company EQ ipcCompany
        AND itemUoM.itemID EQ ipcItemID
        AND itemUoM.itemType EQ ipcItemType
        AND NOT itemUoM.inactive:

        cPurposes = IF itemUoM.canPurchase THEN "Price,Cost," ELSE "".
        cPurposes = cPurposes + IF itemUoM.canSell THEN "OrderQty,POQty," ELSE "".
        cPurposes = TRIM(cPurposes,",").

        RUN pAddUOM(itemUoM.uom, iplOverride, itemUoM.uomBase,itemUOM.descr, itemUoM.convFactor, "Item UOM", cPurposes).

    END.  /*Each ttItem UOM*/
    
END PROCEDURE.

PROCEDURE pBuildUOMS PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a RowID, build the UOMs for the item.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItem AS ROWID NO-UNDO.

    DEFINE BUFFER bf-itemfg FOR itemfg.
    DEFINE BUFFER bf-item   FOR ITEM.
    
    RUN pGetBuffersByRowid(ipriItem, BUFFER bf-itemfg, BUFFER bf-item).
    RUN pBuildBaseUOMs.    
    IF AVAILABLE bf-itemfg AND fUseItemUOM(bf-itemfg.company) THEN 
        RUN pBuildUOMsForItemFG(BUFFER bf-itemfg).           
    ELSE IF AVAILABLE bf-item AND fUseItemUOM(bf-item.company) THEN 
            RUN pBuildUOMsForItemRM(BUFFER bf-item).

END PROCEDURE.

PROCEDURE pBuildUOMsForItemFG PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an itemfg buffer, build the UOM temp-table, including those
        uoms from the item master
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-itemfg FOR itemfg.
    
    DEFINE VARIABLE cSourceItemMaster AS CHARACTER NO-UNDO INITIAL "Item Master".
    
    IF AVAILABLE ipbf-itemfg THEN 
    DO:
        /*Add UOMs from itemfg Master*/
        IF ipbf-itemfg.case-count NE 0 THEN 
        DO:
            RUN pAddUOM("CS", YES, "EA","Case", ipbf-itemfg.case-count, cSourceItemMaster, "Price,OrderQty,POQty,Cost").
            RUN pAddUOM("BDL", YES, "EA","Case", ipbf-itemfg.case-count, cSourceItemMaster, "Price,OrderQty").
            RUN pAddUOM("MSF", YES, "EA", "Thousand Square Feet", 1000 / MAX(ipbf-itemfg.t-sqft, 1), cSourceItemMaster, "Cost").
            RUN pAddUOM("SF", YES, "EA", "Square Feet", 1 / MAX(ipbf-itemfg.t-sqft, 1), cSourceItemMaster, "Cost").
            RUN pAddUOM("SQIN", YES, "EA", "Square Inches", 1 / MAX(ipbf-itemfg.t-sqin, 1), cSourceItemMaster, "Cost").
        END.            
        /*Add UOMs from itemUOM table*/
        RUN pAddUOMsFromItemUOM(ipbf-itemfg.company, ipbf-itemfg.i-no, "FG", YES).
    END.
END PROCEDURE.


PROCEDURE pBuildUOMsForItemRM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an item buffer, build the UOM temp-table, including those
        uoms from the item master
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-item FOR item.
    
    
    IF AVAILABLE ipbf-item THEN 
    DO:

        /*Add UOMs from item Master*/
        
        /*Add UOMs from itemUOM table*/
        RUN pAddUOMsFromItemUOM(ipbf-item.company, ipbf-item.i-no, "RM", YES).
    END.
END PROCEDURE.

PROCEDURE pBuildBaseUOMs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Builds the ttUOM table for base units of measure.  Hard-coded 
     units are first then overridden by the uom table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPurposesForEA AS CHARACTER NO-UNDO INITIAL "Price,Cost,OrderQty,POQty,Stock" .
    DEFINE VARIABLE cSourceBase    AS CHARACTER NO-UNDO INITIAL "BaseDefault".
    DEFINE VARIABLE cSourceUOM     AS CHARACTER NO-UNDO INITIAL "UOM System Table".
     
    DEFINE VARIABLE cPurposes      AS CHARACTER NO-UNDO.
    EMPTY TEMP-TABLE ttUOM.
    
    RUN pAddUOM("EA", YES, "EA","Each", 1, cSourceBase, cPurposesForEA).
    RUN pAddUOM("M", YES, "EA","Thousand", 1000, cSourceBase, cPurposesForEA).
    RUN pAddUOM("DZ", YES, "EA","Dozen", 12, cSourceBase, cPurposesForEA).
    RUN pAddUOM("DOZ", YES, "EA","Dozen", 12, cSourceBase, cPurposesForEA).
    RUN pAddUOM("C", YES, "EA","Hundred", 100, cSourceBase, cPurposesForEA).
    RUN pAddUOM("BDL", YES, "EA","Bundle", 1, cSourceBase, cPurposesForEA).
    RUN pAddUOM("CAS", YES, "EA","Case", 1, cSourceBase, cPurposesForEA).
    RUN pAddUOM("CS", YES, "EA","Case", 1, cSourceBase, cPurposesForEA).
    RUN pAddUOM("SET", YES, "EA","Set", 1, cSourceBase, cPurposesForEA).
    RUN pAddUOM("PKG", YES, "EA","Package", 1, cSourceBase, cPurposesForEA).
    RUN pAddUOM("PLT", YES, "EA","Pallet", 1, cSourceBase, cPurposesForEA).
    RUN pAddUOM("ROL", YES, "EA","Roll", 1, cSourceBase, cPurposesForEA).
    RUN pAddUOM("ROLL", YES, "EA","Roll", 1, cSourceBase, cPurposesForEA).
    RUN pAddUOM("DRM", YES, "EA","Drum", 1, cSourceBase, cPurposesForEA).
    RUN pAddUOM("LB", YES, "EA","Pound (Weight)", 1, cSourceBase, cPurposesForEA).
    RUN pAddUOM("LOT", YES, "EA","Lot", 1, cSourceBase,"Price,Cost").
    RUN pAddUOM("L", YES, "EA","Lot", 1, cSourceBase, "Price,Cost").
    
    FOR EACH uom NO-LOCK
        WHERE uom.base-uom NE ""
        AND uom.mult NE 0:
        IF ttUOM.uomBase EQ "EA" THEN
            cPurposes = cPurposesForEA.
        ELSE 
            cPurposes = "".
            
        RUN pAddUOM(uom.uom, YES, uom.base-uom, uom.dscr, uom.mult, cSourceUOM, cPurposes).
    END.

END PROCEDURE.

PROCEDURE pBuildUOMsFromOverrides PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipdBasisWeight AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcBasisWeightUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimWidth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimDepth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcDimUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdOverrideCount AS DECIMAL NO-UNDO.
     
    DEFINE VARIABLE cSourceOverride AS CHARACTER NO-UNDO INITIAL "Override".
    DEFINE VARIABLE dAreaOfEA AS DECIMAL NO-UNDO.
    
    IF ipcItemType EQ "FG" AND ipdOverrideCount GT 0 THEN 
        RUN pAddUOM("CS", YES, "EA","Case", ipdOverrideCount, cSourceOverride, "Price,OrderQty,POQty,Cost").
    IF ipdDimLength GT 0 AND ipdDimWidth GT 0 THEN DO:
        IF ipcDimUOM EQ "IN" THEN DO:
            dAreaOfEA = ipdDimLength * ipdDimWidth.
            RUN pAddUOM("SQIN", YES, "EA","Square Inches", 1 / dAreaOfEA, cSourceOverride, "Cost").
            RUN pAddUOM("SF", YES, "EA","Square Feet", 144 / dAreaOfEA, cSourceOverride, "Cost").
            RUN pAddUOM("MSF", YES, "EA","Thousand Square Feet", 1440000 / dAreaOfEA, cSourceOverride, "Cost").
        END. /*Dimensions in Inches*/
    END. /*Non-zero dimensions*/
    
END PROCEDURE.

PROCEDURE pGetValidUOMs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Returns the valid ttUOMs for 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPurpose AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcValidUOMList AS CHARACTER NO-UNDO.

    FOR EACH ttUOM NO-LOCK 
        WHERE ((ipcPurpose EQ "Price" AND ttUOM.canUsePricePerUnit) 
        OR (ipcPurpose EQ "OrderQty" AND ttUOM.canUseOrderQuantity)
        OR (ipcPurpose EQ "POQty" AND ttUOM.canUsePOQuantity)
        OR (ipcPurpose EQ "Cost" AND ttUOM.canUseCostPerUnit)
        OR (ipcPurpose EQ "All"))
        AND NOT ttUOM.isOverridden:
    
        opcValidUOMList = opcValidUOMList + ttUOM.uom + ",". 
    END. 

    opcValidUOMList = TRIM(opcValidUOMList,","). 

END PROCEDURE.

PROCEDURE pGetBuffersByRowid PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a rowid, set key buffers
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-itemfg FOR itemfg.
    DEFINE PARAMETER BUFFER opbf-item   FOR ITEM.

    FIND FIRST oe-ordl NO-LOCK 
        WHERE ROWID(oe-ordl) EQ ipriRowid
        NO-ERROR.
    IF AVAILABLE oe-ordl AND oe-ordl.i-no NE "" THEN 
        FIND FIRST opbf-itemfg NO-LOCK
            WHERE opbf-itemfg.company EQ oe-ordl.company
            AND opbf-itemfg.i-no EQ oe-ordl.i-no    
            NO-ERROR.
    IF NOT AVAILABLE opbf-itemfg THEN 
        FIND FIRST opbf-itemfg NO-LOCK
            WHERE ROWID(opbf-itemfg) EQ ipriRowID
            NO-ERROR.
    IF NOT AVAILABLE opbf-itemfg THEN 
        FIND FIRST opbf-item NO-LOCK 
            WHERE ROWID(opbf-item) EQ ipriRowID
            NO-ERROR.
        
END PROCEDURE.

PROCEDURE pGetBuffersByValues PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a rowid, set key buffers
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-itemfg FOR itemfg.
    DEFINE PARAMETER BUFFER opbf-item   FOR ITEM.

    IF ipcItemType EQ "FG" THEN 
        FIND FIRST opbf-itemfg NO-LOCK
            WHERE opbf-itemfg.company EQ ipcCompany
            AND opbf-itemfg.i-no EQ ipcItemID    
            NO-ERROR.
    ELSE 
        FIND FIRST opbf-item NO-LOCK 
            WHERE opbf-item.company EQ ipcCompany
            AND opbf-itemfg.i-no EQ ipcItemID
            NO-ERROR.
        
END PROCEDURE.
PROCEDURE pGetMultiplier PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Get multiplier from one UOM to another
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFromUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcToUOM AS CHARACTER NO-UNDO.  
    DEFINE OUTPUT PARAMETER opdMultiplier AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-to-ttUOM   FOR ttUOM.
    DEFINE BUFFER bf-from-ttUOM FOR ttUOM.

    FIND FIRST bf-to-ttUOM NO-LOCK 
        WHERE bf-to-ttUOM.uom EQ ipcToUOM
        AND NOT bf-to-ttUOM.isOverridden
        NO-ERROR.
    IF NOT AVAILABLE bf-to-ttUOM THEN 
    DO: 
        ASSIGN 
            oplError   = YES
            opcMessage = "UOM: " + ipcToUOM + " not valid".
        RETURN.
    END.
    FIND FIRST bf-from-ttUOM NO-LOCK 
        WHERE bf-from-ttUOM.uom EQ ipcFromUOM
        AND NOT bf-from-ttUOM.isOverridden
        NO-ERROR.
    IF NOT AVAILABLE bf-from-ttUOM THEN 
    DO: 
        ASSIGN 
            oplError   = YES
            opcMessage = "UOM: " + ipcFromUOM + " not valid".
        RETURN.
    END.
    IF bf-to-ttUOM.uomBase EQ bf-from-ttUOM.uomBase THEN 
    DO:
        opdMultiplier = bf-from-ttUOM.multiplierToBase.
        IF opdMultiplier EQ 0 THEN 
            opdMultiplier = 1.
        IF bf-to-ttUOM.multiplierToBase NE 0 THEN 
            opdMultiplier = opdMultiplier / bf-to-ttUOM.multiplierToBase.     
        
    END.
    ELSE 
    DO:
    /*Find base converter to common base*/
    END.
    
END PROCEDURE.

PROCEDURE pGetValidUOMsForItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given an Item ROWID, list the valid UOMs for the given scope/purpose
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipcPurpose AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcValidUOMList AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    RUN pBuildUOMs(ipriItem).
    
    RUN pGetValidUOMs(ipcPurpose, OUTPUT opcValidUOMList).

END PROCEDURE.



/* ************************  Function Implementations ***************** */

FUNCTION Conv_IsEAUOM RETURNS LOGICAL 
    (ipcCompany AS CHARACTER, ipcItemID AS CHARACTER, ipcUOM AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Given an item and uom, return YES if the UOM is mult 1 to EA
     Notes:
    ------------------------------------------------------------------------------*/	
       
    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ ipcCompany
        AND itemfg.i-no EQ ipcItemID
        NO-ERROR.
    RUN pBuildBaseUOMs.        
    IF fUseItemUOM(ipcCompany) AND AVAILABLE itemfg THEN
        RUN pBuildUOMs(ROWID(itemfg)).
    
    FIND FIRST ttUOM NO-LOCK 
        WHERE ttUOM.uom EQ ipcUOM
        AND ttUOM.uomBase EQ "EA"
        AND ttUOM.multiplierToBase EQ 1
        AND NOT ttUOM.isOverridden
        NO-ERROR.
    
    RETURN AVAILABLE ttUOM.
    
END FUNCTION.

/*FUNCTION fConvert RETURNS DECIMAL                                                          */
/*    (ipcFromUOM AS CHARACTER , ipcToUOM AS CHARACTER,                                      */
/*    ipdBasisWeightInPoundsPerSqInch AS DECIMAL,                                            */
/*    ipdLengthInInches AS DECIMAL, ipdWidthInInches AS DECIMAL, ipdDepthInInches AS DECIMAL,*/
/*    ipdQuantityOfLotInEA AS DECIMAL, ipdQuantityOfSubUnitInEA AS DECIMAL,                  */
/*    ipdValueToConvert AS DECIMAL):                                                         */
/*------------------------------------------------------------------------------
 Purpose: Replaces all conversion programs
 Notes:  modelled after rm\convcuom.p - should be able to replace all conversion programs
------------------------------------------------------------------------------*/    

/*    DEFINE VARIABLE dValueConverted   AS DECIMAL NO-UNDO.                                                                   */
/*    DEFINE VARIABLE dSquareFootOfEach AS DECIMAL NO-UNDO.                                                                   */
/*    DEFINE VARIABLE dLengthInFeet     AS DECIMAL NO-UNDO.                                                                   */
/*                                                                                                                            */
/*    IF ipdValueToConvert EQ 0 THEN                                                                                          */
/*    DO:                                                                                                                     */
/*        RETURN dValueConverted.                                                                                             */
/*    END.                                                                                                                    */
/*                                                                                                                            */
/*    IF ipdLengthInInches EQ 0 AND                                                                                           */
/*        (LOOKUP(ipcFromUOM,"LF,LI,MLF,MLI") NE 0  OR                                                                        */
/*        LOOKUP(ipcToUOM,"LF,LI,MLF,MLI") NE 0) THEN ipdLengthInInches = 12.                                                 */
/*                                                                                                                            */
/*    IF ipdDepthInInches EQ 0 THEN ipdDepthInInches = 1.                                                                     */
/*                                                                                                                            */
/*    dSquareFootOfEach = ipdLengthInInches * ipdWidthInInches * gdMultiplierForSquareFoot.                                   */
/*    dLengthInFeet = ipdLengthInInches / 12.                                                                                 */
/*    /*Convert FromUOM To Each*/                                                                                             */
/*    CASE ipcFromUOM:                                                                                                        */
/*        WHEN "MSH" OR                                                                                                       */
/*        WHEN "M" THEN                                                                                                       */
/*            ipdValueToConvert = ipdValueToConvert / 1000.                                                                   */
/*        WHEN "MSF" THEN                                                                                                     */
/*            ipdValueToConvert = dSquareFootOfEach * ipdValueToConvert / 1000.                                               */
/*        WHEN "TON" THEN                                                                                                     */
/*            IF ipdWidthInInches NE 0 AND ipdLengthInInches NE 0 AND ipdBasisWeightInPoundsPerSqInch NE 0 THEN               */
/*                ipdValueToConvert = dSquareFootOfEach * ipdValueToConvert / 1000 * ipdBasisWeightInPoundsPerSqInch / 2000.  */
/*        WHEN "LB" THEN                                                                                                      */
/*            IF ipdWidthInInches NE 0 AND ipdLengthInInches NE 0 AND ipdBasisWeightInPoundsPerSqInch NE 0 THEN               */
/*                ipdValueToConvert = dSquareFootOfEach * ipdValueToConvert / 1000 * ipdBasisWeightInPoundsPerSqInch.         */
/*        WHEN "SF" THEN                                                                                                      */
/*            ipdValueToConvert = dSquareFootOfEach * ipdValueToConvert.                                                      */
/*        WHEN "MLF" THEN                                                                                                     */
/*            ipdValueToConvert = (dLengthInFeet * ipdValueToConvert) / 1000.                                                 */
/*        WHEN "LF" THEN                                                                                                      */
/*            ipdValueToConvert = dLengthInFeet * ipdValueToConvert.                                                          */
/*        WHEN "MLI" THEN                                                                                                     */
/*            ipdValueToConvert = (ipdLengthInInches  * ipdValueToConvert) / 1000.                                            */
/*        WHEN "LI" THEN                                                                                                      */
/*            ipdValueToConvert = ipdLengthInInches * ipdValueToConvert.                                                      */
/*        WHEN "BF" OR                                                                                                        */
/*        WHEN "BSF" THEN                                                                                                     */
/*            ipdValueToConvert = ((ipdLengthInInches * ipdWidthInInches * ipdDepthInInches) / 144) * ipdValueToConvert.      */
/*        WHEN "CAS" OR WHEN "C" THEN                                                                                         */
/*            ipdValueToConvert = ipdValueToConvert / ipdQuantityOfSubUnitInEA.                                               */
/*        WHEN "LOT" OR WHEN "L" THEN                                                                                         */
/*            ipdValueToConvert = ipdValueToConvert / ipdQuantityOfLotInEA.                                                   */
/*        OTHERWISE                                                                                                           */
/*        DO:                                                                                                                 */
/*            fromuom:                                                                                                        */
/*            REPEAT:                                                                                                         */
/*                /* put cost into an EA uom */                                                                               */
/*                FIND FIRST uom NO-LOCK                                                                                      */
/*                    WHERE uom.uom  EQ ipcFromUOM                                                                            */
/*                    AND uom.mult NE 0                                                                                       */
/*                    NO-ERROR.                                                                                               */
/*                IF AVAILABLE uom THEN                                                                                       */
/*                DO:                                                                                                         */
/*                    ipdValueToConvert = (IF ipdValueToConvert EQ 0 THEN 1 ELSE ipdValueToConvert / uom.mult).               */
/*                                                                                                                            */
/*                    IF uom.other NE "" AND uom.other NE uom.uom THEN                                                        */
/*                    DO:                                                                                                     */
/*                        ipcFromUOM = uom.other.                                                                             */
/*                        NEXT fromuom.                                                                                       */
/*                    END.                                                                                                    */
/*                END.                                                                                                        */
/*                                                                                                                            */
/*                ELSE ipdValueToConvert = (IF ipdValueToConvert EQ 0 THEN 1 ELSE ipdValueToConvert).                         */
/*                                                                                                                            */
/*                LEAVE fromuom.                                                                                              */
/*            END.                                                                                                            */
/*        END.                                                                                                                */
/*    END CASE.                                                                                                               */
/*                                                                                                                            */
/*    /*Convert from Each to ToUOM*/                                                                                          */
/*    CASE ipcToUOM:                                                                                                          */
/*        WHEN "MSH" OR                                                                                                       */
/*        WHEN "M" THEN                                                                                                       */
/*            dValueConverted = ipdValueToConvert * 1000.                                                                     */
/*        WHEN "MSF" THEN                                                                                                     */
/*            IF dSquareFootOfEach NE 0 THEN                                                                                  */
/*                dValueConverted = (1000 * ipdValueToConvert) / (dSquareFootOfEach).                                         */
/*        WHEN "TON" THEN                                                                                                     */
/*            IF dSquareFootOfEach NE 0 AND ipdBasisWeightInPoundsPerSqInch NE 0 THEN                                         */
/*                dValueConverted = (2000 * 1000 * ipdValueToConvert) / (ipdBasisWeightInPoundsPerSqInch * dSquareFootOfEach).*/
/*        WHEN "LB" THEN                                                                                                      */
/*            IF dSquareFootOfEach NE 0 AND ipdBasisWeightInPoundsPerSqInch NE 0 THEN                                         */
/*                dValueConverted = (1000 * ipdValueToConvert) / (ipdBasisWeightInPoundsPerSqInch * dSquareFootOfEach).       */
/*        WHEN "SF" THEN                                                                                                      */
/*            IF dSquareFootOfEach NE 0 THEN                                                                                  */
/*                dValueConverted = ipdValueToConvert / dSquareFootOfEach.                                                    */
/*        WHEN "MLF" THEN                                                                                                     */
/*            IF dLengthInFeet NE 0 THEN                                                                                      */
/*                dValueConverted = (1000 * ipdValueToConvert) / dLengthInFeet.                                               */
/*        WHEN "LF" THEN                                                                                                      */
/*            IF dLengthInFeet NE 0 THEN                                                                                      */
/*                dValueConverted = ipdValueToConvert / dLengthInFeet.                                                        */
/*        WHEN "MLI" THEN                                                                                                     */
/*            IF ipdLengthInInches NE 0 THEN                                                                                  */
/*                dValueConverted = (1000 * ipdValueToConvert) / ipdLengthInInches.                                           */
/*        WHEN "LI" THEN                                                                                                      */
/*            IF ipdLengthInInches NE 0 THEN                                                                                  */
/*                dValueConverted = ipdValueToConvert / ipdLengthInInches.                                                    */
/*        WHEN "BF" OR                                                                                                        */
/*        WHEN "BSF" THEN                                                                                                     */
/*            IF ipdLengthInInches NE 0 AND ipdWidthInInches NE 0 AND ipdDepthInInches NE 0 THEN                              */
/*                dValueConverted = ipdValueToConvert / ((ipdLengthInInches * ipdWidthInInches * ipdDepthInInches) / 144).    */
/*        WHEN "CAS" OR WHEN "C" THEN                                                                                         */
/*            ipdValueToConvert = ipdValueToConvert * ipdQuantityOfSubUnitInEA.                                               */
/*        WHEN "LOT" OR WHEN "L" THEN                                                                                         */
/*            ipdValueToConvert = ipdValueToConvert * ipdQuantityOfLotInEA.                                                   */
/*                                                                                                                            */
/*        OTHERWISE                                                                                                           */
/*        DO:                                                                                                                 */
/*            touom:                                                                                                          */
/*            REPEAT:                                                                                                         */
/*                FIND FIRST uom NO-LOCK                                                                                      */
/*                    WHERE uom.uom  EQ ipcToUOM                                                                              */
/*                    AND uom.mult NE 0                                                                                       */
/*                    NO-ERROR.                                                                                               */
/*                IF AVAILABLE uom THEN                                                                                       */
/*                DO:                                                                                                         */
/*                    ipdValueToConvert = (IF ipdValueToConvert NE 0 THEN (ipdValueToConvert * uom.mult) ELSE 0).             */
/*                    IF uom.other NE "" AND uom.other NE uom.uom THEN                                                        */
/*                    DO:                                                                                                     */
/*                        ipcToUOM = uom.other.                                                                               */
/*                        NEXT touom.                                                                                         */
/*                    END.                                                                                                    */
/*                END.                                                                                                        */
/*                                                                                                                            */
/*                dValueConverted = ipdValueToConvert.                                                                        */
/*                                                                                                                            */
/*                LEAVE touom.                                                                                                */
/*            END.                                                                                                            */
/*        END.                                                                                                                */
/*    END CASE.                                                                                                               */
/*                                                                                                                            */
/*    RETURN dValueConverted.                                                                                                 */
    
/*END FUNCTION.*/

/*FUNCTION fConvertValueToEAFromUOM RETURNS DECIMAL                                                                                                               */
/*    ( ipcCompany AS CHARACTER, ipcItemType AS CHARACTER, ipcItemID AS CHARACTER, ipdValueToConvert AS DECIMAL, ipcValueToConvertUom AS CHARACTER):              */
/*                                                                                                                                                                */
/*    /*------------------------------------------------------------------------------                                                                            */
/*     Purpose: Given an item, quantity and UOM, return the quantity in EA UOM                                                                                    */
/*     Notes:  Assumes EA UOM is EA                                                                                                                               */
/*    ------------------------------------------------------------------------------*/                                                                            */
/*                                                                                                                                                                */
/*    DEFINE VARIABLE opdValueConverted AS DECIMAL   NO-UNDO.                                                                                                     */
/*    DEFINE VARIABLE lError            AS LOGICAL   NO-UNDO.                                                                                                     */
/*    DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO.                                                                                                     */
/*    DEFINE VARIABLE dConversionFactor AS DECIMAL   NO-UNDO.                                                                                                     */
/*                                                                                                                                                                */
/*    RUN pGetConversionFactorFromItemUOM(ipcCompany, ipcItemID, ipcItemType, "", ipcValueToConvertUOM, OUTPUT dConversionFactor, OUTPUT lError, OUTPUT cMessage).*/
/*                                                                                                                                                                */
/*    IF NOT lError AND dConversionFactor NE 0 THEN                                                                                                               */
/*        opdValueConverted = ipdValueToConvert * dConversionFactor.                                                                                              */
/*    ELSE                                                                                                                                                        */
/*        opdValueConverted = ipdValueToConvert.                                                                                                                  */
/*                                                                                                                                                                */
/*    RETURN opdValueConverted.                                                                                                                                   */
/*                                                                                                                                                                */
/*END FUNCTION.                                                                                                                                                   */
/*                                                                                                                                                                */
/*FUNCTION fConvertValueFromEAToUOM RETURNS DECIMAL                                                                                                               */
/*    ( ipcCompany AS CHARACTER, ipcItemType AS CHARACTER, ipcItemID AS CHARACTER, ipdValueInEA AS DECIMAL, ipcValueToConvertUom AS CHARACTER):                   */
/*                                                                                                                                                                */
/*    /*------------------------------------------------------------------------------                                                                            */
/*     Purpose: Given an item, quantity and UOM, return the quantity in EA UOM                                                                                    */
/*     Notes:  Assumes EA UOM is EA                                                                                                                               */
/*    ------------------------------------------------------------------------------*/                                                                            */
/*                                                                                                                                                                */
/*    DEFINE VARIABLE opdValueConverted AS DECIMAL   NO-UNDO.                                                                                                     */
/*    DEFINE VARIABLE lError            AS LOGICAL   NO-UNDO.                                                                                                     */
/*    DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO.                                                                                                     */
/*    DEFINE VARIABLE dConversionFactor AS DECIMAL   NO-UNDO.                                                                                                     */
/*                                                                                                                                                                */
/*    RUN pGetConversionFactorFromItemUOM(ipcCompany, ipcItemID, ipcItemType, "", ipcValueToConvertUOM, OUTPUT dConversionFactor, OUTPUT lError, OUTPUT cMessage).*/
/*                                                                                                                                                                */
/*    IF NOT lError AND dConversionFactor NE 0 THEN                                                                                                               */
/*        opdValueConverted = ipdValueInEA / dConversionFactor.                                                                                                   */
/*    ELSE                                                                                                                                                        */
/*        opdValueConverted = ipdValueInEA.                                                                                                                       */
/*                                                                                                                                                                */
/*    RETURN opdValueConverted.                                                                                                                                   */
/*                                                                                                                                                                */
/*END FUNCTION.                                                                                                                                                   */

FUNCTION fUseItemUOM RETURNS LOGICAL PRIVATE
    ( ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Returns setting for FGItemUOM
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cReturn     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lUseItemUOM AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "FGItemUoM", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN ASSIGN 
            lUseItemUoM = LOGICAL(cReturn) NO-ERROR.
	
    RETURN lUseItemUOM.
	
END FUNCTION.
