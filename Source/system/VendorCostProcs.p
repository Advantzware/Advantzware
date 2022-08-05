
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
{system/VendorCostProcs.i}
{po/ttVendCostReport.i}
{oe/ttPriceHold.i}
{util/ttInactiveQuotes.i}
{est/ttEstimateQuantity.i} 

/*Constants*/
DEFINE VARIABLE gcItemTypeFG       AS CHARACTER NO-UNDO INITIAL "FG".
DEFINE VARIABLE gcItemTypeRM       AS CHARACTER NO-UNDO INITIAL "RM".
DEFINE VARIABLE gdQuantityOffset   AS DECIMAL   NO-UNDO INITIAL 0.000001.
DEFINE VARIABLE gdQuantityMax      AS DECIMAL   NO-UNDO INITIAL 9999999.99.
DEFINE VARIABLE gdLengthWidthMax   AS DECIMAL   NO-UNDO INITIAL 99999.9999.
DEFINE VARIABLE gcScopeAll         AS CHARACTER NO-UNDO INITIAL "All".
DEFINE VARIABLE gcScopeEffective   AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired".
DEFINE VARIABLE gcScopeRMStandard  AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - RM Standard".
DEFINE VARIABLE gcScopeRMOverride  AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - RM Override".
DEFINE VARIABLE gcScopeFGEstimated AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - FG Estimated".
DEFINE VARIABLE gcScopeFGPurchased AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - FG Purchased".
DEFINE VARIABLE gcScopeList        AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcScopeDefault     AS CHARACTER NO-UNDO.

/*Settings Variables*/
DEFINE VARIABLE gcAutoSetMaximum   AS CHARACTER NO-UNDO.

DEFINE VARIABLE cAdderLExt AS CHARACTER EXTENT 6 NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION fGetSettingAutoSetMaximum RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER) FORWARD.

FUNCTION VendCost_GetUnlimitedQuantity RETURNS DECIMAL 
    (  ) FORWARD.

FUNCTION VendCost_GetValidScopes RETURNS CHARACTER 
    (ipcContext AS CHARACTER) FORWARD.
    
FUNCTION fVendCostHasEstimateOverride RETURNS LOGICAL 
    (ipcCompany AS CHARACTER,
    ipcEstimateID AS CHARACTER,
    ipiFormNo AS INTEGER,
    ipcItemID AS CHARACTER) FORWARD.    



/* ***************************  Main Block  *************************** */
ASSIGN 
    gcScopeList    = gcScopeAll + "," + gcScopeEffective + "," + gcScopeRMStandard + "," + gcScopeRMOverride + "," + gcScopeFGEstimated + "," + gcScopeFGPurchased
    gcScopeDefault = gcScopeEffective
    .

/* **********************  Internal Procedures  *********************** */

/*
PROCEDURE BuildVendItemCosts:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcScope AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeBlankVendor AS LOGICAL NO-UNDO.
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
    DEFINE INPUT PARAMETER ipcAdderList AS CHARACTER EXTENT 6 NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttVendItemCost.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lResetTemptable AS LOGICAL   NO-UNDO INIT YES.
    DEFINE VARIABLE iCount          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cAdderItem      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCost           AS DECIMAL   NO-UNDO.
    DEFINE BUFFER buf-ttVendItemCost FOR ttVendItemCost.
    
    EMPTY TEMP-TABLE ttVendItemCost.
    /*create data for primary item*/
    RUN pBuildVendItemCosts(
        INPUT  ipcCompany,
        INPUT  ipcItemID,
        INPUT  ipcItemType,
        INPUT  ipcScope,
        INPUT  iplIncludeBlankVendor,
        INPUT  ipcEstimateNo,
        INPUT  ipiFormNo,
        INPUT  ipiBlankNo,
        INPUT  ipdQuantity,
        INPUT  ipcQuantityUOM,
        INPUT  ipdDimLength,
        INPUT  ipdDimWidth,
        INPUT  ipdDimDepth,
        INPUT  ipcDimUOM,
        INPUT  ipdBasisWeight,
        INPUT  ipcBasisWeightUOM,
        INPUT  lResetTemptable,
        OUTPUT  TABLE  ttVendItemCost,
        OUTPUT  oplError,
        OUTPUT  opcMessage).
        /*create ttVenditem for adders*/
    DO iCount = 1 TO EXTENT(ipcAdderList):
        IF ipcAdderList[iCount] <> "" THEN
        DO:
            lResetTemptable = NO.
            RUN pBuildVendItemCosts(
                INPUT  ipcCompany,
                INPUT  ipcAdderList[iCount],
                INPUT  ipcItemType,
                INPUT  ipcScope,
                INPUT  iplIncludeBlankVendor,
                INPUT  ipcEstimateNo,
                INPUT  ipiFormNo,
                INPUT  ipiBlankNo,
                INPUT  ipdQuantity,
                INPUT  ipcQuantityUOM,
                INPUT  ipdDimLength,
                INPUT  ipdDimWidth,
                INPUT  ipdDimDepth,
                INPUT  ipcDimUOM,
                INPUT  ipdBasisWeight,
                INPUT  ipcBasisWeightUOM,
                INPUT  lResetTemptable,
                OUTPUT  TABLE  ttVendItemCost,
                OUTPUT  oplError,
                OUTPUT  opcMessage).
        END.
    END.
    
    IF NOT lResetTemptable THEN
    DO: 
        VendItemCostBlk:
        FOR EACH ttVendItemCost WHERE ttVendItemCost.isValid AND ttVendItemCost.ItemID = ipcItemID:
            dCost = 0.
            DO iCount = 1 TO EXTENT(ipcAdderList):
                IF ipcAdderList[iCount] <> "" THEN
                DO:
                    FIND FIRST venditemcost WHERE vendItemCost.company EQ ipcCompany 
                        AND venditemcost.itemtype = ipcItemType AND venditemcost.itemid = ipcAdderList[iCount]
                        AND venditemcost.vendorid = ttVendItemCost.vendorid NO-LOCK NO-ERROR.
                    IF AVAILABLE venditemcost THEN
                    DO:
                        FOR FIRST buf-ttVendItemCost 
                            WHERE buf-ttVendItemCost.isValid 
                              AND buf-ttVendItemCost.VendorID = venditemcost.vendorid
                              AND buf-ttVendItemCost.ItemID = venditemcost.itemid:
                            dCost = dCost + buf-ttVendItemCost.costPerVendorUOM.
                        END.                   
                    END.
                    ELSE
                    DO: 
                        ASSIGN   
                            ttVendItemCost.isValid        = NO
                            ttVendItemCost.reasonNotValid = "Vendor not available for the item " + ipcAdderList[iCount]. 
                        NEXT VendItemCostBlk.
                    END.  
                END.
            END.
            //Add the cost adder cost to the itemCost here
            ttVendItemCost.costPerVendorUOM = ttVendItemCost.costPerVendorUOM + dCost.
        END.
        /*Delete vendors related to adders as they are added to the item adders*/
        FOR EACH ttVendItemCost WHERE ttVendItemCost.ItemID <> ipcItemID:
            DELETE ttVendItemCost.
        END.
    END.
    
END PROCEDURE. */

PROCEDURE BuildVendItemCosts:
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcScope AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeBlankVendor AS LOGICAL NO-UNDO.
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
    DEFINE OUTPUT PARAMETER TABLE FOR ttVendItemCost.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cAdderList AS CHARACTER EXTENT 6 NO-UNDO.
    
    RUN pBuildVendItemCosts(
        INPUT  ipcCompany,
        INPUT  ipcItemID,
        INPUT  ipcItemType,
        INPUT  ipcScope,
        INPUT  iplIncludeBlankVendor,
        INPUT  ipcEstimateNo,
        INPUT  ipiFormNo,
        INPUT  ipiBlankNo,
        INPUT  ipdQuantity,
        INPUT  ipcQuantityUOM,
        INPUT  ipdDimLength,
        INPUT  ipdDimWidth,
        INPUT  ipdDimDepth,
        INPUT  ipcDimUOM,
        INPUT  ipdBasisWeight,
        INPUT  ipcBasisWeightUOM,
        INPUT  cAdderList,
        OUTPUT  TABLE  ttVendItemCost,
        OUTPUT  oplError,
        OUTPUT  opcMessage).
    
END PROCEDURE.

PROCEDURE BuildVendItemCostsWithAdders:
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcScope AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeBlankVendor AS LOGICAL NO-UNDO.
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
    DEFINE INPUT PARAMETER ipcAdderList AS CHARACTER EXTENT 6 NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttVendItemCost.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    RUN pBuildVendItemCosts(
        INPUT  ipcCompany,
        INPUT  ipcItemID,
        INPUT  ipcItemType,
        INPUT  ipcScope,
        INPUT  iplIncludeBlankVendor,
        INPUT  ipcEstimateNo,
        INPUT  ipiFormNo,
        INPUT  ipiBlankNo,
        INPUT  ipdQuantity,
        INPUT  ipcQuantityUOM,
        INPUT  ipdDimLength,
        INPUT  ipdDimWidth,
        INPUT  ipdDimDepth,
        INPUT  ipcDimUOM,
        INPUT  ipdBasisWeight,
        INPUT  ipcBasisWeightUOM,
        INPUT  ipcAdderList,
        OUTPUT  TABLE  ttVendItemCost,
        OUTPUT  oplError,
        OUTPUT  opcMessage).
    
END PROCEDURE.

PROCEDURE GetNextPriceBreak:
    /*------------------------------------------------------------------------------
     Purpose: Given company, item and item properties, it calculate the next price
              break for quantity
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttEstimateQuantity.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdformlength  AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdformwidth  AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdformdepth  AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER iprowidEF     AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipinumup      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendorID   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdNextQuantity AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    DEFINE VARIABLE cVendCostUOM AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iN-out AS INTEGER NO-UNDO.
    
    RUN est/ef-#out.p (iprowidEF, OUTPUT iN-out).
    FIND FIRST item NO-LOCK 
          WHERE ITEM.company EQ ipcCompany
            AND item.i-no EQ ipcItemID NO-ERROR.
    
    FOR EACH vendItemCost NO-LOCK
        WHERE vendItemCost.company  EQ ipcCompany
        AND vendItemCost.itemType EQ ipcItemType
        AND vendItemCost.itemID EQ ipcItemID
        AND (vendItemCost.vendorID EQ ipcVendorID OR ipcVendorID EQ "")
        AND (ipcItemType EQ "FG" AND (vendItemCost.estimateNo   EQ ipcEstNo
        AND vendItemCost.formNo  EQ ipiFormNo
        AND vendItemCost.blankNo EQ ipiBlankNo)
        OR (ipcItemType EQ "RM" AND vendItemCost.estimateNo EQ ""))
        BREAK BY vendItemCost.vendorID:
        IF FIRST(vendItemCost.vendorID) THEN 
        DO:    
            iIndex = 0.
            FOR EACH vendItemCostLevel NO-LOCK WHERE vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostId
                BY vendItemCostLevel.quantityBase:
                cVendCostUOM = IF vendItemCost.vendorUOM <> "" THEN vendItemCost.vendorUOM ELSE "EA".
                iIndex = iIndex + 1.
                IF iIndex LE 20 AND ttEstimateQuantity.EstQuantity[iIndex] GT 0 THEN 
                DO: 
                    RUN sys/ref/convquom.p(cVendCostUOM, "EA", (IF AVAILABLE ITEM THEN item.basis-w ELSE 0),
                        ipdformlength, ipdformwidth, ipdformdepth,
                        vendItemCostLevel.quantityBase, OUTPUT opdNextQuantity).
                    opdNextQuantity = opdNextQuantity * ipinumup * iN-out.                     
                    opdNextQuantity = IF opdNextQuantity - INT64(opdNextQuantity) GT 0 THEN (INT64 (opdNextQuantity) + 1)
                                      ELSE INT64 (opdNextQuantity).         
                    IF opdNextQuantity GT ttEstimateQuantity.EstQuantity[iIndex] THEN  
                        ASSIGN ttEstimateQuantity.EstNextQuantity[iIndex] = opdNextQuantity. 
                END.                  
            END.
        END. 
    END.

END PROCEDURE.

PROCEDURE pBuildVendItemCosts PRIVATE:
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
    DEFINE INPUT PARAMETER ipcAdderList AS CHARACTER EXTENT 6 NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttVendItemCost.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-vendItemCost   FOR vendItemCost.
    DEFINE BUFFER bf-ttVendItemCost FOR ttVendItemCost.
    
    DEFINE VARIABLE lAvailAdder    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iCount         AS INTEGER NO-UNDO.
    DEFINE VARIABLE lAvailVendItem AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dCost          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotalCost     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cVendorCostMatrixUseEstimate AS CHARACTER NO-UNDO.
    
    RUN spGetSettingByName ("VendorCostMatrixUseEstimate", OUTPUT cVendorCostMatrixUseEstimate).
    
    DO iCount = 1 TO EXTENT(ipcAdderList):
        IF ipcAdderList[iCount] <> "" THEN 
        DO:
            lAvailAdder = YES.
            LEAVE.
        END.
    END.    
    
    EMPTY TEMP-TABLE ttVendItemCost.    
    IF LOOKUP(ipcScope,gcScopeList) EQ 0 THEN 
        ipcScope = gcScopeDefault.

    CASE ipcScope:
        WHEN gcScopeAll THEN  
            DO:  /*All*/
                FOR EACH bf-vendItemCost NO-LOCK
                    WHERE bf-vendItemCost.company EQ ipcCompany
                    AND bf-vendItemCost.itemID EQ ipcItemID
                    AND bf-vendItemCost.itemType EQ ipcItemType
                    AND (bf-vendItemCost.vendorID NE "" OR iplIncludeBlankVendor)
                    :
                    RUN pAddTTVendItemCost(BUFFER bf-vendItemCost, ipdQuantity, ipcQuantityUOM, ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM, 
                        ipdBasisWeight, ipcBasisWeightUOM, OUTPUT oplError, INPUT-OUTPUT opcMessage).    
                END.
            END.  /*All*/
        WHEN gcScopeEffective THEN 
            DO:
                FOR EACH bf-vendItemCost NO-LOCK  /*All but only effective*/
                    WHERE bf-vendItemCost.company EQ ipcCompany
                    AND bf-vendItemCost.itemID EQ ipcItemID
                    AND bf-vendItemCost.itemType EQ ipcItemType
                    AND bf-vendItemCost.effectiveDate LE TODAY
                    AND (bf-vendItemCost.expirationDate GE TODAY OR bf-vendItemCost.expirationDate EQ ? OR bf-vendItemCost.expirationDate EQ 01/01/0001)
                    AND (bf-vendItemCost.vendorID NE "" OR iplIncludeBlankVendor)
                    :
                    RUN pAddTTVendItemCost(BUFFER bf-vendItemCost, ipdQuantity, ipcQuantityUOM, ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM, 
                        ipdBasisWeight, ipcBasisWeightUOM, OUTPUT oplError, INPUT-OUTPUT opcMessage).   
                END.
            END.
        WHEN gcScopeRMStandard THEN 
            DO:  /*RM Standard*/
                FOR EACH bf-vendItemCost NO-LOCK  
                    WHERE bf-vendItemCost.company EQ ipcCompany
                    AND bf-vendItemCost.itemID EQ ipcItemID
                    AND bf-vendItemCost.itemType EQ ipcItemType
                    AND (bf-vendItemCost.estimateNo EQ "" OR cVendorCostMatrixUseEstimate EQ "No")
                    AND bf-vendItemCost.effectiveDate LE TODAY
                    AND (bf-vendItemCost.expirationDate GE TODAY OR bf-vendItemCost.expirationDate EQ ? OR bf-vendItemCost.expirationDate EQ 01/01/0001)
                    AND (bf-vendItemCost.vendorID NE "" OR iplIncludeBlankVendor)
                    :
                    RUN pAddTTVendItemCost(BUFFER bf-vendItemCost, ipdQuantity, ipcQuantityUOM, ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM, 
                        ipdBasisWeight, ipcBasisWeightUOM, OUTPUT oplError, INPUT-OUTPUT opcMessage).   
                END.
            END. /*RM Standard*/
        WHEN gcScopeRMOverride OR 
        WHEN gcScopeFGEstimated THEN 
            DO:  /*RM Estimate Override*/

                FOR EACH bf-vendItemCost NO-LOCK  
                    WHERE bf-vendItemCost.company EQ ipcCompany
                    AND bf-vendItemCost.itemID EQ ipcItemID
                    AND bf-vendItemCost.itemType EQ ipcItemType
                    AND (bf-vendItemCost.estimateNo EQ ipcEstimateNo OR cVendorCostMatrixUseEstimate EQ "No")
                    AND (bf-vendItemCost.formNo EQ ipiFormNo OR cVendorCostMatrixUseEstimate EQ "No")
                    AND (bf-vendItemCost.blankNo EQ ipiBlankNo OR cVendorCostMatrixUseEstimate EQ "No")
                    AND bf-vendItemCost.effectiveDate LE TODAY
                    AND (bf-vendItemCost.expirationDate GE TODAY OR bf-vendItemCost.expirationDate EQ ? OR bf-vendItemCost.expirationDate EQ 01/01/0001)
                    AND (bf-vendItemCost.vendorID NE "" OR iplIncludeBlankVendor)
                    :

                        RUN pAddTTVendItemCost(BUFFER bf-vendItemCost, ipdQuantity, ipcQuantityUOM, ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM, 
                                ipdBasisWeight, ipcBasisWeightUOM, OUTPUT oplError, INPUT-OUTPUT opcMessage). 
                END.
                IF NOT CAN-FIND(FIRST ttVendItemCost) AND ipcItemID NE "" THEN  /*Find with out estimate*/ 
                    FOR EACH bf-vendItemCost NO-LOCK  
                        WHERE bf-vendItemCost.company EQ ipcCompany
                        AND bf-vendItemCost.itemID EQ ipcItemID
                        AND bf-vendItemCost.itemType EQ ipcItemType
                        AND (bf-vendItemCost.estimateNo EQ "" OR cVendorCostMatrixUseEstimate EQ "No")
                        AND bf-vendItemCost.effectiveDate LE TODAY
                        AND (bf-vendItemCost.expirationDate GE TODAY OR bf-vendItemCost.expirationDate EQ ? OR bf-vendItemCost.expirationDate EQ 01/01/0001)
                        AND (bf-vendItemCost.vendorID NE "" OR iplIncludeBlankVendor)
                        :
                                RUN pAddTTVendItemCost(BUFFER bf-vendItemCost, ipdQuantity, ipcQuantityUOM, ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM, 
                                    ipdBasisWeight, ipcBasisWeightUOM, OUTPUT oplError, INPUT-OUTPUT opcMessage).  
                    END.
            END. /*RM Estimated Override*/    
       
    END CASE.
    /*add adder items to ttVendItem*/
    IF lAvailAdder THEN 
    DO:
        DO iCount = 1 TO EXTENT(ipcAdderList):
            IF ipcAdderList[iCount] <> "" THEN 
            DO:    
                FOR EACH bf-vendItemCost NO-LOCK  
                    WHERE bf-vendItemCost.company EQ ipcCompany
                    AND bf-vendItemCost.itemID EQ ipcAdderList[iCount]
                    AND bf-vendItemCost.itemType EQ ipcItemType
                    AND (bf-vendItemCost.estimateNo EQ ipcEstimateNo OR cVendorCostMatrixUseEstimate EQ "No")
                    AND (bf-vendItemCost.formNo EQ ipiFormNo OR cVendorCostMatrixUseEstimate EQ "No")
                    AND (bf-vendItemCost.blankNo EQ ipiBlankNo OR cVendorCostMatrixUseEstimate EQ "No")
                    AND bf-vendItemCost.effectiveDate LE TODAY
                    AND (bf-vendItemCost.expirationDate GE TODAY OR bf-vendItemCost.expirationDate EQ ? OR bf-vendItemCost.expirationDate EQ 01/01/0001)
                    AND (bf-vendItemCost.vendorID NE "" OR iplIncludeBlankVendor)
                    :
                    IF NOT CAN-FIND(FIRST ttVendItemCost WHERE ttVendItemCost.VendItemCostID = bf-vendItemCost.VendItemCostID) THEN
                    DO: 
                        RUN pAddTTVendItemCost(BUFFER bf-vendItemCost, ipdQuantity, ipcQuantityUOM, ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM, 
                            ipdBasisWeight, ipcBasisWeightUOM, OUTPUT oplError, INPUT-OUTPUT opcMessage).
                        lAvailVendItem = TRUE.
                    END. 
                END.
                IF NOT lAvailVendItem THEN  /*Find with out estimate*/ 
                    FOR EACH bf-vendItemCost NO-LOCK  
                        WHERE bf-vendItemCost.company EQ ipcCompany
                        AND bf-vendItemCost.itemID EQ ipcAdderList[iCount]
                        AND bf-vendItemCost.itemType EQ ipcItemType
                        AND (bf-vendItemCost.estimateNo EQ "" OR cVendorCostMatrixUseEstimate EQ "No")
                        AND bf-vendItemCost.effectiveDate LE TODAY
                        AND (bf-vendItemCost.expirationDate GE TODAY OR bf-vendItemCost.expirationDate EQ ? OR bf-vendItemCost.expirationDate EQ 01/01/0001)
                        AND (bf-vendItemCost.vendorID NE "" OR iplIncludeBlankVendor)
                        :
                        IF NOT CAN-FIND(FIRST ttVendItemCost WHERE ttVendItemCost.VendItemCostID = bf-vendItemCost.VendItemCostID) THEN
                        RUN pAddTTVendItemCost(BUFFER bf-vendItemCost, ipdQuantity, ipcQuantityUOM, ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM, 
                            ipdBasisWeight, ipcBasisWeightUOM, OUTPUT oplError, INPUT-OUTPUT opcMessage).  
                    END.
            END. /*not blank adderlist*/
        END. /*Do iCount end*/

        VendItemCostBlk:
        FOR EACH ttVendItemCost WHERE ttVendItemCost.isValid AND ttVendItemCost.ItemID = ipcItemID:
            ASSIGN 
                dCost = 0
                dtotalCost = 0.
            DO iCount = 1 TO EXTENT(ipcAdderList):
                IF ipcAdderList[iCount] <> "" THEN
                DO:
                    FIND FIRST venditemcost WHERE vendItemCost.company EQ ipcCompany 
                        AND venditemcost.itemtype = ipcItemType AND venditemcost.itemid = ipcAdderList[iCount]
                        AND venditemcost.vendorid = ttVendItemCost.vendorid NO-LOCK NO-ERROR.
                    IF AVAILABLE venditemcost THEN
                    DO:
                        FOR FIRST bf-ttVendItemCost 
                            WHERE bf-ttVendItemCost.isValid 
                            AND bf-ttVendItemCost.VendorID = venditemcost.vendorid
                            AND bf-ttVendItemCost.ItemID = venditemcost.itemid:
                            ASSIGN 
                            dCost = dCost + bf-ttVendItemCost.costPerVendorUOM
                            dtotalCost = dtotalCost + bf-ttVendItemCost.Costtotal.
                             .
                        END.              
                    END.
                    ELSE
                    DO: 
                        ASSIGN   
                            ttVendItemCost.isValid        = NO
                            ttVendItemCost.reasonNotValid = "Vendor not available for the item " + ipcAdderList[iCount]. 
                        NEXT VendItemCostBlk.
                    END.  
                END.
            END.
            //Add the cost adder cost to the itemCost here
            ASSIGN
            ttVendItemCost.costPerVendorUOM = ttVendItemCost.costPerVendorUOM + dCost
            ttVendItemCost.Costtotal = ttVendItemCost.Costtotal + dtotalCost.
        END.

        /*Delete vendors related to adders as they are added to the item adders*/
        FOR EACH ttVendItemCost WHERE ttVendItemCost.ItemID <> ipcItemID:
            DELETE ttVendItemCost.
        END.
    END. /*lAvailAdder*/
END PROCEDURE.

PROCEDURE CopyVendItemCost:
    /*------------------------------------------------------------------------------
     Purpose: Copy vendItemCost records from an estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimate    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo      AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcNewEstimate AS CHARACTER NO-UNDO.

    DEFINE BUFFER vendItemCost         FOR vendItemCost.
    DEFINE BUFFER bf-vendItemCost      FOR vendItemCost.
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.

    FOR EACH vendItemCost NO-LOCK
        WHERE vendItemCost.company    EQ ipcCompany
        AND vendItemCost.estimateNo EQ ipcEstimate
        AND vendItemCost.formNo     EQ ipiFormNo
        AND vendItemCost.blankno    EQ ipiBlankNo
        AND vendItemCost.itemID     EQ ipcItemID:
        
        FIND FIRST bf-vendItemCost EXCLUSIVE-LOCK
             WHERE bf-vendItemCost.company EQ ipcCompany
             AND bf-vendItemCost.itemID EQ ipcItemID
             AND bf-vendItemCost.ItemType EQ vendItemCost.ItemType
             AND bf-vendItemCost.vendorID EQ vendItemCost.vendorID
             AND bf-vendItemCost.customerID EQ vendItemCost.customerID
             AND trim(bf-vendItemCost.EstimateNo) EQ trim(ipcNewEstimate)
             AND bf-vendItemCost.formNo EQ INTEGER(vendItemCost.formNo)              
             AND bf-vendItemCost.effectiveDate EQ DATE(vendItemCost.effectiveDate)
             NO-ERROR.
        
        IF NOT AVAILABLE bf-vendItemCost THEN
        CREATE bf-vendItemCost .
        BUFFER-COPY vendItemCost EXCEPT company estimateNo rec_key vendItemCostID itemID TO bf-vendItemCost.
        ASSIGN
            bf-vendItemCost.company    = ipcCompany
            bf-vendItemCost.estimateNo = ipcNewEstimate
            bf-vendItemCost.ItemID     = ipcItemID
            .
        FOR EACH vendItemCostLevel NO-LOCK    
            WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID:
            CREATE bf-vendItemCostLevel.
            BUFFER-COPY vendItemCostLevel EXCEPT rec_key vendItemCostID vendItemCostLevelID TO bf-vendItemCostLevel. 
            bf-vendItemCostLevel.vendItemCostID = bf-vendItemCost.vendItemCostID.    
        END.          
    END. 
    RELEASE bf-vendItemCost.
    RELEASE bf-vendItemCostLevel.
END PROCEDURE.

PROCEDURE CreateVendItemCost:
    /*------------------------------------------------------------------------------
     Purpose: To Create VendItemCost Records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimate    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo      AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNO     AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER vendItemCost         FOR vendItemCost.
    DEFINE BUFFER bf-vendItemCost      FOR vendItemCost.
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.

    IF ipcItemID EQ "" AND 
        NOT CAN-FIND(FIRST vendItemCost
        WHERE vendItemCost.company    EQ ipcCompany
        AND vendItemCost.estimateNo EQ ipcEstimate
        AND vendItemCost.formNo     EQ ipiFormNo
        AND vendItemCost.blankNo    EQ ipiBlankNO
        AND vendItemCost.itemID     EQ ""
        AND vendItemCost.vendorID   EQ "") THEN 
    DO TRANSACTION:

        CREATE bf-vendItemCost. 
        ASSIGN 
            bf-vendItemCost.company    = ipcCompany
            bf-vendItemCost.itemType   = "FG"
            bf-vendItemCost.estimateNo = ipcEstimate
            bf-vendItemCost.formNo     = ipiFormNo
            bf-vendItemCost.blankNo    = ipiBlankNo
            .
        CREATE bf-vendItemCostLevel.
        ASSIGN 
            bf-vendItemCostLevel.vendItemCostID = bf-vendItemCost.vendItemCostID
            bf-vendItemCostLevel.quantityBase   = 99999999
            .           
    END.
    ELSE IF ipcItemID NE "" AND
            NOT CAN-FIND(FIRST vendItemCost
            WHERE vendItemCost.company    EQ ipcCompany
            AND vendItemCost.estimateNo EQ ipcEstimate
            AND vendItemCost.formNo     EQ ipiFormNo
            AND vendItemCost.blankNo    EQ ipiBlankNO
            AND vendItemCost.itemID     EQ ipcItemID) THEN 
        DO:

            FOR EACH vendItemCost NO-LOCK
                WHERE vendItemCost.company    EQ ipcCompany 
                AND vendItemCost.itemID     EQ ipcItemID 
                AND vendItemCost.estimateNo EQ "" :   

                DO TRANSACTION:
                    CREATE bf-vendItemCost.
                    BUFFER-COPY vendItemCost EXCEPT estimateNo vendItemCostID rec_key TO bf-vendItemCost.
                    ASSIGN 
                        bf-vendItemCost.estimateNo = ipcEstimate
                        bf-vendItemCost.formNo     = ipiFormNo
                        bf-vendItemCost.blankNo    = ipiBlankNo
                        .            
                    FOR EACH vendItemCostLevel NO-LOCK
                        WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID:
                        CREATE bf-vendItemCostLevel.
                        BUFFER-COPY vendItemCostLevel EXCEPT vendItemCostLevelID rec_key TO bf-vendItemCostLevel.
                        bf-vendItemCostLevel.vendItemCostID = bf-vendItemCost.vendItemCostID.
                    END.           
                END.
            END.
        END.         
    RELEASE bf-vendItemCost.
    RELEASE bf-vendItemCostLevel. 

END PROCEDURE.

PROCEDURE DeleteVendItemCost:
    /*------------------------------------------------------------------------------
     Purpose: To delete vendItemCost Records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimate AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo  AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER vendItemCost         FOR vendItemCost.
    DEFINE BUFFER bf-vendItemCost      FOR vendItemCost.
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.

    FOR EACH bf-vendItemCost EXCLUSIVE-LOCK
        WHERE bf-vendItemCost.company    EQ ipcCompany
        AND bf-vendItemCost.estimateNo EQ ipcEstimate
        AND bf-vendItemCost.itemID     EQ ipcItemID
        AND bf-vendItemCost.formNo     EQ ipiFormNo
        AND bf-vendItemCost.blankNo    EQ ipiBlankNo:    
        FOR EACH bf-vendItemCostLevel EXCLUSIVE-LOCK 
            WHERE bf-vendItemCost.vendItemCostID EQ bf-vendItemCostLevel.vendItemCostID:
            DELETE bf-vendItemCostLevel.  
        END.  
        DELETE bf-vendItemCost.  
    END. 

    RELEASE bf-vendItemCost.
    RELEASE bf-vendItemCostLevel.

END PROCEDURE.


PROCEDURE GetFirstVendCostFromReport:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper procedure to build temp-table tt-report from report table
              and return first available record               
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTerm         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipriJobMat      AS RECID     NO-UNDO.
    DEFINE INPUT  PARAMETER iplItemType     AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipItemID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQty          AS DECIMAL   NO-UNDO.  /* Qty to use for cost determination */
    DEFINE INPUT  PARAMETER ipcUom          AS CHARACTER NO-UNDO.  /* UOm to use for cost determination */
    DEFINE INPUT  PARAMETER iplUseNewTables AS LOGICAL   NO-UNDO.  /* Sent true to use new vendItemCost tables, false to use old tables */
    DEFINE OUTPUT PARAMETER opriReport      AS RECID     NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess      AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.
    
    IF iplUseNewTables THEN
        RUN pBuildVendorCostFromReportUsingNewTables (
            INPUT  ipcCompany,
            INPUT  ipcTerm,
            INPUT  ipriJobMat,
            INPUT  iplItemType,
            INPUT  ipItemID,
            INPUT  ipdQty,
            INPUT  ipcUom,
            OUTPUT oplSuccess,
            OUTPUT opcMessage            
            ) NO-ERROR.      
    ELSE
        RUN pBuildVendorCostFromReportUsingOldTables (
            INPUT  ipcCompany,
            INPUT  ipcTerm,
            INPUT  ipriJobMat,
            INPUT  iplItemType,
            INPUT  ipItemID,
            INPUT  ipdQty,
            INPUT  ipcUom,
            OUTPUT oplSuccess,
            OUTPUT opcMessage            
            ) NO-ERROR.
    
    FIND FIRST tt-report NO-LOCK NO-ERROR.
    IF AVAILABLE tt-report THEN
        opriReport = tt-report.rec-id.
    
    /* Empty temp-table once the request is processed */
    EMPTY TEMP-TABLE tt-report.              
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
            RUN pConvertQuantity(ipcCompany, ipcItemID, ipcItemType, ipdQuantity, ipcQuantityUOM, opcCostUOM, 
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
    DEFINE VARIABLE dCostPerUOMBase      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lIsSelected          AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dCostDeviation       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iLeadDays            AS INTEGER NO-UNDO.
    
    RUN pSetGlobalSettings(ipcCompany).
    RUN pGetVendItemCostBuffer(ipcCompany, ipcItemID, ipcItemType, ipcVendorID, ipcCustomerID, ipcEstimateNo, ipiFormNo, ipiBlankNo, iplExactMatch,
        BUFFER bf-vendItemCost, OUTPUT oplError, OUTPUT opcMessage).
    IF NOT oplError AND AVAILABLE bf-vendItemCost THEN 
    DO:
        ASSIGN 
            opcCostUOM = bf-vendItemCost.vendorUOM.
        IF opcCostUOM NE ipcQuantityUOM THEN 
        DO:  
            RUN pConvertQuantity(ipcCompany, ipcItemID, ipcItemType, ipdQuantity, ipcQuantityUOM, opcCostUOM, 
                ipdBasisWeight, ipcBasisWeightUOM, ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM,
                OUTPUT dQuantityInVendorUOM, OUTPUT oplError, INPUT-OUTPUT opcMessage).
        END.
        ELSE 
            dQuantityInVendorUOM = ipdQuantity. 
        
        RUN pGetVendorCosts(BUFFER bf-vendItemCost, dQuantityInVendorUOM, ipdDimLength, ipdDimWidth, ipcDimUOM, 
            OUTPUT opdCostPerUOM, OUTPUT opdCostSetup, OUTPUT dCostPerUOMUpcharge, OUTPUT dCostPerUOMBase,
            OUTPUT opdCostTotal, OUTPUT lIsSelected, 
            OUTPUT iLeadDays, OUTPUT dCostDeviation,
            OUTPUT oplError, INPUT-OUTPUT opcMessage).
        
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
         
    IF NOT AVAILABLE bf-vendItemCost THEN 
    DO:
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

PROCEDURE pCreateVendItemCostLevel PRIVATE:
 /*------------------------------------------------------------------------------
     Purpose:  Creates a vend item cost level given all inputs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiVendItemCostID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityBase AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostSetup AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostDeviation AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiLeadTimeDays AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplUseForBestCost AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opriVendItemCostLevel AS ROWID NO-UNDO.
    
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
    
    DEFINE VARIABLE lReturnError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturnMessage AS CHARACTER NO-UNDO.
     
    CREATE bf-vendItemCostLevel .
    ASSIGN
        bf-vendItemCostLevel.vendItemCostID = ipiVendItemCostID
        bf-vendItemCostLevel.quantityBase   = ipdQuantityBase  
        bf-vendItemCostLevel.costPerUOM     = ipdCostPerUOM 
        bf-vendItemCostLevel.costSetup      = ipdCostSetup
        bf-vendItemCostLevel.costDeviation  = ipdCostDeviation
        bf-vendItemCostLevel.leadTimeDays   = ipiLeadTimeDays
        bf-vendItemCostLevel.useForBestCost = iplUseForBestCost
        . 
    
    opriVendItemCostLevel = ROWID(bf-vendItemCostLevel).
    
    RUN pSetMaxOrderValue (BUFFER bf-vendItemCostLevel).
        
    RELEASE bf-vendItemCostLevel.
    
END PROCEDURE.

PROCEDURE pExpireOldCost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendorID   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo    AS INTEGER   NO-UNDO.

    DEFINE VARIABLE dtExpirationDate AS DATE NO-UNDO.

    DEFINE BUFFER bfVendItemCost FOR vendItemCost.

    FOR EACH vendItemCost NO-LOCK 
        WHERE vendItemCost.company    EQ ipcCompany
        AND vendItemCost.itemID     EQ ipcItemID
        AND vendItemCost.itemType   EQ ipcItemType
        AND vendItemCost.vendorID   EQ ipcVendorID
        AND vendItemCost.customerID EQ ipcCustomerID
        AND vendItemCost.estimateNo EQ ipcEstimateNo
        AND vendItemCost.formNo     EQ ipiFormNo
        AND vendItemCost.blankNo    EQ ipiBlankNo
        BREAK BY vendItemCost.effectiveDate  DESCENDING
        BY vendItemCost.expirationDate DESCENDING
        :
        IF NOT FIRST(vendItemCost.effectiveDate) AND
            vendItemCost.expirationDate NE dtExpirationDate THEN
        DO TRANSACTION:
            FIND FIRST bfVendItemCost EXCLUSIVE-LOCK
                WHERE ROWID(bfVendItemCost) EQ ROWID(vendItemCost).
            bfVendItemCost.expirationDate = dtExpirationDate.
            RELEASE bfVendItemCost.
        END. /* if not first-of */
        dtExpirationDate = vendItemCost.effectiveDate - 1.
    END. /* each venditemcost */

END PROCEDURE.

PROCEDURE pExpirePriceByCust PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplProcess  AS LOGICAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-VendItemCost.
    
    DEFINE BUFFER bf-vendItemCost FOR vendItemCost.
    
    EMPTY TEMP-TABLE tt-VendItemCost.
    
    FOR EACH bf-vendItemCost NO-LOCK  
        WHERE bf-vendItemCost.company        EQ ipcCompany
        AND bf-vendItemCost.customerID     EQ ipcCustomer
        AND(bf-vendItemCost.expirationDate GT TODAY OR bf-vendItemCost.expirationDate EQ ?) :
        IF iplProcess THEN 
        DO:
            FIND CURRENT bf-vendItemCost EXCLUSIVE-LOCK NO-ERROR. 
            bf-vendItemCost.expirationDate = TODAY.
        END.    
        ELSE 
        DO:
            CREATE tt-VendItemCost.
            ASSIGN 
                tt-VendItemCost.company           = bf-vendItemCost.company
                tt-VendItemCost.estimate          = bf-vendItemCost.estimate
                tt-VendItemCost.blankNo           = bf-vendItemCost.blankNo
                tt-VendItemCost.customerID        = bf-vendItemCost.customerId
                tt-VendItemCost.effectiveDate     = bf-vendItemCost.effectiveDate
                tt-VendItemCost.formNo            = bf-vendItemCost.formNo
                tt-VendItemCost.itemID            = bf-vendItemCost.itemId
                tt-VendItemCost.itemType          = bf-vendItemCost.itemType
                tt-VendItemCost.oldExpirationDate = bf-vendItemCost.expirationDate
                tt-VendItemCost.newExpirationDate = TODAY
                tt-VendItemCost.UOM               = bf-vendItemCost.VendorUom
                tt-VendItemCost.vendorID          = bf-vendItemCost.vendorID
                .                      
        END.              
    END.

END PROCEDURE.

PROCEDURE pSetMaxOrderValue PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a vendItemCostLevel buffer, determine if the vendItemCost max order
        quantity should be updated (NK1 VendItemCostMaximum = "AutoSetOnAdd" functionality)
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-vendItemCostLevel FOR vendItemCostLevel.
    
    DEFINE           BUFFER bf-vendItemCost        FOR vendItemCost.
    
    IF gcAutoSetMaximum EQ "" THEN 
    DO:
        FIND FIRST bf-vendItemCost NO-LOCK 
            WHERE bf-vendItemCost.vendItemCostID EQ ipbf-vendItemCostLevel.vendItemCostID
            NO-ERROR. 
        IF AVAILABLE bf-vendItemCost THEN  
            gcAutoSetMaximum = fGetSettingAutoSetMaximum(bf-vendItemCost.company).
    END. 
    IF gcAutoSetMaximum EQ "AutoSetOnAdd" THEN 
    DO:
        IF NOT CAN-FIND(FIRST vendItemCostLevel 
            WHERE vendItemCostLevel.vendItemCostID EQ ipbf-vendItemCostLevel.vendItemCostID
            AND vendItemCostLevel.quantityBase GT ipbf-vendItemCostLevel.quantityBase ) THEN 
        DO:
            FIND FIRST bf-vendItemCost EXCLUSIVE-LOCK 
                WHERE bf-vendItemCost.vendItemCostID EQ ipbf-vendItemCostLevel.vendItemCostID
                NO-ERROR.
            IF AVAILABLE bf-vendItemCost THEN 
                ASSIGN 
                    bf-vendItemCost.quantityMaximumOrder = ipbf-vendItemCostLevel.quantityBase
                    .
            RELEASE bf-vendItemCost.
        END.    
    END.

END PROCEDURE.

PROCEDURE pUpdateVendItemCostLevel PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose: Updates a given VendItemCostLevel given ROWID and inputs.  
     Will also create if rowid not found (or 0) 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriVendItemCostLevel AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipiVendItemCostID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityBase AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostSetup AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostDeviation AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiLeadTimeDays AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplUseForBestCost AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
    
    DEFINE VARIABLE lReturnError        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturnMessage      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE riVendItemCostLevel AS ROWID     NO-UNDO.
    
    FIND bf-vendItemCostLevel EXCLUSIVE-LOCK 
        WHERE ROWID(bf-vendItemCostLevel) EQ ipriVendItemCostLevel
        NO-ERROR.
    IF NOT AVAILABLE bf-vendItemCostLevel THEN 
    DO: 
        RUN pCreateVendItemCostLevel (ipiVendItemCostID,
            ipdQuantityBase,
            ipdCostPerUOM,
            ipdCostSetup,
            ipdCostDeviation,
            ipiLeadTimeDays,
            iplUseForBestCost,
            OUTPUT riVendItemCostLevel).
    END.
    ELSE 
    DO:
        ASSIGN 
            bf-vendItemCostLevel.quantityBase   = ipdQuantityBase  
            bf-vendItemCostLevel.costPerUOM     = ipdCostPerUOM 
            bf-vendItemCostLevel.costSetup      = ipdCostSetup
            bf-vendItemCostLevel.costDeviation  = ipdCostDeviation 
            bf-vendItemCostLevel.leadTimeDays   = ipiLeadTimeDays
            bf-vendItemCostLevel.useForBestCost = iplUseForBestCost
            .
        RUN pSetMaxOrderValue (BUFFER bf-vendItemCostLevel).    
    
    END.
    RELEASE bf-vendItemCostLevel.


END PROCEDURE.

PROCEDURE VendCost_CreateVendItemCostLevel:
    /*------------------------------------------------------------------------------
     Purpose:  Creates a vend item cost level
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiVendItemCostID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityBase AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostSetup AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostDeviation AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiLeadTimeDays AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplUseForBestCost AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opriVendItemCostLevel AS ROWID NO-UNDO.
    
    DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cReturnMessage AS CHARACTER NO-UNDO.
    
    RUN pCreateVendItemCostLevel (ipiVendItemCostID,
        ipdQuantityBase,
        ipdCostPerUOM,
        ipdCostSetup,
        ipdCostDeviation,
        ipiLeadTimeDays,
        iplUseForBestCost,
        OUTPUT opriVendItemCostLevel).
    
    RUN RecalculateFromAndTo (ipiVendItemCostID, OUTPUT lReturnError, OUTPUT cReturnMessage).
    
END PROCEDURE.

PROCEDURE VendCost_UpdateVendItemCostLevel:
    /*------------------------------------------------------------------------------
     Purpose: Updates a given VendItemCostLevel given ROWID and inputs.  
     Will also create if rowid not found (or 0) 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriVendItemCostLevel AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipiVendItemCostID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityBase AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostSetup AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostDeviation AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiLeadTimeDays AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplUseForBestCost AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cReturnMessage AS CHARACTER NO-UNDO.
    
    RUN pUpdateVendItemCostLevel (ipriVendItemCostLevel,
        ipiVendItemCostID,
        ipdQuantityBase,
        ipdCostPerUOM,
        ipdCostSetup,
        ipdCostDeviation,
        ipiLeadTimeDays,
        iplUseForBestCost).
    RUN RecalculateFromAndTo (ipiVendItemCostID, OUTPUT lReturnError, OUTPUT cReturnMessage).

END PROCEDURE.

PROCEDURE VendCost_UpdateVendItemCostLevelNoRecalc:
    /*------------------------------------------------------------------------------
     Purpose: Updates a given VendItemCostLevel given ROWID and inputs.  
     Will also create if rowid not found (or 0) .
     Does NOT do a recalc of levels
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriVendItemCostLevel AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipiVendItemCostID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityBase AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostSetup AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostDeviation AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiLeadTimeDays AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplUseForBestCost AS LOGICAL NO-UNDO.
        
    RUN pUpdateVendItemCostLevel (ipriVendItemCostLevel,
        ipiVendItemCostID,
        ipdQuantityBase,
        ipdCostPerUOM,
        ipdCostSetup,
        ipdCostDeviation,
        ipiLeadTimeDays,
        iplUseForBestCost).

END PROCEDURE.

PROCEDURE Vendor_CheckPriceHoldForPo:
    /*------------------------------------------------------------------------------
     Purpose: Given an oe-ord rowid, check all order lines to see if Price Hold criteria
     is met.  Return price hold.  
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriPoOrd          AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER iplUpdateDB        AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplPriceHold       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPriceHoldReason AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-po-ord       FOR po-ord.
    DEFINE BUFFER bf-po-ordl      FOR po-ordl.        
    DEFINE BUFFER bf-vendItemCost FOR vendItemCost.
    
    DEFINE VARIABLE dQuantityInVendorUOM AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMUpcharge  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMBase      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lIsSelected          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dCostDeviation       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iLeadDays            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lError               AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCostUOM             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCostPerUOM          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostSetup           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostTotal           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cPOPriceHold         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPOPriceHold         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iVendCostLevelID     AS INTEGER   NO-UNDO.
 
    FIND FIRST bf-po-ord NO-LOCK 
        WHERE ROWID(bf-po-ord) EQ ipriPoOrd
        NO-ERROR.    
    IF AVAILABLE bf-po-ord THEN 
    DO:                  
        RUN sys/ref/nk1look.p (
            bf-po-ord.company,
            "POPriceHold",
            "L",
            YES,
            NO,
            bf-po-ord.vend-no,
            "", 
            OUTPUT cPOPriceHold,
            OUTPUT lPOPriceHold
            ).
        IF lPOPriceHold EQ NO OR cPOPriceHold EQ "NO" THEN RETURN.

        EMPTY TEMP-TABLE ttPriceHold.
        FOR EACH bf-po-ordl NO-LOCK
            WHERE bf-po-ordl.company EQ bf-po-ord.company
            AND bf-po-ordl.po-no EQ bf-po-ord.po-no
            :                                                    
            RUN pGetVendItemCostBuffer (
                bf-po-ord.company,
                bf-po-ordl.i-no,
                (IF bf-po-ordl.item-type THEN "RM" ELSE "FG"),
                bf-po-ord.vend-no,
                bf-po-ordl.cust-no,
                "",
                0,
                0,
                NO,
                BUFFER bf-vendItemCost,
                OUTPUT lError,
                OUTPUT cMessage
                ).                      
            IF NOT lError AND AVAILABLE bf-vendItemCost THEN 
            DO:    
                cCostUOM = bf-vendItemCost.vendorUOM.
                IF cCostUOM NE bf-po-ordl.pr-qty-uom THEN 
                DO: 
                    RUN pConvertQuantity (
                        bf-po-ord.company,
                        bf-po-ordl.i-no,
                        (IF bf-po-ordl.item-type THEN "RM" ELSE "FG"),
                        bf-po-ordl.ord-qty,
                        bf-po-ordl.pr-qty-uom,
                        cCostUOM, 
                        0,
                        "LB/EA",
                        bf-po-ordl.s-len,
                        bf-po-ordl.s-wid,
                        bf-po-ordl.s-dep,
                        "IN",
                        OUTPUT dQuantityInVendorUOM,
                        OUTPUT lError,
                        INPUT-OUTPUT cMessage
                        ).
                END.
                ELSE 
                    dQuantityInVendorUOM = bf-po-ordl.ord-qty. 
                
                RUN pGetVendorCosts (
                    BUFFER bf-vendItemCost,
                    dQuantityInVendorUOM,
                    bf-po-ordl.s-len,
                    bf-po-ordl.s-wid,
                    "IN", 
                    OUTPUT dCostPerUOM,
                    OUTPUT dCostSetup,
                    OUTPUT dCostPerUOMUpcharge,
                    OUTPUT dCostPerUOMBase,
                    OUTPUT dCostTotal,
                    OUTPUT lIsSelected, 
                    OUTPUT iLeadDays,
                    OUTPUT dCostDeviation,
                    OUTPUT lError,
                    INPUT-OUTPUT cMessage
                    ).
                
                RUN pGetCostLevel(bf-vendItemCost.vendItemCostID, dQuantityInVendorUOM, OUTPUT iVendCostLevelID).
                     
                IF dCostPerUOM NE bf-po-ordl.cost OR iVendCostLevelID EQ 0 THEN
                DO:
                    CREATE ttPriceHold.                 
                    ASSIGN                       
                        ttPriceHold.cFGItemID        = bf-po-ordl.i-no
                        ttPriceHold.cCustID          = bf-po-ord.cust-no
                        ttPriceHold.cShipID          = ""
                        ttPriceHold.dQuantity        = bf-po-ordl.ord-qty                
                        ttPriceHold.lPriceHold       = YES
                        ttPriceHold.cPriceHoldDetail = ""  .
                    IF iVendCostLevelID EQ 0 THEN
                        ttPriceHold.cPriceHoldReason = "No matrix exist ".
                    ELSE
                        ttPriceHold.cPriceHoldReason = "Item Cost for " + ttPriceHold.cFGItemID + " not matched in Vendor Cost table" .                                            
                END. 
                
                IF dQuantityInVendorUOM LT bf-vendItemCost.quantityMinimumOrder OR (dQuantityInVendorUOM GT bf-vendItemCost.quantityMaximumOrder AND bf-vendItemCost.quantityMaximumOrder NE 0) THEN
                DO:
                    CREATE ttPriceHold.                 
                    ASSIGN                       
                        ttPriceHold.cFGItemID        = bf-po-ordl.i-no
                        ttPriceHold.cCustID          = bf-po-ord.cust-no
                        ttPriceHold.cShipID          = ""
                        ttPriceHold.dQuantity        = bf-po-ordl.ord-qty                
                        ttPriceHold.lPriceHold       = YES
                        ttPriceHold.cPriceHoldDetail = ""
                        ttPriceHold.cPriceHoldReason = IF dQuantityInVendorUOM LT bf-vendItemCost.quantityMinimumOrder THEN "Item quantity is less then min order qty(Item - " + ttPriceHold.cFGItemID + "  Min Qty - " + STRING(bf-vendItemCost.quantityMinimumOrder) + ")"
                                                       ELSE "Item quantity is greater then max order qty(Item - " + ttPriceHold.cFGItemID + "  Max Qty - " + STRING(bf-vendItemCost.quantityMaximumOrder) + ")"
                        .                    
                END.
            END.  
            ELSE 
            DO:             
                CREATE ttPriceHold.                
                ASSIGN                  
                    ttPriceHold.cFGItemID        = bf-po-ordl.i-no
                    ttPriceHold.cCustID          = bf-po-ord.cust-no
                    ttPriceHold.cShipID          = ""
                    ttPriceHold.dQuantity        = bf-po-ordl.ord-qty                 
                    ttPriceHold.lPriceHold       = YES
                    ttPriceHold.cPriceHoldDetail = ""
                    ttPriceHold.cPriceHoldReason = "No matrix found"
                    .                             
            END.                   
        END.
    END.
    
    FIND FIRST ttPriceHold NO-LOCK
        WHERE ttPriceHold.lPriceHold
        NO-ERROR.
    IF AVAILABLE ttPriceHold THEN
        ASSIGN 
            oplPriceHold       = YES
            opcPriceHoldReason = ttPriceHold.cPriceHoldReason
            .
    ELSE 
        ASSIGN 
            oplPriceHold       = NO
            opcPriceHoldReason = ""
            .
    IF iplUpdateDB AND oplPriceHold THEN 
    DO:
        FIND FIRST bf-po-ord EXCLUSIVE-LOCK 
            WHERE ROWID(bf-po-ord) EQ ipriPoOrd
            NO-ERROR.               
        IF AVAILABLE bf-po-ord THEN 
            ASSIGN          
                bf-po-ord.priceHold = oplPriceHold
                bf-po-ord.stat      = "H"                  
                .
        FIND CURRENT bf-po-ord NO-LOCK.
        RUN ClearTagsByRecKey (bf-po-ord.rec_key).
        FOR EACH ttPriceHold NO-LOCK
            WHERE ttPriceHold.lPriceHold
            :          
            RUN AddTagHold (
                bf-po-ord.rec_key,
                "po-ord",
                ttPriceHold.cPriceHoldReason,
                "",
                ""
                ).
        END.
    END.
    RELEASE bf-po-ord.
    
END PROCEDURE.

PROCEDURE Vendor_ExpireOldCost:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendorID   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo    AS INTEGER   NO-UNDO.

    RUN pExpireOldCost (
        ipcCompany,
        ipcItemID,
        ipcItemType,
        ipcVendorID,
        ipcCustomerID,
        ipcEstimateNo,
        ipiFormNo,
        ipiBlankNo   
        ).

END PROCEDURE.

PROCEDURE Vendor_ExpirePriceByCust:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER NO-UNDO.
    
    RUN pExpirePriceByCust(
        INPUT ipcCompany,
        INPUT ipcCustomer,
        INPUT YES,
        INPUT-OUTPUT TABLE tt-VendItemCost
        ).

END PROCEDURE.

PROCEDURE Vendor_ExpirePriceByCustTT:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-VendItemCost.
    
    RUN pExpirePriceByCust(
        INPUT ipcCompany,
        INPUT ipcCustomer,
        INPUT NO,
        INPUT-OUTPUT TABLE tt-VendItemCost
        ).

END PROCEDURE.

PROCEDURE Vendor_VendItemCostWithPercentage:
    /*------------------------------------------------------------------------------
     Purpose: update vendcostprice with percentage  
     Parameters:  <none>
     Notes:       
     ------------------------------------------------------------------------------*/

    DEFINE INPUT        PARAMETER ipriID                 AS ROWID     NO-UNDO.
    DEFINE INPUT        PARAMETER ipdPercentage          AS DECIMAL   NO-UNDO.     
    DEFINE OUTPUT       PARAMETER oplError               AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage             AS CHARACTER NO-UNDO.
        
    DEFINE BUFFER bf-vendItemCost FOR VendItemCost.
    
    FIND FIRST bf-vendItemCost NO-LOCK 
        WHERE ROWID(bf-vendItemCost) EQ ipriID
        NO-ERROR.
         
    IF NOT AVAILABLE bf-vendItemCost THEN 
    DO:
        ASSIGN 
            oplError   = TRUE
            opcMessage = "Invalid vendItemCost record"
            .
        RETURN.
    END.
    
    RUN pUpdateVendCostPriceWithPercentage(
        BUFFER bf-vendItemCost, 
        INPUT  ipdPercentage              
        ).
        
    RELEASE bf-vendItemCost.    
    
END PROCEDURE.


PROCEDURE pAddTTVendItemCost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds TT Vend Item Cost given vendItemCost buffer and inputs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-vendItemCost FOR vendItemCost.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimWidth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimDepth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcDimUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdBasisWeight AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcBasisWeightUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttVendItemCost FOR ttVendItemCost.
    
    FIND FIRST TTVendItemCost WHERE TTVendItemCost.company = ipbf-vendItemCost.Company
        AND TTVendItemCost.itemID         = ipbf-vendItemCost.ItemID
        AND TTVendItemCost.itemType       = ipbf-vendItemCost.ItemType
        AND TTVendItemCost.vendorID       = ipbf-vendItemCost.vendorID
        AND TTVendItemCost.customerID     = ipbf-vendItemCost.customerID
        AND TTVendItemCost.estimateNo     = ipbf-vendItemCost.EstimateNo
        AND TTVendItemCost.formNo         = ipbf-vendItemCost.FormNo
        AND TTVendItemCost.blankNo        = ipbf-vendItemCost.BlankNo
        AND TTVendItemCost.effectiveDate  = ipbf-vendItemCost.effectiveDate
        AND TTVendItemCost.expirationDate = ipbf-vendItemCost.expirationDate NO-LOCK NO-ERROR.
    IF NOT AVAILABLE TTVendItemCost THEN
    DO:
        CREATE ttVendItemCost.
        BUFFER-COPY ipbf-vendItemCost TO ttVendItemCost.
    END.

    ASSIGN 
        ttVendItemCost.isExpired                 = ipbf-vendItemCost.expirationDate LT TODAY AND ipbf-vendItemCost.expirationDate NE ? AND ipbf-vendItemCost.expirationDate NE 01/01/0001
        ttVendItemCost.isNotEffective            = ipbf-vendItemCost.effectiveDate GT TODAY
        ttVendItemCost.quantityTarget            = ipdQuantity
        ttVendItemCost.quantityTargetInVendorUOM = ipdQuantity
        ttVendItemCost.quantityTargetUOM         = ipcQuantityUOM
        ttVendItemCost.dimLengthInVendorDimUOM   = ipdDimLength
        ttVendItemCost.dimWidthInVendorDimUOM    = ipdDimWidth
        ttVendItemCost.dimDepthInVendorDimUOM    = ipdDimDepth
        .
    IF ttVendItemCost.isExpired  THEN 
        ttVendItemCost.reasonNotValid = ttVendItemCost.reasonNotValid + "Expired,".
    IF ttVendItemCost.isNotEffective  THEN 
        ttVendItemCost.reasonNotValid = ttVendItemCost.reasonNotValid + "Not Yet Effective,".
        
    IF ttVendItemCost.quantityTargetUOM NE ttVendItemCost.vendorUOM THEN
        RUN pConvertQuantity(ipbf-vendItemCost.company, ipbf-vendItemCost.itemID, ipbf-vendItemCost.itemType, ipdQuantity, ttVendItemCost.quantityTargetUOM, ttVendItemCost.vendorUOM, 
            ipdBasisWeight, ipcBasisWeightUOM, ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM,
            OUTPUT ttVendItemCost.quantityTargetInVendorUOM, OUTPUT oplError, INPUT-OUTPUT opcMessage).
    IF ipcDimUOM NE ipbf-vendItemCost.dimUOM THEN 
    DO:
        RUN pConvertDim(ipbf-vendItemCost.company, ipdDimLength, ipcDimUOM, ipbf-vendItemCost.dimUOM,
            OUTPUT ttVendItemCost.dimLengthInVendorDimUOM, OUTPUT oplError, INPUT-OUTPUT opcMessage).
        IF NOT oplError THEN 
            RUN pConvertDim(ipbf-vendItemCost.company, ipdDimWidth, ipcDimUOM, ipbf-vendItemCost.dimUOM, 
                OUTPUT ttVendItemCost.dimWidthInVendorDimUOM, OUTPUT oplError, INPUT-OUTPUT opcMessage).
        IF NOT oplError THEN 
            RUN pConvertDim(ipbf-vendItemCost.company, ipdDimDepth, ipcDimUOM, ipbf-vendItemCost.dimUOM,
                OUTPUT ttVendItemCost.dimDepthInVendorDimUOM, OUTPUT oplError, INPUT-OUTPUT opcMessage).
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
    IF ttVendItemCost.dimLengthMaximum NE 0 AND ttVendItemCost.dimLengthInVendorDimUOM GT ttVendItemCost.dimLengthMaximum THEN 
        ttVendItemCost.reasonNotValid = ttVendItemCost.reasonNotValid + 
            "Item length of " + STRING(ttVendItemCost.dimLengthInVendorDimUOM) + 
            " less than maximum length of " + STRING(ttVendItemCost.dimLengthMaximum) + 
            " " + ttVendItemCost.dimUOM + ",".
    IF ttVendItemCost.dimWidthInVendorDimUOM LT ttVendItemCost.dimWidthMinimum THEN 
        ttVendItemCost.reasonNotValid = ttVendItemCost.reasonNotValid + 
            "Item width of " + STRING(ttVendItemCost.dimWidthInVendorDimUOM) + 
            " less than minimum width of " + STRING(ttVendItemCost.dimWidthMinimum) + 
            " " + ttVendItemCost.dimUOM + ",".
    IF ttVendItemCost.dimLengthMaximum NE 0 AND ttVendItemCost.dimWidthInVendorDimUOM GT ttVendItemCost.dimWidthMaximum THEN 
        ttVendItemCost.reasonNotValid = ttVendItemCost.reasonNotValid + 
            "Item width of " + STRING(ttVendItemCost.dimWidthInVendorDimUOM) + 
            " less than maximum width of " + STRING(ttVendItemCost.dimWidthMaximum) + 
            " " + ttVendItemCost.dimUOM + ",".
    ttVendItemCost.reasonNotValid = TRIM(TRIM(ttVendItemCost.reasonNotValid,",")).
    IF ttVendItemCost.reasonNotValid EQ "" THEN 
        ttVendItemCost.isValid = YES.
    IF ttVendItemCost.isValid THEN 
    DO:
        opcMessage = "".
        RUN pGetVendorCosts(BUFFER ipbf-vendItemCost, ttVendItemCost.quantityTargetInVendorUOM, 
            ttVendItemCost.dimLengthInVendorDimUOM, ttVendItemCost.dimWidthInVendorDimUOM, ttVendItemCost.dimUOM, 
            OUTPUT ttVendItemCost.costPerVendorUOM, OUTPUT ttVendItemCost.costSetup, OUTPUT ttVendItemCost.costPerVendorUOMUpcharge, OUTPUT ttVendItemCost.costPerVendorUOMBase, 
            OUTPUT ttVendItemCost.costTotal, OUTPUT ttVendItemCost.isSelected,
            OUTPUT ttVendItemCost.leadDays, OUTPUT ttVendItemCost.costDeviation,
            OUTPUT oplError, INPUT-OUTPUT opcMessage).
        IF oplError THEN 
            ASSIGN 
                ttVendItemCost.isValid        = NO 
                ttVendItemCost.reasonNotValid = opcMessage
                .
    END.
    FOR EACH bf-ttVendItemCost 
        WHERE bf-ttVendItemCost.company EQ ipbf-vendItemCost.company
        AND bf-ttVendItemCost.itemID EQ ipbf-vendItemCost.itemID
        AND bf-ttVendItemCost.vendorID EQ ipbf-vendItemCost.vendorID
        AND bf-ttVendItemCost.estimateNo EQ ipbf-vendItemCost.estimateNo
        AND bf-ttVendItemCost.formNo EQ ipbf-vendItemCost.formNo
        AND bf-ttVendItemCost.blankNo EQ ipbf-vendItemCost.blankNo
        AND bf-ttVendItemCost.effectiveDate LT ipbf-vendItemCost.effectiveDate 
        :
        bf-ttVendItemCost.reasonNotValid = "Not Recent".
        bf-ttVendItemCost.isValid = NO.          
    END.

END PROCEDURE.

PROCEDURE pBuildVendorCostFromReportUsingNewTables PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Build vendor cost temp-table tt-report from report table using
              new tables
     Notes: This is exact replication of internal procedure build-table in 
            po/d-vndcstN.w
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTerm     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipriJobMat  AS RECID     NO-UNDO.
    DEFINE INPUT  PARAMETER iplItemType AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQty      AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUom      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cUom        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQty        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCalcCost   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSetup      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lIsABoard   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cFGItem     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVendID     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex      AS INTEGER   NO-UNDO.

    DEFINE VARIABLE dCostTotal  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOM AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostSetup  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCostUOM    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
      
    DEFINE VARIABLE cRtnChar    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound   AS LOGICAL   NO-UNDO.
    
    /* Empty temp-table for every build so that, when running persistently
       doesn't populate unwanted records */       
    EMPTY TEMP-TABLE tt-report.
    
    DEFINE BUFFER bf-report            FOR report.
    DEFINE BUFFER bf-unique-report     FOR report.
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
    DEFINE BUFFER bf-vendItemCost      FOR vendItemCost.
    DEFINE BUFFER bf-job-mat           FOR job-mat.
    DEFINE BUFFER bf-job-hdr           FOR job-hdr.
    DEFINE BUFFER bf-vend              FOR vend.
    DEFINE BUFFER bf-fg-rcpth          FOR fg-rcpth.
    DEFINE BUFFER bf-fg-rdtlh          FOR fg-rdtlh.
    DEFINE BUFFER bf-est               FOR est.
    DEFINE BUFFER bf-ef                FOR ef.
    DEFINE BUFFER bf-po-ord            FOR po-ord.
    DEFINE BUFFER bf-item              FOR item.
    DEFINE BUFFER bf-itemfg            FOR itemfg.
    
    FIND FIRST bf-job-mat NO-LOCK
        WHERE RECID(bf-job-mat) EQ ipriJobMat
        NO-ERROR.
                 
    FOR EACH bf-report NO-LOCK 
        WHERE bf-report.term-id EQ ipcTerm:
        FIND bf-unique-report EXCLUSIVE-LOCK 
            WHERE ROWID(bf-unique-report) EQ ROWID(bf-report) NO-ERROR NO-WAIT.
        IF LOCKED(bf-unique-report) OR NOT AVAILABLE bf-unique-report THEN
            NEXT.
            
        FIND FIRST bf-vendItemCostLevel NO-LOCK 
            WHERE RECID(bf-vendItemCostLevel) EQ bf-unique-report.rec-id
            NO-ERROR.
        
        IF AVAILABLE bf-vendItemCostLevel THEN     
            FIND FIRST bf-vendItemCost NO-LOCK   
                WHERE bf-vendItemCost.vendItemCostID EQ bf-vendItemCostLevel.vendItemCostID 
                NO-ERROR.
        
        IF AVAILABLE bf-vendItemCost THEN
            cUom = bf-vendItemCost.vendorUOM.                                   

        dCalcCost = DECIMAL(bf-unique-report.key-01).
               
        IF bf-unique-report.key-08 EQ "RECALC" AND AVAILABLE(bf-vendItemCost) AND ipdQty GT 0 THEN 
        DO:      
            FIND FIRST bf-itemfg NO-LOCK
                WHERE bf-itemfg.company EQ ipcCompany
                AND bf-itemfg.i-no    EQ bf-vendItemCost.itemID 
                NO-ERROR.

            RUN GetVendorCost(
                INPUT  bf-vendItemCost.company, 
                INPUT  bf-vendItemCost.ItemID, 
                INPUT  bf-vendItemCost.itemType, 
                INPUT  bf-vendItemCost.vendorID, 
                INPUT  bf-vendItemCost.customerID, 
                INPUT  "", 
                INPUT  0, 
                INPUT  0,
                INPUT  ipdQty, 
                INPUT  bf-vendItemCost.vendorUOM,
                INPUT  IF AVAILABLE bf-itemfg THEN bf-itemfg.t-len ELSE 0, 
                INPUT  IF AVAILABLE bf-itemfg THEN bf-itemfg.t-wid ELSE 0, 
                INPUT  0, 
                INPUT  "IN", 
                INPUT  IF AVAILABLE bf-itemfg THEN bf-itemfg.weight-100 / 100 ELSE 0, 
                INPUT  "LB/EA", 
                INPUT  NO,
                OUTPUT dCostPerUOM, 
                OUTPUT dCostSetup, 
                OUTPUT cCostUOM,
                OUTPUT dCostTotal, 
                OUTPUT lError, 
                OUTPUT cMessage
                ).  
                                     
            ASSIGN
                bf-unique-report.key-01 = STRING(dCostTotal)  /*dCalcCost*/
                bf-unique-report.key-02 = STRING(ipdQty)
                bf-unique-report.key-05 = STRING((dCostSetup / ipdQty),"9999999999.9999")
                bf-unique-report.key-06 = STRING(dCostSetup,"9999999999.9999")    /* dSetup*/
                .
        END.
      
        FIND FIRST bf-vend NO-LOCK
            WHERE bf-vend.company EQ ipcCompany
            AND bf-vend.vend-no EQ cVendID
            NO-ERROR.
       
        CREATE tt-report.
        ASSIGN 
            tt-report.key-02      = IF bf-unique-report.key-08 EQ "RECALC" THEN 
                                           STRING(ipdQty) 
                                       ELSE 
                                           bf-unique-report.key-02
            tt-report.key-03      = bf-unique-report.key-03
            tt-report.key-04      = bf-unique-report.key-04
            tt-report.vend-name   = IF AVAILABLE bf-vend THEN 
                                           bf-vend.name 
                                       ELSE 
                                           ""
            tt-report.report-cost = dCalcCost
            tt-report.disc-days   = IF AVAILABLE bf-vend THEN 
                                           bf-vend.disc-days 
                                       ELSE
                                           0 
            tt-report.ext-price   = DECIMAL(bf-unique-report.key-02) * tt-report.report-cost
            tt-report.rec-id      = RECID(bf-unique-report)
            tt-report.cost-uom    = cUom
            .

        IF AVAILABLE bf-vendItemCost THEN
            ASSIGN
                tt-report.vend-item = bf-vendItemCost.vendorItemID    /*e-itemfg-vend.vend-item*/
                tt-report.wid-min   = bf-vendItemCost.dimWidthMinimum /*e-itemfg-vend.roll-w[27]*/
                tt-report.wid-max   = bf-vendItemCost.dimWidthMaximum /*e-itemfg-vend.roll-w[28]*/
                tt-report.len-min   = bf-vendItemCost.dimLengthMinimum /*e-itemfg-vend.roll-w[29]*/
                tt-report.len-max   = bf-vendItemCost.dimLengthMaximum /*e-itemfg-vend.roll-w[30]*/
                .
       
        IF AVAILABLE bf-job-mat THEN
            FIND FIRST bf-job-hdr NO-LOCK 
                WHERE bf-job-hdr.company EQ bf-job-mat.company
                AND bf-job-hdr.job-no  EQ bf-job-mat.job-no
                AND bf-job-hdr.job-no2 EQ bf-job-mat.job-no2
                NO-ERROR.
      
        IF AVAILABLE bf-job-hdr THEN
            cFGItem  = bf-job-hdr.i-no.
        ELSE
            cFGItem  = ipcItemID.

        FOR EACH bf-fg-rcpth FIELDS(r-no rita-code po-no) NO-LOCK
            WHERE bf-fg-rcpth.company EQ ipcCompany 
            AND bf-fg-rcpth.i-no    EQ cFGItem
            AND bf-fg-rcpth.po-no   NE "" 
            AND bf-fg-rcpth.rita-code EQ "R",
            FIRST bf-fg-rdtlh NO-LOCK 
            WHERE bf-fg-rdtlh.r-no      EQ bf-fg-rcpth.r-no 
            AND bf-fg-rdtlh.rita-code EQ bf-fg-rcpth.rita-code             
            BY bf-fg-rcpth.trans-date DESCENDING
            BY bf-fg-rdtlh.trans-time DESCENDING:
        
            FIND FIRST bf-po-ord NO-LOCK 
                WHERE bf-po-ord.company EQ ipcCompany 
                AND bf-po-ord.po-no   EQ INTEGER(bf-fg-rcpth.po-no)
                NO-ERROR.
            IF AVAILABLE bf-po-ord AND tt-report.key-03 EQ bf-po-ord.vend-no THEN
                cVendID = bf-po-ord.vend-no.
                                 
            IF AVAILABLE bf-po-ord AND tt-report.key-03 EQ bf-po-ord.vend-no AND NOT iplItemType THEN /* FG Item Type */
                tt-report.po-no = bf-fg-rcpth.po-no.
            
            LEAVE.
        END.
    END.
  
    lIsABoard = TRUE.

    IF AVAILABLE bf-job-mat THEN 
    DO:  
        FIND FIRST bf-job-hdr NO-LOCK 
            WHERE bf-job-hdr.company EQ bf-job-mat.company
            AND bf-job-hdr.job-no  EQ bf-job-mat.job-no
            AND bf-job-hdr.job-no2 EQ bf-job-mat.job-no2
            NO-ERROR.
      
        FIND FIRST bf-item NO-LOCK 
            WHERE bf-item.company EQ bf-job-mat.company
            AND bf-item.i-no    EQ bf-job-mat.i-no
            NO-ERROR.
        IF AVAILABLE bf-item AND bf-item.mat-type NE "B" THEN
            lIsABoard = FALSE.
    END.
    
    IF AVAILABLE bf-job-hdr AND bf-job-hdr.est-no GT "" THEN
        FIND FIRST bf-est NO-LOCK 
            WHERE bf-est.company EQ bf-job-hdr.company
            AND bf-est.est-no  EQ bf-job-hdr.est-no
            NO-ERROR.

    IF AVAILABLE bf-est AND lIsABoard THEN 
    DO:
        ADDER-BLOCK:
        FOR EACH tt-report.   
            FOR EACH bf-ef NO-LOCK
                WHERE bf-ef.company EQ bf-est.company
                AND bf-ef.est-no  EQ bf-est.est-no
                AND CAN-FIND(FIRST eb OF bf-ef WHERE NOT eb.pur-man):
                DO iIndex = 1 TO 6:
                    IF bf-ef.adder[iIndex] NE "" AND
                        NOT CAN-FIND(FIRST vendItemCost 
                        WHERE vendItemCost.company = ipcCompany
                        AND vendItemCost.itemID  = bf-ef.adder[iIndex]
                        AND vendItemCost.vendorID = tt-report.key-03) THEN 
                    DO:
                        DELETE tt-report.
                        NEXT ADDER-BLOCK.
                    END.
                END.
            END.
        END.
    END.

    RELEASE bf-report.
    RELEASE bf-unique-report.
    RELEASE bf-vendItemCostLevel.
    RELEASE bf-vendItemCost.
    RELEASE bf-job-mat.
    RELEASE bf-job-hdr.
    RELEASE bf-vend.
    RELEASE bf-fg-rcpth.
    RELEASE bf-fg-rdtlh.
    RELEASE bf-est.
    RELEASE bf-ef.
    RELEASE bf-po-ord.
    RELEASE bf-item.
    RELEASE bf-itemfg.
    
    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE pBuildVendorCostFromReportUsingOldTables PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Build vendor cost temp-table tt-report from report table using
              old tables
     Notes: This is exact replication of internal procedure build-table in 
            po/d-vndcst.w
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTerm     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipriJobMat  AS RECID     NO-UNDO.
    DEFINE INPUT  PARAMETER iplItemType AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQty      AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUom      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cUom       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQty       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCalcCost  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSetup     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lIsABoard  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cFGItem    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVendID    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex     AS INTEGER   NO-UNDO.

    DEFINE VARIABLE dWid       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dLen       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dDimCharge AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQtyComp   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dBasisW    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dDep       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cRtnChar   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lCorr      AS LOGICAL   NO-UNDO.

    /* Empty temp-table for every build so that, when running persistently
       doesn't populate unwanted records */       
    EMPTY TEMP-TABLE tt-report.
    
    DEFINE BUFFER bf-report        FOR report.
    DEFINE BUFFER bf-unique-report FOR report.
    DEFINE BUFFER bf-e-item-vend   FOR e-item-vend.
    DEFINE BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.
    DEFINE BUFFER bf-e-itemfg      FOR e-itemfg.
    DEFINE BUFFER bf-job-mat       FOR job-mat.
    DEFINE BUFFER bf-job-hdr       FOR job-hdr.
    DEFINE BUFFER bf-vend          FOR vend.
    DEFINE BUFFER bf-fg-rcpth      FOR fg-rcpth.
    DEFINE BUFFER bf-fg-rdtlh      FOR fg-rdtlh.
    DEFINE BUFFER bf-est           FOR est.
    DEFINE BUFFER bf-ef            FOR ef.
    DEFINE BUFFER bf-po-ord        FOR po-ord.
    DEFINE BUFFER bf-item          FOR item.
    DEFINE BUFFER bf-itemfg        FOR itemfg.
    
    FIND FIRST bf-job-mat NO-LOCK
        WHERE RECID(bf-job-mat) EQ ipriJobMat
        NO-ERROR.

    /* Code to fetch sys-ctrl configuration "MSFCALC" log field */
    RUN sys/ref/nk1look.p (
        INPUT  ipcCompany, /* Company       */
        INPUT  "MSFCALC",  /* Sys-Ctrl Name */
        INPUT  "C",        /* Logical       */
        INPUT  NO,         /* Check by cust */
        INPUT  YES,        /* Use Cust      */
        INPUT  "",         /* Customer      */
        INPUT  "",         /* Ship-to       */
        OUTPUT cRtnChar,
        OUTPUT lRecFound
        ).
    IF lRecFound THEN
        lCorr = cRtnChar = "Corrware".
                 
    FOR EACH bf-report NO-LOCK 
        WHERE bf-report.term-id EQ ipcTerm:

        FIND bf-unique-report EXCLUSIVE-LOCK 
            WHERE ROWID(bf-unique-report) EQ ROWID(bf-report) NO-ERROR NO-WAIT.
        IF LOCKED(bf-unique-report) OR NOT AVAILABLE bf-unique-report THEN
            NEXT.
            
        FIND FIRST bf-e-item-vend NO-LOCK 
            WHERE RECID(bf-e-item-vend) EQ bf-unique-report.rec-id
            NO-ERROR.
        IF NOT AVAILABLE bf-e-item-vend THEN
            FIND FIRST bf-e-itemfg-vend NO-LOCK
                WHERE RECID(bf-e-itemfg-vend) EQ bf-unique-report.rec-id
                NO-ERROR.
        
        ASSIGN
            cUom    = ""
            cVendID = ""
            .
        
        IF AVAILABLE bf-e-item-vend THEN
            ASSIGN
                cUom    = bf-e-item-vend.std-uom
                cVendID = bf-e-item-vend.vend-no
                .
        ELSE IF AVAILABLE bf-e-itemfg-vend THEN
                ASSIGN
                    cUom    = bf-e-itemfg-vend.std-uom
                    cVendID = bf-e-itemfg-vend.vend-no
                    .
                                   
        dCalcCost = DECIMAL(bf-unique-report.key-01).

        IF cUom EQ "" THEN 
        DO:
            FIND FIRST bf-e-itemfg NO-LOCK 
                WHERE bf-e-itemfg.company EQ ipcCompany
                AND bf-e-itemfg.i-no    EQ bf-unique-report.key-08
                NO-ERROR.
            IF AVAILABLE bf-e-itemfg THEN
                cUom = bf-e-itemfg.std-uom.
        END.
               
        IF bf-unique-report.key-08 EQ "RECALC" AND AVAILABLE(bf-e-itemfg-vend) AND ipdQty GT 0 THEN 
        DO:      
            FIND FIRST bf-itemfg NO-LOCK
                WHERE bf-itemfg.company EQ ipcCompany
                AND bf-itemfg.i-no    EQ bf-e-itemfg-vend.i-no 
                NO-ERROR.
            IF AVAILABLE bf-itemfg THEN 
                ASSIGN
                    dWid    = bf-itemfg.t-wid
                    dLen    = bf-itemfg.t-len
                    dDep    = 0
                    dBasisW = bf-itemfg.t-wid * bf-itemfg.t-len * 100
                    dBasisW = bf-itemfg.weight-100 /
                               (IF lCorr THEN 
                                    (dBasisW * .007)
                                ELSE 
                                    (dBasisW / 144) / 1000).
                                     
            DO iIndex = 1 TO EXTENT(bf-e-itemfg-vend.run-qty):      
                IF ipdQty LE bf-e-itemfg-vend.run-qty[iIndex] THEN
                    LEAVE.
            END.
            
            IF iIndex GT 20 THEN
                iIndex = 20.
  
            ASSIGN
                dSetup     = bf-e-itemfg-vend.setups[iIndex]
                dDimCharge = 0
                .
     
            RUN est/dim-charge.p (
                INPUT        bf-e-itemfg-vend.rec_key,
                INPUT        dWid,
                INPUT        dLen,
                INPUT-OUTPUT dDimCharge
                ) NO-ERROR.     
            
            dCalcCost  = bf-e-itemfg-vend.run-cost[iIndex] + dDimCharge.
                                     
            ASSIGN
                bf-unique-report.key-01 = STRING(dCalcCost)
                bf-unique-report.key-02 = STRING(ipdQty)
                bf-unique-report.key-05 = STRING((dSetup / ipdQty), "9999999999.9999")
                bf-unique-report.key-06 = STRING(dSetup,"9999999999.9999")
                .       
        END.
      
        FIND FIRST bf-vend NO-LOCK
            WHERE bf-vend.company EQ ipcCompany
            AND bf-vend.vend-no EQ cVendID
            NO-ERROR.
       
        CREATE tt-report.
        ASSIGN 
            tt-report.key-02      = IF bf-unique-report.key-08 EQ "RECALC" THEN 
                                           STRING(ipdQty) 
                                       ELSE 
                                           bf-unique-report.key-02
            tt-report.key-03      = bf-unique-report.key-03
            tt-report.key-04      = bf-unique-report.key-04
            tt-report.vend-name   = IF AVAILABLE bf-vend THEN 
                                           bf-vend.name 
                                       ELSE 
                                           ""
            tt-report.report-cost = dCalcCost
            tt-report.disc-days   = IF AVAILABLE bf-vend THEN 
                                           bf-vend.disc-days 
                                       ELSE
                                           0 
            tt-report.ext-price   = DECIMAL(bf-unique-report.key-02) * tt-report.report-cost
            tt-report.rec-id      = RECID(bf-unique-report)
            tt-report.cost-uom    = cUom
            .

        IF AVAILABLE bf-e-itemfg-vend THEN
            ASSIGN
                tt-report.vend-item = bf-e-itemfg-vend.vend-item
                tt-report.wid-min   = bf-e-itemfg-vend.roll-w[27]
                tt-report.wid-max   = bf-e-itemfg-vend.roll-w[28]
                tt-report.len-min   = bf-e-itemfg-vend.roll-w[29]
                tt-report.len-max   = bf-e-itemfg-vend.roll-w[30]
                .
       
        IF AVAILABLE bf-job-mat THEN
            FIND FIRST bf-job-hdr NO-LOCK 
                WHERE bf-job-hdr.company EQ bf-job-mat.company
                AND bf-job-hdr.job-no  EQ bf-job-mat.job-no
                AND bf-job-hdr.job-no2 EQ bf-job-mat.job-no2
                NO-ERROR.
      
        IF AVAILABLE bf-job-hdr THEN
            cFGItem  = bf-job-hdr.i-no.
        ELSE
            cFGItem  = ipcItemID.

        FOR EACH bf-fg-rcpth FIELDS(r-no rita-code po-no) NO-LOCK
            WHERE bf-fg-rcpth.company EQ ipcCompany 
            AND bf-fg-rcpth.i-no    EQ cFGItem
            AND bf-fg-rcpth.po-no   NE "" 
            AND bf-fg-rcpth.rita-code EQ "R",
            FIRST bf-fg-rdtlh NO-LOCK 
            WHERE bf-fg-rdtlh.r-no      EQ bf-fg-rcpth.r-no 
            AND bf-fg-rdtlh.rita-code EQ bf-fg-rcpth.rita-code             
            BY bf-fg-rcpth.trans-date DESCENDING
            BY bf-fg-rdtlh.trans-time DESCENDING:
        
            FIND FIRST bf-po-ord NO-LOCK 
                WHERE bf-po-ord.company EQ ipcCompany 
                AND bf-po-ord.po-no   EQ INTEGER(bf-fg-rcpth.po-no)
                NO-ERROR.
            IF AVAILABLE bf-po-ord AND tt-report.key-03 EQ bf-po-ord.vend-no THEN
                cVendID = bf-po-ord.vend-no.
                                 
            IF AVAILABLE bf-po-ord AND tt-report.key-03 EQ bf-po-ord.vend-no AND NOT iplItemType THEN /* FG Item Type */
                tt-report.po-no = bf-fg-rcpth.po-no.
            
            LEAVE.
        END.
    END.
  
    lIsABoard = TRUE.

    IF AVAILABLE bf-job-mat THEN 
    DO:  
        FIND FIRST bf-job-hdr NO-LOCK 
            WHERE bf-job-hdr.company EQ bf-job-mat.company
            AND bf-job-hdr.job-no  EQ bf-job-mat.job-no
            AND bf-job-hdr.job-no2 EQ bf-job-mat.job-no2
            NO-ERROR.
      
        FIND FIRST bf-item NO-LOCK 
            WHERE bf-item.company EQ bf-job-mat.company
            AND bf-item.i-no    EQ bf-job-mat.i-no
            NO-ERROR.
        IF AVAILABLE bf-item AND bf-item.mat-type NE "B" THEN
            lIsABoard = FALSE.
    END.
    
    IF AVAILABLE bf-job-hdr AND bf-job-hdr.est-no GT "" THEN
        FIND FIRST bf-est NO-LOCK 
            WHERE bf-est.company EQ bf-job-hdr.company
            AND bf-est.est-no  EQ bf-job-hdr.est-no
            NO-ERROR.

    IF AVAILABLE bf-est AND lIsABoard THEN 
    DO:
        ADDER-BLOCK:
        FOR EACH tt-report.   
            FOR EACH bf-ef NO-LOCK
                WHERE bf-ef.company EQ bf-est.company
                AND bf-ef.est-no  EQ bf-est.est-no
                AND CAN-FIND(FIRST eb OF bf-ef WHERE NOT eb.pur-man):
                DO iIndex = 1 TO 6:
                    IF bf-ef.adder[iIndex] NE "" AND
                        NOT CAN-FIND(FIRST e-item-vend
                        WHERE e-item-vend.company EQ ipcCompany
                        AND e-item-vend.i-no    EQ bf-ef.adder[iIndex]
                        AND e-item-vend.vend-no EQ tt-report.key-03) THEN 
                    DO:
                        DELETE tt-report.
                        NEXT ADDER-BLOCK.
                    END.
                END.
            END.
        END.
    END.

    RELEASE bf-report.
    RELEASE bf-unique-report.
    RELEASE bf-e-item-vend.
    RELEASE bf-e-itemfg-vend.
    RELEASE bf-job-mat.
    RELEASE bf-job-hdr.
    RELEASE bf-vend.
    RELEASE bf-fg-rcpth.
    RELEASE bf-fg-rdtlh.
    RELEASE bf-est.
    RELEASE bf-ef.
    RELEASE bf-po-ord.
    RELEASE bf-e-itemfg.
    RELEASE bf-item.
    RELEASE bf-itemfg.
    
    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
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
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
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
    DEFINE VARIABLE cMessage                AS CHARACTER NO-UNDO.

    /*Refactor handling of non-assumed UOMs of "IN" and "LBS/MSF" and handle error & message propagation*/
    ASSIGN 
        dBasisWeightInLBSPerMSF = ipdBasisWeight
        dDimLengthInIN          = ipdDimLength
        dDimWidthInIN           = ipdDimWidth
        dDimDepthInIN           = ipdDimDepth
        .
    RUN Conv_QuantityFromUOMToUOM (ipcCompany, ipcItemID, ipcItemType, ipdQtyInFromUOM, ipcFromUOM , ipcToUOM, 
        dBasisWeightInLBSPerMSF,
        dDimLengthInIN, 
        dDimWidthInIN, 
        dDimDepthInIN, 
        0,
        OUTPUT opdQtyInToUOM,
        OUTPUT oplError,
        OUTPUT cMessage
        ). 
    iopcMessage = iopcMessage + cMessage.

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
    DEFINE OUTPUT PARAMETER oplIsSelected AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiLeadTime AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostDeviation AS DECIMAL NO-UNDO.
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
            opdCostPerUOM    = bf-vendItemCostLevel.costPerUOM
            opdCostSetup     = bf-vendItemCostLevel.costSetup
            oplIsSelected    = bf-vendItemCostLevel.useForBestCost
            opiLeadTime      = bf-vendItemCostLevel.leadTimeDays
            opdCostDeviation = bf-vendItemCostLevel.costDeviation
            oplError         = NO
            iopcMessage      = iopcMessage + " in range." 
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
        dCostUpChargeUnderLength = IF dDimLengthInVendorDimUOM LT ipbf-vendItemCost.dimLengthUnder THEN ipbf-vendItemCost.dimLengthUnderCharge ELSE 0.
    IF ipdDimWidth NE 0 AND ipbf-vendItemCost.dimWidthUnder NE 0 THEN 
        dCostUpChargeUnderWidth = IF dDimWidthInVendorDimUOM LT ipbf-vendItemCost.dimWidthUnder THEN ipbf-vendItemCost.dimWidthUnderCharge ELSE 0.
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
    DEFINE VARIABLE cEstNoFromItem  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDefaultVendorCostStatusFGIApproval AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVendorCostMatrixUseEstimate        AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-itemfg FOR itemfg.
    
    RUN spGetSettingByName ("DefaultVendorCostStatusFGIApproval", OUTPUT cDefaultVendorCostStatusFGIApproval).
    
    RUN spGetSettingByName ("VendorCostMatrixUseEstimate", OUTPUT cVendorCostMatrixUseEstimate).
        
    &SCOPED-DEFINE RequiredCriteria ~
    WHERE opbf-vendItemCost.company           EQ ipcCompany ~
      AND opbf-vendItemCost.itemID            EQ ipcItemID ~
      AND opbf-vendItemCost.itemType          EQ ipcItemType ~
      AND opbf-vendItemCost.effectiveDate     LE TODAY ~
      AND (opbf-vendItemCost.expirationDate   GE TODAY ~
       OR opbf-vendItemCost.expirationDate    EQ ? ~
       OR opbf-vendItemCost.expirationDate    EQ 01/01/0001) ~
      AND ((opbf-vendItemCost.approved        EQ YES ~
      AND cDefaultVendorCostStatusFGIApproval EQ "Yes" ~
      AND opbf-vendItemCost.itemType          EQ "FG") ~
       OR cDefaultVendorCostStatusFGIApproval EQ "No" ~
       OR opbf-vendItemCost.itemType          EQ "RM")

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
        AND (opbf-vendItemCost.estimateNo EQ ipcEstimateNo OR cVendorCostMatrixUseEstimate EQ "No")
        AND (opbf-vendItemCost.formNo EQ ipiFormNo OR cVendorCostMatrixUseEstimate EQ "No")
        AND (opbf-vendItemCost.blankNo EQ ipiBlankNo OR cVendorCostMatrixUseEstimate EQ "No")
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
                        AND (opbf-vendItemCost.estimateNo EQ ipcEstimateNo OR cVendorCostMatrixUseEstimate EQ "No")
                        AND (opbf-vendItemCost.formNo EQ ipiFormNo OR cVendorCostMatrixUseEstimate EQ "No")
                        AND (opbf-vendItemCost.blankNo EQ ipiBlankNo OR cVendorCostMatrixUseEstimate EQ "No")
                        AND opbf-vendItemCost.vendorID EQ ""
                        NO-ERROR.
                    IF NOT AVAILABLE opbf-vendItemCost THEN 
                        FIND FIRST opbf-vendItemCost NO-LOCK /*Match with blank vendor blank customer*/
                            {&RequiredCriteria}
                        AND (opbf-vendItemCost.estimateNo EQ ipcEstimateNo OR cVendorCostMatrixUseEstimate EQ "No")
                        AND (opbf-vendItemCost.formNo EQ ipiFormNo OR cVendorCostMatrixUseEstimate EQ "No")
                        AND (opbf-vendItemCost.blankNo EQ ipiBlankNo OR cVendorCostMatrixUseEstimate EQ "No")
                        NO-ERROR.
                    IF AVAILABLE opbf-vendItemCost THEN 
                        cMsgUsing = cMsgConstUsing + cMsgConstVend + (IF opbf-vendItemCost.vendorID EQ "" THEN cMsgConstBlank ELSE opbf-vendItemCost.vendorID).
                END.
                ELSE 
                DO: /*Fall back options for PO*/
                    cMsgInputs = cMsgFGInputs.
                    FIND FIRST opbf-vendItemCost NO-LOCK /*Match with blank est and non-blank customer*/
                        {&RequiredCriteria}
                        AND (opbf-vendItemCost.estimateNo EQ "" OR cVendorCostMatrixUseEstimate EQ "No")
                        AND opbf-vendItemCost.vendorID EQ ipcVendorID
                        AND opbf-vendItemCost.customerID EQ ipcCustomerID
                        NO-ERROR.
                    IF NOT AVAILABLE opbf-vendItemCost THEN 
                        FIND FIRST opbf-vendItemCost NO-LOCK /*Match with blank customer*/
                            {&RequiredCriteria}
                        AND (opbf-vendItemCost.estimateNo EQ "" OR cVendorCostMatrixUseEstimate EQ "No")
                        AND opbf-vendItemCost.vendorID EQ ipcVendorID
                        AND opbf-vendItemCost.customerID EQ ""
                        NO-ERROR.
                    IF NOT AVAILABLE opbf-vendItemCost THEN 
                        FIND FIRST opbf-vendItemCost NO-LOCK /*Match with blank vendor blank customer*/
                            {&RequiredCriteria}
                        AND (opbf-vendItemCost.estimateNo EQ "" OR cVendorCostMatrixUseEstimate EQ "No")
                        AND opbf-vendItemCost.vendorID EQ ""
                        AND opbf-vendItemCost.customerID EQ ""
                        NO-ERROR.
                    IF NOT AVAILABLE opbf-vendItemCost THEN 
                    DO: /*Match based on estimate for FG Item #*/
                        FIND FIRST bf-itemfg NO-LOCK 
                            WHERE bf-itemfg.company EQ ipcCompany
                            AND bf-itemfg.i-no EQ ipcItemID
                            NO-ERROR.
                        IF AVAILABLE bf-itemfg AND TRIM(bf-itemfg.est-no) NE "" THEN 
                        DO:
                            cEstNoFromItem = bf-itemfg.est-no.
                            FIND FIRST opbf-vendItemCost NO-LOCK /*Match with estimate from FG Item and customer*/
                                {&RequiredCriteria}
                                AND (opbf-vendItemCost.estimateNo EQ cEstNoFromItem OR cVendorCostMatrixUseEstimate EQ "No")
                                AND opbf-vendItemCost.vendorID EQ ipcVendorID
                                AND opbf-vendItemCost.customerID EQ ipcCustomerID
                                NO-ERROR.
                            IF NOT AVAILABLE opbf-vendItemCost THEN
                                FIND FIRST opbf-vendItemCost NO-LOCK /*Match with blank customer*/
                                    {&RequiredCriteria}
                                AND (opbf-vendItemCost.estimateNo EQ cEstNoFromItem OR cVendorCostMatrixUseEstimate EQ "No")
                                AND opbf-vendItemCost.vendorID EQ ipcVendorID
                                AND opbf-vendItemCost.customerID EQ ""
                                NO-ERROR.
                            IF NOT AVAILABLE opbf-vendItemCost THEN 
                                FIND FIRST opbf-vendItemCost NO-LOCK /*Match with blank vendor blank customer*/
                                    {&RequiredCriteria}
                                AND (opbf-vendItemCost.estimateNo EQ cEstNoFromItem OR cVendorCostMatrixUseEstimate EQ "No")
                                AND opbf-vendItemCost.vendorID EQ ""
                                AND opbf-vendItemCost.customerID EQ ""
                                NO-ERROR.
                        END.
                    END.
                    IF AVAILABLE opbf-vendItemCost THEN 
                        ASSIGN 
                            cMsgUsing = cMsgConstUsing + cMsgConstVend + (IF opbf-vendItemCost.vendorID EQ "" THEN cMsgConstBlank ELSE opbf-vendItemCost.vendorID)
                            cMsgUsing = cMsgUsing + cMsgConstCust + (IF opbf-vendItemCost.customerID EQ "" THEN cMsgConstBlank ELSE opbf-vendItemCost.customerID)
                            cMsgUsing = cMsgUsing + cMsgConstEst + (IF opbf-vendItemCost.EstimateNo EQ "" THEN cMsgConstBlank ELSE opbf-vendItemCost.estimateNo + " from FG Item")
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
         RUN pGetVendorCosts(BUFFER bf-vendItemCost, dQuantityInVendorUOM, ipdDimLength, ipdDimWidth, ipcDimUOM, 
            OUTPUT opdCostPerUOM, OUTPUT opdCostSetup, OUTPUT dCostPerUOMUpcharge, OUTPUT dCostPerUOMBase,
            OUTPUT opdCostTotal, OUTPUT lIsSelected, 
            OUTPUT iLeadDays, OUTPUT dCostDeviation,
            OUTPUT oplError, INPUT-OUTPUT opcMessage).
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-vendItemCost FOR vendItemCost.
    DEFINE INPUT PARAMETER ipdQuantityInVendorUOM AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimWidth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcDimUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostSetup AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMUpcharge AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMBase AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsSelected AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiLeadDays AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostDeviation AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcMessage AS CHARACTER NO-UNDO.


    RUN pGetCostsForVendItemCost(BUFFER ipbf-vendItemCost, ipdQuantityInVendorUOM,
        OUTPUT opdCostPerUOMBase, OUTPUT opdCostSetup, OUTPUT oplIsSelected, OUTPUT opiLeadDays, OUTPUT opdCostDeviation, OUTPUT oplError, INPUT-OUTPUT iopcMessage).   
    IF NOT oplError THEN 
    DO:
        RUN pGetUpchargeCostsForVendItemCost(BUFFER ipbf-vendItemCost, ipdDimLength, ipdDimWidth, ipcDimUOM, 
            OUTPUT opdCostPerUOMUpcharge, OUTPUT oplError, INPUT-OUTPUT iopcMessage).             
        ASSIGN 
            opdCostPerUOM = opdCostPerUOMBase + opdCostPerUOMUpcharge
            opdCostTotal  = opdCostPerUOM * ipdQuantityInVendorUOM + opdCostSetup
            . 
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

PROCEDURE VendCost_GetBestVendorWithAdders:
    DEFINE INPUT PARAMETER ipcCompany            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcScope              AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeBlankVendor AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo             AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo            AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity           AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimLength          AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimWidth           AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimDepth           AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcDimUOM             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdBasisWeight        AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcBasisWeightUOM     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAdderList          AS CHARACTER EXTENT 6 NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBestCostVendorID  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.
    
      RUN pGetBestVendor(ipcCompany, 
                         ipcItemID, 
                         ipcItemType, 
                         ipcScope, 
                         iplIncludeBlankVendor,
                         ipcEstimateNo,
                         ipiFormNo,
                         ipiBlankNo, 
                         ipdQuantity, 
                         ipcQuantityUOM, 
                         ipdDimLength ,
                         ipdDimWidth ,
                         ipdDimDepth ,
                         ipcDimUOM ,
                         ipdBasisWeight, 
                         ipcBasisWeightUOM, 
                         ipcAdderList ,
                         OUTPUT opcBestCostVendorID, 
                         OUTPUT oplError ,
                         OUTPUT opcMessage).  
    
END.

PROCEDURE VendCost_GetBestVendor:
    DEFINE INPUT PARAMETER ipcCompany            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcScope              AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeBlankVendor AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo             AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo            AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity           AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimLength          AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimWidth           AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimDepth           AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcDimUOM             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdBasisWeight        AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcBasisWeightUOM     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBestCostVendorID  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cAdderList AS CHARACTER EXTENT 6.
    
    RUN pGetBestVendor(ipcCompany, 
        ipcItemID, 
        ipcItemType, 
        ipcScope, 
        iplIncludeBlankVendor,
        ipcEstimateNo,
        ipiFormNo,
        ipiBlankNo, 
        ipdQuantity, 
        ipcQuantityUOM, 
        ipdDimLength ,
        ipdDimWidth ,
        ipdDimDepth ,
        ipcDimUOM ,
        ipdBasisWeight, 
        ipcBasisWeightUOM, 
        cAdderList ,
        OUTPUT opcBestCostVendorID, 
        OUTPUT oplError ,
        OUTPUT opcMessage).  
    
END.

PROCEDURE pGetBestVendor PRIVATE:
    DEFINE INPUT PARAMETER ipcCompany            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcScope              AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeBlankVendor AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo             AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo            AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity           AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimLength          AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimWidth           AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdDimDepth           AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcDimUOM             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdBasisWeight        AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcBasisWeightUOM     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAdderList          AS CHARACTER EXTENT 6 NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBestCostVendorID  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.

 
    RUN BuildVendItemCostsWithAdders(ipcCompany, ipcItemID, ipcItemType, ipcScope, iplIncludeBlankVendor,
        ipcEstimateNo, ipiFormNo, ipiBlankNo,
        ipdQuantity, ipcQuantityUOM, 
        ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM,
        ipdBasisWeight, ipcBasisWeightUOM, ipcAdderList, 
        OUTPUT TABLE ttVendItemCost,
        OUTPUT oplError, OUTPUT opcMessage).
        
    FOR EACH ttVendItemCost NO-LOCK
        WHERE ttVendItemCost.isSelected
        AND ttVendItemCost.isValid
        AND ttVendItemCost.costTotal GT 0
        BY ttVendItemCost.costTotal:
        opcBestCostVendorID  = ttVendItemCost.vendorID.
        LEAVE.
    END. 
    IF opcBestCostVendorID = "" THEN
        FOR EACH ttVendItemCost NO-LOCK
            WHERE ttVendItemCost.isValid
            AND ttVendItemCost.costTotal GT 0
            BY ttVendItemCost.costTotal:
            opcBestCostVendorID  = ttVendItemCost.vendorID.
            LEAVE.
        END.            

    EMPTY TEMP-TABLE ttVendItemCost.
END PROCEDURE.

PROCEDURE VendCost_GetBestCost:
    /*------------------------------------------------------------------------------
     Purpose:  Given all inputs, determine the best cost per UOM, Setup and VendorID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcScope AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeBlankVendor AS LOGICAL NO-UNDO.
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
    DEFINE OUTPUT PARAMETER opdBestCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBestCostVendorUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdBestCostSetup AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBestCostVendorID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdBestCostDeviation AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdBestCostTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-ef FOR ef.
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    RUN BuildVendItemCosts(ipcCompany, ipcItemID, ipcItemType, ipcScope, iplIncludeBlankVendor,
        ipcEstimateNo, ipiFormNo, ipiBlankNo,
        ipdQuantity, ipcQuantityUOM, 
        ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM,
        ipdBasisWeight, ipcBasisWeightUOM, 
        OUTPUT TABLE ttVendItemCost,
        OUTPUT oplError, OUTPUT opcMessage).
    /*Find "selected" vendor level first*/
    FOR EACH ttVendItemCost NO-LOCK
        WHERE ttVendItemCost.isSelected
        AND ttVendItemCost.isValid
        AND ttVendItemCost.costTotal GT 0
        BY ttVendItemCost.costTotal:
        ASSIGN 
            opdBestCostPerUOM    = ttVendItemCost.costPerVendorUOM
            opdBestCostSetup     = ttVendItemCost.costSetup
            opcBestCostVendorID  = ttVendItemCost.vendorID 
            opcBestCostVendorUOM = ttVendItemCost.vendorUOM
            opdBestCostDeviation = ttVendItemCost.costDeviation
            opdBestCostTotal     = ttVendItemCost.costTotal
            .
        LEAVE.
    END. 
    /*If no "Selected" levels, pick the best vendor*/
    IF opdBestCostPerUOM EQ 0 AND opdBestCostSetup EQ 0 THEN
        FOR EACH ttVendItemCost NO-LOCK
            WHERE ttVendItemCost.isValid
            AND ttVendItemCost.costTotal GT 0
            BY ttVendItemCost.costTotal:
            ASSIGN 
                opdBestCostPerUOM    = ttVendItemCost.costPerVendorUOM
                opdBestCostSetup     = ttVendItemCost.costSetup
                opcBestCostVendorID  = ttVendItemCost.vendorID 
                opcBestCostVendorUOM = ttVendItemCost.vendorUOM
                opdBestCostDeviation = ttVendItemCost.costDeviation
                opdBestCostTotal     = ttVendItemCost.costTotal
                .
            LEAVE.
        END.
            
END PROCEDURE.

PROCEDURE VendCost_GetWorstCost:
    /*------------------------------------------------------------------------------
        Purpose:  Given all inputs, determine the worst cost per UOM, Setup and VendorID
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcScope AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeBlankVendor AS LOGICAL NO-UNDO.
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
    DEFINE OUTPUT PARAMETER opdBestCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBestCostVendorUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdBestCostSetup AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBestCostVendorID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdBestCostDeviation AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    RUN BuildVendItemCosts(ipcCompany, ipcItemID, ipcItemType, ipcScope, iplIncludeBlankVendor,
        ipcEstimateNo, ipiFormNo, ipiBlankNo,
        ipdQuantity, ipcQuantityUOM, 
        ipdDimLength, ipdDimWidth, ipdDimDepth, ipcDimUOM,
        ipdBasisWeight, ipcBasisWeightUOM, 
        OUTPUT TABLE ttVendItemCost,
        OUTPUT oplError, OUTPUT opcMessage).
    FOR EACH ttVendItemCost NO-LOCK
        WHERE ttVendItemCost.isValid
        AND ttVendItemCost.costTotal GT 0
        BY ttVendItemCost.costTotal DESCENDING:
        ASSIGN 
            opdBestCostPerUOM    = ttVendItemCost.costPerVendorUOM
            opcBestCostVendorUOM = ttVendItemCost.vendorUOM
            opdBestCostSetup     = ttVendItemCost.costSetup
            opcBestCostVendorID  = ttVendItemCost.vendorID 
            opdBestCostDeviation = ttVendItemCost.costDeviation
            .
        RETURN.
    END. 


END PROCEDURE.

PROCEDURE VendCost_UpdateItemFGVend:
    /*------------------------------------------------------------------------------
     Purpose: To Update the FG Item in e-itemfg-vend
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimate AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiForm     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlank    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcOldFG    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcNewFG    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiEQty     AS INTEGER   NO-UNDO.
  
    DEFINE BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.
    DEFINE BUFFER e-itemfg-vend    FOR e-itemfg-vend.

    FOR EACH e-itemfg-vend NO-LOCK
        WHERE e-itemfg-vend.company  EQ ipcCompany 
        AND e-itemfg-vend.est-no   EQ ipcEstimate
        AND e-itemfg-vend.form-no  EQ ipiForm
        AND e-itemfg-vend.blank-no EQ ipiBlank
        AND e-itemfg-vend.i-no     EQ ipcOldFG:
        FIND FIRST bf-e-itemfg-vend EXCLUSIVE-LOCK
            WHERE bf-e-itemfg-vend.company  EQ e-itemfg-vend.company
            AND bf-e-itemfg-vend.est-no   EQ e-itemfg-vend.est-no
            AND bf-e-itemfg-vend.form-no  EQ e-itemfg-vend.form-no
            AND bf-e-itemfg-vend.blank-no EQ e-itemfg-vend.blank-no
            AND bf-e-itemfg-vend.i-no     EQ e-itemfg-vend.i-no
            NO-ERROR.
        IF AVAILABLE bf-e-itemfg-vend THEN 
        DO:
            ASSIGN 
                bf-e-itemfg-vend.i-no = ipcNewFG
                bf-e-itemfg-vend.eQty = IF bf-e-itemfg-vend.eQty NE ipiEQty THEN ipiEQty 
                                        ELSE bf-e-itemfg-vend.eQty
                .                                   
        END.                 
    END. 
    RELEASE bf-e-itemfg-vend. 


END PROCEDURE.

PROCEDURE VendCost_UpdateVendItemCost:
    /*------------------------------------------------------------------------------
     Purpose: To Update the FG Item in VendItemCost
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimate AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiForm     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlank    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcOldItem  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcNewItem  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER vendItemCost         FOR vendItemCost.    
    DEFINE BUFFER bf-vendItemCost      FOR vendItemCost.
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
    
    FOR EACH vendItemCost NO-LOCK
        WHERE vendItemCost.company  EQ ipcCompany
        AND vendItemCost.estimate EQ ipcEstimate 
        AND vendItemCost.formNo   EQ ipiForm
        AND vendItemCost.blankNo  EQ ipiBlank
        AND vendItemCost.itemID   EQ ipcOldItem:
        FIND FIRST bf-vendItemCost EXCLUSIVE-LOCK
            WHERE ROWID(bf-vendItemCost)  EQ ROWID(vendItemCost)             
            NO-ERROR.
        IF AVAILABLE bf-vendItemCost THEN 
            bf-vendItemCost.itemID = ipcNewItem.                       
    END.
    RELEASE bf-vendItemCost.
END PROCEDURE.

PROCEDURE VendCost_GetVendorItemID:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper function for GetVendorItem that returns first vendor item #
        for a given item/type/vendor.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendorID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcVendorItemID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

    RUN GetVendorItem(ipcCompany, ipcItemID, ipcItemType, ipcVendorID, 
        "","", 0, 0, NO, 
        OUTPUT opcVendorItemID, 
        OUTPUT lError, OUTPUT cMessage).

END PROCEDURE.

PROCEDURE Vendor_GetVendItemNumber:
    /*------------------------------------------------------------------------------
     Purpose: To get VendorItemID based on company, item and vendor
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcVendNo      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplUseVendCost AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcVendItemNo  AS CHARACTER NO-UNDO.
    
    IF iplUseVendCost THEN 
    DO:
        FIND FIRST vendItemCost NO-LOCK 
            WHERE vendItemCost.company  EQ ipcCompany
            AND vendItemCost.vendorID EQ ipcVendNo
            AND vendItemCost.itemID   EQ ipcItemID
            AND vendItemCost.itemType EQ "RM"
            NO-ERROR.
        IF AVAILABLE vendItemCost THEN 
            opcVendItemNo = vendItemCost.vendorItemID.
        ELSE 
        DO:
            FIND FIRST vendItemCost NO-LOCK 
                WHERE vendItemCost.company  EQ ipcCompany
                AND vendItemCost.vendorID EQ ""
                AND vendItemCost.itemID   EQ ipcItemID
                AND vendItemCost.itemType EQ "RM"
                NO-ERROR. 
            IF AVAILABLE vendItemCost THEN 
                opcVendItemNo = vendItemCost.vendorItemID.                        
        END.                          
    END.
    ELSE 
    DO:
        FIND FIRST e-item-vend NO-LOCK 
            WHERE e-item-vend.company EQ ipcCompany
            AND e-item-vend.vend-no EQ ipcVendNo
            AND e-item-vend.i-no    EQ ipcItemID
            NO-ERROR.
        IF AVAILABLE e-item-ven THEN 
            opcVendItemNo = e-item-vend.vend-item.
        ELSE 
        DO:
            FIND FIRST e-item-vend NO-LOCK 
                WHERE e-item-vend.company EQ ipcCompany
                AND e-item-vend.vend-no EQ ""
                AND e-item-vend.i-no    EQ ipcItemID
                NO-ERROR.
            IF AVAILABLE e-item-vend THEN 
                opcVendItemNo = e-item-vend.vend-item.            
        END.                                
    END.    

END PROCEDURE.

PROCEDURE pUpdateVendCostPriceWithPercentage PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a VendItemCostBuffer, and a quantity, get appropriate costs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-vendItemCost FOR vendItemCost.
    DEFINE INPUT PARAMETER ipdPercentage AS DECIMAL NO-UNDO.
         
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
    
    FOR EACH bf-vendItemCostLevel EXCLUSIVE-LOCK
        WHERE bf-vendItemCostLevel.vendItemCostID EQ ipbf-vendItemCost.vendItemCostID 
        BY bf-vendItemCostLevel.quantityBase:
   
        bf-vendItemCostLevel.costPerUom = bf-vendItemCostLevel.costPerUom + 
            (bf-vendItemCostLevel.costPerUom * ipdPercentage / 100).                                
    END.
    RELEASE bf-vendItemCostLevel.
      

END PROCEDURE.

PROCEDURE Vendor_ExpirePriceByItem:
    /*------------------------------------------------------------------------------
     Purpose: Expire the vendItemCost Prices based upon company and item
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-vendItemCost FOR vendItemCost.
    
    FOR EACH bf-vendItemCost EXCLUSIVE-LOCK 
        WHERE bf-vendItemCost.company        EQ ipcCompany
        AND bf-vendItemCost.ItemID         EQ ipcItemID
        AND(bf-vendItemCost.expirationDate GT TODAY OR bf-vendItemCost.expirationDate EQ ?) :
        bf-vendItemCost.expirationDate = TODAY.      
    END.

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fGetSettingAutoSetMaximum RETURNS CHARACTER PRIVATE
    ( ipcCompany AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Get the NK1 VendItemCostMaximum
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1look.p (INPUT ipcCompany, 
        INPUT "VendItemCostMaximum", 
        INPUT "C" /* Logical */, 
        INPUT  NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, 
        INPUT "" /* cust */, 
        INPUT "" /* ship-to*/,
        OUTPUT cReturn, OUTPUT lFound).
    
    RETURN cReturn.
		
END FUNCTION.

FUNCTION VendCost_GetUnlimitedQuantity RETURNS DECIMAL 
    (  ):
    /*------------------------------------------------------------------------------
    Purpose:  Returns the constant for Unlimited Value
     Notes:
    ------------------------------------------------------------------------------*/	

    RETURN gdQuantityMax.
		
END FUNCTION.

FUNCTION VendCost_GetUnlimitedLenWid RETURNS DECIMAL 
    (  ):
    /*------------------------------------------------------------------------------
    Purpose:  Returns the constant for Unlimited Value
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN gdLengthWidthMax.
        
END FUNCTION.

FUNCTION VendCost_GetValidScopes RETURNS CHARACTER 
    ( ipcContext AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: returns the global property of valid scopes
     Notes:
    ------------------------------------------------------------------------------*/	
    CASE ipcContext:
        WHEN "Est-RM" THEN 
            RETURN gcScopeRMStandard.
        WHEN "Est-FG" THEN 
            RETURN gcScopeFGEstimated.
        WHEN "Purch-RM" THEN 
            RETURN gcScopeRMStandard.
        WHEN "Purch-FG" THEN 
            RETURN gcScopeFGPurchased.
        WHEN "Est-RM-Over" THEN
            RETURN gcScopeRMOverride .    
        OTHERWISE  
        RETURN gcScopeList.
    END CASE.
		
END FUNCTION.

FUNCTION fVendCostHasEstimateOverride RETURNS LOGICAL 
    ( ipcCompany AS CHARACTER, ipcEstimateID AS CHARACTER ,ipiFormNo AS INTEGER,ipcItemID AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: returns the global property of valid scopes
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.
    FIND FIRST vendItemCost NO-LOCK
        WHERE vendItemCost.company  EQ ipcCompany
        AND vendItemCost.estimate EQ ipcEstimateID 
        AND vendItemCost.formNo   EQ ipiFormNo        
        AND vendItemCost.itemID   EQ ipcItemID NO-ERROR.
        
    lReturn = IF AVAIL vendItemCost THEN TRUE ELSE FALSE .
    RETURN lReturn.
		
END FUNCTION.



