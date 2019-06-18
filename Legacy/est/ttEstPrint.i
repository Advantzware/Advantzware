
/*------------------------------------------------------------------------
    File        : ttEstPrint.i
    Purpose     : 

    Syntax      :

    Description : Holds tables to build the estimate print out

    Author(s)   : BV
    Created     : Wed Jan 23 17:17:51 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE {1} TEMP-TABLE ttEstHeader /*Master Print*/
    FIELD rec_Key  AS CHARACTER 
    FIELD estHeaderID AS CHARACTER /*Unique ID*/
    FIELD company AS CHARACTER
    FIELD locationID AS CHARACTER 
    FIELD cEstNo AS CHARACTER
    FIELD riEstRowID AS ROWID
    FIELD dQtyMaster AS DECIMAL /*Master Qty Calculated*/
    FIELD cCalculator AS CHARACTER /*User ID of who calculated*/
    FIELD cPrinter AS CHARACTER /*User ID of who printed*/
    FIELD cEstType AS CHARACTER /*Set, Single, Combo, Tandem*/
    FIELD cIndustry AS CHARACTER 
    FIELD dtCalcDateTime AS DATETIME 
    FIELD dtPrintDateTime AS DATETIME 
    FIELD dMarginPct AS DECIMAL 
    FIELD cMarginOn AS CHARACTER 
    FIELD dWarehouseMarkupPct AS DECIMAL 
    FIELD dHandlingChargePct AS DECIMAL 
    FIELD dHandlingRatePerCWTRMPct AS DECIMAL 
    FIELD dSpecial1MarkupPct AS DECIMAL 
    FIELD dSpecial2MarkupPct AS DECIMAL 
    FIELD dSpecial3MarkupPct AS DECIMAL
    FIELD dSpecial1FlatValue AS DECIMAL 
    FIELD dSpecial2FlatValue AS DECIMAL 
    FIELD dSpecial3FlatValue AS DECIMAL  
    FIELD lShowCommissoins AS LOGICAL
    FIELD lShowLaborRates AS LOGICAL
    FIELD lAddToFactCostSpecial1 AS LOGICAL
    FIELD lAddToFactCostSpecial2 AS LOGICAL 
    FIELD lAddToFactCostSpecial3 AS LOGICAL 
    FIELD lAddToFactCostFreight AS LOGICAL 
    FIELD lAddToFactCostGSA AS LOGICAL 
    FIELD lAddToFactCostRoyalty AS LOGICAL 
    FIELD lAddToFactCostComm AS LOGICAL 
    FIELD dFoldPct AS DECIMAL
    FIELD dHandlingRatePerCWTFGPct AS DECIMAL 
    FIELD dHandlingRatePerCWTRMFarmPct AS DECIMAL 
    FIELD dHandlingRatePerCWTFGFarmPct AS DECIMAL 
    FIELD dHandlingChargeFarmPct AS DECIMAL
    FIELD dDirectMaterialPct AS DECIMAL
    FIELD dGSAMaterialPct AS DECIMAL 
    FIELD dGSALaborPct AS DECIMAL
    FIELD lRecalcSetupTime AS LOGICAL 
    FIELD lRecalcRunSpeed AS LOGICAL 
    FIELD lRecalcRunWaste AS LOGICAL 
    FIELD lRecalcSetupCrew AS LOGICAL 
    FIELD lRecalcRunCrew AS LOGICAL 
    FIELD lRecalcSetupWaste AS LOGICAL 
    FIELD lRecalcSetupRates AS LOGICAL 
    FIELD lRecalcRunRates AS LOGICAL 
    FIELD lForRealItemsUseAvgCost AS LOGICAL
    FIELD lIsUnitizedSet AS LOGICAL
    .

DEFINE {1} TEMP-TABLE ttEstItem
    FIELD rec_key AS CHARACTER
    FIELD company AS CHARACTER 
    FIELD estItemID AS CHARACTER 
    FIELD estItemIDParent AS CHARACTER /*Link to Set*/
    FIELD estHeaderID AS CHARACTER /*Link to Header*/
    FIELD dQtyPerParent AS DECIMAL
    FIELD dQtyRequired AS DECIMAL
    FIELD dQtyYielded AS DECIMAL
    FIELD cFGItemID AS CHARACTER
    FIELD cItemName AS CHARACTER 
    FIELD cItemDescription1 AS CHARACTER
    FIELD cItemDescription2 AS CHARACTER
    FIELD cStyleID AS CHARACTER 
    FIELD cStyle AS CHARACTER  
    FIELD lIsSet AS LOGICAL
    FIELD cCustomerID AS CHARACTER
    FIELD cCustomerName AS CHARACTER  
    FIELD cCustomerAddress1 AS CHARACTER 
    FIELD cCustomerAddress2 AS CHARACTER
    FIELD cCustomerAddress3 AS CHARACTER
    FIELD cShipToID AS CHARACTER 
    FIELD cShipToName AS CHARACTER  
    FIELD cShipToAddress1 AS CHARACTER 
    FIELD cShipToAddress2 AS CHARACTER
    FIELD cShipToAddress3 AS CHARACTER
    FIELD cSalesgroupID AS CHARACTER 
    FIELD cSalesgroupName AS CHARACTER 
    FIELD cCustomerPart AS CHARACTER
    FIELD cSize AS CHARACTER
    FIELD cColor AS CHARACTER
    FIELD dCostTotalBoard AS DECIMAL 
    FIELD dCostTotalMaterial AS DECIMAL
    FIELD dCostTotalLabor AS DECIMAL 
    FIELD dCostTotalFactory AS DECIMAL
    FIELD dCostTotalNonFactory AS DECIMAL  
    FIELD dCostTotalFull AS DECIMAL

    .
   
DEFINE {1} TEMP-TABLE ttEstForm
    FIELD rec_key AS CHARACTER 
    FIELD company AS CHARACTER 
    FIELD estFormID AS CHARACTER /*Unique ID*/
    FIELD estHeaderID AS CHARACTER /*Link to Header*/
    FIELD riEfRowID AS ROWID
    FIELD iFormNo AS INTEGER
    FIELD dGrossWidth AS DECIMAL 
    FIELD dGrossLength AS DECIMAL 
    FIELD dGrossDepth AS DECIMAL /*3D Foam*/
    FIELD dGrossArea AS DECIMAL
    FIELD dNetWidth AS DECIMAL 
    FIELD dNetLength AS DECIMAL
    FIELD dNetDepth AS DECIMAL /*3D Foam*/
    FIELD dNetArea AS DECIMAL
    FIELD dDieWidth AS DECIMAL 
    FIELD dDieLength AS DECIMAL
    FIELD dDieDepth AS DECIMAL /*3D Foam*/
    FIELD dDieArea AS DECIMAL
    FIELD cUOMDimension AS CHARACTER /*Inches/cm*/
    FIELD cUOMArea AS CHARACTER /*MSF*/
    FIELD dGrossQtyRequiredNoWaste AS DECIMAL /* Products / Number Out*/
    FIELD dGrossQtyRequiredSetupWaste AS DECIMAL /*Wasted forms in Setup*/
    FIELD dGrossQtyRequiredRunWaste AS DECIMAL /*Wasted forms in Run*/
    FIELD dGrossQtyRequiredTotal AS DECIMAL 
    FIELD dGrossQtyRequiredTotalWeight AS DECIMAL 
    FIELD cUOMGrossQtyRequiredTotalWeight AS CHARACTER
    FIELD dGrossQtyRequiredTotalArea AS DECIMAL 
    FIELD cUOMGrossQtyRequiredTotalArea AS CHARACTER 
    FIELD dBasisWeightInLbsPerMSF AS DECIMAL 
    FIELD iNumOutNetLength AS INTEGER 
    FIELD iNumOutNetWidth AS INTEGER 
    FIELD iNumOutNetDepth AS INTEGER 
    FIELD iNumOutNet AS INTEGER
    FIELD iNumOutBlanksOnNet AS INTEGER
    FIELD iNumOut AS INTEGER  
    FIELD dWeightGross AS DECIMAL
    FIELD cUOMWeightGross AS CHARACTER 
    FIELD dWeightNet AS DECIMAL
    FIELD cUOMWeightNet AS CHARACTER  
    FIELD dWeightDie AS DECIMAL 
    FIELD cUOMWeightDie AS CHARACTER 
    FIELD dRollWidth AS DECIMAL
    FIELD dQtyFGOnForm AS DECIMAL
    FIELD dCostPerUOMOverride AS DECIMAL 
    FIELD cCostUOMOverride AS CHARACTER 
    FIELD lNoCharge AS LOGICAL
    FIELD dBlankArea AS DECIMAL 
    FIELD dCostTotalBoard AS DECIMAL 
    FIELD dCostTotalMaterial AS DECIMAL
    FIELD dCostTotalLabor AS DECIMAL 
    FIELD dCostTotalFactory AS DECIMAL 
    FIELD dCostTotalNonFactory AS DECIMAL 
    FIELD dCostTotalFull AS DECIMAL
    FIELD dCostTotalGroupLevel AS DECIMAL EXTENT 10
    .

DEFINE {1} TEMP-TABLE ttEstBlank
    FIELD rec_key AS CHARACTER 
    FIELD company AS CHARACTER 
    FIELD estBlankID AS CHARACTER /*Unique ID*/
    FIELD estFormID AS CHARACTER /*Parent form*/
    FIELD estItemID AS CHARACTER /*Parent item*/
    FIELD estHeaderID AS CHARACTER /*Parent header*/
    FIELD riEbRowID AS ROWID
    FIELD iFormNo AS INTEGER
    FIELD iBlankNo AS INTEGER
    FIELD dBlankWidth AS DECIMAL 
    FIELD dBlankLength AS DECIMAL 
    FIELD dBlankDepth AS DECIMAL /*3D Foam*/
    FIELD cUOMDimension AS CHARACTER /*Inches/cm*/
    FIELD dBlankArea AS DECIMAL 
    FIELD dBlankAreaWindow AS DECIMAL 
    FIELD dBlankAreaNetWindow AS DECIMAL
    FIELD cUOMArea AS CHARACTER /*Sqin*/
    FIELD iNumOutWidth AS INTEGER 
    FIELD iNumOutLength AS INTEGER 
    FIELD iNumOutDepth AS INTEGER 
    FIELD iNumOut AS INTEGER 
    FIELD dLength AS DECIMAL 
    FIELD dWidth AS DECIMAL 
    FIELD dDepth AS DECIMAL 
    FIELD dWeight AS DECIMAL 
    FIELD cUOMWeight AS CHARACTER
    FIELD lOutputInitialized AS LOGICAL /*Truly a temp-table field and not a db field*/
    FIELD dQtyPerSet AS DECIMAL
    FIELD dQtyRequired AS DECIMAL
    FIELD dQtyYielded AS DECIMAL
    FIELD iSubUnits AS INTEGER
    FIELD iUnits AS INTEGER
    FIELD dPctOfForm AS DECIMAL
    .
  
DEFINE {1} TEMP-TABLE ttEstMisc
    FIELD rec_key AS CHARACTER 
    FIELD company AS CHARACTER 
    FIELD estMiscID AS CHARACTER /*Unique ID*/
    FIELD estFormID AS CHARACTER /*link to parent form*/
    FIELD estBlankID AS CHARACTER /*link to parent blank*/
    FIELD estHeaderID AS CHARACTER /*Link to Parent Header*/
    FIELD iFormNo AS INTEGER 
    FIELD iBlankNo AS INTEGER
    FIELD cItemID AS CHARACTER /*RM Item Code*/
    FIELD cCostDescription AS CHARACTER /*Can be RM Item Name, Prep Name, Manual description*/
    FIELD cPrepID AS CHARACTER /*Prep ID*/
    FIELD dQtyPerSourceQty AS DECIMAL 
    FIELD dSourceQty AS DECIMAL
    FIELD cSourceQtyUOM AS CHARACTER  
    FIELD dQtyRequiredTotal AS DECIMAL 
    FIELD cQtyUOM AS CHARACTER 
    FIELD cSourceQtySource AS CHARACTER /*Each product, each blank, each case, each pallet, */
    FIELD dCostPerUOM AS DECIMAL 
    FIELD cCostUOM AS CHARACTER 
    FIELD dCostSetup AS DECIMAL
    FIELD cSIMON AS CHARACTER 
    FIELD cCostType AS CHARACTER /*Material, Labor, other*/
    FIELD dProfitPercent AS DECIMAL /*Margin or Markup*/
    FIELD cPercentType AS CHARACTER /*Margin or Markup*/
    FIELD dCostTotalBeforeProfit AS DECIMAL
    FIELD dCostTotal AS DECIMAL 
    FIELD dCostTotalPerMFinished AS DECIMAL
    FIELD dProfit AS DECIMAL  
    FIELD dAmortization AS DECIMAL
    FIELD lIsPrep AS LOGICAL
    .
DEFINE {1} TEMP-TABLE ttEstMaterial
    FIELD rec_key AS CHARACTER 
    FIELD company AS CHARACTER
    FIELD estMaterialID AS CHARACTER /*UniqueID*/
    FIELD estFormID AS CHARACTER /*link to parent form*/
    FIELD estBlankID AS CHARACTER /*link to parent blank*/
    FIELD estHeaderID AS CHARACTER /*Link to Parent Header*/
    FIELD riItemRowID AS ROWID
    FIELD iFormNo AS INTEGER 
    FIELD iBlankNo AS INTEGER
    FIELD cItemID AS CHARACTER /*RM Item Code*/
    FIELD cItemName AS CHARACTER 
    FIELD cVendorID AS CHARACTER 
    FIELD dQtyPerSourceQty AS DECIMAL /* to hold quantity per case, pallet, etc.*/
    FIELD dSourceQty AS DECIMAL 
    FIELD cSourceQtySource AS CHARACTER 
    FIELD dQtyRequiredNoWaste AS DECIMAL 
    FIELD dQtyRequiredSetupWaste AS DECIMAL 
    FIELD dQtyRequiredRunWaste AS DECIMAL
    FIELD dQtyRequiredMinDiff AS DECIMAL 
    FIELD dQtyRequiredTotal AS DECIMAL
    FIELD dQtyRequiredNoWasteInCostUOM AS DECIMAL 
    FIELD dQtyRequiredSetupWasteInCostUOM AS DECIMAL 
    FIELD dQtyRequiredRunWasteInCostUOM AS DECIMAL
    FIELD dQtyRequiredMinDiffInCostUOM AS DECIMAL 
    FIELD dQtyRequiredTotalInCostUOM AS DECIMAL
    FIELD cQtyUOM AS CHARACTER 
    FIELD cQtyUOMWaste AS CHARACTER
    FIELD dCostPerUOM AS DECIMAL 
    FIELD dCostPerUOMAvg AS DECIMAL 
    FIELD dCostPerUOMLast AS DECIMAL
    FIELD dCostPerUOMOverride AS DECIMAL
    FIELD cCostUOM AS CHARACTER  
    FIELD dCostSetup AS DECIMAL
    FIELD dCostTotal AS DECIMAL
    FIELD dCostTotalNoWaste AS DECIMAL
    FIELD dCostTotalSetupWaste AS DECIMAL 
    FIELD dCostTotalRunWaste AS DECIMAL 
    FIELD dCostTotalMinDiff AS DECIMAL
    FIELD dCostTotalPerMFinished AS DECIMAL
    FIELD dCostTotalPerMFinishedNoWaste AS DECIMAL
    FIELD dCostTotalPerMFinishedSetupWaste AS DECIMAL 
    FIELD dCostTotalPerMFinishedRunWaste AS DECIMAL 
    FIELD lIsPrimarySubstrate AS LOGICAL 
    FIELD lAddToWeightFG AS LOGICAL 
    FIELD lAddToWeightTare AS LOGICAL 
    FIELD dBasisWeight AS DECIMAL
    FIELD cBasisWeightUOM AS CHARACTER
    FIELD dDimLength AS DECIMAL
    FIELD dDimWidth AS DECIMAL 
    FIELD dDimDepth AS DECIMAL  
    FIELD cDimUOM AS CHARACTER 
    FIELD lIsRealMaterial AS LOGICAL
    FIELD cMaterialType AS CHARACTER
    FIELD iSequence AS INTEGER 
    
    .
    
DEFINE {1} TEMP-TABLE ttEstOperation
    FIELD rec_key AS CHARACTER 
    FIELD company AS CHARACTER
    FIELD estOperationID AS CHARACTER /*Unique ID*/
    FIELD estFormID AS CHARACTER /*link to parent form*/
    FIELD estBlankID AS CHARACTER /*link to parent blank*/
    FIELD estHeaderID AS CHARACTER /*link to parent header*/
    FIELD iFormNo AS INTEGER 
    FIELD iBlankNo AS INTEGER 
    FIELD cOperationID AS CHARACTER /*Mach code*/
    FIELD cOperationName AS CHARACTER
    FIELD cOperationFeedType AS CHARACTER /*B,S,R, etc*/
    FIELD cOperationOutputType AS CHARACTER /*new B,S, etc*/
    FIELD iSequence AS INTEGER
    FIELD cDepartmentIDPrimary AS CHARACTER
    FIELD cDepartmentIDs AS CHARACTER EXTENT 4
    FIELD iPass AS INTEGER 
    FIELD lSpeedInLF AS LOGICAL
    FIELD dHoursRun AS DECIMAL 
    FIELD dHoursSetup AS DECIMAL 
    FIELD dSpeed AS DECIMAL 
    FIELD dCrewSizeRun AS DECIMAL
    FIELD dCrewSizeSetup AS DECIMAL 
    FIELD dCostPerManHourDLSetup AS DECIMAL
    FIELD dCostPerHourFOSetup AS DECIMAL 
    FIELD dCostPerHourVOSetup AS DECIMAL
    FIELD dCostPerHourTotalSetup AS DECIMAL
    FIELD dCostPerManHourDLRun AS DECIMAL 
    FIELD dCostPerHourFORun AS DECIMAL 
    FIELD dCostPerHourVORun AS DECIMAL
    FIELD dCostPerHourTotalRun AS DECIMAL
    FIELD dCostTotal AS DECIMAL
    FIELD dCostTotalRun AS DECIMAL 
    FIELD dCostTotalDLRun AS DECIMAL 
    FIELD dCostTotalVORun AS DECIMAL 
    FIELD dCostTotalFORun AS DECIMAL 
    FIELD dCostTotalSetup AS DECIMAL
    FIELD dCostTotalDLSetup AS DECIMAL 
    FIELD dCostTotalVOSetup AS DECIMAL 
    FIELD dCostTotalFOSetup AS DECIMAL
    FIELD dCostTotalMinDiffSetup AS DECIMAL /*portion of total cost attributed to min charge*/
    FIELD dCostTotalMinDiffRun AS DECIMAL /*portion of total cost attributed to min charge*/
    FIELD dCostTotalMinDiff AS DECIMAL /*portion of total cost attributed to min charge*/
    FIELD dCostTotalMinDiffDLSetup AS DECIMAL /*portion of total cost attributed to min charge*/ 
    FIELD dCostTotalMinDiffVOSetup AS DECIMAL /*portion of total cost attributed to min charge*/ 
    FIELD dCostTotalMinDiffFOSetup AS DECIMAL /*portion of total cost attributed to min charge*/ 
    FIELD dCostTotalMinDiffDLRun AS DECIMAL /*portion of total cost attributed to min charge*/ 
    FIELD dCostTotalMinDiffVORun AS DECIMAL /*portion of total cost attributed to min charge*/ 
    FIELD dCostTotalMinDiffFORun AS DECIMAL /*portion of total cost attributed to min charge*/ 
    FIELD dMinCharge AS DECIMAL   
    FIELD dQtyGrossSheets AS DECIMAL
    FIELD dQtyIn AS DECIMAL 
    FIELD dQtyInAfterSetupWaste AS DECIMAL
    FIELD dQtyInAfterSetupWasteLF AS DECIMAL
    FIELD dQtyInSetupWaste AS DECIMAL 
    FIELD dQtyInRunWaste AS DECIMAL
    FIELD dQtyInRunWastePercent AS DECIMAL
    FIELD dQtyInNoWaste AS DECIMAL
    FIELD dQtyOut AS DECIMAL 
    FIELD iNumOutDivisor AS INTEGER
    FIELD iNumOutForOperation AS INTEGER
    FIELD lIsLocked AS LOGICAL 
    FIELD lIsPrinter AS LOGICAL
    FIELD lIsCoater AS LOGICAL 
    FIELD lIsGluer AS LOGICAL 
    FIELD lIsLeafer AS LOGICAL
    FIELD lIsNetSheetMaker AS LOGICAL 
    FIELD lIsBlankMaker AS LOGICAL
    FIELD dQtyInSetupWastePerColor AS DECIMAL
    FIELD iCountInks AS INTEGER
    FIELD iCountCoats AS INTEGER 
    FIELD iCountPlateChanges AS INTEGER 
    FIELD iCountFountainChanges AS INTEGER
    FIELD dQtyInkLbsWastedPerSetup AS DECIMAL 
    FIELD dQtyInkLbsWastedPerColorInSetup AS DECIMAL
    .

DEFINE {1} TEMP-TABLE ttEstCostDetail
    FIELD rec_key AS CHARACTER 
    FIELD estCostDetailID AS CHARACTER
    FIELD estCostCategoryID AS CHARACTER 
    FIELD estHeaderID AS CHARACTER 
    FIELD estFormID AS CHARACTER 
    FIELD estBlankID AS CHARACTER
    FIELD estSourceID AS CHARACTER /*MaterialID, OperationID, etc.*/ 
    FIELD cSourceType AS CHARACTER /*Material, Operation, etc.*/
    FIELD cDetailDescription AS CHARACTER 
    FIELD dCost AS DECIMAL
    FIELD dProfitPercent AS DECIMAL /*Markup or Margin*/ 
    .

DEFINE {1} TEMP-TABLE ttEstCostSummary
    FIELD rec_key AS CHARACTER 
    FIELD estCostSummaryID AS CHARACTER 
    FIELD estCostGroupID AS CHARACTER 
    FIELD cScope AS CHARACTER /*Form, Item, Header*/
    FIELD dCostPerM AS DECIMAL 
    FIELD dCostTotal AS DECIMAL 
    .
    
DEFINE {1} TEMP-TABLE ttEstCostCategory
    FIELD rec_key AS CHARACTER 
    FIELD estCostCategoryID AS CHARACTER
    FIELD estCostGroupID AS CHARACTER 
    FIELD cCategoryLabel AS CHARACTER
    FIELD cCategoryDescription AS CHARACTER 
    FIELD lIncludeInBoardCost AS LOGICAL 
    FIELD lIncludeInFactoryCost AS LOGICAL 
    FIELD lIncludeInMaterialCost AS LOGICAL
    FIELD lIncludeInLaborCost AS LOGICAL 
    FIELD lIncludeInNonFactoryCost AS LOGICAL 
    FIELD cBasis AS CHARACTER 
    FIELD cCostModel AS CHARACTER
    .
    
DEFINE {1} TEMP-TABLE ttEstCostGroup
    FIELD rec_key AS CHARACTER 
    FIELD estCostGroupID AS CHARACTER 
    FIELD cGroupDescription AS CHARACTER 
    FIELD cGroupLabel AS CHARACTER 
    FIELD iSequence AS INTEGER 
    FIELD cGroupType AS CHARACTER
    FIELD iCostGroupLevel AS INTEGER
    .
    
DEFINE {1} TEMP-TABLE ttEstCostGroupLevel
    FIELD rec_key AS CHARACTER
    FIELD iCostGroupLevel AS INTEGER 
    FIELD cCostGroupLevelDescription AS CHARACTER 
    .    
    
DEFINE {1} TEMP-TABLE ttEstError
    FIELD estHeaderID AS CHARACTER
    FIELD iFormNo AS INTEGER 
    FIELD iBlankNo AS INTEGER
    FIELD cErrorType AS CHARACTER
    FIELD cError AS CHARACTER
    .  
    
DEFINE {1} TEMP-TABLE ttInk
    FIELD rec_key AS CHARACTER 
    FIELD company AS CHARACTER 
    FIELD estBlankID AS CHARACTER /*Unique ID*/
    FIELD estFormID AS CHARACTER /*Parent form*/
    FIELD estItemID AS CHARACTER /*Parent item*/
    FIELD estHeaderID AS CHARACTER /*Parent header*/
    FIELD riEbRowID AS ROWID
    FIELD iFormNo AS INTEGER
    FIELD iBlankNo AS INTEGER
    FIELD cItemID AS CHARACTER
    FIELD iPass AS INTEGER
    FIELD iCountInks AS INTEGER 
    FIELD iCountCoatings AS INTEGER
    FIELD cMaterialType AS CHARACTER
    FIELD cPressType AS CHARACTER
    FIELD cDescription AS CHARACTER
    FIELD dCoveragePercent    AS DECIMAL 
    FIELD dCoverageRate AS DECIMAL 
    FIELD cCoverageRateUOM AS CHARACTER 
    FIELD dQtyRequiredPerBlank AS DECIMAL
    FIELD dQtyRequired AS DECIMAL
    FIELD cQtyUOM AS CHARACTER
    FIELD dMinLbsPerJob AS DECIMAL
    .
    
DEFINE {1} TEMP-TABLE ttGlue
    FIELD rec_key AS CHARACTER 
    FIELD company AS CHARACTER 
    FIELD estBlankID AS CHARACTER /*Unique ID*/
    FIELD estFormID AS CHARACTER /*Parent form*/
    FIELD estItemID AS CHARACTER /*Parent item*/
    FIELD estHeaderID AS CHARACTER /*Parent header*/
    FIELD riEbRowID AS ROWID
    FIELD iFormNo AS INTEGER
    FIELD iBlankNo AS INTEGER
    FIELD cItemID AS CHARACTER
    FIELD cMaterialType AS CHARACTER
    FIELD cDescription AS CHARACTER
    FIELD dCoverageRate AS DECIMAL 
    FIELD cCoverageRateUOM AS CHARACTER 
    FIELD dQtyRequiredPerBlank AS DECIMAL
    FIELD cQtyUOM AS CHARACTER
    FIELD dMinLbsPerJob AS DECIMAL
    . 
DEFINE {1} TEMP-TABLE ttLeaf
    FIELD rec_key AS CHARACTER 
    FIELD company AS CHARACTER 
    FIELD estBlankID AS CHARACTER /*Unique ID*/
    FIELD estFormID AS CHARACTER /*Parent form*/
    FIELD estItemID AS CHARACTER /*Parent item*/
    FIELD estHeaderID AS CHARACTER /*Parent header*/
    FIELD riEbRowID AS ROWID
    FIELD iFormNo AS INTEGER
    FIELD iBlankNo AS INTEGER
    FIELD cItemID AS CHARACTER
    FIELD cMaterialType AS CHARACTER
    FIELD cDescription AS CHARACTER
    FIELD dDimWidth AS DECIMAL 
    FIELD dDimLength AS DECIMAL
    FIELD dAreaInSQIn AS DECIMAL 
    FIELD cDimUOM AS CHARACTER
    FIELD cQtyUOM AS CHARACTER
    FIELD dCoverageRate AS DECIMAL 
    FIELD cCoverageRateUOM AS CHARACTER
    FIELD dQtyRequiredPerLeaf AS DECIMAL
    FIELD lIsSheetFed AS LOGICAL 
    FIELD lIsWindow AS LOGICAL
    . 
    
DEFINE {1} TEMP-TABLE ttPack
    FIELD rec_key AS CHARACTER 
    FIELD company AS CHARACTER 
    FIELD estBlankID AS CHARACTER /*Unique ID*/
    FIELD estFormID AS CHARACTER /*Parent form*/
    FIELD estItemID AS CHARACTER /*Parent item*/
    FIELD estHeaderID AS CHARACTER /*Parent header*/
    FIELD riEbRowID AS ROWID
    FIELD iFormNo AS INTEGER
    FIELD iBlankNo AS INTEGER
    FIELD cItemID AS CHARACTER
    FIELD cMaterialType AS CHARACTER
    FIELD cDescription AS CHARACTER
    FIELD iCountPerSubUnit AS INTEGER
    FIELD iCountSubUnitsPerUnit AS INTEGER
    FIELD iCountPerUnit AS INTEGER
    FIELD dQtyMultiplier AS DECIMAL 
    FIELD cQtyMultiplierPer AS CHARACTER 
    FIELD dWeightCapacity AS DECIMAL
    FIELD dWeightTare AS DECIMAL  
    FIELD dDimLength AS DECIMAL
    FIELD dDimWidth AS DECIMAL 
    FIELD dDimDepth AS DECIMAL  
    FIELD cDimUOM AS CHARACTER 
    FIELD cQtyUOM AS CHARACTER
    FIELD lIsCase AS LOGICAL 
    FIELD lIsPallet AS LOGICAL 
    FIELD dCostPerUOMOverride AS DECIMAL
    .     
        
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
