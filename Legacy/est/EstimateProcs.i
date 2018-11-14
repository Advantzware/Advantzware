
/*------------------------------------------------------------------------
    File        : EstimateProcs.i
    Purpose     : 

    Syntax      :

    Description : Definition file for EstimateProcs (estimate calculation engine)

    Author(s)   : BV
    Created     : Tue Oct 23 11:28:10 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {1} TEMP-TABLE ttEstimateMaster /*Store estimate-level settings*/
    FIELD rec_key AS CHARACTER 
    FIELD rec_keyParent AS CHARACTER 
    FIELD riSource AS ROWID 
    FIELD lUI AS LOGICAL 
    FIELD cCompany AS CHARACTER 
    FIELD cLoc AS CHARACTER 
    FIELD cEstNo AS CHARACTER 
    FIELD cIndustry AS CHARACTER 
    FIELD cDisplayFormat AS CHARACTER 
    FIELD cUserID AS CHARACTER 
    FIELD dtCalcDateTime AS DATETIME 
    FIELD iEstType AS INTEGER 
    FIELD cEstTypeDesc AS CHARACTER 
    FIELD dMarginPct AS DECIMAL 
    FIELD cMarginOn AS CHARACTER 
    FIELD dWarehouseMarkupPct AS DECIMAL 
    FIELD dHandlingChargePct AS DECIMAL 
    FIELD dHandlingRatePerCWTRMPct AS DECIMAL 
    FIELD dSpecial1MarkupPct AS DECIMAL 
    FIELD dSpecial2MarkupPct AS DECIMAL 
    FIELD dSpecial3MarkupPct AS DECIMAL 
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
    .
DEFINE {1} TEMP-TABLE ttQuantities  /*Store quantity-level settings and overrides*/
    LIKE ttEstimateMaster 
    FIELD rec_keyEstimate AS CHARACTER 
    FIELD dMasterQuantity AS DECIMAL  
    FIELD iReleases AS INTEGER 
    FIELD lRunAndShip AS LOGICAL 
    .
    
DEFINE {1} TEMP-TABLE ttForms
    FIELD rec_key AS CHARACTER 
    FIELD rec_keyParent AS CHARACTER 
    FIELD rec_keyEstimate AS CHARACTER 
    FIELD iFormNo AS INTEGER
    . 
    
DEFINE {1} TEMP-TABLE ttBlanks
    FIELD rec_key AS CHARACTER /*unique identifier*/
    FIELD rec_keyParent AS CHARACTER 
    FIELD rec_keyEstimate AS CHARACTER /*links to estimate table*/
    FIELD rec_keyForm AS CHARACTER /*links to form table*/
    FIELD rec_keyItem AS CHARACTER /*links to item table*/
    FIELD iFormNo AS INTEGER 
    FIELD iBlankNo AS INTEGER 
    .

DEFINE {1} TEMP-TABLE ttItems
    FIELD rec_key AS CHARACTER 
    FIELD rec_keyParent AS CHARACTER 
    FIELD rec_keyEstimate AS CHARACTER 
    FIELD cCustID AS CHARACTER
    FIELD cPartID AS CHARACTER 
    .

DEFINE {1} TEMP-TABLE ttOperations
    FIELD rec_key AS CHARACTER 
    FIELD rec_keyParent AS CHARACTER 
    FIELD rec_keyEstimate AS CHARACTER 
    FIELD rec_keyForm AS CHARACTER 
    FIELD rec_keyBlank AS CHARACTER 
    FIELD iPass AS INTEGER 
    FIELD cOperationID AS CHARACTER /*m-code*/
    FIELD cOperationDesc AS CHARACTER
    FIELD cTypeFeed AS CHARACTER /*sheets in*/
    FIELD cTypeOut AS CHARACTER   /*Blanks out*/
    FIELD dFeedsPerHour AS DECIMAL 
    FIELD dFeedsWastedInSetup AS DECIMAL 
    FIELD dFeedsWastedInRun AS DECIMAL /*calc*/
    FIELD dFeedsWastedRate AS DECIMAL 
    FIELD dHoursSetup AS DECIMAL /*calc*/
    FIELD dHoursRun AS DECIMAL /*calc*/
    FIELD dHoursDown AS DECIMAL /*calc*/
    FIELD dHours AS DECIMAL /*calc*/ 
    FIELD dQtyFeeds AS DECIMAL 
    FIELD dQtyOuts AS DECIMAL 
    FIELD dCostPerHourDLPerCrewSetup AS DECIMAL 
    FIELD dCostPerHourDLPerCrewRun AS DECIMAL 
    FIELD dCostPerHourVOHSetup AS DECIMAL 
    FIELD dCostPerHourFOHSetup AS DECIMAL 
    FIELD dCostPerHourVOHRun AS DECIMAL 
    FIELD dCostPerHourFOHRun AS DECIMAL 
    FIELD dCrewCountSetup AS DECIMAL 
    FIELD dCrewCountRun AS DECIMAL
    FIELD dCostTotalSetupDL AS DECIMAL  /*calc*/ 
    FIELD dCostTotalSetupVOH AS DECIMAL /*calc*/
    FIELD dCostTotalSetupFOH AS DECIMAL /*calc*/
    FIELD dCostTotalSetup AS DECIMAL  /*calc*/
    FIELD dCostTotalRunDL AS DECIMAL 
    FIELD dCostTotalRunVOH AS DECIMAL 
    FIELD dCostTotalRunFOH AS DECIMAL 
    FIELD dCostTotalRun AS DECIMAL 
    FIELD dManHoursSetup AS DECIMAL 
    FIELD dManHoursRun AS DECIMAL 
    FIELD dManHours AS DECIMAL 
    .
        
DEFINE {1} TEMP-TABLE ttMaterials
    FIELD rec_key AS CHARACTER 
    FIELD rec_keyParent AS CHARACTER 
    FIELD rec_keyEstimate AS CHARACTER 
    FIELD rec_keyForm AS CHARACTER 
    FIELD rec_keyBlank AS CHARACTER 
    FIELD rec_keyOperation AS CHARACTER 
    FIELD cItemID AS CHARACTER 
    FIELD cItemDesc AS CHARACTER 
    FIELD cMaterialType AS CHARACTER 
    FIELD cMaterialTypeDesc AS CHARACTER 
    FIELD cVendorID AS CHARACTER 
    FIELD cVendorName AS CHARACTER 
    FIELD cVendorItemID AS CHARACTER 
    FIELD dLookupQty AS DECIMAL 
    FIELD cLookupQtyUOM AS CHARACTER
    FIELD dLookupCostPerUOM AS DECIMAL 
    FIELD cLookupCostUOM AS CHARACTER  
    FIELD dLookupCostFlat AS DECIMAL 
    FIELD dCostFlat AS DECIMAL 
    FIELD dCostPerUom AS DECIMAL
    FIELD cCostUOM AS CHARACTER  
    FIELD dCostTotal AS DECIMAL 
    FIELD dQtyRunWaste AS DECIMAL 
    FIELD dQtyRun AS DECIMAL 
    FIELD dQtySetup AS DECIMAL 
    FIELD dQtySetupWaste AS DECIMAL 
    FIELD dQtyRequired AS DECIMAL 
    FIELD cQtyUOM AS CHARACTER 
    FIELD dConvFactorWeightToArea AS DECIMAL 
    FIELD cConvFactorWeightToAreaUOM AS CHARACTER 
    FIELD dConvFactorWeightToVolume AS DECIMAL 
    FIELD cConvFactorWeightToVolumeUOM AS CHARACTER
    FIELD dGrossLength AS DECIMAL 
    FIELD dGrossWidth AS DECIMAL 
    FIELD dGrossDepth AS DECIMAL 
    FIELD dGrossArea AS DECIMAL
    FIELD dGrossVolume AS DECIMAL 
    FIELD cGrossUOM AS DECIMAL 
    FIELD dPercentage AS DECIMAL 
    .
    
DEFINE {1} TEMP-TABLE ttCostTable NO-UNDO
    FIELD dRunQty AS DECIMAL EXTENT 20
    FIELD dRunCost AS DECIMAL EXTENT 20
    FIELD cRunCostUom AS CHARACTER 
    FIELD dSetups AS DECIMAL EXTENT 20
    .

DEFINE {1} TEMP-TABLE ttCalculationErrors
    FIELD cEstNo AS CHARACTER 
    FIELD iFormNo AS INTEGER 
    FIELD iBlankNo AS INTEGER 
    FIELD iPassNo AS INTEGER
    FIELD cError AS CHARACTER 
    FIELD cMessage AS CHARACTER 
    FIELD lCritical AS LOGICAL
    .
    

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
