
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
    FIELD cEstNo AS CHARACTER
    FIELD dQtyMaster AS DECIMAL /*Master Qty Calculated*/
    FIELD cCalculator AS CHARACTER /*User ID of who calculated*/
    FIELD cPrinter AS CHARACTER /*User ID of who printed*/
    FIELD cEstType AS CHARACTER /*Set, Single, Combo, Tandem*/
    FIELD dtCalcDateTime AS DATETIME 
    FIELD dtPrintDateTime AS DATETIME 
    .

DEFINE {1} TEMP-TABLE ttEstItem
    FIELD rec_key AS CHARACTER
    FIELD estItemID AS CHARACTER 
    FIELD estItemIDParent AS CHARACTER /*Link to Set*/
    FIELD estHeaderID AS CHARACTER /*Link to Header*/
    FIELD dQtyPerParent AS DECIMAL
    FIELD dQtyRequired AS DECIMAL
    FIELD dQtyYielded AS DECIMAL
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
    .
   
DEFINE {1} TEMP-TABLE ttEstForm
    FIELD rec_key AS CHARACTER 
    FIELD estFormID AS CHARACTER /*Unique ID*/
    FIELD estHeaderID AS CHARACTER /*Link to Header*/
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
    FIELD dGrossQtyRequiredWasteMR AS DECIMAL /*Wasted forms in MR*/
    FIELD dGrossQtyRequiredWasteRun AS DECIMAL /*Wasted forms in Run*/
    FIELD dGrossQtyRequiredTotal AS DECIMAL 
    FIELD dGrossQtyRequiredTotalWeight AS DECIMAL 
    FIELD cUOMGrossQtyRequiredTotalWeight AS CHARACTER
    FIELD dGrossQtyRequiredTotalArea AS DECIMAL 
    FIELD cUOMGrossQtyRequiredTotalArea AS CHARACTER 
    FIELD dBasisWeightInLbsPerMSF AS DECIMAL 
    FIELD iNumOutLength AS INTEGER 
    FIELD iNumOutWidth AS INTEGER 
    FIELD iNumOutDepth AS INTEGER 
    FIELD iNumOut AS INTEGER 
    FIELD dWeightGross AS DECIMAL
    FIELD cUOMWeightGross AS CHARACTER 
    FIELD dWeightNet AS DECIMAL
    FIELD cUOMWeightNet AS CHARACTER  
    FIELD dWeightDie AS DECIMAL 
    FIELD cUOMWeightDie AS CHARACTER 
    FIELD dRollWidth AS DECIMAL
    FIELD dQtyFGOnForm AS DECIMAL
    .

DEFINE {1} TEMP-TABLE ttEstBlank
    FIELD rec_key AS CHARACTER 
    FIELD estBlankID AS CHARACTER /*Unique ID*/
    FIELD estFormID AS CHARACTER /*Parent form*/
    FIELD estItemID AS CHARACTER /*Parent item*/
    FIELD estHeaderID AS CHARACTER /*Parent header*/
    FIELD iBlankNo AS INTEGER
    FIELD dBlankWidth AS DECIMAL 
    FIELD dBlankLength AS DECIMAL 
    FIELD dBlankDepth AS DECIMAL /*3D Foam*/
    FIELD cUOMDimension AS CHARACTER /*Inches/cm*/
    FIELD dBlankArea AS DECIMAL 
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
    .
    
DEFINE {1} TEMP-TABLE ttEstMaterial
    FIELD rec_key AS CHARACTER 
    FIELD estMaterialID AS CHARACTER /*UniqueID*/
    FIELD estFormID AS CHARACTER /*link to parent form*/
    FIELD estBlankID AS CHARACTER /*link to parent blank*/
    FIELD estHeaderID AS CHARACTER /*Link to Parent Header*/
    FIELD iFormNo AS INTEGER 
    FIELD iBlankNo AS INTEGER
    FIELD cItemID AS CHARACTER /*RM Item Code*/
    FIELD cItemName AS CHARACTER 
    FIELD dQtyRequiredNoWaste AS DECIMAL 
    FIELD dQtyRequiredWasteMR AS DECIMAL 
    FIELD dQtyRequiredWasteRun AS DECIMAL
    FIELD dQtyRequiredTotal AS DECIMAL
    FIELD cQtyUOM AS CHARACTER 
    FIELD dCostPerUOM AS DECIMAL 
    FIELD cCostUOM AS CHARACTER  
    FIELD dCostMR AS DECIMAL
    FIELD dCostTotal AS DECIMAL
    FIELD dCostTotalNoWaste AS DECIMAL
    FIELD dCostTotalWasteMR AS DECIMAL 
    FIELD dCostTotalWasteRun AS DECIMAL 
    FIELD dCostTotalPerMFinished AS DECIMAL
    FIELD dCostTotalPerMFinishedNoWaste AS DECIMAL
    FIELD dCostTotalPerMFinishedWasteMR AS DECIMAL 
    FIELD dCostTotalPerMFinishedWasteRun AS DECIMAL 
    FIELD lIsPrimarySubstrate AS LOGICAL 
    FIELD lAddToWeightFG AS LOGICAL 
    FIELD lAddToWeightTare AS LOGICAL 
    .
    
DEFINE {1} TEMP-TABLE ttEstOperation
    FIELD rec_key AS CHARACTER 
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
    FIELD cDepartmentID AS CHARACTER
    FIELD cAlt1DepartmentID AS CHARACTER 
    FIELD cAlt2DepartmentID AS CHARACTER 
    FIELD cAlt3DepartmentID AS CHARACTER 
    FIELD iPass AS INTEGER 
    FIELD lSpeedInLF AS LOGICAL
    FIELD dHoursRun AS DECIMAL 
    FIELD dHoursSetup AS DECIMAL 
    FIELD dSpeed AS DECIMAL 
    FIELD dCrewSizeRun AS DECIMAL
    FIELD dCrewSizeSetup AS DECIMAL 
    FIELD dQtyInRunWastePercent AS DECIMAL
    FIELD dQtyInSetupWasteCount AS DECIMAL
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
    FIELD dQtyIn AS DECIMAL 
    FIELD dQtyOut AS DECIMAL 
    FIELD dQtyWasteSetup AS DECIMAL 
    FIELD dQtyWasteRun AS DECIMAL
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
    FIELD dMarkup AS DECIMAL 
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
    FIELD cIncludeIn AS CHARACTER 
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
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
