
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
    FIELD rec_KeyHeader  AS CHARACTER /*Unique ID*/
    FIELD cCompany AS CHARACTER
    FIELD cEstNo AS CHARACTER
    FIELD dQtyMaster AS DECIMAL /*Master Qty Calculated*/
    FIELD cCalculator AS CHARACTER /*User ID of who calculated*/
    FIELD cPrinter AS CHARACTER /*User ID of who printed*/
    FIELD cEstType AS CHARACTER /*Set, Single, Combo, Tandem*/
    FIELD dtCalcDateTime AS DATETIME 
    FIELD dtPrintDateTime AS DATETIME 
    .

DEFINE {1} TEMP-TABLE ttEstItem
    FIELD rec_keyItem AS CHARACTER 
    FIELD rec_keyItemParent AS CHARACTER /*Link to Set*/
    FIELD rec_keyHeader AS CHARACTER /*Link to Header*/
    FIELD dQtyPerParent AS DECIMAL
    FIELD dQtyRequired AS DECIMAL
    FIELD dQtyYielded AS DECIMAL
    FIELD cItemName AS CHARACTER 
    FIELD cItemDescription1 AS CHARACTER
    FIELD cItemDescription2 AS CHARACTER
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
    FIELD rec_KeyForm AS CHARACTER /*Unique ID*/
    FIELD rec_keyHeader AS CHARACTER /*Link to Header*/
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
    FIELD rec_keyBlank AS CHARACTER /*Unique ID*/
    FIELD rec_keyForm AS CHARACTER /*Parent form*/
    FIELD rec_keyItem AS CHARACTER /*Parent item*/
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
    FIELD rec_keyMaterial AS CHARACTER /*UniqueID*/
    FIELD rec_keyForm AS CHARACTER /*link to parent form*/
    FIELD rec_keyBlank AS CHARACTER /*link to parent blank*/
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
    
DEFINE {1} TEMP-TABLE ttEstOperations
    FIELD rec_keyOperation AS CHARACTER /*Unique ID*/
    FIELD rec_keyForm AS CHARACTER /*link to parent form*/
    FIELD rec_keyBlank AS CHARACTER /*link to parent blank*/
    FIELD cOperationID AS CHARACTER /*Mach code*/
    FIELD cOperationName AS CHARACTER 
    FIELD dHoursRun AS DECIMAL 
    FIELD dHoursSetup AS DECIMAL 
    FIELD dSpeed AS DECIMAL 
    FIELD dCostPerHourDL AS DECIMAL 
    FIELD dCostPerHourFO AS DECIMAL 
    FIELD dCostPerHourVO AS DECIMAL
    FIELD dCostTotal AS DECIMAL
    FIELD dQtyIn AS DECIMAL 
    FIELD dQtyOut AS DECIMAL 
    FIELD dQtyWasteSetup AS DECIMAL 
    FIELD dQtyWasteRun AS DECIMAL
    .

DEFINE {1} TEMP-TABLE ttEstCostDetail
    FIELD rec_keyCostDetail AS CHARACTER
    FIELD rec_keyCostCategory AS CHARACTER 
    FIELD rec_keyHeader AS CHARACTER 
    FIELD rec_keySource AS CHARACTER 
    FIELD cSourceType AS CHARACTER
    FIELD cDetailDescription AS CHARACTER 
    FIELD dCost AS DECIMAL
    FIELD dMarkup AS DECIMAL 
    .

DEFINE {1} TEMP-TABLE ttEstCostCategory
    FIELD rec_keyCostCategory AS CHARACTER
    FIELD rec_keyCostGroup AS CHARACTER 
    FIELD cCategoryLabel AS CHARACTER
    FIELD cCategoryDescription AS CHARACTER 
    FIELD cIncludeIn AS CHARACTER 
    FIELD cBasis AS CHARACTER 
    
    .
    
DEFINE {1} TEMP-TABLE ttEstCostGroup
    FIELD rec_keyCostGroup AS CHARACTER 
    FIELD cGroupDescription AS CHARACTER 
    FIELD cGroupLabel AS CHARACTER 
    FIELD iSequence AS INTEGER 
    FIELD cGroupType AS CHARACTER
    FIELD iCostGroupLevel AS INTEGER
    .
DEFINE {1} TEMP-TABLE ttEstCostGroupLevel
    FIELD iCostGroupLevel AS INTEGER 
    FIELD cCostGroupLevelDescription AS CHARACTER 
    .    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
