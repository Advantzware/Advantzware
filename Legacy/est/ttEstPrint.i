
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
    FIELD cEstNo AS CHARACTER
    FIELD dQuantityMaster AS DECIMAL /*Master Quantity Calculated*/
    FIELD cCalculator AS CHARACTER /*User ID of who calculated*/
    FIELD cPrinter AS CHARACTER /*User ID of who printed*/
    FIELD cEstType AS CHARACTER /*Set, Single, Combo, Tandem*/
    FIELD cCustNo AS CHARACTER /*Can be multiple on a given estimate if combo/tandem*/
    FIELD cShipNo AS CHARACTER /*Can be multiple on a given estimate if combo/tandem*/
    .

DEFINE {1} TEMP-TABLE ttEstItem
    FIELD rec_keyItem AS CHARACTER 
    FIELD rec_keyItemParent AS CHARACTER /*Link to Set*/
    FIELD rec_keyHeader AS CHARACTER /*Link to Header*/
    FIELD dQuantityPerSet AS DECIMAL
    FIELD dQuantityRequired AS DECIMAL
    FIELD dQuantityYielded AS DECIMAL
    FIELD cItemName AS CHARACTER 
    FIELD cItemDescription1 AS CHARACTER
    FIELD cItemDescription2 AS CHARACTER
    FIELD cStyle AS CHARACTER  
    FIELD lIsSet AS LOGICAL
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
    FIELD dDieLenth AS DECIMAL
    FIELD dDieDepth AS DECIMAL /*3D Foam*/
    FIELD dDieArea AS DECIMAL
    FIELD cUOMDimension AS CHARACTER /*Inches/cm*/
    FIELD cUOMArea AS CHARACTER /*MSF*/
    FIELD dQuantityRequiredNet AS DECIMAL /* Products / Number Out*/
    FIELD dQuantityRequiredWasteMR AS DECIMAL /*Wasted forms in MR*/
    FIELD dQuantityRequiredWasteRun AS DECIMAL /*Wasted forms in Run*/
    FIELD dBasisWeightInLbsPerSqin AS DECIMAL 
    .

DEFINE {1} TEMP-TABLE ttEstBlank
    FIELD rec_keyBlank AS CHARACTER /*Unique ID*/
    FIELD rec_keyForm AS CHARACTER /*Parent form*/
    FIELD rec_keyItem AS CHARACTER /*Parent item*/
    FIELD iBlankNo AS INTEGER
    FIELD dBlankWidth AS DECIMAL 
    FIELD dBlankLength AS DECIMAL 
    FIELD dBlankDepth AS DECIMAL /*3D Foam*/
    FIELD cDimensionUOM AS CHARACTER /*Inches/cm*/
    FIELD dBlankArea AS DECIMAL 
    FIELD cUOMArea AS CHARACTER /*Sqin*/
    FIELD iNumOutWidth AS INTEGER 
    FIELD iNumOutLength AS INTEGER 
    FIELD iNumOutDepth AS INTEGER 
    .
    
DEFINE {1} TEMP-TABLE ttEstMaterial
    FIELD rec_keyMaterial AS CHARACTER /*UniqueID*/
    FIELD rec_keyForm AS CHARACTER /*link to parent form*/
    FIELD rec_keyBlank AS CHARACTER /*link to parent blank*/
    FIELD cItemID AS CHARACTER /*RM Item Code*/
    FIELD cItemName AS CHARACTER 
    FIELD dQuantityRequired AS DECIMAL 
    FIELD cQuantityUOM AS CHARACTER 
    FIELD dCostPerUOM AS DECIMAL 
    FIELD dCostUOM AS DECIMAL 
    FIELD dCostTotal AS DECIMAL
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
    FIELD dQuantityIn AS DECIMAL 
    FIELD dQuantityOut AS DECIMAL 
    FIELD dQuantityWasteSetup AS DECIMAL 
    FIELD dQuantityWasteRun AS DECIMAL
    .
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
