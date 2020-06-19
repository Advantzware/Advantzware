
/*------------------------------------------------------------------------
    File        : ttEstCost.i
    Purpose     : 

    Syntax      :

    Description : Holds tables to build the estimate Calculation Tables

    Author(s)   : BV
    Created     : Wed Jan 23 17:17:51 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttEstCostHeader 
    LIKE estCostHeader.

DEFINE TEMP-TABLE ttEstCostMaterial
    LIKE estCostMaterial.
    
DEFINE TEMP-TABLE ttEstCostOperation
    LIKE estCostOperation.

DEFINE TEMP-TABLE ttEstCostForm
    LIKE estCostForm.

DEFINE TEMP-TABLE ttEstCostBlank
    LIKE estCostBlank.

DEFINE DATASET dsEstimate FOR 
    ttEstCostHeader,
    ttEstCostMaterial,
    ttEstCostOperation, 
    
        
DEFINE TEMP-TABLE ttEstHeaderToCalc /*Master Print*/
    FIELD iEstCostHeaderID AS INT64
    .

DEFINE TEMP-TABLE ttEstBlank 
    FIELD estCostBlankID AS INT64
    FIELD estCostFormID AS INT64
    FIELD iOut AS INTEGER
    FIELD dQtyInOut AS DECIMAL
    FIELD dQtyInOutRunWaste AS DECIMAL
    FIELD dQtyInOutSetupWaste AS DECIMAL
    FIELD lOutputInitialized AS LOGICAL /*Truly a temp-table field and not a db field*/
    . 

DEFINE TEMP-TABLE ttEstError
    FIELD estHeaderID AS INT64
    FIELD iFormNo AS INTEGER 
    FIELD iBlankNo AS INTEGER
    FIELD cErrorType AS CHARACTER
    FIELD cError AS CHARACTER
    .  
    
DEFINE {1} TEMP-TABLE ttInk
    FIELD rec_key AS CHARACTER 
    FIELD company AS CHARACTER 
    FIELD estBlankID AS INT64 /*Parent blank*/
    FIELD estFormID AS INT64 /*Parent form*/
    FIELD estItemID AS INT64 /*Parent item*/
    FIELD estHeaderID AS INT64 /*Parent header*/
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
    FIELD lNoCharge AS LOGICAL
    .
    
DEFINE {1} TEMP-TABLE ttGlue
    FIELD rec_key AS CHARACTER 
    FIELD company AS CHARACTER 
    FIELD estBlankID AS INT64 /*Parent blank*/
    FIELD estFormID AS INT64 /*Parent form*/
    FIELD estItemID AS INT64 /*Parent item*/
    FIELD estHeaderID AS INT64 /*Parent header*/
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
    FIELD estBlankID AS INT64 /*Parent blank*/
    FIELD estFormID AS INT64 /*Parent form*/
    FIELD estItemID AS INT64 /*Parent item*/
    FIELD estHeaderID AS INT64 /*Parent header*/
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
    FIELD estBlankID AS INT64 /*Parent blank*/
    FIELD estFormID AS INT64 /*Parent form*/
    FIELD estItemID AS INT64 /*Parent item*/
    FIELD estHeaderID AS INT64 /*Parent header*/
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
    FIELD lNoCharge AS LOGICAL
    .     
        
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
