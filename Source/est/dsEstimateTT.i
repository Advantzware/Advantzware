
/*------------------------------------------------------------------------
    File        : dsEstimateTT.i
    Purpose     : 

    Syntax      :

    Description : Temp-table definitions for temp-tables used in estimate dataset

    Author(s)   : BV
    Created     : Tue Jun 02 17:59:28 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttEstimate NO-UNDO
    FIELD estimateID       AS INT64
    FIELD company          AS CHARACTER 
    FIELD estimateNo       AS CHARACTER
    FIELD estimateTypeID   AS CHARACTER
    FIELD sourceEstimateID LIKE ttEstimate.estimateID
    .

DEFINE TEMP-TABLE ttEstimateQuantity NO-UNDO
    FIELD estimateID         LIKE ttEstimate.estimateID
    FIELD estimateQuantityID LIKE ttEstimateQuantity.estimateID
    FIELD quantityMaster     AS DECIMAL /*master quantity*/
    FIELD quantityOfReleases AS INTEGER 
    FIELD quantityUOM        AS CHARACTER
    FIELD quantityTotal      AS DECIMAL  /*For combo/tandem this is the "probe" quantity and Master is the "reference" quantity*/
    FIELD isInteger          AS LOGICAL
    .

DEFINE TEMP-TABLE ttEstimateItem NO-UNDO
    FIELD estimateID           LIKE ttEstimate.estimateID
    FIELD estimateItemID       LIKE ttEstimateItem.estimateID
    FIELD partID               AS CHARACTER
    FIELD itemID               AS CHARACTER 
    FIELD partName             AS CHARACTER
    FIELD quantityPerSet       AS DECIMAL /*For combo/tandem this is the multiplier for ttEstimateQuantity.quantity*/
    FIELD parentEstimateItemID LIKE ttEstimateItem.estimateItemID  /*For sets and sub-assemblies*/
    FIELD sourceEstimateID     LIKE ttEstimate.estimateID /*For sourceEstimate cost calculations*/
    FIELD isPricedByYield      AS LOGICAL
    FIELD itemType             AS CHARACTER
    FIELD isPurchased          AS LOGICAL 
    .

DEFINE TEMP-TABLE ttEstimateItemQuantity NO-UNDO
    FIELD estimateItemID       LIKE ttEstimateItem.estimateID
    FIELD estimateQuantityID LIKE ttEstimateQuantity.estimateQuantityID
    FIELD estimateItemQuantityID LIKE ttEstimateItem.estimateItemID
    FIELD quantityRequested AS DECIMAL 
    FIELD quantityYielded AS DECIMAL
    .

DEFINE TEMP-TABLE ttEstimateItemQuantityRelease NO-UNDO
    FIELD estimateItemQuantityID LIKE ttEstimateItem.estimateItemID
    FIELD quantityReleased AS DECIMAL
    .    
    
DEFINE TEMP-TABLE ttEstimateForm NO-UNDO
    FIELD estimateID     LIKE ttEstimate.estimateID
    FIELD estimateFormID LIKE ttEstimateForm.estimateID
    FIELD formNo         AS INTEGER
    FIELD dimLengthGross AS DECIMAL
    FIELD dimWidthGross  AS DECIMAL 
    FIELD dimDepthGross  AS DECIMAL
    FIELD dimLengthNet   AS DECIMAL
    FIELD dimWidthNet    AS DECIMAL 
    FIELD dimDepthNet    AS DECIMAL
    FIELD dimLengthDie   AS DECIMAL
    FIELD dimWidthDie    AS DECIMAL 
    FIELD dimDepthDie    AS DECIMAL
    FIELD dimUOM         AS CHARACTER
    .

DEFINE TEMP-TABLE ttEstimateFormQuantity NO-UNDO
    FIELD estimateFormID LIKE ttEstimateForm.estimateID
    FIELD estimateQuantityID LIKE ttEstimateQuantity.estimateQuantityID
    FIELD quantityRequired AS DECIMAL 
    FIELD quantityWasteSetup AS DECIMAL
    FIELD quantityWasteRun AS DECIMAL 
    .
    
DEFINE TEMP-TABLE ttEstimateBlank NO-UNDO
    FIELD estimateFormID LIKE ttEstimateForm.estimateFormID
    FIELD estimateBlankID LIKE ttEstimateForm.estimateFormID
    FIELD estimateItemID LIKE ttEstimateItem.estimateItemID
    FIELD formNo         AS INTEGER
    FIELD blankNo        AS INTEGER
    FIELD dimLength      AS DECIMAL
    FIELD dimWidth       AS DECIMAL 
    FIELD dimDepth       AS DECIMAL
    FIELD dimLengthBlank AS DECIMAL
    FIELD dimWidthBlank  AS DECIMAL
    FIELD dimDepthBlank  AS DECIMAL
    FIELD dimUOM         AS CHARACTER
    FIELD percentOfForm  AS DECIMAL
    .

DEFINE TEMP-TABLE ttEstimateBlankQuantity NO-UNDO
    FIELD estimateBlankID LIKE ttEstimateBlank.estimateBlankID
    FIELD estimateQuantityID LIKE ttEstimateQuantity.estimateQuantityID
    FIELD quantityYielded AS DECIMAL
    FIELD quantityRequested AS DECIMAL
    .
    
DEFINE TEMP-TABLE ttEstimateOperation NO-UNDO
    FIELD estimateID         LIKE ttEstimate.estimateID
    FIELD estimateOperationID    LIKE ttEstimate.estimateID
    FIELD estimateQuantityID LIKE ttEstimateQuantity.estimateQuantityID
    FIELD operationID        AS CHARACTER
    FIELD estimateFormID     LIKE ttEstimateForm.estimateFormID
    FIELD estimateBlankID    LIKE ttEstimateBlank.estimateBlankID
    FIELD isMiscellaneous    AS LOGICAL 
    FIELD quantityIn     AS DECIMAL
    FIELD quantityOut AS DECIMAL 
    FIELD quantityWasteSetup AS DECIMAL 
    FIELD quantityWasteRun AS DECIMAL
    .     

DEFINE TEMP-TABLE ttEstimateMaterial NO-UNDO
    FIELD estimateID         LIKE ttEstimate.estimateID
    FIELD estimateMaterialID LIKE ttEstimate.estimateID
    FIELD estimateFormID     LIKE ttEstimateForm.estimateFormID
    FIELD estimateBlankID    LIKE ttEstimateBlank.estimateBlankID
    FIELD isMiscellaneous    AS LOGICAL 
    FIELD estimateOperationID    LIKE ttEstimateOperation.estimateOperationID
    FIELD quantityMultiplier AS DECIMAL 
    FIELD quantityPerUOM AS CHARACTER  /*UOM of the quantityMultiplier*/ 
    FIELD quantityBasis AS CHARACTER /*Basis of what quantityMultiplier is applied*/
    FIELD isInteger          AS LOGICAL
    .
    
DEFINE TEMP-TABLE ttEstimateError NO-UNDO
    FIELD estimateID LIKE ttEstimate.estimateID
    FIELD estimateFormID     LIKE ttEstimateForm.estimateFormID
    FIELD estimateBlankID    LIKE ttEstimateBlank.estimateBlankID
    FIELD errorType AS CHARACTER
    FIELD errorMessage AS CHARACTER
    .
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
