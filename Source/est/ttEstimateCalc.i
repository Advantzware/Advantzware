
/*------------------------------------------------------------------------
    File        : ttEstimateCalc.i
    Purpose     : 

    Syntax      :

    Description : Temp-table definitions for temp-tables used in estimate dataset

    Author(s)   : BV
    Created     : Tue Jun 02 17:59:28 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttEstCostForm NO-UNDO
    LIKE estCostForm
    .
    
DEFINE TEMP-TABLE ttEstCostDetail NO-UNDO
    LIKE estCostDetail
    .
    
DEFINE TEMP-TABLE ttEstCostSummary NO-UNDO
    LIKE estCostSummary
    .    
DEFINE TEMP-TABLE ttEstCostOperation NO-UNDO
    LIKE estCostOperation
    .  
        
DEFINE TEMP-TABLE ttEstCostMisc NO-UNDO
    LIKE estCostMisc
    .
DEFINE TEMP-TABLE ttEstCostMaterial NO-UNDO
    LIKE estCostMaterial
    .    
DEFINE TEMP-TABLE ttEstCostItem NO-UNDO
    LIKE estCostItem
    FIELD DBEstCostItemID LIKE estCostItem.estCostItemID
    FIELD DBRec_key       LIKE estCostItem.rec_key
    .     

DEFINE TEMP-TABLE ttEstCostBlank NO-UNDO
    LIKE estCostBlank
    .  
    
DEFINE TEMP-TABLE ttEstCostHeader NO-UNDO
    LIKE estCostHeader
    .    
     
         
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
