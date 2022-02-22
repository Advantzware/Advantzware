
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

DEFINE TEMP-TABLE ttEstCostForm NO-UNDO
    LIKE estCostForm
    .
    
DEFINE TEMP-TABLE ttEstCostDetail NO-UNDO
    LIKE estCostDetail
    .
    
DEFINE TEMP-TABLE ttEstCostSummary NO-UNDO
    LIKE estCostSummary
    .    
    
    
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
