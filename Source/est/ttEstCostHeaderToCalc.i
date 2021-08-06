
/*------------------------------------------------------------------------
    File        : ttEstCostHeaderToCalc.i
    Purpose     : 

    Syntax      :

    Description : Temp Table to track calculated estCostHeaders		

    Author(s)   : BV
    Created     : Fri Jul 30 13:31:39 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE {1} TEMP-TABLE ttEstCostHeaderToCalc /*Master Print*/
    FIELD iEstCostHeaderID AS INT64
    FIELD lRecalcRequired AS LOGICAL
    .


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
