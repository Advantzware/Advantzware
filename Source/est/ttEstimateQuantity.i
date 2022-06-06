
/*------------------------------------------------------------------------
    File        : ttEstimateQuantity.i
    Purpose     : 

    Syntax      :

    Description : Temp-table definitions for temp-tables used in estimate quantity calculation

    Author(s)   : 
    Created     : 06/03/2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttEstimateQuantity NO-UNDO
       FIELD EstQuantity       LIKE est-qty.qty EXTENT 20
       FIELD EstRelease        LIKE est-qty.qty EXTENT 20
       FIELD EstRunship        LIKE est-qty.whsed EXTENT 20
       FIELD EstMSF            AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U EXTENT 20
       FIELD EstNextQuantity   LIKE est-qty.qty EXTENT 20
    .    
     
         
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
