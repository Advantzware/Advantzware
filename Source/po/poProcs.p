
/*------------------------------------------------------------------------
    File        : poProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Jan 15 15:02:50 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
BLOCK-LEVEL ON ERROR UNDO, THROW.

{system/VendorCostProcs.i}



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE doPo:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:  input param rowid(oe-ordl)
         output param ttItemPurchased(old w-job-mat) , ttVendItemCost
------------------------------------------------------------------------------*/
  RUN BuildTtItemPurchased.
  RUN buildTTVendItemCost.
  
  

END PROCEDURE.

