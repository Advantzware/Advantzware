
/*------------------------------------------------------------------------
    File        : ttEstSysConfig.i.i
    Purpose     : 

    Syntax      :

    Description : Holds tables to store system configuration data

    Author(s)   : sakshi.singh
    Created     : Wed Dec 08 17:17:51 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttEstCostCategory NO-UNDO
    LIKE estCostCategory.

DEFINE TEMP-TABLE ttEstCostGroupLevel NO-UNDO
    LIKE estCostGroupLevel.
    
DEFINE TEMP-TABLE ttEstCostGroup NO-UNDO
    LIKE EstCostGroup. 
    
/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */
