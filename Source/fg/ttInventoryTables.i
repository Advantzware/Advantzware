
/*------------------------------------------------------------------------
    File        : ttInventoryTables.i
    Purpose     : 

    Syntax      :

    Description : temp-table definitions for Inventory Stock conversion

    Author(s)   : Mithun Porandla
    Created     : Tue Jan 07 02:49:54 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttInventoryStock NO-UNDO
    LIKE InventoryStock
    FIELD valid     AS LOGICAL   INITIAL TRUE  
    FIELD comment   AS CHARACTER
    FIELD processed AS LOGICAL
    .

DEFINE TEMP-TABLE ttInventoryTransaction NO-UNDO
    LIKE InventoryTransaction
    FIELD valid     AS LOGICAL   INITIAL TRUE  
    FIELD comment   AS CHARACTER
    FIELD processed AS LOGICAL
    .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
