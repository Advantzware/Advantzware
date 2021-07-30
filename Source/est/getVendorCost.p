
/*------------------------------------------------------------------------
    File        : getVendorCost.p
    Purpose     : It takes details of Item, Vendor and quantity as Input and returns the cost for that Item.

    Syntax      :

    Description : 

    Author(s)   : sakshi.singh
    Created     : Fri Jul 30 06:44:53 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE INPUT  PARAMETER ipcCompanyCode AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemCode AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemType AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcVendorNo AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipdQty      AS DECIMAL NO-UNDO.
DEFINE INPUT  PARAMETER ipcQtyUOM   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opdItemCost AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdSetupCost AS DECIMAL NO-UNDO.


DEFINE VARIABLE cQtyUOM     AS CHARACTER NO-UNDO.
DEFINE VARIABLE dCostTotal  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCostPerUOM AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCostSetup  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cCostUOM    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lError      AS LOGICAL NO-UNDO.
DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-Item FOR Item.


FIND FIRST bf-Item NO-LOCK
    WHERE bf-Item.company = ipcCompanyCode
    AND bf-Item.i-no    = ipcItemCode NO-ERROR.
      
IF AVAILABLE bf-Item THEN
    RUN GetVendorCost(bf-Item.company, 
        bf-Item.i-no, 
        ipcItemType, 
        ipcVendorNo, 
        "",
        "", 
        0, 
        0,
        ipdQty, 
        ipcQtyUOM,
        bf-Item.s-len, 
        bf-Item.s-wid, 
        0, 
        "IN", 
        bf-Item.basis-w, 
        "LB/EA", 
        NO,
        OUTPUT dCostPerUOM, 
        OUTPUT dCostSetup, 
        OUTPUT cCostUOM,
        OUTPUT dCostTotal, 
        OUTPUT lError, 
        OUTPUT cMessage).
        
IF lError THEN
    RETURN.
        
ASSIGN 
    opdItemCost  = dCostPerUOM  
    opdSetupCost = dCostSetup
    . 
