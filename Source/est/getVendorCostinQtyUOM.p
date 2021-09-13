
/*------------------------------------------------------------------------
    File        : getVendorCostinQtyUOM.p
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
DEFINE INPUT  PARAMETER ipcCompanyCode      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemCode         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemType         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcVendorNo         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcEstNo            AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiFormNo           AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiBlankNo          AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipdQty              AS DECIMAL NO-UNDO.
DEFINE INPUT  PARAMETER ipcQtyUOM           AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostQtyUOM       AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdSetupCostQtyUOM  AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostTotal        AS DECIMAL NO-UNDO.


DEFINE VARIABLE cQtyUOM     AS CHARACTER NO-UNDO.
DEFINE VARIABLE dCostPerUOM AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCostSetup  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCostTotal  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cCostUOM    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
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
        ipcEstNo, 
        ipiFormNo, 
        ipiBlankNo,
        ipdQty, 
        ipcQtyUOM,
        bf-Item.s-len, 
        bf-Item.s-wid, 
        bf-Item.s-dep, 
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
    
opdCostTotal = dCostTotal.

    
IF cCostUOM EQ "" THEN 
    cCostUOM = "EA".
        
IF cCostUOM NE ipcQtyUOM THEN 
DO:
        
    RUN pConvertCostFromUOMToUOM(ipcCompanyCode, ipcItemCode, ipcItemType, cCostUOM, ipcQtyUOM, 
        bf-Item.basis-w, bf-Item.s-len, bf-Item.s-wid, bf-Item.s-dep, 
        dCostPerUOM, OUTPUT opdCostQtyUOM).
        
    RUN pConvertCostFromUOMToUOM(ipcCompanyCode, ipcItemCode, ipcItemType, cCostUOM, ipcQtyUOM, 
        bf-Item.basis-w, bf-Item.s-len, bf-Item.s-wid, bf-Item.s-dep, 
        dCostSetup, OUTPUT opdSetupCostQtyUOM).
        
        
END. 
        


PROCEDURE pConvertCostFromUOMToUOM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Wrapper procedure for conversion programs.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFromUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcToUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdBasisWeight AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDepth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostInFromUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostInToUOM AS DECIMAL NO-UNDO.

    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

    RUN Conv_ValueFromUOMtoUOM (ipcCompany, ipcItemID, ipcItemType,
        ipdCostInFromUOM, ipcFromUOM, ipcToUOM,
        ipdBasisWeight, ipdLength, ipdWidth, ipdDepth, 0,
        OUTPUT opdCostInToUOM, OUTPUT lError, OUTPUT cMessage).
    
    
END PROCEDURE.     
    