
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
DEFINE INPUT  PARAMETER ipdLength           AS DECIMAL NO-UNDO.
DEFINE INPUT  PARAMETER ipdWidth            AS DECIMAL NO-UNDO.
DEFINE INPUT  PARAMETER ipdDepth            AS DECIMAL NO-UNDO.
DEFINE INPUT  PARAMETER ipdBasisWeight      AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostQtyUOM       AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdSetupCost        AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostTotal        AS DECIMAL NO-UNDO.


DEFINE VARIABLE cQtyUOM     AS CHARACTER NO-UNDO.
DEFINE VARIABLE dCostPerUOM AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCostSetup  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCostTotal  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cCostUOM    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendorID   AS CHARACTER NO-UNDO.
DEFINE VARIABLE dCostDeviation AS DECIMAL NO-UNDO.
DEFINE VARIABLE cScope         AS CHARACTER NO-UNDO.


DEFINE BUFFER bf-Item FOR Item.


FIND FIRST bf-Item NO-LOCK
    WHERE bf-Item.company = ipcCompanyCode
    AND bf-Item.i-no    = ipcItemCode NO-ERROR.
      
IF AVAILABLE bf-Item THEN
DO:
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
            ipdLength,
            ipdWidth,
            ipdDepth,
            "IN", 
            ipdBasisWeight,
            "LB/EA", 
            NO,
            OUTPUT dCostPerUOM, 
            OUTPUT dCostSetup, 
            OUTPUT cCostUOM,
            OUTPUT dCostTotal, 
            OUTPUT lError, 
            OUTPUT cMessage).
            
    /* IF Any error or No cost available for Vendor or if Blank Vendor record is not there,
        fall back to this logic */
        
    IF lError OR dCostTotal = 0 THEN
    DO:
        cScope = DYNAMIC-FUNCTION("VendCost_GetValidScopes","Est-RM-Over").
        
        RUN VendCost_GetBestCost(bf-Item.company, 
            bf-Item.i-no, 
            ipcItemType, 
            cScope, 
            YES, 
            ipcEstNo,
            ipiFormNo, 
            ipiBlankNo,
            ipdQty, 
            ipcQtyUOM, 
            ipdLength, 
            ipdWidth, 
            ipdDepth, 
            "IN", 
            ipdBasisWeight, 
            "LBS/MSF",
            OUTPUT dCostPerUOM, 
            OUTPUT cCostUOM, 
            OUTPUT dCostSetup, 
            OUTPUT cVendorID, 
            OUTPUT dCostDeviation,
            OUTPUT dCostTotal,
            OUTPUT lError, 
            OUTPUT cMessage).
    END.
          
END.                            
        
IF lError THEN
    RETURN.
    
opdCostTotal = dCostTotal.

    
IF cCostUOM EQ "" THEN 
    cCostUOM = "EA".
        
IF cCostUOM NE ipcQtyUOM THEN 
DO:
        
    RUN pConvertCostFromUOMToUOM(ipcCompanyCode, ipcItemCode, ipcItemType, cCostUOM, ipcQtyUOM, 
        ipdBasisWeight, ipdLength, ipdWidth, ipdDepth, 
        dCostPerUOM, OUTPUT opdCostQtyUOM).
    opdSetupCost = dCostSetup.      
        
END.
ELSE
    ASSIGN
        opdCostQtyUOM      = dCostPerUOM
        opdSetupCost       = dCostSetup. 
        


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
    