
/*------------------------------------------------------------------------
    File        : FGProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Feb 08 13:04:49 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{fg/ttFGItem.i}

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */


PROCEDURE FG_BuildFGItemForCustPart:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustItem AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttFGItem.

    DEFINE VARIABLE lMultipleItemCheck AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttFGItem.

    RUN pBuildFGItemForCustPart (
        INPUT  ipcCompany,
        INPUT  ipcItemID,
        INPUT  ipcCustItem,
        INPUT  FALSE,  /* Check for multiple items only */
        OUTPUT lMultipleItemCheck,
        OUTPUT TABLE ttFGItem
        ).
END PROCEDURE.

PROCEDURE FG_ExpirePricesByItem:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID   AS CHARACTER NO-UNDO.
    
    RUN Price_ExpirePriceMatrixByItem( 
        INPUT ipcCompany,
        INPUT ipcItemID
        ).     
    RUN Price_ExpireQuotesByItem(
        INPUT ipcCompany,
        INPUT ipcItemID
        ).
    RUN Vendor_ExpirePriceByItem(
        INPUT ipcCompany,
        INPUT ipcItemID
        ).     

END PROCEDURE.

PROCEDURE FG_GetDefaultFG:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcItemID        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcItemName      AS CHARACTER   NO-UNDO.
    
    DEFINE BUFFER opbf-itemfg FOR itemfg.    
    
    RUN pGetDefaultFGBuffer(
        INPUT  ipcCompany,
        buffer opbf-itemfg
        ). 
    IF AVAIL opbf-itemfg THEN
    ASSIGN
        opcItemID = opbf-itemfg.i-no
        opcItemName = opbf-itemfg.i-name.
END PROCEDURE.

PROCEDURE FG_HasMultipleFGItemsForCustPart:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustItem      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcItemID        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplMulitpleItems AS LOGICAL   NO-UNDO.

    EMPTY TEMP-TABLE ttFGItem.
    
    RUN pBuildFGItemForCustPart (
        INPUT  ipcCompany,
        INPUT  ipcItemID,
        INPUT  ipcCustItem,
        INPUT  TRUE,  /* Check for multiple items only */
        OUTPUT oplMulitpleItems,
        OUTPUT TABLE ttFGItem
        ). 
    
    FIND FIRST ttFGItem NO-ERROR.
    IF AVAILABLE ttFGitem THEN
        opcItemID = ttFGItem.itemID.
    
    EMPTY TEMP-TABLE ttFGItem.
END PROCEDURE.

PROCEDURE pBuildFGItemForCustPart PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID              AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustItem            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsMultipleItemCheck AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplMulitpleItems       AS LOGICAL   NO-UNDO.    
    DEFINE OUTPUT PARAMETER TABLE FOR ttFGItem.

    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
        
    DEFINE BUFFER bf-itemfg    FOR itemfg.
    DEFINE BUFFER bf-cust-part FOR cust-part.
    
    FOR EACH bf-cust-part NO-LOCK
        WHERE bf-cust-part.company EQ ipcCompany
          AND bf-cust-part.part-no BEGINS ipcCustItem,         
        FIRST bf-itemfg NO-LOCK
        WHERE bf-itemfg.company EQ bf-cust-part.company
          AND bf-itemfg.i-no    EQ bf-cust-part.i-no
          AND (bf-itemfg.stat   EQ "A" OR bf-itemfg.q-onh GT 0):
        IF NOT bf-cust-part.i-no BEGINS ipcItemID THEN
            NEXT.
        
        FIND FIRST ttFGItem
             WHERE ttFGItem.company        EQ bf-cust-part.company
               AND ttFGItem.itemID         EQ bf-cust-part.i-no
               AND ttFGItem.customerPartID EQ bf-cust-part.part-no
             NO-ERROR.
        IF AVAILABLE ttFGItem THEN
            NEXT.
        
        iCount = iCount + 1.
        
        IF iplIsMultipleItemCheck AND iCount GT 1 THEN
            LEAVE.
            
        CREATE ttFGItem.
        ASSIGN
            ttFGItem.company        = bf-itemfg.company
            ttFGItem.itemID         = bf-itemfg.i-no
            ttFGItem.itemDesc       = bf-itemfg.i-dscr
            ttFGItem.itemName       = bf-itemfg.i-name
            ttFGItem.customerPartID = bf-cust-part.part-no
            ttFGItem.quantityOnHand = bf-itemfg.q-onh
            .
    END.
    
    oplMulitpleItems = iCount GT 1.
END PROCEDURE.

PROCEDURE pGetDefaultFGBuffer PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-itemfg      FOR itemfg.
    
    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound       AS LOGICAL NO-UNDO.
    DEFINE BUFFER bf-itemfg FOR itemfg.
    RUN sys/ref/nk1Look.p(INPUT ipcCompany,
                        INPUT "FGMASTER",
                        INPUT "C",
                        INPUT NO,
                        INPUT NO,
                        INPUT "",
                        INPUT "",
                        OUTPUT cReturnValue,
                        OUTPUT lFound).
    IF lFound THEN 
    FIND FIRST ipbf-itemfg NO-LOCK  
         WHERE ipbf-itemfg.company = ipcCompany
         AND ipbf-itemfg.i-no EQ cReturnValue 
         NO-ERROR.

END PROCEDURE.
