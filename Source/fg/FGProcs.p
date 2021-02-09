
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
    DEFINE INPUT  PARAMETER ipcCustItem AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttFGItem.

    EMPTY TEMP-TABLE ttFGItem.

    RUN pBuildFGItemForCustPart (
        INPUT ipcCompany,
        INPUT ipcCustItem,
        OUTPUT TABLE ttFGItem
        ).
END PROCEDURE.

PROCEDURE FG_HasMultipleFGItemsForCustPart:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustItem      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplMulitpleItems AS LOGICAL   NO-UNDO.
    
    RUN pHasMultipleFGItemsForCustPart (
        INPUT  ipcCompany,
        INPUT  ipcCustItem,
        OUTPUT oplMulitpleItems 
        ). 
END PROCEDURE.

PROCEDURE pBuildFGItemForCustPart PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustItem AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttFGItem.
        
    DEFINE BUFFER bf-itemfg FOR itemfg.
    
    FOR EACH bf-itemfg NO-LOCK
        WHERE bf-itemfg.company EQ ipcCompany
          AND bf-itemfg.part-no BEGINS ipcCustItem
          AND (bf-itemfg.stat   EQ "A" OR bf-itemfg.q-onh GT 0):
        CREATE ttFGItem.
        ASSIGN
            ttFGItem.company        = bf-itemfg.company
            ttFGItem.itemID         = bf-itemfg.i-no
            ttFGItem.itemDesc       = bf-itemfg.i-dscr
            ttFGItem.itemName       = bf-itemfg.i-name
            ttFGItem.customerPartID = bf-itemfg.part-no
            ttFGItem.quantityOnHand = bf-itemfg.q-onh
            .
    END.
END PROCEDURE.

PROCEDURE pHasMultipleFGItemsForCustPart PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Returns if a customer part exists for multiple items
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustItem      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplMulitpleItems AS LOGICAL   NO-UNDO.

    DEFINE BUFFER bf-itemfg FOR itemfg.
    
    FIND bf-itemfg NO-LOCK
        WHERE bf-itemfg.company EQ ipcCompany
          AND bf-itemfg.part-no BEGINS ipcCustItem
          AND (bf-itemfg.stat   EQ "A" OR bf-itemfg.q-onh GT 0)
        NO-ERROR.
    IF AMBIGUOUS bf-itemfg THEN
        oplMulitpleItems = TRUE.

END PROCEDURE.

