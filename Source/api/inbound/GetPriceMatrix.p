/*------------------------------------------------------------------------
    File        : api/inbound/GetPriceMatrix.p
    Purpose     : Returns the price and price UOM for given inputs

    Syntax      :

    Description : Returns the price and price UOM for given inputs

    Author(s)   : Porandla Mithun
    Created     : Mon Jul 06 07:33:22 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcCustID   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcShipToID AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemID   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opriOePrmtx AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.

{Inventory/ttInventory.i "NEW SHARED"}

DEFINE VARIABLE hdPriceProcs      AS HANDLE NO-UNDO.
DEFINE VARIABLE hdInventoryProcs  AS HANDLE NO-UNDO.

RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.

/* Input validation */
RUN pValidateInputs (
    INPUT  ipcCompany,
    INPUT  ipcCustID,
    INPUT  ipcShipToID,
    INPUT  ipcItemID,
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ) NO-ERROR.
  
IF ERROR-STATUS:ERROR THEN
    opcMessage = ERROR-STATUS:GET-MESSAGE(1).

/* Input processing */
IF oplSuccess THEN DO: 
    RUN pProcessInputs (
        INPUT  ipcCompany,
        INPUT  ipcCustID,
        INPUT  ipcShipToID,
        INPUT  ipcItemID,
        OUTPUT opriOePrmtx,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
        opcMessage = ERROR-STATUS:GET-MESSAGE(1).
END.
    
DELETE PROCEDURE hdPriceProcs.
DELETE PROCEDURE hdInventoryProcs.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pProcessInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcShipToID       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opriOePrmtx       AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess        AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage        AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQuantityInEA          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lPriceMatrixMatchFound AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-itemfg FOR itemfg.
    
    RUN pGetItemBuffer (
        INPUT  ipcCompany,
        INPUT  ipcItemID,
        INPUT  ipcCustID,
        BUFFER bf-itemfg
        ) NO-ERROR.
    IF NOT AVAILABLE bf-itemfg THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid ItemID '" + ipcItemID + "'"
            .
        RETURN.
    END.

    RUN GetPriceMatrix IN hdPriceProcs (
        INPUT  ipcCompany,
        INPUT  ipcItemID,
        INPUT  ipcCustID,
        INPUT  ipcShipToID,
        OUTPUT opriOePrmtx,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).
END PROCEDURE.

PROCEDURE pValidateInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Procedure to validate inputs
 Notes:
------------------------------------------------------------------------------*/     
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcShipToID    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-itemfg FOR itemfg.
    
    /* Company validation */
    IF ipcCompany EQ "" THEN DO:
        ASSIGN
            opcMessage = "Empty Company"
            oplSuccess = NO
            .
        RETURN.
    END.

    IF NOT CAN-FIND(FIRST company NO-LOCK
                    WHERE company.company EQ ipcCompany) THEN DO:
        ASSIGN
            opcMessage = "Invalid Company '" + ipcCompany + "'"
            oplSuccess = NO
            .
        RETURN.
    END.

    IF ipcItemID EQ "" THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Empty ItemID"
            .
        RETURN.
    END.

    RUN pGetItemBuffer (
        INPUT  ipcCompany,
        INPUT  ipcItemID,
        INPUT  ipcCustID,
        BUFFER bf-itemfg
        ).
    IF NOT AVAILABLE bf-itemfg THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid ItemID '" + ipcItemID + "'"
            .
        RETURN.
    END.
        
    IF ipcCustID NE "" THEN DO:
        RUN ValidateCust IN hdInventoryProcs (
            INPUT  ipcCompany,
            INPUT  ipcCustID,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.

        IF ERROR-STATUS:ERROR THEN
            opcMessage = ERROR-STATUS:GET-MESSAGE(1).

        IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
            RETURN.
    END.

    IF ipcShipToID NE "" THEN DO:
        RUN ValidateShipTo IN hdInventoryProcs (
            INPUT  ipcCompany,
            INPUT  ipcCustID,
            INPUT  ipcShipToID,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.

        IF ERROR-STATUS:ERROR THEN
            opcMessage = ERROR-STATUS:GET-MESSAGE(1).

        IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
            RETURN.
    END.
    
    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE pGetItemBuffer PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to get itemfg buffer
     Notes:
    ------------------------------------------------------------------------------*/     
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustID  AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-itemfg FOR itemfg.

    FIND FIRST opbf-itemfg
         WHERE opbf-itemfg.company EQ ipcCompany
           AND opbf-itemfg.i-no    EQ ipcItemID
         NO-ERROR.
    IF NOT AVAILABLE opbf-itemfg THEN
        FIND FIRST opbf-itemfg
             WHERE opbf-itemfg.company EQ ipcCompany
               AND opbf-itemfg.part-no EQ ipcItemID
               AND opbf-itemfg.cust-no EQ ipcCustID
             NO-ERROR.
END PROCEDURE.

    