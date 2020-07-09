/*------------------------------------------------------------------------
    File        : api/inbound/GetPrice.p
    Purpose     : Returns the price and price UOM for given inputs

    Syntax      :

    Description : Returns the price and price UOM for given inputs

    Author(s)   : Porandla Mithun
    Created     : Mon Jul 06 07:33:22 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcCustID         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcShipToID       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemID         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipdQuantity       AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcQuantityUOM    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTargetUOM      AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opdPrice          AS DECIMAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcPriceUOM       AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess        AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage        AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdPriceProcs      AS HANDLE NO-UNDO.
DEFINE VARIABLE hdConversionProcs AS HANDLE NO-UNDO.
DEFINE VARIABLE hdValidateProcs   AS HANDLE NO-UNDO.

RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
RUN system/ConversionProcs.p PERSISTENT SET hdConversionProcs.
RUN util/Validate.p PERSISTENT SET hdValidateProcs.

/* Input validation */
RUN pValidateInputs (
    INPUT  ipcCompany,
    INPUT  ipcCustID,
    INPUT  ipcShipToID,
    INPUT  ipcItemID,
    INPUT  ipdQuantity,
    INPUT  ipcQuantityUOM,
    INPUT  ipcTargetUOM,
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ) NO-ERROR.
  
IF ERROR-STATUS:ERROR THEN
    opcMessage = ERROR-STATUS:GET-MESSAGE(1).

IF oplSuccess THEN DO:
    /* Input processing */
    RUN pProcessInputs (
        INPUT  ipcCompany,
        INPUT  ipcCustID,
        INPUT  ipcShipToID,
        INPUT  ipcItemID,
        INPUT  ipdQuantity,
        INPUT  ipcQuantityUOM,
        INPUT  ipcTargetUOM,
        OUTPUT opdPrice,
        OUTPUT opcPriceUOM,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
      
    IF ERROR-STATUS:ERROR THEN
        opcMessage = ERROR-STATUS:GET-MESSAGE(1).
END.

DELETE PROCEDURE hdPriceProcs.
DELETE PROCEDURE hdConversionProcs.
DELETE PROCEDURE hdValidateProcs.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pProcessInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Process the inputs to get the result
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcShipToID       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantity       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTargetUOM      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdPrice          AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPriceUOM       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess        AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage        AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQuantityInEA          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lPriceMatrixMatchFound AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lError                 AS LOGICAL NO-UNDO.
    
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
    
    RUN Conv_QtyToEA IN hdConversionProcs (
        INPUT  ipcCompany,
        INPUT  ipcItemID,
        INPUT  ipdQuantity,
        INPUT  ipcQuantityUOM,
        INPUT  bf-itemfg.case-count,
        OUTPUT dQuantityInEA
        ).
    
    RUN GetPriceMatrixPriceSimple IN hdPriceProcs (
        INPUT  ipcCompany,
        INPUT  ipcItemID,
        INPUT  ipcCustID,
        INPUT  ipcShipToID,
        INPUT  dQuantityInEA,
        OUTPUT lPriceMatrixMatchFound,
        INPUT-OUTPUT opdPrice,
        INPUT-OUTPUT opcPriceUOM
        ).
    IF NOT lPriceMatrixMatchFound THEN
        ASSIGN
            opdPrice    = bf-itemfg.sell-price
            opcPriceUOM = bf-itemfg.sell-uom
            .
    
    IF ipcTargetUOM NE "" THEN DO:
        RUN Conv_ValueFromUOMToUOMForItem IN hdConversionProcs (
            INPUT  ROWID(bf-itemfg),
            INPUT  opdPrice,
            INPUT  opcPriceUOM,
            INPUT  ipcTargetUOM,
            OUTPUT opdPrice,
            OUTPUT lError,
            OUTPUT opcMessage            
            ) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            opcMessage = ERROR-STATUS:GET-MESSAGE(1).

        IF ERROR-STATUS:ERROR OR lError THEN
            RETURN.            
        
        opcPriceUOM = ipctargetUOM.
    END.
    
    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
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
    DEFINE INPUT  PARAMETER ipdQuantity    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTargetUOM   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cValidItemUOMList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError            AS LOGICAL   NO-UNDO.
    
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
    
    RUN Conv_GetValidPriceUOMsForItem IN hdConversionProcs (
        INPUT  ROWID(bf-itemfg),
        OUTPUT cValidItemUOMList,
        OUTPUT lError,
        OUTPUT opcMessage
        ).
    IF ipcQuantityUOM NE "" AND LOOKUP(ipcQuantityUOM, cValidItemUOMList) LE 0 THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid QuantityUOM '" + ipcQuantityUOM + "' for item '" + ipcItemID
            .
        RETURN.
    END.

    IF ipcTargetUOM NE "" AND LOOKUP(ipcTargetUOM, cValidItemUOMList) LE 0 THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid TargetUOM '" + ipcTargetUOM + "' for item '" + ipcItemID
            .
        RETURN.
    END.
    
    IF ipcCustID NE "" THEN DO:
        RUN pIsValidCustomerID IN hdValidateProcs (
            INPUT  ipcCustID,
            INPUT  TRUE,     /* Is required */
            INPUT  ipcCompany,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.

        IF ERROR-STATUS:ERROR THEN
            opcMessage = ERROR-STATUS:GET-MESSAGE(1).

        IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
            RETURN.
    END.

    IF ipcShipToID NE "" THEN DO:
        RUN pIsValidShiptoID IN hdValidateProcs (
            INPUT  ipcCustID,
            INPUT  ipcShipToID,
            INPUT  TRUE, /* Is required */
            INPUT  ipcCompany,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.

        IF ERROR-STATUS:ERROR THEN
            opcMessage = ERROR-STATUS:GET-MESSAGE(1).

        IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
            RETURN.
    END.
    
    IF ipdQuantity LE 0 THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Quantity cannot be less than or equal zero"
            .
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

    