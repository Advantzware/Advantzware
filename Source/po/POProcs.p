
/*------------------------------------------------------------------------
    File        : POProcs.p
    Purpose     : 

    Syntax      :

    Description : Holds procedures for entering, editing and processing purchase orders

    Author(s)   : Rahul Rawat
    Created     : Wed Mar 25 02:17:43 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE CheckPOLineStatus:
/*------------------------------------------------------------------------------
 Purpose: To Check a PO line status
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoNo    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoLine  AS INTEGER   NO-UNDO.
    
    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company EQ ipcCompany
           AND po-ordl.po-no   EQ ipiPoNo
           AND po-ordl.line    EQ ipiPoLine
         NO-ERROR.
    IF AVAILABLE po-ordl AND NOT po-ordl.opened THEN DO:
        RUN DisplayMessage("19").
        RETURN ERROR.
    END. 


END PROCEDURE.

PROCEDURE pGetPOLineAdderData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Main logic to process all adders for a given PO line
        Note - this has the ability to override key dimensions for conversion purposes
        or for Screen-value processing
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoNo     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoLine   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPoLen    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPoWid    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPoDep    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCostUom  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostSetup AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAdderText AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-po-ordl-add FOR po-ordl-add.
    DEFINE BUFFER bf-item FOR ITEM.
    
    DEFINE VARIABLE dCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    
        FOR EACH bf-po-ordl-add NO-LOCK    
            WHERE bf-po-ordl-add.company EQ ipcCompany
            AND bf-po-ordl-add.po-no   EQ ipiPoNo
            AND bf-po-ordl-add.line    EQ ipiPoLine,
            FIRST bf-item NO-LOCK 
            WHERE bf-item.company  EQ bf-po-ordl-add.company
            AND bf-item.i-no     EQ bf-po-ordl-add.adder-i-no
            AND bf-item.mat-type EQ "A" :
            
            IF ipcCostUOM NE bf-po-ordl-add.pr-uom THEN 
            DO:
                RUN Conv_ValueFromUOMToUOM (
                    INPUT  bf-po-ordl-add.company,
                    INPUT  bf-po-ordl-add.adder-i-no,
                    INPUT  "RM",
                    INPUT  bf-po-ordl-add.cost,
                    INPUT  bf-po-ordl-add.pr-uom, 
                    INPUT  ipcCostUOM,
                    INPUT  bf-item.basis-w,
                    INPUT  ipdPOLen,
                    INPUT  ipdPOWid,
                    INPUT  ipdPODep,
                    INPUT  0,
                    OUTPUT dCostPerUOM,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).
            END.
            ELSE 
                dCostPerUOM = bf-po-ordl-add.cost.
                 
            ASSIGN 
                opdCostPerUOM = opdCostPerUOM + dCostPerUOM
                opdCostSetup = opdCostSetup + bf-po-ordl-add.setup
                opcAdderText = opcAdderText + SUBSTR(bf-item.i-name,1,18) +
                    FILL(' ',19 - LENGTH(SUBSTR(bf-item.i-name,1,18))) +
                    STRING(bf-po-ordl-add.cost,'-z,zz9.99') + STRING(bf-po-ordl-add.setup,'-zzz9.99') + CHR(10)
                .          
        END.

END PROCEDURE.

PROCEDURE PO_GetAddersCostInCostUOM:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoNo     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoLine   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPoLen    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPoWid    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPoDep    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCostUom  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOM AS DECIMAL   NO-UNDO.    
    
    DEFINE VARIABLE dCostSetup AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cAdderText AS CHARACTER NO-UNDO.
    
    RUN pGetPOLineAdderData(ipcCompany, ipiPoNo, ipiPoLine, ipdPoLen, ipdPoWid, ipdPODep, ipcCostUOM, OUTPUT opdCostPerUOM, OUTPUT dCostSetup, OUTPUT cAdderText).
    
END PROCEDURE.

PROCEDURE PO_GetAddersText:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoLine     AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAdderText  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dCostSetup AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOM AS DECIMAL NO-UNDO.
    
    RUN pGetPOLineAdderData(ipcCompany, ipiPoNo, ipiPoLine, 0, 0, 0, "", OUTPUT dCostPerUOM, OUTPUT dCostSetup, OUTPUT opcAdderText).

END PROCEDURE.

PROCEDURE PO_CreatePoAdders:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoLine     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER IpcItemID     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostPerUom AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdSetupCost  AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcPriceUom   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-Po-ordl-add FOR po-ordl-add.
    
    CREATE bf-po-ordl-add. 
    ASSIGN 
        bf-po-ordl-add.company    = ipcCompany
        bf-po-ordl-add.po-no      = ipiPoNo
        bf-po-ordl-add.line       = ipiPoLine
        bf-Po-ordl-add.adder-i-no = ipcItemID
        bf-Po-ordl-add.cost       = ipdCostPerUom
        bf-po-ordl-add.setup      = ipdSetupCost
        bf-po-ordl-add.pr-uom     = ipcPriceUom
        .
        
    RELEASE bf-po-ordl-add.
END PROCEDURE.

PROCEDURE PO_GetLineScoresAndTypes:
/*------------------------------------------------------------------------------
 Purpose: Procedure to fetch the score size and types from reftable
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoID       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoLine     AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdScores     AS DECIMAL   NO-UNDO EXTENT 20.
    DEFINE OUTPUT PARAMETER opcScoreTypes AS CHARACTER NO-UNDO EXTENT 20.
    
    DEFINE VARIABLE iIndex         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hdFormulaProcs AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lRecFound      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cSizeFormat    AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-po-ordl   FOR po-ordl.
    
    FIND FIRST bf-po-ordl NO-LOCK
         WHERE bf-po-ordl.company EQ ipcCompany
           AND bf-po-ordl.po-no   EQ ipiPoID
           AND bf-po-ordl.line    EQ ipiPoLine
         NO-ERROR.
    IF NOT AVAILABLE bf-po-ordl THEN
        RETURN.

    RUN system/FormulaProcs.p PERSISTENT SET hdFormulaProcs.
    
    RUN Formula_BuildAndSavePanelDetailsForPO IN hdFormulaProcs (
        INPUT ROWID(bf-po-ordl)
        ).
    
    RUN GetPanelScoreAndTypeForPO IN hdFormulaProcs (
        INPUT  bf-po-ordl.company,
        INPUT  bf-po-ordl.po-no,
        INPUT  bf-po-ordl.line,
        INPUT  IF bf-po-ordl.spare-char-1 EQ "LENGTH" THEN "L" ELSE "W",
        OUTPUT opdScores,
        OUTPUT opcScoreTypes
        ).

    RUN sys/ref/nk1look.p (
        INPUT ipcCompany,     /* Company Code */ 
        INPUT "CECSCRN",      /* sys-ctrl name */
        INPUT "C",            /* Output return value */
        INPUT NO,             /* Use ship-to */
        INPUT NO,             /* ship-to vendor */
        INPUT "",             /* ship-to vendor value */
        INPUT "",             /* shi-id value */
        OUTPUT cSizeFormat, 
        OUTPUT lRecFound
        ).
    
    IF cSizeFormat NE "Decimal" THEN DO:                   
        DO iIndex = 1 TO EXTENT(opdScores):
            IF opdScores[IIndex] EQ 0 THEN
                NEXT.
            
            IF cSizeFormat EQ "16th's" THEN    
                RUN ConvertDecimalTo16ths IN hdFormulaProcs (
                    INPUT-OUTPUT opdScores[iIndex]
                    ).
            ELSE IF cSizeFormat EQ "32nd's" THEN
                RUN ConvertDecimalTo32nds IN hdFormulaProcs (
                    INPUT-OUTPUT opdScores[iIndex]
                    ).
        END.
    END.
    
    DELETE PROCEDURE hdFormulaProcs.
    
END PROCEDURE.

PROCEDURE PO_RecalculateCostsPO:
/*------------------------------------------------------------------------------
 Purpose: Public wrapper for pRecalculateCostPO
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriPOOrd AS ROWID NO-UNDO.
    
    RUN pRecalculateCostsPO(ipriPOOrd).
    

END PROCEDURE.

PROCEDURE PO_UpdatePoAdders:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoLine     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCostPerUom AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdSetupCost  AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcPriceUom   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-Po-ordl-add FOR po-ordl-add.
    
    FIND FIRST bf-Po-ordl-add EXCLUSIVE-LOCK 
         WHERE bf-Po-ordl-add.company    EQ ipcCompany
           AND bf-Po-ordl-add.po-no      EQ ipiPoNo
           AND bf-Po-ordl-add.line       EQ ipiPoLine
           AND bf-Po-ordl-add.adder-i-no EQ ipcItemID
         NO-ERROR.  
    IF AVAILABLE bf-Po-ordl-add THEN 
        ASSIGN 
            bf-Po-ordl-add.cost   = ipdCostPerUom
            bf-po-ordl-add.setup  = ipdSetupCost
            bf-po-ordl-add.pr-uom = ipcPriceUom
            .  
             
    RELEASE bf-Po-ordl-add. 
END PROCEDURE.

PROCEDURE PO_CheckPurchaseLimit:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-po-ord FOR po-ord.
    DEFINE OUTPUT PARAMETER oplCheckHold AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdPurchaseLimit AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dPurchaseCost AS DECIMAL NO-UNDO.    
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    
    FIND FIRST users NO-LOCK
         WHERE users.user_id EQ USERID(LDBNAME(1)) NO-ERROR .
    opdPurchaseLimit = users.purchaseLimit .
         
    FOR EACH bf-po-ordl NO-LOCK
        WHERE bf-po-ordl.company EQ ipbf-po-ord.company
        AND bf-po-ordl.po-no EQ ipbf-po-ord.po-no :
        dPurchaseCost = dPurchaseCost + bf-po-ordl.t-cost .
    END.
    IF dPurchaseCost GT opdPurchaseLimit THEN
    DO:
        oplCheckHold = YES .
    END.
END PROCEDURE.

PROCEDURE pRecalculateCostsPO PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Runs pRecalculateCostPOLine for each po line of a given po-ord
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriPOOrd AS ROWID NO-UNDO.
    
    DEFINE BUFFER bf-po-ord FOR po-ord.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    
    FIND bf-po-ord NO-LOCK 
        WHERE ROWID(bf-po-ord) EQ ipriPOOrd NO-ERROR.
    IF AVAILABLE bf-po-ord THEN DO:
        FOR EACH bf-po-ordl NO-LOCK 
            WHERE bf-po-ordl.company EQ bf-po-ord.company
            AND bf-po-ordl.po-no EQ bf-po-ord.po-no:
            RUN pRecalculateCostsPOLine(ROWID(bf-po-ordl)).
        END.
    END.

END PROCEDURE.

PROCEDURE pRecalculateCostsPOLine PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Given a PO line, recalculate the unit, setup, adder and total costs
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriPOOrdl AS ROWID NO-UNDO.
    
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-po-ord FOR po-ord.
    DEFINE BUFFER bf-item FOR ITEM.
    
    DEFINE VARIABLE dCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostSetup AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostTotal AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cCostUOM AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dBasisWeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cItemType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQtyInCostUOM AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostPerUOMAdders AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostSetupAdders AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cAdderText AS CHARACTER NO-UNDO.
    
    FOR bf-po-ordl NO-LOCK 
        WHERE ROWID(bf-po-ordl) EQ ipriPOOrdl
        ,
        FIRST bf-po-ord NO-LOCK 
        WHERE bf-po-ord.company EQ bf-po-ordl.company
        AND bf-po-ord.po-no EQ bf-po-ordl.po-no:
        
        IF bf-po-ordl.item-type THEN DO:
            cItemType = 'RM'.
            FIND FIRST bf-item NO-LOCK 
            WHERE bf-item.company EQ bf-po-ordl.company
            AND bf-item.i-no EQ bf-po-ordl.i-no
            NO-ERROR.
            IF AVAILABLE bf-item THEN 
                dBasisWeight = bf-item.basis-w.
            
            RUN pGetPOLineAdderData(bf-po-ordl.company, 
                bf-po-ordl.po-no, 
                bf-po-ordl.line, 
                bf-po-ordl.s-len, 
                bf-po-ordl.s-wid, 
                bf-po-ordl.s-dep, 
                bf-po-ordl.pr-uom,
                OUTPUT dCostPerUOMAdders,
                OUTPUT dCostSetupAdders,
                OUTPUT cAdderText).                    
        END.
        ELSE 
            cItemType = 'FG'.
            
        RUN GetVendorCost(bf-po-ordl.company, 
            bf-po-ordl.i-no, 
            cItemType, 
            bf-po-ord.vend-no, 
            bf-po-ordl.cust-no, 
            "", 0, 0, /*Estimate/F/B*/
            bf-po-ordl.ord-qty, 
            bf-po-ordl.pr-qty-uom,
            bf-po-ordl.s-len, 
            bf-po-ordl.s-wid, 
            bf-po-ordl.s-dep, 
            "IN", 
            IF AVAILABLE bf-item THEN bf-item.basis-w ELSE 0, 
            "LB/EA", 
            NO,
            OUTPUT dCostPerUOM, 
            OUTPUT dCostSetup, 
            OUTPUT cCostUOM,
            OUTPUT dCostTotal, 
            OUTPUT lError, 
            OUTPUT cMessage).  
        
        IF lError THEN RETURN.
        
        
        ASSIGN 
            dCostPerUOM = dCostPerUOM + dCostPerUOMAdders
            dCostSetup = dCostSetup + dCostSetupAdders
            .
        
        IF bf-po-ordl.pr-uom NE bf-po-ordl.pr-qty-uom THEN 
            RUN Conv_QuantityFromUOMToUOM (
                bf-po-ordl.company,
                bf-po-ordl.i-no,
                cItemType,
                bf-po-ordl.ord-qty,
                bf-po-ordl.pr-qty-uom, 
                bf-po-ordl.pr-uom,
                dBasisWeight,
                bf-po-ordl.s-len, 
                bf-po-ordl.s-wid, 
                bf-po-ordl.s-dep,
                0,
                OUTPUT dQtyInCostUOM,
                OUTPUT lError,
                OUTPUT cMessage
                ). 
        ELSE 
            dQtyInCostUOM = bf-po-ordl.ord-qty.
                
        dCostTotal = dQtyInCostUOM * dCostPerUOM + dCostSetup.  
                                  
        IF bf-po-ordl.disc NE 0 THEN
            dCostTotal = dCostTotal * (1 - (bf-po-ordl.disc / 100)).
        IF dCostTotal NE 0 THEN DO:
            FIND CURRENT bf-po-ordl EXCLUSIVE-LOCK. 
            ASSIGN 
                bf-po-ordl.cost = dCostPerUOM
                bf-po-ordl.pr-uom = cCostUOM
                bf-po-ordl.setup = dCostSetup
                bf-po-ordl.t-cost = dCostTotal
                .
            FIND CURRENT bf-po-ordl NO-LOCK.             
        END.
    END.

END PROCEDURE.

