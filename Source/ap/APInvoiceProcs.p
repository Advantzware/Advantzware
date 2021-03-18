
/*------------------------------------------------------------------------
    File        : APInvoiceProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Thu Feb 25 09:37:06 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE APInvoice_AutoSelectReceipts:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAPInvoiceLineRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityToInvoice   AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError               AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage             AS CHARACTER NO-UNDO.

    RUN pAutoSelectReceipts (
        INPUT  ipcAPInvoiceLineRecKey,
        INPUT  ipdQuantityToInvoice,
        INPUT  ipcQuantityUOM,
        OUTPUT oplError,
        OUTPUT opcMessage
        ).    
END PROCEDURE.

PROCEDURE pAutoSelectReceipts PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAPInvoiceLineRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityToInvoice   AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError               AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage             AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dQuantityAvailableToInvoice AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError                      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage                    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQuantityInvoiced           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dRunningQuantityToInvoice   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dReceiptQuantityInvoiced    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dItemBasisWeight            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQuantityAvailable          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cItemType                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReceiptUOM                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cConvertedQuantityUOM       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityInvoicedUOM        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecordsFound               AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-ap-invl       FOR ap-invl.
    DEFINE BUFFER bf-po-ord        FOR po-ord.
    DEFINE BUFFER bf-po-ordl       FOR po-ordl.
    DEFINE BUFFER bf-fg-rdtlh      FOR fg-rdtlh.
    DEFINE BUFFER bf-rm-rdtlh      FOR rm-rdtlh.
    DEFINE BUFFER bf-rm-rcpth      FOR rm-rcpth.
    DEFINE BUFFER bf-fg-rcpth      FOR fg-rcpth.
    DEFINE BUFFER bf-item          FOR item.
    DEFINE BUFFER bf-POReceiptLink FOR POReceiptLink.

    RUN pGetBuffers (
        INPUT  ipcAPInvoiceLineRecKey,
        BUFFER bf-ap-invl,
        BUFFER bf-po-ord,
        BUFFER bf-po-ordl
        ).

    RUN pValidateBuffers (
        BUFFER bf-ap-invl,
        BUFFER bf-po-ord,
        BUFFER bf-po-ordl,
        OUTPUT lError,
        OUTPUT cMessage
        ).
    IF lError THEN
        RETURN.

    RUN pGetInvoicedQty (
        INPUT  bf-ap-invl.rec_key,
        OUTPUT dQuantityInvoiced,
        OUTPUT cQuantityInvoicedUOM
        ).
    
    dRunningQuantityToInvoice = ipdQuantityToInvoice.
    
    IF bf-ap-invl.item-type THEN
        cItemType = "RM".
    ELSE
        cItemType = "FG".
    
    IF ipcQuantityUOM NE "" AND cQuantityInvoicedUOM NE "" AND ipcQuantityUOM NE cQuantityInvoicedUOM AND dQuantityInvoiced NE 0 THEN DO:
        RUN Conv_QuantityFromUOMToUOM (
            INPUT  bf-po-ordl.company,
            INPUT  bf-po-ordl.i-no,
            INPUT  cItemType,
            INPUT  dQuantityInvoiced,
            INPUT  cQuantityInvoicedUOM, 
            INPUT  ipcQuantityUOM,
            INPUT  dItemBasisWeight, /* Item basis weight */
            INPUT  bf-po-ordl.s-len,
            INPUT  bf-po-ordl.s-wid,
            INPUT  bf-po-ordl.s-dep,
            INPUT  0,
            OUTPUT dQuantityInvoiced,
            OUTPUT lError,
            OUTPUT cMessage
            ).
    END.

    /* If quantity to invoice and quantity invoice are same return */
    IF ipdQuantityToInvoice EQ dQuantityInvoiced THEN
        RETURN.
        
    /* Quantity trying to update is less than quantity previously selected */
    IF ipdQuantityToInvoice - dQuantityInvoiced LT 0 THEN DO:
        RUN pUpdateReceiptsQty (
            INPUT  bf-ap-invl.rec_key,
            INPUT  "", /* Inventory stock rec key */
            INPUT  ipdQuantityToInvoice,
            INPUT  ipcQuantityUOM,
            OUTPUT oplError,
            OUTPUT opcMessage
            ).

        RETURN.
    END.

    /* Delete the existing links for invoice and re-link */
    RUN pDeleteAPInvoiceLineLinks (
        INPUT bf-ap-invl.rec_key
        ).

/*    ipdQuantityToInvoice = ipdQuantityToInvoice - dQuantityInvoiced*/
    IF bf-ap-invl.item-type THEN DO:
        
        FIND FIRST bf-item NO-LOCK
             WHERE bf-item.company EQ bf-po-ordl.company
               AND bf-item.i-no    EQ bf-po-ordl.i-no
             NO-ERROR.
        IF AVAILABLE bf-item THEN
            dItemBasisWeight = bf-item.basis-w.

        FOR EACH bf-rm-rcpth NO-LOCK
            WHERE bf-rm-rcpth.company   EQ bf-po-ordl.company
              AND bf-rm-rcpth.i-no      EQ bf-po-ordl.i-no
              AND bf-rm-rcpth.po-no     EQ TRIM(STRING(bf-po-ordl.po-no,">>>>>>>>>>"))
              AND bf-rm-rcpth.job-no    EQ bf-po-ordl.job-no
              AND bf-rm-rcpth.job-no2   EQ bf-po-ordl.job-no2
              AND bf-rm-rcpth.rita-code EQ "R"
            USE-INDEX item-po,
            EACH bf-rm-rdtlh NO-LOCK
            WHERE bf-rm-rdtlh.r-no        EQ bf-rm-rcpth.r-no
              AND bf-rm-rdtlh.rita-code   EQ bf-rm-rcpth.rita-code
              AND (bf-rm-rdtlh.s-num      EQ bf-po-ordl.s-num OR bf-po-ordl.s-num EQ 0)
              AND bf-rm-rdtlh.receiver-no EQ "" :            
            IF dRunningQuantityToInvoice LE 0 THEN
                LEAVE.

            RUN pGetInvoicedQty(
                INPUT  bf-ap-invl.rec_key,
                OUTPUT dQuantityInvoiced,
                OUTPUT cQuantityInvoicedUOM
                ).                

            IF ipcQuantityUOM NE "" AND cReceiptUOM NE "" AND ipcQuantityUOM NE cReceiptUOM AND dQuantityInvoiced NE 0 THEN
                RUN Conv_QuantityFromUOMToUOM (
                    INPUT  bf-po-ordl.company,
                    INPUT  bf-po-ordl.i-no,
                    INPUT  cItemType,
                    INPUT  dQuantityInvoiced,
                    INPUT  cReceiptUOM,
                    INPUT  ipcQuantityUOM,
                    INPUT  dItemBasisWeight, /* Item basis weight */
                    INPUT  bf-po-ordl.s-len,
                    INPUT  bf-po-ordl.s-wid,
                    INPUT  bf-po-ordl.s-dep,
                    INPUT  0,
                    OUTPUT dQuantityInvoiced,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).
                                
            IF dQuantityInvoiced GE ipdQuantityToInvoice THEN
                LEAVE.
            
            dRunningQuantityToInvoice = ipdQuantityToInvoice - dQuantityInvoiced.
                 
            RUN pGetReceiptHistoryQtyInvoiced (
                INPUT  bf-rm-rdtlh.rec_key,
                OUTPUT dReceiptQuantityInvoiced
                ).
            
            dQuantityAvailable = bf-rm-rdtlh.qty - dReceiptQuantityInvoiced.
            
            IF dQuantityAvailable EQ 0 THEN
                NEXT.

            RUN pUpdateReceiptsQty (
                INPUT  bf-ap-invl.rec_key,
                INPUT  bf-rm-rdtlh.rec_key,
                INPUT  dRunningQuantityToInvoice,
                INPUT  ipcQuantityUOM,
                OUTPUT lError,
                OUTPUT cMessage
                ).
       
        END.
    END.
    ELSE DO:
        cReceiptUOM = "EA".
        
        FOR EACH bf-fg-rcpth NO-LOCK
            WHERE bf-fg-rcpth.company   EQ bf-po-ordl.company
              AND bf-fg-rcpth.i-no      EQ bf-po-ordl.i-no
              AND bf-fg-rcpth.po-no     EQ TRIM(STRING(bf-po-ordl.po-no,">>>>>>>>>>"))
              AND bf-fg-rcpth.rita-code EQ "R"
            USE-INDEX item-po,        
            EACH bf-fg-rdtlh NO-LOCK
            WHERE bf-fg-rdtlh.r-no        EQ bf-fg-rcpth.r-no
              AND bf-fg-rdtlh.rita-code   EQ bf-fg-rcpth.rita-code
              AND bf-fg-rdtlh.receiver-no EQ "":
            IF dRunningQuantityToInvoice LE 0 THEN
                LEAVE.

            RUN pGetInvoicedQty(
                INPUT  bf-ap-invl.rec_key,
                OUTPUT dQuantityInvoiced,
                OUTPUT cQuantityInvoicedUOM
                ).                

            IF ipcQuantityUOM NE "" AND cReceiptUOM NE "" AND ipcQuantityUOM NE cReceiptUOM AND dQuantityInvoiced NE 0 THEN
                RUN Conv_QuantityFromUOMToUOM (
                    INPUT  bf-po-ordl.company,
                    INPUT  bf-po-ordl.i-no,
                    INPUT  cItemType,
                    INPUT  dQuantityInvoiced,
                    INPUT  cReceiptUOM,
                    INPUT  ipcQuantityUOM,
                    INPUT  dItemBasisWeight, /* Item basis weight */
                    INPUT  bf-po-ordl.s-len,
                    INPUT  bf-po-ordl.s-wid,
                    INPUT  bf-po-ordl.s-dep,
                    INPUT  0,
                    OUTPUT dQuantityInvoiced,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).

            IF dQuantityInvoiced GE ipdQuantityToInvoice THEN
                LEAVE.
            
            dRunningQuantityToInvoice = ipdQuantityToInvoice - dQuantityInvoiced.
                 
            RUN pGetReceiptHistoryQtyInvoiced (
                INPUT  bf-fg-rdtlh.rec_key,
                OUTPUT dReceiptQuantityInvoiced
                ).

            dQuantityAvailable = bf-fg-rdtlh.qty - dReceiptQuantityInvoiced.
            
            IF dQuantityAvailable EQ 0 THEN
                NEXT.

            RUN pUpdateReceiptsQty (
                INPUT  bf-ap-invl.rec_key,
                INPUT  bf-fg-rdtlh.rec_key,
                INPUT  dRunningQuantityToInvoice,
                INPUT  ipcQuantityUOM,
                OUTPUT lError,
                OUTPUT cMessage
                ).
        END.
    END.    
END PROCEDURE.

PROCEDURE pGetFGReceiptQuantityUOM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcInventoryStockRecKey AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcQuantityUOM          AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-fg-rdtlh FOR fg-rdtlh.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    
    FIND FIRST bf-fg-rdtlh NO-LOCK
        WHERE bf-fg-rdtlh.rec_key EQ ipcInventoryStockRecKey
        NO-ERROR.
    IF AVAILABLE bf-fg-rdtlh THEN
        FIND FIRST bf-fg-rcpth NO-LOCK
            WHERE bf-fg-rcpth.r-no      EQ bf-fg-rdtlh.r-no
            AND bf-fg-rcpth.rita-code EQ bf-fg-rdtlh.rita-code
            NO-ERROR.
    
    IF AVAILABLE bf-fg-rcpth THEN
        opcQuantityUOM = bf-fg-rcpth.pur-uom.
     
END PROCEDURE.

PROCEDURE APInvoice_GetReceiptsQtyAvailable:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAPInvoiceLineRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecordsFound        AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityAvailable   AS DECIMAL   NO-UNDO.

    RUN pGetReceiptsQtyAvailable (
        INPUT  ipcAPInvoiceLineRecKey,
        INPUT  ipcQuantityUOM,
        OUTPUT oplRecordsFound,
        OUTPUT opdQuantityAvailable
        ).
END PROCEDURE.

PROCEDURE pGetReceiptsQtyAvailable PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAPInvoiceLineRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecordsFound        AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityAvailable   AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE lError               AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQuantityInvoiced    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dItemBasisWeight     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQuantityAvailable   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cItemType            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReceiptUOM          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityInvoicedUOM AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ap-invl  FOR ap-invl.
    DEFINE BUFFER bf-po-ord   FOR po-ord.
    DEFINE BUFFER bf-po-ordl  FOR po-ordl.
    DEFINE BUFFER bf-fg-rdtlh FOR fg-rdtlh.
    DEFINE BUFFER bf-rm-rdtlh FOR rm-rdtlh.
    DEFINE BUFFER bf-rm-rcpth FOR rm-rcpth.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-item     FOR item.
    
    RUN pGetBuffers (
        INPUT  ipcAPInvoiceLineRecKey,
        BUFFER bf-ap-invl,
        BUFFER bf-po-ord,
        BUFFER bf-po-ordl
        ).

    RUN pValidateBuffers (
        BUFFER bf-ap-invl,
        BUFFER bf-po-ord,
        BUFFER bf-po-ordl,
        OUTPUT lError,
        OUTPUT cMessage
        ).
    IF lError THEN
        RETURN.

    IF bf-ap-invl.item-type THEN DO:
        cItemType = "RM".
        
        FIND FIRST bf-item NO-LOCK
             WHERE bf-item.company EQ bf-po-ordl.company
               AND bf-item.i-no    EQ bf-po-ordl.i-no
             NO-ERROR.
        IF AVAILABLE bf-item THEN
            dItemBasisWeight = bf-item.basis-w.

        FOR EACH bf-rm-rcpth NO-LOCK
            WHERE bf-rm-rcpth.company   EQ bf-po-ordl.company
              AND bf-rm-rcpth.i-no      EQ bf-po-ordl.i-no
              AND bf-rm-rcpth.po-no     EQ TRIM(STRING(bf-po-ordl.po-no,">>>>>>>>>>"))
              AND bf-rm-rcpth.job-no    EQ bf-po-ordl.job-no
              AND bf-rm-rcpth.job-no2   EQ bf-po-ordl.job-no2
              AND bf-rm-rcpth.rita-code EQ "R"
            USE-INDEX item-po,
            EACH bf-rm-rdtlh NO-LOCK
            WHERE bf-rm-rdtlh.r-no        EQ bf-rm-rcpth.r-no
              AND bf-rm-rdtlh.rita-code   EQ bf-rm-rcpth.rita-code
              AND (bf-rm-rdtlh.s-num      EQ bf-po-ordl.s-num OR bf-po-ordl.s-num EQ 0)
              AND bf-rm-rdtlh.receiver-no EQ "" :        
            oplRecordsFound = TRUE.
            
            cReceiptUOM = bf-rm-rcpth.pur-uom.
            
            RUN pGetReceiptHistoryQtyInvoiced (
                INPUT  bf-rm-rdtlh.rec_key,
                OUTPUT dQuantityInvoiced
                ).
                 
            dQuantityAvailable = bf-rm-rdtlh.qty - dQuantityInvoiced.
            
            IF ipcQuantityUOM NE "" AND cReceiptUOM NE "" AND ipcQuantityUOM NE cReceiptUOM AND dQuantityAvailable NE 0 THEN DO:
                RUN Conv_QuantityFromUOMToUOM (
                    INPUT  bf-po-ordl.company,
                    INPUT  bf-po-ordl.i-no,
                    INPUT  cItemType,
                    INPUT  dQuantityAvailable,
                    INPUT  cReceiptUOM, 
                    INPUT  ipcQuantityUOM,
                    INPUT  dItemBasisWeight, /* Item basis weight */
                    INPUT  bf-po-ordl.s-len,
                    INPUT  bf-po-ordl.s-wid,
                    INPUT  bf-po-ordl.s-dep,
                    INPUT  0,
                    OUTPUT dQuantityAvailable,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ). 
            END.
            
            opdQuantityAvailable = opdQuantityAvailable + dQuantityAvailable.        
        END.
    END.
    ELSE DO:
        ASSIGN
            cItemType   = "FG"
            cReceiptUOM = "EA"
            .
        
        FOR EACH bf-fg-rcpth NO-LOCK
            WHERE bf-fg-rcpth.company   EQ bf-po-ordl.company
              AND bf-fg-rcpth.i-no      EQ bf-po-ordl.i-no
              AND bf-fg-rcpth.po-no     EQ TRIM(STRING(bf-po-ordl.po-no,">>>>>>>>>>"))
              AND bf-fg-rcpth.rita-code EQ "R"
            USE-INDEX item-po,        
            EACH bf-fg-rdtlh NO-LOCK
            WHERE bf-fg-rdtlh.r-no        EQ bf-fg-rcpth.r-no
              AND bf-fg-rdtlh.rita-code   EQ bf-fg-rcpth.rita-code
              AND bf-fg-rdtlh.receiver-no EQ "":
            
            RUN pGetReceiptHistoryQtyInvoiced (
                INPUT  bf-fg-rdtlh.rec_key,
                OUTPUT dQuantityInvoiced
                ).
                
            oplRecordsFound = TRUE.
            
            dQuantityAvailable = bf-fg-rdtlh.qty - dQuantityInvoiced.
            
            IF ipcQuantityUOM NE "" AND cReceiptUOM NE "" AND ipcQuantityUOM NE cReceiptUOM AND dQuantityAvailable NE 0 THEN
                RUN Conv_QuantityFromUOMToUOM (
                    INPUT  bf-po-ordl.company,
                    INPUT  bf-po-ordl.i-no,
                    INPUT  cItemType,
                    INPUT  dQuantityAvailable,
                    INPUT  cReceiptUOM, 
                    INPUT  ipcQuantityUOM,
                    INPUT  0, /* Item basis weight */
                    INPUT  bf-po-ordl.s-len,
                    INPUT  bf-po-ordl.s-wid,
                    INPUT  bf-po-ordl.s-dep,
                    INPUT  0,
                    OUTPUT dQuantityAvailable,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ). 

            opdQuantityAvailable = opdQuantityAvailable + dQuantityAvailable. 
        END.
    END.
    
    RUN pGetInvoicedQty (
        INPUT  bf-ap-invl.rec_key,
        OUTPUT dQuantityInvoiced,
        OUTPUT cQuantityInvoicedUOM
        ).

    IF ipcQuantityUOM NE "" AND cQuantityInvoicedUOM NE "" AND ipcQuantityUOM NE cQuantityInvoicedUOM AND dQuantityInvoiced NE 0 THEN
        RUN Conv_QuantityFromUOMToUOM (
            INPUT  bf-po-ordl.company,
            INPUT  bf-po-ordl.i-no,
            INPUT  cItemType,
            INPUT  dQuantityInvoiced,
            INPUT  cQuantityInvoicedUOM, 
            INPUT  ipcQuantityUOM,
            INPUT  dItemBasisWeight, /* Item basis weight */
            INPUT  bf-po-ordl.s-len,
            INPUT  bf-po-ordl.s-wid,
            INPUT  bf-po-ordl.s-dep,
            INPUT  0,
            OUTPUT dQuantityInvoiced,
            OUTPUT lError,
            OUTPUT cMessage
            ).    
    
    opdQuantityAvailable = opdQuantityAvailable + dQuantityInvoiced.
END PROCEDURE.

PROCEDURE pGetRMReceiptQuantityUOM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcInventoryStockRecKey AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcQuantityUOM          AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-rm-rdtlh FOR rm-rdtlh.
    DEFINE BUFFER bf-rm-rcpth FOR rm-rcpth.
    
    FIND FIRST bf-rm-rdtlh NO-LOCK
        WHERE bf-rm-rdtlh.rec_key EQ ipcInventoryStockRecKey
        NO-ERROR.
    IF AVAILABLE bf-rm-rdtlh THEN
        FIND FIRST bf-rm-rcpth NO-LOCK
            WHERE bf-rm-rcpth.r-no      EQ bf-rm-rdtlh.r-no
              AND bf-rm-rcpth.rita-code EQ bf-rm-rdtlh.rita-code
            NO-ERROR.
    
    IF AVAILABLE bf-rm-rcpth THEN
        opcQuantityUOM = bf-rm-rcpth.pur-uom.

END PROCEDURE.

PROCEDURE pUpdatePOReceiptLink PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAPInvoiceLineRecKey  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcInventoryStockRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityInvoiced     AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM          AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-POReceiptLink FOR POReceiptLink.

    MAIN-BLOCK:
    DO TRANSACTION ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        FIND FIRST bf-POReceiptLink EXCLUSIVE-LOCK
            WHERE bf-POReceiptLink.apInvoiceLineRecKey  EQ ipcAPInvoiceLineRecKey
              AND bf-POReceiptLink.inventoryStockRecKey EQ ipcInventoryStockRecKey
            NO-ERROR.
        IF ipdQuantityInvoiced EQ 0 AND AVAILABLE bf-POReceiptLink THEN DO:
            DELETE bf-POReceiptLink.

            RETURN.    
        END.
        
        IF NOT AVAILABLE bf-POReceiptLink THEN 
        DO:
            CREATE bf-POReceiptLink.
            ASSIGN
                bf-POReceiptLink.apInvoiceLineRecKey  = ipcAPInvoiceLineRecKey
                bf-POReceiptLink.inventoryStockRecKey = ipcInventoryStockRecKey
                .
        END. 

        ASSIGN
            bf-POReceiptLink.quantityInvoiced    = ipdQuantityInvoiced
            bf-POReceiptLink.quantityInvoicedUOM = ipcQuantityUOM
            .            
    END.
END PROCEDURE.

PROCEDURE pGetBuffers PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAPInvoiceLineRecKey AS CHARACTER NO-UNDO.    
    DEFINE PARAMETER BUFFER opbf-ap-invl FOR ap-invl.
    DEFINE PARAMETER BUFFER opbf-po-ord  FOR po-ord.
    DEFINE PARAMETER BUFFER opbf-po-ordl FOR po-ordl.

    FIND FIRST opbf-ap-invl NO-LOCK
         WHERE opbf-ap-invl.rec_key EQ ipcAPInvoiceLineRecKey
         NO-ERROR.
    IF NOT AVAILABLE opbf-ap-invl THEN
        RETURN.
    
    FIND FIRST opbf-po-ord NO-LOCK
         WHERE opbf-po-ord.company EQ opbf-ap-invl.company
           AND opbf-po-ord.po-no   EQ opbf-ap-invl.po-no
         NO-ERROR.
    IF NOT AVAILABLE opbf-po-ord THEN
        RETURN.        
    
    FIND FIRST opbf-po-ordl NO-LOCK
         WHERE opbf-po-ordl.company EQ opbf-ap-invl.company
           AND opbf-po-ordl.po-no   EQ opbf-ap-invl.po-no
           AND opbf-po-ordl.line    EQ opbf-ap-invl.line + (opbf-ap-invl.po-no * 1000 * -1)
         NO-ERROR.        
END PROCEDURE.

PROCEDURE APInvoice_GetReceiptHistoryQtyInvoiced:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchInventoryStockRecKey AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opdQauntityInvoiced      AS DECIMAL   NO-UNDO.

    RUN pGetReceiptHistoryQtyInvoiced (
        INPUT  ipchInventoryStockRecKey,
        OUTPUT opdQauntityInvoiced
        ).
END PROCEDURE.

PROCEDURE pGetReceiptHistoryQtyInvoiced PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcInventoryStockRecKey AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opdQuantityInvoiced     AS DECIMAL   NO-UNDO.
    
    DEFINE BUFFER bf-POReceiptLink FOR POReceiptLink.

    FOR EACH bf-POReceiptLink NO-LOCK
        WHERE bf-POReceiptLink.inventoryStockRecKey EQ ipcInventoryStockRecKey:
        opdQuantityInvoiced = opdQuantityInvoiced + bf-POReceiptLink.quantityInvoiced.
    END.

END PROCEDURE.

PROCEDURE pGetInvoicedQty PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAPInvoiceLineRecKey AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityInvoiced    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcQuantityInvoicedUOM AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-POReceiptLink FOR POReceiptLink.
    
    FOR EACH bf-POReceiptLink
        WHERE bf-POReceiptLink.apInvoiceLineRecKey EQ ipcAPInvoiceLineRecKey:
        opdQuantityInvoiced = opdQuantityInvoiced + bf-POReceiptLink.quantityInvoiced.
        
        opcQuantityInvoicedUOM = bf-POReceiptLink.quantityInvoicedUOM.
    END.    

END PROCEDURE.

PROCEDURE APInvoice_GetQtyAvailable:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriAPInvl                    AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM                AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecordsFound               AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityAvailableToInvoice AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE lError           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dItemBasisWeight AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cItemType        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReceiptUOM      AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ap-invl       FOR ap-invl.
    DEFINE BUFFER bf-po-ordl       FOR po-ordl.
    DEFINE BUFFER bf-item          FOR item.
    DEFINE BUFFER bf-POReceiptLink FOR POReceiptLink.
    
    FIND FIRST bf-ap-invl NO-LOCK
        WHERE ROWID(bf-ap-invl) EQ ipriAPInvl
        NO-ERROR.
    IF NOT AVAILABLE bf-ap-invl THEN
        RETURN.
    
    FIND FIRST bf-po-ordl NO-LOCK
         WHERE bf-po-ordl.company EQ bf-ap-invl.company
           AND bf-po-ordl.po-no   EQ bf-ap-invl.po-no
           AND bf-po-ordl.line    EQ (bf-ap-invl.line + (bf-ap-invl.po-no * 1000 * -1))
         NO-ERROR.
    IF NOT AVAILABLE bf-po-ordl THEN
        RETURN.

    IF bf-ap-invl.item-type THEN
        cItemType = "RM".
    ELSE
        ASSIGN
            cReceiptUOM = "EA"
            cItemType   = "FG"
            .
  
    IF bf-ap-invl.item-type THEN 
    DO:
        RUN pGetRMQtyAvailable(
            INPUT  bf-ap-invl.rec_key,
            OUTPUT oplRecordsFound,
            OUTPUT opdQuantityAvailableToInvoice
            ).

        FIND FIRST bf-item NO-LOCK
            WHERE bf-item.company EQ bf-po-ordl.company
            AND bf-item.i-no    EQ bf-po-ordl.i-no
            NO-ERROR.
        IF AVAILABLE bf-item THEN
            dItemBasisWeight = bf-item.basis-w.
          
        FIND FIRST bf-POReceiptLink NO-LOCK
            WHERE bf-POReceiptLink.apInvoiceLineRecKey EQ bf-ap-invl.rec_key
            NO-ERROR.
        IF AVAILABLE bf-POReceiptLink THEN
            cReceiptUOM = bf-POReceiptLink.quantityInvoicedUOM.      
    END.  
    ELSE
        RUN pGetFGQtyAvailable(
            INPUT  bf-ap-invl.rec_key,
            OUTPUT oplRecordsFound,
            OUTPUT opdQuantityAvailableToInvoice
            ).

    IF ipcQuantityUOM NE cReceiptUOM THEN
        RUN Conv_QuantityFromUOMToUOM (
            INPUT  bf-po-ordl.company,
            INPUT  bf-po-ordl.i-no,
            INPUT  cItemType,
            INPUT  opdQuantityAvailableToInvoice,
            INPUT  cReceiptUOM, 
            INPUT  ipcQuantityUOM,
            INPUT  dItemBasisWeight, /* Item basis weight */
            INPUT  bf-po-ordl.s-len,
            INPUT  bf-po-ordl.s-wid,
            INPUT  bf-po-ordl.s-dep,
            INPUT  0,
            OUTPUT opdQuantityAvailableToInvoice,
            OUTPUT lError,
            OUTPUT cMessage
            ).            

END PROCEDURE.

PROCEDURE pGetFGQtyAvailable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcInvoiceLineRecKey          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecordsFound               AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityAvailableToInvoice AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE dFGQuantityInvoiced AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-POReceiptLink FOR POReceiptLink.
    DEFINE BUFFER bf-fg-rdtlh      FOR fg-rdtlh.
    
    FOR EACH bf-POReceiptLink
        WHERE bf-POReceiptLink.apInvoiceLineRecKey EQ ipcInvoiceLineRecKey:
        ASSIGN
            dFGQuantityInvoiced = 0.
            oplRecordsFound     = TRUE
            .
        
        FIND FIRST bf-fg-rdtlh NO-LOCK
            WHERE bf-fg-rdtlh.rec_key EQ bf-POReceiptLink.inventoryStockRecKey
            NO-ERROR.
        IF AVAILABLE bf-fg-rdtlh THEN DO:
            RUN pGetReceiptHistoryQtyInvoiced (
                INPUT  bf-fg-rdtlh.rec_key,
                OUTPUT dFGQuantityInvoiced
                ).
            opdQuantityAvailableToInvoice = opdQuantityAvailableToInvoice + bf-fg-rdtlh.qty - dFGQuantityInvoiced + bf-POReceiptLink.quantityInvoiced.
        END.
    END.        

END PROCEDURE.

PROCEDURE pGetRMQtyAvailable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcInvoiceLineRecKey          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecordsFound               AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityAvailableToInvoice AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE dRMQuantityInvoiced AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-POReceiptLink FOR POReceiptLink.
    DEFINE BUFFER bf-rm-rdtlh      FOR rm-rdtlh.

    FOR EACH bf-POReceiptLink
        WHERE bf-POReceiptLink.apInvoiceLineRecKey EQ ipcInvoiceLineRecKey:
        ASSIGN
            dRMQuantityInvoiced = 0.
            oplRecordsFound     = TRUE
            .

        FIND FIRST bf-rm-rdtlh NO-LOCK
            WHERE bf-rm-rdtlh.rec_key EQ bf-POReceiptLink.inventoryStockRecKey
            NO-ERROR.
             
        IF AVAILABLE bf-rm-rdtlh THEN DO:
            RUN pGetReceiptHistoryQtyInvoiced (
                INPUT  bf-rm-rdtlh.rec_key,
                OUTPUT dRMQuantityInvoiced
                ).

            opdQuantityAvailableToInvoice = opdQuantityAvailableToInvoice + bf-rm-rdtlh.qty - dRMQuantityInvoiced + bf-POReceiptLink.quantityInvoiced.
        END.
    END.        

END PROCEDURE.

PROCEDURE pUpdateRMInvoicedQty PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ap-invl  FOR ap-invl.
    DEFINE PARAMETER BUFFER ipbf-rm-rdtlh FOR rm-rdtlh.
    DEFINE INPUT  PARAMETER ipcQuantityUOM              AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQuantityToInvoice AS DECIMAL   NO-UNDO.
    
    DEFINE VARIABLE dQuantityInvoiced           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQuantityRemaining          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQuantityInvoicedForInvoice AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cReceiptUOM                 AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-POReceiptLink FOR POReceiptLink.
    
    IF NOT AVAILABLE ipbf-ap-invl OR NOT AVAILABLE ipbf-rm-rdtlh THEN
        RETURN.

    /* Fetch the quantity already invoiced */
    FOR EACH bf-POReceiptLink
        WHERE bf-POReceiptLink.inventoryStockRecKey EQ ipbf-rm-rdtlh.rec_key:
        IF bf-POReceiptLink.apInvoiceLineRecKey EQ ipbf-ap-invl.rec_key THEN
            dQuantityInvoicedForInvoice = dQuantityInvoicedForInvoice + bf-POReceiptLink.quantityInvoiced.
            
        dQuantityInvoiced = dQuantityInvoiced + bf-POReceiptLink.quantityInvoiced.
    END.

    dQuantityRemaining = ipbf-rm-rdtlh.qty - dQuantityInvoiced + dQuantityInvoicedForInvoice.

    IF dQuantityRemaining NE 0 THEN
        RUN pUpdatePOReceiptLink(
            INPUT ipbf-ap-invl.rec_key,
            INPUT ipbf-rm-rdtlh.rec_key,
            INPUT MINIMUM(dQuantityRemaining, iopdQuantityToInvoice),
            INPUT ipcQuantityUOM
            ).
    
    /* Empty the receiver no if whole invoice quantity is tagged and if quantity was tagged for this invoice */
    IF iopdQuantityToInvoice EQ 0 AND dQuantityInvoicedForInvoice EQ dQuantityInvoiced THEN
        RUN pDeleteReceiptHistoryLinks (
            INPUT ipbf-rm-rdtlh.rec_key
            ).
                
    iopdQuantityToInvoice = iopdQuantityToInvoice - MINIMUM(dQuantityRemaining, iopdQuantityToInvoice).   
         
END PROCEDURE.

PROCEDURE pUpdateFGInvoicedQty PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ap-invl  FOR ap-invl.
    DEFINE PARAMETER BUFFER ipbf-fg-rdtlh FOR fg-rdtlh.
    DEFINE INPUT  PARAMETER ipcQuantityUOM              AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQuantityToInvoice AS DECIMAL   NO-UNDO.
    
    DEFINE VARIABLE dQuantityInvoiced           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQuantityRemaining          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQuantityInvoicedForInvoice AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-POReceiptLink FOR POReceiptLink.
    
    IF NOT AVAILABLE ipbf-ap-invl OR NOT AVAILABLE ipbf-fg-rdtlh THEN
        RETURN.

    /* Fetch the quantity already invoiced */
    FOR EACH bf-POReceiptLink
        WHERE bf-POReceiptLink.inventoryStockRecKey EQ ipbf-fg-rdtlh.rec_key:
        IF bf-POReceiptLink.apInvoiceLineRecKey EQ ipbf-ap-invl.rec_key THEN
            dQuantityInvoicedForInvoice = dQuantityInvoicedForInvoice + bf-POReceiptLink.quantityInvoiced.
            
        dQuantityInvoiced = dQuantityInvoiced + bf-POReceiptLink.quantityInvoiced.
    END.

    dQuantityRemaining = ipbf-fg-rdtlh.qty - dQuantityInvoiced + dQuantityInvoicedForInvoice.

    IF dQuantityRemaining NE 0 THEN
        RUN pUpdatePOReceiptLink(
            INPUT ipbf-ap-invl.rec_key,
            INPUT ipbf-fg-rdtlh.rec_key,
            INPUT MINIMUM(dQuantityRemaining, iopdQuantityToInvoice),
            INPUT ipcQuantityUOM
            ).
    
    /* Empty the receiver no if whole invoice quantity is tagged and if quantity was tagged for this invoice */
    IF iopdQuantityToInvoice EQ 0 AND dQuantityInvoicedForInvoice EQ dQuantityInvoiced THEN
        RUN pDeleteReceiptHistoryLinks (
            INPUT ipbf-fg-rdtlh.rec_key
            ).

    iopdQuantityToInvoice = iopdQuantityToInvoice - MINIMUM(dQuantityRemaining, iopdQuantityToInvoice).
           
END PROCEDURE.

PROCEDURE pCalculateAndUpdateRMInvoicedQty PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ap-invl FOR ap-invl.
    DEFINE PARAMETER BUFFER ipbf-po-ordl FOR po-ordl.
    DEFINE INPUT  PARAMETER ipdQuantityToInvoice    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcInventoryStockRecKey AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQuantityToInvoice AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lReceiptFound      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cQuantityUOM       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dItemBasisWeight   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cReceiptUOM        AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-rm-rcpth      FOR rm-rcpth.
    DEFINE BUFFER bf-rm-rdtlh      FOR rm-rdtlh.
    DEFINE BUFFER bf-POReceiptLink FOR POReceiptLink.
    DEFINE BUFFER bf-item          FOR item.
    
    IF NOT AVAILABLE ipbf-ap-invl THEN
        RETURN.
            
    dQuantityToInvoice = ipdQuantityToInvoice.    
    
    IF dQuantityToInvoice EQ 0 THEN
        dQuantityToInvoice = ipbf-ap-invl.qty.
    
    IF ipcQuantityUOM EQ "" THEN
        ipcQuantityUOM = ipbf-ap-invl.cons-uom.
    
    /* This would be create logic */
    IF ipcInventoryStockRecKey NE "" THEN DO:
        FIND FIRST bf-rm-rdtlh NO-LOCK
             WHERE bf-rm-rdtlh.rec_key EQ ipcInventoryStockRecKey
             NO-ERROR.
        IF AVAILABLE bf-rm-rdtlh THEN DO:
            lReceiptFound = TRUE.
    
            RUN pGetRMReceiptQuantityUOM (
                INPUT  bf-rm-rdtlh.rec_key,
                OUTPUT cReceiptUOM
                ).
                            
            IF cReceiptUOM NE ipcQuantityUOM AND cReceiptUOM NE cQuantityUOM AND dQuantityToInvoice NE 0 THEN DO:
                FIND FIRST bf-item NO-LOCK
                     WHERE bf-item.company EQ ipbf-po-ordl.company
                       AND bf-item.i-no    EQ ipbf-po-ordl.i-no
                     NO-ERROR.
                IF AVAILABLE bf-item THEN
                    dItemBasisWeight = bf-item.basis-w.
                    
                RUN Conv_QuantityFromUOMToUOM (
                    INPUT  ipbf-po-ordl.company,
                    INPUT  ipbf-po-ordl.i-no,
                    INPUT  "RM",
                    INPUT  dQuantityToInvoice,
                    INPUT  ipcQuantityUOM, 
                    INPUT  cReceiptUOM,
                    INPUT  dItemBasisWeight,
                    INPUT  ipbf-po-ordl.s-len,
                    INPUT  ipbf-po-ordl.s-wid,
                    INPUT  ipbf-po-ordl.s-dep,
                    INPUT  0,
                    OUTPUT dQuantityToInvoice,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).            
                
                cQuantityUOM = cReceiptUOM.    
            END.
            
            RUN pUpdateRMInvoicedQty (
                BUFFER ipbf-ap-invl,
                BUFFER bf-rm-rdtlh,
                INPUT  cReceiptUOM,
                INPUT-OUTPUT dQuantityToInvoice
                ).
        END.
    END.
    /* Case where rm-rdtlh's receiver-no is tagged to invoice line (partial quantiy), 
       but overriden by another invoice line later */
    IF NOT lReceiptFound THEN DO:
        FOR EACH bf-POReceiptLink
            WHERE bf-POReceiptLink.apInvoiceLineRecKey EQ ipbf-ap-invl.rec_key:
            FIND FIRST bf-rm-rdtlh NO-LOCK
                 WHERE bf-rm-rdtlh.rec_key EQ bf-POReceiptLink.inventoryStockRecKey
                 NO-ERROR.
            IF NOT AVAILABLE bf-rm-rdtlh THEN
                NEXT.

            RUN pGetRMReceiptQuantityUOM (
                INPUT  bf-rm-rdtlh.rec_key,
                OUTPUT cReceiptUOM
                ).
            
            IF cReceiptUOM NE ipcQuantityUOM AND cReceiptUOM NE cQuantityUOM AND dQuantityToInvoice NE 0 THEN DO:
                FIND FIRST bf-item NO-LOCK
                    WHERE bf-item.company EQ ipbf-po-ordl.company
                    AND bf-item.i-no      EQ ipbf-po-ordl.i-no
                    NO-ERROR.
                IF AVAILABLE bf-item THEN
                    dItemBasisWeight = bf-item.basis-w.
                    
                RUN Conv_QuantityFromUOMToUOM (
                    INPUT  ipbf-po-ordl.company,
                    INPUT  ipbf-po-ordl.i-no,
                    INPUT  "RM",
                    INPUT  dQuantityToInvoice,
                    INPUT  ipcQuantityUOM, 
                    INPUT  cReceiptUOM,
                    INPUT  dItemBasisWeight,
                    INPUT  ipbf-po-ordl.s-len,
                    INPUT  ipbf-po-ordl.s-wid,
                    INPUT  ipbf-po-ordl.s-dep,
                    INPUT  0,
                    OUTPUT dQuantityToInvoice,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).            
                
                cQuantityUOM = cReceiptUOM.    
            END.
                    
            RUN pUpdateRMInvoicedQty (
                BUFFER ipbf-ap-invl,
                BUFFER bf-rm-rdtlh,
                INPUT  cReceiptUOM,
                INPUT-OUTPUT dQuantityToInvoice
                ).
        END.
    END.

END PROCEDURE.

PROCEDURE pCalculateAndUpdateFGInvoicedQty PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ap-invl FOR ap-invl.
    DEFINE PARAMETER BUFFER ipbf-po-ordl FOR po-ordl.
    DEFINE INPUT  PARAMETER ipdQuantityToInvoice    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcInventoryStockRecKey AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQuantityToInvoice AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lReceiptFound      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cQuantityUOM       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dItemBasisWeight   AS DECIMAL   NO-UNDO.
    
    DEFINE BUFFER bf-fg-rcpth      FOR fg-rcpth.
    DEFINE BUFFER bf-fg-rdtlh      FOR fg-rdtlh.
    DEFINE BUFFER bf-POReceiptLink FOR POReceiptLink.
    
    IF NOT AVAILABLE ipbf-ap-invl THEN
        RETURN.
            
    dQuantityToInvoice = ipdQuantityToInvoice.    
    
    IF dQuantityToInvoice EQ 0 THEN
        dQuantityToInvoice = ipbf-ap-invl.qty.
    
    IF ipcQuantityUOM EQ "" THEN
        ipcQuantityUOM = ipbf-ap-invl.cons-uom.
        
    /* This would be create logic */
    IF ipcInventoryStockRecKey NE "" THEN DO:
        FIND FIRST bf-fg-rdtlh NO-LOCK
             WHERE bf-fg-rdtlh.rec_key EQ ipcInventoryStockRecKey
             NO-ERROR.
        IF AVAILABLE bf-fg-rdtlh THEN DO:
            lReceiptFound = TRUE.
    
            IF ipcQuantityUOM NE "EA" AND cQuantityUOM NE "EA" AND dQuantityToInvoice NE 0 THEN DO:
                RUN Conv_QuantityFromUOMToUOM (
                    INPUT  ipbf-po-ordl.company,
                    INPUT  ipbf-po-ordl.i-no,
                    INPUT  "FG",
                    INPUT  dQuantityToInvoice,
                    INPUT  ipcQuantityUOM, 
                    INPUT  "EA",
                    INPUT  0, /* Item basis weight */
                    INPUT  ipbf-po-ordl.s-len,
                    INPUT  ipbf-po-ordl.s-wid,
                    INPUT  ipbf-po-ordl.s-dep,
                    INPUT  0,
                    OUTPUT dQuantityToInvoice,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).            
                
                cQuantityUOM = "EA".    
            END.
            
            RUN pUpdateFGInvoicedQty (
                BUFFER ipbf-ap-invl,
                BUFFER bf-fg-rdtlh,
                INPUT  "EA",
                INPUT-OUTPUT dQuantityToInvoice
                ).
        END.
    END.
    
    
    IF NOT lReceiptFound THEN 
    DO:
        FOR EACH bf-POReceiptLink
            WHERE bf-POReceiptLink.apInvoiceLineRecKey EQ ipbf-ap-invl.rec_key:
            FIND FIRST bf-fg-rdtlh NO-LOCK
                WHERE bf-fg-rdtlh.rec_key EQ bf-POReceiptLink.inventoryStockRecKey
                NO-ERROR.
            IF NOT AVAILABLE bf-fg-rdtlh THEN
                NEXT.

            IF ipcQuantityUOM NE "EA" AND cQuantityUOM NE "EA" AND dQuantityToInvoice NE 0 THEN DO:                    
                RUN Conv_QuantityFromUOMToUOM (
                    INPUT  ipbf-po-ordl.company,
                    INPUT  ipbf-po-ordl.i-no,
                    INPUT  "FG",
                    INPUT  dQuantityToInvoice,
                    INPUT  ipcQuantityUOM, 
                    INPUT  "EA",
                    INPUT  0,  /* Item Basis Weight */
                    INPUT  ipbf-po-ordl.s-len,
                    INPUT  ipbf-po-ordl.s-wid,
                    INPUT  ipbf-po-ordl.s-dep,
                    INPUT  0,
                    OUTPUT dQuantityToInvoice,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).            
                
                cQuantityUOM = "EA".    
            END.
                    
            RUN pUpdateFGInvoicedQty (
                BUFFER ipbf-ap-invl,
                BUFFER bf-fg-rdtlh,
                INPUT  "EA",
                INPUT-OUTPUT dQuantityToInvoice
                ).
        END.
    END.

END PROCEDURE.

PROCEDURE pDeleteReceiptHistoryLinks PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcInventoryStocRecKey AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-POReceiptLink FOR POReceiptLink.

    FOR EACH bf-POReceiptLink EXCLUSIVE-LOCK
        WHERE bf-POReceiptLink.inventoryStockRecKey EQ ipcInventoryStocRecKey:
        DELETE bf-POReceiptLink.
    END.
END PROCEDURE.

PROCEDURE APInvoice_UpdateReceiptsQty:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAPInvoiceLineRecKey  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcInventoryStockRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityToInvoice    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError                AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
    RUN pUpdateReceiptsQty (
        INPUT  ipcAPInvoiceLineRecKey,
        INPUT  ipcInventoryStockRecKey,
        INPUT  ipdQuantityToInvoice,
        INPUT  ipcQuantityUOM,
        OUTPUT oplError,
        OUTPUT opcMessage
        ).
END PROCEDURE.

PROCEDURE APInvoice_DeleteAPInvoiceLineLinks:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAPInvoiceLineRecKey AS CHARACTER NO-UNDO.
            
    RUN pDeleteAPInvoiceLineLinks (
        INPUT  ipcAPInvoiceLineRecKey
        ).
END PROCEDURE.

PROCEDURE pDeleteAPInvoiceLineLinks PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAPInvoiceLineRecKey AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-POReceiptLink FOR POReceiptLink.

    FOR EACH bf-POReceiptLink EXCLUSIVE-LOCK
        WHERE bf-POReceiptLink.apInvoiceLineRecKey EQ ipcAPInvoiceLineRecKey:
        DELETE bf-POReceiptLink.
    END.
END PROCEDURE.

PROCEDURE pUpdateReceiptsQty PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAPInvoiceLineRecKey  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcInventoryStockRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityToInvoice    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError                AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ap-invl FOR ap-invl.
    DEFINE BUFFER bf-po-ord  FOR po-ord.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    
    RUN pGetBuffers (
        INPUT  ipcAPInvoiceLineRecKey,
        BUFFER bf-ap-invl,
        BUFFER bf-po-ord,
        BUFFER bf-po-ordl
        ).

    RUN pValidateBuffers (
        BUFFER bf-ap-invl,
        BUFFER bf-po-ord,
        BUFFER bf-po-ordl,
        OUTPUT oplError,
        OUTPUT opcMessage
        ).
    IF oplError THEN
        RETURN.
                
    IF NOT bf-po-ordl.item-type THEN
        RUN pCalculateAndUpdateFGInvoicedQty (
            BUFFER bf-ap-invl,
            BUFFER bf-po-ordl,
            INPUT  ipdQuantityToInvoice,
            INPUT  ipcQuantityUOM,            
            INPUT  ipcInventoryStockRecKey
            ).
    ELSE
        RUN pCalculateAndUpdateRMInvoicedQty (
            BUFFER bf-ap-invl,
            BUFFER bf-po-ordl,
            INPUT  ipdQuantityToInvoice,
            INPUT  ipcQuantityUOM,            
            INPUT  ipcInventoryStockRecKey
            ).
    
END PROCEDURE.

PROCEDURE pValidateBuffers PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ap-invl FOR ap-invl.
    DEFINE PARAMETER BUFFER ipbf-po-ord  FOR po-ord.
    DEFINE PARAMETER BUFFER ipbf-po-ordl FOR po-ordl.
    DEFINE OUTPUT PARAMETER oplError   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    IF NOT AVAILABLE ipbf-ap-invl THEN 
    DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Invalid AP Invoice Line ROWID"
            .
        RETURN.    
    END.
    
    IF NOT AVAILABLE ipbf-po-ord THEN 
    DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Invalid PO # '" + STRING(ipbf-ap-invl.po-no) + "' for invoice '" + STRING(ipbf-ap-invl.inv-no)
            .
        RETURN.        
    END.    
    
    IF NOT AVAILABLE ipbf-po-ordl THEN 
    DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Invalid PO # '" + STRING(ipbf-ap-invl.po-no) + "' line for invoice '" + STRING(ipbf-ap-invl.inv-no)
            .
        RETURN.        
    END.        

END PROCEDURE.
