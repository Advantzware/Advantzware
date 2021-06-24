/*------------------------------------------------------------------------
    File        : api/SendReceipt.p
    Purpose     : Returns the request data for receipt

    Syntax      :

    Description : Returns the request data for receipt

    Author(s)   : DEVA$!
    Created     : Fri Jun 04 07:04:19 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/
    {api/ttArgs.i}
    {api/CommonAPIProcs.i}
    
    DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cReceiverNumber  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReceiptDate     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReceiptTime     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemUOM         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemQuantity    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPOID            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPOLineNumber    AS CHARACTER NO-UNDO. 

    DEFINE BUFFER bf-rm-rcpth FOR rm-rcpth.
    DEFINE BUFFER bf-rm-rdtlh FOR rm-rdtlh.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-fg-rdtlh FOR fg-rdtlh.
             
    IF ipcRequestHandler NE "" THEN 
        RUN VALUE(ipcRequestHandler) (
            INPUT  TABLE ttArgs,
            INPUT  ipiAPIOutboundID,
            INPUT  ipiAPIOutboundTriggerID,
            INPUT-OUTPUT ioplcRequestData,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
    ELSE DO:
        FIND FIRST ttArgs
             WHERE ttArgs.argType  = "ROWID"
               AND ttArgs.argKey   = "rm-rcpth" NO-ERROR.
        IF AVAILABLE ttArgs THEN
            FIND FIRST bf-rm-rcpth NO-LOCK
                 WHERE ROWID(bf-rm-rcpth) EQ TO-ROWID(ttArgs.argValue)
                 NO-ERROR.

        FIND FIRST ttArgs
             WHERE ttArgs.argType  = "ROWID"
               AND ttArgs.argKey   = "fg-rcpth" NO-ERROR.
        IF AVAILABLE ttArgs THEN
            FIND FIRST bf-fg-rcpth NO-LOCK
                 WHERE ROWID(bf-fg-rcpth) EQ TO-ROWID(ttArgs.argValue)
                 NO-ERROR.

        IF NOT AVAILABLE bf-fg-rcpth AND NOT AVAILABLE bf-rm-rcpth THEN DO:
            ASSIGN
                opcMessage = "Invalid receipt ROWID passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.

        FIND FIRST ttArgs
             WHERE ttArgs.argType  = "ROWID"
               AND ttArgs.argKey   = "Quantity" NO-ERROR.
        IF AVAILABLE ttArgs THEN
            cItemQuantity = ttArgs.argValue.
            
        FIND FIRST ttArgs
             WHERE ttArgs.argType  = "ROWID"
               AND ttArgs.argKey   = "QuantityUOM" NO-ERROR.
        IF AVAILABLE ttArgs THEN
            cItemUOM = ttArgs.argValue.
        
        IF AVAILABLE bf-rm-rcpth THEN DO:
            FIND FIRST bf-rm-rdtlh NO-LOCK
                 WHERE bf-rm-rdtlh.company   EQ bf-rm-rcpth.company
                   AND bf-rm-rdtlh.r-no      EQ bf-rm-rcpth.r-no
                   AND bf-rm-rcpth.rita-code EQ bf-rm-rcpth.rita-code
                 NO-ERROR.
            IF NOT AVAILABLE bf-rm-rdtlh THEN DO:
                ASSIGN
                    opcMessage = "No Raw Material detail record found"
                    oplSuccess = FALSE
                    .
                RETURN.            
            END.
        END.

        IF AVAILABLE bf-fg-rcpth THEN DO:
            FIND FIRST bf-fg-rdtlh NO-LOCK
                 WHERE bf-fg-rdtlh.company   EQ bf-fg-rcpth.company
                   AND bf-fg-rdtlh.r-no      EQ bf-fg-rcpth.r-no
                   AND bf-fg-rcpth.rita-code EQ bf-fg-rcpth.rita-code
                 NO-ERROR.                 
            IF NOT AVAILABLE bf-fg-rdtlh THEN DO:
                ASSIGN
                    opcMessage = "No Finished good detail record found"
                    oplSuccess = FALSE
                    .
                RETURN.            
            END.
        END.        
        
        IF AVAILABLE bf-rm-rcpth THEN DO:
            ASSIGN
                cReceiverNumber  = STRING(bf-rm-rdtlh.r-no)
                cReceiptDate     = STRING(bf-rm-rdtlh.upd-date)
                cReceiptTime     = STRING(bf-rm-rdtlh.upd-time)
                cItemID          = bf-rm-rdtlh.i-no
                cItemDescription = bf-rm-rcpth.i-name
                cItemUOM         = IF cItemUOM EQ "" THEN 
                                       bf-rm-rcpth.pur-uom
                                   ELSE
                                       cItemUOM
                cItemQuantity    = IF DECIMAL(cItemQuantity) EQ 0 THEN
                                       STRING(bf-rm-rdtlh.qty)
                                   ELSE
                                       cItemQuantity
                cPOID            = STRING(bf-rm-rcpth.po-no)
                cPOLineNumber    = STRING(bf-rm-rcpth.po-line)
                . 
        END.

        IF AVAILABLE bf-fg-rcpth THEN DO:
            ASSIGN
                cReceiverNumber  = STRING(bf-fg-rdtlh.r-no)
                cReceiptDate     = STRING(bf-fg-rdtlh.upd-date)
                cReceiptTime     = STRING(bf-fg-rdtlh.upd-time)
                cItemID          = bf-fg-rdtlh.i-no
                cItemDescription = bf-fg-rcpth.i-name
                cItemUOM         = IF cItemUOM EQ "" THEN 
                                       bf-fg-rcpth.pur-uom
                                   ELSE
                                       cItemUOM
                cItemQuantity    = IF DECIMAL(cItemQuantity) EQ 0 THEN
                                       STRING(bf-fg-rdtlh.qty)
                                   ELSE
                                       cItemQuantity

                cPOID            = STRING(bf-fg-rcpth.po-no)
                cPOLineNumber    = STRING(bf-fg-rcpth.po-line)
                . 
        END.
        
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ReceiverNumber", cReceiverNumber).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ReceiptDate", cReceiptDate).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ReceiptTime", cReceiptTime).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ItemID", cItemID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ItemDescription", cItemDescription).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ItemUOM", cItemUOM).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ItemQuantity", cItemQuantity).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "POID", cPOID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "POLineNumber", cPOLineNumber).
            
        ASSIGN
            opcMessage = ""
            oplSuccess = TRUE
            .
    END.
