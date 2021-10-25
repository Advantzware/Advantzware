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

    DEFINE VARIABLE cReceiverNumber   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReceiptDate      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReceiptTime      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemID           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDescription  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityUOM      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityReceived AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPOID             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPOLineNumber     AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cJobID            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobID2           AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cLocation         AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iQuantityReceivedJobTotal AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-rm-rcpth FOR rm-rcpth.
    DEFINE BUFFER bf-rm-rdtlh FOR rm-rdtlh.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-fg-rdtlh FOR fg-rdtlh.
    
    RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).
                 
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
                cReceiverNumber   = STRING(bf-rm-rdtlh.r-no)
                cLocation         = bf-rm-rcpth.loc
                cReceiptDate      = STRING(bf-rm-rdtlh.upd-date)
                cReceiptTime      = STRING(bf-rm-rdtlh.upd-time)
                cItemID           = bf-rm-rdtlh.i-no
                cItemDescription  = bf-rm-rcpth.i-name
                cQuantityUOM      = bf-rm-rcpth.pur-uom
                cQuantityReceived = STRING(bf-rm-rdtlh.qty)
                cPOID             = STRING(bf-rm-rcpth.po-no)
                cPOLineNumber     = STRING(bf-rm-rcpth.po-line)
                cJobID            = bf-rm-rcpth.job-no
                cJobID2           = STRING (bf-rm-rcpth.job-no2)
                . 
        END.

        IF AVAILABLE bf-fg-rcpth THEN DO:
            ASSIGN
                cReceiverNumber   = STRING(bf-fg-rdtlh.r-no)
                cLocation         = bf-fg-rcpth.loc
                cReceiptDate      = STRING(bf-fg-rdtlh.upd-date)
                cReceiptTime      = STRING(bf-fg-rdtlh.upd-time)
                cItemID           = bf-fg-rdtlh.i-no
                cItemDescription  = bf-fg-rcpth.i-name
                cQuantityUOM      = bf-fg-rcpth.pur-uom
                cQuantityReceived = STRING(bf-fg-rdtlh.qty)
                cPOID             = STRING(bf-fg-rcpth.po-no)
                cPOLineNumber     = STRING(bf-fg-rcpth.po-line)
                cJobID            = bf-fg-rcpth.job-no
                cJobID2           = STRING (bf-fg-rcpth.job-no2)
                .

            RUN fg/GetProductionQty.p (
                INPUT  bf-fg-rdtlh.company,
                INPUT  bf-fg-rcpth.job-no,
                INPUT  bf-fg-rcpth.job-no2,
                INPUT  bf-fg-rdtlh.i-no,
                INPUT  NO,
                OUTPUT iQuantityReceivedJobTotal
                ).                 
        END.
        
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ReceiverNumber", cReceiverNumber).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Location", cLocation).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ReceiptDate", cReceiptDate).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ReceiptTime", cReceiptTime).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Item", cItemID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ItemDescription", cItemDescription).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "QuantityUOM", cQuantityUOM).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "QuantityReceived", cQuantityReceived).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "POID", cPOID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "POLineNumber", cPOLineNumber).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "JobID", cJobID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "JobID2", cJobID2).        
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "QuantityReceivedJobTotal", STRING(iQuantityReceivedJobTotal)).
            
        ASSIGN
            opcMessage = ""
            oplSuccess = TRUE
            .
    END.