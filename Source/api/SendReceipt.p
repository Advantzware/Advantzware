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
    {api/ttReceipt.i}
    
    DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE mptrTTReceipt    AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE cReceiverNumber  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReceiptDate     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReceiptTime     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemUOM         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemQuantity    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPOID            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPOLineNumber    AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE hdTTHandle       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lcLineData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcLineDataConcat AS LONGCHAR  NO-UNDO.
    
    DEFINE BUFFER bf-line-APIOutboundDetail FOR APIOutboundDetail.
    
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
               AND ttArgs.argKey   = "TTReceiptHandle"
             NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid temp-table handle"
                .
            
            RETURN.
        END.
        
        hdTTHandle = HANDLE(ttArgs.argValue).
        
        IF NOT VALID-HANDLE (hdTTHandle) THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid temp-table handle"
                .
            
            RETURN.        
        END.

        FIND FIRST bf-line-APIOutboundDetail NO-LOCK
             WHERE bf-line-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
               AND bf-line-APIOutboundDetail.detailID      EQ "LineDetail"
               AND bf-line-APIOutboundDetail.parentID      EQ "SendReceipt"
             NO-ERROR.
             
        /* Code to send data from dynamic temp-table handle to static temp-table */
        hdTTHandle:WRITE-XML("MEMPTR", mptrTTReceipt).
        
        TEMP-TABLE ttReceipt:READ-XML("MEMPTR", mptrTTReceipt, "EMPTY", ?, FALSE).
        
        SET-SIZE(mptrTTReceipt) = 0.

        ASSIGN
            cReceiverNumber  = STRING("Test")
            .
        
        IF AVAILABLE bf-line-APIOutboundDetail THEN DO:            
            FOR EACH ttReceipt
                BY ttReceipt.lineID:
                lcLineData = bf-line-APIOutboundDetail.data.
                
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "LineNumber", STRING(ttReceipt.lineID)).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "ItemID", ttReceipt.itemID).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "ItemDescription", ttReceipt.itemName).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "ItemUOM", ttReceipt.quantityUOM).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "ItemQuantity", STRING(ttReceipt.quantity)).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "POID", STRING(ttReceipt.poID)).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "POLineNumber", STRING(ttReceipt.poLine)).
                
                lcLineDataConcat = lcLineDataConcat + lcLineData.
            END.            
        END.
        
        RUN pUpdateDelimiter (INPUT-OUTPUT lcLineDataConcat, gcRequestDataType).
        
        ioplcRequestData = REPLACE(ioplcRequestData, "$LineDetail$", lcLineDataConcat).
        
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ReceiverNumber", cReceiverNumber).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ReceiptDate", cReceiptDate).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ReceiptTime", cReceiptTime).
                    
        ASSIGN
            opcMessage = ""
            oplSuccess = TRUE
            .
    END.
