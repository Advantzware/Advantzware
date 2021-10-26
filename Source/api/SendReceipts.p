/*------------------------------------------------------------------------
    File        : api/SendReceipts.p
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
    DEFINE VARIABLE dItemQuantity    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cPOID            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPOLineNumber    AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE hdTTHandle       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lcLineData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcLineDataConcat AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE iCount           AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-line-APIOutboundDetail FOR APIOutboundDetail.
    DEFINE BUFFER bf-ttReceipt              FOR ttReceipt.
    DEFINE BUFFER bf-po-ordl                FOR po-ordl.
    
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
               AND bf-line-APIOutboundDetail.parentID      EQ "SendReceipts"
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
                WHERE ttReceipt.poID NE 0
                BREAK BY ttReceipt.poID:
                IF NOT FIRST-OF (ttReceipt.poID) THEN
                    NEXT.
                    
                FOR EACH bf-ttReceipt
                    WHERE bf-ttReceipt.company EQ ttReceipt.company
                      AND bf-ttReceipt.poID    EQ ttReceipt.poID
                      BREAK BY bf-ttReceipt.poLine:
                    IF NOT FIRST-OF (bf-ttReceipt.poLine) THEN
                        NEXT.

                    FIND FIRST bf-po-ordl NO-LOCK
                         WHERE bf-po-ordl.company EQ bf-ttReceipt.company
                           AND bf-po-ordl.po-no   EQ bf-ttReceipt.poID
                           AND bf-po-ordl.line    EQ bf-ttReceipt.poLine
                         NO-ERROR.
                    IF NOT AVAILABLE bf-po-ordl THEN
                        NEXT.
                                            
                    iCount = iCount + 1.
                    RUN pGetPOQuantity(
                        BUFFER bf-po-ordl,
                        INPUT  bf-ttReceipt.quantityUOM,
                        OUTPUT dItemQuantity
                        ).
                    
                    lcLineData = bf-line-APIOutboundDetail.data.
                    
                    RUN updateRequestData(INPUT-OUTPUT lcLineData, "LineNumber", STRING(iCount)).
                    RUN updateRequestData(INPUT-OUTPUT lcLineData, "ItemID", bf-ttReceipt.itemID).
                    RUN updateRequestData(INPUT-OUTPUT lcLineData, "ItemDescription", bf-ttReceipt.itemName).
                    RUN updateRequestData(INPUT-OUTPUT lcLineData, "ItemUOM", bf-ttReceipt.quantityUOM).
                    RUN updateRequestData(INPUT-OUTPUT lcLineData, "ItemQuantity", STRING(dItemQuantity)).
                    RUN updateRequestData(INPUT-OUTPUT lcLineData, "POID", STRING(bf-ttReceipt.poID)).
                    RUN updateRequestData(INPUT-OUTPUT lcLineData, "POLineNumber", STRING(bf-ttReceipt.poLine)).
                    
                    lcLineDataConcat = lcLineDataConcat + lcLineData.
                END.
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


/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetPOQuantity:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-po-ordl FOR po-ordl.
    DEFINE INPUT  PARAMETER ipcUOM      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantity AS DECIMAL   NO-UNDO.
        
    DEFINE VARIABLE lError    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQuantity AS DECIMAL   NO-UNDO.
        
    DEFINE BUFFER bf-ttReceipt FOR ttReceipt.
    
    FOR EACH bf-ttReceipt
        WHERE bf-ttReceipt.company EQ ipbf-po-ordl.company
          AND bf-ttReceipt.poID    EQ ipbf-po-ordl.po-no
          AND bf-ttReceipt.poLine  EQ ipbf-po-ordl.line:
          dQuantity = bf-ttReceipt.quantity.
          IF ipbf-po-ordl.item-type AND ipcUOM NE bf-ttReceipt.quantityUOM THEN DO:
              RUN Conv_QuantityFromUOMToUOM (
                  INPUT  ipbf-po-ordl.company,
                  INPUT  ipbf-po-ordl.i-no,
                  INPUT  "RM",
                  INPUT  dQuantity,
                  INPUT  bf-ttReceipt.quantityUOM, 
                  INPUT  ipcUOM,
                  INPUT  0,
                  INPUT  ipbf-po-ordl.s-len,
                  INPUT  ipbf-po-ordl.s-wid,
                  INPUT  ipbf-po-ordl.s-dep,
                  INPUT  0,
                  OUTPUT dQuantity,
                  OUTPUT lError,
                  OUTPUT cMessage
                  ).  
          END. 
          
          opdQuantity = opdQuantity + dQuantity.        
    END.
    
END PROCEDURE.
