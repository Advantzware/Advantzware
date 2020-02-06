/*------------------------------------------------------------------------
    File        : api/SendAdvancedShipNotice.p
    Purpose     : Returns the request data with BOL information

    Syntax      :

    Description : Returns the request data with BOL information

    Author(s)   : Mithun Porandla
    Created     : Tue Jan 28 07:33:22 EDT 2019
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
    
    /* Variables to store the header data */
    DEFINE VARIABLE lcHeaderData       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcHeaderToData     AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcHeaderFromData   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcHeaderSenderData AS LONGCHAR NO-UNDO.
    
    /* Variables to store request tag data */
    DEFINE VARIABLE lcRequestData                 AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipNoticeRequestData       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipNoticeHeaderData        AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipNoticeHeaderContactData AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipNoticeHeaderCommentData AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipControlData             AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipNoticePortionData       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipNoticeItemData          AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcShipNoticeItemConcatData    AS LONGCHAR NO-UNDO.
    
    DEFINE VARIABLE lAvailable AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iOrderID   AS INTEGER NO-UNDO.
    DEFINE VARIABLE lValidBOL  AS LOGICAL NO-UNDO INITIAL TRUE.
    
    IF ipcRequestHandler NE "" THEN
        RUN VALUE(ipcRequestHandler) (
            INPUT TABLE  ttArgs,
            INPUT ipiAPIOutboundID,
            INPUT ipiAPIOutboundTriggerID,
            INPUT-OUTPUT ioplcRequestData,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
    ELSE DO:
        FIND FIRST ttArgs
             WHERE ttArgs.argType  = "ROWID"
               AND ttArgs.argKey   = "oe-bolh" NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "No valid oe-bolh record passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST oe-bolh NO-LOCK
             WHERE ROWID(oe-bolh) = TO-ROWID(ttArgs.argValue) NO-ERROR.
        IF NOT AVAILABLE oe-bolh THEN DO:
            ASSIGN
                opcMessage = "Invalid oe-bolh ROWID passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FOR EACH oe-boll NO-LOCK
            WHERE oe-boll.company EQ oe-bolh.company
              AND oe-boll.b-no    EQ oe-bolh.b-no:
            IF oe-boll.ord-no EQ 0 THEN DO:
                lValidBOL = FALSE.
                LEAVE.
            END.
            
            IF iOrderID NE 0 AND iOrderID NE oe-boll.ord-no THEN DO:
                lValidBOL = FALSE.
                LEAVE.
            END.
            
            iOrderID = oe-boll.ord-no.
        END.

        IF NOT lValidBOL THEN DO:
            ASSIGN
                opcMessage = IF iOrderID EQ 0 THEN
                                 "Invalid order exists in BOL" 
                             ELSE
                                 "Multiple orders exists in BOL"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST oe-ord NO-LOCK
             WHERE oe-ord.company EQ oe-bolh.company
               AND oe-ord.ord-no  EQ iOrderID
             NO-ERROR.
        IF NOT AVAILABLE oe-ord THEN DO:
            ASSIGN
                opcMessage = "Invalid order number " + STRING(iOrderID) + " in BOL"
                oplSuccess = FALSE
                .
            RETURN.                   
        END.
        
        IF oe-ord.spare-char-3 EQ "" THEN DO:
            ASSIGN
                opcMessage = "Empty payloadID"
                oplSuccess = FALSE
                .
            RETURN.            
        END.

        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PayloadID", oe-ord.spare-char-3).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "NoticeDate", STRING(YEAR(TODAY),"9999") + "-" + STRING(MONTH(TODAY),"99") + "-" + STRING(DAY(TODAY),"99")).
        
        /* Header data update */        
        RUN pGetOutboundDetailData (
            INPUT  ipiAPIOutboundID,
            INPUT  "HeaderID",
            INPUT  "SendAdvancedShipNotice",
            OUTPUT lAvailable,
            OUTPUT lcHeaderData
            ).
        IF lAvailable THEN DO:            
            /* Header From data */
            RUN pGetOutboundDetailData (
                INPUT  ipiAPIOutboundID,
                INPUT  "HeaderFromID",
                INPUT  "HeaderID",
                OUTPUT lAvailable,
                OUTPUT lcHeaderFromData
                ).            
            
            /* Header To data */
            RUN pGetOutboundDetailData (
                INPUT  ipiAPIOutboundID,
                INPUT  "HeaderToID",
                INPUT  "HeaderID",
                OUTPUT lAvailable,
                OUTPUT lcHeaderToData
                ).
                
            /* Header Sender data */
            RUN pGetOutboundDetailData (
                INPUT  ipiAPIOutboundID,
                INPUT  "HeaderSenderID",
                INPUT  "HeaderID",
                OUTPUT lAvailable,
                OUTPUT lcHeaderSenderData
                ).            
        END.

        /* Request tag data update */
        RUN pGetOutboundDetailData (
            INPUT  ipiAPIOutboundID,
            INPUT  "RequestID",
            INPUT  "SendAdvancedShipNotice",
            OUTPUT lAvailable,
            OUTPUT lcRequestData
            ).
        IF lAvailable THEN DO:
            RUN updateRequestData(INPUT-OUTPUT lcRequestData, "RequestDeploymentMode", "production").
            /* ShipNoticeRequest tag data */
            RUN pGetOutboundDetailData (
                INPUT  ipiAPIOutboundID,
                INPUT  "ShipNoticeRequestID",
                INPUT  "RequestID",
                OUTPUT lAvailable,
                OUTPUT lcShipNoticeRequestData
                ).              
            IF lAvailable THEN DO:
                /* ShipNoticeHeader tag data */
                RUN pGetOutboundDetailData (
                    INPUT  ipiAPIOutboundID,
                    INPUT  "ShipNoticeHeaderID",
                    INPUT  "ShipNoticeRequestID",
                    OUTPUT lAvailable,
                    OUTPUT lcShipNoticeHeaderData
                    ).
                IF lAvailable THEN DO:
                    RUN pGetOutboundDetailData (
                        INPUT  ipiAPIOutboundID,
                        INPUT  "ShipNoticeHeaderContactID",
                        INPUT  "ShipNoticeHeaderID",
                        OUTPUT lAvailable,
                        OUTPUT lcShipNoticeHeaderContactData
                        ).     

                    RUN pGetOutboundDetailData (
                        INPUT  ipiAPIOutboundID,
                        INPUT  "ShipNoticeHeaderCommentID",
                        INPUT  "ShipNoticeHeaderID",
                        OUTPUT lAvailable,
                        OUTPUT lcShipNoticeHeaderCommentData
                        ). 
                END.
                
                ASSIGN
                    lcShipNoticeHeaderData = REPLACE(lcShipNoticeHeaderData,"$ShipNoticeHeaderContactID$",lcShipNoticeHeaderContactData)
                    lcShipNoticeHeaderData = REPLACE(lcShipNoticeHeaderData,"$ShipNoticeHeaderCommentID$",lcShipNoticeHeaderCommentData)
                    .

                /* ShipNoticeRequest tag data */
                RUN pGetOutboundDetailData (
                    INPUT  ipiAPIOutboundID,
                    INPUT  "ShipControlID",
                    INPUT  "ShipNoticeRequestID",
                    OUTPUT lAvailable,
                    OUTPUT lcShipControlData
                    ).                

                RUN pGetOutboundDetailData (
                    INPUT  ipiAPIOutboundID,
                    INPUT  "ShipNoticePortionID",
                    INPUT  "ShipNoticeRequestID",
                    OUTPUT lAvailable,
                    OUTPUT lcShipNoticePortionData
                    ).                    
                IF lAvailable THEN DO:
                    FOR EACH oe-boll EXCLUSIVE-LOCK
                        WHERE oe-boll.company EQ oe-bolh.company
                          AND oe-boll.b-no    EQ oe-bolh.b-no:
                        RUN pGetOutboundDetailData (
                            INPUT  ipiAPIOutboundID,
                            INPUT  "ShipNoticeItemID",
                            INPUT  "ShipNoticePortionID",
                            OUTPUT lAvailable,
                            OUTPUT lcShipNoticeItemData
                            ).                    
                        
                        lcShipNoticeItemConcatData = lcShipNoticeItemConcatData + lcShipNoticeItemData.
                    END.                    
                END.
                
                lcShipNoticePortionData = REPLACE(lcShipNoticePortionData,"$ShipNoticeItemID$",lcShipNoticeItemConcatData).                
            END.
            
            ASSIGN
                lcShipNoticeRequestData = REPLACE(lcShipNoticeRequestData,"$ShipNoticeHeaderID$",lcShipNoticeHeaderData)
                lcShipNoticeRequestData = REPLACE(lcShipNoticeRequestData,"$ShipControlID$",lcShipControlData)
                lcShipNoticeRequestData = REPLACE(lcShipNoticeRequestData,"$ShipNoticePortionID$",lcShipNoticePortionData)
                .    
        END.

        ASSIGN
            lcHeaderData  = REPLACE(lcHeaderData,"$HeaderToID$",lcHeaderToData)
            lcHeaderData  = REPLACE(lcHeaderData,"$HeaderFromID$",lcHeaderFromData)
            lcHeaderData  = REPLACE(lcHeaderData,"$HeaderSenderID$",lcHeaderSenderData)
            lcRequestData = REPLACE(lcRequestData,"$ShipNoticeRequestID$",lcShipNoticeRequestData)
            .

        ASSIGN
            ioplcRequestData = REPLACE(ioplcRequestData,"$HeaderID$",lcHeaderData)
            ioplcRequestData = REPLACE(ioplcRequestData,"$RequestID$",lcRequestData)
            .
        
        ASSIGN   
            opcMessage       = ""
            oplSuccess       = TRUE
            .
    END.        
        


/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetOutboundDetailData PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Returns the request data struction for given inputs
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiAPIOutboundID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDetailID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcParentID      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplAvailable     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcData         AS LONGCHAR  NO-UNDO.
    
    DEFINE BUFFER bf-APIOutboundDetail FOR APIOutboundDetail.
  
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID
           AND bf-APIOutboundDetail.parentID      EQ ipcParentID
         NO-ERROR.        
    IF AVAILABLE bf-APIOutboundDetail THEN
        ASSIGN
            oplAvailable = TRUE
            oplcData     = bf-APIOutboundDetail.data
            .

    RELEASE bf-APIoutboundDetail.
END PROCEDURE.
