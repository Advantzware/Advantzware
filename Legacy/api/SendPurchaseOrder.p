/*------------------------------------------------------------------------
    File        : api/SendPurchaseOrder.p
    Purpose     : Returns the request data for purchase order addition

    Syntax      :

    Description : Returns the request data for purchase order addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
    {api/ttArgs.i}
    
    DEFINE INPUT        PARAMETER TABLE             FOR ttArgs.
    DEFINE INPUT        PARAMETER ipcParentID       AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestHandler AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData  AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess        AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage        AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lcDetailData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatDetailData AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cCardName          AS CHARACTER NO-UNDO.
    
    IF ipcRequestHandler NE "" THEN
        RUN VALUE(ipcRequestHandler) (
            INPUT TABLE ttArgs,
            INPUT ipcParentID,
            INPUT-OUTPUT ioplcRequestData,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
    ELSE DO:
        FIND FIRST APIOutboundDetail NO-LOCK
             WHERE APIOutboundDetail.detailID EQ "detail"
               AND APIOutboundDetail.parentID EQ ipcParentID
             NO-ERROR.
        
        IF NOT AVAILABLE APIOutboundDetail THEN DO:
            ASSIGN
                opcMessage = "No APIOutboundDetail record found for [ detail ]"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST ttArgs
             WHERE ttArgs.argType  EQ "ROWID"
               AND ttArgs.argKey   EQ "po-ord"
             NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "No valid po-ord record passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST po-ord NO-LOCK
             WHERE ROWID(po-ord) EQ TO-ROWID(ttArgs.argValue)
             NO-ERROR.
        IF NOT AVAILABLE po-ord THEN DO:
            ASSIGN
                opcMessage = "Invalid po-ord ROWID passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
    
        IF NOT CAN-FIND(FIRST po-ordl
             WHERE po-ordl.company EQ po-ord.company
               AND po-ordl.po-no   EQ po-ord.po-no) THEN DO:
            ASSIGN
                opcMessage = "No po-ordl records available for po [ " + STRING(po-ord.po-no) + " ]"
                oplSuccess = FALSE
                .
            RETURN.
        END.
            
        FOR EACH po-ordl
            WHERE po-ordl.company EQ po-ord.company
              AND po-ordl.po-no   EQ po-ord.po-no:                       
            
            ASSIGN
                lcDetailData = STRING(APIOutboundDetail.data)
                lcDetailData = REPLACE(lcDetailData, "TBD9", "localhost")    /* lastRequestIP */
                lcDetailData = REPLACE(lcDetailData, "po-ordl.pr-qty-uom", po-ordl.pr-qty-uom)
                lcDetailData = REPLACE(lcDetailData, "TBD7", "24") /* QtyPerPack */
                lcDetailData = REPLACE(lcDetailData, "po-ord.loc", po-ord.loc)
                lcDetailData = REPLACE(lcDetailData, "po-ordl.ord-qty", STRING(po-ordl.ord-qty))
                lcDetailData = REPLACE(lcDetailData, "po-ordl.cons-uom", po-ordl.cons-uom)
                lcDetailData = REPLACE(lcDetailData, "po-ordl.i-name", REPLACE(po-ordl.i-name,'"',''))
                lcDetailData = REPLACE(lcDetailData, "po-ordl.i-no", po-ordl.i-no)
                lcDetailData = REPLACE(lcDetailData, "po-ordl.line", STRING(po-ordl.line))            
                .   
                
            lcConcatDetailData = lcConcatDetailData + "," + lcDetailData.
        END.      
        
        lcConcatDetailData = TRIM(lcConcatDetailData,",").
        
        FIND FIRST vend NO-LOCK
             WHERE vend.company EQ po-ord.company
               AND vend.vend-no EQ po-ord.vend-no NO-ERROR.
        IF AVAILABLE vend THEN
            cCardName = vend.name.
                         
        ASSIGN
            ioplcRequestData = REPLACE(ioplcRequestData, "TBD12", lcConcatDetailData)
            ioplcRequestData = REPLACE(ioplcRequestData, "TBD11", "localhost") /* lastRequestIP */
            ioplcRequestData = REPLACE(ioplcRequestData, "po-ord.due-date", STRING(po-ord.due-date))
            ioplcRequestData = REPLACE(ioplcRequestData, "TBD9", "123")       /* numatcard*/
            ioplcRequestData = REPLACE(ioplcRequestData, "cCardName", cCardName)
            ioplcRequestData = REPLACE(ioplcRequestData, "po-ord.vend-no", po-ord.vend-no)
            ioplcRequestData = REPLACE(ioplcRequestData, "po-ord.last-ship-date", STRING(po-ord.last-ship-date))
            ioplcRequestData = REPLACE(ioplcRequestData, "po-ord.po-date", STRING(po-ord.po-date))
            ioplcRequestData = REPLACE(ioplcRequestData, "po-ord.stat", po-ord.stat)
            ioplcRequestData = REPLACE(ioplcRequestData, "po-ord.po-no", STRING(po-ord.po-no))
            ioplcRequestData = REPLACE(ioplcRequestData, "po-ord.po-no", STRING(po-ord.po-no))
            ioplcRequestData = REPLACE(ioplcRequestData, "po-ord.type", po-ord.type)
            ioplcRequestData = REPLACE(ioplcRequestData, "po-ord.company", po-ord.company)
            opcMessage = ""
            oplSuccess = TRUE
             .
    END.
