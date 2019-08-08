/*------------------------------------------------------------------------
    File        : api/SendPurchaseOrderRequestHandler0001.p
    Purpose     : Returns the request data for purchase order addition

    Syntax      :

    Description : Returns the request data for purchase order addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
    {api/ttArgs.i}
    {api/CommonAPIProcs.i}
    
    DEFINE INPUT        PARAMETER TABLE            FOR ttArgs.
    DEFINE INPUT        PARAMETER ipcParentID      AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lcDetailData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatDetailData AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cCardName          AS CHARACTER NO-UNDO.
    
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
        
        lcDetailData = STRING(APIOutboundDetail.data).
        
        RUN updateRequestData(INPUT-OUTPUT lcDetailData, "TBD9", "localhost").    /* lastRequestIP */
        RUN updateRequestData(INPUT-OUTPUT lcDetailData, "po-ordl.pr-qty-uom", po-ordl.pr-qty-uom).
        RUN updateRequestData(INPUT-OUTPUT lcDetailData, "TBD7", "24"). /* QtyPerPack */
        RUN updateRequestData(INPUT-OUTPUT lcDetailData, "po-ord.loc", po-ord.loc).
        RUN updateRequestData(INPUT-OUTPUT lcDetailData, "po-ordl.ord-qty", STRING(po-ordl.ord-qty)).
        RUN updateRequestData(INPUT-OUTPUT lcDetailData, "po-ordl.cons-uom", po-ordl.cons-uom).
        RUN updateRequestData(INPUT-OUTPUT lcDetailData, "po-ordl.i-name", po-ordl.i-name).
        RUN updateRequestData(INPUT-OUTPUT lcDetailData, "po-ordl.i-no", po-ordl.i-no).
        RUN updateRequestData(INPUT-OUTPUT lcDetailData, "po-ordl.line", STRING(po-ordl.line)).
            
        lcConcatDetailData = lcConcatDetailData + "," + lcDetailData.
    END.      
    
    lcConcatDetailData = TRIM(lcConcatDetailData,",").
    
    FIND FIRST vend NO-LOCK
         WHERE vend.company EQ po-ord.company
           AND vend.vend-no EQ po-ord.vend-no NO-ERROR.
    IF AVAILABLE vend THEN
        cCardName = vend.name.
                     
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TBD11", "localhost"). /* lastRequestIP */
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.due-date", STRING(po-ord.due-date)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TBD9", "123").       /* numatcard*/
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "cCardName", cCardName).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.vend-no", po-ord.vend-no).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.last-ship-date", STRING(po-ord.last-ship-date)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.po-date", STRING(po-ord.po-date)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.stat", po-ord.stat).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.po-no", STRING(po-ord.po-no)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.po-no", STRING(po-ord.po-no)).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.type", po-ord.type).
    RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "po-ord.company", po-ord.company).

    /* This replace is required for replacing nested JSON data */
    ioplcRequestData = REPLACE(ioplcRequestData, "$TBD12$", lcConcatDetailData).

    ASSIGN
        opcMessage = ""
        oplSuccess = TRUE
         .
