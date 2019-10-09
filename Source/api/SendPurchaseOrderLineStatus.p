/*------------------------------------------------------------------------
    File        : api/SendPurchaseOrderLineStatus.p
    Purpose     : Returns the request data for purchase order line status

    Syntax      :

    Description : Returns the request data for purchase order line status

    Author(s)   : Mithun Porandla
    Created     : Fri Oct 10 07:33:22 EDT 2019
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
    
    /* Purchase Order Header Variables */
    DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoID         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoLine       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoLineStatus AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-APIOutboundDetail1 FOR APIOutboundDetail.
    DEFINE BUFFER bf-APIOutboundDetail2 FOR APIOutboundDetail.    
    
    /* This is to run client specific request handler to fetch request data */
    IF ipcRequestHandler NE "" THEN
        RUN VALUE(ipcRequestHandler) (
            INPUT TABLE ttArgs,
            INPUT ipiAPIOutboundID,
            INPUT ipiAPIOutboundTriggerID,
            INPUT-OUTPUT ioplcRequestData,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
    ELSE DO:    
        FIND FIRST ttArgs
             WHERE ttArgs.argType  EQ "ROWID"
               AND ttArgs.argKey   EQ "po-ordl"
             NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "No valid po-ordl record passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST po-ordl NO-LOCK
             WHERE ROWID(po-ordl) EQ TO-ROWID(ttArgs.argValue)
             NO-ERROR.
        IF NOT AVAILABLE po-ordl THEN DO:
            ASSIGN
                opcMessage = "Invalid po-ordl ROWID passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
    
        ASSIGN
            cCompany      = STRING(po-ordl.company)
            cPoID         = STRING(po-ordl.po-no)
            cPoLine       = STRING(po-ordl.line)
            cPoLineStatus = STRING(po-ordl.stat)
            .
        
        /* Fetch purchase order notes from notes table */    
                 
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "company", cCompany).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "poID", cPoID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "poLine", cPoLine).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "poLineStatus", cPoLineStatus).

        ASSIGN
            opcMessage = ""
            oplSuccess = TRUE
             .
    END.
