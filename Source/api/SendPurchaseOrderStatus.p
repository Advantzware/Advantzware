/*------------------------------------------------------------------------
    File        : api/SendPurchaseOrderStatus.p
    Purpose     : Returns the request data for purchase order status

    Syntax      :

    Description : Returns the request data for purchase order status

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
    DEFINE VARIABLE cCompany           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoID              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoStatus          AS CHARACTER NO-UNDO.
    
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
    
        ASSIGN
            cCompany           = STRING(po-ord.company)
            cPoID              = STRING(po-ord.po-no)
            cPoStatus          = STRING(po-ord.stat)
            .
        
        /* Fetch purchase order notes from notes table */    
                 
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "company", cCompany).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "poID", cPoID).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "poStatus", cPoStatus).

        ASSIGN
            opcMessage = ""
            oplSuccess = TRUE
             .
    END.
