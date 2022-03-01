/*------------------------------------------------------------------------
    File        : api/SendCustomer.p
    Purpose     : Returns the request data for customer addition

    Syntax      :

    Description : Returns the request data for customer addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
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
    
    DEFINE VARIABLE cCalcAddress AS CHARACTER NO-UNDO.

    RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).
        
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
               AND ttArgs.argKey   = "cust" NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "No valid cust record passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST cust NO-LOCK
             WHERE ROWID(cust) = TO-ROWID(ttArgs.argValue) NO-ERROR.
        IF NOT AVAILABLE cust THEN DO:
            ASSIGN
                opcMessage = "Invalid cust ROWID passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.

        cCalcAddress     = cust.addr[1] + ", " + cust.addr[2] + ", " + cust.city + ", " + cust.state + ", " + cust.zip.

        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData,"cust.company",cust.company).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData,"cust.cust-no",cust.cust-no).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData,"cust.name",cust.name).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData,"cust.type",cust.type).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData,"calcAddress", cCalcAddress).
        
        ASSIGN   
            opcMessage       = ""
            oplSuccess       = TRUE
            .
    END.        
        
