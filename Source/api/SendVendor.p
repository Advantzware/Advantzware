/*------------------------------------------------------------------------
    File        : api/SendVendor.p
    Purpose     : Returns the request data for vendor addition

    Syntax      :

    Description : Returns the request data for vendor addition

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
               AND ttArgs.argKey   = "vend" NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "No valid vend record passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST vend NO-LOCK
             WHERE ROWID(vend) = TO-ROWID(ttArgs.argValue) NO-ERROR.
        IF NOT AVAILABLE vend THEN DO:
            ASSIGN
                opcMessage = "Invalid vend ROWID passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
    
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vend.type", vend.type).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vend.name", vend.name).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vend.vend-no", vend.vend-no).
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "vend.company", vend.company).
            
        ASSIGN
            opcMessage = ""
            oplSuccess = TRUE
            .
    END.
