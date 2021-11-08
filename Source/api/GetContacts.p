/*------------------------------------------------------------------------
    File        : api/GetContact.p
    Purpose     : Returns the request data for contact (phone)

    Syntax      :

    Description : Returns the request data for contact (phone)

    Author(s)   : DEVA$!
    Created     : Sun Sep 12 07:33:22 EDT 2021
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
    
    RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).
    
    DEFINE VARIABLE cNextPageLinkID AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-cust FOR cust.
    
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
        IF AVAILABLE ttArgs THEN DO:
            FIND FIRST bf-cust NO-LOCK
                 WHERE ROWID(cust) = TO-ROWID(ttArgs.argValue) 
                 NO-ERROR.
        END.

        FIND FIRST ttArgs
             WHERE ttArgs.argType  = "ROWID"
               AND ttArgs.argKey   = "HubSpotNextPageLinkID" NO-ERROR.
        IF AVAILABLE ttArgs THEN
            cNextPageLinkID = ttArgs.argValue.
        
        IF cNextPageLinkID EQ "" THEN
            cNextPageLinkID = "0".

        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData,"NextPageLinkID",cNextPageLinkID).        
        
        ASSIGN   
            opcMessage       = ""
            oplSuccess       = TRUE
            .
    END.        
        
