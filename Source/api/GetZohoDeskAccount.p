/*------------------------------------------------------------------------
    File        : api/GetZohoDeskAccount.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Wed Apr 27 07:33:22 EDT 2022
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
    
    DEFINE VARIABLE cAdvantzwareAccountID AS CHARACTER NO-UNDO.
    
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
        RUN spGetSettingByName("AdvantzwareAccountID", OUTPUT cAdvantzwareAccountID).
        
        IF cAdvantzwareAccountID EQ "" THEN DO:
            ASSIGN
                opcMessage = "AdvantzwareAccountID is not set in NK6 'AdvantzwareAccountID'"
                oplSuccess = FALSE
                .
            
            RETURN.
        END.
        
        FIND FIRST apiOutboundContent EXCLUSIVE-LOCK
             WHERE apiOutboundContent.apiOutboundID EQ ipiAPIOutboundID
               AND apiOutboundContent.contentKey    EQ "AdvantzwareAccountID"
             NO-ERROR.
          
        IF NOT AVAILABLE apiOutboundContent THEN DO:
            CREATE apiOutboundContent.
            ASSIGN
                apiOutboundContent.apiOutboundID = ipiAPIOutboundID
                apiOutboundContent.contentType   = "User"
                apiOutboundContent.contentKey    = "AdvantzwareAccountID"
                .
        END.
        
        apiOutboundContent.contentValue = cAdvantzwareAccountID.
        
        ASSIGN   
            opcMessage       = ""
            oplSuccess       = TRUE
            .
    END.        
        
