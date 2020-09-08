/*------------------------------------------------------------------------
    File        : api/PrepareOutboundRequest.p
    Purpose     : Prepares outbound request

    Syntax      :

    Description : Prepares outbound request

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
USING System.SharedConfig.

{api/ttArgs.i}

DEFINE INPUT  PARAMETER TABLE                   FOR ttArgs.
DEFINE INPUT  PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER oplcRequestData         AS LONGCHAR  NO-UNDO.  
DEFINE OUTPUT PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage              AS CHARACTER NO-UNDO.

DEFINE VARIABLE scInstance           AS CLASS system.SharedConfig NO-UNDO.
DEFINE VARIABLE lAPIOutboundTestMode AS LOGICAL                   NO-UNDO.

ASSIGN 
    scInstance        = SharedConfig:instance
    lAPIOutboundTestMode = LOGICAL(scInstance:GetValue("APIOutboundTestMode"))NO-ERROR
    .
    
FIND FIRST APIOutbound NO-LOCK
     WHERE APIOutbound.apiOutboundID EQ ipiAPIOutboundID
     NO-ERROR.
IF AVAILABLE APIOutbound AND 
   (lAPIOutboundTestMode OR NOT APIOutbound.Inactive)THEN DO:           
    oplcRequestData = APIOutbound.requestData.
    /* Transform Request Data */
    RUN pPrepareRequest (
        INPUT  APIOutbound.apiID,
        INPUT  APIOutbound.apiOutboundID,
        INPUT  ipiAPIOutboundTriggerID,        
        INPUT  APIOutbound.requestHandler,
        INPUT-OUTPUT oplcRequestData,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).
END.
ELSE
    ASSIGN
        opcMessage = "API Outbound configuration for Outbound Sequence ID [" 
                   + STRING(ipiAPIOutboundID)
                   + "], is not available or inactive"
        oplSuccess = FALSE
        .
        
PROCEDURE pPrepareRequest PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Prepares request for the given API ID
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcAPIID                AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER oplcRequestData         AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdOutboundProcs AS HANDLE NO-UNDO.
    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.
    
    RUN Outbound_IncrementAPITransactionCounter IN hdOutboundProcs (
        INPUT ipiAPIOutboundID
        ).
        
    IF SEARCH("api/" + ipcAPIID + ".r") EQ ? THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Request handler for API " + ipcAPIID + " not Found!"
            .
        RETURN.
    END. 
    
    RUN VALUE("api/" + ipcAPIID + ".r") (
        INPUT TABLE  ttArgs,
        INPUT        ipiAPIOutboundID,
        INPUT        ipiAPIOutboundTriggerID,
        INPUT        ipcRequestHandler,
        INPUT-OUTPUT oplcRequestData,
        OUTPUT       oplSuccess,
        OUTPUT       opcMessage
        ).
    IF oplSuccess THEN
        RUN Outbound_UpdateGlobalFieldValues IN hdOutboundProcs (
            INPUT        ipiAPIOutboundID,
            INPUT-OUTPUT oplcRequestData
            ).

    DELETE PROCEDURE hdOutboundProcs.

END PROCEDURE.
