/*------------------------------------------------------------------------
    File        : api\InboundProcs.p
    Purpose     : Procedures related to Inbound API

    Syntax      :

    Description : Procedures related to Inbound API

    Author(s)   : Vishnu Vellanki
    Created     : Tue August 27 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
{api\ttInboundRequest.i}

/* Loads data from CSV log file to temp table */
PROCEDURE LoadRequestsFomCSV:
    DEFINE INPUT  PARAMETER ipcCSVFile AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttInboundRequest. 

    DEFINE VARIABLE cAPIRoute         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRequestVerb      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRequestData      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cResponseData     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRequestDataType  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cAppServer        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cErrorMessage     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDateTime         AS CHARACTER  NO-UNDO.

    EMPTY TEMP-TABLE ttInboundRequest.
    
    INPUT FROM VALUE(ipcCSVFile).

    REPEAT:
        IMPORT DELIMITER '|' 
            cAPIRoute 
            cRequestVerb 
            cRequestData 
            cResponseData 
            cRequestDataType 
            cAppServer 
            cErrorMessage 
            cDateTime
            .
       
        FIND FIRST APIInbound NO-LOCK
             WHERE APIInbound.apiRoute EQ cAPIRoute
               AND APIInbound.canBeQueued
             NO-ERROR.
        IF NOT AVAILABLE APIInbound THEN 
            NEXT.
       
        CREATE ttInboundRequest.
        ASSIGN 
            ttInboundRequest.APIRoute        = cAPIRoute
            ttInboundRequest.RequestVerb     = cRequestVerb
            ttInboundRequest.RequestData     = cRequestData
            ttInboundRequest.ResponseData    = cResponseData
            ttInboundRequest.RequestDataType = cRequestDataType
            ttInboundRequest.RequestTime     = cDateTime
            ttInboundRequest.Exception       = cErrorMessage
            .
            
    END.
    INPUT CLOSE.

END PROCEDURE.

/* Processes temp table data */
PROCEDURE ProcessRequests:
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttInboundRequest. 
    DEFINE INPUT PARAMETER ipcUserName AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER ipcPassword AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE cResponse         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iInboundEventID   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cAPIInboundEvent  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRecordSource     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lSuccess          AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cMessage          AS CHARACTER  NO-UNDO.        

    cRecordSource = "Offline".
            
    FOR EACH ttInboundRequest
        WHERE NOT ttInboundRequest.processed:
        RUN api\inbound\APIRequestRouterAS.p (
            INPUT  ttInboundRequest.APIRoute,
            INPUT  ttInboundRequest.RequestVerb,
            INPUT  ipcUserName,
            INPUT  ipcPassword,
            INPUT  ttInboundRequest.RequestDataType,
            INPUT  ttInboundRequest.RequestData,
            INPUT  cRecordSource,     
            OUTPUT cResponse,
            OUTPUT cAPIInboundEvent
            ) NO-ERROR.
       
        FIND FIRST APIInboundEvent NO-LOCK 
             WHERE ROWID(APIInboundEvent) EQ TO-ROWID(cAPIInboundEvent) 
             NO-ERROR.
        IF AVAILABLE APIInboundEvent THEN DO:
            ASSIGN
                iInboundEventID = APIInboundEvent.apiInboundEventID
                lSuccess        = APIInboundEvent.success
                cMessage        = APIInboundEvent.errormessage
                .
        END.
        
        UPDATE 
            ttInboundRequest.exception    = cMessage
            ttInboundRequest.success      = lSuccess
            ttInboundRequest.processed    = YES
            ttInboundRequest.responsedata = cResponse
            .
    END.
END PROCEDURE.

PROCEDURE Inbound_ReTrigger:
     /*------------------------------------------------------------------------------
     Purpose: Public wrapper procedure to re-triggers the Inbound API request for 
              the given APIInboundEventID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiAPIInboundEventID AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess           AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-APIInboundEvent FOR APIInboundEvent.
    
    DEFINE VARIABLE cVerb                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserName               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRequestDataType        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcRequestData           AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cAPIInboundEvent        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponseDataStructure  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRequestHandler         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRequestVerb            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcResponseData          AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lRetrigger              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCompany                AS CHARACTER NO-UNDO.
    
    lRetrigger = YES.
    
    FIND FIRST bf-APIInboundEvent NO-LOCK
         WHERE bf-APIInboundEvent.apiInboundEventID EQ ipiAPIInboundEventID
         NO-ERROR.

    IF AVAILABLE bf-APIInboundEvent THEN DO:
        FIND FIRST APIInbound NO-LOCK
             WHERE APIInbound.apiRoute EQ bf-APIInboundEvent.apiRoute
             NO-ERROR.
        IF AVAILABLE APIInbound THEN
            ASSIGN
                cRequestHandler        = APIInbound.requestHandler
                cResponseDataStructure = APIInbound.responseData
                cRequestVerb           = APIInbound.requestVerb
                cRequestDataType       = APIInbound.requestDataType
                lcRequestData          = bf-APIInboundEvent.requestData
                .
        RUN VALUE(cRequestHandler)(
            INPUT  bf-APIInboundEvent.apiRoute,
            INPUT  cRequestVerb,
            INPUT  cRequestDataType,
            INPUT  lcRequestData,
            INPUT  cResponseDataStructure,
            INPUT  bf-APIInboundEvent.requestedBy,
            INPUT  bf-APIInboundEvent.recordSource,
            INPUT  bf-APIInboundEvent.notes,
            INPUT  cUsername,
            OUTPUT lcResponseData,
            OUTPUT oplSuccess,
            OUTPUT opcMessage,
            OUTPUT cAPIInboundEvent
            ) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            ASSIGN
                oplSuccess = FALSE
                opcMessage = ERROR-STATUS:GET-MESSAGE(1)
                .

        RUN api\CreateAPIInboundEvent.p (
            INPUT  lRetrigger,
            INPUT  ipiAPIInboundEventID,
            INPUT  cCompany,            
            INPUT  bf-APIInboundEvent.apiRoute,
            INPUT  lcRequestData,
            INPUT  lcResponseData,
            INPUT  oplSuccess,
            INPUT  opcMessage,
            INPUT  NOW,
            INPUT  bf-APIInboundEvent.requestedBy,
            INPUT  bf-APIInboundEvent.recordSource,
            INPUT  bf-APIInboundEvent.notes,
            INPUT  bf-APIInboundEvent.externalID, /* PayloadID */
            OUTPUT cAPIInboundEvent
            ) NO-ERROR. 
    END.

    RELEASE bf-APIInboundEvent.

END PROCEDURE.


