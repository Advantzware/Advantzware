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
    DEFINE VARIABLE cEventID          AS INTEGER    NO-UNDO.
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
                cEventID = APIInboundEvent.eventid
                lSuccess = APIInboundEvent.success
                cMessage = APIInboundEvent.errormessage
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


