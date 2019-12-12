
/*------------------------------------------------------------------------
    File        : testers/OutboundAPITester.p
    Purpose     : Outbound API Tester

    Syntax      :

    Description : Outbound API Tester

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 11 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE cAPIID             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cClientID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTriggerID         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cROWIDList         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTableList         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSuccess           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cPrimaryID         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEventDescription  AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutboundProcs    AS HANDLE    NO-UNDO.
DEFINE VARIABLE lPrepareAndCall    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lPrepareBeforeCall AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lReTrigger         AS LOGICAL   NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */
{api/ttAPIOutboundEvent.i}

ASSIGN
    lPrepareAndCall    = FALSE
    lPrepareBeforeCall = TRUE
    lReTrigger         = TRUE
    .

RUN api\OutboundProcs.p PERSISTENT SET hdOutboundProcs.

FIND LAST oe-relh NO-LOCK NO-ERROR.

ASSIGN
    cAPIID            = "SendRelease"
    cClientID         = ""
    cTriggerID        = "PrintRelease"
    cCompany          = "001"
    cLocation         = "MAIN"
    cPrimaryID        = STRING(oe-relh.release#)
    cROWIDList        = STRING(ROWID(oe-relh))
    cTableList        = "oe-relh"
    cEventDescription = "Print Release"
    .

IF lPrepareAndCall THEN
    /* Prepare request data and call outbound API in one call. */
    RUN Outbound_PrepareAndExecute IN hdOutboundProcs (
        INPUT  cCompany,                /* Company Code (Mandatory) */
        INPUT  cLocation,               /* Location Code (Mandatory) */
        INPUT  cAPIID,                  /* API ID (Mandatory) */
        INPUT  cClientID,               /* Client ID (Optional) - Pass empty in case to make request for all clients */
        INPUT  cTriggerID,              /* Trigger ID (Mandatory) */
        INPUT  cTableList,              /* Comma separated list of table names for which data being sent (Mandatory) */
        INPUT  cROWIDList,              /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
        INPUT  cPrimaryID,              /* Primary ID for which API is called for (Mandatory) */   
        INPUT  cEventDescription,       /* Event's description (Optional) */
        OUTPUT lSuccess,                /* Success/Failure flag */
        OUTPUT cMessage                 /* Status message */
        ) NO-ERROR.

IF lPrepareBeforeCall THEN DO:
    /* Prepare request data and process Outbound API later. This is useful 
       particularly in case of delete triggers, where request data is generated 
       before deleting the record and call Outbound API after successful deletion of the record.
       User can prepare as many request as required before calling the Outbound APIs. */
    RUN Outbound_PrepareRequest IN hdOutboundProcs (
        INPUT  cCompany,                /* Company Code (Mandatory) */
        INPUT  cLocation,               /* Location Code (Mandatory) */
        INPUT  cAPIID,                  /* API ID (Mandatory) */
        INPUT  cClientID,               /* Client ID (Optional) - Pass empty in case to make request for all clients */
        INPUT  cTriggerID,              /* Trigger ID (Mandatory) */
        INPUT  cTableList,              /* Comma separated list of table names for which data being sent (Mandatory) */
        INPUT  cROWIDList,              /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */
        INPUT  cPrimaryID,              /* Primary ID for which API is called for (Mandatory) */
        INPUT  cEventDescription,       /* Event's description (Optional) */
        OUTPUT lSuccess,                /* Success/Failure flag */
        OUTPUT cMessage                 /* Status message */
        ) NO-ERROR.

    IF lSuccess THEN
        /* Call the Outbound APIs for the requests prepared previously. Once
           called, all the requests prepared previously will be deleted */
        RUN Outbound_Execute IN hdOutboundProcs (
            OUTPUT       lSuccess,
            OUTPUT       cMessage
            ) NO-ERROR.           

    IF lReTrigger THEN DO:    
        FIND LAST APIOutboundEvent NO-LOCK
             WHERE APIOutboundEvent.apiOutboundEventID EQ 1007 
             NO-ERROR.
        IF AVAILABLE APIOutboundEvent THEN DO:
            ASSIGN
                cAPIID            = APIOutboundEvent.apiID
                cClientID         = APIOutboundEvent.clientID
                cTriggerID        = APIOutboundEvent.sourceTriggerID
                cCompany          = APIOutboundEvent.company
                cLocation         = "MAIN"
                cROWIDList        = STRING(ROWID(APIOutboundEvent))
                cTableList        = "APIOutboundEvent"
                cPrimaryID        = APIOutboundEvent.primaryID
                cEventDescription = "Re-Trigger Test"
                .
            /* Preparing request data for re-trigger events */
            RUN Outbound_ReTrigger IN hdOutboundProcs (
                INPUT  APIOutboundEvent.apiOutboundEventID,
                OUTPUT       lSuccess,                /* Success/Failure flag */
                OUTPUT       cMessage                 /* Status message */
                ).                
        END.
    END.
END.

RUN Outbound_GetEvents IN hdOutboundProcs (
    OUTPUT TABLE ttAPIOutboundEvent
    ).

FOR EACH ttAPIOutboundEvent:
    DISP ttAPIOutboundEvent.
END.
    
MESSAGE "Message:" cMessage SKIP
        "Success:" lSuccess SKIP
    VIEW-AS ALERT-BOX.    

IF VALID-HANDLE (hdOutboundProcs) THEN
    DELETE PROCEDURE hdOutboundProcs.
