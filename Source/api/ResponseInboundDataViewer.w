&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: api/ResponseInboundDataViewer.w

  Description: Displays the Request/Response Data of a API Inbound Event

  Input Parameters:
      ipriInboundEvent: ROWID of the APIInboundEvent

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipiApiInboundEventID AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hdInboundProcs AS HANDLE NO-UNDO.
RUN api/InboundProcs.p PERSISTENT SET hdInboundProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 edDescription edRequestData ~
edResponseData edErrorMessage Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiReqDataType fiRequestVerb fiCanBeQueued ~
fiReqDataTypelb fiRequestVerblb ficanBeQueuedlbb fiapiRoute fiRecordSource ~
fiReqAPIRoutelb fiRecordSourcelb fiExternalID fiExternalIDLabel ~
fiDescriptionLabel edDescription fiRequestDataLabel edRequestData ~
fiResponseDataLabel edResponseData fiErrorMessageLabel edErrorMessage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U
     LABEL "Cancel" 
     SIZE 20 BY 2.62
     BGCOLOR 8 .

DEFINE BUTTON btReTrigger 
     LABEL "Re-Trigger Request" 
     SIZE 31 BY 2.62
     FONT 6.

DEFINE BUTTON btUpdateRequestData  NO-CONVERT-3D-COLORS
     LABEL "Update Request Data" 
     SIZE 31 BY 2.62
     BGCOLOR 8 FONT 6.

DEFINE VARIABLE edDescription AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 119 BY 2.62
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE edErrorMessage AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 119 BY 2.48
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE edRequestData AS LONGCHAR 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 119 BY 5.71
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE edResponseData AS LONGCHAR 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 119 BY 5.95
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fiapiRoute AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiCanBeQueued AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE ficanBeQueuedlbb AS CHARACTER FORMAT "X(256)":U INITIAL "Can Be Queued:" 
     VIEW-AS FILL-IN 
     SIZE 21.4 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiDescriptionLabel AS CHARACTER FORMAT "X(256)":U INITIAL "API Route Description:" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiErrorMessageLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Response Result:" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiExternalID AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 98 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiExternalIDLabel AS CHARACTER FORMAT "X(30)":U INITIAL "External ID:" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiRecordSource AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiRecordSourcelb AS CHARACTER FORMAT "X(256)":U INITIAL "Record Source:" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiReqAPIRoutelb AS CHARACTER FORMAT "X(256)":U INITIAL "API Route:" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiReqDataType AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiReqDataTypelb AS CHARACTER FORMAT "X(256)":U INITIAL "Request Datatype:" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiRequestDataLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Request Data:" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiRequestVerb AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiRequestVerblb AS CHARACTER FORMAT "X(256)":U INITIAL "Request Verb:" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiResponseDataLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Response Data:" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     FONT 35 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 128 BY 3.81.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 128 BY 24.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiReqDataType AT ROW 1.71 COL 32.4 COLON-ALIGNED NO-LABEL WIDGET-ID 118
     fiRequestVerb AT ROW 1.71 COL 75.2 COLON-ALIGNED NO-LABEL WIDGET-ID 122
     fiCanBeQueued AT ROW 1.76 COL 120 COLON-ALIGNED NO-LABEL WIDGET-ID 114
     fiReqDataTypelb AT ROW 1.81 COL 6 COLON-ALIGNED NO-LABEL WIDGET-ID 120
     fiRequestVerblb AT ROW 1.81 COL 54.4 COLON-ALIGNED NO-LABEL WIDGET-ID 124
     ficanBeQueuedlbb AT ROW 1.86 COL 98 COLON-ALIGNED NO-LABEL WIDGET-ID 116
     fiapiRoute AT ROW 3.76 COL 21 COLON-ALIGNED NO-LABEL WIDGET-ID 126
     fiRecordSource AT ROW 3.76 COL 97.4 COLON-ALIGNED NO-LABEL WIDGET-ID 110
     fiReqAPIRoutelb AT ROW 3.86 COL 5.6 COLON-ALIGNED NO-LABEL WIDGET-ID 128
     fiRecordSourcelb AT ROW 3.86 COL 75.4 COLON-ALIGNED NO-LABEL WIDGET-ID 112
     fiExternalID AT ROW 6.19 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 132
     fiExternalIDLabel AT ROW 6.24 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 130
     fiDescriptionLabel AT ROW 7.67 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     edDescription AT ROW 8.86 COL 11 NO-LABEL WIDGET-ID 14
     fiRequestDataLabel AT ROW 11.71 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     edRequestData AT ROW 13.14 COL 11 NO-LABEL WIDGET-ID 2
     fiResponseDataLabel AT ROW 19.1 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     edResponseData AT ROW 20.52 COL 11 NO-LABEL WIDGET-ID 10
     fiErrorMessageLabel AT ROW 26.62 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     edErrorMessage AT ROW 27.76 COL 11 NO-LABEL WIDGET-ID 94
     btUpdateRequestData AT ROW 31 COL 13
     btReTrigger AT ROW 31 COL 61.8 WIDGET-ID 134
     Btn_Cancel AT ROW 31 COL 110
     RECT-1 AT ROW 1.48 COL 7 WIDGET-ID 84
     RECT-2 AT ROW 5.91 COL 7 WIDGET-ID 86
     SPACE(6.79) SKIP(3.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 
         TITLE "Inbound Request/Response"
         CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btReTrigger IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btUpdateRequestData IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       edDescription:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       edErrorMessage:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       edRequestData:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       edResponseData:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiapiRoute IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiapiRoute:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiCanBeQueued IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiCanBeQueued:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN ficanBeQueuedlbb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       ficanBeQueuedlbb:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiDescriptionLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiDescriptionLabel:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiErrorMessageLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiErrorMessageLabel:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiExternalID IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiExternalID:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiExternalIDLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiExternalIDLabel:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiRecordSource IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiRecordSource:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiRecordSourcelb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiRecordSourcelb:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiReqAPIRoutelb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiReqAPIRoutelb:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiReqDataType IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiReqDataType:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiReqDataTypelb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiReqDataTypelb:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiRequestDataLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiRequestDataLabel:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiRequestVerb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiRequestVerb:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiRequestVerblb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiRequestVerblb:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiResponseDataLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiResponseDataLabel:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Inbound Request/Response */
DO:
    IF btReTrigger:SENSITIVE THEN DO:
        MESSAGE "Request data modified, but event is not re-triggered yet." SKIP
            "Do you want to exit the screen without re-triggering the event?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lAnswer AS LOGICAL.
        IF NOT lAnswer THEN
            RETURN.
    END.

    IF VALID-HANDLE(hdInboundProcs) THEN
        DELETE PROCEDURE hdInboundProcs.

    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    IF btReTrigger:SENSITIVE THEN DO:
        MESSAGE "Request data modified, but event is not re-triggered yet." SKIP
            "Do you want to exit the screen without re-triggering the event?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lAnswer AS LOGICAL.
        IF NOT lAnswer THEN
            RETURN.
    END.
    
    IF VALID-HANDLE(hdInboundProcs) THEN
        DELETE PROCEDURE hdInboundProcs.  

    APPLY "END-ERROR":U TO SELF.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReTrigger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReTrigger Dialog-Frame
ON CHOOSE OF btReTrigger IN FRAME Dialog-Frame /* Re-Trigger Request */
DO:
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    SELF:SENSITIVE = FALSE.
    
    RUN Inbound_ReTrigger IN hdInboundProcs (
        INPUT  ipiAPIInboundEventID,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ) NO-ERROR.  
    
    RUN pInit.
        
    MESSAGE "Re-trigger complete"
        VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUpdateRequestData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdateRequestData Dialog-Frame
ON CHOOSE OF btUpdateRequestData IN FRAME Dialog-Frame /* Update Request Data */
DO:
    DEFINE VARIABLE lcRequestData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lRequestDataChanged AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cRequestDataChanges AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage            AS CHARACTER NO-UNDO.

    ASSIGN
        edRequestData
        fiReqDataType
        .
        
    RUN api/APIInboundEventRequestViewer.w (
        INPUT  edRequestData,
        INPUT  fiReqDataType,
        OUTPUT lcRequestData,
        OUTPUT lRequestDataChanged,
        OUTPUT cRequestDataChanges
        ).

    IF lRequestDataChanged THEN DO:
        RUN Inbound_UpdateEventRequestData IN hdInboundProcs (
            INPUT  ipiAPIInboundEventID,
            INPUT  lcRequestData,
            INPUT  cRequestDataChanges,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).
        
        RUN pInit.
        
        btReTrigger:SENSITIVE = TRUE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  ASSIGN
      edRequestData:WORD-WRAP  = TRUE
      edResponseData:WORD-WRAP = TRUE
      .

  RUN enable_UI.
  RUN pInit.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiReqDataType fiRequestVerb fiCanBeQueued fiReqDataTypelb 
          fiRequestVerblb ficanBeQueuedlbb fiapiRoute fiRecordSource 
          fiReqAPIRoutelb fiRecordSourcelb fiExternalID fiExternalIDLabel 
          fiDescriptionLabel edDescription fiRequestDataLabel edRequestData 
          fiResponseDataLabel edResponseData fiErrorMessageLabel edErrorMessage 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 RECT-2 edDescription edRequestData edResponseData 
         edErrorMessage Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit Dialog-Frame 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lcData AS LONGCHAR NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    FIND FIRST APIInboundEvent NO-LOCK 
         WHERE APIInboundEvent.apiInboundEventID EQ ipiApiInboundEventID
         NO-ERROR.
    IF AVAILABLE APIInboundEvent THEN DO:
        ASSIGN
            lcData                      = APIInboundEvent.requestData
            edRequestData:SCREEN-VALUE  = lcData
            lcData                      = APIInboundEvent.responseData
            edResponseData:SCREEN-VALUE = lcData
            edErrorMessage:SCREEN-VALUE = APIInboundEvent.errorMessage
            edErrorMessage:FGCOLOR      = IF APIInboundEvent.success THEN
                                              2
                                          ELSE
                                              12            
            fiRecordSource:SCREEN-VALUE = APIInboundEvent.recordSource 
            fiExternalID:SCREEN-VALUE   = APIInboundEvent.externalID                                                
            NO-ERROR.            
    
       FIND FIRST APIInbound NO-LOCK 
            WHERE APIInbound.apiRoute EQ APIInboundEvent.apiRoute 
            NO-ERROR.
       IF AVAILABLE APIInbound THEN
           ASSIGN
               fiRequestVerb:SCREEN-VALUE  = APIInbound.requestVerb
               fiReqDataType:SCREEN-VALUE  = APIInbound.requestDataType
               edDescription:SCREEN-VALUE  = APIInbound.description
               fiapiRoute:SCREEN-VALUE     = APIInbound.apiRoute
               fiCanBeQueued:SCREEN-VALUE  = STRING(APIInbound.canBeQueued,"YES/NO")
               NO-ERROR.
        
        /* Allow updating request data only if event is failed and 
           api can be queued */
        IF NOT APIInboundEvent.success AND APIInbound.canBeQueued THEN
            btUpdateRequestData:SENSITIVE = TRUE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

