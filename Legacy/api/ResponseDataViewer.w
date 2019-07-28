&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: api/ResponseDataViewer.w

  Description: Displays the Request/Response Data of a API Outbound Event

  Input Parameters:
      ipriOutboundEvent: ROWID of the APIOutboundEvent

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipriOutboundEvent AS ROWID NO-UNDO.
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 edEndpoint edRequestData ~
edResponseData edErrorMessage edNotes Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiRequestVerb fiSSLEnabled fiClientID ~
fiRequestVerblb fiSSLEnabledlb fiClientIDlb fiReqDataType fiAuthType ~
fiReqDataTypelb fiRequestVerb-2 fiEndPointLabel edEndpoint ~
fiRequestDataLabel edRequestData fiResponseDataLabel edResponseData ~
fiErrorMessageLabel edErrorMessage fiNotesLabel edNotes 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "Cancel" 
     SIZE 20 BY 2.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/check.ico":U
     LABEL "OK" 
     SIZE 20 BY 2.62
     BGCOLOR 8 .

DEFINE VARIABLE edEndpoint AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 119 BY 2.62
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE edErrorMessage AS LONGCHAR 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 119 BY 2.48
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE edNotes AS LONGCHAR 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 119 BY 4.38
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE edRequestData AS LONGCHAR 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 119 BY 5.71
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE edResponseData AS LONGCHAR 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 119 BY 4.05
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fiAuthType AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiClientID AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiClientIDlb AS CHARACTER FORMAT "X(256)":U INITIAL "Client ID:" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiEndPointLabel AS CHARACTER FORMAT "X(256)":U INITIAL "End Point:" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiErrorMessageLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Response Result:" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiNotesLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Notes:" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiReqDataType AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiReqDataTypelb AS CHARACTER FORMAT "X(256)":U INITIAL "Req Datatype:" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiRequestDataLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Request Data:" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiRequestVerb AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiRequestVerb-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Auth Type:" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiRequestVerblb AS CHARACTER FORMAT "X(256)":U INITIAL "Request Verb:" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiResponseDataLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Response Data:" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiSSLEnabled AS LOGICAL FORMAT "YES/NO":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiSSLEnabledlb AS CHARACTER FORMAT "X(256)":U INITIAL "SSL Enabled:" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     FONT 35 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 126 BY 3.81.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 126 BY 25.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiRequestVerb AT ROW 1.57 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 82
     fiSSLEnabled AT ROW 1.57 COL 71.8 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     fiClientID AT ROW 1.57 COL 107.6 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     fiRequestVerblb AT ROW 1.67 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     fiSSLEnabledlb AT ROW 1.67 COL 52.4 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     fiClientIDlb AT ROW 1.67 COL 93.4 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     fiReqDataType AT ROW 3.29 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     fiAuthType AT ROW 3.29 COL 71.8 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     fiReqDataTypelb AT ROW 3.33 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     fiRequestVerb-2 AT ROW 3.33 COL 52.4 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     fiEndPointLabel AT ROW 5.24 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     edEndpoint AT ROW 6.33 COL 11 NO-LABEL WIDGET-ID 14
     fiRequestDataLabel AT ROW 9.05 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     edRequestData AT ROW 10.14 COL 11 NO-LABEL WIDGET-ID 2
     fiResponseDataLabel AT ROW 15.95 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     edResponseData AT ROW 17.05 COL 11 NO-LABEL WIDGET-ID 10
     fiErrorMessageLabel AT ROW 21.19 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     edErrorMessage AT ROW 22.29 COL 11 NO-LABEL WIDGET-ID 94
     fiNotesLabel AT ROW 24.91 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     edNotes AT ROW 26 COL 11 NO-LABEL WIDGET-ID 98
     Btn_OK AT ROW 31 COL 41.2
     Btn_Cancel AT ROW 31 COL 81
     RECT-1 AT ROW 1.14 COL 8 WIDGET-ID 84
     RECT-2 AT ROW 5.1 COL 8 WIDGET-ID 86
     SPACE(7.79) SKIP(2.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 
         TITLE "Request/Response"
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

ASSIGN 
       edEndpoint:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       edErrorMessage:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       edNotes:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       edRequestData:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       edResponseData:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiAuthType IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiAuthType:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiClientID IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiClientID:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiClientIDlb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiClientIDlb:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiEndPointLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiEndPointLabel:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiErrorMessageLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiErrorMessageLabel:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiNotesLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiNotesLabel:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

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

/* SETTINGS FOR FILL-IN fiRequestVerb-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiRequestVerb-2:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiRequestVerblb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiRequestVerblb:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiResponseDataLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiResponseDataLabel:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiSSLEnabled IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiSSLEnabled:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiSSLEnabledlb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiSSLEnabledlb:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Request/Response */
DO:
  APPLY "END-ERROR":U TO SELF.
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
      edNotes:WORD-WRAP        = TRUE
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
  DISPLAY fiRequestVerb fiSSLEnabled fiClientID fiRequestVerblb fiSSLEnabledlb 
          fiClientIDlb fiReqDataType fiAuthType fiReqDataTypelb fiRequestVerb-2 
          fiEndPointLabel edEndpoint fiRequestDataLabel edRequestData 
          fiResponseDataLabel edResponseData fiErrorMessageLabel edErrorMessage 
          fiNotesLabel edNotes 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 RECT-2 edEndpoint edRequestData edResponseData edErrorMessage 
         edNotes Btn_OK Btn_Cancel 
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
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    FIND FIRST APIOutboundEvent NO-LOCK 
         WHERE ROWID(APIOutboundEvent) EQ ipriOutboundEvent 
         NO-ERROR.
    IF AVAILABLE APIOutboundEvent THEN DO:
        ASSIGN
            edRequestData:SCREEN-VALUE  = STRING(APIOutboundEvent.requestData)
            edResponseData:SCREEN-VALUE = STRING(APIOutboundEvent.responseData)                    
            edNotes:SCREEN-VALUE        = STRING(APIOutboundEvent.notes)
            edErrorMessage:SCREEN-VALUE = IF APIOutboundEvent.success THEN
                                              "SUCCESS:" + "~n" + APIOutboundEvent.errorMessage
                                          ELSE
                                              "FAILURE:" + "~n" + APIOutboundEvent.errorMessage
            edErrorMessage:FGCOLOR      = IF APIOutboundEvent.success THEN
                                              2
                                          ELSE
                                              12
            .            
    
       FIND FIRST APIOutbound NO-LOCK 
            WHERE APIOutbound.apiID EQ APIOutboundEvent.apiID 
            NO-ERROR.
       IF AVAILABLE APIOutbound THEN
           ASSIGN
               edEndpoint:SCREEN-VALUE     = APIOutbound.endPoint
               fiRequestVerb:SCREEN-VALUE  = APIOutbound.requestVerb
               fiReqDataType:SCREEN-VALUE  = APIOutbound.requestDataType
               fiSSLEnabled:SCREEN-VALUE   = STRING(APIOutbound.isSSLEnabled)
               fiClientID:SCREEN-VALUE     = APIOutbound.clientID
               fiAuthType:SCREEN-VALUE     = APIOutbound.authType
               .
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

