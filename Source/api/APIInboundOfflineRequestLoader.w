&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: api\APIInboundOfflineRequestLoader.w

  Description: Loads and processes the Inbound requests from CSV

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Vishnu Vellanki

  Created: 8/27/2019

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{api\ttInboundRequest.i}

DEFINE VARIABLE cCSVFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdInboundProcs AS HANDLE    NO-UNDO.

RUN api/InboundProcs.p PERSISTENT SET hdInboundProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-4

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttInboundRequest

/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 ttInboundRequest.apiRoute ttInboundRequest.requestVerb ttInboundRequest.requestDataType ttInboundRequest.requestTime ttInboundRequest.processed ttInboundRequest.success   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4   
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH ttInboundRequest
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME} FOR EACH ttInboundRequest.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 ttInboundRequest
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 ttInboundRequest


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 btexit fiFileSelector btOpenFile ~
BROWSE-4 edRequestData edResponseData 
&Scoped-Define DISPLAYED-OBJECTS fiFileSelector fiMessageText ~
fiRequestDataLabel edRequestData edResponseData fiResponseDataLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btexit 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "" 
     SIZE 9 BY 2.14 TOOLTIP "Exit Window".

DEFINE BUTTON btLoad 
     LABEL "Load" 
     SIZE 15 BY 1.91
     FONT 6.

DEFINE BUTTON btOpenFile 
     LABEL "Select File" 
     SIZE 18.2 BY 1.19
     FGCOLOR 9 FONT 6.

DEFINE BUTTON btTrigger 
     LABEL "Trigger" 
     SIZE 15 BY 1.91
     FONT 6.

DEFINE VARIABLE edRequestData AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 134.6 BY 5.71 NO-UNDO.

DEFINE VARIABLE edResponseData AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 134.4 BY 4 NO-UNDO.

DEFINE VARIABLE fiFileSelector AS CHARACTER FORMAT "X(256)":U INITIAL "Select File to Load..." 
     VIEW-AS FILL-IN 
     SIZE 104 BY 1.19
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE fiMessageText AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 82 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiRequestDataLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Request Data" 
     VIEW-AS FILL-IN 
     SIZE 17.2 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiResponseDataLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Response Data" 
     VIEW-AS FILL-IN 
     SIZE 19.2 BY 1
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 141.6 BY 5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-4 FOR 
      ttInboundRequest SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 C-Win _FREEFORM
  QUERY BROWSE-4 DISPLAY
      ttInboundRequest.apiRoute COLUMN-LABEL "API Route" FORMAT "X(30)"
        WIDTH 30
    ttInboundRequest.requestVerb COLUMN-LABEL "Request Verb" FORMAT "X(30)"
        WIDTH 25
    ttInboundRequest.requestDataType COLUMN-LABEL "Request Data Type" FORMAT "X(30)"
        WIDTH 25
    ttInboundRequest.requestTime COLUMN-LABEL "Request Time" FORMAT "X(30)"
        WIDTH 30
    ttInboundRequest.processed COLUMN-LABEL "Processed"
        WIDTH 15
    ttInboundRequest.success COLUMN-LABEL "Success"
        WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 155 BY 12.52
         FONT 5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btexit AT ROW 1.29 COL 151.6 WIDGET-ID 12
     fiFileSelector AT ROW 1.95 COL 7.4 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     btOpenFile AT ROW 1.95 COL 114.8 WIDGET-ID 4
     btLoad AT ROW 3.62 COL 9.2 WIDGET-ID 6
     btTrigger AT ROW 3.67 COL 27.2 WIDGET-ID 8
     fiMessageText AT ROW 3.86 COL 49 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     BROWSE-4 AT ROW 6.57 COL 4.6 WIDGET-ID 200
     fiRequestDataLabel AT ROW 20.24 COL 4.6 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     edRequestData AT ROW 20.24 COL 24.4 NO-LABEL WIDGET-ID 20
     edResponseData AT ROW 27.19 COL 24.6 NO-LABEL WIDGET-ID 18
     fiResponseDataLabel AT ROW 27.24 COL 2.8 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     RECT-1 AT ROW 1.24 COL 4.4 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 161.8 BY 30.95
         BGCOLOR 15  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "API Inbound Offline Request Loader"
         HEIGHT             = 30.95
         WIDTH              = 161.8
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 273.2
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-4 fiMessageText DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btLoad IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btTrigger IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       edRequestData:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       edResponseData:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiFileSelector:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiMessageText IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiRequestDataLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiResponseDataLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttInboundRequest.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* API Inbound Offline Request Loader */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* API Inbound Offline Request Loader */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-4
&Scoped-define SELF-NAME BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 C-Win
ON VALUE-CHANGED OF BROWSE-4 IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE ttInboundRequest THEN
        ASSIGN
            edRequestData:SCREEN-VALUE  = STRING(ttInboundRequest.requestData)
            edResponseData:SCREEN-VALUE = STRING(ttInboundRequest.responseData)
            .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btexit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btexit C-Win
ON CHOOSE OF btexit IN FRAME DEFAULT-FRAME
DO:
    APPLY "CLOSE" TO THIS-PROCEDURE.
    
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLoad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLoad C-Win
ON CHOOSE OF btLoad IN FRAME DEFAULT-FRAME /* Load */
DO:
    cCSVFile = fiFileSelector:SCREEN-VALUE.
    
    RUN LoadRequestsFomCSV IN hdInboundProcs (
        INPUT cCSVFile,
        OUTPUT TABLE ttInboundRequest
        ) NO-ERROR.

   {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOpenFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOpenFile C-Win
ON CHOOSE OF btOpenFile IN FRAME DEFAULT-FRAME /* Select File */
DO:
    DEFINE VARIABLE cCSVFileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOkPressed   AS LOGICAL   NO-UNDO INITIAL TRUE.
    
    OPEN-FILE-BLOCK:    
    REPEAT:
        SYSTEM-DIALOG GET-FILE cCSVFileName
          TITLE   "Choose file to Run ..."
          FILTERS "Source Files (*.csv)"   "*.csv"
          MUST-EXIST
          USE-FILENAME
          UPDATE lOkPressed.
  
        IF lOkPressed THEN       
            ASSIGN
                fiFileSelector:SCREEN-VALUE = cCSVFileName
                btLoad:SENSITIVE            = TRUE
                btTrigger:SENSITIVE         = TRUE
                .
        LEAVE OPEN-FILE-BLOCK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btTrigger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTrigger C-Win
ON CHOOSE OF btTrigger IN FRAME DEFAULT-FRAME /* Trigger */
DO:
    DEFINE VARIABLE iProcessCount AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSuccessCount AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cUser         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPassword     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.

    SESSION:SET-WAIT-STATE("GENERAL").
    
    RUN getUserDetails (
        OUTPUT cUser,
        OUTPUT cPassword,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).
    
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.    
    
    RUN ProcessRequests IN hdInboundProcs (
        INPUT-OUTPUT TABLE ttInboundRequest,
        INPUT cUser,
        INPUT cPassword
        ).
        
    FOR EACH ttInboundRequest 
        WHERE ttInboundRequest.processed:
        ASSIGN
            iProcessCount  = iProcessCount + 1
            iSuccessCount  = IF success THEN 
                                 iSuccessCount + 1 
                             ELSE 
                                 iSuccessCount
            .
    END.
    
    SESSION:SET-WAIT-STATE("").
    
    fiMessageText:SCREEN-VALUE = "Total Processed: " + STRING(iProcessCount) 
                               + " " + " Total Success: " 
                               + STRING(iSuccessCount).
        
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}

    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
{methods/template/brwcustom.i}
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fiFileSelector fiMessageText fiRequestDataLabel edRequestData 
          edResponseData fiResponseDataLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 btexit fiFileSelector btOpenFile BROWSE-4 edRequestData 
         edResponseData 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getUserDetails C-Win 
PROCEDURE getUserDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcUser     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPassword AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    FIND FIRST ASI.users NO-LOCK
         WHERE ASI.users.user_id EQ USERID("ASI")
         NO-ERROR.
    IF NOT AVAILABLE ASI.users THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "Authentication Failed"
            .
        
        RETURN.
    END.
    
    /* Checks _user table */
    FIND FIRST ASI._user NO-LOCK
         WHERE ASI._user._Userid EQ ASI.users.user_id NO-ERROR.
    IF NOT AVAILABLE ASI._user THEN DO:
        ASSIGN 
            oplSuccess = NO
            opcMessage = "Authentication Failed"
            .
               
        RETURN.
    END.
	
    ASSIGN
        opcUser     = _user._Userid
        opcPassword = _user._password
        oplSuccess  = TRUE
        opcMessage  = "Success"
        .
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

