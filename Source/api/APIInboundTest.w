&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: api/APIInboundTest.w

  Description: Utility to test the API Inbound calls

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Porandla Mithun

  Created: 10/16/2019

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
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cRequestURLModified AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-19 RECT-20 btSend cbRequestVerb ~
cbRequestDataType fiRequestURL fiUserName fiPassword edRequestData ~
edResponseData 
&Scoped-Define DISPLAYED-OBJECTS cbRequestVerb cbRequestDataType ~
fiRequestURL fiUserName fiPassword fiResponseTime edRequestData ~
edResponseData 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btSend 
     IMAGE-UP FILE "Graphics/32x32/check.ico":U
     LABEL "Send" 
     SIZE 17.8 BY 3.

DEFINE VARIABLE cbRequestDataType AS CHARACTER FORMAT "X(256)":U INITIAL "JSON" 
     LABEL "Request Datatype" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "JSON","XML" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cbRequestVerb AS CHARACTER FORMAT "X(256)":U INITIAL "POST" 
     LABEL "Request Verb" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "POST","GET" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE edRequestData AS LONGCHAR 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 157 BY 8.24 NO-UNDO.

DEFINE VARIABLE edResponseData AS LONGCHAR 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 157 BY 12.62 NO-UNDO.

DEFINE VARIABLE fiPassword AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 33.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiRequestURL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Request URL" 
     VIEW-AS FILL-IN 
     SIZE 118.8 BY 1 NO-UNDO.

DEFINE VARIABLE fiResponseTime AS CHARACTER FORMAT "X(256)":U 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fiUserName AS CHARACTER FORMAT "X(256)":U 
     LABEL "User Name" 
     VIEW-AS FILL-IN 
     SIZE 26.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 137.2 BY 3.1.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 115 BY 1.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btSend AT ROW 1.52 COL 142 WIDGET-ID 24
     cbRequestVerb AT ROW 1.81 COL 18.4 COLON-ALIGNED WIDGET-ID 2
     cbRequestDataType AT ROW 1.81 COL 59 COLON-ALIGNED WIDGET-ID 4
     fiRequestURL AT ROW 3.1 COL 18.2 COLON-ALIGNED WIDGET-ID 6
     fiUserName AT ROW 5.05 COL 17.6 COLON-ALIGNED WIDGET-ID 10
     fiPassword AT ROW 5.05 COL 61.6 COLON-ALIGNED WIDGET-ID 12 PASSWORD-FIELD 
     fiResponseTime AT ROW 5.1 COL 126 COLON-ALIGNED WIDGET-ID 26
     edRequestData AT ROW 7.29 COL 3 NO-LABEL WIDGET-ID 16
     edResponseData AT ROW 16.52 COL 3 NO-LABEL WIDGET-ID 20
     "Response:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.76 COL 3.2 WIDGET-ID 22
     "Request Body:" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 6.52 COL 3.4 WIDGET-ID 18
     RECT-19 AT ROW 1.48 COL 2.8 WIDGET-ID 8
     RECT-20 AT ROW 4.71 COL 2.8 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.48
         BGCOLOR 15 FGCOLOR 1 FONT 6 WIDGET-ID 100.


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
         TITLE              = "API Inbound Tester"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 31.38
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 31.38
         VIRTUAL-WIDTH      = 160
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
ASSIGN 
       edResponseData:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiResponseTime IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* API Inbound Tester */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* API Inbound Tester */
DO:
    RUN setUserPrint.
    
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSend C-Win
ON CHOOSE OF btSend IN FRAME DEFAULT-FRAME /* Send */
DO:
    DEFINE VARIABLE cCommand       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRequestFile   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponseFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcRequestData  AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcResponseData AS LONGCHAR  NO-UNDO.    
    DEFINE VARIABLE iTimeElapsed   AS INT64     NO-UNDO.
    
    IF fiRequestURL:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "Request URL cannot be empty"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.     

    IF fiUserName:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "User Name cannot be empty"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.     

    IF fiPassword:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "User Name cannot be empty"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.     

    IF SEARCH("curl.exe") EQ ? THEN DO:
        MESSAGE "curl not found!"
            VIEW-AS ALERT-BOX ERROR.
        RETURN. 
    END.    

    ASSIGN
        cRequestFile  = "C:\Tmp\request"
        cResponseFile = "C:\Tmp\response"
        .
            
    cCommand = SEARCH("curl.exe") + ' --user '
              + fiUserName:SCREEN-VALUE + ':' + fiPassword:SCREEN-VALUE + ' '
              + '-H "Content-Type: application/' +  lc(cbRequestDataType:SCREEN-VALUE + '"') /* handles XML or JSON only - not RAW */
              + (IF cbRequestVerb:SCREEN-VALUE NE 'GET' THEN ' -d "@' + cRequestFile + '" ' ELSE '')
              + (IF cbRequestVerb:SCREEN-VALUE NE 'GET' THEN ' -X ' + cbRequestVerb:SCREEN-VALUE ELSE '')  + ' '
              + fiRequestURL:SCREEN-VALUE
              + ' > ' + cResponseFile.    
    
    lcRequestData = edRequestData:SCREEN-VALUE.
    
    /* Put Request Data from a variable into a Temporary file */
    COPY-LOB lcRequestData TO FILE cRequestFile.
    
    SESSION:SET-WAIT-STATE("GENERAL").
    
    ETIME(YES).
    
    /* execute CURL command with required parameters to call the API */
    DOS SILENT VALUE(cCommand).

    iTimeElapsed = ETIME.
    
    SESSION:SET-WAIT-STATE("").    
        
    fiResponseTime:SCREEN-VALUE = IF iTimeElapsed GT 999 THEN
                                      (STRING(TRUNCATE(iTimeElapsed / 1000, 0)) + "s" + " " + STRING(INT(iTimeElapsed MOD 1000)) + "ms")
                                  ELSE
                                      STRING(iTimeElapsed) + "ms"
                                  .
    
    /* Put Response Data from Temporary file into a variable */
    COPY-LOB FILE cResponseFile TO lcResponseData. 
    
    edResponseData:SCREEN-VALUE = IF STRING(lcResponseData) NE "" THEN
                                      STRING(lcResponseData)
                                  ELSE
                                      "Could not get any response".
    
    OS-DELETE VALUE(cRequestFile).
    OS-DELETE VALUE(cResponseFile).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiRequestURL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRequestURL C-Win
ON ENTRY OF fiRequestURL IN FRAME DEFAULT-FRAME /* Request URL */
DO:
    cRequestURLModified = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRequestURL C-Win
ON LEAVE OF fiRequestURL IN FRAME DEFAULT-FRAME /* Request URL */
DO:
    IF cRequestURLModified NE SELF:SCREEN-VALUE THEN
        RUN pUpdateRequestData (
            INPUT SELF:SCREEN-VALUE
            ) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

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
  RUN pInit.
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
  DISPLAY cbRequestVerb cbRequestDataType fiRequestURL fiUserName fiPassword 
          fiResponseTime edRequestData edResponseData 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-19 RECT-20 btSend cbRequestVerb cbRequestDataType fiRequestURL 
         fiUserName fiPassword edRequestData edResponseData 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit C-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    {custom/usrprint.i}
    
    APPLY "LEAVE" TO fiRequestURL.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRequestData C-Win 
PROCEDURE pUpdateRequestData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcAPIRoute AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cAPIRoute AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    cAPIRoute = "/api/" 
              + ENTRY(NUM-ENTRIES(ipcAPIRoute,"/"), ipcAPIRoute, "/").
              
    FIND FIRST APIInbound NO-LOCK
         WHERE APIInbound.apiRoute EQ cAPIRoute
         NO-ERROR.
    IF AVAILABLE APIInbound THEN
        edRequestData:SCREEN-VALUE = STRING(APIInbound.requestData).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setUserPrint C-Win 
PROCEDURE setUserPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN custom/setUserPrint.p (
        INPUT g_company,
        INPUT "APIInboundTest.",
        INPUT "cbRequestVerb,cbRequestDataType,fiRequestURL,fiUserName,fiPassword",
        INPUT cbRequestVerb:SCREEN-VALUE + ',' + 
              cbRequestDataType:SCREEN-VALUE + ',' + 
              fiRequestURL:SCREEN-VALUE + ',' +
              fiUserName:SCREEN-VALUE + ',' +
              fiPassword:SCREEN-VALUE
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

