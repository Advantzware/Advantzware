&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: monitor.w

  Description: Generic Monitor

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 7.5.2014

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}
{sys/inc/var.i "new shared"}

ASSIGN
  cocode = g_company
  locode = g_loc.

DEFINE VARIABLE labelLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE dataLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE hPgmSecurity AS HANDLE NO-UNDO.

DEFINE STREAM monitorStrm.

IF INDEX(PROPATH,".\custom") EQ 0 THEN
PROPATH = ".\custom," + PROPATH.

&IF '{1}' NE '' &THEN
{{1}/{1}Defs.i}
&ENDIF

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
   {methods/lockWindowUpdate.i}
&ENDIF

SESSION:SET-WAIT-STATE('').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnClearLog btnViewLog btnClose ~
monitorActivity 
&Scoped-Define DISPLAYED-OBJECTS monitorImportDir monitorActivity 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClearLog 
     LABEL "Clear/Archive &Log" 
     SIZE 20 BY 1.43.

DEFINE BUTTON btnClose 
     LABEL "&Close" 
     SIZE 15 BY 1.43.

DEFINE BUTTON btnViewLog 
     LABEL "&View Log" 
     SIZE 15 BY 1.43.

DEFINE VARIABLE monitorActivity AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 168 BY 25
     BGCOLOR 15 FGCOLOR 1 FONT 2 NO-UNDO.

DEFINE VARIABLE monitorImportDir AS CHARACTER FORMAT "X(256)":U INITIAL "./cXML" 
     LABEL "Monitoring Directory" 
     VIEW-AS FILL-IN 
     SIZE 89 BY 1
     BGCOLOR 14  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     monitorImportDir AT ROW 1 COL 20 COLON-ALIGNED WIDGET-ID 2
     btnClearLog AT ROW 1.24 COL 117 HELP
          "Clear Current Log"
     btnViewLog AT ROW 1.24 COL 138 WIDGET-ID 6
     btnClose AT ROW 1.24 COL 154 HELP
          "Close TCP/IP Server Process"
     monitorActivity AT ROW 2.91 COL 1 HELP
          "Use Arrows, Page Up/Down, Home & End to Scroll Log Screen" NO-LABEL
     "Date                   Time                   Activity" VIEW-AS TEXT
          SIZE 48 BY .62 AT ROW 2.19 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 168.4 BY 27.05.


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
         TITLE              = "{1} Monitor"
         HEIGHT             = 27.05
         WIDTH              = 168.4
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       monitorActivity:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN monitorImportDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1
       COLUMN          = 90
       HEIGHT          = 4.76
       WIDTH           = 20
       WIDGET-ID       = 4
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(monitorImportDir:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* {1} Monitor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
       RUN system/userLogOut.p.
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* {1} Monitor */
DO:
       RUN system/userLogOut.p.
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearLog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearLog C-Win
ON CHOOSE OF btnClearLog IN FRAME DEFAULT-FRAME /* Clear/Archive Log */
DO:
  OS-COPY VALUE(monitorImportDir + '/monitor/monitor.log')
          VALUE(monitorImportDir + '/monitor/archived/monitor.' +
                STRING(TODAY,'99999999') + '.' + STRING(TIME,'99999') +
                '.log').
  monitorActivity:SAVE-FILE(monitorImportDir + '/monitor/monitor.log').
  monitorActivity:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME DEFAULT-FRAME /* Close */
DO:
       RUN system/userLogOut.p.
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnViewLog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnViewLog C-Win
ON CHOOSE OF btnViewLog IN FRAME DEFAULT-FRAME /* View Log */
DO:
  OS-COMMAND NO-WAIT notepad.exe VALUE(monitorImportDir + '/monitor/monitor.log').
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
  RUN postMonitor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.    
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
  RUN monitorActivity ('{1} Monitor Stopped',YES,'').
  monitorActivity:SAVE-FILE(monitorImportDir + '/monitor/monitor.tmp.log') IN FRAME {&FRAME-NAME}.
  OS-APPEND VALUE(monitorImportDir + '/monitor/monitor.tmp.log')
            VALUE(monitorImportDir + '/monitor/monitor.log').
       RUN system/userLogOut.p.
  RUN disable_UI.
    DELETE OBJECT hPgmSecurity.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN winReSize.
  &IF '{1}' NE 'cXML' &THEN
    FIND FIRST sys-ctrl NO-LOCK
         WHERE sys-ctrl.company EQ g_company
           AND sys-ctrl.name EQ '{2}' NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN DO:
      MESSAGE 'System Parameter {2} Does Not Exist' SKIP
        'Create System Parameter?'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE monitorSysCtrl AS LOGICAL.
      IF monitorSysCtrl THEN DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
          sys-ctrl.company = g_company
          sys-ctrl.name = '{2}'
          sys-ctrl.descrip = '{1} Location of Files to Import'
          sys-ctrl.log-fld = NO.
      END. /* monitorsysctrl */
      ELSE DO:
        RUN system/userLogOut.p.
        QUIT.
      END.
    END. /* not avail sys-ctrl */
    IF NOT sys-ctrl.log-fld THEN DO:
      MESSAGE '{1} Monitor Session Cannot Be Initiated' SKIP
        'System Parameter {2} is set to NO' SKIP
        VIEW-AS ALERT-BOX WARNING.
       RUN system/userLogOut.p.
      QUIT.
    END. /* not log-fld */
    IF sys-ctrl.char-fld EQ '' THEN DO:
      MESSAGE '{1} Monitor Session Cannot Be Initiated' SKIP
        'System Parameter {2} File Location Is Blank' SKIP
        VIEW-AS ALERT-BOX WARNING.
       RUN system/userLogOut.p.
      QUIT.
    END. /* char-fld eq '' */
    ASSIGN
      monitorImportDir:SCREEN-VALUE = sys-ctrl.char-fld
      monitorImportDir.
  &ENDIF
  RUN monitorActivity ('{1} Monitor Started',YES,'').
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "monitor.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "monitor.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  RUN control_load.
  DISPLAY monitorImportDir monitorActivity 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnClearLog btnViewLog btnClose monitorActivity 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE monitorActivity C-Win 
PROCEDURE monitorActivity :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipActivity AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipDateTimeStamp AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipmonitorFile AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF ipDateTimeStamp THEN
    monitorActivity:INSERT-STRING(STRING(TODAY,'99/99/9999') + ' ' + 
                                  STRING(TIME,'HH:MM:SS am') + ' ').
    ELSE monitorActivity:INSERT-STRING(FILL(' ',23)).

    monitorActivity:INSERT-STRING(ipActivity +
                   (IF ipmonitorFile NE '' THEN ' [File: ' + ipmonitorFile + ']'
                    ELSE '') + CHR(10)).
    
    IF LENGTH(monitorActivity:SCREEN-VALUE) GT 20000 THEN
    APPLY 'CHOOSE':U TO btnClearLog.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE winReSize C-Win 
PROCEDURE winReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF
  ASSIGN
    {&WINDOW-NAME}:WINDOW-STATE = 1
    {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - 30
    {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    monitorActivity:WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS - 2
    monitorActivity:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 45.
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (0,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = FALSE.
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

