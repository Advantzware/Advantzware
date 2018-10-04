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

DEF TEMP-TABLE tt-allLocks
FIELD allFileName AS CHAR FORMAT "x(20)" COLUMN-LABEL "Table"
FIELD allTableNumber AS INT COLUMN-LABEL ""
FIELD allUserNum AS INT COLUMN-LABEL ""
FIELD allUser AS  CHAR FORMAT "x(12)"  COLUMN-LABEL "User"
FIELD allType AS CHAR COLUMN-LABEL "Lock Type"
FIELD allFlags AS CHAR COLUMN-LABEL "Flags"
FIELD allCnt AS INT COLUMN-LABEL "Count".

ASSIGN
  cocode = g_company
  locode = g_loc.

DEFINE VARIABLE labelLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE dataLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIsTrans AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIsLocks AS LOGICAL NO-UNDO.
DEFINE STREAM monitorStrm.

IF INDEX(PROPATH,".\custom") EQ 0 THEN
PROPATH = ".\custom," + PROPATH.



{methods/lockWindowUpdate.i}

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
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 btnViewLog btnClose edTrans ~
edLocks 
&Scoped-Define DISPLAYED-OBJECTS edTrans edLocks 

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
DEFINE BUTTON btnClose 
     LABEL "&Close" 
     SIZE 15 BY 1.43.

DEFINE BUTTON btnViewLog 
     LABEL "&View / Refresh Locks" 
     SIZE 23 BY 1.43.

DEFINE VARIABLE edLocks AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 62 BY 5 NO-UNDO.

DEFINE VARIABLE edTrans AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 62 BY 5 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 65 BY 5.95.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 65 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnViewLog AT ROW 1.48 COL 5 WIDGET-ID 6
     btnClose AT ROW 1.48 COL 47.8 HELP
          "Close TCP/IP Server Process"
     edTrans AT ROW 4.57 COL 4 NO-LABEL WIDGET-ID 16
     edLocks AT ROW 11.95 COL 4 NO-LABEL WIDGET-ID 18
     "Transaction When Red" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 3.71 COL 4.2 WIDGET-ID 20
          FGCOLOR 9 
     "Locking Records When Red" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 10.91 COL 4.2 WIDGET-ID 22
          FGCOLOR 9 
     RECT-1 AT ROW 4.1 COL 3 WIDGET-ID 24
     RECT-2 AT ROW 11.24 COL 3 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72 BY 17.62.


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
         TITLE              = "Transaction Monitor"
         HEIGHT             = 17.62
         WIDTH              = 72
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

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
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
       ROW             = 3.38
       COLUMN          = 43
       HEIGHT          = 4.76
       WIDTH           = 20
       WIDGET-ID       = 4
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(btnClose:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Transaction Monitor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Transaction Monitor */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME DEFAULT-FRAME /* Close */
DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnViewLog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnViewLog C-Win
ON CHOOSE OF btnViewLog IN FRAME DEFAULT-FRAME /* View / Refresh Locks */
DO:
  DEFINE VARIABLE cLockDisplayLine AS CHARACTER NO-UNDO.
  edLocks:SCREEN-VALUE = "".
  FOR EACH tt-AllLocks:
      cLockDisplayLine = 
         tt-AllLocks.allFileName + " Type: " + tt-AllLocks.allType + " Cnt: " + STRING(tt-AllLocks.allCnt)
         + chr(13)
         . 
      edLocks:SCREEN-VALUE = edLocks:SCREEN-VALUE + cLockDisplayLine.

  END.
  RETURN NO-APPLY.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
DO:
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.
  RUN monitorActivity ('Transaction Monitor Stopped',YES,'').

  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  /* RUN winReSize. */

  RUN monitorActivity ('Transaction Monitor Started',YES,'').
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY edTrans edLocks 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 btnViewLog btnClose edTrans edLocks 
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

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postMonitor C-Win 
PROCEDURE postMonitor :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&frame-name}:
    RUN system/checkTrans.p (OUTPUT lIsTrans).

    ENABLE edTrans edLocks.
    edTrans:SENSITIVE = TRUE.
    IF lIsTrans THEN 
      ASSIGN edTrans:BGCOLOR = 12  
             edTrans:SCREEN-VALUE = "In a Transaction".
    ELSE
      ASSIGN edTrans:BGCOLOR = 10
             edTrans:SCREEN-VALUE = "None".
    RUN system/checkLocks.p (INPUT TABLE tt-allLocks BY-REFERENCE, OUTPUT lIsLocks).
    edLocks:SENSITIVE = TRUE.
    IF lIsLocks THEN 
      ASSIGN edLocks:BGCOLOR = 12
             edLocks:SCREEN-VALUE = "Records Locked".
    ELSE DO: 
      ASSIGN
        edLocks:BGCOLOR = 10
        edLocks:SCREEN-VALUE = "None"
      .
    END.
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
  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
  ASSIGN
    {&WINDOW-NAME}:WINDOW-STATE = 1
    {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - 30
    {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
.
  RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

