&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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

      DEF VAR inImagegood AS CHAR.
DEF VAR OKpressedgood AS LOGICAL.
DEF VAR inPy AS CHAR.
DEF VAR inNo AS CHAR.
DEF VAR OKpressedbad AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bselWin bPlayGood bselPy bPlayPython bselNo ~
bPlayNode 
&Scoped-Define DISPLAYED-OBJECTS fiWindows fiPython fiNode EDITOR-1 ~
EDITOR-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bPlayGood 
     LABEL "Play with windows" 
     SIZE 23 BY 1.14.

DEFINE BUTTON bPlayNode 
     LABEL "Play with Node" 
     SIZE 22 BY 1.14.

DEFINE BUTTON bPlayPython 
     LABEL "Play with Python" 
     SIZE 22 BY 1.14.

DEFINE BUTTON bselNo 
     LABEL "Select" 
     SIZE 8.4 BY 1.

DEFINE BUTTON bselPy 
     LABEL "Select" 
     SIZE 8.4 BY 1.

DEFINE BUTTON bselWin 
     LABEL "Select" 
     SIZE 8.4 BY 1.

DEFINE VARIABLE EDITOR-1 AS CHARACTER INITIAL "Python version : 3.0  Need to install Python Libray: ~"playsound~"                    Command to be execute on CLI : pip install playsound" 
     VIEW-AS EDITOR
     SIZE 79 BY 1.91 NO-UNDO.

DEFINE VARIABLE EDITOR-2 AS CHARACTER INITIAL "Node version: 15.8.0 , Need to install node module: ~"sound-play~"                 Command to be execute on CLI: npm install sound-play" 
     VIEW-AS EDITOR
     SIZE 79 BY 1.91 NO-UNDO.

DEFINE VARIABLE fiNode AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\Asigui~\Repository~\Source~\util~\done.mp3" 
     LABEL "Sound" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 70 BY 1 NO-UNDO.

DEFINE VARIABLE fiPython AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\Asigui~\Repository~\Source~\util~\done.mp3" 
     LABEL "Sound" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 70 BY 1 NO-UNDO.

DEFINE VARIABLE fiWindows AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\Asigui~\Repository~\Source~\util~\done.wav" 
     LABEL "Sound" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 70 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiWindows AT ROW 2.19 COL 17 COLON-ALIGNED WIDGET-ID 2
     bselWin AT ROW 2.24 COL 89.6 WIDGET-ID 4
     bPlayGood AT ROW 3.38 COL 75 WIDGET-ID 6
     fiPython AT ROW 5.05 COL 17 COLON-ALIGNED WIDGET-ID 12
     bselPy AT ROW 5.1 COL 89.6 WIDGET-ID 10
     bPlayPython AT ROW 6.24 COL 76 WIDGET-ID 8
     fiNode AT ROW 7.91 COL 17 COLON-ALIGNED WIDGET-ID 18
     bselNo AT ROW 7.95 COL 89.6 WIDGET-ID 16
     bPlayNode AT ROW 9.1 COL 76 WIDGET-ID 14
     EDITOR-1 AT ROW 11.48 COL 9 NO-LABEL WIDGET-ID 28
     EDITOR-2 AT ROW 14.38 COL 9 NO-LABEL WIDGET-ID 30
     "**Node" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 13.67 COL 9.2 WIDGET-ID 34
          FGCOLOR 12 
     "**Python" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 10.76 COL 10 WIDGET-ID 32
          FGCOLOR 12 
     "*wav" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 3.62 COL 68 WIDGET-ID 20
     "*wav, mp3" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 6.52 COL 64 WIDGET-ID 22
     "*wav, mp3, flac" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 9.33 COL 58 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.8 BY 16.48 WIDGET-ID 100.


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
         TITLE              = "Play Sound"
         HEIGHT             = 16.48
         WIDTH              = 102.8
         MAX-HEIGHT         = 16.48
         MAX-WIDTH          = 106.6
         VIRTUAL-HEIGHT     = 16.48
         VIRTUAL-WIDTH      = 106.6
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR EDITOR EDITOR-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       EDITOR-1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       EDITOR-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiNode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPython IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiWindows IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Play Sound */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Play Sound */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bPlayGood
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bPlayGood C-Win
ON CHOOSE OF bPlayGood IN FRAME DEFAULT-FRAME /* Play with windows */
DO:

  IF fiWindows:screen-value = "" THEN
  DO:
    MESSAGE "Please select any file to play."
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      RETURN NO-APPLY.
  END.
  
  DEF VAR play-status AS INTEGER.
  RUN sndPlaySoundA (
        INPUT fiWindows:screen-value, 
        INPUT 2,
        OUTPUT play-status
        ).

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bPlayNode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bPlayNode C-Win
ON CHOOSE OF bPlayNode IN FRAME DEFAULT-FRAME /* Play with Node */
DO:
  IF fiNode:screen-value = "" THEN
  DO:
    MESSAGE "Please select any file to play."
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      RETURN NO-APPLY.
  END.
  
  DEFINE  VARIABLE  cRunNodeScript AS CHARACTER .   
  DEFINE  VARIABLE  cNodeFile AS CHARACTER .

  cNodeFile = SEARCH("util\playsoundnode.js").
  cRunNodeScript  = "node " + cNodeFile + " " + fiNode:screen-value.
  OS-COMMAND SILENT VALUE(cRunNodeScript)  .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bPlayPython
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bPlayPython C-Win
ON CHOOSE OF bPlayPython IN FRAME DEFAULT-FRAME /* Play with Python */
DO:
  IF fiPython:screen-value = "" THEN
  DO:
    MESSAGE "Please select any file to play."
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      RETURN NO-APPLY.
  END.
  
  DEFINE  VARIABLE  cRunPyScript AS CHARACTER .   
  DEFINE  VARIABLE  cPyFile AS CHARACTER .

 cPyFile = SEARCH("util\sound.py") .
 cRunPyScript  = "python " + cPyFile + " " + fiPython:screen-value.
  OS-COMMAND SILENT VALUE(cRunPyScript)  .
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bselNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bselNo C-Win
ON CHOOSE OF bselNo IN FRAME DEFAULT-FRAME /* Select */
DO:
    SYSTEM-DIALOG GET-FILE inNo
    TITLE   "Choose File to Play ..."
    FILTERS "Source Files (*)"   "*"
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressedbad.
    fiNode:screen-value =   inNo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bselPy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bselPy C-Win
ON CHOOSE OF bselPy IN FRAME DEFAULT-FRAME /* Select */
DO:
    SYSTEM-DIALOG GET-FILE inPy
    TITLE   "Choose File to Play ..."
    FILTERS "Source Files (*)"   "*"
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressedbad.
    fiPython:screen-value =   inPy.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bselWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bselWin C-Win
ON CHOOSE OF bselWin IN FRAME DEFAULT-FRAME /* Select */
DO:
      
  SYSTEM-DIALOG GET-FILE inImagegood
    TITLE   "Choose File to Play ..."
    FILTERS "Source Files (*)"   "*"
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressedgood.
    
    fiWindows:screen-value =   inImagegood.
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
  
    PROCEDURE sndPlaySoundA EXTERNAL "winmm.dll":
        DEFINE INPUT  PARAMETER ic  AS CHARACTER.
        DEFINE INPUT  PARAMETER ish AS LONG.
        DEFINE OUTPUT PARAMETER osh AS LONG.
    END PROCEDURE.
    
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
  DISPLAY fiWindows fiPython fiNode EDITOR-1 EDITOR-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE bselWin bPlayGood bselPy bPlayPython bselNo bPlayNode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

