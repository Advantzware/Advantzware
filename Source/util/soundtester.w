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

DEFINE  VARIABLE  inWinSoundPath AS CHARACTER NO-UNDO.
DEFINE  VARIABLE  OKpressedgood AS LOGICAL NO-UNDO.
DEFINE  VARIABLE  inPySoundPath AS CHARACTER NO-UNDO.
DEFINE  VARIABLE  inNoSoundPath AS CHARACTER NO-UNDO.
DEFINE  VARIABLE  OKpressedbad AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bselWin bPlayGood bPlayPython bPlayNode 
&Scoped-Define DISPLAYED-OBJECTS fiSound EDITOR-1 EDITOR-2 

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
     SIZE 23 BY 1.14.

DEFINE BUTTON bPlayPython 
     LABEL "Play with Python" 
     SIZE 23 BY 1.14.

DEFINE BUTTON bselWin 
     LABEL "Select" 
     SIZE 8.4 BY 1.

DEFINE VARIABLE EDITOR-1 AS CHARACTER INITIAL "Python version : 3.0  Need to install Python Libray: ~"playsound~"                    Command to be execute on CLI : pip install playsound" 
     VIEW-AS EDITOR
     SIZE 79 BY 1.91 NO-UNDO.

DEFINE VARIABLE EDITOR-2 AS CHARACTER INITIAL "Node version: 15.8.0 , Need to install node module: ~"sound-play~"                 Command to be execute on CLI: npm install sound-play" 
     VIEW-AS EDITOR
     SIZE 79 BY 1.91 NO-UNDO.

DEFINE VARIABLE fiSound AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\Asigui~\Repository~\Source~\util~\done.wav" 
     LABEL "Sound" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 70 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiSound AT ROW 2.19 COL 17 COLON-ALIGNED WIDGET-ID 2
     bselWin AT ROW 2.24 COL 89.6 WIDGET-ID 4
     bPlayGood AT ROW 3.38 COL 75 WIDGET-ID 6
     bPlayPython AT ROW 4.76 COL 75.2 WIDGET-ID 8
     bPlayNode AT ROW 6.19 COL 75.4 WIDGET-ID 14
     EDITOR-1 AT ROW 7.95 COL 9 NO-LABEL WIDGET-ID 28
     EDITOR-2 AT ROW 10.86 COL 9 NO-LABEL WIDGET-ID 30
     "*wav, mp3, flac" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 6.43 COL 58 WIDGET-ID 26
     "*wav, mp3" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 5.05 COL 63.4 WIDGET-ID 22
     "*wav" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 3.62 COL 68 WIDGET-ID 20
     "**Python" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 7.24 COL 10 WIDGET-ID 32
          FGCOLOR 12 
     "**Node" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 10.14 COL 9.2 WIDGET-ID 34
          FGCOLOR 12 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.8 BY 12.48 WIDGET-ID 100.


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
         HEIGHT             = 12.48
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

/* SETTINGS FOR FILL-IN fiSound IN FRAME DEFAULT-FRAME
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
  DEFINE VARIABLE oplcPlayStatus AS INTEGER.
  
  RUN OS_PlaySound (INPUT fiSound:screen-value, 
                    OUTPUT oplcPlayStatus
                    ).

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bPlayNode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bPlayNode C-Win
ON CHOOSE OF bPlayNode IN FRAME DEFAULT-FRAME /* Play with Node */
DO:
  IF fiSound:screen-value = "" THEN
  DO:
    MESSAGE "Please select any file to play."
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      RETURN NO-APPLY.
  END.
  
  DEFINE VARIABLE cRunNodeScript AS CHARACTER NO-UNDO.   
  DEFINE VARIABLE cNodeFile      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lSuccess       AS CHARACTER NO-UNDO.   
  DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.

  cNodeFile = SEARCH("util\playsoundnode.js").
  
  cRunNodeScript  = "node " + cNodeFile + " " + fiSound:screen-value.
  
  RUN OS_RunCommand(INPUT  cRunNodeScript,
                    INPUT  "",  // Output File
                    INPUT  YES, // Silent
                    INPUT  NO,  // No wait
                    OUTPUT lSuccess ,
                    OUTPUT cMessage).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bPlayPython
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bPlayPython C-Win
ON CHOOSE OF bPlayPython IN FRAME DEFAULT-FRAME /* Play with Python */
DO:
    IF fiSound:SCREEN-VALUE = "" THEN
    DO:
        MESSAGE "Please select any file to play."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN NO-APPLY.
    END.
  
    DEFINE VARIABLE cRunPyScript AS CHARACTER NO-UNDO.   
    DEFINE VARIABLE cPyFile      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess     AS CHARACTER NO-UNDO.   
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
 
    cPyFile = SEARCH("util\sound.py") .
    
    cRunPyScript  = "python " + cPyFile + " " + fiSound:screen-value.
 
    RUN OS_RunCommand(INPUT  cRunPyScript,
                      INPUT  "",  // Output File
                      INPUT  YES, // Silent
                      INPUT  NO,  // No wait
                      OUTPUT lSuccess,
                      OUTPUT cMessage).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bselWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bselWin C-Win
ON CHOOSE OF bselWin IN FRAME DEFAULT-FRAME /* Select */
DO:
      
  SYSTEM-DIALOG GET-FILE inWinSoundPath
    TITLE   "Choose File to Play ..."
    FILTERS "Source Files (*)"   "*"
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressedgood.
    
    fiSound:screen-value =   inWinSoundPath.
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
  DISPLAY fiSound EDITOR-1 EDITOR-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE bselWin bPlayGood bPlayPython bPlayNode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

