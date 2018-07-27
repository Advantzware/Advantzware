&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wAbout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wAbout 
/*------------------------------------------------------------------------

  File : wAbout.p
  Desc : Shows the 'About' info of DD

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{ DataDigger.i }

/* For debugging in the UIB */
&IF DEFINED(UIB_is_Running) <> 0 &THEN
  RUN startDiggerLib.p.
&ENDIF

DEFINE VARIABLE glIntroRunning AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnDataDigger imgPlayer imgBall rcCord BtnOK ~
edChangelog fiWebsite 
&Scoped-Define DISPLAYED-OBJECTS edChangelog fiDataDigger-1 fiDataDigger-2 ~
fiWebsite 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wAbout AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDataDigger  NO-FOCUS FLAT-BUTTON
     LABEL "D" 
     SIZE 6.4 BY 1.52.

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE VARIABLE edChangelog AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-VERTICAL LARGE
     SIZE-PIXELS 620 BY 4
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiDataDigger-1 AS CHARACTER FORMAT "X(256)":U INITIAL "DataDigger ~{&&version} - ~{&&edition}" 
      VIEW-AS TEXT 
     SIZE-PIXELS 275 BY 13
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiDataDigger-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Build ~{&&build}" 
      VIEW-AS TEXT 
     SIZE-PIXELS 155 BY 13
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiWebsite AS CHARACTER FORMAT "X(256)":U INITIAL "https://datadigger.wordpress.com/" 
      VIEW-AS TEXT 
     SIZE-PIXELS 210 BY 20
     FGCOLOR 9  NO-UNDO.

DEFINE IMAGE imgBall
     FILENAME "adeicon/blank":U
     STRETCH-TO-FIT RETAIN-SHAPE TRANSPARENT
     SIZE 2 BY .48.

DEFINE IMAGE imgPlayer CONVERT-3D-COLORS
     STRETCH-TO-FIT RETAIN-SHAPE
     SIZE 5.6 BY 1.33.

DEFINE RECTANGLE rcCord
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE .4 BY 1.43
     BGCOLOR 12 FGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnDataDigger AT ROW 1.71 COL 3 WIDGET-ID 324
     BtnOK AT Y 10 X 540 WIDGET-ID 48
     edChangelog AT Y 60 X 5 NO-LABEL WIDGET-ID 72
     fiDataDigger-1 AT Y 15 X 40 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     fiDataDigger-2 AT Y 32 X 40 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     fiWebsite AT Y 425 X 190 COLON-ALIGNED NO-LABEL WIDGET-ID 298
     imgPlayer AT ROW 2.43 COL 59 WIDGET-ID 328
     imgBall AT ROW 5.43 COL 59.8 WIDGET-ID 330
     rcCord AT ROW 4.05 COL 60.6 WIDGET-ID 332
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 126.4 BY 21.71 WIDGET-ID 100.


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
  CREATE WINDOW wAbout ASSIGN
         HIDDEN             = YES
         TITLE              = "About the DataDigger"
         HEIGHT             = 21.71
         WIDTH              = 126.4
         MAX-HEIGHT         = 54
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 54
         VIRTUAL-WIDTH      = 384
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
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
/* SETTINGS FOR WINDOW wAbout
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

ASSIGN 
       edChangelog:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiDataDigger-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDataDigger-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wAbout)
THEN wAbout:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wAbout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wAbout wAbout
ON END-ERROR OF wAbout /* About the DataDigger */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:

  APPLY 'CLOSE' TO THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wAbout wAbout
ON WINDOW-CLOSE OF wAbout /* About the DataDigger */
DO:
  IF glIntroRunning THEN RETURN NO-APPLY.

  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME wAbout
ON F12 OF FRAME DEFAULT-FRAME
ANYWHERE DO:

  RUN gameOver(YES).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDataDigger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDataDigger wAbout
ON CHOOSE OF btnDataDigger IN FRAME DEFAULT-FRAME /* D */
DO:
  RUN SokoDigger.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK wAbout
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiWebsite
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiWebsite wAbout
ON MOUSE-SELECT-CLICK OF fiWebsite IN FRAME DEFAULT-FRAME
DO:

  OS-COMMAND NO-WAIT START VALUE(SELF:SCREEN-VALUE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wAbout 


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

  RUN initializeUi.
  RUN initializeObject.

  WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS edChangelog.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE blinkLogo wAbout 
PROCEDURE blinkLogo :
/* Blink the DD logo
  */
  DEFINE VARIABLE ii   AS INTEGER NO-UNDO.
  DEFINE VARIABLE xx   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE yy   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dx   AS DECIMAL NO-UNDO INITIAL -5. /* hor speed */
  DEFINE VARIABLE dy   AS DECIMAL NO-UNDO INITIAL 0.  /* ver speed */
  DEFINE VARIABLE grav AS DECIMAL NO-UNDO INITIAL .2. /* gravity acceleration */

  DO WITH FRAME {&FRAME-NAME}:

    glIntroRunning = TRUE.
    BtnOK:VISIBLE = FALSE.
    fiDataDigger-1:VISIBLE = FALSE.
    fiDataDigger-2:VISIBLE = FALSE.
    fiWebsite:VISIBLE = FALSE.
    btnDataDigger:MOVE-TO-TOP().
    btnDataDigger:X = 600.
    btnDataDigger:Y = 0.
    btnDataDigger:VISIBLE = FALSE.

    yy = btnDataDigger:Y.
    xx = btnDataDigger:X.

    RUN justWait(100).
    btnDataDigger:VISIBLE = TRUE.
  END.

  #Bounce:
  REPEAT:
    /* Normal flow */
    dy = dy + grav.
    xx = xx + dx.
    yy = yy + dy.

    /* Bounce at bottom of frame */
    IF yy > (FRAME {&FRAME-NAME}:HEIGHT-PIXELS - btnDataDigger:HEIGHT-PIXELS) THEN
    DO:
      yy = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - btnDataDigger:HEIGHT-PIXELS.
      dy = -1 * dy.
    END.
    IF xx <= 5 THEN LEAVE #Bounce.

    btnDataDigger:X = xx.
    btnDataDigger:Y = yy.

    btnDataDigger:MOVE-TO-TOP().
    RUN justWait(8).
  END.

  btnDataDigger:X = 10.
  btnDataDigger:Y = 15.

  DO ii = 1 TO 2:
    btnDataDigger:SENSITIVE = NO.
    RUN justWait(250).
    btnDataDigger:SENSITIVE = YES.
    RUN justWait(250).
  END.
  fiDataDigger-1:VISIBLE = TRUE.
  fiDataDigger-2:VISIBLE = TRUE.

  RUN justWait(500).

  /* player jumps */
  DO ii = 30 TO 425 BY 3:
    imgPlayer:Y = ii.
    IF ii > 90 THEN 
    DO: 
      imgBall:Y = imgBall:Y + 3.
      rcCord:Y = rcCord:Y + 3.
      edChangelog:HEIGHT-PIXELS = edChangelog:HEIGHT-PIXELS + 3.
    END.

    RUN justWait(5).
  END.

  BtnOK:VISIBLE = TRUE.
  fiWebsite:VISIBLE = TRUE.
  edChangelog:HEIGHT-PIXELS = 350.
  imgPlayer:VISIBLE = FALSE.
  imgBall:VISIBLE = FALSE.
  rcCord:VISIBLE = FALSE.
  glIntroRunning = FALSE.

END PROCEDURE. /* blinkLogo */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wAbout  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wAbout)
  THEN DELETE WIDGET wAbout.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wAbout 
PROCEDURE initializeObject :
/* Init frame
  */
  DO WITH FRAME {&FRAME-NAME}:
    wAbout:VISIBLE = FALSE.

    /* Position frame relative to main window */
    wAbout:X = MAXIMUM(0, INTEGER(getRegistry('DataDigger', 'Window:x' )) - 50).
    wAbout:Y = MAXIMUM(0, INTEGER(getRegistry('DataDigger', 'Window:y' )) - 20).

    /* Prepare frame */
    FRAME {&FRAME-NAME}:FONT = getFont('Default').
    fiDataDigger-1:FONT      = getFont('Fixed').
    fiDataDigger-2:FONT      = getFont('Fixed').
    edChangelog:FONT         = getFont('Fixed').

    btnDataDigger:LOAD-IMAGE(getImagePath('box.gif')).
    imgPlayer:LOAD-IMAGE(getImagePath('player.gif')).
    imgBall:LOAD-IMAGE(getImagePath('ball.gif')).

    /* Set version name */
    fiDataDigger-1:SCREEN-VALUE = "DataDigger {&version} - {&edition}".
    fiDataDigger-2:SCREEN-VALUE = 'Build {&build}'.

    /* Load changelog */
    edChangeLog:INSERT-FILE(getProgramDir() + 'DataDigger.txt').
    edChangeLog:CURSOR-OFFSET = 1.

    RUN setTransparency(INPUT FRAME {&FRAME-NAME}:HANDLE, 1).

    /* For some reasons, these #*$&# scrollbars keep coming back */
    RUN showScrollBars(FRAME {&FRAME-NAME}:HANDLE, NO, NO). /* KILL KILL KILL */

    wAbout:VISIBLE = TRUE.

    RUN blinkLogo.
  END.

END PROCEDURE. /* initializeObject. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeUi wAbout 
PROCEDURE initializeUi :
/* Enable the user interface
 */

  /* From enable_ui */
  DISPLAY edChangelog fiDataDigger-1 fiDataDigger-2 fiWebsite
      WITH FRAME DEFAULT-FRAME IN WINDOW wAbout.

  ENABLE btnDataDigger BtnOK edChangelog fiWebsite
      WITH FRAME DEFAULT-FRAME IN WINDOW wAbout.

  VIEW FRAME DEFAULT-FRAME IN WINDOW wAbout.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}

END PROCEDURE. /* initializeUi */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE justWait wAbout 
PROCEDURE justWait :
/* Wait a few miliseconds
 */
  DEFINE INPUT PARAMETER piWait AS INTEGER NO-UNDO.
  DEFINE VARIABLE iStart AS INTEGER NO-UNDO.

  iStart = ETIME.

  DO WHILE ETIME < iStart + piWait:
    PROCESS EVENTS.
  END.

END PROCEDURE. /* justWait */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

