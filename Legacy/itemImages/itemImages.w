&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: itemImages.w

  Description: auto scan for item image files

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 5.28.2008

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

DEFINE VARIABLE NK1Values AS CHARACTER NO-UNDO.
DEFINE VARIABLE company AS CHARACTER NO-UNDO.
DEFINE VARIABLE firstTime AS LOGICAL NO-UNDO.
DEFINE VARIABLE imageExt AS CHARACTER NO-UNDO.
DEFINE VARIABLE running AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

SESSION:SET-WAIT-STATE('').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 fromDir toDir processInterval ~
btnLog btnStart btnClose 
&Scoped-Define DISPLAYED-OBJECTS fromDir toDir processInterval 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fromDir toDir processInterval btnLog btnClose 
&Scoped-define List-2 fromDir toDir processInterval 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClose 
     LABEL "&Close" 
     SIZE 13 BY 1.14.

DEFINE BUTTON btnLog 
     LABEL "&Log" 
     SIZE 13 BY 1.14.

DEFINE BUTTON btnStart 
     LABEL "&Start" 
     SIZE 12 BY 1.14.

DEFINE VARIABLE fromDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Item Images Initial Location" 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE processInterval AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Process Interval (Minutes)" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE toDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Item Images Location" 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY 1.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 15 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fromDir AT ROW 1.24 COL 26 COLON-ALIGNED HELP
          "Enter Item Images Initial Location"
     toDir AT ROW 2.43 COL 26 COLON-ALIGNED HELP
          "Enter Item Images Location"
     processInterval AT ROW 3.62 COL 26 COLON-ALIGNED HELP
          "Enter Process Interval (Minutes)"
     btnLog AT ROW 3.86 COL 60 HELP
          "Access Log"
     btnStart AT ROW 3.86 COL 76 HELP
          "Start Item Image Monitor"
     btnClose AT ROW 3.86 COL 89 HELP
          "Cancel Item Image Monitor"
     RECT-1 AT ROW 3.62 COL 75
     RECT-2 AT ROW 3.62 COL 59
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.6 BY 4.52.


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
         TITLE              = "Item Image Monitor v1.0"
         HEIGHT             = 4.52
         WIDTH              = 102.6
         MAX-HEIGHT         = 4.52
         MAX-WIDTH          = 102.6
         VIRTUAL-HEIGHT     = 4.52
         VIRTUAL-WIDTH      = 102.6
         RESIZE             = yes
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR BUTTON btnClose IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnLog IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN fromDir IN FRAME DEFAULT-FRAME
   1 2                                                                  */
/* SETTINGS FOR FILL-IN processInterval IN FRAME DEFAULT-FRAME
   1 2                                                                  */
/* SETTINGS FOR FILL-IN toDir IN FRAME DEFAULT-FRAME
   1 2                                                                  */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Item Image Monitor v1.0 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Item Image Monitor v1.0 */
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
  running = ?.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLog C-Win
ON CHOOSE OF btnLog IN FRAME DEFAULT-FRAME /* Log */
DO:
  OS-COMMAND NO-WAIT notepad itemImages/itemImages.log.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStart C-Win
ON CHOOSE OF btnStart IN FRAME DEFAULT-FRAME /* Start */
DO:
  IF SELF:LABEL EQ '~&Stop' THEN DO:
    ASSIGN
      SELF:LABEL = '~&Start'
      running = NO.
    ENABLE {&List-1} WITH FRAME {&FRAME-NAME}.
    RUN logEntry ('Stopped').
  END.
  ELSE DO:
    ASSIGN
      SELF:LABEL = '~&Stop'
      running = YES
      firstTime = YES
      {&List-2}.
    DISABLE {&List-1} WITH FRAME {&FRAME-NAME}.
    RUN logEntry ('Started').
    RUN getImages.
  END.
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
RUN enable_UI.

CONNECT -pf itemImages/itemImages.pf NO-ERROR.
IF ERROR-STATUS:ERROR THEN
DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
  MESSAGE ERROR-STATUS:GET-NUMBER(i)
          ERROR-STATUS:GET-MESSAGE(i) VIEW-AS ALERT-BOX.
END.

IF CONNECTED('asi') THEN DO:
  INPUT FROM 'itemImages/itemImages.dat' NO-ECHO.
  IMPORT UNFORMATTED company.
  IMPORT UNFORMATTED imageExt.
  INPUT CLOSE.
  RUN itemImages/NK1Values.p (company,OUTPUT NK1Values).
  ASSIGN
    toDir:SCREEN-VALUE = ENTRY(1,NK1Values)
    fromDir:SCREEN-VALUE = ENTRY(2,NK1Values)
    processInterval:SCREEN-VALUE = ENTRY(3,NK1Values).
  DISCONNECT 'asi'.
END.
ELSE DO:
  APPLY 'CLOSE':U TO THIS-PROCEDURE.
  QUIT.
END.

MAIN-BLOCK:
DO WHILE TRUE:
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE PAUSE 3.
  PROCESS EVENTS.
  IF running THEN RUN getImages.
  ELSE IF running EQ ? THEN LEAVE.
END.
APPLY 'CLOSE':U TO THIS-PROCEDURE.
QUIT.

{itemImages/logEntry.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE connectDB C-Win 
PROCEDURE connectDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  CONNECT -pf itemImages/itemImages.pf NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
    RUN logEntry ('DB Connection Error: ' +
                  STRING(ERROR-STATUS:GET-NUMBER(i)) + ' ' +
                  STRING(ERROR-STATUS:GET-MESSAGE(i))).
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disconnectDB C-Win 
PROCEDURE disconnectDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF CONNECTED('asi') THEN
  DISCONNECT 'asi'.

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
  DISPLAY fromDir toDir processInterval 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 fromDir toDir processInterval btnLog btnStart btnClose 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getImages C-Win 
PROCEDURE getImages :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF ETIME GE processInterval * 60 * 1000 OR firstTime THEN DO:
    RUN connectDB.
    IF CONNECTED('asi') THEN
    RUN itemImages/getImages.p (company,fromDir,toDir,imageExt).
    RUN disconnectDB.
    ETIME(TRUE).
    firstTime = NO.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

