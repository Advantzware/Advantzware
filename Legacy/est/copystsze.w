&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
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
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-copied AS LOG NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEF BUFFER b-stack-size FOR stack-size.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_pallet begin_line end_line end_pallet ~
btn-process btn-cancel RECT-17 RECT-18 RECT-19 
&Scoped-Define DISPLAYED-OBJECTS begin_pallet begin_line end_line ~
end_pallet 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_line AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "From Line#" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE begin_pallet AS CHARACTER FORMAT "x(10)" 
     LABEL "From Pallet Code" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE end_line AS INTEGER FORMAT ">9" INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE end_pallet AS CHARACTER FORMAT "x(10)" 
     LABEL "To Pallet Code" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50 BY 10.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 45 BY 4.52.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 45 BY 4.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_pallet AT ROW 3.86 COL 24 COLON-ALIGNED HELP
          "Enter Report Code to Be Copied From"
     begin_line AT ROW 5.29 COL 24 COLON-ALIGNED HELP
          "Enter Report Code to Be Copied From"
     end_line AT ROW 5.29 COL 36 COLON-ALIGNED HELP
          "Enter Report Code to Be Copied From" NO-LABEL
     end_pallet AT ROW 8.86 COL 24 COLON-ALIGNED HELP
          "Enter Report Code to Be Copied To"
     btn-process AT ROW 11.24 COL 5
     btn-cancel AT ROW 11.24 COL 27
     RECT-17 AT ROW 1 COL 1
     RECT-18 AT ROW 2.19 COL 2
     RECT-19 AT ROW 6.71 COL 2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .95 AT ROW 1.24 COL 4
          FONT 4
     "C O P Y  T O" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 6.71 COL 15
          BGCOLOR 8 FGCOLOR 9 
     "C O P Y  F R O M" VIEW-AS TEXT
          SIZE 21 BY 1 AT ROW 2.19 COL 15
          BGCOLOR 8 FGCOLOR 9 
     "To" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 5.29 COL 33
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.8 BY 12.14
         FONT 6.


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
         TITLE              = "Copy Stacking Matrix"
         HEIGHT             = 12.24
         WIDTH              = 47.2
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 160
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         FONT               = 6
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
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_line:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_pallet:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_line:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_pallet:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Copy Stacking Matrix */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Copy Stacking Matrix */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_line
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_line C-Win
ON HELP OF begin_line IN FRAME FRAME-A /* From Line# */
DO:
  RUN pallet-help (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_pallet C-Win
ON HELP OF begin_pallet IN FRAME FRAME-A /* From Pallet Code */
DO:
  RUN pallet-help (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_pallet C-Win
ON LEAVE OF begin_pallet IN FRAME FRAME-A /* From Pallet Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-begin_pallet (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  DEF VAR v-process AS LOG INIT NO NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-begin_pallet (begin_pallet:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-end_pallet (end_pallet:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) + " " +
          TRIM(STRING(begin_pallet)) + " to " +
          TRIM(STRING(end_pallet)) + "?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE v-process.

  IF v-process THEN RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_line
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_line C-Win
ON HELP OF end_line IN FRAME FRAME-A
DO:
  RUN pallet-help (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_pallet C-Win
ON HELP OF end_pallet IN FRAME FRAME-A /* To Pallet Code */
DO:
  RUN pallet-help (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_pallet C-Win
ON LEAVE OF end_pallet IN FRAME FRAME-A /* To Pallet Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-end_pallet (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
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

  FIND stack-size WHERE ROWID(stack-size) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL stack-size THEN DO:
    ASSIGN
     cocode       = stack-size.company
     locode       = stack-size.loc
     begin_pallet = stack-size.pallet.

    RUN enable_UI.

    APPLY "entry" TO begin_line.

    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  END.
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
  DISPLAY begin_pallet begin_line end_line end_pallet 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_pallet begin_line end_line end_pallet btn-process btn-cancel 
         RECT-17 RECT-18 RECT-19 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pallet-help C-Win 
PROCEDURE pallet-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  DEF VAR char-val AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN windows/l-itemp.w (cocode, "", ip-focus:SCREEN-VALUE, OUTPUT char-val).
    IF char-val NE "" AND ip-focus:SCREEN-VALUE NE ENTRY(1,char-val) THEN
      ip-focus:SCREEN-VALUE = ENTRY(1,char-val).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/*------------------------------------------------------------------------------
  Purpose:     copy order record
  Parameters:  <none>
  Notes:       copyOrder procedure is contained in oe/copyOrder.i include
------------------------------------------------------------------------------*/

  SESSION:SET-WAIT-STATE("general").

  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH stack-size NO-LOCK
        WHERE stack-size.company EQ cocode
          AND stack-size.loc     EQ locode
          AND stack-size.pallet  EQ begin_pallet
          AND stack-size.line#   GE begin_line
          AND stack-size.line#   LE end_line:

      FIND FIRST b-stack-size
          WHERE b-stack-size.company EQ stack-size.company
            AND b-stack-size.loc     EQ stack-size.loc
            AND b-stack-size.pallet  EQ end_pallet
            AND b-stack-size.line#   EQ stack-size.line#
          NO-ERROR.
      IF AVAIL b-stack-size THEN DELETE b-stack-size.

      CREATE b-stack-size.
      BUFFER-COPY stack-size EXCEPT rec_key TO b-stack-size
      ASSIGN
       b-stack-size.pallet = end_pallet.
    END.
  END.

  op-copied = YES.

  SESSION:SET-WAIT-STATE("").

  MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-begin_pallet C-Win 
PROCEDURE valid-begin_pallet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  DEF VAR v-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    v-msg = "".

    IF v-msg EQ "" THEN
      IF NOT CAN-FIND(FIRST b-stack-size
                      WHERE b-stack-size.company EQ cocode
                        AND b-stack-size.loc     EQ locode
                        AND b-stack-size.pallet  EQ ip-focus:SCREEN-VALUE) THEN
        v-msg = TRIM(ip-focus:LABEL) + " doesn't exist, try help".

    IF v-msg NE "" THEN DO:
      MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-end_pallet C-Win 
PROCEDURE valid-end_pallet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  DEF VAR v-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    v-msg = "".

    IF v-msg EQ "" THEN
      IF TRIM(ip-focus:SCREEN-VALUE) EQ TRIM(begin_pallet:SCREEN-VALUE) THEN
        v-msg = TRIM(ip-focus:LABEL) +
                " may not be the same as " +
                TRIM(begin_pallet:LABEL).

    IF v-msg EQ "" THEN
      IF NOT CAN-FIND(FIRST item
                      WHERE item.company  EQ cocode
                        AND item.i-no     EQ ip-focus:SCREEN-VALUE
                        AND item.mat-type EQ "D") THEN
        v-msg = TRIM(ip-focus:LABEL) + " not in RM file".

    /*IF v-msg EQ "" THEN
      IF CAN-FIND(FIRST b-stack-size
                  WHERE b-stack-size.company EQ cocode
                    AND b-stack-size.pallet  EQ ip-focus:SCREEN-VALUE) THEN
        v-msg = TRIM(ip-focus:LABEL) + " already exists".*/

    IF v-msg EQ "" THEN
      IF ip-focus:SCREEN-VALUE EQ "" THEN
        v-msg = TRIM(ip-focus:LABEL) + " may not be spaces".

    IF v-msg NE "" THEN DO:
      MESSAGE TRIM(v-msg) + ", please re-enter..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

