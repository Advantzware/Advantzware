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

DEF BUFFER b-gl-rpt FOR gl-rpt.
DEF BUFFER b-gl-rptd FOR gl-rptd.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_company begin_rpt end_company end_rpt ~
btn-process btn-cancel RECT-17 RECT-18 RECT-19 
&Scoped-Define DISPLAYED-OBJECTS begin_company begin_name begin_rpt ~
begin_dscr end_company end_name end_rpt 

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

DEFINE VARIABLE begin_company AS CHARACTER FORMAT "XXX" INITIAL "001" 
     LABEL "From Company" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE VARIABLE begin_dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE begin_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rpt AS CHARACTER FORMAT "xxxx" 
     LABEL "From Report Code" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE VARIABLE end_company AS CHARACTER FORMAT "XXX" INITIAL "001" 
     LABEL "To Company" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE VARIABLE end_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE end_rpt AS CHARACTER FORMAT "xxxx" 
     LABEL "To Report Code" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 96 BY 10.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 4.52.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 3.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_company AT ROW 3.14 COL 24 COLON-ALIGNED HELP
          "Enter Company To Copy From"
     begin_name AT ROW 3.14 COL 38 COLON-ALIGNED NO-LABEL
     begin_rpt AT ROW 4.1 COL 24 COLON-ALIGNED HELP
          "Enter Report Code to Be Copied From"
     begin_dscr AT ROW 4.1 COL 38 COLON-ALIGNED NO-LABEL
     end_company AT ROW 8.38 COL 24 COLON-ALIGNED HELP
          "Copy To Company"
     end_name AT ROW 8.38 COL 38 COLON-ALIGNED NO-LABEL
     end_rpt AT ROW 9.33 COL 24 COLON-ALIGNED HELP
          "Enter Report Code to Be Copied To"
     btn-process AT ROW 11.48 COL 26
     btn-cancel AT ROW 11.48 COL 57
     RECT-17 AT ROW 1 COL 1
     RECT-18 AT ROW 2.19 COL 2
     RECT-19 AT ROW 7.67 COL 2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .95 AT ROW 1.24 COL 4
          FONT 4
     "C O P Y  T O" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 7.19 COL 40
          BGCOLOR 8 FGCOLOR 9 
     "C O P Y  F R O M" VIEW-AS TEXT
          SIZE 21 BY 1 AT ROW 1.95 COL 38
          BGCOLOR 8 FGCOLOR 9 
     "(Enter spaces to copy all)" VIEW-AS TEXT
          SIZE 31 BY 1 AT ROW 5.29 COL 26
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
         TITLE              = "Copy Financial Statement"
         HEIGHT             = 12.24
         WIDTH              = 96.8
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
       begin_company:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_dscr IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN begin_name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       begin_rpt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_company:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       end_rpt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Copy Financial Statement */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Copy Financial Statement */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_company C-Win
ON LEAVE OF begin_company IN FRAME FRAME-A /* From Company */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-company (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_company C-Win
ON VALUE-CHANGED OF begin_company IN FRAME FRAME-A /* From Company */
DO:
  RUN new-company (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rpt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rpt C-Win
ON LEAVE OF begin_rpt IN FRAME FRAME-A /* From Report Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-begin_rpt NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rpt C-Win
ON VALUE-CHANGED OF begin_rpt IN FRAME FRAME-A /* From Report Code */
DO:
  RUN new-begin_rpt.
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


  RUN valid-company (begin_company:HANDLE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-begin_rpt NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-company (end_company:HANDLE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-to_rpt NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    IF begin_rpt:SCREEN-VALUE EQ ""                           AND
       begin_company:SCREEN-VALUE EQ end_company:SCREEN-VALUE THEN DO:
      MESSAGE "From & To Companies may not be the same when copying all..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO end_company.
      RETURN NO-APPLY.
    END.

    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) + " " +
          TRIM(STRING(begin_rpt)) + " from Company " + TRIM(begin_company) + " to " +
          " Company " + TRIM(end_company) + "?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE v-process.

  IF v-process THEN RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_company C-Win
ON LEAVE OF end_company IN FRAME FRAME-A /* To Company */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-company (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_company C-Win
ON VALUE-CHANGED OF end_company IN FRAME FRAME-A /* To Company */
DO:
  RUN new-company (FOCUS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rpt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rpt C-Win
ON LEAVE OF end_rpt IN FRAME FRAME-A /* To Report Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-to_rpt NO-ERROR.
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

  FIND gl-rpt WHERE ROWID(gl-rpt) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL gl-rpt THEN DO:
    ASSIGN
     begin_company = gl-rpt.company
     end_company   = gl-rpt.company
     begin_rpt     = gl-rpt.rpt
     end_rpt       = gl-rpt.rpt.

    RUN enable_UI.

    DO WITH FRAME {&FRAME-NAME}:
      RUN new-company (begin_company:HANDLE).
      RUN new-company (end_company:HANDLE).
    END.

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
  DISPLAY begin_company begin_name begin_rpt begin_dscr end_company end_name 
          end_rpt 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_company begin_rpt end_company end_rpt btn-process btn-cancel 
         RECT-17 RECT-18 RECT-19 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-begin_rpt C-Win 
PROCEDURE new-begin_rpt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND b-gl-rpt NO-LOCK
        WHERE b-gl-rpt.company EQ begin_company:SCREEN-VALUE
          AND b-gl-rpt.rpt     EQ begin_rpt:SCREEN-VALUE
          AND b-gl-rpt.line    EQ 0
        NO-ERROR.
    IF AVAIL b-gl-rpt THEN DO:
      ASSIGN
       begin_dscr:SCREEN-VALUE = b-gl-rpt.dscr
       end_rpt:SCREEN-VALUE    = b-gl-rpt.rpt.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-company C-Win 
PROCEDURE new-company :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    FIND company WHERE company.company BEGINS ip-focus:SCREEN-VALUE NO-LOCK NO-ERROR.

    IF AVAIL company THEN DO:
      FOCUS:SCREEN-VALUE = company.company.

      IF ip-focus:NAME EQ "begin_company" THEN DO:
        begin_name:SCREEN-VALUE = company.name.
        RUN new-begin_rpt.
      END.

      ELSE end_name:SCREEN-VALUE = company.name.
    END.
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
  DEF VAR new-rpt LIKE gl-rpt.rpt NO-UNDO.


  SESSION:SET-WAIT-STATE("general").

  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH gl-rpt NO-LOCK
        WHERE gl-rpt.company EQ begin_company
          AND (gl-rpt.rpt    EQ begin_rpt OR begin_rpt EQ "")
        USE-INDEX main:

      new-rpt = IF begin_rpt EQ "" THEN gl-rpt.rpt ELSE end_rpt.

      IF NOT CAN-FIND(FIRST b-gl-rpt
                      WHERE b-gl-rpt.company EQ end_company
                        AND b-gl-rpt.rpt     EQ new-rpt
                        AND b-gl-rpt.line    EQ gl-rpt.line
                      USE-INDEX main) THEN DO:
        CREATE b-gl-rpt.
        BUFFER-COPY gl-rpt EXCEPT rec_key TO b-gl-rpt
        ASSIGN
         b-gl-rpt.company = end_company
         b-gl-rpt.rpt     = new-rpt.
      END.

      FOR EACH gl-rptd NO-LOCK
          WHERE gl-rptd.company EQ gl-rpt.company
            AND gl-rptd.rpt     EQ gl-rpt.rpt
            AND gl-rptd.line    EQ gl-rpt.line:

        IF NOT CAN-FIND(FIRST b-gl-rptd
                        WHERE b-gl-rptd.company EQ end_company
                          AND b-gl-rptd.rpt     EQ new-rpt
                          AND b-gl-rptd.line    EQ gl-rptd.line
                          AND b-gl-rptd.seq     EQ gl-rptd.seq) THEN DO:
          CREATE b-gl-rptd.
          BUFFER-COPY gl-rptd EXCEPT rec_key TO b-gl-rptd
          ASSIGN
           b-gl-rptd.company = end_company
           b-gl-rptd.rpt     = new-rpt.
        END.
      END.
    END.
  END.

  op-copied = YES.

  SESSION:SET-WAIT-STATE("").

  MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-begin_rpt C-Win 
PROCEDURE valid-begin_rpt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    v-msg = "".

    IF v-msg EQ "" THEN
      IF begin_rpt:SCREEN-VALUE NE "" AND
         NOT CAN-FIND(FIRST b-gl-rpt
                      WHERE b-gl-rpt.company EQ begin_company:SCREEN-VALUE
                        AND b-gl-rpt.rpt     EQ begin_rpt:SCREEN-VALUE
                        AND b-gl-rpt.line    EQ 0) THEN
        v-msg = TRIM(begin_rpt:LABEL) + " doesn't exist, try help...".

    IF v-msg NE "" THEN DO:
      MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO begin_rpt.
      RETURN ERROR.
    END.

    IF begin_rpt:SCREEN-VALUE EQ "" THEN
      ASSIGN
       end_rpt:SCREEN-VALUE = ""
       end_rpt:SENSITIVE    = NO.

    ELSE DO:
      end_rpt:SENSITIVE = YES.
      IF end_rpt EQ "" THEN
        end_rpt:SCREEN-VALUE = begin_rpt:SCREEN-VALUE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-company C-Win 
PROCEDURE valid-company :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.

  DEF VAR lv-msg AS CHAR INIT "" NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST company WHERE company.company EQ ip-focus:SCREEN-VALUE) THEN
      lv-msg = "Invalid entry, try help".

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-to_rpt C-Win 
PROCEDURE valid-to_rpt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    v-msg = "".

    IF v-msg EQ "" THEN
      IF end_rpt:SCREEN-VALUE NE "" AND CAN-FIND(FIRST b-gl-rpt
                  WHERE b-gl-rpt.company EQ end_company:SCREEN-VALUE
                    AND b-gl-rpt.rpt     EQ end_rpt:SCREEN-VALUE
                    AND b-gl-rpt.line    EQ 0) THEN
        v-msg = TRIM(end_rpt:LABEL) + " already exists, please re-enter...".

    IF v-msg NE "" THEN DO:
      MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO end_rpt.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

