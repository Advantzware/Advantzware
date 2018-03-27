&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = gcompany
 locode = gloc.

DEF NEW SHARED BUFFER xest FOR est.

{sys/inc/msfcalc.i}

{ce/mach-ink.i NEW}

DEF STREAM st-excell.

DEFINE VARIABLE v-style-list AS CHARACTER   NO-UNDO.
DEFINE VARIABLE v-sel-style-list AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_mach end_mach ~
begin_job-no begin_job-no2 end_job-no end_job-no2 begin_date end_date rsQty ~
tb_styles sel-styles tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_mach end_mach begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_date end_date rsQty tb_styles ~
sel-styles lv-ornt lines-per-page rd-dest lv-font-no lv-font-name tb_excel ~
tb_runExcel fi_file td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-colors C-Win 
FUNCTION get-colors RETURNS INTEGER
    (in-est AS CHAR, in-form AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi-styles AS CHARACTER FORMAT "X(256)":U 
     LABEL "Selected styles" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-wipstd.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "L" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE VARIABLE rsQty AS CHARACTER INITIAL "A" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Actual Qty", "A",
"Estimated Qty", "E"
     SIZE 56 BY 1.43 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 13.57.

DEFINE VARIABLE sel-styles AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 20 BY 4.29 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL yes 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_styles AS LOGICAL INITIAL no 
     LABEL "Select styles (uncheck box for report on all styles)" 
     VIEW-AS TOGGLE-BOX
     SIZE 60 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_mach AT ROW 2.91 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_mach AT ROW 2.91 COL 64 COLON-ALIGNED HELP
          "Enter Ending Machine"
     begin_job-no AT ROW 4.57 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 2
     begin_job-no2 AT ROW 4.57 COL 38 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 4
     end_job-no AT ROW 4.57 COL 64 COLON-ALIGNED HELP
          "Enter Ending Job Number" WIDGET-ID 6
     end_job-no2 AT ROW 4.57 COL 77 COLON-ALIGNED HELP
          "Enter Ending Job Number" WIDGET-ID 8
     begin_date AT ROW 6.24 COL 26 COLON-ALIGNED
     end_date AT ROW 6.24 COL 64 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     rsQty AT ROW 7.48 COL 28 NO-LABEL WIDGET-ID 10
     tb_styles AT ROW 9.1 COL 24 WIDGET-ID 20
     sel-styles AT ROW 9.91 COL 27.8 NO-LABEL WIDGET-ID 14
     fi-styles AT ROW 11.24 COL 65 COLON-ALIGNED WIDGET-ID 16 NO-TAB-STOP 
     lv-ornt AT ROW 16 COL 33 NO-LABEL
     lines-per-page AT ROW 16 COL 86 COLON-ALIGNED
     rd-dest AT ROW 16.95 COL 8 NO-LABEL
     lv-font-no AT ROW 17.43 COL 37 COLON-ALIGNED
     lv-font-name AT ROW 18.62 COL 31 COLON-ALIGNED NO-LABEL
     tb_excel AT ROW 20.05 COL 70 RIGHT-ALIGNED
     tb_runExcel AT ROW 20.05 COL 92 RIGHT-ALIGNED
     fi_file AT ROW 21 COL 48 COLON-ALIGNED HELP
          "Enter File Name"
     td-show-parm AT ROW 21.24 COL 7
     btn-ok AT ROW 23.62 COL 27
     btn-cancel AT ROW 23.62 COL 59
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 15.52 COL 5
     RECT-6 AT ROW 14.57 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.1
         SIZE 94.4 BY 24.19.


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
         TITLE              = "Machines MSF Produced by Job"
         HEIGHT             = 24.33
         WIDTH              = 95.8
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
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
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN fi-styles IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi-styles:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rd-dest IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Machines MSF Produced by Job */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Machines MSF Produced by Job */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  RUN run-report. 
  STATUS DEFAULT "Processing Complete".

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
  END CASE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON LEAVE OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
   ASSIGN lv-font-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON LEAVE OF lv-ornt IN FRAME FRAME-A
DO:
  ASSIGN lv-ornt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON VALUE-CHANGED OF lv-ornt IN FRAME FRAME-A
DO:
  {custom/chgfont.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sel-styles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sel-styles C-Win
ON VALUE-CHANGED OF sel-styles IN FRAME FRAME-A
DO:
    assign
        tb_styles:SCREEN-VALUE = "YES"
        tb_styles.
    RUN GetSelectedStyles.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_styles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_styles C-Win
ON VALUE-CHANGED OF tb_styles IN FRAME FRAME-A /* Select styles (uncheck box for report on all styles) */
DO:
  ASSIGN tb_styles.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    assign {&self-name}.
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

 /* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  ASSIGN
   begin_date  = DATE (1,1,YEAR(TODAY))
   end_date    = DATE (12,31,year(TODAY)).

  RUN enable_UI.

  for each style WHERE style.company = cocode NO-LOCK:
     v-style-list = v-style-list + string(style.style,"x(5)") /* + " " + mat.dscr */ + ",".
   end.
   if substr(v-style-list,length(trim(v-style-list)),1) eq "," then
     substr(v-style-list,length(trim(v-style-list)),1) = "".

   sel-styles:LIST-ITEMS = v-style-list.

/*    do i = 1 to sel-styles:num-items:    */
/*        sel-styles:is-selected(i) = YES. */
/*    end.                                 */

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_mach.
  END.

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
  DISPLAY begin_mach end_mach begin_job-no begin_job-no2 end_job-no end_job-no2 
          begin_date end_date rsQty tb_styles sel-styles lv-ornt lines-per-page 
          rd-dest lv-font-no lv-font-name tb_excel tb_runExcel fi_file 
          td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_mach end_mach begin_job-no begin_job-no2 
         end_job-no end_job-no2 begin_date end_date rsQty tb_styles sel-styles 
         tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectedStyles C-Win 
PROCEDURE GetSelectedStyles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
v-sel-style-list = "".
DO WITH FRAME {&frame-name}:
    DO i = 1 to sel-styles:NUM-ITEMS:
      IF sel-styles:IS-SELECTED(i) then
          v-sel-style-list = v-sel-style-list + TRIM(SUBSTR(sel-styles:ENTRY(i),1,5)) + ",".
    END.
    IF v-sel-style-list NE "" THEN
        IF SUBSTR(v-sel-style-list,LENGTH(TRIM(v-sel-style-list)),1) EQ "," THEN
            SUBSTR(v-sel-style-list,LENGTH(TRIM(v-sel-style-list)),1) = "". 
    fi-styles = v-sel-style-list.
    DO i = 1 TO LENGTH(fi-styles):
        IF SUBSTR(fi-styles,i,1) EQ "," THEN SUBSTR(fi-styles,i,1) = " ".
    END.  
    DISPLAY fi-styles.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file C-Win 
PROCEDURE output-to-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
    /*     CREATE-TEST-FILE*/
         SAVE-AS
         USE-FILENAME

         UPDATE OKpressed.

     IF NOT OKpressed THEN  RETURN NO-APPLY.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
  */
  RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-screen C-Win 
PROCEDURE output-to-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  list-name = fi_file.

  RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report PRIVATE :
{sys/form/r-top3w.f}

DEF VAR str_buffa   AS   CHAR NO-UNDO.
DEF VAR v-hdr       AS   CHAR NO-UNDO.
DEF VAR lv-rc-seq   LIKE dept.fc NO-UNDO.
DEF VAR ld-msf      AS   DEC NO-UNDO.
DEF VAR li-up       AS   INT NO-UNDO.
DEF VAR lv-out      AS   CHAR NO-UNDO.
DEF VAR ld-qty-ton AS DEC NO-UNDO.
DEF VAR ld-qty-msf AS DEC NO-UNDO.
DEF VAR ld-tot-msf AS DEC NO-UNDO.
DEF VAR v-adder AS CHAR NO-UNDO.

SESSION:SET-WAIT-STATE ("general").

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

    /*
 v-hdr = "Machine#,S,B,Charge Code,Charge Category,Date,Job#,Shift,Hours,Qt" +
         "y,Waste,FG Item,Style,Length,Width,Depth,Blank Length,Blank Width" +
         ",Blank Square Inches,Board Code,Board Caliper,MSF,Wgt/MSF,Roll Wi" +
         "dth,Gross Sheet Width,Gross Sheet Length,Net Sheet Width,Net Shee" +
         "t Length,Film Width,Film Length,# Colors,Die Inches,Number Up,Num" +
         "ber Out,Glue Inches".

      */
 v-hdr = "Date,Job#,Machine,Form#,Board,Adder,Style,Blank Width,Blank Length,Gross Sheet Width,Gross Sheet Length,Net Sheet Width,Net Sheet Length,Qty,MSF Produced,Total MSF,Customer #,Length,Width,Depth,Total # Up on Die,Colors".

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

RUN est/rc-seq.p (OUTPUT lv-rc-seq).

IF tb_excel THEN DO:
    OUTPUT STREAM st-excell TO VALUE(fi_file).

    PUT STREAM st-excell UNFORMATTED v-hdr SKIP.
END.


IF tb_styles THEN RUN GetSelectedStyles.          
/*   do i = 1 to sel-styles:num-items:                                                      */
/*     if sel-styles:is-selected(i) then                                                    */
/*       v-sel-style-list = v-sel-style-list + trim(substr(sel-styles:entry(i),1,5)) + ",". */
/*   end.                                                                                   */
/*                                                                                          */
/* /*   IF length(TRIM(v-sel-style-list)) EQ 0 THEN */                                      */
/* /*   DO:                                         */                                      */
/* /*      MESSAGE "No Style Selected."             */                                      */
/* /*          VIEW-AS ALERT-BOX INFO BUTTONS OK.   */                                      */
/* /*      LEAVE.                                   */                                      */
/* /*   END.                                        */                                      */
/*                                                                                          */
/*   if substr(v-sel-style-list,length(trim(v-sel-style-list)),1) eq "," then               */
/*     substr(v-sel-style-list,length(trim(v-sel-style-list)),1) = "".                      */
/*                                                                                          */
/*   fi-styles = v-sel-style-list.                                                          */
/*                                                                                          */
/*   do i = 1 to length(fi-styles):                                                         */
/*     if substr(fi-styles,i,1) eq "," then substr(fi-styles,i,1) = " ".                    */
/*   end.                                                                                   */
/*                                                                                          */
/*   display fi-styles.                                                                     */
/*                                                                                          */
/* end.                                                                                     */

IF rsQty = "A" THEN DO:
  FOR EACH mch-act NO-LOCK
      WHERE mch-act.company EQ cocode
        AND mch-act.m-code  GE begin_mach
        AND mch-act.m-code  LE end_mach
        AND mch-act.job-no GE begin_job-no
        AND mch-act.job-no LE END_job-no
        AND mch-act.op-date GE begin_date
        AND mch-act.op-date LE end_date
      USE-INDEX dly-idx,

      FIRST mach NO-LOCK
      WHERE mach.company EQ mch-act.company
        AND mach.m-code  EQ mch-act.m-code,

      FIRST job NO-LOCK
      WHERE job.company EQ mch-act.company
        AND job.job     EQ mch-act.job
        AND job.job-no  EQ mch-act.job-no
        AND job.job-no2 EQ mch-act.job-no2
      USE-INDEX job:

      {custom/statusMsg.i " 'Processing Job#  '  + job.job-no "}

    FIND FIRST job-code WHERE job-code.code EQ mch-act.code NO-LOCK NO-ERROR.

    FIND FIRST job-mch NO-LOCK
        WHERE job-mch.company  EQ mch-act.company
          AND job-mch.job      EQ mch-act.job
          AND job-mch.job-no   EQ mch-act.job-no
          AND job-mch.job-no2  EQ mch-act.job-no2
          AND job-mch.frm      EQ mch-act.frm
          AND job-mch.blank-no EQ mch-act.blank-no
          AND job-mch.m-code   EQ mch-act.m-code
          AND job-mch.pass     EQ mch-act.pass
        NO-ERROR.

    RELEASE est.
    RELEASE ef.
    RELEASE eb.

    IF TRIM(job.est-no) NE "" THEN
    FIND FIRST est NO-LOCK
        WHERE est.company EQ job.company
          AND est.est-no  EQ job.est-no
        NO-ERROR.

    IF AVAIL est THEN
    FIND FIRST ef NO-LOCK
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ mch-act.frm
        NO-ERROR.

    IF AVAIL ef THEN
    FIND FIRST eb NO-LOCK
        WHERE eb.company   EQ ef.company
          AND eb.est-no    EQ ef.est-no
          AND eb.form-no   EQ ef.form-no
          AND (eb.blank-no EQ mch-act.blank-no OR mch-act.blank-no EQ 0)
        NO-ERROR.

    IF tb_styles AND AVAIL eb THEN
          IF LOOKUP(eb.style,v-sel-style-list) = 0 THEN NEXT.

    IF AVAIL ef THEN
    FIND FIRST est-flm NO-LOCK
        WHERE est-flm.company  EQ ef.company
          AND est-flm.est-no   EQ ef.est-no
          AND est-flm.snum     EQ ef.form-no
          AND est-flm.bnum     EQ mch-act.blank-no
        NO-ERROR.

    IF AVAIL est THEN
    FIND FIRST est-op NO-LOCK
        WHERE est-op.company EQ est.company
          AND est-op.est-no  EQ est.est-no
          AND est-op.s-num   EQ mch-act.frm
          AND (est-op.b-num  EQ mch-act.blank-no OR
               mch-act.blank-no EQ 0)
          AND est-op.m-code  EQ mch-act.m-code
          AND est-op.op-pass EQ mch-act.pass
          AND est-op.dept    EQ mch-act.dept
          AND est-op.line    LT 500
        NO-ERROR.

    ld-msf = 0.
    IF AVAIL ef THEN DO:
      IF mach.d-seq LT lv-rc-seq THEN
        ld-msf = ef.nsh-len * ef.nsh-wid.
      ELSE
        ld-msf = ef.gsh-len * ef.gsh-wid.

      IF v-corr THEN ld-msf = ld-msf * .007.
                ELSE ld-msf = ld-msf / 144.

      ld-msf = ld-msf / 1000.
    END.

    RELEASE w-ink.
    IF AVAIL est THEN DO:
      FIND xest WHERE ROWID(xest) EQ ROWID(est) NO-LOCK NO-ERROR.
      RUN ce/mach-ink.p.
      FIND FIRST w-ink
          WHERE w-ink.form-no EQ mch-act.frm
            AND w-ink.pass    EQ mch-act.pass
          NO-ERROR.
    END.

    li-up = 1.
    IF AVAIL job-mch THEN DO:
      IF job-mch.n-out GT 0 THEN li-up = job-mch.n-out.
      IF job-mch.n-on  GT 0 THEN li-up = job-mch.n-on / li-up.
      IF li-up EQ ? THEN li-up = 1.
    END.


    IF (mach.p-type = "R" OR mach.p-type = "S" OR mach.p-type = "B") THEN DO:     
        /*
        FOR each job-mat WHERE job-mat.company = mch-act.company
                 AND job-mat.job = mch-act.job
                 AND job-mat.job-no = mch-act.job-no
                 AND job-mat.job-no2 = mch-act.job-no2
                 AND job-mat.frm = mch-act.frm
                     use-index seq-idx NO-LOCK,
             FIRST item OF job-mat WHERE item.company EQ job-mat.company 
                 AND item.i-no EQ job-mat.rm-i-no
                 AND (ITEM.mat-type = "B" /*OR ITEM.mat-type = "G"*/) NO-LOCK:

             IF mach.p-type = "R" OR mach.p-type = "S" THEN
                ASSIGN ld-qty-ton = (mch-act.qty * job-mat.wid * job-mat.len / 144000 * ITEM.basis-w / 2000)
                       ld-qty-msf = (mch-act.qty * job-mat.wid * job-mat.len / 144000)
                                    .
             ELSE DO:
                FIND FIRST job-hdr WHERE job-hdr.company = job-mat.company
                                         AND job-hdr.job-no = job-mat.job-no
                                         AND job-hdr.job-no2 = job-mat.job-no2
                                         AND job-hdr.frm = job-mat.frm NO-LOCK NO-ERROR.
                FIND itemfg WHERE itemfg.company = job-hdr.company
                                        AND itemfg.i-no = job-hdr.i-no NO-LOCK NO-ERROR.
                ASSIGN ld-qty-msf = mch-act.qty * itemfg.t-sqin / 144000
                       ld-qty-ton = mch-act.qty * itemfg.t-sqin / 144000 * ITEM.basis-w / 2000
                                    .         

             END.
             LEAVE.
        END.
        */
        /*
        Blank Fed Machine use the (Blank W x L / 144) x  (Qty / 1000)
        Sheet Fed Machines use the (Net Sheet W x L  / 144) x (Qty / 1000)
        Roll Fed Machine use (Gross Sheet W x L/144 x (Qty / 1000)
       */

        IF mach.p-type = "R" THEN
           ld-qty-msf = mch-act.qty * ef.gsh-wid * ef.gsh-len / 1000 / 144.
        ELSE IF mach.p-type = "S" THEN
           ld-qty-msf = mch-act.qty * ef.nsh-wid * ef.nsh-len / 1000 / 144.
        ELSE IF mach.p-type = "B" THEN
           ld-qty-msf = mch-act.qty * eb.t-wid * eb.t-len / 1000 / 144.

         ld-tot-msf = ld-tot-msf + ld-qty-msf.
     END.  
     /* Task 11251304 */
      FIND FIRST item WHERE item.company EQ cocode 
                 AND item.i-no EQ ef.adder[1]
                 AND (ITEM.mat-type = "A") NO-LOCK NO-ERROR.
         IF AVAIL ITEM THEN
             ASSIGN v-adder = ITEM.i-no .
         ELSE v-adder = "".  /* Task 11251304 */

     IF tb_excel THEN DO:

      lv-out =
          TRIM(STRING(mch-act.op-date,"99/99/99"))                      + "," +
          TRIM(job.job-no) + "-" + STRING(job.job-no2,"99")             + "," +
          TRIM(mch-act.m-code)                                          + "," +
          TRIM(string(eb.form-no))                                      + "," +
          TRIM(ef.board)                                               + "," +
          TRIM(v-adder)                                               + "," +
          TRIM(IF AVAIL eb THEN eb.style ELSE "")                       + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.t-wid,">>>>9.9<<<<") ELSE "") + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.t-len,">>>>9.9<<<<") ELSE "") + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.gsh-wid,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.gsh-len,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.nsh-wid,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.nsh-len,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(STRING(mch-act.qty,">>>>>>>>>>"))                        + "," +
          STRING(ld-qty-msf)                                            + ",," +
          TRIM(IF AVAIL eb THEN eb.cust-no ELSE "")                     + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.len,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.wid,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.dep,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.num-up,">9") ELSE "")         + "," +
          TRIM(IF AVAIL eb THEN STRING(get-colors(eb.est-no, eb.form-no),">9") ELSE "")
          .
     /*     
          TRIM(STRING(mch-act.frm,">>9"))                               + "," +
          TRIM(STRING(mch-act.blank-no,">>9"))                          + "," +
          TRIM(mch-act.code)                                            + "," +
          TRIM(IF AVAIL job-code THEN job-code.cat ELSE "")             + "," +

          TRIM(STRING(mch-act.shift,">>"))                              + "," +
          TRIM(STRING(mch-act.hours,">>>>>>>>>9.9<<"))                  + "," +
          TRIM(STRING(mch-act.qty,">>>>>>>>>>"))                        + "," +
          TRIM(STRING(mch-act.waste,">>>>>>>>>>"))                      + "," +
          TRIM(IF AVAIL eb THEN eb.stock-no ELSE "")                    + "," +

          TRIM(IF AVAIL eb THEN STRING(eb.len,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.wid,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.dep,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.t-len,">>>>9.9<<<<") ELSE "") + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.t-wid,">>>>9.9<<<<") ELSE "") + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.t-sqin,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN ef.board ELSE "")                       + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.cal,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(STRING(ld-msf,">>>>9.9<<<<"))                            + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.weight,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.roll-wid,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.gsh-wid,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.gsh-len,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.nsh-wid,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.nsh-len,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL est-flm THEN STRING(est-flm.len,">>>>9.9<<<<")
                                ELSE "")                                + "," +
          TRIM(IF AVAIL est-flm THEN STRING(est-flm.wid,">>>>9.9<<<<")
                                ELSE "")                                + "," +
          TRIM(IF AVAIL w-ink THEN STRING(w-ink.inks + w-ink.varn,">>")
                              ELSE "")                                  + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.die-in,">>>>9") ELSE "")      + "," +
          TRIM(STRING(li-up,">>>"))                                     + "," +
          TRIM(IF AVAIL job-mch THEN STRING(job-mch.n-out,">>>") ELSE "")
                                                                        + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.lin-in,">>>>9.9<<<<") ELSE "").
      */
         IF lv-out NE ? THEN PUT STREAM st-excell UNFORMATTED lv-out SKIP.
     END.
  END.  /* for each mch-act */
END.  /* rsQty = "A" */
ELSE DO:   /* rsQty = "E" */    

      FOR EACH job-mch NO-LOCK
      WHERE job-mch.company EQ cocode
        AND job-mch.job-no  GE begin_job-no
        AND job-mch.job-no LE END_job-no
        AND job-mch.m-code GE begin_mach
        AND job-mch.m-code LE END_mach
        ,
        FIRST job OF job-mch NO-LOCK WHERE job.close-date GE begin_date
                    AND job.close-date LE END_date
          ,
       FIRST mach NO-LOCK
      WHERE mach.company EQ job-mch.company
        AND mach.m-code  EQ job-mch.m-code:

          {custom/statusMsg.i " 'Processing Job#  '  + job.job-no "}

    IF TRIM(job.est-no) NE "" THEN
    FIND FIRST est NO-LOCK
        WHERE est.company EQ job.company
          AND est.est-no  EQ job.est-no
        NO-ERROR.

    IF AVAIL est THEN
    FIND FIRST ef NO-LOCK
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ job-mch.frm
        NO-ERROR.    

    IF AVAIL ef THEN
     FIND FIRST eb NO-LOCK
       WHERE eb.company   EQ ef.company
         AND eb.est-no    EQ ef.est-no
         AND eb.form-no   EQ ef.form-no
         AND (eb.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0)
       NO-ERROR.

    IF tb_styles AND AVAIL eb THEN
          IF LOOKUP(eb.style,v-sel-style-list) = 0 THEN NEXT.

  IF (mach.p-type = "R" OR mach.p-type = "S" OR mach.p-type = "B") THEN DO:     
               IF mach.p-type = "R" THEN
           ld-qty-msf = job-mch.run-qty * ef.gsh-wid * ef.gsh-len / 1000 / 144.
        ELSE IF mach.p-type = "S" THEN
           ld-qty-msf = job-mch.run-qty * ef.nsh-wid * ef.nsh-len / 1000 / 144.
        ELSE IF mach.p-type = "B" THEN
           ld-qty-msf = job-mch.run-qty * eb.t-wid * eb.t-len / 1000 / 144.

         ld-tot-msf = ld-tot-msf + ld-qty-msf.
     END.  
  /* Task 11251304 */
    FIND FIRST item WHERE item.company EQ cocode 
                 AND item.i-no EQ ef.adder[1]
                 AND (ITEM.mat-type = "A") NO-LOCK NO-ERROR .
         IF AVAIL ITEM THEN
             ASSIGN v-adder = ITEM.i-no .
         ELSE v-adder = "".  /* Task 11251304 */

     IF tb_excel THEN DO:
      lv-out =
          TRIM(STRING(job.close-date,"99/99/99"))                      + "," +
          TRIM(job.job-no) + "-" + STRING(job.job-no2,"99")             + "," +
          TRIM(job-mch.m-code)                                          + "," +
          TRIM(string(eb.form-no))                                     + "," +
          TRIM(ef.board)                                               + "," +
          TRIM(v-adder)                                               + "," +
          TRIM(IF AVAIL eb THEN eb.style ELSE "")                       + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.t-wid,">>>>9.9<<<<") ELSE "") + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.t-len,">>>>9.9<<<<") ELSE "") + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.gsh-wid,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.gsh-len,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.nsh-wid,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(IF AVAIL ef THEN STRING(ef.nsh-len,">>>>9.9<<<<")
                           ELSE "")                                     + "," +
          TRIM(STRING(job-mch.run-qty,">>>>>>>>>>"))                    + "," +
          STRING(ld-qty-msf)                                            + ",," +
          TRIM(IF AVAIL eb THEN eb.cust-no ELSE "")                     + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.len,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.wid,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.dep,">>>>9.9<<<<") ELSE "")   + "," +
          TRIM(IF AVAIL eb THEN STRING(eb.num-up,">9") ELSE "")         + "," +
          TRIM(IF AVAIL eb THEN STRING(get-colors(eb.est-no, eb.form-no),">9") ELSE "").
        IF lv-out NE ? THEN PUT STREAM st-excell UNFORMATTED lv-out SKIP.
     END.
  END.  /* for each job-mch */
END.       /* rsQty = "E" */

  IF tb_excel THEN DO:
    lv-out =
      ""                      + "," +
      ""             + "," +
      ""             + "," +
      ""             + "," +
      ""             + "," +
      ""             + "," +
        ""             + "," +
        ""             + "," +
        ""             + "," +
        ""             + "," +
        ""             + "," +
      "" + "," + STRING(ld-tot-msf)
      .
    IF lv-out NE ? THEN PUT STREAM st-excell UNFORMATTED lv-out SKIP.
  END.


SESSION:SET-WAIT-STATE("").

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

IF tb_excel THEN DO:
  OUTPUT STREAM st-excell CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-frame-hdl as handle no-undo.
  def var lv-group-hdl as handle no-undo.
  def var lv-field-hdl as handle no-undo.
  def var lv-field2-hdl as handle no-undo.
  def var parm-fld-list as cha no-undo.
  def var parm-lbl-list as cha no-undo.
  def var i as int no-undo.
  def var lv-label as cha.

  lv-frame-hdl = frame {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:first-child.
  lv-field-hdl = lv-group-hdl:first-child .

  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," 
                     .
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     .
              lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name then do:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".
                  end.
                  lv-field2-hdl = lv-field2-hdl:next-sibling.                 
              end.       
           end.                 
        end.            
     lv-field-hdl = lv-field-hdl:next-sibling.   
  end.

  put space(28)
      "< Selection Parameters >"
      skip(1).

  do i = 1 to num-entries(parm-fld-list,","):
    if entry(i,parm-fld-list) ne "" or
       entry(i,parm-lbl-list) ne "" then do:

      lv-label = fill(" ",34 - length(trim(entry(i,parm-lbl-list)))) +
                 trim(entry(i,parm-lbl-list)) + ":".

      put lv-label format "x(35)" at 5
          space(1)
          trim(entry(i,parm-fld-list)) format "x(40)"
          skip.              
    end.
  end.

  put fill("-",80) format "x(80)" skip.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-colors C-Win 
FUNCTION get-colors RETURNS INTEGER
    (in-est AS CHAR, in-form AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR v-col-list AS CHAR NO-UNDO.
DEF VAR i          AS INT  NO-UNDO.
DEF BUFFER bf-eb FOR eb.

v-col-list = "".
FOR EACH bf-eb WHERE bf-eb.company EQ cocode 
                 AND bf-eb.est-no  EQ in-est
                 AND bf-eb.form-no EQ in-form
               NO-LOCK.  
 /* task 11251304  */
 IF est.est-type >= 1 and est.est-type <= 4 THEN do:
  DO i = 1 TO 10:
      IF bf-eb.i-code2[i] GT "" THEN DO:
          IF  LOOKUP(bf-eb.i-code2[i], v-col-list) = 0 THEN
              v-col-list = v-col-list + bf-eb.i-code2[i] + ",".
      END.
  END.
 END.
 IF est.est-type >= 5 THEN do:
  DO i = 1 TO 10:
      IF bf-eb.i-code[i] GT "" THEN DO:
          IF  LOOKUP(bf-eb.i-code[i], v-col-list) = 0 THEN
              v-col-list = v-col-list + bf-eb.i-code[i] + ",".
      END.
  END.
 END.  /* task 11251304  */


END.
v-col-list = TRIM(v-col-list, ",").
RETURN NUM-ENTRIES(v-col-list).   /* Function return */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

