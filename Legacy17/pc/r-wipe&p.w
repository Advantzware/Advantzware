&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: pc\r-wipe&p.w

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
DEFINE VARIABLE tb_excel AS LOG NO-UNDO. /* To prevent compile error as mch-edit.i contains this var */
DEFINE STREAM excel . /* To prevent compile error as mch-edit.i contains this stream */
/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_IS_RUNNING) NE 0 &THEN
DEF VAR ip-post AS LOG INIT NO NO-UNDO.
&ELSE
DEF INPUT PARAMETER ip-post AS LOG NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEF VAR ll-ok-to-post AS LOG NO-UNDO.

def var v-autopost  as   log NO-UNDO.
def var v-auto-bin  like sys-ctrl.char-fld no-undo.
def var v-rm-fg     as   log NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

def TEMP-TABLE w-job NO-UNDO field job like job.job.

{pc/pcprdd4u.i NEW}

{jc/jcgl-sh.i NEW}

DEF BUFFER bf-prdd FOR pc-prdd.

DO TRANSACTION:
  {sys/inc/dcpostgl.i}
  {sys/inc/tspost.i}
  {sys/inc/tspostfg.i}
  {sys/inc/fgrecpt.i}
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_date end_date begin_mach end_mach ~
begin_job-no begin_job-no2 end_job-no end_job-no2 begin_shift end_shift ~
tb_tot-hrs tb_pg-brk rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
btn-ok btn-cancel RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_mach end_mach ~
begin_job-no begin_job-no2 end_job-no end_job-no2 begin_shift end_shift ~
tb_tot-hrs tb_pg-brk rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
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
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Machine#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_shift AS INTEGER FORMAT ">>" INITIAL 1 
     LABEL "Beginning Shift" 
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
     LABEL "Ending Machine#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_shift AS INTEGER FORMAT ">>" INITIAL 99 
     LABEL "Ending Shift" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

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

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
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

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 92 BY 7.14.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 10.71.

DEFINE VARIABLE tb_pg-brk AS LOGICAL INITIAL no 
     LABEL "Print Department Page Break?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE tb_tot-hrs AS LOGICAL INITIAL yes 
     LABEL "Print Total Hours?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_date AT ROW 2.67 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 2.67 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_mach AT ROW 3.62 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Machine Number"
     end_mach AT ROW 3.62 COL 69 COLON-ALIGNED HELP
          "Enter Ending Machine Number"
     begin_job-no AT ROW 4.57 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 4.57 COL 38 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 4.57 COL 69 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 4.57 COL 81 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_shift AT ROW 5.52 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Machine Number"
     end_shift AT ROW 5.52 COL 69 COLON-ALIGNED HELP
          "Enter Beginning Machine Number"
     tb_tot-hrs AT ROW 7.19 COL 31
     tb_pg-brk AT ROW 8.38 COL 65 RIGHT-ALIGNED
     rd-dest AT ROW 13.38 COL 6 NO-LABEL
     lv-ornt AT ROW 13.62 COL 31 NO-LABEL
     lines-per-page AT ROW 13.62 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 15.76 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 17.19 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17.43 COL 6
     btn-ok AT ROW 19.81 COL 19
     btn-cancel AT ROW 19.81 COL 57
     RECT-6 AT ROW 11.95 COL 2
     RECT-7 AT ROW 1.24 COL 1
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.43 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 21.57.


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
         TITLE              = "Production Control Edit List"
         HEIGHT             = 21.81
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
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
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
       begin_shift:PRIVATE-DATA IN FRAME FRAME-A     = 
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

ASSIGN 
       end_shift:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_pg-brk IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_pg-brk:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_tot-hrs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Production Control Edit List */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Production Control Edit List */
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
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_shift C-Win
ON LEAVE OF begin_shift IN FRAME FRAME-A /* Beginning Shift */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEF VAR lv-post AS LOG NO-UNDO.


  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "AUTOPOST"
      no-lock no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "AUTOPOST"
     sys-ctrl.descrip = "Autopost to Finished Goods Receipts?".
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  end.
  assign
   v-autopost = sys-ctrl.log-fld
   v-auto-bin = sys-ctrl.char-fld.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "RM=FG"
      no-lock no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "RM=FG"
     sys-ctrl.descrip = "Validate RM issues = FG Produced Plus Waste?"
     sys-ctrl.log-fld = no.
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  end.
  v-rm-fg = sys-ctrl.log-fld.

  ll-ok-to-post = NO.

  FOR EACH tt-report:
    DELETE tt-report.
  END.

  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.

  IF ip-post THEN DO:
    IF ll-ok-to-post THEN DO:
      lv-post = NO.

      MESSAGE "Post WIP?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE lv-post.

      IF lv-post THEN do:
        RUN post-wip.

        MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
      END.
    END.

    ELSE MESSAGE "No WIP available for posting..." VIEW-AS ALERT-BOX ERROR.
  END. 
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_shift C-Win
ON LEAVE OF end_shift IN FRAME FRAME-A /* Ending Shift */
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


&Scoped-define SELF-NAME tb_pg-brk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_pg-brk C-Win
ON VALUE-CHANGED OF tb_pg-brk IN FRAME FRAME-A /* Print Department Page Break? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_tot-hrs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tot-hrs C-Win
ON VALUE-CHANGED OF tb_tot-hrs IN FRAME FRAME-A /* Print Total Hours? */
DO:
  assign {&self-name}.
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

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
    begin_date  = TODAY
    end_date    = TODAY
    c-win:TITLE = IF ip-post THEN "Transfer WIP to Job Cost"
                             ELSE "WIP Edit List".

  RUN enable_UI.

  {methods/nowait.i}
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images1.p */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY begin_date end_date begin_mach end_mach begin_job-no begin_job-no2 
          end_job-no end_job-no2 begin_shift end_shift tb_tot-hrs tb_pg-brk 
          rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_date end_date begin_mach end_mach begin_job-no begin_job-no2 
         end_job-no end_job-no2 begin_shift end_shift tb_tot-hrs tb_pg-brk 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok 
         btn-cancel RECT-6 RECT-7 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-wip C-Win 
PROCEDURE post-wip :
/* --------------------------------------------------- pc/pc-post.p  8/94 gb  */
/* Production Control - Posting Entry                                         */
/* -------------------------------------------------------------------------- */

def var v-loc       like fg-bin.loc NO-UNDO.
def var v-loc-bin   like fg-bin.loc-bin NO-UNDO.
def var v-up-hs     like eb.num-up NO-UNDO.
def var v-up        like eb.num-up NO-UNDO.
def var v-out       like est-op.n-out NO-UNDO.
def var v-on        like eb.num-up NO-UNDO.
def var v-est-type  like est.est-type NO-UNDO.


FOR EACH w-job:
  DELETE w-job.
END.

FOR EACH tt-report NO-LOCK,

    FIRST pc-prdd WHERE RECID(pc-prdd) EQ tt-report.rec-id,

    first mach
    {sys/ref/machW.i}
      and mach.m-code eq pc-prdd.m-code
    no-lock,

    first job
    where job.company eq cocode
      and job.job     eq pc-prdd.job
      and job.job-no  eq pc-prdd.job-no
      and job.job-no2 eq pc-prdd.job-no2

    break by pc-prdd.m-code

    TRANSACTION:

  find first w-job where w-job.job eq job.job no-error.
  if not avail w-job then create w-job.
  w-job.job = job.job.

  assign
   v-up  = 1
   v-out = 1
   v-on  = 1.

  FIND FIRST est
      WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
      NO-LOCK NO-ERROR.
  v-est-type = IF AVAIL est THEN est.est-type ELSE 1.
  IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

  FOR EACH mach-part WHERE
      mach-part.company EQ mach.company AND
      mach-part.m-code EQ mach.m-code
      EXCLUSIVE-LOCK:

      mach-part.total-impressions-run = mach-part.total-impressions-run
                                      + pc-prdd.qty + pc-prdd.waste.

      FIND FIRST reftable WHERE
           reftable.reftable EQ "MACHPARTHOURS" AND
           reftable.company  EQ mach-part.company AND
           reftable.loc      EQ mach-part.m-code AND
           reftable.code     EQ mach-part.rm-part-code
           EXCLUSIVE-LOCK NO-ERROR.

      IF NOT AVAIL reftable THEN DO:
         CREATE reftable.
         ASSIGN
            reftable.reftable = "MACHPARTHOURS"
            reftable.company  = mach-part.company
            reftable.loc      = mach-part.m-code
            reftable.code     = mach-part.rm-part-code. 
      END.

      reftable.val[1] = reftable.val[1]
                      + pc-prdd.hours.

      RELEASE reftable.
  END.

  IF mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
     mach.dept[3] EQ "PR" OR mach.dept[4] EQ "PR" THEN
    RUN update-plate-die (ROWID(pc-prdd), "P", v-est-type).

  IF mach.dept[1] EQ "DC" OR mach.dept[2] EQ "DC" OR
     mach.dept[3] EQ "DC" OR mach.dept[4] EQ "DC" THEN
    RUN update-plate-die (ROWID(pc-prdd), "D", v-est-type).

  assign
   v-up  = 1
   v-out = 1
   v-on  = 1.

  if avail est and INDEX("AP",mach.p-type) LE 0 then do:
    run sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, output v-up).

    find first ef
        where ef.company eq est.company
          and ef.est-no  eq est.est-no
          and ef.form-no eq pc-prdd.frm
        no-lock no-error.

    IF AVAIL ef THEN DO:
      RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).  
      v-on = v-up * v-on.
    END.

    find first est-op
        where est-op.company eq est.company
          and est-op.est-no  eq est.est-no
          and est-op.s-num   eq pc-prdd.frm
          and (est-op.b-num  eq pc-prdd.blank-no or
               pc-prdd.blank-no eq 0)
          and est-op.m-code  eq pc-prdd.m-code
          and est-op.op-pass eq pc-prdd.pass
          and est-op.dept    eq pc-prdd.dept
          and est-op.line    lt 500
        no-lock no-error.

    if ((avail est-op) and est-op.op-sb)           or
       ((not avail est-op) and mach.p-type ne "B") then do:

      if avail est-op then
        run sys/inc/numout.p (recid(est-op), output v-out).

      else v-out = 1.

      v-up = v-up * v-out.
    end.

    else v-up = 1.

    v-on = v-on / v-up.
  end.

  v-up-hs = 1.

  if pc-prdd.dept eq "HS" and
     avail est            and
     mach.therm           and
     mach.p-type eq "S"   then
    run sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, output v-up-hs).

  {pc/pcmchact.i}

  if pc-prdd.complete and v-autopost then do:
    FIND LAST job-mch
        WHERE job-mch.company EQ pc-prdd.company
          AND job-mch.job     EQ job.job
          AND job-mch.job-no  EQ pc-prdd.job-no
          AND job-mch.job-no2 EQ pc-prdd.job-no2
          AND (job-mch.frm    EQ pc-prdd.frm OR v-est-type EQ 2)
        USE-INDEX line-idx NO-LOCK NO-ERROR.

    IF job-mch.blank-no NE 0 THEN
    FIND LAST job-mch
        WHERE job-mch.company   EQ pc-prdd.company
          AND job-mch.job       EQ job.job
          AND job-mch.job-no    EQ pc-prdd.job-no
          AND job-mch.job-no2   EQ pc-prdd.job-no2
          AND (job-mch.frm      EQ pc-prdd.frm OR v-est-type EQ 2)
          AND (job-mch.blank-no EQ pc-prdd.blank-no OR
               mach.p-type      NE "B"              OR
               v-est-type       EQ 1)
        USE-INDEX line-idx NO-LOCK NO-ERROR.

    if avail job-mch                    and
       job-mch.m-code eq pc-prdd.m-code and
       job-mch.pass   eq pc-prdd.pass   and
       pc-prdd.code   eq "RUN"          then
    for each job-hdr
        where job-hdr.company   eq cocode
          and job-hdr.job-no    eq job-mch.job-no
          and job-hdr.job-no2   eq job-mch.job-no2
          and (job-mch.frm      eq pc-prdd.frm OR
               v-est-type       EQ 2)
          and (job-hdr.blank-no eq job-mch.blank-no or
               job-mch.blank-no eq 0)
        no-lock,

        first itemfg
        where itemfg.company    eq cocode
          and itemfg.i-no       eq job-hdr.i-no
          and itemfg.case-count gt 0
        no-lock:

      x = 1.
      FOR EACH fg-rctd no-lock BY fg-rctd.r-no DESC:
        LEAVE.
      END.
      if avail fg-rctd then x = fg-rctd.r-no.

      find last fg-rcpth use-index r-no no-lock no-error.
      if avail fg-rcpth and fg-rcpth.r-no GT x then x = fg-rcpth.r-no.

      create fg-rctd.
      assign
       fg-rctd.r-no       = X + 1
       fg-rctd.rct-date   = pc-prdd.op-date
       fg-rctd.trans-time = pc-prdd.op-time
       fg-rctd.company    = cocode
       fg-rctd.rita-code  = "R"
       fg-rctd.i-name     = itemfg.i-name
       fg-rctd.i-no       = job-hdr.i-no
       fg-rctd.job-no     = pc-prdd.job-no
       fg-rctd.job-no2    = pc-prdd.job-no2.

      assign
       v-up  = 1
       v-out = 1.

      if avail est and index("APB",mach.p-type) le 0 then do:
        run sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, output v-up).

        find first est-op
            where est-op.company eq est.company
              and est-op.est-no  eq est.est-no
              and est-op.s-num   eq pc-prdd.frm
              and (est-op.b-num  eq pc-prdd.blank-no or
                   pc-prdd.blank-no eq 0)
              and est-op.m-code  eq pc-prdd.m-code
              and est-op.op-pass eq pc-prdd.pass
              and est-op.dept    eq pc-prdd.dept
              and est-op.line    lt 500
            no-lock no-error.
        if avail est-op and est-op.n-out ne 0 then v-out = est-op.n-out.
      end.

      ASSIGN
       fg-rctd.b-num    = pc-prdd.blank-no
       fg-rctd.s-num    = pc-prdd.frm
       fg-rctd.t-qty    = pc-prdd.qty / v-up-hs * v-out * v-up
       fg-rctd.pur-uom  = itemfg.prod-uom
       fg-rctd.cost-uom = itemfg.prod-uom
       fg-rctd.std-cost = job-hdr.std-tot-cost
       fg-rctd.ext-cost = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
       fg-rctd.qty-case = itemfg.case-count
       fg-rctd.partial  = fg-rctd.t-qty modulo itemfg.case-count
       fg-rctd.cases    = trunc(fg-rctd.t-qty / itemfg.case-count,0).

      if fg-rctd.t-qty le 0 then fg-rctd.cases = 0.

      release fg-bin.

      if v-auto-bin eq "ShipTo" then do:
        /*get estimate blank file from finished goods item file*/
        find first eb
            where eb.company  eq cocode
              and eb.est-no   eq itemfg.est-no
              and eb.stock-no eq itemfg.i-no
            use-index est-no no-lock no-error.

        if avail eb then do:
          /*get customer file from estimate blank file*/
          find first cust
              where cust.company eq cocode
                and cust.cust-no eq eb.cust-no
              no-lock no-error.
          if avail cust then do:              
            find first shipto
                where shipto.company = cocode
                  and shipto.cust-no = cust.cust-no 
                no-lock no-error.
            if avail shipto then do:
              find first fg-bin
                  where fg-bin.company eq cocode
                    and fg-bin.loc     eq shipto.loc
                    and fg-bin.loc-bin eq shipto.loc-bin
                    and fg-bin.i-no    eq ""
                  no-lock no-error.
              if avail fg-bin then 
                ASSIGN
                 v-loc     = shipto.loc
                 v-loc-bin = shipto.loc-bin.
            end.

            if v-loc eq "" and v-loc-bin eq "" then do:
              find first fg-bin
                  where fg-bin.company eq cocode
                    and fg-bin.loc     eq itemfg.def-loc
                    and fg-bin.loc-bin eq itemfg.def-loc-bin
                    and fg-bin.i-no    eq ""
                  no-lock no-error.
              if avail fg-bin then 
                assign 
                 v-loc     = itemfg.def-loc
                 v-loc-bin = itemfg.def-loc-bin.
            end. /*if avail shipto*/
          end. /*if avail cust*/
        end. /*if avail eb*/
      end. /*if system default is shipto*/
      /*else if "FGFILE" then get from finished goods file*/
      else do:
        find first fg-bin
            where fg-bin.company eq cocode
              and fg-bin.loc     eq itemfg.def-loc
              and fg-bin.loc-bin eq itemfg.def-loc-bin
              and fg-bin.i-no    eq ""
            no-lock no-error.
        if avail fg-bin then 
          ASSIGN
           v-loc     = itemfg.def-loc
           v-loc-bin = itemfg.def-loc-bin.
      end. /*else FGFILE*/

      /*if bin and warehouse are blank, goto cust "X" shipto file*/
      if v-loc eq "" and v-loc-bin eq "" then do:
        find first cust
            where cust.company eq cocode
              and cust.active  eq "X"
            no-lock no-error.

        if avail cust then do:
          find first shipto
              where shipto.company eq cocode
                and shipto.cust-no eq cust.cust-no  
              no-lock no-error.
          if avail shipto then do:
            find first fg-bin
                where fg-bin.company eq cocode
                  and fg-bin.loc     eq shipto.loc
                  and fg-bin.loc-bin eq shipto.loc-bin
                  and fg-bin.i-no    eq ""
                no-lock no-error.
             ASSIGN
              v-loc     = shipto.loc
              v-loc-bin = shipto.loc-bin.
          end.                                  
        end.
      end.

      ASSIGN
       fg-rctd.loc     = v-loc
       fg-rctd.loc-bin = v-loc-bin.

      FIND FIRST fg-bin
          WHERE fg-bin.company EQ fg-rctd.company
            AND fg-bin.i-no    EQ fg-rctd.i-no
            AND fg-bin.job-no  EQ pc-prdd.job-no
            AND fg-bin.job-no2 EQ pc-prdd.job-no2
            AND fg-bin.loc     EQ fg-rctd.loc
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin
            AND fg-bin.tag     EQ fg-rctd.tag
          NO-LOCK NO-ERROR.
      IF AVAIL fg-bin THEN fg-rctd.cases-unit = fg-bin.cases-unit.

      RUN fg/comprcpt.p (ROWID(fg-rctd)).
    end.
  end.

  delete pc-prdd.
end. /* for each pc-prdd */

for each w-job,
    first job
    where job.company eq cocode
      and job.job     eq w-job.job
    no-lock:
  run jc/job-cls2.p (recid(job)).
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ----------------------------------------------- pc/rep/mch-edit.p 8/94 gb  */
/* Production Control -transactions edit list                                 */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var v-date like pc-prdd.op-date extent 2 format "99/99/9999" init TODAY no-undo.
def var v-job-no like job.job-no extent 2 init ["","zzzzzz"] no-undo.
def var v-job-no2 like job.job-no2 extent 2 format "99" init [0,99] no-undo.
def var v-m-code like mach.m-code extent 2 init ["","zzzzzz"] no-undo.
def var v-shift like pc-prdd.shift extent 2 no-undo.

def var doe    as log init true.
def var dor    as log init true.
def var detail as log init false.
def var v-value as dec format "->>,>>>,>>9.99".
def var v-toth  as log init true.
def var tothour like pc-prdd.hours.
def var totqty  like pc-prdd.qty.
def var totwst  like pc-prdd.waste.
def var uline as char format "x(44)".
def var totchar as char format "x(15)" init "Machine Totals:".
def var v-start as char format "x(5)".
def var v-stopp as char format "x(5)".
def var v-comp as char format "x".
def var v-dept-paging as log init no.
def var v-recid as recid.
def var v-tot-rm    like mat-act.qty NO-UNDO.
def var v-tot-fg    like mch-act.qty NO-UNDO.     
def var v-up-hs     like eb.num-up NO-UNDO.
def var v-up        like eb.num-up NO-UNDO.
def var v-out       like est-op.n-out NO-UNDO.
def var v-on        like eb.num-up NO-UNDO.
def var v-est-type  like est.est-type NO-UNDO.
DEF VAR ll-one-item AS LOG NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
def var vmr-crusiz  like mach.mr-crusiz.
def var vrun-crusiz like mach.run-crusiz.
def var v-dscr          like account.dscr.
def var v-disp-actnum   like account.actnum.
def var v-disp-amt      as   dec format ">>,>>>,>>9.99cr".

form
    pc-prdd.m-code column-label "MACH"
    mach.m-dscr column-label "DESCRIPT" format "x(10)"
    mach.dept[1] column-label "DP"
    pc-prdd.op-date column-label "DATE" FORMAT "99/99/99"
    space(0)
    pc-prdd.shift column-label "SH" format ">>"
    pc-prdd.job-no column-label "  JOB #" space(0) "-" space(0)
    pc-prdd.job-no2 no-label format "99"
    pc-prdd.frm column-label " S" space(0) "/" space(0)
    pc-prdd.blank-no column-label "/B"
    job-hdr.i-no column-label "ITEM #"
    itemfg.i-name column-label "ITEM DESCRIPTION" format "x(15)"
    pc-prdd.code column-label "CODE"
    pc-prdd.hours column-label "HOURS "
    v-start column-label "START"
    v-stopp column-label " STOP"
    pc-prdd.crew column-label "CR" format ">9"
    pc-prdd.qty format "->>>>>>>9" column-label "RUN QTY"
    pc-prdd.waste format "->>>>9" column-label "WASTE"
    v-comp column-label "C"
    with frame mch-edit no-box down STREAM-IO width 132.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-m-code[1]   = begin_mach
 v-m-code[2]   = end_mach
 v-date[1]     = begin_date
 v-date[2]     = end_date
 v-job-no[1]   = fill(" ",6 - length(trim(begin_job-no))) +
                 trim(begin_job-no) + string(int(begin_job-no2),"99")
 v-job-no[2]   = fill(" ",6 - length(trim(end_job-no)))   +
                 trim(end_job-no)   + string(int(end_job-no2),"99")
 v-shift[1]    = begin_shift
 v-shift[2]    = end_shift
 v-toth        = tb_tot-hrs
 v-dept-paging = tb_pg-brk.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display "" with frame r-top.

for each pc-prdd
    where pc-prdd.company eq cocode
      and pc-prdd.m-code  ge v-m-code[1]
      and pc-prdd.m-code  le v-m-code[2]
      and pc-prdd.op-date ge v-date[1]
      and pc-prdd.op-date le v-date[2]
      and pc-prdd.shift   ge v-shift[1]
      and pc-prdd.shift   le v-shift[2]
      and pc-prdd.job-no  ge substr(v-job-no[1],1,6)
      and pc-prdd.job-no  le substr(v-job-no[2],1,6)
      and fill(" ",6 - length(trim(pc-prdd.job-no))) +
          trim(pc-prdd.job-no) + string(int(pc-prdd.job-no2),"99")
                          ge v-job-no[1]
      and fill(" ",6 - length(trim(pc-prdd.job-no))) +
          trim(pc-prdd.job-no) + string(int(pc-prdd.job-no2),"99")
                          le v-job-no[2]
      and ((pc-prdd.stopp - pc-prdd.start
                          ne 0) or
           (pc-prdd.qty   ne 0) or
           (pc-prdd.waste ne 0))
    no-lock,

    first mach
    {sys/ref/machW.i}
      and mach.m-code eq pc-prdd.m-code
    no-lock,

    first job
    where job.company eq cocode
      and job.job     eq pc-prdd.job
      and job.job-no  eq pc-prdd.job-no
      and job.job-no2 eq pc-prdd.job-no2
    NO-LOCK:

  assign
   v-up  = 1
   v-out = 1
   v-on  = 1.

  FIND FIRST est
      WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
      NO-LOCK NO-ERROR.
  v-est-type = IF AVAIL est THEN est.est-type ELSE 1.
  IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

  assign
   v-up  = 1
   v-out = 1
   v-on  = 1.

  if avail est and INDEX("AP",mach.p-type) LE 0 then do:
    run sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, output v-up).

    find first ef
        where ef.company eq est.company
          and ef.est-no  eq est.est-no
          and ef.form-no eq pc-prdd.frm
        no-lock no-error.

    if avail ef then
      v-on = v-up *
             (if ef.n-out   eq 0 then 1 else ef.n-out) *
             (if ef.n-out-l eq 0 then 1 else ef.n-out-l) *
             (if ef.n-out-d eq 0 then 1 else ef.n-out-d).

    find first est-op
        where est-op.company eq est.company
          and est-op.est-no  eq est.est-no
          and est-op.s-num   eq pc-prdd.frm
          and (est-op.b-num  eq pc-prdd.blank-no or
               pc-prdd.blank-no eq 0)
          and est-op.m-code  eq pc-prdh.m-code
          and est-op.op-pass eq pc-prdd.pass
          and est-op.dept    eq pc-prdd.dept
          and est-op.line    lt 500
        no-lock no-error.

    if ((avail est-op) and est-op.op-sb)           or
       ((not avail est-op) and mach.p-type ne "B") then do:

      if avail est-op then
        run sys/inc/numout.p (recid(est-op), output v-out).

      else v-out = 1.

      v-up = v-up * v-out.
    end.

    else v-up = 1.

    v-on = v-on / v-up.
  end.

  v-up-hs = 1.

  if pc-prdd.dept eq "HS" and
     avail est            and
     mach.therm           and
     mach.p-type eq "S"   then
    run sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, output v-up-hs).

  if v-rm-fg then do:      /* validate rm > fg produced + waste */
    assign
     v-tot-rm = 0
     v-tot-fg = (pc-prdd.qty + pc-prdd.waste) / v-up-hs.

    for each mch-act
        where mch-act.company  eq cocode
          and mch-act.job      eq job.job
          and mch-act.job-no   eq job.job-no
          and mch-act.job-no2  eq job.job-no2
          and mch-act.frm      eq pc-prdd.frm
          and mch-act.blank-no eq pc-prdd.blank-no
          and mch-act.pass     eq pc-prdd.pass
          and mch-act.m-code   eq pc-prdh.m-code
        use-index job no-lock:
      v-tot-fg = v-tot-fg + (mch-act.qty + mch-act.waste).
    end.

    v-tot-fg = v-tot-fg / v-on.

    release job-mat.
    for each job-mat
        where job-mat.company eq cocode
          and job-mat.job     eq job.job
          and job-mat.job-no  eq job.job-no
          and job-mat.job-no2 eq job.job-no2
          and job-mat.frm     eq pc-prdd.frm
        no-lock,
        first item
        where item.company    eq cocode
          and item.i-no       eq job-mat.i-no
          and item.mat-type   eq "B"
          and item.i-code     eq "R"
        no-lock:
      leave.
    end.

    if avail job-mat then do:
      for each mat-act
          where mat-act.company eq cocode
            and mat-act.job     eq job.job
            and mat-act.job-no  eq job.job-no
            and mat-act.job-no2 eq job.job-no2
            and mat-act.i-no    eq job-mat.i-no
            and mat-act.s-num   eq job-mat.frm
            and mat-act.b-num   eq job-mat.blank-no
          use-index job no-lock:
        v-tot-rm = v-tot-rm + mat-act.qty.
      end.

      if v-tot-fg gt v-tot-rm then next.
    end.
  end.

  CREATE tt-report.
  tt-report.rec-id = RECID(pc-prdd).
END.

if v-dept-paging then do:
  {pc/rep/mch-edit.i pc-prdd.dept "by pc-prdd.m-code"}
end.

else do:
  {pc/rep/mch-edit.i pc-prdd.m-code}
end.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-plate-die C-Win 
PROCEDURE update-plate-die :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid    AS   ROWID NO-UNDO.
  DEF INPUT PARAM ip-upd-type AS   CHAR NO-UNDO.
  DEF INPUT PARAM ip-est-type LIKE est.est-type NO-UNDO.

  DEF BUFFER b-pc-prdd FOR pc-prdd.


  FIND b-pc-prdd WHERE ROWID(b-pc-prdd) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-pc-prdd THEN

    FOR FIRST job
        WHERE job.company eq cocode
          AND job.job     eq b-pc-prdd.job
          AND job.job-no  eq b-pc-prdd.job-no
          AND job.job-no2 eq b-pc-prdd.job-no2
        NO-LOCK,

        FIRST job-hdr
        WHERE job-hdr.company   EQ cocode
          AND job-hdr.job       EQ b-pc-prdd.job
          AND job-hdr.job-no    EQ b-pc-prdd.job-no
          AND job-hdr.job-no2   EQ b-pc-prdd.job-no2
          AND (job-mch.frm      EQ b-pc-prdd.frm OR
               ip-est-type      EQ 2)
        NO-LOCK:

      FIND FIRST itemfg
          WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ job-hdr.i-no
          NO-LOCK NO-ERROR.

      IF ip-est-type EQ 2 AND job.est-no NE "" AND
         AVAIL itemfg AND itemfg.isaset        THEN
      FOR EACH eb
          WHERE eb.company EQ cocode
            AND eb.est-no  EQ job.est-no
            AND eb.form-no EQ b-pc-prdd.frm
          NO-LOCK,
          FIRST itemfg
          WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ eb.stock-no
          NO-LOCK:
        LEAVE.
      END.

      IF AVAIL itemfg THEN DO:

        IF ip-upd-type EQ "P" AND itemfg.plate-no NE "" THEN
        FIND FIRST prep
            WHERE prep.company EQ cocode
              AND prep.code    EQ itemfg.plate-no
            NO-ERROR.

        ELSE
        IF ip-upd-type EQ "D" AND itemfg.die-no NE "" THEN
        FIND FIRST prep
            WHERE prep.company EQ cocode
              AND prep.code    EQ itemfg.die-no
            NO-ERROR.

        IF AVAIL prep THEN DO:

          ASSIGN prep.no-of-impressions = prep.no-of-impressions +
                                          b-pc-prdd.qty + b-pc-prdd.waste
                 prep.last-date         = b-pc-prdd.op-date.

          FIND FIRST reftable WHERE
               reftable.reftable EQ "PREPLASTJOB" AND
               reftable.company  EQ prep.company AND
               reftable.loc      EQ prep.loc AND
               reftable.code     EQ prep.CODE
               NO-ERROR.

          IF NOT AVAIL reftable THEN DO:
            CREATE reftable.
            ASSIGN
              reftable.reftable = "PREPLASTJOB"
              reftable.company  = prep.company
              reftable.loc      = prep.loc
              reftable.code     = prep.CODE. 
          END.

          ASSIGN
            reftable.code2    = b-pc-prdd.job-no
            reftable.val[1]   = b-pc-prdd.job-no2.

          RELEASE reftable.
          RELEASE prep.
        END.
      END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

