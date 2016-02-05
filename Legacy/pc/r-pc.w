&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

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

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def stream mess.

def var save_id as RECID NO-UNDO.
def var time_stamp as ch NO-UNDO.
time_stamp = string(time,"hh:mmam").
DEF VAR prt-copies AS INT NO-UNDO.


def var v-date like pc-prdd.op-date extent 2 format "99/99/9999" init today
  no-undo.
def var v-job-no like job.job-no extent 2 init ["","zzzzzz"] no-undo.
def var v-job-no2 like job.job-no2 extent 2 format "99" init [0,99] no-undo.
def var v-m-code like mach.m-code extent 2 init ["","zzzzzz"] no-undo.
def var doe    as log init true no-undo.
def var dor    as log init true no-undo.
def var detail as log init false no-undo.
def var v-value as dec format "->>,>>>,>>9.99" no-undo.
def var v-toth  as log init true no-undo.
def var tothour like pc-prdd.hours no-undo.
def var totqty  like pc-prdd.qty no-undo.
def var totwst  like pc-prdd.waste no-undo.
def var uline as char format "x(44)" no-undo.
def var totchar as char format "x(15)" init "Machine Totals:" no-undo.
def var v-start as char format "x(5)" no-undo.
def var v-stopp as char format "x(5)" no-undo.
def var v-comp as char format "x" no-undo.
def var v-dept-paging as log init no no-undo.
def var v-recid as recid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_pc-date end_pc-date begin_mach ~
end_mach begin_job-no begin_job-no2 end_job-no end_job-no2 tb_tot-hour ~
tb_page-break rd-dest lines-per-page td-show-parm btn-ok btn-cancel RECT-6 ~
RECT-7 
&Scoped-Define DISPLAYED-OBJECTS begin_pc-date end_pc-date begin_mach ~
end_mach begin_job-no begin_job-no2 end_job-no end_job-no2 lbl_tot-hr ~
tb_tot-hour lbl_dept-brk tb_page-break rd-dest lines-per-page td-show-parm 

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

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)" INITIAL "0" 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE begin_job-no2 AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_pc-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE end_job-no2 AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzzzz" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_pc-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_dept-brk AS CHARACTER FORMAT "X(256)":U INITIAL "Dept Page Break?" 
     LABEL "Dept Page Break?" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_tot-hr AS CHARACTER FORMAT "X(256)":U INITIAL "Print Total Hour?" 
     LABEL "Print Total Hour?" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 55 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 92 BY 5.48.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 12.38.

DEFINE VARIABLE tb_page-break AS LOGICAL INITIAL yes 
     LABEL "Print Instructions?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE tb_tot-hour AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_pc-date AT ROW 1.95 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_pc-date AT ROW 1.95 COL 70 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_mach AT ROW 2.91 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Machine Code"
     end_mach AT ROW 2.91 COL 70 COLON-ALIGNED HELP
          "Enter Ending Machine Code"
     begin_job-no AT ROW 3.86 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 3.86 COL 39 COLON-ALIGNED NO-LABEL
     end_job-no AT ROW 3.86 COL 70 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 3.86 COL 82 COLON-ALIGNED NO-LABEL
     lbl_tot-hr AT ROW 6.48 COL 31 COLON-ALIGNED
     tb_tot-hour AT ROW 6.48 COL 51
     lbl_dept-brk AT ROW 7.43 COL 31 COLON-ALIGNED
     tb_page-break AT ROW 7.43 COL 54 RIGHT-ALIGNED
     rd-dest AT ROW 14.57 COL 11 NO-LABEL
     lines-per-page AT ROW 15.29 COL 72 COLON-ALIGNED
     td-show-parm AT ROW 16.95 COL 58
     btn-ok AT ROW 19.81 COL 19
     btn-cancel AT ROW 19.81 COL 57
     RECT-6 AT ROW 13.86 COL 2
     RECT-7 AT ROW 1 COL 1
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.62 COL 5
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
         TITLE              = "Production Control Edit Listing"
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("images\progress":U) THEN
    MESSAGE "Unable to load icon: images\progress"
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
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_pc-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_pc-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_dept-brk IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_tot-hr IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_page-break IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_page-break:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_tot-hour:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Production Control Edit Listing */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Production Control Edit Listing */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_pc-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_pc-date C-Win
ON LEAVE OF begin_pc-date IN FRAME FRAME-A /* Beginning Date */
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
  assign rd-dest.
       
  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case. 

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


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_pc-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_pc-date C-Win
ON LEAVE OF end_pc-date IN FRAME FRAME-A /* Ending Date */
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


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_page-break
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_page-break C-Win
ON VALUE-CHANGED OF tb_page-break IN FRAME FRAME-A /* Print Instructions? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_tot-hour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tot-hour C-Win
ON VALUE-CHANGED OF tb_tot-hour IN FRAME FRAME-A
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  assign
   begin_pc-date = date(1,1,year(today))
   end_pc-date   = today.

  RUN enable_UI.
  
  {methods/nowait.i}
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
  DISPLAY begin_pc-date end_pc-date begin_mach end_mach begin_job-no 
          begin_job-no2 end_job-no end_job-no2 lbl_tot-hr tb_tot-hour 
          lbl_dept-brk tb_page-break rd-dest lines-per-page td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_pc-date end_pc-date begin_mach end_mach begin_job-no 
         begin_job-no2 end_job-no end_job-no2 tb_tot-hour tb_page-break rd-dest 
         lines-per-page td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
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
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
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
  run scr-rpt.w (list-name,c-win:title). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* --------------------------------------------- pc/rep/mch-edit.i 01/96 JLF  */
/* Production Control -transactions edit list (main body)                     */
/* -------------------------------------------------------------------------- */
DEF VAR tmp-dir AS cha NO-UNDO.
DEF VAR X AS INT NO-UNDO.

form
    pc-prdd.m-code column-label "MACH"
    mach.m-dscr column-label "DESCRIPT" format "x(10)"
    mach.dept[1] column-label "DP"
    pc-prdd.op-date column-label "DATE"
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
form
    pc-prdd.m-code column-label "MACH"
    mach.m-dscr column-label "DESCRIPT" format "x(10)"
    mach.dept[1] column-label "DP"
    pc-prdd.op-date column-label "DATE"
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
    with frame mch-edit1 no-box down STREAM-IO width 132.

ASSIGN v-m-code[1] = begin_mach
       v-m-code[2] = END_mach
       v-date[1] = begin_pc-date
       v-date[2] = END_pc-date
       v-job-no[1] = begin_job-no
       v-job-no[2] = END_job-no
       v-job-no2[1] = begin_job-no2
       v-job-no2[2] = END_job-no2
       v-toth = tb_tot-hour
       v-dept-paging = tb_page-break       
       .
do x = 1 to 2:
      v-job-no[x] = fill(" ", 6 - integer(length(trim(v-job-no[x])))) +
                    trim(v-job-no[x]).
end.
/* 
if v-dept-paging then
      {pc/rep/mch-edit.i pc-prdd.dept "by pc-prdd.m-code"}
else
      {pc/rep/mch-edit.i pc-prdd.m-code}
*/
{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)}
if td-show-parm then run show-param. 
/* ============= */
IF v-dept-paging THEN
for each pc-prdd
    where pc-prdd.company eq cocode
      and pc-prdd.m-code  ge v-m-code[1]
      and pc-prdd.m-code  le v-m-code[2]
      and pc-prdd.op-date ge v-date[1]
      and pc-prdd.op-date le v-date[2]
      and pc-prdd.job-no  ge v-job-no[1]
      and pc-prdd.job-no  le v-job-no[2]
      and pc-prdd.job-no2 ge v-job-no2[1]
      and pc-prdd.job-no2 le v-job-no2[2]
    no-lock,
    
    first mach
    where mach.company eq cocode
      and mach.loc     eq locode
      and mach.m-code  eq pc-prdd.m-code
    no-lock

    break by pc-prdd.dept
          BY pc-prdd.m-code   
          by pc-prdd.op-date
          by pc-prdd.start
          by pc-prdd.stopp
 
    with frame mch-edit:

  if first-of(pc-prdd.op-date) then put " " skip.

  v-recid = ?.
  for each job-hdr
      where job-hdr.company eq cocode
        and job-hdr.job     eq pc-prdd.job
        and job-hdr.job-no  eq pc-prdd.job-no
        and job-hdr.job-no2 eq pc-prdd.job-no2
        and job-hdr.frm     eq pc-prdd.frm
      no-lock
      by job-hdr.blank-no desc:
              
    v-recid = recid(job-hdr).
              
    if job-hdr.blank-no eq pc-prdd.blank-no then leave.
  end.
  find job-hdr where recid(job-hdr) eq v-recid no-lock no-error.
  
  release itemfg.
  if avail job-hdr then
  find first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq job-hdr.i-no
      no-lock no-error.

  display pc-prdd.m-code     when first-of(pc-prdd.m-code)
          mach.m-dscr        when first-of(pc-prdd.m-code)
          mach.dept[1]       when first-of(pc-prdd.dept)
          pc-prdd.op-date when first-of(pc-prdd.op-date)
          pc-prdd.shift
          pc-prdd.job-no
          pc-prdd.job-no2
          pc-prdd.frm
          pc-prdd.blank-no
          job-hdr.i-no when avail job-hdr
          itemfg.i-name when avail itemfg
          pc-prdd.code
          pc-prdd.hours
          string(pc-prdd.start,"HH:MM") @ v-start
          string(pc-prdd.stopp,"HH:MM") @ v-stopp
          pc-prdd.crew
          pc-prdd.qty
          pc-prdd.waste
          "Y" when pc-prdd.complete @ v-comp WITH STREAM-IO.
  down.

  assign
   tothour = tothour + pc-prdd.hours
   totqty  = totqty  + pc-prdd.qty
   totwst  = totwst  + pc-prdd.waste.

  if last-of(pc-prdd.dept) then do:
    if v-toth then do:
      put uline at 89 skip
          totchar at 68
          tothour at 90
          totqty  to 121
          totwst  to 128 skip(1).
      down.
    end.
    
    assign
     tothour = 0
     totqty  = 0
     totwst  = 0.
  end.    
    
  /*if "{1}" eq "pc-prdd.dept" then*/ page.
end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
ELSE for each pc-prdd
    where pc-prdd.company eq cocode
      and pc-prdd.m-code  ge v-m-code[1]
      and pc-prdd.m-code  le v-m-code[2]
      and pc-prdd.op-date ge v-date[1]
      and pc-prdd.op-date le v-date[2]
      and pc-prdd.job-no  ge v-job-no[1]
      and pc-prdd.job-no  le v-job-no[2]
      and pc-prdd.job-no2 ge v-job-no2[1]
      and pc-prdd.job-no2 le v-job-no2[2]
    no-lock,
    
    first mach
    where mach.company eq cocode
      and mach.loc     eq locode
      and mach.m-code  eq pc-prdd.m-code
    no-lock

    BREAK BY pc-prdd.m-code   
          by pc-prdd.op-date
          by pc-prdd.start
          by pc-prdd.stopp
 
    with frame mch-edit1:

  if first-of(pc-prdd.op-date) then put " " skip.

  v-recid = ?.
  for each job-hdr
      where job-hdr.company eq cocode
        and job-hdr.job     eq pc-prdd.job
        and job-hdr.job-no  eq pc-prdd.job-no
        and job-hdr.job-no2 eq pc-prdd.job-no2
        and job-hdr.frm     eq pc-prdd.frm
      no-lock
      by job-hdr.blank-no desc:
              
    v-recid = recid(job-hdr).
              
    if job-hdr.blank-no eq pc-prdd.blank-no then leave.
  end.
  find job-hdr where recid(job-hdr) eq v-recid no-lock no-error.
  
  release itemfg.
  if avail job-hdr then
  find first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq job-hdr.i-no
      no-lock no-error.

  display pc-prdd.m-code     when first-of(pc-prdd.m-code)
          mach.m-dscr        when first-of(pc-prdd.m-code)
          mach.dept[1]       when first-of(pc-prdd.m-code)
          pc-prdd.op-date when first-of(pc-prdd.op-date)
          pc-prdd.shift
          pc-prdd.job-no
          pc-prdd.job-no2
          pc-prdd.frm
          pc-prdd.blank-no
          job-hdr.i-no when avail job-hdr
          itemfg.i-name when avail itemfg
          pc-prdd.code
          pc-prdd.hours
          string(pc-prdd.start,"HH:MM") @ v-start
          string(pc-prdd.stopp,"HH:MM") @ v-stopp
          pc-prdd.crew
          pc-prdd.qty
          pc-prdd.waste
          "Y" when pc-prdd.complete @ v-comp WITH STREAM-IO.
  down.

  assign
   tothour = tothour + pc-prdd.hours
   totqty  = totqty  + pc-prdd.qty
   totwst  = totwst  + pc-prdd.waste.

  if last-of(pc-prdd.m-code) then do:
    if v-toth then do:
      put uline at 89 skip
          totchar at 68
          tothour at 90
          totqty  to 121
          totwst  to 128 skip(1).
      down.
    end.
    
    assign
     tothour = 0
     totqty  = 0
     totwst  = 0.
  end.    
    
  /*if "{1}" eq "pc-prdd.dept" then*/ page.
end.




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

