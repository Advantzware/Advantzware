&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: cerep\r-mach.w

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

def new shared var sp-dscr as ch format "x(25)" extent 2.
def new shared var head1   as ch format "x(78)" init
  "   STANDARDS:  X axis (Columns) ===========  Y Axis (Rows) =============".

def new shared var head as ch format "x(78)" extent 4.

DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL NO-UNDO.


/* gdm - 10130802 */
DEF STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_mach end_mach begin_dept ~
end_dept tb_show-stds rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_mach end_mach begin_dept end_dept ~
tb_show-stds rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_dept AS CHARACTER FORMAT "X(2)" 
     LABEL "Beginning Dept. Code" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Machine#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_dept AS CHARACTER FORMAT "X(2)" INITIAL "zz" 
     LABEL "Ending Dept. Code" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)" INITIAL "zzzzz" 
     LABEL "Ending Machine#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-mach.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43.6 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 9.76.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 9.29.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1.05 NO-UNDO.

DEFINE VARIABLE tb_show-stds AS LOGICAL INITIAL no 
     LABEL "Show Standards?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_mach AT ROW 3.38 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Machine Number"
     end_mach AT ROW 3.38 COL 69 COLON-ALIGNED HELP
          "Enter Ending Machine Number"
     begin_dept AT ROW 4.33 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Department Code"
     end_dept AT ROW 4.33 COL 69 COLON-ALIGNED HELP
          "Enter Ending Department Code"
     tb_show-stds AT ROW 6.71 COL 50
     rd-dest AT ROW 11.95 COL 5 NO-LABEL
     lv-ornt AT ROW 12.91 COL 31 NO-LABEL
     lines-per-page AT ROW 12.91 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 14.33 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 15.29 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.71 COL 30
     tb_excel AT ROW 17.81 COL 50.2 RIGHT-ALIGNED WIDGET-ID 4
     tb_runExcel AT ROW 17.81 COL 72.8 RIGHT-ALIGNED WIDGET-ID 6
     fi_file AT ROW 19 COL 28.2 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 2
     btn-ok AT ROW 20.86 COL 19
     btn-cancel AT ROW 20.86 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 10.76 COL 2
     RECT-7 AT ROW 1 COL 2
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
         TITLE              = "Machine File"
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
       begin_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_show-stds:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Machine File */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Machine File */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_dept C-Win
ON LEAVE OF begin_dept IN FRAME FRAME-A /* Beginning Dept. Code */
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
    SESSION:SET-WAIT-STATE ("general").

    /* gdm - 10130802 */
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&displayed-objects}.
    END.

  assign rd-dest.

  run run-report. 

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &type=" "
                            &begin_cust="rd-dest"
                            &end_cust="rd-dest" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE=" "
                             &begin_cust=''
                             &end_cust=''
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE=" "
                                  &begin_cust=''
                                  &end_cust=''
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  END CASE.

  SESSION:SET-WAIT-STATE ("").


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_dept C-Win
ON LEAVE OF end_dept IN FRAME FRAME-A /* Ending Dept. Code */
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


&Scoped-define SELF-NAME tb_show-stds
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-stds C-Win
ON VALUE-CHANGED OF tb_show-stds IN FRAME FRAME-A /* Show Standards? */
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

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.
  RUN enable_UI.
  {methods/nowait.i}
  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY 'ENTRY' TO begin_mach.
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
  DISPLAY begin_mach end_mach begin_dept end_dept tb_show-stds rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_mach end_mach begin_dept end_dept tb_show-stds 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel 
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
{custom/out2file.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-port C-Win 
PROCEDURE output-to-port :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN custom/d-print.w (list-name).

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
 /*    DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
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

   RUN custom/prntproc.p (list-name,int(lv-font-no),lv-ornt).
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
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* --------------------------------------------------- sys/rep/mach.p 5/91 cd */
/* MACHINE FILE PRINTOUT                                                      */
/* -------------------------------------------------------------------------- */


def var save_id as RECID NO-UNDO.
def var time_stamp as ch NO-UNDO.
time_stamp = string(time, "hh:mmam").

def var fmac like mach.m-code NO-UNDO.
def var tmac like fmac init "zzzzzz" NO-UNDO.
def var fdep like est-op.dept NO-UNDO.
def var tdep like fdep init "zz" NO-UNDO.
def var sho-stds as log format "Y/N" init NO NO-UNDO.
DEF VAR lv-need-display AS LOG NO-UNDO.

/* gdm - 10130802 */
DEF VAR v_head     AS CHAR NO-UNDO.
DEF VAR v_numlw    AS CHAR NO-UNDO.
DEF VAR v_col-pass AS CHAR NO-UNDO.
DEF VAR v_coater   AS CHAR NO-UNDO.

head[1] =
" =============================  R  A  T  E  S  =============================  "
.
head[2] = "  ==== LIMITS     Min       Max ==="   .
head[3] = "==========  Printing Press  =========" .


{sys/form/r-top.f}
{sys/ref/mstd.f}
{sys/ref/mmtx.f}
{sys/ref/mmtx2.f}
{sys/ref/mmty.f}
{sys/ref/mach.i}

format
  "Code:" to 7 mach.m-code
  "Desc:" to 33 mach.m-dscr
  "Rigid:" to 72 mach.setup skip

  "Loc:" to 7 mach.loc
  "Dept:" to 33 mach.dept[1] mach.dept[2] mach.dept[3] mach.dept[4]
  "Sequence:" to 57 mach.d-seq mach.m-seq
  "Fold:" to 72 mach.fc skip

  "Feed:" to 7 space(1) mach.p-type
  "Run Spoil. %:" to 33 space(1) mach.run-spoil
  "MR Waste:" to 57 mach.mr-waste
  "Corr:" to 72 mach.corr skip

  /* "Die Rule :"     to 33 mach.die-cost
  "Die Mat'l:"     to 57 mach.matl-cost */
  "Therm:" to 72 mach.therm skip

  head[1] format "x(78)"

  /* line 4 */
  " LABOR Rate1:" mach.lab-rate[1] FORM ">>>9.99"
  "Market:" to 31 mach.mrk-rate    FORM ">>>9.99"
  "MR D.L.:" to 48 mach.mr-rate    FORM ">>>9.99"
  "RUN D.L.:" to 69 mach.run-rate FORM ">>>9.99" skip

  "       Rate2:" mach.lab-rate[2] FORM ">>>9.99"
  "MR  Crew:" to 31 space(2) mach.mr-crusiz FORM ">>9.99"
  "Var OH:" to 48 mach.mr-varoh     FORM ">>>9.99"
  "Var OH:" to 69 mach.run-varoh FORM ">>>9.99" skip

  "       Rate3:" mach.lab-rate[3] FORM ">>>9.99"
  "RUN Crew:" to 31 space(2) mach.run-crusiz FORM ">>9.99"
  "Fix OH:" to 48 mach.mr-fixoh  FORM ">>>9.99"
  "Fix OH:" to 69 mach.run-fixoh FORM ">>>9.99" skip

  "     Default:" mach.lab-drate
  "   TOTAL:" to 48 mach.mr-trate FORM ">>>9.99"
  "TOTAL:" to 69 mach.run-trate FORM ">>>9.99" skip(1)

  head[2] format "x(35)" head[3] format "x(37)" AT 38
  v-label[1]   to 12 mach.min-len to 23  mach.max-len to 35
  "Printer Type:" to 54 space(2) mach.pr-type
  v-label[2]   to 12 mach.min-wid to 23  mach.max-wid to 35
  "Washup Hours:" to 54 mach.washup "Col/Pass:" to 70 mach.col-pass
  v-label[3]   to 12 mach.min-triml to 23
  "Max # colors:" to 54 space(1) mach.max-color
  "Coater On:" to 70 mach.coater
  v-label[4]   to 12 mach.min-trimw to 23
  " MR Waste/color:" to 54 space(2) mach.col-wastesh
  "Caliper:"   to 12 space(3) mach.min-cal to 23  mach.max-cal to 35
  " INK Waste Lbs/MR:" to 54 mach.ink-waste "Lbs/Col:" mach.col-wastelb
  "Run Qty:"   to 12 mach.min-run   mach.max-run to 35
  " Tandem MR/Plate :" to 54 mach.Tan-MRP   " /Fountn:" mach.tan-MRF
  skip(2)
  with frame mach overlay no-labels width 90 STREAM-IO
  /*color value(col-bg) prompt-for value(col-input)*/ .

SESSION:SET-WAIT-STATE ("general").

ASSIGN
str-tit  = coname + " - " + loname
str-tit2 = "Machines List"
str-tit3 = ""
x = (56 - length(str-tit)) / 2
str-tit  = fill(" ",x) + str-tit
x = (56 - length(str-tit2)) / 2
str-tit2 = fill(" ",x) + str-tit2
x = (80 - length(str-tit3)) / 2
str-tit3 = fill(" ",x) + str-tit3
fmac     = begin_mach
tmac     = end_mach
fdep     = begin_dept
tdep     = end_dept
sho-stds = tb_show-stds.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    if td-show-parm then run show-param.

    view frame r-top.

/* gdm - 10130802 */
IF tb_excel THEN DO:
    OUTPUT STREAM excel TO VALUE(fi_file).
    PUT STREAM excel UNFORMATTED
        "Company Name,Warehouse - Descr,Code,Loc,Feed,Desc,Dept,Run Spoil. %,Sequence," 
        "MR Waste,Rigid,Fold,Corr,Therm,LABOR Rate1,Rate2,Rate3,Default," 
        "Market,MR Crew,RUN Crew,MR D.L.,Var OH,Fix OH,TOTAL,RUN D.L.," 
        "Var OH,Fix OH,TOTAL,Lngth Min,Lngth Max,Width Min,Width Max," 
        "Trim Len,Trim Wid,Caliper Min,Caliper Max,Run Qty Min,Run Qty Max,"
        "Printer Type,Washup Hours,Max # colors,MR Waste/color,INK Waste Lbs/MR," 
        "Tandem MR/Plate,Col/Pass,Coater On,Lbs/Col,/Fountn,Panel Len,Panel Wid,"
        "Sheets/Run,Blanks/Run,Lin.Ft/Run"
     SKIP.

END.

    for each mach
        where mach.company  eq cocode
          and mach.loc      eq locode
          and mach.m-code   ge fmac
          and mach.m-code   le tmac
          and mach.dept[1]  ge fdep
          and mach.dept[1]  le tdep
        no-lock,
        first company
        where company.company eq mach.company
        no-lock,
        first loc
        where loc.company eq mach.company
          and loc.loc     eq mach.loc
        no-lock with frame mach BREAK BY mach.m-code:

      clear frame mach all no-pause.
      /*if line-counter gt 40 then page.     */
      IF FIRST-OF(mach.m-code) THEN PAGE.

      put "  Company: " mach.company space(5) company.name  skip
          "Warehouse: " loc.loc      space(3) loc.dscr      skip.
      {sys/ref/mach.v}
      down.

      /* gdm - 10130802 */
      ASSIGN 
       v_numlw = ""
       v_numlw = IF mach.num-len EQ 0 THEN "" ELSE STRING(mach.num-len)
       v_numlw = v_numlw + " / " +
                 IF mach.num-wid EQ 0 THEN "" ELSE STRING(mach.num-wid)
       v_numlw = IF TRIM(v_numlw) = "/" THEN "" ELSE v_numlw
       v_col-pass = IF STRING(TRIM(mach.col-pass)) = ? 
                      THEN '' ELSE STRING(mach.col-pass) 
       v_coater = IF mach.coater = ? THEN '' ELSE STRING(mach.coater).

      IF tb_excel THEN DO:
          PUT STREAM excel UNFORMATTED
              '"' STRING(mach.company + ' - ' + STRING(company.name)) '",'
              '"' String(string(loc.loc) + ' - ' + STRING(loc.dscr))  '",'
              '"' REPLACE(mach.m-code,'"','')         '",'
              '"' REPLACE(mach.loc,'"','')            '",'
              '"' mach.p-type                         '",'
              '"' REPLACE(mach.m-dscr,'"','')         '",'
              '"' STRING(mach.dept[1] + " " + mach.dept[2] + " " + 
                         mach.dept[3] + " " + mach.dept[4])  '",'
              '"' mach.run-spoil                      '",'
              '"' mach.d-seq mach.m-seq               '",'
              '"' mach.mr-waste                       '",'
              '"' mach.setup                          '",'
              '"' mach.fc                             '",'
              '"' mach.corr                           '",'
              '"' mach.therm                          '",'
              '"' STRING(mach.lab-rate[1],">>>9.99")  '",'
              '"' STRING(mach.lab-rate[2],">>>9.99")  '",'
              '"' STRING(mach.lab-rate[3],">>>9.99")  '",'
              '"' STRING(mach.lab-drate,">>>9.99")    '",'
              '"' STRING(mach.mrk-rate,">>>9.99")     '",'
              '"' STRING(mach.mr-crusiz,">>9.99")     '",'
              '"' STRING(mach.run-crusiz,">>9.99")    '",'
              '"' STRING(mach.mr-rate,">>>9.99")      '",'
              '"' STRING(mach.mr-varoh,">>>9.99")     '",'
              '"' STRING(mach.mr-fixoh,">>>9.99")     '",'
              '"' STRING(mach.mr-trate,">>>9.99")     '",'
              '"' STRING(mach.run-rate,">>>9.99")     '",'
              '"' STRING(mach.run-varoh,">>>9.99")    '",'
              '"' STRING(mach.run-fixoh,">>>9.99")    '",'
              '"' STRING(mach.run-trate,">>>9.99")    '",'
              '"' mach.min-len                        '",'
              '"' mach.max-len                        '",'
              '"' mach.min-wid                        '",'
              '"' mach.max-wid                        '",'
              '"' mach.min-triml                      '",'
              '"' mach.min-trimw                      '",'
              '"' mach.min-cal                        '",'
              '"' mach.max-cal                        '",'
              '"' mach.min-run                        '",'
              '"' mach.max-run                        '",'
              '"' mach.pr-type                        '",'
              '"' mach.washup                         '",'
              '"' mach.max-color                      '",'
              '"' mach.col-wastesh                    '",'
              '"' mach.ink-waste                      '",'
              '"' mach.Tan-MRP                        '",'
              '"' v_col-pass                          '",'
              '"' v_coater                            '",'
              '"' mach.col-wastelb                    '",'
              '"' mach.tan-MRF                        '",'
              '"' mach.min-pan-l                      '",'
              '"' mach.min-pan-w                      '",'.

          IF mach.p-type eq "B"  
            THEN 
              PUT STREAM excel UNFORMATTED
              "," v_numlw SKIP.
            ELSE 
             IF mach.therm AND mach.p-type EQ "R" 
               THEN 
                 PUT STREAM excel UNFORMATTED
                 ",," v_numlw SKIP.
               ELSE 
                 PUT STREAM excel UNFORMATTED
                     v_numlw SKIP.

      END.

      if sho-stds then do:

        for each mstd of mach with frame mstd:
          clear frame mstd all no-pause.

          /*if line-counter gt 40 then page.*/
          display skip(1).
          find first dept
              where dept.company eq cocode
                and dept.code    eq mstd.dept
              no-lock no-error.
          find first style
             where style.company eq cocode
               and style.style   eq mstd.style
             no-lock no-error.
          display mstd.m-code
                  mach.m-dscr when avail mach
                  mstd.dept
                  dept.dscr   when avail dept
                  mstd.style FORM "x(6)"
                  style.dscr when avail style.
          find std-code where std-code.code eq string(mr-x,"99") no-lock no-error.
          display mr-x std-code.dscr when avail std-code @ mx-dscr[1].
          find std-code where std-code.code eq string(mr-y,"99") no-lock no-error.
          display mr-y std-code.dscr when avail std-code @ mx-dscr[2].
          find std-code where std-code.code eq string(rs-x,"99") no-lock no-error.
          display rs-x std-code.dscr when avail std-code @ mx-dscr[3].
          find std-code where std-code.code eq string(rs-y,"99") no-lock no-error.
          display rs-y std-code.dscr when avail std-code @ mx-dscr[4].
          find std-code where std-code.code eq string(sp-x,"99") no-lock no-error.
          display sp-x std-code.dscr when avail std-code @ sp-dscr[1].
          find std-code where std-code.code eq string(sp-y,"99") no-lock no-error.
          display sp-y std-code.dscr when avail std-code @ sp-dscr[2].

/*
          display run-qty[1 for 9]
                  X-sheets[1 for 9]
                  board-cal[1 for 9]
                  spd-reduc[1 for 9].
*/
          run sys/ref/mach-1.p (recid(mstd)).

          for each mmty of mstd with frame mmty:
            clear frame mmty all no-pause.
            /*if line-counter gt 55 then */ page.
            display mmty.m-code
                    mach.m-dscr
                    mmty.style
                    style.dscr  when avail style
                    mmty.c-title
                    mmty.rtit[1 for 15].
            do i = 1 to 15:
              display mmty.row-value[i] when mmty.row-value[i] ne 0.
            end.
            do i = 1 to 10:
              display mmty.col-value[i] when mmty.col-value[i] ne 0
                      mmty.vals[10  + i] when mmty.vals[10  + i] ne 0
                      mmty.vals[20  + i] when mmty.vals[20  + i] ne 0
                      mmty.vals[30  + i] when mmty.vals[30  + i] ne 0
                      mmty.vals[40  + i] when mmty.vals[40  + i] ne 0
                      mmty.vals[50  + i] when mmty.vals[50  + i] ne 0
                      mmty.vals[60  + i] when mmty.vals[60  + i] ne 0
                      mmty.vals[70  + i] when mmty.vals[70  + i] ne 0
                      mmty.vals[80  + i] when mmty.vals[80  + i] ne 0
                      mmty.vals[90  + i] when mmty.vals[90  + i] ne 0
                      mmty.vals[100 + i] when mmty.vals[100 + i] ne 0
                      mmty.vals[110 + i] when mmty.vals[110 + i] ne 0
                      mmty.vals[120 + i] when mmty.vals[120 + i] ne 0
                      mmty.vals[130 + i] when mmty.vals[130 + i] ne 0
                      mmty.vals[140 + i] when mmty.vals[140 + i] ne 0
                      mmty.vals[150 + i] when mmty.vals[150 + i] ne 0.
            end.

            down.
          end.

          for each mmtx of mstd with frame mmtx :
          /*  clear frame mmtx all no-pause. */

            if line-counter gt 55 then page.

            lv-need-display = IF mmtx.page-no = 0 AND mmtx.across-no = 0 THEN YES ELSE NO.
            IF mmtx.page-no <> 0 or mmtx.across-no <> 0 THEN DO:
                 DO i = 11 TO 160:
                    IF mmtx.vals[i] <> 0 THEN lv-need-display = YES.
                    IF lv-need-display THEN LEAVE.
                 END.
            END.

            IF lv-need-display THEN DO:
               display mmtx.m-code
                    mach.m-dscr
                    mmtx.style
                    style.dscr  when avail style
                    mmtx.mr-run when not mr-run
                    "SPOIL" when mr-run @ mmtx.mr-run
                    mmtx.c-title
                    with frame mmtx.
               do i = 1 to 15:
                  display mmtx.rtit[i] mmtx.row-value[i] when mmtx.row-value[i] ne 0
                  with frame mmtx.
               END.
            end.
            /* The following is for RUN matrix's only */
            if not mr-run then
            do:
                IF lv-need-display THEN do i = 1 to 10:
                display mmtx.col-value[i] mmtx.vals[10 + i]
                        mmtx.vals[20 + i]
                        mmtx.vals[30 + i]
                        mmtx.vals[40 + i]
                        mmtx.vals[50 + i]
                        mmtx.vals[60 + i]
                        mmtx.vals[70 + i]
                        mmtx.vals[80 + i]
                        mmtx.vals[90 + i]
                        mmtx.vals[100 + i]
                        mmtx.vals[110 + i]
                        mmtx.vals[120 + i]
                        mmtx.vals[130 + i]
                        mmtx.vals[140 + i]
                        mmtx.vals[150 + i]
                    with frame mmtx.

              end.
            /*  run sys/ref/mach-1.p (recid(mstd)). */
            end.
            else
            /* This is for RUN SPOILAGE matrix's only */
            do i = 1 to 10:
              display mmtx.col-value[i] when mmtx.col-value[i] ne 0
                      mmtx.vals[10 + i] when mmtx.vals[10 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[10 + i]
                      mmtx.vals[20 + i] when mmtx.vals[20 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[20 + i]
                      mmtx.vals[30 + i] when mmtx.vals[30 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[30 + i]
                      mmtx.vals[40 + i] when mmtx.vals[40 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[40 + i]
                      mmtx.vals[50 + i] when mmtx.vals[50 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[50 + i]
                      mmtx.vals[60 + i] when mmtx.vals[60 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[60 + i]
                      mmtx.vals[70 + i] when mmtx.vals[70 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[70 + i]
                      mmtx.vals[80 + i] when mmtx.vals[80 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[80 + i]
                      mmtx.vals[90 + i] when mmtx.vals[90 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[90 + i]
                      mmtx.vals[100 + i] when mmtx.vals[100 + i] ne 0
                        format ">>9.99"   @ mmtx.vals[100 + i]
                      mmtx.vals[110 + i] when mmtx.vals[110 + i] ne 0
                        format ">>9.99"   @ mmtx.vals[110 + i]
                      mmtx.vals[120 + i] when mmtx.vals[120 + i] ne 0
                        format ">>9.99"   @ mmtx.vals[120 + i]
                      mmtx.vals[130 + i] when mmtx.vals[130 + i] ne 0
                        format ">>9.99"   @ mmtx.vals[130 + i]
                      mmtx.vals[140 + i] when mmtx.vals[140 + i] ne 0
                        format ">>9.99"   @ mmtx.vals[140 + i]
                      mmtx.vals[150 + i] when mmtx.vals[150 + i] ne 0
                        format ">>9.99"   @ mmtx.vals[150 + i]
                  with frame mmtx.
            end.

            IF lv-need-display OR mmtx.mr-run THEN down.      
          end.
          PUT SKIP(2).
        end.

        /*page.*/
      end.
    end.
    /* gdm - 10130802 */
    IF tb_excel THEN DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_runExcel THEN
            OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
    END.

    OUTPUT CLOSE.

    /* gdm - 10130802 */
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("general").

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-old C-Win 
PROCEDURE run-report-old :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* --------------------------------------------------- sys/rep/mach.p 5/91 cd */
/* MACHINE FILE PRINTOUT                                                      */
/* -------------------------------------------------------------------------- */
/*
{sys/form/r-top.f}

def var fmac like mach.m-code.
def var tmac like fmac init "zzzzzz".
def var fdep like est-op.dept.
def var tdep like fdep init "zz".
def var sho-stds as log format "Y/N" init no.

def var i as int.

def var sp-dscr as ch format "x(25)" extent 2.

def var head1   as ch format "x(78)" init
  "   STANDARDS:  X axis (Columns) ===========  Y Axis (Rows) =============".

def var head as ch format "x(78)" extent 4.

{sys/ref/mstd.f}
{sys/ref/mmtx.f}
{sys/ref/mmtx2.f}
{sys/ref/mmty.f}
{sys/ref/mach.i}

format
  "Code:" to 7 mach.m-code
  "Desc:" to 33 mach.m-dscr
  "Rigid:" to 72 mach.setup skip

  "Loc:" to 7 mach.loc
  "Dept:" to 33 mach.dept[1] mach.dept[2] mach.dept[3] mach.dept[4]
  "Sequence:" to 57 mach.d-seq mach.m-seq
  "Fold:" to 72 mach.fc skip

  "Feed:" to 7 space(1) mach.p-type
  "Run Spoil. %:" to 33 space(1) mach.run-spoil
  "MR Waste:" to 57 mach.mr-waste
  "Corr:" to 72 mach.corr skip

  /* "Die Rule :"     to 33 mach.die-cost
  "Die Mat'l:"     to 57 mach.matl-cost */
  "Therm:" to 72 mach.therm skip

  head[1] format "x(78)"

  /* line 4 */
  " LABOR Rate1:" mach.lab-rate[1]
  "Market:" to 31 mach.mrk-rate
  "MR D.L.:" to 48 mach.mr-rate
  "RUN D.L.:" to 69 mach.run-rate skip

  "       Rate2:" mach.lab-rate[2]
  "MR  Crew:" to 31 space(2) mach.mr-crusiz
  "Var OH:" to 48 mach.mr-varoh
  "Var OH:" to 69 mach.run-varoh skip

  "       Rate3:" mach.lab-rate[3]
  "RUN Crew:" to 31 space(2) mach.run-crusiz
  "Fix OH:" to 48 mach.mr-fixoh
  "Fix OH:" to 69 mach.run-fixoh skip

  "     Default:" mach.lab-drate
  "   TOTAL:" to 48 mach.mr-trate
  "TOTAL:" to 69 mach.run-trate skip(1)

  head[2] format "x(35)" head[3] format "x(37)" AT 38
  v-label[1]   to 12 mach.min-len to 23  mach.max-len to 35
  "Printer Type:" to 54 space(2) mach.pr-type
  v-label[2]   to 12 mach.min-wid to 23  mach.max-wid to 35
  "Washup Hours:" to 54 mach.washup "Col/Pass:" to 70 mach.col-pass
  v-label[3]   to 12 mach.min-triml to 23
  "Max # colors:" to 54 space(1) mach.max-color
  "Coater On:" to 70 mach.coater
  v-label[4]   to 12 mach.min-trimw to 23
  " MR Waste/color:" to 54 space(2) mach.col-wastesh
  "Caliper:"   to 12 space(3) mach.min-cal to 23  mach.max-cal to 35
  " INK Waste Lbs/MR:" to 54 mach.ink-waste "Lbs/Col:" mach.col-wastelb
  "Run Qty:"   to 12 mach.min-run   mach.max-run to 35
  " Tandem MR/Plate :" to 54 mach.Tan-MRP   " /Fountn:" mach.tan-MRF
  skip(2)
  with frame mach overlay no-labels stream-io width 81.


assign
 str-tit2 = "Machines List"
 str-tit2 = fill(" ",int((56 - length(trim(str-tit2))) / 2)) + trim(str-tit2)

 head[1]  = " =============================  R  A  T  E  S  =============================  "
 head[2]  = "  ==== LIMITS     Min       Max ==="
 head[3]  = "==========  Printing Press  =========" 

 fmac     = begin_mach
 tmac     = end_mach
 fdep     = begin_dept
 tdep     = end_dept
 sho-stds = tb_show-stds.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display str-tit with frame r-top.

for each mach
    where mach.company  eq cocode
      and mach.loc      eq locode
      and mach.m-code   ge fmac
      and mach.m-code   le tmac
      and mach.dept[1]  ge fdep
      and mach.dept[1]  le tdep
    no-lock,

    first company
    where company.company eq mach.company
    no-lock,

    first loc
    where loc.company eq mach.company
      and loc.loc     eq mach.loc
    no-lock with frame mach:

  clear frame mach all no-pause.

  put "  Company: " mach.company space(5) company.name  skip
      "Warehouse: " loc.loc      space(3) loc.dscr      skip.
  {sys/ref/mach.v}
  down.


  if sho-stds then do:
    for each mstd
        where mstd.company eq mach.company
          and mstd.m-code  eq mach.m-code
        no-lock
        with frame mstd: 

      clear frame mstd all no-pause.

      display skip(1).

      find first dept
          where dept.company eq cocode
            and dept.code    eq mstd.dept
          no-lock no-error.
      find first style
          where style.company eq cocode
            and style.style   eq mstd.style
          no-lock no-error.

      display mstd.m-code
              mach.m-dscr
              mstd.dept
              dept.dscr   when avail dept
              mstd.style
              style.dscr when avail style.

      find std-code where std-code.code eq string(mr-x,"99") no-lock no-error.

      display mr-x std-code.dscr when avail std-code @ mx-dscr[1].

      find std-code where std-code.code eq string(mr-y,"99") no-lock no-error.

      display mr-y std-code.dscr when avail std-code @ mx-dscr[2].

      find std-code where std-code.code eq string(rs-x,"99") no-lock no-error.

      display rs-x std-code.dscr when avail std-code @ mx-dscr[3].

      find std-code where std-code.code eq string(rs-y,"99") no-lock no-error.

      display rs-y std-code.dscr when avail std-code @ mx-dscr[4].

      find std-code where std-code.code eq string(sp-x,"99") no-lock no-error.

      display sp-x std-code.dscr when avail std-code @ sp-dscr[1].

      find std-code where std-code.code eq string(sp-y,"99") no-lock no-error.

      display sp-y std-code.dscr when avail std-code @ sp-dscr[2].

      for each mmty of mstd with frame mmty:
        clear frame mmty all no-pause.

        display mmty.m-code
                mach.m-dscr
                mmty.style
                style.dscr  when avail style
                mmty.c-title
                mmty.rtit[1 for 15].

        do i = 1 to 15:
          display mmty.row-value[i] when mmty.row-value[i] ne 0.
        end.

        do i = 1 to 10:
          display mmty.col-value[i] when mmty.col-value[i] ne 0
                  mmty.vals[10  + i] when mmty.vals[10  + i] ne 0
                  mmty.vals[20  + i] when mmty.vals[20  + i] ne 0
                  mmty.vals[30  + i] when mmty.vals[30  + i] ne 0
                  mmty.vals[40  + i] when mmty.vals[40  + i] ne 0
                  mmty.vals[50  + i] when mmty.vals[50  + i] ne 0
                  mmty.vals[60  + i] when mmty.vals[60  + i] ne 0
                  mmty.vals[70  + i] when mmty.vals[70  + i] ne 0
                  mmty.vals[80  + i] when mmty.vals[80  + i] ne 0
                  mmty.vals[90  + i] when mmty.vals[90  + i] ne 0
                  mmty.vals[100 + i] when mmty.vals[100 + i] ne 0
                  mmty.vals[110 + i] when mmty.vals[110 + i] ne 0
                  mmty.vals[120 + i] when mmty.vals[120 + i] ne 0
                  mmty.vals[130 + i] when mmty.vals[130 + i] ne 0
                  mmty.vals[140 + i] when mmty.vals[140 + i] ne 0
                  mmty.vals[150 + i] when mmty.vals[150 + i] ne 0.
        end.

        down.
      end.

      for each mmtx of mstd with frame mmtx down:
        clear frame mmtx all no-pause.

        display mmtx.m-code
                mach.m-dscr
                mmtx.style
                style.dscr  when avail style
                mmtx.mr-run when not mr-run
                "SPOIL" when mr-run @ mmtx.mr-run
                mmtx.c-title
                mmtx.rtit[1 for 15] with frame mmtx.

        do i = 1 to 15:
          display mmtx.row-value[i] when mmtx.row-value[i] ne 0
              with frame mmtx.
        end.

        /* The following is for RUN matrix's only */
        if not mr-run then do:
          do i = 1 to 10:
            display mmtx.col-value[i] mmtx.vals[10 + i]
                    mmtx.vals[20 + i]
                    mmtx.vals[30 + i]
                    mmtx.vals[40 + i]
                    mmtx.vals[50 + i]
                    mmtx.vals[60 + i]
                    mmtx.vals[70 + i]
                    mmtx.vals[80 + i]
                    mmtx.vals[90 + i]
                    mmtx.vals[100 + i]
                    mmtx.vals[110 + i]
                    mmtx.vals[120 + i]
                    mmtx.vals[130 + i]
                    mmtx.vals[140 + i]
                    mmtx.vals[150 + i]
                with frame mmtx.
          end.


          run sys/ref/mach-1.p (recid(mstd)).
        end.

        else              /* This is for RUN SPOILAGE matrix's only */
        do i = 1 to 10:
          display mmtx.col-value[i] when mmtx.col-value[i] ne 0
                  mmtx.vals[10 + i] when mmtx.vals[10 + i] ne 0
                    format ">>9.99"  @ mmtx.vals[10 + i]
                  mmtx.vals[20 + i] when mmtx.vals[20 + i] ne 0
                    format ">>9.99"  @ mmtx.vals[20 + i]
                  mmtx.vals[30 + i] when mmtx.vals[30 + i] ne 0
                    format ">>9.99"  @ mmtx.vals[30 + i]
                  mmtx.vals[40 + i] when mmtx.vals[40 + i] ne 0
                    format ">>9.99"  @ mmtx.vals[40 + i]
                  mmtx.vals[50 + i] when mmtx.vals[50 + i] ne 0
                    format ">>9.99"  @ mmtx.vals[50 + i]
                  mmtx.vals[60 + i] when mmtx.vals[60 + i] ne 0
                    format ">>9.99"  @ mmtx.vals[60 + i]
                  mmtx.vals[70 + i] when mmtx.vals[70 + i] ne 0
                    format ">>9.99"  @ mmtx.vals[70 + i]
                  mmtx.vals[80 + i] when mmtx.vals[80 + i] ne 0
                    format ">>9.99"  @ mmtx.vals[80 + i]
                  mmtx.vals[90 + i] when mmtx.vals[90 + i] ne 0
                    format ">>9.99"  @ mmtx.vals[90 + i]
                  mmtx.vals[100 + i] when mmtx.vals[100 + i] ne 0
                    format ">>9.99"   @ mmtx.vals[100 + i]
                  mmtx.vals[110 + i] when mmtx.vals[110 + i] ne 0
                    format ">>9.99"   @ mmtx.vals[110 + i]
                  mmtx.vals[120 + i] when mmtx.vals[120 + i] ne 0
                    format ">>9.99"   @ mmtx.vals[120 + i]
                  mmtx.vals[130 + i] when mmtx.vals[130 + i] ne 0
                    format ">>9.99"   @ mmtx.vals[130 + i]
                  mmtx.vals[140 + i] when mmtx.vals[140 + i] ne 0
                    format ">>9.99"   @ mmtx.vals[140 + i]
                  mmtx.vals[150 + i] when mmtx.vals[150 + i] ne 0
                    format ">>9.99"   @ mmtx.vals[150 + i]
              with frame mmtx.
        end.

        down.
      end.
    end.

    page.
  end.
end.

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */

end procedure.

*/

END PROCEDURE.

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

