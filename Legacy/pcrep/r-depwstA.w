&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: pcrep\r-depwst.w

  Description: Data Collection Waste by Dept.

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

 def TEMP-TABLE work-rep NO-UNDO
   field dept as char format 'xx'
   field m-code like mach.m-code
   field run-qty as int format '>>>>>>>>>9'
   field wst-qty as int format '>>>>>>>9'
   field mr-qty  as int
   field mr-waste as int
   field std-run-qty as int
   field std-wst-qty as int
   field std-mr-qty as int.

def TEMP-TABLE work-mch NO-UNDO
   field job like job.job
   field frm like job-mch.frm
   field blank-no like job-mch.blank-no
   field m-code like job-mch.m-code
   field dept like job-mch.dept
   field pass like job-mch.pass
   field run-qty as dec
   field wst-qty as dec
   field mr-qty as dec
   field mr-waste as dec.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_dept end_dept begin_mach ~
end_mach begin_date end_date lv-ornt lines-per-page rd-dest lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_dept end_dept begin_mach end_mach ~
begin_date end_date lv-ornt lines-per-page rd-dest lv-font-no lv-font-name ~
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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_dept AS CHARACTER FORMAT "X(4)" 
     LABEL "Beginning Department" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_dept AS CHARACTER FORMAT "X(4)" INITIAL "zzzz" 
     LABEL "Ending Department" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-depwst.csv" 
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
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 93 BY 9.76.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 9.52.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_dept AT ROW 4.33 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Department"
     end_dept AT ROW 4.33 COL 69 COLON-ALIGNED HELP
          "Enter Ending Department"
     begin_mach AT ROW 5.29 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_mach AT ROW 5.29 COL 69 COLON-ALIGNED HELP
          "Enter Ending Machine"
     begin_date AT ROW 6.24 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 6.24 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     lv-ornt AT ROW 11.95 COL 31 NO-LABEL
     lines-per-page AT ROW 11.95 COL 84 COLON-ALIGNED
     rd-dest AT ROW 12.19 COL 5 NO-LABEL
     lv-font-no AT ROW 14.33 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 15.52 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.71 COL 31
     tb_excel AT ROW 18.33 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 18.33 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 19.14 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 20.71 COL 19
     btn-cancel AT ROW 20.71 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.48 COL 3
     RECT-6 AT ROW 10.76 COL 1
     RECT-7 AT ROW 1 COL 1
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
         TITLE              = "Departmental Waste Report"
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
       begin_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
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

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Departmental Waste Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Departmental Waste Report */
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


&Scoped-define SELF-NAME begin_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_dept C-Win
ON LEAVE OF begin_dept IN FRAME FRAME-A /* Beginning Department */
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
  SESSION:SET-WAIT-STATE ("general").

  assign rd-dest.
  IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "southpak"
       THEN is-xprint-form = YES.     
  ELSE is-xprint-form = NO.

  run run-report. 

  SESSION:SET-WAIT-STATE ("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= ''
                            &begin_cust=begin_mach
                            &END_cust= begin_mach
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = ''
                             &begin_cust= begin_mach
                             &END_cust=begin_mach
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust= begin_mach
                                  &END_cust=begin_mach
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
     END.
       END.
       WHEN 6 THEN RUN OUTPUT-to-port.

  end case. 
  SESSION:SET-WAIT-STATE("").
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


&Scoped-define SELF-NAME end_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_dept C-Win
ON LEAVE OF end_dept IN FRAME FRAME-A /* Ending Department */
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

  assign
   begin_date = date(month(TODAY), 1, year(TODAY))
   end_date   = today.

  RUN enable_UI.

  {methods/nowait.i}

   DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_dept.
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
  DISPLAY begin_dept end_dept begin_mach end_mach begin_date end_date lv-ornt 
          lines-per-page rd-dest lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_dept end_dept begin_mach end_mach begin_date 
         end_date lv-ornt lines-per-page rd-dest lv-font-no td-show-parm 
         tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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

   {custom/out2file.i}.

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
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ pc/rep/mch-wst.p 8/94 gb */
/* Departmental Waste Report                                        */
/* -------------------------------------------------------------------------- */
{sys/form/r-topw.f}

def var v-date as date format "99/99/9999" extent 2 no-undo.
def var v-dept like mch-act.dept extent 2 initial ["","zz"].
def var v-mach like mch-act.m-code extent 2 initial ["","zzzzzz"].
def var v2-dept as ch format 'x(4)' no-undo.
def var mr-std-pct as dec format '>>>9.99' no-undo.
def var mr-act-pct as dec format '>>>9.99' no-undo.
def var mr-var-pct as dec format '->>>9.99' no-undo.
def var wst-std-pct as dec format '>>>9.99' no-undo.
def var wst-act-pct as dec format '>>>9.99' no-undo.
def var wst-var-pct as dec format '->>>9.99' no-undo.
def var tot-std-pct as dec format '>>>9.99' no-undo.
def var tot-act-pct as dec format '>>>9.99' no-undo.
def var tot-var-pct as dec format '->>>9.99' no-undo.
def var d-run-qty as int.
def var d-mr-qty as int.
def var d-wst-qty as int.
def var d-std-run-qty as int.
def var d-std-mr-qty as int.
def var d-std-wst-qty as int.
def var g-run-qty as int.
def var g-mr-qty as int.
def var g-wst-qty as int.
def var g-std-run-qty as int.
def var g-std-mr-qty as int.
def var g-std-wst-qty as int.
def var v-dscr like dept.dscr no-undo.
DEF VAR excelheader AS CHAR NO-UNDO.

SESSION:SET-WAIT-STATE ("general").

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-dept[1]   = begin_dept
 v-dept[2]   = end_dept
 v-mach[1]   = begin_mach
 v-mach[2]   = end_mach
 v-date[1]   = begin_date
 v-date[2]   = END_date.


{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "MACH CODE,DESCRIPTION,RUN QUANTITY,MR QUANTITY,"
              + "WASTE QUANTITY,MR STD PERCENT,MR ACT PERCENT,"
              + "MR VARIANCE,WASTE STD PCT,WASTE ACT PCT,"
              + "WASTE VARIANCE,TOTAL STD PCT,TOTAL ACT PCT,"
              + "TOTAL VARIANCE".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

display "" with frame r-top.

  for each mch-act
       where mch-act.company eq cocode
         and mch-act.op-date ge v-date[1]
         and mch-act.op-date le v-date[2]
         and mch-act.m-code  ge v-mach[1]
         and mch-act.m-code  le v-mach[2]
         and mch-act.dept    ge v-dept[1]
         and mch-act.dept    le v-dept[2]
       use-index dte-idx no-lock:

      find first work-mch
          where work-mch.job = mch-act.job
            and work-mch.frm = mch-act.frm
            and work-mch.blank-no = mch-act.blank-no
            and work-mch.m-code = mch-act.m-code
            and work-mch.pass = mch-act.pass
          no-error.
      if not available work-mch then do:
         create work-mch.
         assign
          work-mch.job = mch-act.job
          work-mch.frm = mch-act.frm
          work-mch.blank-no = mch-act.blank-no
          work-mch.m-code = mch-act.m-code
          work-mch.dept = mch-act.dept
          work-mch.pass = mch-act.pass.
      end.
      find job-code where job-code.code = mch-act.code no-lock.
      if job-code.cat eq "RUN" then
         assign
          work-mch.run-qty = work-mch.run-qty +
                             IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty
          work-mch.wst-qty = work-mch.wst-qty + mch-act.waste.
      else
      if job-code.cat eq "MR" then
         assign
          work-mch.mr-qty   = work-mch.mr-qty +
                              IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty
          work-mch.mr-waste = work-mch.mr-waste + mch-act.waste.
   end.

   for each work-mch:
      find first job-mch
          where job-mch.company  = cocode
            and job-mch.job      = work-mch.job
            and job-mch.frm      = work-mch.frm
            and job-mch.blank-no = work-mch.blank-no
            and job-mch.m-code   = work-mch.m-code
            and job-mch.pass     = work-mch.pass
          no-lock no-error.
      v2-dept = if avail job-mch then job-mch.dept else work-mch.dept.

      find first work-rep
          where work-rep.dept   = v2-dept
            and work-rep.m-code = work-mch.m-code
          no-error.
      if not available work-rep then do:
         create work-rep.
         assign
          work-rep.dept = v2-dept
          work-rep.m-code = work-mch.m-code.
      end.
      assign
       work-rep.run-qty = work-rep.run-qty + work-mch.run-qty
       work-rep.wst-qty = work-rep.wst-qty + work-mch.wst-qty
       work-rep.mr-qty  = work-rep.mr-qty  + work-mch.mr-qty
       work-rep.mr-waste = work-rep.mr-waste + work-mch.mr-waste.
      if available job-mch then
         assign
          work-rep.std-run-qty = work-rep.std-run-qty + job-mch.run-qty
          work-rep.std-wst-qty = work-rep.std-wst-qty   +
                                 (job-mch.run-qty * (job-mch.wst-prct * .01))
          work-rep.std-mr-qty  = work-rep.std-mr-qty + job-mch.mr-waste.
   end.



      for each work-rep break by work-rep.dept by work-rep.m-code:

         find first mach
             where mach.company = cocode
               and mach.m-code  = work-rep.m-code
             no-lock.

         assign
          mr-std-pct  = work-rep.std-mr-qty / work-rep.std-run-qty * 100
          mr-act-pct  = (work-rep.mr-qty + work-rep.mr-waste) /
                                                  work-rep.run-qty * 100

          wst-std-pct = work-rep.std-wst-qty / work-rep.std-run-qty * 100
          wst-act-pct = work-rep.wst-qty     / work-rep.run-qty     * 100

          tot-std-pct = (work-rep.std-mr-qty + work-rep.std-wst-qty) /
                                               work-rep.std-run-qty * 100
          tot-act-pct = (work-rep.mr-qty + work-rep.mr-waste + work-rep.wst-qty)
                                             / work-rep.run-qty * 100.

         if mr-std-pct  = ? then mr-std-pct  = 0.
         if mr-act-pct  = ? then mr-act-pct  = 0.
         if wst-act-pct = ? then wst-act-pct = 0.
         if wst-std-pct = ? then wst-std-pct = 0.
         if tot-act-pct = ? then tot-act-pct = 0.
         if tot-std-pct = ? then tot-std-pct = 0.

         assign
          mr-var-pct  = mr-std-pct  - mr-act-pct
          wst-var-pct = wst-std-pct - wst-act-pct
          tot-var-pct = tot-std-pct - tot-act-pct.

         display work-rep.m-code    column-label "MACH!CODE"
                    when first-of(work-rep.m-code)
                 mach.m-dscr        column-label "DESCRIPTION"
                    when first-of(work-rep.m-code)
                 work-rep.run-qty   column-label "RUN!QUANTITY"
                 (work-rep.mr-waste + work-rep.mr-qty) format ">>>>>>>9"
                                    column-label "MR!QUANTITY"
                 work-rep.wst-qty   column-label "WASTE!QUANTITY"
                 mr-std-pct         column-label "MR STD!PERCENT"
                 mr-act-pct         column-label "MR ACT!PERCENT"
                 mr-var-pct         column-label "MR!VARIANCE"
                 wst-std-pct        column-label "WASTE!STD PCT"
                 wst-act-pct        column-label "WASTE!ACT PCT"
                 wst-var-pct        column-label "WASTE!VARIANCE"
                 tot-std-pct        column-label "TOTAL!STD PCT"
                 tot-act-pct        column-label "TOTAL!ACT PCT"
                 tot-var-pct        column-label "TOTAL!VARIANCE"
             with frame det STREAM-IO width 132 no-box down.

         down with frame det.

         IF tb_excel THEN
            PUT STREAM excel UNFORMATTED
               '"' (IF first-of(work-rep.m-code) THEN
                       work-rep.m-code ELSE "")                  '",'
               '"' (IF first-of(work-rep.m-code) THEN
                       mach.m-dscr ELSE "")                      '",'
               '"' work-rep.run-qty                              '",'
               '"' STRING((work-rep.mr-waste + work-rep.mr-qty),
                           ">>>>>>>9-")                          '",'
               '"' work-rep.wst-qty                              '",'
               '"' string(mr-std-pct,'>>>9.99')                  '",'
               '"' STRING(mr-act-pct,'>>>9.99')                  '",'
               '"' STRING(mr-var-pct,'>>>9.99-')                 '",'
               '"' STRING(wst-std-pct,'>>>9.99')                 '",'
               '"' STRING(wst-act-pct,'>>>9.99')                 '",'
               '"' STRING(wst-var-pct,'->>>9.99')                '",'
               '"' STRING(tot-std-pct,'>>>9.99')                 '",'
               '"' STRING(tot-act-pct,'>>>9.99')                 '",'
               '"' STRING(tot-var-pct,'->>>9.99')                '",'
               SKIP.

         assign
          d-run-qty = d-run-qty + work-rep.run-qty
          d-mr-qty  = d-mr-qty  + work-rep.mr-waste + work-rep.mr-qty
          d-wst-qty = d-wst-qty + work-rep.wst-qty

          d-std-run-qty = d-std-run-qty + work-rep.std-run-qty
          d-std-mr-qty  = d-std-mr-qty  + work-rep.std-mr-qty
          d-std-wst-qty = d-std-wst-qty + work-rep.std-wst-qty.

         if last-of(work-rep.dept) then do:
            find dept where dept.company = cocode and
                            dept.code    = work-rep.dept
                            no-lock no-error.
            if not available dept then
               find dept where dept.company = "" and
                               dept.code    = work-rep.dept
                               no-lock no-error.
            if available dept then
               v-dscr = dept.dscr.
            else
               v-dscr = "Department Not on File".

            assign
             mr-std-pct  = d-std-mr-qty  / d-std-run-qty * 100
             mr-act-pct  = d-mr-qty      / d-run-qty     * 100

             wst-std-pct = d-std-wst-qty / d-std-run-qty * 100
             wst-act-pct = d-wst-qty     / d-run-qty     * 100

             tot-std-pct = (d-std-mr-qty + d-std-wst-qty) / d-std-run-qty * 100
             tot-act-pct = (d-mr-qty     + d-wst-qty)     / d-run-qty     * 100.

            if mr-std-pct  = ? then mr-std-pct  = 0.
            if mr-act-pct  = ? then mr-act-pct  = 0.
            if wst-act-pct = ? then wst-act-pct = 0.
            if wst-std-pct = ? then wst-std-pct = 0.
            if tot-act-pct = ? then tot-act-pct = 0.
            if tot-std-pct = ? then tot-std-pct = 0.

            assign
             mr-var-pct  = mr-std-pct  - mr-act-pct
             wst-var-pct = wst-std-pct - wst-act-pct
             tot-var-pct = tot-std-pct - tot-act-pct.

            put  fill("-",124) format "x(124)" at 8 skip
                 v-dscr at 8       format "x(23)"
                 d-run-qty         format ">>>>>>>9-"
                 d-mr-qty          format ">>>>>>>9-"
                 d-wst-qty         format ">>>>>>>9-"
                 mr-std-pct        space(1)
                 mr-act-pct        space(1)
                 mr-var-pct        space(1)
                 wst-std-pct       space(1)
                 wst-act-pct       space(1)
                 wst-var-pct       space(1)
                 tot-std-pct       space(1)
                 tot-act-pct       space(1)
                 tot-var-pct       skip
                 fill("-", 131) format "x(131)" skip.

            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                  SKIP(1)
                  '"' ""                            '",'
                  '"' v-dscr                        '",'
                  '"' STRING(d-run-qty,">>>>>>>9-") '",'
                  '"' STRING(d-mr-qty,">>>>>>>9-")  '",'
                  '"' STRING(d-wst-qty,">>>>>>>9-") '",'
                  '"' STRING(mr-std-pct,'>>>9.99')  '",'
                  '"' STRING(mr-act-pct,'>>>9.99')  '",'
                  '"' STRING(mr-var-pct,'->>>9.99') '",'
                  '"' STRING(wst-std-pct,'>>>9.99') '",'
                  '"' STRING(wst-act-pct,'>>>9.99') '",'
                  '"' STRING(wst-var-pct,'->>>9.99') '",'
                  '"' STRING(tot-std-pct,'>>>9.99')                 '",'
                  '"' STRING(tot-act-pct,'>>>9.99')                 '",'
                  '"' STRING(tot-var-pct,'->>>9.99')                '",'
                  SKIP(1).

            assign
             g-run-qty = g-run-qty + d-run-qty
             g-mr-qty  = g-mr-qty  + d-mr-qty
             g-wst-qty = g-wst-qty + d-wst-qty

             g-std-run-qty = g-std-run-qty + d-std-run-qty
             g-std-mr-qty  = g-std-mr-qty  + d-std-mr-qty
             g-std-wst-qty = g-std-wst-qty + d-std-wst-qty

             d-run-qty = 0
             d-mr-qty  = 0
             d-wst-qty = 0

             d-std-run-qty = 0
             d-std-mr-qty  = 0
             d-std-wst-qty = 0.
         end.

         if last(work-rep.dept) then do:
            assign
             mr-std-pct  = g-std-mr-qty  / g-std-run-qty * 100
             mr-act-pct  = g-mr-qty      / g-run-qty     * 100

             wst-std-pct = g-std-wst-qty / g-std-run-qty * 100
             wst-act-pct = g-wst-qty     / g-run-qty     * 100

             tot-std-pct = (g-std-mr-qty + g-std-wst-qty) / g-std-run-qty * 100
             tot-act-pct = (g-mr-qty     + g-wst-qty)     / g-run-qty     * 100.

            if mr-std-pct  = ? then mr-std-pct  = 0.
            if mr-act-pct  = ? then mr-act-pct  = 0.
            if wst-act-pct = ? then wst-act-pct = 0.
            if wst-std-pct = ? then wst-std-pct = 0.
            if tot-act-pct = ? then tot-act-pct = 0.
            if tot-std-pct = ? then tot-std-pct = 0.

            assign
             mr-var-pct  = mr-std-pct  - mr-act-pct
             wst-var-pct = wst-std-pct - wst-act-pct
             tot-var-pct = tot-std-pct - tot-act-pct.

            put  fill("-",131) format "x(131)" skip
                 "GRAND TOTALS"    format "x(23)" at 8
                 g-run-qty         format ">>>>>>>9-"
                 g-mr-qty          format ">>>>>>>9-"
                 g-wst-qty         format ">>>>>>>9-"
                 mr-std-pct        space(1)
                 mr-act-pct        space(1)
                 mr-var-pct        space(1)
                 wst-std-pct       space(1)
                 wst-act-pct       space(1)
                 wst-var-pct       space(1)
                 tot-std-pct       space(1)
                 tot-act-pct       space(1)
                 tot-var-pct       skip
                 fill("-", 131) format "x(131)" skip.

            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                  SKIP(1)
                  '"' ""                            '",'
                  '"' "GRAND TOTALS"                '",'
                  '"' STRING(g-run-qty,">>>>>>>9-") '",'
                  '"' STRING(g-mr-qty,">>>>>>>9-")  '",'
                  '"' STRING(g-wst-qty,">>>>>>>9-") '",'
                  '"' STRING(mr-std-pct,'>>>9.99')  '",'
                  '"' STRING(mr-act-pct,'>>>9.99')  '",'
                  '"' STRING(mr-var-pct,'->>>9.99') '",'
                  '"' STRING(wst-std-pct,'>>>9.99') '",'
                  '"' STRING(wst-act-pct,'>>>9.99') '",'
                  '"' STRING(wst-var-pct,'->>>9.99') '",'
                  '"' STRING(tot-std-pct,'>>>9.99')                 '",'
                  '"' STRING(tot-act-pct,'>>>9.99')                 '",'
                  '"' STRING(tot-var-pct,'->>>9.99')                '",'
                  SKIP(1).

         end.
      end. /* each item */

EMPTY TEMP-TABLE work-rep.
EMPTY TEMP-TABLE work-mch.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

SESSION:SET-WAIT-STATE ("").
RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
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
  def var lv-label as cha NO-UNDO.

  ASSIGN
  lv-frame-hdl = frame {&frame-name}:HANDLE
  lv-group-hdl = lv-frame-hdl:first-child
  lv-field-hdl = lv-group-hdl:first-child.

  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + ",".
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
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

