&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: addon\touch\r-postre.w

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
DEF VAR tmp-dir AS cha NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

/*{sys/inc/var.i new shared} */
def new shared var cocode as cha no-undo.
def new shared var locode as cha no-undo.

assign
 cocode = gcompany
 locode = gloc.

DEF VAR v-invalid AS LOG NO-UNDO.
def var v-download as log init no no-undo.
def var v-prior as log init no no-undo.

def buffer tmp-per for period.
DEF BUFFER b-mach FOR mach.

def stream st-mach.
def stream st-emp.


DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

&Scoped-define SCOPDEFS post
&SCOPED-DEFINE where-statement machtran.company = g_company


DEFINE TEMP-TABLE ttbl_pc-prdd NO-UNDO LIKE pc-prdd
       INDEX ttbl_pc-prdd IS PRIMARY
             company m-code op-date shift job frm blank-no.
DEFINE TEMP-TABLE ttbl_pc-prdh NO-UNDO LIKE pc-prdh
       INDEX ttbl_pc-prdh IS PRIMARY
             company m-code trans-date shift.
DEFINE TEMP-TABLE ttbl_rowid NO-UNDO
  FIELD pc-prdd_rowid AS ROWID
  FIELD total_time AS INTEGER.
DEFINE VARIABLE machtotaltime AS DECIMAL NO-UNDO.
DEFINE VARIABLE shiftpct AS DECIMAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE waste-qty AS DECIMAL NO-UNDO.
DEFINE VARIABLE run-qty AS DECIMAL NO-UNDO.
DEFINE VARIABLE selected-company AS CHARACTER FORMAT "X(3)" LABEL "Company" NO-UNDO.

DEF VAR lv-valid-to-post AS LOG NO-UNDO.

{pc/pcprdd4u.i NEW}

{jc/jcgl-sh.i NEW}
DO TRANSACTION:
  {sys/inc/dcpostgl.i}
  {sys/inc/tspost.i}
  {sys/inc/tssecure.i}
END.
def var v-autopost  as   log NO-UNDO.
def var v-auto-bin  like sys-ctrl.char-fld no-undo.
def var v-rm-fg     as   log NO-UNDO.
DEF TEMP-TABLE tt-report NO-UNDO LIKE report.
def TEMP-TABLE w-job field job like job.job.


FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "AUTOPOST" NO-LOCK NO-ERROR.
v-autopost = IF AVAIL sys-ctrl THEN sys-ctrl.log-fld ELSE NO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_machine end_machine ~
begin_job-no begin_job-no2 end_job-no end_job-no2 begin_date end_date ~
begin_shift end_shift t-prt-rate rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_machine end_machine begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_date end_date begin_shift ~
end_shift t-prt-rate rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE begin_job-no2 AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_machine AS CHARACTER FORMAT "X(10)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE begin_shift AS CHARACTER FORMAT "X":U 
     LABEL "Beginning Shift" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE end_job-no2 AS INTEGER FORMAT ">9":U INITIAL 99 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE end_machine AS CHARACTER FORMAT "X(10)" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE end_shift AS CHARACTER FORMAT "X":U 
     LABEL "Ending Shift" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

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
     SIZE 92 BY 8.57.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.24.

DEFINE VARIABLE post AS LOGICAL INITIAL no 
     LABEL "RePost Data Collection Entries" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE t-prt-rate AS LOGICAL INITIAL no 
     LABEL "Print Labor Rates" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_machine AT ROW 2.43 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_machine AT ROW 2.43 COL 59 COLON-ALIGNED HELP
          "Enter Ending Machine"
     begin_job-no AT ROW 3.62 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     begin_job-no2 AT ROW 3.62 COL 35 COLON-ALIGNED HELP
          "Enter Beginning Shift"
     end_job-no AT ROW 3.62 COL 59 COLON-ALIGNED HELP
          "Enter Ending Machine"
     end_job-no2 AT ROW 3.62 COL 71 COLON-ALIGNED HELP
          "Enter Beginning Shift"
     begin_date AT ROW 5.05 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 5.05 COL 59 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_shift AT ROW 6.48 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Shift"
     end_shift AT ROW 6.71 COL 59 COLON-ALIGNED HELP
          "Enter Ending Shift"
     post AT ROW 8.38 COL 27 HELP
          "Post to ASI Database Indicator"
     t-prt-rate AT ROW 9.57 COL 27
     rd-dest AT ROW 12.91 COL 6 NO-LABEL
     lv-ornt AT ROW 12.91 COL 29 NO-LABEL
     lines-per-page AT ROW 12.91 COL 82 COLON-ALIGNED
     lv-font-no AT ROW 15.05 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 16 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 18.38 COL 31
     btn-ok AT ROW 21.24 COL 18
     btn-cancel AT ROW 21.24 COL 56
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.95 COL 3
     RECT-6 AT ROW 11.71 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 22.81.


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
         TITLE              = "RePos Touch Data to ASI Database"
         HEIGHT             = 23.38
         WIDTH              = 95.4
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
   FRAME-NAME L-To-R                                                    */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX post IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       post:HIDDEN IN FRAME FRAME-A           = TRUE
       post:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       t-prt-rate:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* RePos Touch Data to ASI Database */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* RePos Touch Data to ASI Database */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON HELP OF begin_date IN FRAME FRAME-A /* Beginning Date */
DO:
  {methods/calendar.i}
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
  ASSIGN {&displayed-objects}.

  IF t-prt-rate AND lv-tssecure THEN DO:
     DEF VAR lv-passwd AS cha NO-UNDO.
     RUN custom/d-passwd.w (OUTPUT lv-passwd).
     IF lv-tssecure AND lv-tssecure-val <> lv-passwd THEN DO:
        MESSAGE "Security Error! Invalid Password. " VIEW-AS ALERT-BOX ERROR.
        t-prt-rate = NO.
        DISPLAY t-prt-rate.
     END.
  END.

  IF LENGTH(begin_job-no) < 6 THEN begin_job-no = FILL(" ", 6 - LENGTH(begin_job-no)) + TRIM(begin_job-no).
  IF LENGTH(end_job-no) < 6 THEN end_job-no = FILL(" ", 6 - LENGTH(end_job-no)) + TRIM(end_job-no).

  lv-valid-to-post = NO.
  RUN run-report.

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type="TS Posting"
                            &begin_cust=begin_machine
                            &END_cust= begin_machine
                            &fax-subject="TS Posting"
                            &fax-body="TS Posting"
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "TS Posting"
                             &begin_cust= begin_machine
                             &END_cust=begin_machine
                             &mail-subject="TS Posting"
                             &mail-body="TS Posting"
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "TS Posting"
                                  &begin_cust= begin_machine
                                  &END_cust=begin_machine
                                  &mail-subject=CURRENT-WINDOW:TITLE
                                  &mail-body=CURRENT-WINDOW:TITLE
                                  &mail-file=list-name }
     END.
       END.
       WHEN 6 THEN RUN OUTPUT-to-port.

  end case. 

  IF lv-valid-to-post THEN DO:
        MESSAGE "Are you ready to Post?" VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans AS LOG.
        IF ll-ans THEN DO:
           RUN do-post.
           MESSAGE "Post completed..." VIEW-AS ALERT-BOX.
        END.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON HELP OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
  {methods/calendar.i}
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
/*{sys/inc/f3helpw.i} */
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

  RUN init-proc.
  RUN enable_UI.
  {custom/usrprint.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-post C-Win 
PROCEDURE do-post :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE tt-report.

FOR EACH ttbl_pc-prdh:
  FIND first pc-prdh WHERE pc-prdh.company = ttbl_pc-prdh.company
                AND pc-prdh.m-code = ttbl_pc-prdh.m-code
                AND pc-prdh.shift = ttbl_pc-prdh.shift 
                AND pc-prdh.trans-date = ttbl_pc-prdh.trans-date
                EXCLUSIVE-LOCK NO-ERROR.
  if not AVAIL pc-prdh  THEN    CREATE  pc-prdh.
  BUFFER-COPY ttbl_pc-prdh TO pc-prdh.

END.

FOR EACH ttbl_pc-prdd:
   FIND FIRST pc-prdd
           WHERE pc-prdd.company = ttbl_pc-prdd.company
             AND pc-prdd.m-code = ttbl_pc-prdd.m-code 
             AND pc-prdd.job-no = ttbl_pc-prdd.job-no 
             AND pc-prdd.job-no2 = ttbl_pc-prdd.job-no2 
             AND pc-prdd.frm = ttbl_pc-prdd.frm 
             AND pc-prdd.blank-no = ttbl_pc-prdd.blank-no 
             AND pc-prdd.pass = ttbl_pc-prdd.pass 
             AND pc-prdd.i-no = ttbl_pc-prdd.i-no
             AND pc-prdd.code = ttbl_pc-prdd.code
             AND pc-prdd.op-date = ttbl_pc-prdd.op-date
             AND pc-prdd.start = ttbl_pc-prdd.start 
             AND pc-prdd.stopp = ttbl_pc-prdd.stopp 
             AND pc-prdd.shift = ttbl_pc-prdd.shift 
             exclusive-lock no-error.
    if not avail pc-prdd THEN        CREATE  pc-prdd.
    BUFFER-COPY ttbl_pc-prdd TO pc-prdd.
    IF v-tspost THEN DO:
       CREATE tt-report.
       tt-report.rec-id = RECID(pc-prdd).
    END.

END.

IF v-tspost THEN RUN post-wip.

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
  DISPLAY begin_machine end_machine begin_job-no begin_job-no2 end_job-no 
          end_job-no2 begin_date end_date begin_shift end_shift t-prt-rate 
          rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_machine end_machine begin_job-no begin_job-no2 
         end_job-no end_job-no2 begin_date end_date begin_shift end_shift 
         t-prt-rate rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gl-from-work C-Win 
PROCEDURE gl-from-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-run AS INT NO-UNDO.
  DEF INPUT PARAM ip-trnum AS INT NO-UNDO.

  def var credits as dec init 0 no-undo.
  def var debits as dec init 0 no-undo. 


  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE TODAY
        AND period.pend    GE TODAY
      NO-LOCK.

  for each work-gl 
      where (ip-run eq 1 and work-gl.job-no ne "")
         or (ip-run eq 2 and work-gl.job-no eq "")
      break by work-gl.actnum:

    assign
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    if last-of(work-gl.actnum) then do:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-gl.actnum
       gltrans.jrnl    = "JCOST"
       gltrans.period  = period.pnum
       gltrans.tr-amt  = debits - credits
       gltrans.tr-date = TODAY
       gltrans.tr-dscr = "Production Job Costing"
       gltrans.trnum   = ip-trnum.

      assign
       debits  = 0
       credits = 0.
    end.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc C-Win 
PROCEDURE init-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN begin_date = DATE(1,1,YEAR(TODAY))
         END_date = TODAY
         begin_machine = ""
         END_machine = "zzzzzzzz"
         begin_shift = ""
         END_shift = "z".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN custom/prntproc.p (list-name,INT(lv-font-no), lv-ornt).

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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
def var v-trnum like gl-ctrl.trnum no-undo.
DEF VAR X AS INT NO-UNDO.

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

  assign
   w-job.job = job.job
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

  IF INDEX("AP",mach.p-type) GT 0 THEN
    ASSIGN
     v-on  = 1
     v-up  = 1
     v-out = 1.

  ELSE
  if avail est then do:
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

  if pc-prdd.complete then do:
    RUN pc/pcprdd4u.p (ROWID(pc-prdd)).

    for each tt-job-hdr,

        first itemfg
        where itemfg.company    eq cocode
          and itemfg.i-no       eq tt-job-hdr.i-no
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
       fg-rctd.i-no       = tt-job-hdr.i-no
       fg-rctd.job-no     = pc-prdd.job-no
       fg-rctd.job-no2    = pc-prdd.job-no2.

      assign
       v-up  = 1
       v-out = 1.

      if avail est and index("AB",mach.p-type) le 0 then do:
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
       fg-rctd.b-num      = pc-prdd.blank-no
       fg-rctd.s-num      = pc-prdd.frm
       fg-rctd.t-qty      = pc-prdd.qty / v-up-hs * v-out * v-up
       fg-rctd.pur-uom    = itemfg.prod-uom
       fg-rctd.cost-uom   = itemfg.prod-uom
       fg-rctd.std-cost   = tt-job-hdr.std-tot-cost
       fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
       fg-rctd.qty-case   = itemfg.case-count
       fg-rctd.partial    = fg-rctd.t-qty modulo itemfg.case-count
       fg-rctd.cases      = trunc(fg-rctd.t-qty / itemfg.case-count,0)
       fg-rctd.cases-unit = 1.

      if fg-rctd.t-qty le 0 then fg-rctd.cases = 0.

      release fg-bin.

      FIND FIRST reftable NO-LOCK
          WHERE reftable.reftable EQ "pc/pcprddu3.p"
            AND reftable.company  EQ pc-prdd.company
            AND reftable.code     EQ /*pc-prdd.rec_key*/ STRING(RECID(pc-prdd))
          NO-ERROR.

      IF AVAIL reftable THEN DO:
        ASSIGN
         fg-rctd.cases      = reftable.val[1]
         fg-rctd.qty-case   = reftable.val[2]
         fg-rctd.cases-unit = reftable.val[3]
         fg-rctd.partial    = fg-rctd.t-qty - (fg-rctd.cases * fg-rctd.qty-case).

        FIND FIRST fg-bin 
            WHERE fg-bin.rec_key EQ reftable.code2 /*RECID(fg-bin) EQ INT(reftable.code2)*/ 
            NO-LOCK NO-ERROR.
      END.

      IF AVAIL fg-bin THEN
        ASSIGN
         v-loc       = fg-bin.loc
         v-loc-bin   = fg-bin.loc-bin
         fg-rctd.tag = fg-bin.tag.

      else
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

IF dcpostgl-log THEN DO TRANSACTION:
  /* gdm - 11050906 */
  REPEAT:
     FIND FIRST gl-ctrl EXCLUSIVE-LOCK
       WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
     IF AVAIL gl-ctrl THEN DO:
       ASSIGN v-trnum       = gl-ctrl.trnum + 1
              gl-ctrl.trnum = v-trnum.
       FIND CURRENT gl-ctrl NO-LOCK.
       RUN gl-from-work (1, v-trnum).
       RUN gl-from-work (2, v-trnum).
       LEAVE.
     END.  /* IF AVAIL gl-ctrl */
  END. /* REPEAT */
  /* gdm - 11050906 */
END.

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
/*==== Report main body procedure ================================
==================================================================*/
DEF VAR ld-total-time AS INT NO-UNDO.
DEF VAR ld-emp-time AS INT NO-UNDO.
DEF VAR ld-mr-time AS INT NO-UNDO.
DEF VAR ld-run-time AS INT NO-UNDO.
DEF VAR li-job-cnt AS INT NO-UNDO.
DEF VAR li-mr-cnt AS INT NO-UNDO.
DEF VAR ld-mr-avg AS int NO-UNDO.
DEF VAR ld-run-avg AS int NO-UNDO.
DEF VAR ld-tot-avg AS int NO-UNDO.
def var lv-tmp-mr as int no-undo.
def var lv-tmp-run as int no-undo.
def var lv-tmp-tot as int no-undo.
def var ls-tot-mr as cha  no-undo.
def var ls-tot-run as cha no-undo.
def var ls-tot-tot as cha  no-undo.
def var ld-waste% as dec form ">,>>9.99"       no-undo.
def var lv-rqty like machtran.run_qty no-undo.
def var lv-wqty like machtran.waste_qty no-undo.
DEF VAR v-sch-mach AS LOG NO-UNDO.

SESSION:SET-WAIT-STATE("general").

{sys/form/r-top3w.f}

FORM HEADER SKIP(1) WITH FRAME r-top.


assign
   str-tit2 = c-win:TITLE
   {sys/inc/ctrtext.i str-tit2 112}

   str-tit3 = "Employee Time by Job and Machine"
   {sys/inc/ctrtext.i str-tit3 132}.



{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)}




    FOR EACH ttbl_pc-prdh:    
        DELETE ttbl_pc-prdh.
    END.

    FOR EACH ttbl_pc-prdd:
        DELETE ttbl_pc-prdd.
    END.

    if td-show-parm then run show-param.


VIEW FRAME r-top.
selected-company = g_company.

{methods/lstlogic/custom/postre_.i}

output close.


RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE("").

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
  DEF BUFFER bf-job FOR job.

  FIND b-pc-prdd WHERE ROWID(b-pc-prdd) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-pc-prdd THEN
     FOR FIRST bf-job
         WHERE bf-job.company eq b-pc-prdd.company
           AND bf-job.job     eq b-pc-prdd.job
           AND bf-job.job-no  eq b-pc-prdd.job-no
           AND bf-job.job-no2 eq b-pc-prdd.job-no2
         NO-LOCK,
         FIRST job-hdr
         WHERE job-hdr.company   EQ bf-job.company
           AND job-hdr.job       EQ bf-job.job
           AND job-hdr.job-no    EQ bf-job.job-no
           AND job-hdr.job-no2   EQ bf-job.job-no2
           AND (job-hdr.frm      EQ b-pc-prdd.frm OR
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
             AND eb.est-no  EQ bf-job.est-no
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
           ASSIGN
             prep.no-of-impressions = prep.no-of-impressions +
                                      b-pc-prdd.qty + b-pc-prdd.waste
             prep.last-date        = b-pc-prdd.op-date
             prep.last-job-no      = b-pc-prdd.job-no
             prep.last-job-no2     = b-pc-prdd.job-no2
             .
           RELEASE prep.
         END.
       END.
     END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

