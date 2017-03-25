&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-fnstmt.w

  Description: GL Financial Statements

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
DEF VAR tmp-dir AS cha NO-UNDO.

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

{gl/gl-fs.i NEW}

format
  v-hdr[1] at 1 format "x(200)" skip
  v-hdr[2] at 1 format "x(200)" skip
  v-hdr[3] at 1 format "x(200)" skip
  v-hdr[4] at 1 format "x(200)" skip
  v-hdr[5] at 1 format "x(200)" skip(2)
  r-top1   at 1 format "x(200)"
  r-top2   at 1 format "x(200)"
  with frame rpt-top page-top no-box no-labels width 200 STREAM-IO.

 DEF BUFFER bf-rpt FOR gl-rpt.
 DEF VAR is-xprint-form AS LOG NO-UNDO.
 DEF VAR ls-fax-file AS cha NO-UNDO.
 DEF NEW SHARED STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-10 RECT-6 RECT-7 v-dscr tran-date ~
select-rpt tb_pre tb_paid tb_supp tb_acct# tb_round tb_mul-comp ~
lv-company-list sub-acct-lvl begin_sub-acct end_sub-acct lv-ornt ~
lines-per-page rd-dest lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS v-dscr tran-date select-rpt tran-period ~
tb_pre tb_paid tb_supp tb_acct# tb_round tb_mul-comp lv-company-list ~
sub-acct-lvl begin_sub-acct end_sub-acct lv-ornt lines-per-page rd-dest ~
lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_sub-acct AS INTEGER FORMAT "->>>>>>>>9" INITIAL 0 
     LABEL "Beginning Subacct" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE end_sub-acct AS INTEGER FORMAT "->>>>>>>>9" INITIAL 999999999 
     LABEL "Ending Subacct" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-fnstmt.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-company-list AS CHARACTER FORMAT "X(100)" 
     LABEL "List" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE rpt-codes AS CHARACTER FORMAT "X(256)":U 
     LABEL "Reports" 
     VIEW-AS FILL-IN 
     SIZE 1 BY 1 NO-UNDO.

DEFINE VARIABLE sub-acct-lvl AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Sub Account Level" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-dscr AS CHARACTER FORMAT "x(50)" 
     LABEL "Report Heading" 
     VIEW-AS FILL-IN 
     SIZE 57 BY 1.

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
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 3.76.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 8.57.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 15.

DEFINE VARIABLE select-rpt AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     SIZE 49 BY 5.71
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_acct# AS LOGICAL INITIAL no 
     LABEL "Print GL Acct#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_mul-comp AS LOGICAL INITIAL no 
     LABEL "Multiple Companies?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tb_paid AS LOGICAL INITIAL yes 
     LABEL "Skip Zero Lines?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_pre AS LOGICAL INITIAL yes 
     LABEL "Pre Close Period?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_round AS LOGICAL INITIAL no 
     LABEL "Suppress Decimals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_supp AS LOGICAL INITIAL yes 
     LABEL "Suppress Zero Fields?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     rpt-codes AT ROW 1.24 COL 88 COLON-ALIGNED
     v-dscr AT ROW 2.43 COL 20 COLON-ALIGNED HELP
          "Report Description"
     tran-date AT ROW 3.62 COL 20 COLON-ALIGNED
     select-rpt AT ROW 4.33 COL 45 HELP
          "Select Reports you wish to print." NO-LABEL
     tran-period AT ROW 4.81 COL 20 COLON-ALIGNED
     tb_pre AT ROW 6 COL 5
     tb_paid AT ROW 6.95 COL 5
     tb_supp AT ROW 7.91 COL 5
     tb_acct# AT ROW 8.86 COL 5
     tb_round AT ROW 9.81 COL 5
     tb_mul-comp AT ROW 10.76 COL 5
     lv-company-list AT ROW 10.76 COL 33 COLON-ALIGNED HELP
          "List"
     sub-acct-lvl AT ROW 13.38 COL 31 COLON-ALIGNED
     begin_sub-acct AT ROW 14.57 COL 31 COLON-ALIGNED
     end_sub-acct AT ROW 14.57 COL 66 COLON-ALIGNED
     lv-ornt AT ROW 16.95 COL 30 NO-LABEL
     lines-per-page AT ROW 16.95 COL 83 COLON-ALIGNED
     rd-dest AT ROW 17.43 COL 5 NO-LABEL
     lv-font-no AT ROW 18.86 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 20.05 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 21.29 COL 30.2
     tb_excel AT ROW 22.43 COL 50 RIGHT-ALIGNED
     tb_runExcel AT ROW 22.43 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 23.38 COL 45.2 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 25.05 COL 26
     btn-cancel AT ROW 25.05 COL 56
     "SORT OPTIONS" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 12.43 COL 38
          FGCOLOR 9 FONT 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 4
          BGCOLOR 2 
     "Select/Deselect Reports" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 3.62 COL 51
          FONT 6
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 16.24 COL 5
     RECT-10 AT ROW 12 COL 5
     RECT-6 AT ROW 16 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.4 BY 25.67.


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
         TITLE              = "GL Financial Statements"
         HEIGHT             = 25.76
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
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_sub-acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_sub-acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       lv-company-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rpt-codes IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rpt-codes:HIDDEN IN FRAME FRAME-A           = TRUE
       rpt-codes:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       sub-acct-lvl:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_acct#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_mul-comp:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_paid:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_pre:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_round:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_supp:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tran-period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       v-dscr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* GL Financial Statements */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* GL Financial Statements */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON HELP OF FRAME FRAME-A
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR lk-recid AS RECID NO-UNDO.

    CASE FOCUS:NAME :
        WHEN "lv-rpt" THEN DO:
             RUN gl/l-glrpt (g_company,FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT lk-recid).
             FIND gl-rpt WHERE RECID(gl-rpt) EQ lk-recid NO-LOCK NO-ERROR.
             IF AVAIL  gl-rpt AND gl-rpt.rpt NE FOCUS:SCREEN-VALUE THEN DO:              
               FOCUS:SCREEN-VALUE = gl-rpt.rpt.
               RUN new-rpt.
             END.
        END.
    END CASE.

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
  /*RUN valid-rpt NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.*/

  RUN check-date NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-sub-lvl NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN /*rd-dest
           tran-date
           tran-period
           tb_pre
           tb_paid
           tb_mul-comp
           lv-company-list
           sub-acct-lvl
           begin_sub-acct
           END_sub-acct*/
           {&displayed-objects}
           udate = tran-date
           uperiod = tran-period
           pre-close = tb_pre
           skip_zero = tb_paid
           supp_zero = tb_supp
           consolidate = tb_mul-comp
           company-list = lv-company-list
           subac-lvl = sub-acct-lvl
           fsubac = begin_sub-acct
           tsubac = END_sub-acct
           ll-acct# = tb_acct#.
  END.      

  run run-report.

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=tran-date
                            &END_cust=tran-date
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:title 
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end CASE.
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


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON VALUE-CHANGED OF lv-ornt IN FRAME FRAME-A
DO:
  {custom/chgfont.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rpt-codes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rpt-codes C-Win
ON LEAVE OF rpt-codes IN FRAME FRAME-A /* Reports */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME select-rpt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL select-rpt C-Win
ON VALUE-CHANGED OF select-rpt IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sub-acct-lvl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sub-acct-lvl C-Win
ON LEAVE OF sub-acct-lvl IN FRAME FRAME-A /* Sub Account Level */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-sub-lvl NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
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


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Transaction Date */
DO:
  if lastkey ne -1 then do:
    run check-date NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
DEF VAR lv-rpt-list AS CHAR NO-UNDO.

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

  TRAN-date = TODAY.

  RUN init-proc.

  RUN enable_UI.

  RUN check-date. 

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO v-dscr.
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN 
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date C-Win 
PROCEDURE check-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST period                   
        WHERE period.company EQ cocode
          AND period.pst     LE DATE(tran-date:SCREEN-VALUE)
          AND period.pend    GE DATE(tran-date:SCREEN-VALUE)
        NO-LOCK NO-ERROR.
    IF AVAIL period THEN tran-period:SCREEN-VALUE = STRING(period.pnum).

    ELSE DO:
      message "No Defined Period Exists for" DATE(tran-date:SCREEN-VALUE) VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO tran-date.
      RETURN ERROR.
    END.
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
  DISPLAY v-dscr tran-date select-rpt tran-period tb_pre tb_paid tb_supp 
          tb_acct# tb_round tb_mul-comp lv-company-list sub-acct-lvl 
          begin_sub-acct end_sub-acct lv-ornt lines-per-page rd-dest lv-font-no 
          lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-10 RECT-6 RECT-7 v-dscr tran-date select-rpt tb_pre tb_paid 
         tb_supp tb_acct# tb_round tb_mul-comp lv-company-list sub-acct-lvl 
         begin_sub-acct end_sub-acct lv-ornt lines-per-page rd-dest lv-font-no 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
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

DO WITH FRAME {&FRAME-NAME}:
  FOR EACH gl-rpt
      WHERE gl-rpt.company EQ cocode
        AND gl-rpt.line    EQ 0
      NO-LOCK:

    lv-rpt-list = lv-rpt-list + STRING(gl-rpt.rpt,"x(6)") + " " + gl-rpt.dscr + ",".

    IF TRIM(v-dscr) EQ "" THEN DO:
      FIND FIRST bf-rpt
          WHERE bf-rpt.company    EQ cocode
            AND bf-rpt.rpt        EQ gl-rpt.rpt
            AND bf-rpt.line       EQ 7
            AND TRIM(bf-rpt.dscr) NE ""
          NO-LOCK NO-ERROR.
      IF AVAIL bf-rpt THEN v-dscr = bf-rpt.dscr.
    END.
  END.
  IF SUBSTR(lv-rpt-list,LENGTH(TRIM(lv-rpt-list)),1) EQ "," THEN
    SUBSTR(lv-rpt-list,LENGTH(TRIM(lv-rpt-list)),1) = "".

  select-rpt:LIST-ITEMS = lv-rpt-list.

  FOR EACH company:
    company-list = company-list + company.company + ",".
  END.
  IF company-list NE "" THEN
    SUBSTR(company-list,LENGTH(TRIM(company-list)),1) = "".

  FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
  IF AVAIL company THEN DO:
    aclevel = company.acc-level.
    IF company-list EQ "" THEN company-list = company.company.
  END.  
  ELSE aclevel = 1.

  ASSIGN
   sub-acct-lvl    = aclevel
   lv-company-list = company-list.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-rpt C-Win 
PROCEDURE new-rpt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /*DO WITH FRAME {&FRAME-NAME}:
    FIND gl-rpt
        WHERE gl-rpt.company EQ cocode
          AND gl-rpt.rpt     BEGINS lv-rpt:SCREEN-VALUE 
          AND gl-rpt.line    EQ 7 
        NO-LOCK NO-ERROR.
    IF AVAIL gl-rpt THEN DO: 
      ASSIGN
       lv-rpt:SCREEN-VALUE = TRIM(CAPS(gl-rpt.rpt))
       v-dscr:SCREEN-VALUE = gl-rpt.dscr.

      DO li = 1 TO LENGTH(lv-rpt:SCREEN-VALUE):
        APPLY "cursor-right" TO lv-rpt.
      END.
    END.
  END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ouput-to-port C-Win 
PROCEDURE ouput-to-port :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN custom/d-print.w (list-name).

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
/*     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY.  */

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
 run custom/prntproc.p (list-name,INT(lv-font-no), lv-ornt). /* open file-name, title */ 
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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no), lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
DEF VAR lv-company-save AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-rcode AS CHAR FORMAT "x(47)".
DEF VAR lv-pct-hdr AS CHAR INIT "  % Sales" NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  DO li = 1 TO select-rpt:NUM-ITEMS:
    IF select-rpt:IS-SELECTED(li) THEN
      lv-rcode = lv-rcode + TRIM(SUBSTR(select-rpt:ENTRY(li),1,6)) + ",".
  END.

  IF LENGTH(lv-rcode) > 0 AND
    SUBSTR(lv-rcode,length(TRIM(lv-rcode)),1) EQ "," THEN
    SUBSTR(lv-rcode,length(TRIM(lv-rcode)),1) = "".

  rpt-codes = lv-rcode.

  DO li = 1 TO length(rpt-codes):
    IF SUBSTR(rpt-codes,li,1) EQ "," THEN SUBSTR(rpt-codes,li,1) = " ".
  END.

  lv-company-save = company-list.

  DO li = 1 TO LENGTH(lv-company-list):
    IF SUBSTR(lv-company-list,li,1) EQ "," THEN
      SUBSTR(lv-company-list,li,1) = " ".
  END.

  ASSIGN
   lv-company-list:SCREEN-VALUE = lv-company-list
   lv-company-list
   rpt-codes:SCREEN-VALUE = rpt-codes
   rpt-codes.
END.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

IF tb_excel THEN
  OUTPUT STREAM excel TO VALUE(fi_file).

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
   lv-company-list:SCREEN-VALUE = lv-company-save
   lv-company-list.
END. 

SESSION:SET-WAIT-STATE("general").

FOR EACH gl-rpt
    WHERE gl-rpt.company EQ cocode
      AND LOOKUP(gl-rpt.rpt,lv-rcode) GT 0
      AND gl-rpt.line    EQ 0
    NO-LOCK
    BY gl-rpt.rpt:

  DO TRANSACTION:
    FIND FIRST bf-rpt
        WHERE bf-rpt.company EQ cocode
          AND bf-rpt.rpt     EQ gl-rpt.rpt
          AND bf-rpt.line    EQ 7
        NO-ERROR.               
    IF AVAIL bf-rpt THEN
    DO:
       bf-rpt.dscr = v-dscr.
       FIND CURRENT bf-rpt NO-LOCK.
    END.

  END.

  ASSIGN
   v-d-wid = 0
   r-top1  = ""
   r-top2  = ""
   r-top3  = ""
   r-top4  = ""
   v-hdr   = "".

  fil_id = recid(gl-rpt).
  run gl/gl-rptg.p (input fil_id, input no).

  IF NOT all-per AND tb_round THEN DO:
    ASSIGN
     tot-format  = "->>>,>>>,>>9"
     pct-format  = "->>,>>>,>>9%"
     pct-formats = "->>>9%"
     sul-format  = " -----------"
     sul-formats = " -----"
     dul-format  = " ==========="
     dul-formats = " ====="
     lv-pct-hdr  = " % Sls".

    DO i = 1 TO v-no-col:
      v-ch[i] = SUBSTR(TRIM(v-ch[i]),1,11).
    END.
  END.

  if consolidate and index(company-list,",") gt 0 then
    v-hdr[5] = v-hdr[5] + (if v-hdr[5] eq "" then "" else " - ") +
               "Companies: " + trim(company-list).

  /* form headers */
  do i = 1 to 5:
    assign tot2[i] = length(v-hdr[i])
      tot2[i] = int(v-col-used - int(tot2[i])) / 2
      v-hdr[i] = fill(" ", int (tot2[i])) + v-hdr[i].
  end.

  r-top1 = fill(" ",v-d-wid).
  r-top2 = r-top1.
  do i = 1 to v-no-col:
    assign r-top3 = FILL(" ",((IF all-per THEN 10 ELSE IF tb_round THEN 11 ELSE 14) - LENGTH(v-ch[i]))) +
                    v-ch[i] +
                    IF all-per THEN " " ELSE (IF v-per[i] THEN lv-pct-hdr ELSE "")
      r-top1 = r-top1 + " " + r-top3
      r-top4 = r-top4 + dul-format + (IF v-per[i] THEN dul-formats ELSE "").
  end.
  r-top2 = r-top2 + r-top4.

  DISPLAY v-hdr r-top1 r-top2 WITH FRAME rpt-top.

  v-rpt = gl-rpt.rpt.

  IF tb_excel THEN DO:
    ASSIGN excelheader = ",".

    DO i = 1 TO v-no-col:
      excelheader = excelheader + v-ch[i] + ","
                  + (IF all-per THEN " " ELSE (IF v-per[i] THEN lv-pct-hdr + "," ELSE "")).
    END.

    excelheader = RIGHT-TRIM(excelheader,",").

    PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
  END.

  RUN gl/gl-fs.p(INPUT tb_excel).

  HIDE FRAME rpt-top.
  PUT SKIP(1).
  PAGE.
END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE("").

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
           ELSE IF lv-field-hdl:TYPE = "Fill-in" THEN 
               assign parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:help + "," 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-rpt C-Win 
PROCEDURE valid-rpt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /*FIND FIRST gl-rpt WHERE gl-rpt.company = cocode                      
                       AND gl-rpt.rpt = lv-rpt:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                       AND gl-rpt.LINE = 1
       NO-LOCK NO-ERROR.
   IF NOT AVAIL gl-rpt THEN DO:
      MESSAGE "Invalid Report Name, try help..." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR. 
   END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sub-lvl C-Win 
PROCEDURE valid-sub-lvl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF INT(sub-acct-lvl:SCREEN-VALUE) GT aclevel THEN DO:
      MESSAGE TRIM(sub-acct-lvl:LABEL) +
              " may not be greater than " +
              TRIM(STRING(aclevel,">>>>")) +
              "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO sub-acct-lvl.
      RETURN ERROR. 
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

