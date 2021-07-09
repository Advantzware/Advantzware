&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: gl\r-tribal.w

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
{gl\ttTrialBalance.i}

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

DEF VAR v-invalid AS LOG NO-UNDO.
def var v-download as log init no no-undo.
def var v-prior as log init no no-undo.

def buffer tmp-per for period.

def stream s-temp.
DEF VAR v-postable AS LOG NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEFINE VARIABLE dtDateRange1 AS DATE NO-UNDO.
DEFINE VARIABLE dtDateRange2 AS DATE NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-10 RECT-6 RECT-7 tb_show-all-account ~
tran-date begin_acct-no end_acct-no tb_sup-zero tb_show-detail ~
tb_include-summary-total tb_fill-field tb_Period-Detail tb_sub-acct ~
begin_sub-acct end_sub-acct rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_show-all-account lbl_show-all-account ~
tran-date tran-period begin_acct-no end_acct-no lbl_paid tb_sup-zero ~
lbl_show-detail tb_show-detail lbl_include-summery tb_include-summary-total ~
tb_fill-field lbl_fill-field lbl_Period-Details tb_Period-Detail lbl_paid-2 ~
tb_sub-acct v-sub-acct-lvl begin_sub-acct end_sub-acct rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel ~
fi_file 

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

DEFINE VARIABLE begin_acct-no AS CHARACTER FORMAT "x(25)" INITIAL "0" 
     LABEL "Beginning Acct#" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE begin_sub-acct AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Beginning Subacct" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE end_acct-no AS CHARACTER FORMAT "x(25)" INITIAL "zzzzzzzz" 
     LABEL "Ending Acct#" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE end_sub-acct AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     LABEL "Ending Subacct" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-tribal.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE lbl_fill-field AS CHARACTER FORMAT "X(256)":U INITIAL "Include the fill in fields:" 
     VIEW-AS FILL-IN 
     SIZE 23.6 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_include-summery AS CHARACTER FORMAT "X(256)":U INITIAL "Include Summary Total:" 
     VIEW-AS FILL-IN 
     SIZE 23.4 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_paid AS CHARACTER FORMAT "X(256)":U INITIAL "Suppress Zero Balances?" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_paid-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Sort by Sub Account Level?" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_Period-Details AS CHARACTER FORMAT "X(256)":U INITIAL "Period Details:" 
     VIEW-AS FILL-IN 
     SIZE 15.4 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_show-all-account AS CHARACTER FORMAT "X(256)":U INITIAL "Show All Account:" 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_show-detail AS CHARACTER FORMAT "X(256)":U INITIAL "Show Detail:" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

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

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-sub-acct-lvl AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Sub Account Level" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

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

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 4.14.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 8.81.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97 BY 13.57.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_fill-field AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE tb_include-summary-total AS LOGICAL INITIAL no 
     LABEL "Show Detail" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE tb_Period-Detail AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_show-all-account AS LOGICAL INITIAL no 
     LABEL "Show Detail" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE tb_show-detail AS LOGICAL INITIAL no 
     LABEL "Show Detail" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sub-acct AS LOGICAL INITIAL no 
     LABEL "Sort by Sub Account Level?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sup-zero AS LOGICAL INITIAL yes 
     LABEL "Suppress Zero Balance" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_show-all-account AT ROW 1.86 COL 81.8 WIDGET-ID 8
     lbl_show-all-account AT ROW 1.91 COL 61 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     tran-date AT ROW 1.95 COL 39 COLON-ALIGNED
     tran-period AT ROW 2.91 COL 39 COLON-ALIGNED
     begin_acct-no AT ROW 4.33 COL 39 COLON-ALIGNED HELP
          "Enter Beginning Account Number"
     end_acct-no AT ROW 5.29 COL 39 COLON-ALIGNED HELP
          "Enter Ending Account Number"
     lbl_paid AT ROW 6.48 COL 12 COLON-ALIGNED NO-LABEL
     tb_sup-zero AT ROW 6.48 COL 41
     lbl_show-detail AT ROW 7.48 COL 25.6 COLON-ALIGNED NO-LABEL
     tb_show-detail AT ROW 7.48 COL 41
     lbl_include-summery AT ROW 7.48 COL 53.6 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     tb_include-summary-total AT ROW 7.48 COL 79.2 WIDGET-ID 4
     tb_fill-field AT ROW 8.52 COL 41 WIDGET-ID 16
     lbl_fill-field AT ROW 8.57 COL 15.2 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     lbl_Period-Details AT ROW 8.57 COL 61.4 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     tb_Period-Detail AT ROW 8.57 COL 79.2 WIDGET-ID 12
     lbl_paid-2 AT ROW 10.67 COL 10 COLON-ALIGNED NO-LABEL
     tb_sub-acct AT ROW 10.67 COL 41
     v-sub-acct-lvl AT ROW 11.86 COL 39 COLON-ALIGNED
     begin_sub-acct AT ROW 13.05 COL 39 COLON-ALIGNED HELP
          "Enter Beginning Sub Account#"
     end_sub-acct AT ROW 13.05 COL 74 COLON-ALIGNED HELP
          "Enter Ending Sub Account#"
     rd-dest AT ROW 16.19 COL 5 NO-LABEL
     lv-ornt AT ROW 16.43 COL 30 NO-LABEL
     lines-per-page AT ROW 16.43 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 17.86 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 18.81 COL 27 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     td-show-parm AT ROW 21.19 COL 27
     tb_excel AT ROW 21.19 COL 71 RIGHT-ALIGNED
     tb_runExcel AT ROW 21.19 COL 95 RIGHT-ALIGNED
     fi_file AT ROW 22.29 COL 49 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 24.14 COL 18
     btn-cancel AT ROW 24.14 COL 55
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.24 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "SORT OPTIONS" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 9.95 COL 41
          FGCOLOR 9 FONT 6
     RECT-10 AT ROW 10.24 COL 5
     RECT-6 AT ROW 14.76 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 98.4 BY 24.76.


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
         TITLE              = "Trial Balance"
         HEIGHT             = 25.14
         WIDTH              = 99.8
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
       begin_acct-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_sub-acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_acct-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_sub-acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_fill-field IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_include-summery IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_paid IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_paid-2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_Period-Details IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_show-all-account IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_show-detail IN FRAME FRAME-A
   NO-ENABLE                                                            */
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
       tb_sub-acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sup-zero:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tran-period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN v-sub-acct-lvl IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Trial Balance */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Trial Balance */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_acct-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_acct-no C-Win
ON LEAVE OF begin_acct-no IN FRAME FRAME-A /* Beginning Acct# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_sub-acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_sub-acct C-Win
ON LEAVE OF begin_sub-acct IN FRAME FRAME-A /* Beginning Subacct */
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
  run check-date.
  if v-invalid then return no-apply.

  DO WITH FRAME frame-a:
     ASSIGN {&DISPLAYED-OBJECTS}.
  END.


  SESSION:SET-WAIT-STATE ("general").

  IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "southpak"
       THEN is-xprint-form = YES.     
  ELSE is-xprint-form = NO.
  
  IF tb_show-detail THEN
  RUN run-report-detail .
  ELSE 
  RUN run-report. 

  SESSION:SET-WAIT-STATE ("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= " "
                            &begin_cust= "begin_acct-no"
                            &END_cust= "begin_acct-no" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = " "
                             &begin_cust= "begin_acct-no"
                             &END_cust= "begin_acct-no"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = " "
                                  &begin_cust="begin_acct-no"
                                  &END_cust="begin_acct-no"
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


&Scoped-define SELF-NAME end_acct-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_acct-no C-Win
ON LEAVE OF end_acct-no IN FRAME FRAME-A /* Ending Acct# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_sub-acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_sub-acct C-Win
ON LEAVE OF end_sub-acct IN FRAME FRAME-A /* Ending Subacct */
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


&Scoped-define SELF-NAME tb_fill-field
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fill-field C-Win
ON VALUE-CHANGED OF tb_fill-field IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_include-summary-total
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_include-summary-total C-Win
ON VALUE-CHANGED OF tb_include-summary-total IN FRAME FRAME-A /* Show Detail */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_Period-Detail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_Period-Detail C-Win
ON VALUE-CHANGED OF tb_Period-Detail IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_show-all-account
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-all-account C-Win
ON VALUE-CHANGED OF tb_show-all-account IN FRAME FRAME-A /* Show Detail */
DO:
  assign {&self-name}.
  
  RUN pSetParameter.   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_show-detail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-detail C-Win
ON VALUE-CHANGED OF tb_show-detail IN FRAME FRAME-A /* Show Detail */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sub-acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sub-acct C-Win
ON VALUE-CHANGED OF tb_sub-acct IN FRAME FRAME-A /* Sort by Sub Account Level? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sup-zero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sup-zero C-Win
ON VALUE-CHANGED OF tb_sup-zero IN FRAME FRAME-A /* Suppress Zero Balance */
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


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Transaction Date */
DO:
  assign {&self-name}.

  if lastkey ne -1 then do:
    run check-date.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-sub-acct-lvl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-sub-acct-lvl C-Win
ON LEAVE OF v-sub-acct-lvl IN FRAME FRAME-A /* Sub Account Level */
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


  tran-date = TODAY.

  RUN init-proc.
  RUN enable_UI.
  {custom/usrprint.i}
  
  tran-date:SCREEN-VALUE = string(TODAY).

  RUN check-date.
  
  RUN pSetParameter.

  {methods/nowait.i}
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
 DO with frame {&frame-name}:
    v-invalid = no.

    find first period                   
        where period.company eq cocode
          and period.pst     le tran-date
          and period.pend    ge tran-date
        no-lock no-error.
    if avail period THEN DO: 
        tran-period:SCREEN-VALUE = string(period.pnum).
        dtDateRange1 = period.pst.
        dtDateRange2 = period.pend.
    END.

    ELSE DO:
      message "No Defined Period Exists for" tran-date view-as alert-box error.
      v-invalid = yes.
    end.
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
  DISPLAY tb_show-all-account lbl_show-all-account tran-date tran-period 
          begin_acct-no end_acct-no lbl_paid tb_sup-zero lbl_show-detail 
          tb_show-detail lbl_include-summery tb_include-summary-total 
          tb_fill-field lbl_fill-field lbl_Period-Details tb_Period-Detail 
          lbl_paid-2 tb_sub-acct v-sub-acct-lvl begin_sub-acct end_sub-acct 
          rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 
          tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-10 RECT-6 RECT-7 tb_show-all-account tran-date begin_acct-no 
         end_acct-no tb_sup-zero tb_show-detail tb_include-summary-total 
         tb_fill-field tb_Period-Detail tb_sub-acct begin_sub-acct end_sub-acct 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel 
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
find first sys-ctrl where
  sys-ctrl.company = cocode and
  sys-ctrl.name    = "TRIALBAL"
  no-lock no-error.
IF NOT(AVAILABLE(sys-ctrl)) then
DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
    sys-ctrl.company = cocode
    sys-ctrl.name    = "TRIALBAL"
    sys-ctrl.descrip = "Download Trial Balance to Excel?".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
v-download = sys-ctrl.log-fld.


find first company where company.company eq cocode no-lock.
v-sub-acct-lvl = company.acc-level.

find first tmp-per where tmp-per.company eq cocode
                     and tmp-per.pstat
                     and tmp-per.yr      eq (period.yr - 1)
                   no-lock no-error.
if avail tmp-per or not company.yend-per then
  assign v-prior = true.

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
 /*    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY. */

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
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/
/*
  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
  */                                  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCompanyAttributes C-Win 
PROCEDURE pGetCompanyAttributes PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsFYEnd AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcContra AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRet AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdtPeriodStart AS DATE NO-UNDO.
    
    FIND FIRST company NO-LOCK 
        WHERE company.company EQ ipcCompany
        NO-ERROR.
    IF AVAILABLE company THEN 
    DO:
        FIND LAST period NO-LOCK 
            WHERE period.company EQ ipcCompany
            AND period.pst     LE ipdtAsOf
            AND period.pend    GE ipdtAsOf
            NO-ERROR.
        IF AVAILABLE period THEN 
            ASSIGN
                opdtPeriodStart = period.pst
                .
            
        FIND FIRST gl-ctrl NO-LOCK
            WHERE gl-ctrl.company EQ company.company
            NO-ERROR.
        IF AVAILABLE gl-ctrl THEN 
            ASSIGN 
                opcContra = gl-ctrl.contra
                opcRet    = gl-ctrl.ret
                .
        FIND LAST period NO-LOCK 
            WHERE period.company EQ company.company 
            AND period.pnum EQ company.num-per  /* it's the last period of (a) year */ 
            AND period.pend EQ ipdtAsOf        /* it's the end date of the last period */
            NO-ERROR.
        ASSIGN 
            oplIsFYEnd = AVAILABLE period.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetParameter C-Win 
PROCEDURE pSetParameter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO with frame {&frame-name}:
    
    IF LOGICAL(tb_show-all-account:SCREEN-VALUE) EQ TRUE THEN
    DO:
      ASSIGN 
          begin_acct-no:SCREEN-VALUE = ""
          end_acct-no:SCREEN-VALUE  = "zzzzzzzzzzzzzzzzzzzzzzzzz".
           DISABLE begin_acct-no end_acct-no .
    END.
    ELSE
    ENABLE begin_acct-no end_acct-no.
   
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
def var save_id as RECID NO-UNDO.
def var time_stamp as ch NO-UNDO.
def var subac as int format ">>>>>>>>9" NO-UNDO.
def var subac-lvl as int format "9" NO-UNDO.
def var fsubac as int format ">>>>>>>>9" init 0 NO-UNDO.
def var tsubac as int format ">>>>>>>>9" init 999999999 NO-UNDO.
def var aclevel as INT NO-UNDO.
def var cyr as dec format "->>>,>>>,>>>,>>9.99" NO-UNDO.
def var tcyr as DEC NO-UNDO.
def var dadj as char label "DB Adjust" format "x(10)" init "__________" NO-UNDO.
def var cadj as char label "CR Adjust" format "x(10)" init "__________" NO-UNDO.
def var bsht as char label "Bal Sheet" format "x(10)" init "__________" NO-UNDO.
def var incs as char label "Income Stat" format "x(11)" init "___________" NO-UNDO.
def var v-rep-tot as dec no-undo.
def var vyear like period.yr no-undo.
def var dtPeriodStart like glhist.tr-date no-undo.
def var v-fisc-yr like period.yr no-undo.

def var tacct like glhist.actnum  label "      To Account Number" NO-UNDO.
def var facct like glhist.actnum  label "    From Account Number" NO-UNDO.
def var ptd-value as dec format "->>>,>>>,>>9.99" init 0 no-undo.
def var tot-ptd as dec format "->>>,>>>,>>9.99" init 0 no-undo.
def var suppress-zero as logical no-undo init true
    label "Suppress Zero Balances?".
def var break-flag as log init no no-undo.
def var start-lvl as int init 0 no-undo.
def var temp_fid as char no-undo.

def var str_buffa as char no-undo.
def var v-first as log init yes no-undo.
def var v-hdr as char initial
"Account#,Description,PTD,YTD,DB Adjust,CR Adjust,Bal Sheet,Income Stat" no-undo.
def var v-comma as char format "x" initial "," no-undo.
DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .
DEFINE VARIABLE cPeriodLabel AS CHARACTER NO-UNDO.
DEFINE VARIABLE cYtdLabel AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAsofDateLabel AS CHARACTER NO-UNDO.
DEFINE VARIABLE dAssetAmountPTD AS DECIMAL NO-UNDO.
DEFINE VARIABLE dAssetAmountYTD AS DECIMAL NO-UNDO. 
DEFINE VARIABLE dCapitalAmountPTD AS DECIMAL NO-UNDO.
DEFINE VARIABLE dCapitalAmountYTD AS DECIMAL NO-UNDO.
DEFINE VARIABLE dExpenseAmountPTD AS DECIMAL NO-UNDO.
DEFINE VARIABLE dExpenseAmountYTD AS DECIMAL NO-UNDO. 
DEFINE VARIABLE dLiabilityAmountPTD AS DECIMAL NO-UNDO.
DEFINE VARIABLE dLiabilityAmountYTD AS DECIMAL NO-UNDO. 
DEFINE VARIABLE dRevenueAmountPTD AS DECIMAL NO-UNDO.
DEFINE VARIABLE dRevenueAmountYTD AS DECIMAL NO-UNDO.  
DEFINE VARIABLE dTitleAmountPTD AS DECIMAL NO-UNDO.
DEFINE VARIABLE dTitleAmountYTD AS DECIMAL NO-UNDO. 
DEFINE VARIABLE cAccountType AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cFinancialReport AS CHARACTER NO-UNDO.
DEFINE VARIABLE str-tit4        AS CHARACTER FORMAT "x(240)" NO-UNDO.
DEFINE VARIABLE str-tit5        AS CHARACTER FORMAT "x(240)" NO-UNDO.
DEFINE VARIABLE dOpenBalance    AS DECIMAL format "->>>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dDebitActivity  AS DECIMAL format "->>>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dCreditActivity AS DECIMAL format "->>>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dEndingBalance  AS DECIMAL format "->>>>,>>>,>>9.99" NO-UNDO.

    DEFINE VARIABLE dTotPTD         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotYTD         AS DECIMAL   NO-UNDO.
    
RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
 
 {sys/inc/print1.i}
 {sys/inc/outprint.i VALUE(lines-per-page)}

 IF td-show-parm THEN RUN show-param.

 
 SESSION:SET-WAIT-STATE("general").
 
 cPeriodLabel = " Period " + string(MONTH(tran-date),"99") + " Total".
 cYtdLabel = "  " + string(YEAR(tran-date)) + " YTD Total".
 cAsofDateLabel = "As of Date: " + string(tran-date) .

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(cFileName).
   IF tb_Period-Detail THEN
   EXPORT STREAM excel DELIMITER ","
       "Account Number"
       "Description"
       cPeriodLabel
       cYtdLabel          
       "Account Type"
       "Financial Report"
       "Opening Balance"
       "Debit Activity"
       "Credit Activity"
       "Ending Balance"
       cAsofDateLabel
       SKIP.
   ELSE
   EXPORT STREAM excel DELIMITER ","
       "Account Number"
       "Description"
       cPeriodLabel
       cYtdLabel          
       "Account Type"
       "Financial Report"
       cAsofDateLabel
       SKIP.
END. 

/* create a unique filename ... */
temp_fid = 
IF OPSYS eq 'win32' THEN
"TryB" + substring(string(TODAY,"999999"),1,6) + ".csv"
  ELSE
"TryB" + substring(string(TODAY,"999999"),1,4) + ".csv".

time_stamp = string(TIME, "hh:mmam").

{sys/form/r-topw3.f}

ASSIGN facct = begin_acct-no
       tacct = END_acct-no
       subac-lvl = v-sub-acct-lvl
       fsubac = begin_sub-acct
       tsubac = END_sub-acct
       break-flag = tb_sub-acct
       suppress-zero = tb_sup-zero
       dTotYTD = 0
       dTotPTD = 0.
       
 FIND FIRST gl-ctrl NO-LOCK
      WHERE gl-ctrl.company EQ company.company
      NO-ERROR.       
       
EMPTY TEMP-TABLE ttTrialBalance.
RUN gl\TrialBalance.p(company.company, tran-date, facct, tacct, INPUT-OUTPUT TABLE ttTrialBalance).

blok:
DO:
   assign
     str-tit  = company.name
     str-tit2 = "TRIAL  BALANCE AS OF " + STRING(tran-date,"99/99/99")
     str-tit3 = "Period " + string(tran-period,"99") + " Date Range:" + STRING(dtDateRange1) + "-" + STRING(dtDateRange2)
     v-rep-tot = 0
     {sys/inc/ctrtext.i str-tit  112}
     {sys/inc/ctrtext.i str-tit2 112}
     {sys/inc/ctrtext.i str-tit3 132}.
     
     IF tb_fill-field AND tb_Period-Detail THEN
     ASSIGN
     str-tit4 = "Account Number           Description                     PTD                 YTD   Opening Balance   Debit Activity  Credit Activity   Ending Balance DB Adjust  CR Adjust  Bal Sheet  Income Stat "
     str-tit5 = "--------------------------------------------- --------------- ------------------- ---------------- ---------------- ---------------- ---------------- ---------- ---------- ---------- ----------- ".
     ELSE IF tb_fill-field THEN
     ASSIGN
     str-tit4 = "Account Number           Description                     PTD                 YTD  DB Adjust  CR Adjust  Bal Sheet  Income Stat "
     str-tit5 = "--------------------------------------------- --------------- ------------------- ---------- ---------- ---------- ----------- ".
     ELSE IF tb_Period-Detail THEN
     ASSIGN
     str-tit4 = "Account Number           Description                     PTD                 YTD   Opening Balance   Debit Activity  Credit Activity   Ending Balance"
     str-tit5 = "--------------------------------------------- --------------- ------------------- ---------------- ---------------- ---------------- ---------------- ".
     ELSE 
     ASSIGN
     str-tit4 = "Account Number           Description                     PTD                 YTD  "
     str-tit5 = "--------------------------------------------- --------------- ------------------- ".
     
     display str-tit3 format "x(130)" SKIP(1)     
     SKIP str-tit4 SKIP str-tit5 SKIP with frame r-top STREAM-IO.    
        
    if break-flag and subac-lvl ne 1 then do:
      start-lvl = 0.
      do i = 1 to (subac-lvl - 1):
        start-lvl = start-lvl + company.acc-dig[i].
      end.
      start-lvl = start-lvl + subac-lvl - 1.
    end.
    if start-lvl le 1 then start-lvl = 1.

    if v-download /*and v-first*/  then
    do:         
          assign str_buffa = ""
                 v-first = no.        
          output stream s-temp TO VALUE(temp_fid). 
          {gl/outstr.i v-hdr 1 70}.
          PUT STREAM s-temp UNFORMATTED str_buffa SKIP.
    end.
    
    FIND LAST period NO-LOCK 
            WHERE period.company EQ cocode
            AND period.pst     LE tran-date
            AND period.pend    GE tran-date
            NO-ERROR.
    
    FOR EACH ttTrialBalance, 
        FIRST account NO-LOCK 
        WHERE account.company EQ company.company
            AND account.actnum EQ ttTrialBalance.accountID        
        BY SUBSTRING(account.actnum,start-lvl) WITH WIDTH 132: 


        subac = IF subac-lvl EQ 1 THEN account.n1 ELSE
            IF subac-lvl EQ 2 THEN account.n2 ELSE
            IF subac-lvl EQ 3 THEN account.n3 ELSE
            IF subac-lvl EQ 4 THEN account.n4 ELSE account.n5.
        IF subac LT fsubac OR subac GT tsubac THEN NEXT.
        cAccountType = "" .
        cFinancialReport = "".
        
        IF account.TYPE EQ "A" THEN
        ASSIGN
        dAssetAmountPTD = dAssetAmountPTD + ttTrialBalance.amountPTD 
        dAssetAmountYTD = dAssetAmountYTD + ttTrialBalance.amountYTD
        cAccountType = "Asset"
        cFinancialReport = "Balance Sheet".
        ELSE IF account.TYPE EQ "C" THEN
        ASSIGN
        dCapitalAmountPTD = dCapitalAmountPTD + ttTrialBalance.amountPTD 
        dCapitalAmountYTD = dCapitalAmountYTD + ttTrialBalance.amountYTD
        cAccountType = "Capital"
        cFinancialReport = "Balance Sheet".
        ELSE IF account.TYPE EQ "E" AND account.actnum NE gl-ctrl.contra  THEN
        ASSIGN
        dExpenseAmountPTD = dExpenseAmountPTD + ttTrialBalance.amountPTD 
        dExpenseAmountYTD = dExpenseAmountYTD + ttTrialBalance.amountYTD
        cAccountType = "Expense"
        cFinancialReport = "Income statement".
        ELSE IF account.TYPE EQ "L" THEN
        ASSIGN
        dLiabilityAmountPTD = dLiabilityAmountPTD + ttTrialBalance.amountPTD 
        dLiabilityAmountYTD = dLiabilityAmountYTD + ttTrialBalance.amountYTD
        cAccountType = "Liability"
        cFinancialReport = "Balance Sheet".
        ELSE IF account.TYPE EQ "R" THEN
        ASSIGN
        dRevenueAmountPTD = dRevenueAmountPTD + ttTrialBalance.amountPTD 
        dRevenueAmountYTD = dRevenueAmountYTD + ttTrialBalance.amountYTD
        cAccountType = "Revenue"
        cFinancialReport = "Income statement".
        ELSE IF account.TYPE EQ "T" THEN
        ASSIGN
        dTitleAmountPTD = dTitleAmountPTD + ttTrialBalance.amountPTD 
        dTitleAmountYTD = dTitleAmountYTD + ttTrialBalance.amountYTD
        cAccountType = "Title".
        
        
        dTotPTD = dTotPTD + ttTrialBalance.amountPTD.
        dTotYTD = dTotYTD + ttTrialBalance.amountYTD.
        ASSIGN
        dOpenBalance    =  ttTrialBalance.amountYTDOpen
        dDebitActivity  =  0
        dCreditActivity =  0
        dEndingBalance  =  ttTrialBalance.amountYTDOpen .
       
        IF NOT suppress-zero OR ttTrialBalance.amountYTD NE 0 OR ttTrialBalance.amountPTD NE 0 THEN
        DO:
        
            FOR EACH glhist NO-LOCK 
               WHERE glhist.company EQ account.company
               AND glhist.actnum  EQ account.actnum
               AND glhist.tr-date GE period.pst 
               AND glhist.tr-date LE tran-date BY glhist.tr-date  :
            
                IF glhist.tr-amt GT 0 THEN
                dDebitActivity = dDebitActivity + glhist.tr-amt.
                ELSE dCreditActivity = dCreditActivity + glhist.tr-amt.
                dEndingBalance = dEndingBalance + glhist.tr-amt.
            END.    
           
           IF tb_fill-field AND tb_Period-Detail THEN
           PUT SKIP(1)
             account.actnum + "  " + account.dscr FORMAT "x(45)" SPACE(1)
             ttTrialBalance.amountPTD  FORMAT "->>>,>>>,>>9.99" SPACE(1)
             ttTrialBalance.amountYTD FORMAT "->>>,>>>,>>>,>>9.99" SPACE(1)
             dOpenBalance SPACE(1) dDebitActivity SPACE(1) dCreditActivity SPACE(1) dEndingBalance SPACE(1)
             dadj SPACE(1) cadj SPACE(1) bsht SPACE(1) incs SKIP .
           
           ELSE IF tb_fill-field THEN
           PUT SKIP(1)
             account.actnum + "  " + account.dscr FORMAT "x(45)" SPACE(1)
             ttTrialBalance.amountPTD  FORMAT "->>>,>>>,>>9.99" SPACE(1)
             ttTrialBalance.amountYTD FORMAT "->>>,>>>,>>>,>>9.99" SPACE(1)
             dadj SPACE(1) cadj SPACE(1) bsht SPACE(1) incs SKIP .
             
           ELSE IF tb_Period-Detail THEN
           PUT SKIP(1)
             account.actnum + "  " + account.dscr FORMAT "x(45)" SPACE(1)
             ttTrialBalance.amountPTD  FORMAT "->>>,>>>,>>9.99" SPACE(1)
             ttTrialBalance.amountYTD FORMAT "->>>,>>>,>>>,>>9.99" SPACE(1)
             dOpenBalance SPACE(1) dDebitActivity SPACE(1) dCreditActivity SPACE(1) dEndingBalance
             SKIP .
           ELSE
           PUT SKIP(1)
            account.actnum + "  " + account.dscr FORMAT "x(45)" SPACE(1)
            ttTrialBalance.amountPTD  FORMAT "->>>,>>>,>>9.99" SPACE(1)
            ttTrialBalance.amountYTD FORMAT "->>>,>>>,>>>,>>9.99"
             SKIP . 
             
            IF tb_excel THEN
            DO:
               IF tb_Period-Detail THEN
                EXPORT STREAM excel DELIMITER ","
                    account.actnum
                    account.dscr
                    ttTrialBalance.amountPTD    
                    ttTrialBalance.amountYTD
                    cAccountType 
                    cFinancialReport
                    dOpenBalance
                    dDebitActivity
                    dCreditActivity
                    dEndingBalance
                    SKIP.
               ELSE
                 EXPORT STREAM excel DELIMITER ","
                    account.actnum
                    account.dscr
                    ttTrialBalance.amountPTD    
                    ttTrialBalance.amountYTD
                    cAccountType 
                    cFinancialReport
                    SKIP.
            END.
            IF v-download THEN
            DO:
                ASSIGN 
                    str_buffa = "".
                ASSIGN 
                    str_buffa = TRIM(account.actnum) + v-comma 
                           + trim(account.dscr)   + v-comma
                           + trim(STRING(ttTrialBalance.amountPTD,'->>>>>>>>9.99')) + v-comma
                           + trim(STRING(ttTrialBalance.amountYTD,'->>>>>>>>9.99'))       + v-comma 
                           + v-comma + v-comma + v-comma.
                PUT STREAM s-temp UNFORMATTED str_buffa SKIP.
            END.
        END.  
            
    end. /* each account */

    put skip(1) "===============" to 61 "===================" to 81 skip
        "TRIAL BALANCE:" AT 10 dTotPTD format "->>>,>>>,>>9.99" to 61
                                  dTotYTD format "->>>,>>>,>>>,>>9.99" to 81.
        IF tb_fill-field THEN                          
        PUT
        " " dadj " " cadj " " bsht " " incs .
        PUT skip(1).
        
    IF tb_include-summary-total THEN
    DO: 
        PAGE.
        put SKIP "===============" to 61 "===================" to 81 skip
            "Total Assets:" AT 10 dAssetAmountPTD format "->>>,>>>,>>9.99" to 61
                                      dAssetAmountYTD format "->>>,>>>,>>>,>>9.99" to 81 SKIP.       
                                      
        put SKIP "===============" to 61 "===================" to 81 skip
            "Total Liabilities:" AT 10 dLiabilityAmountPTD format "->>>,>>>,>>9.99" to 61
                                       dLiabilityAmountYTD format "->>>,>>>,>>>,>>9.99" to 81 SKIP.  
                                       
        put SKIP "===============" to 61 "===================" to 81 skip
            "Total Capital:" AT 10 dCapitalAmountPTD format "->>>,>>>,>>9.99" to 61
                                       dCapitalAmountYTD format "->>>,>>>,>>>,>>9.99" to 81 SKIP.        
                                
        put SKIP "===============" to 61 "===================" to 81 skip
            "Balance Sheet Total:" AT 10 (dAssetAmountPTD + dLiabilityAmountPTD + dCapitalAmountPTD ) format "->>>,>>>,>>9.99" to 61
                                         (dAssetAmountYTD + dLiabilityAmountYTD + dCapitalAmountYTD ) format "->>>,>>>,>>>,>>9.99" to 81 SKIP(1).  
                                         
        put SKIP "===============" to 61 "===================" to 81 skip
            "Total Revenue:" AT 10 dRevenueAmountPTD format "->>>,>>>,>>9.99" to 61
                                       dRevenueAmountYTD format "->>>,>>>,>>>,>>9.99" to 81 SKIP. 
                                       
        put SKIP "===============" to 61 "===================" to 81 skip
            "Total Expenses:" AT 10 dExpenseAmountPTD format "->>>,>>>,>>9.99" to 61
                                       dExpenseAmountYTD format "->>>,>>>,>>>,>>9.99" to 81 SKIP.  
                                       
        put SKIP "===============" to 61 "===================" to 81 skip
            "Net Income:" AT 10 (-1 * dRevenueAmountPTD - dExpenseAmountPTD) format "->>>,>>>,>>9.99" to 61
                                (-1 * dRevenueAmountYTD - dExpenseAmountYTD) format "->>>,>>>,>>>,>>9.99" to 81 SKIP.                                                                  
    END.                                
                                   
   IF tb_excel THEN 
   DO: 
      IF tb_include-summary-total THEN
      DO: 
        EXPORT STREAM excel DELIMITER ","
            "Total Assets:"
            ""
            dAssetAmountPTD
            dAssetAmountYTD SKIP .
            EXPORT STREAM excel DELIMITER ","
             "Total Liabilities:"
             ""
            dLiabilityAmountPTD
            dLiabilityAmountYTD SKIP.
            EXPORT STREAM excel DELIMITER ","
            "Total Capital:"
            ""
            dCapitalAmountPTD
            dCapitalAmountYTD SKIP.
            
            EXPORT STREAM excel DELIMITER ","
            "Balance Sheet Total:"
            ""
            (dAssetAmountPTD + dLiabilityAmountPTD + dCapitalAmountPTD  )
            (dAssetAmountYTD + dLiabilityAmountYTD + dCapitalAmountYTD  ) SKIP(1)  .
            EXPORT STREAM excel DELIMITER ","
            "Total Revenue:"
            ""
            dRevenueAmountPTD
            dRevenueAmountYTD SKIP.
            EXPORT STREAM excel DELIMITER ","
            "Total Expenses:"
            ""
            dExpenseAmountPTD
            dExpenseAmountYTD                     
            SKIP.
            EXPORT STREAM excel DELIMITER ","
            "Net Income:"
            ""
            (-1 * dRevenueAmountPTD - dExpenseAmountPTD)
            (-1 * dRevenueAmountYTD - dExpenseAmountYTD) SKIP.
      END.
   END.   
   
    IF tb_show-all-account EQ TRUE  THEN
    DO:   
        if dTotYTD eq 0 then message "TRIAL BALANCE IN BALANCE" VIEW-AS ALERT-BOX.
        else              message "TRIAL BALANCE NOT IN BALANCE BY " dTotYTD VIEW-AS ALERT-BOX.
    END.
    /*if v-download then  */
       output stream s-temp close. 
 end.

  IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
 END.
 RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
SESSION:SET-WAIT-STATE("").

 end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-detail C-Win 
PROCEDURE run-report-detail :
def var save_id as RECID NO-UNDO.
def var time_stamp as ch NO-UNDO.
def var subac as int format ">>>>>>>>9" NO-UNDO.
def var subac-lvl as int format "9" NO-UNDO.
def var fsubac as int format ">>>>>>>>9" init 0 NO-UNDO.
def var tsubac as int format ">>>>>>>>9" init 999999999 NO-UNDO.
def var aclevel as INT NO-UNDO.
def var cyr as dec format "->>>,>>>,>>>,>>9.99" NO-UNDO.
def var tcyr as DEC NO-UNDO.
def var dadj as char label "DB Adjust" format "x(10)" init "__________" NO-UNDO.
def var cadj as char label "CR Adjust" format "x(10)" init "__________" NO-UNDO.
def var bsht as char label "Bal Sheet" format "x(10)" init "__________" NO-UNDO.
def var incs as char label "Income Stat" format "x(11)" init "___________" NO-UNDO.
def var v-rep-tot as dec no-undo.
def var vyear like period.yr no-undo.
def var dtPeriodStart like glhist.tr-date no-undo.
def var v-fisc-yr like period.yr no-undo.

def var tacct like glhist.actnum  label "      To Account Number" NO-UNDO.
def var facct like glhist.actnum  label "    From Account Number" NO-UNDO.
def var ptd-value as dec format "->>>,>>>,>>9.99" init 0 no-undo.
def var tot-ptd as dec format "->>>,>>>,>>9.99" init 0 no-undo.
def var suppress-zero as logical no-undo init true
    label "Suppress Zero Balances?".
def var break-flag as log init no no-undo.
def var start-lvl as int init 0 no-undo.
def var temp_fid as char no-undo.

def var str_buffa as char no-undo.
def var v-first as log init yes no-undo.
def var v-hdr as char initial
"Account#,Description,PTD,YTD,DB Adjust,CR Adjust,Bal Sheet,Income Stat" no-undo.
def var v-comma as char format "x" initial "," no-undo.
DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .
DEFINE VARIABLE cPeriodLabel AS CHARACTER NO-UNDO.
DEFINE VARIABLE cYtdLabel AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAsofDateLabel AS CHARACTER NO-UNDO.
DEFINE VARIABLE dDebitAmt AS DECIMAL NO-UNDO.
DEFINE VARIABLE dCreditAmt AS DECIMAL NO-UNDO.  
DEFINE VARIABLE dPeriodTotal AS DECIMAL NO-UNDO.
DEFINE VARIABLE dYtdAmount AS DECIMAL NO-UNDO.
DEFINE VARIABLE dTotalOpenBalance AS DECIMAL NO-UNDO.
DEFINE VARIABLE dTotalYtdAmount AS DECIMAL NO-UNDO.
DEFINE VARIABLE dTotYTD         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lRecordExist    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE str-tit4        AS CHARACTER FORMAT "x(240)" NO-UNDO.
DEFINE VARIABLE str-tit5        AS CHARACTER FORMAT "x(240)" NO-UNDO.
DEFINE VARIABLE str-line        AS CHARACTER FORMAT "x(240)" NO-UNDO.
DEFINE VARIABLE dTotalDebitAmt  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dTotalCreditAmt AS DECIMAL   NO-UNDO.
       
RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

 {sys/inc/print1.i}
 {sys/inc/outprint.i VALUE(lines-per-page)}

 IF td-show-parm THEN RUN show-param.

 SESSION:SET-WAIT-STATE("general").
 
 cPeriodLabel = " Period " + string(MONTH(tran-date),"99") + " Total".
 cYtdLabel = "  " + string(YEAR(tran-date)) + " YTD Total".
 cAsofDateLabel = "As of Date: " + string(tran-date) .
 str-line = FILL(" ",45) + "  " + "---------------" + " " + FILL(" ",51) + "--------------" + " " + "--------------" + " " + "--------------" + FILL(" ",57) + "-----------------" .                                                 

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(cFileName).
   EXPORT STREAM excel DELIMITER ","
       "Account Number"
       "Description"
       "Opening Balance"
       "Date"
       "Ref#"
       "Description"
       "Debit Amt"
       "Credit Amt"
       cPeriodLabel
       "Document id"
       "Source Date"
       "Run Number"
       cYtdLabel                
       SKIP.
END.      

/* create a unique filename ... */
temp_fid = 
IF OPSYS eq 'win32' THEN
"TryB" + substring(string(TODAY,"999999"),1,6) + ".csv"
  ELSE
"TryB" + substring(string(TODAY,"999999"),1,4) + ".csv".

time_stamp = string(TIME, "hh:mmam").

{sys/form/r-topw3.f}

ASSIGN facct = begin_acct-no
       tacct = END_acct-no
       subac-lvl = v-sub-acct-lvl
       fsubac = begin_sub-acct
       tsubac = END_sub-acct
       break-flag = tb_sub-acct
       suppress-zero = tb_sup-zero
       dTotYTD = 0
       .
       
EMPTY TEMP-TABLE ttTrialBalance.
RUN gl\TrialBalance.p(company.company, tran-date, facct, tacct, INPUT-OUTPUT TABLE ttTrialBalance).

blok:
DO:
   assign
     str-tit  = company.name
     str-tit2 = "TRIAL  BALANCE AS OF " + STRING(tran-date,"99/99/99")
     str-tit3 = "Period " + string(tran-period,"99") + " Date Range:" + STRING(dtDateRange1) + "-" + STRING(dtDateRange2)
     v-rep-tot = 0
     {sys/inc/ctrtext.i str-tit  112}
     {sys/inc/ctrtext.i str-tit2 112}
     {sys/inc/ctrtext.i str-tit3 132}.
         
    str-tit4 = "Account Number           Description          Opening Balance  Date       Ref      Description                    Debit Amt      Credit Amt     PTD Total      Document ID                      Source Date Run Number YTD".
    str-tit5 = "--------------------------------------------- ---------------- ---------- -------- ------------------------------ -------------- -------------- -------------- -------------------------------- ----------- ---------- -----------------".
    display str-tit3 format "x(130)" SKIP(1)     
     SKIP str-tit4 SKIP str-tit5 SKIP with frame r-top STREAM-IO.

    if break-flag and subac-lvl ne 1 then do:
      start-lvl = 0.
      do i = 1 to (subac-lvl - 1):
        start-lvl = start-lvl + company.acc-dig[i].
      end.
      start-lvl = start-lvl + subac-lvl - 1.
    end.
    if start-lvl le 1 then start-lvl = 1.

    if v-download /*and v-first*/  then
    do:         
          assign str_buffa = ""
                 v-first = no.        
          output stream s-temp TO VALUE(temp_fid). 
          {gl/outstr.i v-hdr 1 70}.
          PUT STREAM s-temp UNFORMATTED str_buffa SKIP.
    end.
    
    FIND LAST period NO-LOCK 
            WHERE period.company EQ cocode
            AND period.pst     LE tran-date
            AND period.pend    GE tran-date
            NO-ERROR.
    
    FOR EACH ttTrialBalance, 
        FIRST account NO-LOCK 
        WHERE account.company EQ company.company
            AND account.actnum EQ ttTrialBalance.accountID        
        BY SUBSTRING(account.actnum,start-lvl) WITH WIDTH 132: 


        subac = IF subac-lvl EQ 1 THEN account.n1 ELSE
            IF subac-lvl EQ 2 THEN account.n2 ELSE
            IF subac-lvl EQ 3 THEN account.n3 ELSE
            IF subac-lvl EQ 4 THEN account.n4 ELSE account.n5.
        IF subac LT fsubac OR subac GT tsubac THEN NEXT.
                        
        dTotYTD = dTotYTD + ttTrialBalance.amountYTD.
        dTotalDebitAmt = 0 .
        dTotalCreditAmt = 0.
        
        IF NOT suppress-zero OR ttTrialBalance.amountYTD NE 0 OR ttTrialBalance.amountPTD NE 0 THEN
        DO:
           dTotalYtdAmount = dTotalYtdAmount + ttTrialBalance.amountYTDOpen .
           dYtdAmount = ttTrialBalance.amountYTDOpen.
           dTotalOpenBalance = dTotalOpenBalance + ttTrialBalance.amountYTDOpen.
           lRecordExist = NO.
           FOR EACH glhist NO-LOCK 
               WHERE glhist.company EQ account.company
               AND glhist.actnum  EQ account.actnum
               AND glhist.tr-date GE period.pst 
               AND glhist.tr-date LE tran-date BY glhist.tr-date  :
            dDebitAmt = 0.
            dCreditAmt = 0.            
            IF glhist.tr-amt GT 0 THEN
            ASSIGN
               dDebitAmt = glhist.tr-amt
               dTotalDebitAmt = dTotalDebitAmt + glhist.tr-amt .
            ELSE
            assign
            dCreditAmt = glhist.tr-amt
             dTotalCreditAmt = dTotalCreditAmt + glhist.tr-amt .
            
            dPeriodTotal = dPeriodTotal + glhist.tr-amt. 
            dYtdAmount = dYtdAmount + glhist.tr-amt.
            dTotalYtdAmount = dTotalYtdAmount + glhist.tr-amt.
            lRecordExist = YES.
            
            PUT 
                account.actnum + "  " + account.dscr FORMAT "x(45)" SPACE(2)                
                 SPACE(16) 
                glhist.tr-date FORMAT "99/99/9999" SPACE(1)
                glhist.jrnl FORMAT "x(8)" SPACE(1)
                glhist.tr-dscr FORMAT "x(30)" SPACE(1)
                dDebitAmt FORMAT "->>,>>>,>>9.99" SPACE(1)
                dCreditAmt FORMAT "->>,>>>,>>9.99" SPACE(1)
                dPeriodTotal FORMAT "->>,>>>,>>9.99"  SPACE(1)
                glhist.documentID FORMAT "x(32)" SPACE(1)
                glhist.sourceDate FORMAT "99/99/9999" SPACE(2)
                glhist.tr-num FORMAT ">>>>>>>>>>" SPACE(1)
                dYtdAmount FORMAT "->,>>>,>>>,>>9.99" SPACE(1)
               SKIP.                
                          
            IF tb_excel THEN 
                EXPORT STREAM excel DELIMITER ","
                    account.actnum
                    account.dscr
                    ttTrialBalance.amountYTDOpen    
                    glhist.tr-date
                    glhist.jrnl
                    glhist.tr-dscr
                    dDebitAmt
                    dCreditAmt
                    dPeriodTotal
                    glhist.documentID
                    glhist.sourceDate
                    glhist.tr-num
                    dYtdAmount
                    dYtdAmount
                    SKIP.                     
           END.           
           
           IF lRecordExist THEN
           DO:
                 PUT SKIP str-line FORMAT "x(240)" SKIP.
                 PUT  "                              Account Total:" FORMAT "x(45)" SPACE(2)
                 ttTrialBalance.amountYTDOpen  FORMAT "->>>,>>>,>>9.99" SPACE(1)
                 SPACE(11) 
                 space(9) 
                 SPACE(31) 
                 dTotalDebitAmt FORMAT "->>,>>>,>>9.99" SPACE(1)
                 dTotalCreditAmt FORMAT "->>,>>>,>>9.99" SPACE(1)
                 dPeriodTotal FORMAT "->>,>>>,>>9.99"  SPACE(1)
                 SPACE(33) 
                 SPACE(12)
                 SPACE(11) 
                 dYtdAmount FORMAT "->,>>>,>>>,>>9.99" SPACE(1)
                 SKIP(1).
                 
                 IF tb_excel THEN 
                 EXPORT STREAM excel DELIMITER ","
                    "Account Total"
                    ""
                    ttTrialBalance.amountYTDOpen
                    ""
                    ""
                    ""
                    dTotalDebitAmt
                    dTotalCreditAmt
                    dPeriodTotal
                    ""
                    ""
                    ""  
                    dYtdAmount
                    SKIP(1).    
           END.
           
           IF NOT lRecordExist THEN
           DO:  
                
                PUT SKIP
                account.actnum + "  " + account.dscr FORMAT "x(45)" SPACE(2)
                ttTrialBalance.amountYTDOpen FORMAT "->>>,>>>,>>9.99" SPACE(1)
                "" FORMAT "x(152)" 
                dYtdAmount FORMAT "->,>>>,>>>,>>9.99" SKIP .
                PUT str-line FORMAT "x(240)" SKIP.
                PUT  "                              Account Total:" FORMAT "x(45)" SPACE(2)
                 ttTrialBalance.amountYTDOpen  FORMAT "->>>,>>>,>>9.99"
                 "" FORMAT "x(153)" 
                 dYtdAmount FORMAT "->,>>>,>>>,>>9.99" SKIP(1).
                
                          
            IF tb_excel THEN 
                EXPORT STREAM excel DELIMITER ","
                    account.actnum
                    account.dscr
                    ttTrialBalance.amountYTDOpen
                    ""
                    ""
                    ""
                    ""
                    ""
                    ""
                    ""
                    ""
                    ""
                    dYtdAmount
                    SKIP(1). 
                    
            IF tb_excel THEN 
                EXPORT STREAM excel DELIMITER ","
                    "Account Total"
                    ""
                    ttTrialBalance.amountYTDOpen
                    ""
                    ""
                    ""
                    ""
                    ""
                    ""
                    ""
                    ""
                    ""
                    dYtdAmount
                    SKIP(1).                
           END.

            IF v-download THEN
            DO:
                ASSIGN 
                    str_buffa = "".
                ASSIGN 
                    str_buffa = TRIM(account.actnum) + v-comma 
                           + trim(account.dscr)   + v-comma
                           + trim(STRING(ttTrialBalance.amountPTD,'->>>>>>>>9.99')) + v-comma
                           + trim(STRING(ttTrialBalance.amountYTD,'->>>>>>>>9.99'))       + v-comma 
                           + v-comma + v-comma + v-comma.
                PUT STREAM s-temp UNFORMATTED str_buffa SKIP.
            END.
        END.  
            
    end. /* each account */

    put skip(1) "===================" to 61  skip
        "Opening Balance Total:" AT 10 dTotalOpenBalance format "->>>,>>>,>>>,>>9.99" to 61 SKIP
         "====================" to 61 SKIP
         "YTD Balance Total:" AT 10  dTotalYtdAmount format "->>>,>>>,>>>,>>9.99" TO 61
         skip(1).       
                                   
   IF tb_excel THEN 
   DO:          
        EXPORT STREAM excel DELIMITER ","
            SKIP
            "Opening Balance Total:"
            ""
            dTotalOpenBalance SKIP .
            EXPORT STREAM excel DELIMITER ","
             SKIP
            "Ytd Balance Total:"
            ""
            dTotalYtdAmount SKIP .                     
                                                     
       END.             
    IF tb_show-all-account EQ TRUE THEN
    DO:   
        if dTotYTD eq 0 then message "TRIAL BALANCE IN BALANCE" VIEW-AS ALERT-BOX.
        else              message "TRIAL BALANCE NOT IN BALANCE BY " dTotYTD VIEW-AS ALERT-BOX.
    END.     
       output stream s-temp close. 
 end.

  IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
 END.
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

