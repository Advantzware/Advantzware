&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: arrep\r-cashs2.w

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

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF STREAM excel.

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
   FIELD inv-no AS INT 
   FIELD chk-inv AS LOG INIT YES.
DEFINE TEMP-TABLE tt-report-inv NO-UNDO LIKE report 
    FIELD inv-no AS INT  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date end_date ~
begin_slsmn end_slsmn rd_sort rd_printname tb_tdisc tb_printaging ~
tb_printterms tb_prep days-old lines-per-page rd-dest lv-ornt lv-font-no ~
tb_show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_slsmn end_slsmn ~
lbl_sort rd_sort lbl_printname rd_printname lbl_days-old lbl_tdisc tb_tdisc ~
lbl_printaging tb_printaging lbl_printterms tb_printterms lbl_printinv ~
tb_printinv lbl_printcheckdt tb_printcheckdt lbl_printdisc tb_printdisc ~
lbl_printamtpd tb_printamtpd lbl_rembalance tb_rembalance lbl_incldprep ~
tb_prep days-old lines-per-page rd-dest lv-ornt lv-font-no lv-font-name ~
tb_show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Beginning Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning SalesRep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE days-old AS INTEGER FORMAT ">>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .95 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     LABEL "Ending Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending SalesRep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-cashs2.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_days-old AS CHARACTER FORMAT "X(256)":U INITIAL "Receipts After How Many Days" 
     VIEW-AS FILL-IN 
     SIZE 1 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_incldprep AS CHARACTER FORMAT "X(256)":U INITIAL "Include Prep Charges?" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_printaging AS CHARACTER FORMAT "X(256)":U INITIAL "Print Aging?" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_printamtpd AS CHARACTER FORMAT "X(256)":U INITIAL "Print Amt Paid by Cust?" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_printcheckdt AS CHARACTER FORMAT "X(256)":U INITIAL "Print Check Date?" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_printdisc AS CHARACTER FORMAT "X(256)":U INITIAL "Print Discount Amt?" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_printinv AS CHARACTER FORMAT "X(256)":U INITIAL "Print Invoice Amt?" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_printname AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_printterms AS CHARACTER FORMAT "X(256)":U INITIAL "Print Terms Code?" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_rembalance AS CHARACTER FORMAT "X(256)":U INITIAL "Print Amt Remaining?" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_tdisc AS CHARACTER FORMAT "X(256)":U INITIAL "Include Terms Discount?" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .95 NO-UNDO.

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
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_printname AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer Name", 1,
"ID", 2
     SIZE 33 BY .95 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Invoice#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer#", "Customer#",
"Invoice#", "Invoice#",
"Sales Rep", "SalesRep#"
     SIZE 44 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 11.67.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_prep AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE tb_printaging AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE tb_printamtpd AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE tb_printcheckdt AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE tb_printdisc AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE tb_printinv AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE tb_printterms AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE tb_rembalance AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tb_tdisc AS LOGICAL INITIAL yes 
     LABEL "Include Terms Discount?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_date AT ROW 2.67 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Receipt Date"
     end_date AT ROW 2.67 COL 71 COLON-ALIGNED HELP
          "Enter Ending Receipt Date"
     begin_slsmn AT ROW 3.62 COL 28 COLON-ALIGNED HELP
          "Enter Beginning SalesRep Number"
     end_slsmn AT ROW 3.62 COL 71 COLON-ALIGNED HELP
          "Enter Ending SalesRep Number"
     lbl_sort AT ROW 4.81 COL 28 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 4.81 COL 38 NO-LABEL
     lbl_printname AT ROW 5.76 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     rd_printname AT ROW 5.76 COL 38 NO-LABEL WIDGET-ID 4
     lbl_days-old AT ROW 6 COL 3 NO-LABEL
     lbl_tdisc AT ROW 6.71 COL 10 COLON-ALIGNED NO-LABEL
     tb_tdisc AT ROW 6.71 COL 40 RIGHT-ALIGNED
     lbl_printaging AT ROW 6.71 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     tb_printaging AT ROW 6.71 COL 68 WIDGET-ID 20
     lbl_printterms AT ROW 7.67 COL 12 NO-LABEL WIDGET-ID 10
     tb_printterms AT ROW 7.67 COL 38 WIDGET-ID 12
     lbl_printinv AT ROW 7.67 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     tb_printinv AT ROW 7.67 COL 68 WIDGET-ID 44
     lbl_printcheckdt AT ROW 8.62 COL 10 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     tb_printcheckdt AT ROW 8.62 COL 38 WIDGET-ID 16
     lbl_printdisc AT ROW 8.62 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     tb_printdisc AT ROW 8.62 COL 68 WIDGET-ID 40
     lbl_printamtpd AT ROW 9.57 COL 10 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     tb_printamtpd AT ROW 9.57 COL 38 WIDGET-ID 24
     lbl_rembalance AT ROW 9.57 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     tb_rembalance AT ROW 9.57 COL 68 WIDGET-ID 36
     lbl_incldprep AT ROW 10.52 COL 10 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     tb_prep AT ROW 10.52 COL 38 WIDGET-ID 48
     days-old AT ROW 11.52 COL 55 COLON-ALIGNED NO-LABEL
     lines-per-page AT ROW 13.38 COL 84 COLON-ALIGNED
     rd-dest AT ROW 14.24 COL 7 NO-LABEL
     lv-ornt AT ROW 14.95 COL 31 NO-LABEL
     lv-font-no AT ROW 16.38 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 17.33 COL 28 COLON-ALIGNED NO-LABEL
     tb_show-parm AT ROW 18.52 COL 30
     tb_excel AT ROW 19.71 COL 50 RIGHT-ALIGNED
     tb_runExcel AT ROW 19.71 COL 52
     fi_file AT ROW 20.67 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 22.19 COL 19
     btn-cancel AT ROW 22.19 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.29 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     "Show Only Invoices with Cash Receipts after" VIEW-AS TEXT
          SIZE 44 BY .62 AT ROW 11.76 COL 12
     "Days" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.76 COL 64
     RECT-6 AT ROW 12.91 COL 1
     RECT-7 AT ROW 1.24 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.4 BY 22.71.


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
         TITLE              = "Cash Receipts by SalesRep Name"
         HEIGHT             = 22.71
         WIDTH              = 94.4
         MAX-HEIGHT         = 22.86
         MAX-WIDTH          = 96.2
         VIRTUAL-HEIGHT     = 22.86
         VIRTUAL-WIDTH      = 96.2
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
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       days-old:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_days-old IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       lbl_days-old:PRIVATE-DATA IN FRAME FRAME-A     = 
                "days-old".

/* SETTINGS FOR FILL-IN lbl_incldprep IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_incldprep:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_printaging IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_printaging:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_printamtpd IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_printamtpd:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_printcheckdt IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_printcheckdt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_printdisc IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_printdisc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_printinv IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_printinv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_printname IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_printname:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_printterms IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       lbl_printterms:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_rembalance IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_rembalance:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lbl_tdisc IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_tdisc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "tb_tdisc".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_printname:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prep:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_printaging:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_printamtpd IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_printamtpd:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_printcheckdt IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_printcheckdt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_printdisc IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_printdisc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_printinv IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_printinv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_printterms:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_rembalance IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_rembalance:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_tdisc IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_tdisc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Cash Receipts by SalesRep Name */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Cash Receipts by SalesRep Name */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Receipt Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning SalesRep# */
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
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  run run-report.
  STATUS DEFAULT "Processing Complete".
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_slsmn
                            &END_cust=END_slsmn
                            &fax-subject="Cash Receipts By Sales Rep"
                            &fax-body="Cash Receipts By Sales Rep"
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_slsmn
                             &END_cust=end_slsmn
                             &mail-subject="Cash Receipts By Sales Rep"
                             &mail-body="Cash Receipts By Sales Rep"
                             &mail-file=lv-pdf-file + ".pdf" }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_slsmn
                                  &END_cust=end_slsmn
                                  &mail-subject="Cash Receipts By Sales Rep"
                                  &mail-body="Cash Receipts By Sales Rep"
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
  SESSION:SET-WAIT-STATE ("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME days-old
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL days-old C-Win
ON LEAVE OF days-old IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Receipt Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending SalesRep# */
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


&Scoped-define SELF-NAME rd_printname
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_printname C-Win
ON VALUE-CHANGED OF rd_printname IN FRAME FRAME-A
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
DO:
  assign {&self-name}.
  IF {&self-name}:SCREEN-VALUE = "SalesRep#" THEN 
      ASSIGN lv-ornt:SCREEN-VALUE = "L".
  ELSE
      ASSIGN lv-ornt:SCREEN-VALUE = "P".
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


&Scoped-define SELF-NAME tb_prep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prep C-Win
ON VALUE-CHANGED OF tb_prep IN FRAME FRAME-A
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_printaging
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_printaging C-Win
ON VALUE-CHANGED OF tb_printaging IN FRAME FRAME-A
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_printamtpd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_printamtpd C-Win
ON VALUE-CHANGED OF tb_printamtpd IN FRAME FRAME-A
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_printcheckdt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_printcheckdt C-Win
ON VALUE-CHANGED OF tb_printcheckdt IN FRAME FRAME-A
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_printdisc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_printdisc C-Win
ON VALUE-CHANGED OF tb_printdisc IN FRAME FRAME-A
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_printinv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_printinv C-Win
ON VALUE-CHANGED OF tb_printinv IN FRAME FRAME-A
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_printterms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_printterms C-Win
ON VALUE-CHANGED OF tb_printterms IN FRAME FRAME-A
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rembalance
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rembalance C-Win
ON VALUE-CHANGED OF tb_rembalance IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-parm C-Win
ON VALUE-CHANGED OF tb_show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_tdisc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tdisc C-Win
ON VALUE-CHANGED OF tb_tdisc IN FRAME FRAME-A /* Include Terms Discount? */
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

 begin_date = date(1,1,year(today)).

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_date.
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
  DISPLAY begin_date end_date begin_slsmn end_slsmn lbl_sort rd_sort 
          lbl_printname rd_printname lbl_days-old lbl_tdisc tb_tdisc 
          lbl_printaging tb_printaging lbl_printterms tb_printterms lbl_printinv 
          tb_printinv lbl_printcheckdt tb_printcheckdt lbl_printdisc 
          tb_printdisc lbl_printamtpd tb_printamtpd lbl_rembalance tb_rembalance 
          lbl_incldprep tb_prep days-old lines-per-page rd-dest lv-ornt 
          lv-font-no lv-font-name tb_show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_date end_date begin_slsmn end_slsmn rd_sort 
         rd_printname tb_tdisc tb_printaging tb_printterms tb_prep days-old 
         lines-per-page rd-dest lv-ornt lv-font-no tb_show-parm tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gt-excel-1 C-Win 
PROCEDURE gt-excel-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-paid AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-dsc AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-amt AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-rem AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-perc AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-com AS DEC NO-UNDO.

  PUT STREAM excel UNFORMATTED
      '"' ""                                         '",'
      '"' "Grand Totals:"                            '",'
      '"' ""                                         '",'
      '"' ""                                         '",'
      '"' ""                                         '",'
      '"' ""                                         '",'
      '"' ""                                         '",'
      '"' STRING(ip-amt,"->>,>>>,>>9.99")            '",' 
      '"' STRING(ip-paid,"->>,>>>,>>9.99")           '",'
      '"' STRING(ip-dsc,"->>,>>>,>>9.99")            '",'
      '"' (IF tb_rembalance THEN STRING(ip-rem,"->>,>>>,>>9.99") ELSE "") '",'
      '"' STRING(ip-perc,"->>9.99")                  '",'
      '"' STRING(ip-com,"->>,>>>,>>9.99")         '",'
      SKIP.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gt-excel-2 C-Win 
PROCEDURE gt-excel-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-paid AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-dsc AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-amt AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-rem AS DEC NO-UNDO.

  PUT STREAM excel UNFORMATTED
      '"' ""                                   '",'
      '"' "Grand Totals:"                      '",'
      '"' ""                                   '",'
      '"' ""                                   '",'
      '"' ""                                   '",'
      '"' ""                                   '",'
      '"' ""                                   '",'
      '"' ""                                   '",'
      '"' STRING(ip-amt,"->>,>>>,>>9.99")      '",'
      '"' STRING(ip-paid,"->>,>>>,>>9.99")     '",'
      '"' STRING(ip-dsc,"->>,>>>,>>9.99")      '",'
      '"' (IF tb_rembalance THEN STRING(ip-rem,"->>,>>>,>>9.99") ELSE "") '",'
      SKIP.
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
  IF rd_sort = "SalesRep#" THEN
     ASSIGN lv-ornt = "L".
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
  IF rd_sort = "SalesRep#" THEN
     ASSIGN lv-ornt = "L".
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
SESSION:SET-WAIT-STATE("general").
{sys/form/r-top3.f}

def var fdate    as   date format "99/99/9999" init "01/01/0001" no-undo.
def var tdate    like fdate init 12/31/9999 NO-UNDO.
def var fsman    as   char format "x(3)" no-undo.
def var tsman    like fsman init "zzz" NO-UNDO.
def var v-cust   as   log format "Customer/Invoice" init no no-undo.
def var v-disc   as   log format "Include/Exclude"  init yes no-undo.
def var v-days   as   int format ">>>>" init 0 no-undo.
DEF VAR sortby   AS CHAR INIT "Customer#" NO-UNDO.

def var v-sman as   CHAR NO-UNDO.
def var v-amt  like ar-cashl.amt-paid extent 2 NO-UNDO.
def var v-paid like v-amt NO-UNDO.
def var v-dsc  like v-amt NO-UNDO.
def var v-com  like ar-cashl.amt-paid NO-UNDO.
def var v-com-2 like v-com NO-UNDO.
def var v-c-%  as   DEC NO-UNDO.
def var v-tax  as   DEC NO-UNDO.
def var v-pct  as   DEC NO-UNDO.
DEF VAR v-misc AS   DEC NO-UNDO.
DEF VAR v-basis LIKE sman.commbasis INIT "" NO-UNDO.

def var v-tot-amt as   dec extent 4 NO-UNDO.
def var v-tot-rem as   dec extent 4 NO-UNDO.
def var v-tot-com like v-tot-amt NO-UNDO.
def var v-tot-dsc like v-tot-amt NO-UNDO.
def var v-tot-paid like v-tot-amt NO-UNDO.
DEF VAR vcustno AS CHAR NO-UNDO.
DEF VAR v-aging AS INTE NO-UNDO.
DEF VAR v-check-date AS DATE NO-UNDO.
DEF VAR v-inv-date AS DATE NO-UNDO.
DEF VAR v-disc-meth AS logi NO-UNDO INIT YES.
DEF VAR v-amt-full AS DECI NO-UNDO FORM "->>>,>>>,>>9.99".
DEF VAR v-inv-full AS DECI NO-UNDO FORM "->>>,>>>,>>9.99".
DEF VAR v-rem-bal AS DECI NO-UNDO FORM "->>>,>>>,>>9.99".
DEF VAR v-dsc2 AS DECI NO-UNDO.
DEF VAR v-tmp-amt-1 AS DEC NO-UNDO.
DEF VAR v-inv-found AS LOG NO-UNDO.

def var i as INT NO-UNDO.
def var j as INT NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

ASSIGN  
 fdate  = begin_date
 tdate  = end_date
 fsman  = begin_slsmn
 tsman  = end_slsmn
 v-cust = rd_sort eq "Customer#" OR rd_sort = "SalesRep#"
 sortby = rd_sort
 v-disc = tb_tdisc
 v-days = days-old.

assign
 str-tit  = coname + " - " + loname
 str-tit3 = "Receipt Date: " + string(fdate,"99/99/9999") + " - " +
                                 string(tdate,"99/99/9999") +
              fill(" ",4) + "Sales Rep: " + fsman + " - " + tsman
 x        = (80 - length(str-tit3)) / 2
 str-tit3 = fill(" ",x) + str-tit3 
 str-tit2 = c-win:TITLE /*"CASH RECEIPTS BY SALESREP"*/
 {sys/inc/ctrtext.i str-tit  56}
 {sys/inc/ctrtext.i str-tit2 56}
 {sys/inc/ctrtext.i str-tit3 80}.


{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).

  IF v-cust THEN
     excelheader = "Slsmn,Customer,Terms,InvDate,CheckDate,Aging,Invoice#,Inv Amount,Amt Paid,Discount,Bal Aft Pymt,Comm%,Comm".
  ELSE
     excelheader = "Slsmn,Name,Customer,Terms,InvDate,CheckDate,Aging,Invoice#,Inv Amount,Amt Paid,Discount,Bal Aft Pymt".

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if tb_show-parm then run show-param.

display str-tit with frame r-top STREAM-IO.

{sa/sa-sls01.i}

  for each cust where cust.company eq cocode no-lock:
      {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}
    if v-days eq 0 then
    for each ar-inv
        where ar-inv.company  eq cocode
          and ar-inv.posted   eq yes
          and ar-inv.cust-no  eq cust.cust-no
          and ar-inv.inv-date ge fdate
          and ar-inv.inv-date le tdate
          and ar-inv.terms    eq "CASH"
        no-lock,

        each ar-invl
        where ar-invl.x-no eq ar-inv.x-no
        no-lock

        transaction:

      do i = 1 to 3:
        v-sman = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                 else ar-invl.sman[i].

        if v-sman   lt fsman                         or
           v-sman   gt tsman                         or
           (i ne 1 and
            (v-sman eq "" or ar-invl.s-pct[i] eq 0)) then next.

        IF NOT CAN-FIND(first tt-report
            where tt-report.term-id eq v-term
              and tt-report.key-01  eq v-sman
              and tt-report.inv-no  eq ar-invl.inv-no
              and tt-report.rec-id  eq recid(ar-invl)) THEN do:
          create tt-report.
          assign
           tt-report.term-id = v-term
           tt-report.key-01  = v-sman
           tt-report.key-09  = cust.cust-no
           tt-report.key-08  = cust.terms
           tt-report.key-10  = cust.NAME
           tt-report.rec-id  = recid(ar-invl)
           tt-report.inv-no  = ar-invl.inv-no.

          IF v-cust THEN
             ASSIGN
                tt-report.key-02  = cust.cust-no
                tt-report.key-03  = string(ar-invl.inv-no,"9999999999").
          ELSE
             ASSIGN
                tt-report.key-02  = string(ar-invl.inv-no,"9999999999")
                tt-report.key-03  = cust.cust-no.

          RELEASE tt-report.
        end.
      end.
    end.      

    for each ar-cash
        where ar-cash.company    eq cocode
          and ar-cash.cust-no    eq cust.cust-no
          and ar-cash.check-date ge fdate
          and ar-cash.check-date le tdate
          and ar-cash.posted     eq yes
          and ar-cash.check-no   ne 0
        no-lock,

        each ar-cashl
        where ar-cashl.c-no   eq ar-cash.c-no
          and ar-cashl.posted eq yes
          and ar-cashl.memo   eq no
        no-lock

        transaction:

      IF NOT (v-days eq 0 or
         (ar-cash.check-date - ar-cashl.inv-date gt v-days and
          ar-cashl.inv-no ne 0)) THEN NEXT.

      if ar-cashl.inv-no ne 0 then
      for each ar-invl
          where ar-invl.company eq cocode
            and ar-invl.cust-no eq ar-cash.cust-no
            and ar-invl.inv-no  eq ar-cashl.inv-no
          no-lock:

         do i = 1 to 3:
            v-sman = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                     else ar-invl.sman[i].

            if v-sman  lt fsman                          or
               v-sman  gt tsman                          or
               (i ne 1 and
                (v-sman eq "" or ar-invl.s-pct[i] eq 0)) then next.

            IF NOT CAN-FIND(first tt-report
                where tt-report.term-id eq v-term
                  and tt-report.key-01  eq v-sman
                  and tt-report.inv-no  eq ar-invl.inv-no
                  and tt-report.rec-id  eq recid(ar-cashl)) then do:
               create tt-report.
               assign
                tt-report.term-id = v-term
                tt-report.key-01  = v-sman
                tt-report.key-09  = cust.cust-no
                tt-report.key-08  = cust.terms
                tt-report.key-10  = cust.NAME
                tt-report.rec-id  = recid(ar-cashl)
                tt-report.inv-no  = ar-invl.inv-no.

               IF v-cust THEN
                  ASSIGN
                     tt-report.key-02  = cust.cust-no
                     tt-report.key-03  = string(ar-invl.inv-no,"9999999999").
               ELSE
                  ASSIGN
                     tt-report.key-02  = string(ar-invl.inv-no,"9999999999")
                     tt-report.key-03  = cust.cust-no.

               RELEASE tt-report.
            end.
         end.
      end.

      else
      if cust.sman ge fsman and
         cust.sman le tsman then do:
         v-sman = cust.sman.

         IF NOT CAN-FIND(first tt-report
             where tt-report.term-id eq v-term
               and tt-report.key-01  eq v-sman
               and tt-report.inv-no  eq ar-cashl.inv-no
               and tt-report.rec-id  eq recid(ar-cashl)) then do:
            create tt-report.

            assign
             tt-report.term-id = v-term
             tt-report.key-01  = v-sman
             tt-report.key-09  = cust.cust-no
             tt-report.key-08  = cust.terms
             tt-report.key-10  = cust.NAME
             tt-report.rec-id  = recid(ar-cashl)
             tt-report.inv-no  = ar-cashl.inv-no.

            IF v-cust THEN
              ASSIGN
                 tt-report.key-02  = cust.cust-no
                 tt-report.key-03  = string(ar-cashl.inv-no,"9999999999").
            ELSE
              ASSIGN
                 tt-report.key-02  = string(ar-cashl.inv-no,"9999999999")
                 tt-report.key-03  = cust.cust-no.

            RELEASE tt-report.
         end.
      end.
    end.
  end.

  FOR EACH tt-report NO-LOCK:
    FIND FIRST tt-report-inv WHERE tt-report-inv.inv-no = tt-report.inv-no 
                                    AND  tt-report-inv.key-01 NE tt-report.key-01  NO-LOCK NO-ERROR.
      IF NOT AVAIL tt-report-inv THEN do:
      CREATE  tt-report-inv.
      ASSIGN
          tt-report-inv.key-01  =    tt-report.key-01
          tt-report-inv.key-02  =    tt-report.key-02
          tt-report-inv.key-03  =    tt-report.key-03
          tt-report-inv.inv-no  =    tt-report.inv-no  .

      END. 
      ELSE do:
          tt-report.chk-inv = FALSE .
      END.

  END.
  FOR EACH tt-report-inv NO-LOCK:
      DELETE tt-report-inv .
  END.

  for each tt-report where tt-report.term-id eq v-term,

      first cust
      where cust.company eq cocode
        and cust.cust-no eq tt-report.key-09
      no-lock

      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            BY tt-report.inv-no

      transaction:
      {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}
    find first sman
        where sman.company eq cocode
          and sman.sman    eq tt-report.key-01
        no-lock no-error.

    IF sortby = "Salesrep#" AND FIRST-of(tt-report.key-01) THEN
       PUT SKIP
           "Sales Rep: " + string(tt-report.key-01) + " - "  FORM "x(20)" (IF AVAIL sman THEN sman.sname ELSE "") FORM "x(30)" SKIP(1).

    release ar-inv.
    release ar-cash.

    ASSIGN v-check-date = ?
           v-inv-date = ?
           v-amt-full = 0
           v-inv-full = 0
           v-dsc2 = 0
           v-misc = 0.

    find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.    

    if avail ar-cashl then do:
       find first ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock.
       assign
          v-dsc[1] = if v-disc then ar-cashl.amt-disc else 0
          v-check-date = ar-cash.check-date
          v-amt-full = ar-cashl.amt-paid
          v-inv-found = NO.

       if ar-cashl.inv-no ne 0 then
       for each ar-invl WHERE
           ar-invl.company eq cocode AND
           ar-invl.cust-no eq ar-cash.cust-no AND
           ar-invl.inv-no  eq ar-cashl.inv-no
           no-lock,
           first ar-inv where
                 ar-inv.x-no eq ar-invl.x-no
                 no-lock
           break by ar-invl.inv-no:

           v-inv-found = YES.

           FIND FIRST itemfg
               WHERE itemfg.company EQ cocode
                 AND itemfg.i-no    EQ ar-invl.i-no
               NO-LOCK NO-ERROR.

           RUN custom/combasis.p (cocode, tt-report.key-01, cust.type,
                                  (IF AVAIL itemfg THEN itemfg.procat ELSE ""), 0,
                                  cust.cust-no,
                                  OUTPUT v-basis).

           if FIRST-OF(tt-report.inv-no) then
           DO:
              IF FIRST(ar-invl.inv-no) THEN
                 assign
                    v-amt    = 0
                    v-amt[1] = ar-inv.tax-amt +
                               (if ar-inv.f-bill then ar-inv.freight else 0)
                    v-inv-full = v-amt[1]
                    v-com    = 0
                    v-com-2  = 0
                    v-inv-date = ar-inv.inv-date.


              ASSIGN
                 v-inv-full = v-inv-full + ar-invl.amt.

              IF NOT tb_prep AND ar-invl.misc THEN v-misc = v-misc + ar-invl.amt.
              ELSE DO:
                ASSIGN
                      v-amt[1] = v-amt[1] + ar-invl.amt
                      v-tmp-amt-1 = v-amt[1].

                  if ar-invl.sman[1] ne "" then
                  do i = 1 to 3:
                     if tt-report.key-01 eq ar-invl.sman[i] then do:
                        ASSIGN
                           v-amt[2] = v-amt[2] + (ar-invl.amt * ar-invl.s-pct[i] / 100)
                           v-com    = v-com +
                                      (((ar-invl.amt - if v-basis EQ "G" then ar-invl.t-cost else 0) *
                                      ar-invl.s-pct[i] / 100) * ar-invl.s-comm[i] / 100).
                        leave.
                     end.
                  end.

                  else
                     assign
                        v-amt[2] = v-amt[2] + ar-invl.amt
                        v-com    = v-com +
                                   ((ar-invl.amt - if v-basis EQ "G" then ar-invl.t-cost else 0) *
                                   (if avail sman then (sman.scomm / 100) else 0)).

              END.
           end. /*end FIRST-OF(tt-report.inv-no)*/

       END. /*end each ar-invl*/

       IF v-inv-found = NO THEN
          ASSIGN
             v-amt[1] = ar-cashl.amt-paid + v-dsc[1] - v-misc
             v-tmp-amt-1 = v-amt[1]
             v-amt[2] = v-amt[1]
             v-com    = v-amt[1] * (if avail sman then (sman.scomm / 100) else 0).

       assign
          v-pct    = v-amt[2] / v-tmp-amt-1
          v-amt[1] = (ar-cashl.amt-paid + v-dsc[1] - v-misc) * v-pct    /* task 02261403 */
          v-pct    = v-amt[1] / v-amt[2]
          v-com-2  = v-com * v-pct.

       release ar-inv.
    end.

    else do:
      find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock.
      find first ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.

      FIND FIRST itemfg
            WHERE itemfg.company EQ cocode
              AND itemfg.i-no    EQ ar-invl.i-no
            NO-LOCK NO-ERROR.

      RUN custom/combasis.p (cocode, tt-report.key-01, cust.type,
                             (IF AVAIL itemfg THEN itemfg.procat ELSE ""), 0,
                             cust.cust-no,
                             OUTPUT v-basis).
     ASSIGN
         v-inv-full = ar-invl.amt .

      IF NOT tb_prep AND ar-invl.misc THEN NEXT.

    ASSIGN
       v-amt[1] = ar-invl.amt 
       v-tmp-amt-1 = v-amt[1]
       v-com    = (ar-invl.amt - if v-basis EQ "G" then ar-invl.t-cost else 0) *
                  (if avail sman then (sman.scomm / 100) else 0)
       v-com-2  = v-com.

    end.

    if v-com-2  eq ? then v-com-2 = 0.
    if v-amt[1] eq ? then
       ASSIGN v-amt[1] = 0
              v-tmp-amt-1 = 0.

    ASSIGN
       v-c-% = v-com-2 / v-amt[1] * 100
       v-amt[1] = v-tmp-amt-1. /*multiple payments against an invoice, reset v-amt[1] value*/

    if v-c-% eq ? then v-c-% = 0.

    v-paid[1] = v-amt-full.

    IF rd_printname = 1 THEN 
       ASSIGN vcustno = tt-report.key-10.
    ELSE
       ASSIGN vcustno = tt-report.key-09.

    v-aging = (IF v-check-date <> ? AND v-inv-date <> ? THEN v-check-date - v-inv-date ELSE 0).

    IF FIRST-OF(tt-report.inv-no) THEN
       v-rem-bal  = v-inv-full - v-amt-full - v-dsc[1].
    ELSE
       v-rem-bal  = v-rem-bal - v-amt-full - v-dsc[1].

    if v-cust THEN
    DO:
      display tt-report.key-01             format "x(3)"       label "Slsmn"
                when first-of(tt-report.key-01)
              vcustno  @ tt-report.key-09         format "x(20)"      label "Customer"
              tt-report.key-08 FORM "x(5)" WHEN tb_printterms         LABEL "Terms"
              v-inv-date                                    LABEL "Inv Date" FORM "99/99/99"
              v-check-date WHEN tb_printcheckdt             LABEL "Check Date" FORM "99/99/99"
              v-aging WHEN tb_printaging                    LABEL "Aging"
              ar-cashl.inv-no    when avail ar-cash         label "Invoice#"
              ar-inv.inv-no   when avail ar-inv   @ ar-cashl.inv-no
              v-inv-full WHEN FIRST-OF(tt-report.inv-no)    label "Inv Amount"
              v-amt-full                                    label "Amt Paid"
              v-dsc[1]                                      label "Discount"
              v-rem-bal WHEN tb_rembalance                  LABEL "Bal Aft Pymt"
              v-c-%  format "->>9.99"                       label "Comm%"
              v-com-2                                       label "Comm"
          with frame detail1 no-box no-attr-space stream-io down width 200.

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' (IF first-of(tt-report.key-01) THEN tt-report.key-01
                 ELSE "")                                         '",'
            '"' vcustno                                           '",'
            '"' (IF tb_printterms THEN tt-report.key-08 ELSE "")            '",'
            '"' (IF tb_printcheckdt AND v-inv-date NE ? THEN
                    STRING(v-inv-date,"99/99/99") ELSE "")        '",'
            '"' (IF tb_printcheckdt AND v-check-date NE ? THEN STRING(v-check-date,"99/99/99")
                 ELSE "")                                         '",'
            '"' (IF tb_printaging THEN string(v-aging) ELSE "")   '",'
            '"' (IF AVAIL ar-cash THEN STRING(ar-cashl.inv-no)
                 ELSE IF AVAIL ar-inv THEN STRING(ar-inv.inv-no)
                 ELSE "")                                         '",'
            '"' (IF first-of(tt-report.inv-no) THEN STRING(v-inv-full,"->>,>>>,>>9.99")
                 ELSE "")                                         '",'
            '"' STRING(v-amt-full,"->>,>>>,>>9.99")               '",'
            '"' STRING(v-dsc[1],"->>,>>>,>>9.99")                 '",'
            '"' (IF tb_rembalance THEN STRING(v-rem-bal,"->>,>>>,>>9.99")
                 ELSE "")                                         '",'
            '"'  STRING(v-c-%,"->>9.99")                          '",'
            '"'  STRING(v-com-2,"->>,>>>,>>9.99")                   '",'
           SKIP.
    END.
    else
    DO:
      display tt-report.key-01         format "x(3)"   label "Slsmn"
                when first-of(tt-report.key-01)
              sman.sname                            label "Name"
                when first-of(tt-report.key-01) and avail sman
              vcustno @ cust.NAME   format "x(20)"  label "Customer"
              tt-report.key-08 FORM "x(5)" WHEN tb_printterms LABEL "Terms"
              v-inv-date                            LABEL "Inv Date" FORM "99/99/99"
              v-check-date WHEN tb_printcheckdt     LABEL "Check Date" FORM "99/99/99"
              v-aging WHEN tb_printaging            LABEL "Aging"
              ar-cashl.inv-no    when avail ar-cash label "Invoice#"
              ar-inv.inv-no   when avail ar-inv   @ ar-cashl.inv-no
              v-inv-full WHEN first-of(tt-report.inv-no) label "Inv Amount"
              v-amt-full                            label "Amt Paid"
              v-dsc[1]                              label "Discount"
              v-rem-bal WHEN tb_rembalance          LABEL "Bal Aft Pymt"
          with frame detail2 no-box no-attr-space stream-io down width 200.

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' (IF first-of(tt-report.key-01) THEN tt-report.key-01
                 ELSE "")                                         '",'
            '"' (IF FIRST-OF(tt-report.key-01) AND AVAIL sman THEN
                    sman.sname ELSE "")                           '",'
            '"' vcustno                                           '",'
            '"' (IF tb_printterms THEN tt-report.key-08 ELSE "")            '",'
            '"' (IF v-inv-date NE ? THEN STRING(v-inv-date,"99/99/99")
                 ELSE "")                                         '",'
            '"' (IF tb_printcheckdt AND v-check-date NE ? THEN
                    STRING(v-check-date,"99/99/99") ELSE "")      '",'
            '"' (IF tb_printaging THEN string(v-aging) ELSE "")   '",'
            '"' (IF AVAIL ar-cash THEN STRING(ar-cashl.inv-no)
                 ELSE IF AVAIL ar-inv THEN STRING(ar-inv.inv-no)
                 ELSE "")                                         '",'
            '"' (IF first-of(tt-report.inv-no) THEN STRING(v-inv-full,"->>,>>>,>>9.99")
                 ELSE "")                                         '",'
            '"' STRING(v-amt-full,"->>,>>>,>>9.99")               '",'
            '"' STRING(v-dsc[1],"->>,>>>,>>9.99")                 '",'
            '"' (IF tb_rembalance THEN STRING(v-rem-bal,"->>,>>>,>>9.99")
                 ELSE "")                                         '",'
           SKIP.
    END.

    assign
     v-tot-paid[1] = v-tot-paid[1] + v-paid[1]
     v-tot-dsc[1] = v-tot-dsc[1] + v-dsc[1]
     v-tot-amt[1] = v-tot-amt[1] + v-inv-full
     v-tot-com[1] = v-tot-com[1] + v-com-2.

    IF LAST-OF(tt-report.inv-no) THEN
       v-tot-rem[1] = v-tot-rem[1] + v-rem-bal.

    if last-of(tt-report.key-02) then do:
      if v-cust then do:
        put skip(1).

        clear frame detail1 no-pause.
        clear frame detail2 no-pause.

        v-c-% = v-tot-com[1] / v-tot-amt[1] * 100.

        if v-c-% eq ? then v-c-% = 0.

        display "Customer Totals:" @ tt-report.key-09
                v-tot-paid[1]      @ v-amt-full
                v-tot-dsc[1]       @ v-dsc[1]
                v-tot-amt[1]       @ v-inv-full
                v-tot-rem[1] WHEN tb_rembalance  @ v-rem-bal
                v-c-%
                v-tot-com[1]       @ v-com-2
            with frame detail1.

        if not last-of(tt-report.key-01) then put skip(1).

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' ""                                         '",'
              '"' "Customer Totals:"                         '",'
              '"' ""                                         '",'
              '"' ""                                         '",'
              '"' ""                                         '",'
              '"' ""                                         '",'
              '"' ""                                         '",'
              '"' STRING(v-tot-amt[1],"->>,>>>,>>9.99")      '",'
              '"' STRING(v-tot-paid[1],"->>,>>>,>>9.99")     '",'
              '"' STRING(v-tot-dsc[1],"->>,>>>,>>9.99")      '",'
              '"' (IF tb_rembalance THEN
                      STRING(v-tot-rem[1],"->>,>>>,>>9.99") ELSE "") '",'
              '"' STRING(v-c-%,"->>9.99")                    '",'
              '"' STRING(v-tot-com[1],"->>,>>>,>>9.99")      '",'
             SKIP.
      end.

      assign
       v-tot-paid[2] = v-tot-paid[2] + v-tot-paid[1]
       v-tot-dsc[2] = v-tot-dsc[2] + v-tot-dsc[1]
       v-tot-amt[2] = v-tot-amt[2] + v-tot-amt[1]
       v-tot-rem[2] = v-tot-rem[2] + v-tot-rem[1]
       v-tot-com[2] = v-tot-com[2] + v-tot-com[1] .

      IF tt-report.chk-inv EQ TRUE THEN ASSIGN
          v-tot-paid[4] = v-tot-paid[4] + v-tot-paid[1].
      ASSIGN
          v-tot-dsc[4] = v-tot-dsc[4] + v-tot-dsc[1]
          v-tot-amt[4] = v-tot-amt[4] + v-tot-amt[1]
          v-tot-rem[4] = v-tot-rem[4] + v-tot-rem[1]
          v-tot-com[4] = v-tot-com[4] + v-tot-com[1] .


      ASSIGN
       v-tot-paid[1] = 0
       v-tot-dsc[1] = 0
       v-tot-amt[1] = 0
       v-tot-rem[1] = 0
       v-tot-com[1] = 0.

    end. /* last of key-02 */

    if last-of(tt-report.key-01) then do:
      put skip(1).

      clear frame detail1 no-pause.
      clear frame detail2 no-pause.

      v-c-% = v-tot-com[2] / v-tot-amt[2] * 100.

      if v-c-% eq ? then v-c-% = 0.

      if v-cust THEN
      DO:
        display "SalesRep Totals:" @ tt-report.key-09
                v-tot-paid[2]      @ v-amt-full
                v-tot-dsc[2]       @ v-dsc[1]
                v-tot-amt[2]       @ v-inv-full
                v-tot-rem[2] WHEN tb_rembalance  @ v-rem-bal
                v-c-%
                v-tot-com[2]       @ v-com-2
            with frame detail1.

        IF tb_excel THEN
          RUN sales-total-excel-1(v-tot-paid[2], v-tot-dsc[2], v-tot-amt[2], v-tot-rem[2], v-c-%, v-tot-com[2]).
      END.

      else
      DO:
        display "SalesRep Totals:" @ cust.NAME
                v-tot-paid[2]      @ v-amt-full
                v-tot-dsc[2]       @ v-dsc[1]
                v-tot-amt[2]       @ v-inv-full
                v-tot-rem[2] WHEN tb_rembalance  @ v-rem-bal
            with frame detail2.

        IF tb_excel THEN
          RUN sales-total-excel-2(v-tot-paid[2], v-tot-dsc[2], v-tot-amt[2], v-tot-rem[2]).
      END.

      put skip(2).

      assign
       v-tot-paid[3] = v-tot-paid[3] + v-tot-paid[4]
       v-tot-dsc[3] = v-tot-dsc[3] + v-tot-dsc[4]
       v-tot-amt[3] = v-tot-amt[3] + v-tot-amt[4]
       v-tot-rem[3] = v-tot-rem[3] + v-tot-rem[4]
       v-tot-com[3] = v-tot-com[3] + v-tot-com[4] .
    ASSIGN   
       v-tot-paid[2] = 0
       v-tot-dsc[2] = 0
       v-tot-amt[2] = 0
       v-tot-rem[2] = 0
       v-tot-com[2] = 0.

    ASSIGN   
       v-tot-paid[4] = 0
       v-tot-dsc[4] = 0
       v-tot-amt[4] = 0
       v-tot-rem[4] = 0
       v-tot-com[4] = 0.

       IF sortby = "Salesrep#" THEN 
          PAGE.                  
    end. /* last of key-01 */

    if last(tt-report.key-01) then do:
      clear frame detail1 no-pause.
      clear frame detail2 no-pause.

      v-c-% = v-tot-com[3] / v-tot-amt[3] * 100.

      if v-c-% eq ? then v-c-% = 0.

      if v-cust then
      DO:
        display "   Grand Totals:" @ tt-report.key-09
                v-tot-paid[3]      @ v-amt-full
                v-tot-dsc[3]       @ v-dsc[1]
                v-tot-amt[3]       @ v-inv-full
                v-tot-rem[3] WHEN tb_rembalance  @ v-rem-bal
                v-c-%
                v-tot-com[3]       @ v-com-2
            with frame detail1.

        IF tb_excel THEN
          RUN gt-excel-1(v-tot-paid[3], v-tot-dsc[3], v-tot-amt[3], v-tot-rem[3], v-c-%, v-tot-com[3]).

      END.
      else
      DO:
        display "   Grand Totals:" @ cust.NAME
                v-tot-paid[3]      @ v-amt-full
                v-tot-dsc[3]       @ v-dsc[1]
                v-tot-amt[3]       @ v-inv-full
                v-tot-rem[3] WHEN tb_rembalance @ v-rem-bal
            with frame detail2.

        IF tb_excel THEN
          RUN gt-excel-2(v-tot-paid[3], v-tot-dsc[3], v-tot-amt[3], v-tot-rem[3]).
      END.
    end.

    delete tt-report.
  end.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
     OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE("").

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sales-total-excel-1 C-Win 
PROCEDURE sales-total-excel-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-paid AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-dsc AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-amt AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-rem AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-perc AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-com AS DEC NO-UNDO.

  PUT STREAM excel UNFORMATTED
      '"' ""                                         '",'
      '"' "SalesRep Totals:"                         '",'
      '"' ""                                         '",'
      '"' ""                                         '",'
      '"' ""                                         '",'
      '"' ""                                         '",'
      '"' ""                                         '",'
      '"' STRING(ip-amt,"->>,>>>,>>9.99")      '",'
      '"' STRING(ip-paid,"->>,>>>,>>9.99")     '",'
      '"' STRING(ip-dsc,"->>,>>>,>>9.99")      '",'
      '"' (IF tb_rembalance THEN STRING(ip-rem,"->>,>>>,>>9.99") ELSE "") '",'  
      '"' STRING(ip-perc,"->>9.99")            '",'
      '"' STRING(ip-com,"->>,>>>,>>9.99")      '",'
      SKIP.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sales-total-excel-2 C-Win 
PROCEDURE sales-total-excel-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-paid AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-dsc AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-amt AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-rem AS DEC NO-UNDO.

  PUT STREAM excel UNFORMATTED
       '"' ""                                         '",'
       '"' "SalesRep Totals:"                         '",'
       '"' ""                                         '",'
       '"' ""                                         '",'
       '"' ""                                         '",'
       '"' ""                                         '",'
       '"' ""                                         '",'
       '"' ""                                         '",'
       '"' STRING(ip-amt,"->>,>>>,>>9.99")      '",'
       '"' STRING(ip-paid,"->>,>>>,>>9.99")     '",'
       '"' STRING(ip-dsc,"->>,>>>,>>9.99")      '",'
       '"' (IF tb_rembalance THEN STRING(ip-rem,"->>,>>>,>>9.99") ELSE "")  '",'
      SKIP.
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

