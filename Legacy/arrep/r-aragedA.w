&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: arrep\r-araged.w

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
DEF VAR list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.

/*{sys/inc/custlistform.i ""AR5"" }*/

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

DEF VAR lv-default-comp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO INIT 0.

FOR EACH usercomp WHERE usercomp.USER_id = USERID("nosweat") AND  usercomp.loc = "" NO-LOCK :
    v-count = v-count + 1 .
END.
FIND FIRST usercomp WHERE usercomp.USER_id = USERID("nosweat") AND
                                  usercomp.company_default NO-LOCK NO-ERROR.
ASSIGN     
lv-default-comp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


{ar/ar-agng2.i new}

DEF TEMP-TABLE w-sort FIELD w-int AS INT.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_comp end_comp ~
btnCustList tb_cust-list begin_cust-no end_cust-no begin_slsmn end_slsmn ~
begin_curr end_curr trend_days as-of-date period-days-1 period-days-2 ~
period-days-3 rs_detail rd_sort rd_sort2 tb_paid tb_include-factored ~
tb_fuel tb_separate-fc begin_inv-date end_inv-date tb_days-old tb_address ~
tb_PrintInvNotes tb_PrintCollectionNotes tb_cust-po tb_job# tgInactiveCust ~
lv-ornt lines-per-page rd-dest lv-font-no td-show-parm tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_comp end_comp tb_cust-list ~
begin_cust-no end_cust-no begin_slsmn end_slsmn begin_curr end_curr ~
trend_days as-of-date period-days-1 period-days-2 period-days-3 rs_detail ~
lbl_sort rd_sort lbl_sort2 rd_sort2 tb_paid tb_include-factored tb_fuel ~
tb_separate-fc begin_inv-date end_inv-date tb_days-old tb_address ~
tb_PrintInvNotes tb_PrintCollectionNotes tb_cust-po tb_job# tgInactiveCust ~
lv-ornt lines-per-page rd-dest lv-font-no lv-font-name td-show-parm ~
tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/01 
     LABEL "As of" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE begin_comp AS CHARACTER FORMAT "X(3)":U 
     LABEL "Beginning Company#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_curr AS CHARACTER FORMAT "X(3)":U 
     LABEL "Beginning Currency" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_inv-date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning SalesRep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_comp AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" 
     LABEL "Ending Company#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_curr AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" 
     LABEL "Ending Currency" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_inv-date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending SalesRep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-araged.csv" 
     LABEL "Excel File Name" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sorted By?" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort2 AS CHARACTER FORMAT "X(256)":U INITIAL "Aged By?" 
     VIEW-AS FILL-IN 
     SIZE 10.8 BY 1 NO-UNDO.

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

DEFINE VARIABLE period-days-1 AS INTEGER FORMAT ">,>>>":U INITIAL 9999 
     LABEL "Period Days 1" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE period-days-2 AS INTEGER FORMAT ">,>>>":U INITIAL 9999 
     LABEL "2" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE period-days-3 AS INTEGER FORMAT ">,>>>":U INITIAL 9999 
     LABEL "3" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE trend_days AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Days for Recent Trend" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Name" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer#", "#Number",
"Name", "Name",
"SalesRep#", "SalesRep#",
"Invoice#", "Invoice#"
     SIZE 56.6 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort2 AS CHARACTER INITIAL "InvDate" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Due Date", "DueDate",
"Invoice Date", "InvDate"
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE rs_detail AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Detail", 1,
"Summary", 2,
"Totals Only", 3
     SIZE 42 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY 8.1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 18.57.

DEFINE VARIABLE tb_address AS LOGICAL INITIAL no 
     LABEL "Print Customer Address?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 31.8 BY .62 NO-UNDO.

DEFINE VARIABLE tb_cust-po AS LOGICAL INITIAL no 
     LABEL "Print Customer PO#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE tb_days-old AS LOGICAL INITIAL no 
     LABEL "Print # of Days Old?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_fuel AS LOGICAL INITIAL yes 
     LABEL "Include Fuel Surcharges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tb_include-factored AS LOGICAL INITIAL no 
     LABEL "Include Factored FG Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE tb_job# AS LOGICAL INITIAL no 
     LABEL "Print JOB#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE tb_paid AS LOGICAL INITIAL no 
     LABEL "Include Paid Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tb_PrintCollectionNotes AS LOGICAL INITIAL no 
     LABEL "Print Collection Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_PrintInvNotes AS LOGICAL INITIAL no 
     LABEL "Print Invoice Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_separate-fc AS LOGICAL INITIAL no 
     LABEL "Separate Finance Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.4 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tgInactiveCust AS LOGICAL INITIAL no 
     LABEL "Inactive Customers?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_comp AT ROW 1.86 COL 27 COLON-ALIGNED
     end_comp AT ROW 1.86 COL 70 COLON-ALIGNED
     btnCustList AT ROW 3 COL 61.6 WIDGET-ID 22
     tb_cust-list AT ROW 3.1 COL 29.2 WIDGET-ID 24
     begin_cust-no AT ROW 3.81 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 3.81 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_slsmn AT ROW 4.76 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 4.76 COL 70 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_curr AT ROW 5.71 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Currency Code"
     end_curr AT ROW 5.71 COL 70 COLON-ALIGNED HELP
          "Enter Ending Currency Code"
     trend_days AT ROW 6.71 COL 70 COLON-ALIGNED WIDGET-ID 14
     as-of-date AT ROW 6.91 COL 27 COLON-ALIGNED
     period-days-1 AT ROW 8.1 COL 27 COLON-ALIGNED
     period-days-2 AT ROW 8.1 COL 39 COLON-ALIGNED
     period-days-3 AT ROW 8.1 COL 51 COLON-ALIGNED
     rs_detail AT ROW 9.05 COL 29 NO-LABEL WIDGET-ID 6
     lbl_sort AT ROW 10 COL 15 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 10 COL 29 NO-LABEL
     lbl_sort2 AT ROW 11 COL 15.8 COLON-ALIGNED NO-LABEL
     rd_sort2 AT ROW 11.05 COL 29 NO-LABEL
     tb_paid AT ROW 11.95 COL 29
     tb_include-factored AT ROW 11.95 COL 59.6
     tb_fuel AT ROW 12.91 COL 29
     tb_separate-fc AT ROW 12.91 COL 59.6
     begin_inv-date AT ROW 13.91 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Date"
     end_inv-date AT ROW 13.91 COL 70 COLON-ALIGNED HELP
          "Enter Ending Invoice Date"
     tb_days-old AT ROW 14.95 COL 29
     tb_address AT ROW 14.95 COL 59
     tb_PrintInvNotes AT ROW 16.24 COL 29 WIDGET-ID 10
     tb_PrintCollectionNotes AT ROW 16.24 COL 59 WIDGET-ID 12
     tb_cust-po AT ROW 17.43 COL 29 WIDGET-ID 16
     tb_job# AT ROW 17.43 COL 59 WIDGET-ID 18
     tgInactiveCust AT ROW 18.48 COL 29 WIDGET-ID 20
     lv-ornt AT ROW 20.48 COL 32 NO-LABEL
     lines-per-page AT ROW 20.48 COL 84.8 COLON-ALIGNED
     rd-dest AT ROW 20.91 COL 7 NO-LABEL
     lv-font-no AT ROW 21.91 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 23.05 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 24.24 COL 31
     tb_excel AT ROW 25.62 COL 70 RIGHT-ALIGNED
     tb_runExcel AT ROW 25.62 COL 93 RIGHT-ALIGNED
     fi_file AT ROW 26.57 COL 48 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 28.38 COL 22
     btn-cancel AT ROW 28.38 COL 61
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     "Detail Only" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 15.71 COL 32.8 WIDGET-ID 4
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 20.05 COL 4
     RECT-6 AT ROW 19.81 COL 2
     RECT-7 AT ROW 1.05 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 99.8 BY 29.33.


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
         TITLE              = "AR Aged Receivables"
         HEIGHT             = 29.67
         WIDTH              = 101.4
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
       as-of-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_comp:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_curr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_inv-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_comp:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_curr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_inv-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lbl_sort2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort2".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       period-days-1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       period-days-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       period-days-3:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm,Sort 1".

ASSIGN 
       rd_sort2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm,Sort 2".

ASSIGN 
       rs_detail:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm,Report Type".

ASSIGN 
       tb_address:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-po:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_days-old:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_fuel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_include-factored:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_job#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_paid:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_PrintCollectionNotes:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_PrintInvNotes:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_separate-fc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tgInactiveCust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       trend_days:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* AR Aged Receivables */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* AR Aged Receivables */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME as-of-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL as-of-date C-Win
ON LEAVE OF as-of-date IN FRAME FRAME-A /* As of */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_comp C-Win
ON LEAVE OF begin_comp IN FRAME FRAME-A /* Beginning Company# */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_curr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_curr C-Win
ON LEAVE OF begin_curr IN FRAME FRAME-A /* Beginning Currency */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv-date C-Win
ON LEAVE OF begin_inv-date IN FRAME FRAME-A /* Beginning Date */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning SalesRep# */
DO:
     ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:

    /*{&WINDOW-NAME}:WINDOW-STATE = WINDOW-minIMIZE.*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN DO:
      EMPTY TEMP-TABLE ttCustList.
      RUN BuildCustList(INPUT cocode,
                        INPUT tb_cust-list AND glCustListActive,
                        INPUT begin_cust-no,
                        INPUT END_cust-no).
  END.
  RUN run-report. 

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &type= 'begin_cust=begin_cust-slsmn'
                            &begin_cust= "begin_slsmn"
                            &end_cust= "begin_slsmn" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
               RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
               {custom/asimail2.i &TYPE = "Customer"
                             &group-title='r-araged.' /* v-prgmname */
                             &begin_cust= "begin_cust-no"
                             &end_cust= "begin_cust-no" 
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr2.i &TYPE = "Customer"
                                  &group-title='r-araged.' /* v-prgmname */
                                  &begin_cust= "begin_cust-no"
                                  &end_cust= "begin_cust-no" 
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  END CASE.
  SESSION:SET-WAIT-STATE ("").
   CURRENT-WINDOW:WINDOW-STATE  = WINDOW-NORMAL.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
DO:
  RUN CustList.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_curr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_curr C-Win
ON LEAVE OF end_curr IN FRAME FRAME-A /* Ending Currency */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv-date C-Win
ON LEAVE OF end_inv-date IN FRAME FRAME-A /* Ending Date */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending SalesRep# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Excel File Name */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME period-days-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period-days-1 C-Win
ON LEAVE OF period-days-1 IN FRAME FRAME-A /* Period Days 1 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME period-days-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period-days-2 C-Win
ON LEAVE OF period-days-2 IN FRAME FRAME-A /* 2 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME period-days-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period-days-3 C-Win
ON LEAVE OF period-days-3 IN FRAME FRAME-A /* 3 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_sort2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort2 C-Win
ON VALUE-CHANGED OF rd_sort2 IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_address
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_address C-Win
ON VALUE-CHANGED OF tb_address IN FRAME FRAME-A /* Print Customer Address? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
DO:
  ASSIGN {&self-name}.
  RUN SetCustRange(INPUT tb_cust-list).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_days-old
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_days-old C-Win
ON VALUE-CHANGED OF tb_days-old IN FRAME FRAME-A /* Print # of Days Old? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_include-factored
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_include-factored C-Win
ON VALUE-CHANGED OF tb_include-factored IN FRAME FRAME-A /* Include Factored FG Items? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_paid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_paid C-Win
ON VALUE-CHANGED OF tb_paid IN FRAME FRAME-A /* Include Paid Invoices? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_PrintCollectionNotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_PrintCollectionNotes C-Win
ON VALUE-CHANGED OF tb_PrintCollectionNotes IN FRAME FRAME-A /* Print Collection Notes? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_PrintInvNotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_PrintInvNotes C-Win
ON VALUE-CHANGED OF tb_PrintInvNotes IN FRAME FRAME-A /* Print Invoice Notes? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME trend_days
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL trend_days C-Win
ON LEAVE OF trend_days IN FRAME FRAME-A /* Days for Recent Trend */
DO:
  ASSIGN {&self-name}.
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
   as-of-date    = TODAY
   period-days-1 = 30
   period-days-2 = 60
   period-days-3 = 90
   end_inv-date  = TODAY
   fi_file = "c:\tmp\ar-aging.csv" 
   begin_comp = lv-default-comp
   end_comp   = lv-default-comp .
  
  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "AR5",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
      
    {custom/usrprint.i}
    as-of-date:SCREEN-VALUE = STRING(TODAY).
    lbl_sort:SCREEN-VALUE =  "Sorted By?" .
    lbl_sort2:SCREEN-VALUE =  "Aged By?" .
    IF v-count LE 1 THEN
        ASSIGN
          begin_comp:SENSITIVE = NO
          end_comp:SENSITIVE   = NO 
          begin_comp:SCREEN-VALUE = lv-default-comp
          begin_comp = lv-default-comp
          end_comp:SCREEN-VALUE = lv-default-comp
          end_comp = lv-default-comp.
    APPLY "entry" TO begin_cust-no.
  END.

  {sys/inc/chblankcust.i ""AR5""}

 IF ou-log THEN DO:
      ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
        tb_cust-list = YES 
        .
      RUN SetCustRange(INPUT tb_cust-list).
  END.
  ELSE
      ASSIGN
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        .

   IF ou-log AND ou-cust-int = 0 THEN DO:
       ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
        tb_cust-list = NO
        .
      RUN SetCustRange(tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "YES").
   END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildCustList C-Win 
PROCEDURE BuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Builds the temp table of customers   
  Parameters:  Company Code, Customer list logical and/or customer range
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplList AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginCust AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEndCust AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-cust FOR cust.

DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.

IF iplList THEN DO:
    RUN sys/ref/CustList.p (INPUT ipcCompany,
                            INPUT 'AR5',
                            INPUT YES,
                            OUTPUT lActive).
END.
ELSE DO:
    FOR EACH bf-cust
        WHERE bf-cust.company EQ ipcCompany
          AND bf-cust.cust-no GE ipcBeginCust
          AND bf-cust.cust-no LE ipcEndCust
        NO-LOCK:
        CREATE ttCustList.
        ASSIGN 
            ttCustList.cust-no = bf-cust.cust-no
            ttCustList.log-fld = YES
        .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustList C-Win 
PROCEDURE CustList :
/*------------------------------------------------------------------------------
  Purpose:  Display a UI of selected customers   
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    RUN sys/ref/CustListManager.w(INPUT cocode,
                                  INPUT 'AR5').


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
  DISPLAY begin_comp end_comp tb_cust-list begin_cust-no end_cust-no begin_slsmn 
          end_slsmn begin_curr end_curr trend_days as-of-date period-days-1 
          period-days-2 period-days-3 rs_detail lbl_sort rd_sort lbl_sort2 
          rd_sort2 tb_paid tb_include-factored tb_fuel tb_separate-fc 
          begin_inv-date end_inv-date tb_days-old tb_address tb_PrintInvNotes 
          tb_PrintCollectionNotes tb_cust-po tb_job# tgInactiveCust lv-ornt 
          lines-per-page rd-dest lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_comp end_comp btnCustList tb_cust-list 
         begin_cust-no end_cust-no begin_slsmn end_slsmn begin_curr end_curr 
         trend_days as-of-date period-days-1 period-days-2 period-days-3 
         rs_detail rd_sort rd_sort2 tb_paid tb_include-factored tb_fuel 
         tb_separate-fc begin_inv-date end_inv-date tb_days-old tb_address 
         tb_PrintInvNotes tb_PrintCollectionNotes tb_cust-po tb_job# 
         tgInactiveCust lv-ornt lines-per-page rd-dest lv-font-no td-show-parm 
         tb_excel tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Output-to-File C-Win 
PROCEDURE Output-to-File :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*  DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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
     */
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
  RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-cust-add C-Win 
PROCEDURE print-cust-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DISPLAY cust.addr[1]                                                SKIP
            cust.addr[2]                                                SKIP
            TRIM(cust.city) + ", " +
            trim(cust.state) + "  " + trim(cust.zip) FORMAT "x(50)"

        WITH NO-LABELS NO-BOX FRAME cust-detail WIDTH 132.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* --------------------------------------------------- ar/ar-aging.p  9/94 RM */
/* A/R Aged Receivables Report Program - A/R Module                           */
/* -------------------------------------------------------------------------- */
DEF  VAR  str-tit4 AS cha FORM "x(300)" NO-UNDO.
DEF  VAR str-tit5 AS cha FORM "x(300)" NO-UNDO.
{sys/form/r-top5L3.f}
DEF VAR li AS INT NO-UNDO.
DEF VAR v-hdr AS CHAR INIT "Customer,Name,Contact,SalesRep,Terms,Address1,Address2,City,State,Zip,Credit Limit,Phone,Fax,Check/Memo,DaysOld,Type,Invoice#,InvoiceDate,InvoiceAmt,Current,ADTP,TD," NO-UNDO.  /*Task# 11151304*/
DEF VAR v-hdr2 AS CHAR NO-UNDO.
DEF VAR v-rpt-type AS CHAR NO-UNDO.

IF rs_detail = 1 THEN 
    v-rpt-type = "DETAIL".
  ELSE IF rs_detail = 2  THEN
      v-rpt-type = "SUMMARY".
  ELSE IF rs_detail = 3 THEN
      v-rpt-type = "TOTALS ONLY".

IF tb_address = YES THEN
   v-hdr2 = "Customer,Name,Address1,Address2,City,State,Zip,Phone,Fax,Amount,Current,ADTP,TD,".      /*Task# 11151304*/
ELSE
   v-hdr2 = "Customer,Name,Phone,Amount,Current,ADTP,TD,".

ASSIGN
 str-tit2 = c-win:TITLE + " - " + v-rpt-type
 {sys/inc/ctrtext.i str-tit2 112}

 v-s-cust           = begin_cust-no
 v-e-cust           = end_cust-no
 v-s-sman           = begin_slsmn
 v-e-sman           = end_slsmn
 v-s-curr           = begin_curr
 v-e-curr           = end_curr
 v-date             = as-of-date
 v-trend-days       = trend_days
 v-days[1]          = period-days-1
 v-days[2]          = period-days-2
 v-days[3]          = period-days-3
 det-rpt            = rs_detail
 v-sort             = rd_sort
 v-sort2            = rd_sort2
 v-inc              = tb_paid
 v-s-dat            = begin_inv-date
 v-e-dat            = end_inv-date
 v-days-old         = tb_days-old
 v-prt-add          = tb_address
 v-exp-name         = fi_file
 v-include-factored = tb_include-factored
 v-export           = tb_excel
 v-include-fuel     = tb_fuel
 v-sep-fc           = tb_separate-fc
 b-comp             = begin_comp
 e-comp             = end_comp
 v-print-job        = tb_job#
 v-inactive-custs   = tgInactiveCust
 v-print-cust-po    = tb_cust-po
 sPrtInvNote = tb_PrintInvNotes
 sPrtCollectionNote = tb_PrintCollectionNotes
 lSelected          = tb_cust-list

 str-tit3 = "Company From: " + STRING(begin_comp) + " To: " + STRING(end_comp) +  "    As of Date: " + STRING(v-date)
 {sys/inc/ctrtext.i str-tit3 132}.
 str-tit4 = "Sorted By: " + STRING(rd_sort) + "     "   +  "Aged By : " + STRING(rd_sort2) .
 {sys/inc/ctrtext.i str-tit4 132}.

SESSION:SET-WAIT-STATE ("general").

DO WITH FRAME {&frame-name}:
 FOR EACH w-sort:
    DELETE w-sort.
  END.
  DO li = 1 TO 3:
    CREATE w-sort.
    w-int = v-days[li].
  END.
  li = 0.
  FOR EACH w-sort BY w-int:
    li = li + 1.
    v-days[li] = w-int.
    IF i GT 2 THEN LEAVE.
  END.
  ASSIGN
   period-days-1:screen-value = STRING(v-days[1])
   period-days-1
   period-days-2:screen-value = STRING(v-days[2])
   period-days-2
   period-days-3:screen-value = STRING(v-days[3])
   period-days-3.
END.
IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN v-s-cust = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN v-e-cust = ttCustList.cust-no .
END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

VIEW FRAME r-top.

ASSIGN grand-t = 0
       grand-t-pri = 0
       grand-t-fc = 0.

  IF td-show-parm THEN RUN show-param.

  IF tb_excel THEN DO:
    OUTPUT stream s-temp to value(v-exp-name).

    IF det-rpt = 1 THEN DO:
      v-hdr = v-hdr + trim(STRING(v-days[1],">,>>>")) + "," +
                      trim(STRING(v-days[2],">,>>>")) + "," +
                      trim(STRING(v-days[3],">,>>>")) + "+". 
      IF tb_cust-po THEN DO:
            v-hdr = v-hdr + "," + trim(STRING("Customer PO#")) .                /*Task# 02071402*/
            PUT STREAM s-temp UNFORMATTED v-hdr SKIP.
      END.
      ELSE
          PUT STREAM s-temp UNFORMATTED v-hdr SKIP.
    END.
    ELSE DO:
       v-hdr2 = v-hdr2 + trim(STRING(v-days[1],">,>>>")) + "," +
                         trim(STRING(v-days[2],">,>>>")) + "," +
                         trim(STRING(v-days[3],">,>>>")) + "+".
           PUT STREAM s-temp UNFORMATTED v-hdr2 SKIP.
    END.
  END.


  IF v-sort2 BEGINS "Due" THEN
    IF v-sort EQ "Name" THEN
      RUN ar/ar-agng1.p.
    ELSE
    IF v-sort EQ "#Number" THEN
      RUN ar/ar-agng2.p.
    ELSE
      IF v-sort EQ "SalesRep#" THEN
      RUN ar/ar-agng7.p.
      ELSE
       RUN ar/ar-agng5.p.
  ELSE
  IF v-sort2 BEGINS "InvD" THEN
    IF v-sort EQ "Name" THEN
      RUN ar/ar-agng3.p.
    ELSE
    IF v-sort EQ "#Number" THEN
      RUN ar/ar-agng4.p.
    ELSE IF v-sort EQ "SalesRep#" THEN
      RUN ar/ar-agng8.p.
    ELSE RUN ar/ar-agng6.p.

 /* ELSE
    IF v-sort EQ "Name" THEN
      RUN ar/ar-agng5.p.
    ELSE
    IF v-sort EQ "#Number" THEN
      RUN ar/ar-agng6.p.
    ELSE
      RUN ar/ar-agng9.p.*/

  IF tb_excel THEN DO:
    OUTPUT STREAM s-temp CLOSE.
    IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
  END.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCustRange C-Win 
PROCEDURE SetCustRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
        begin_cust-no:SENSITIVE = NOT iplChecked
        end_cust-no:SENSITIVE = NOT iplChecked
        begin_cust-no:VISIBLE = NOT iplChecked
        end_cust-no:VISIBLE = NOT iplChecked
        btnCustList:SENSITIVE = iplChecked
       .
  END.
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
  DEF VAR lv-frame-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field2-hdl AS HANDLE NO-UNDO.
  DEF VAR parm-fld-list AS cha NO-UNDO.
  DEF VAR parm-lbl-list AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-label AS cha.

  lv-frame-hdl = FRAME {&frame-name}:handle.
  RUN sys/ref/getParms.p (INPUT lv-frame-hdl, 
                          OUTPUT parm-fld-list, 
                          OUTPUT parm-lbl-list).
/*   lv-group-hdl = lv-frame-hdl:first-child.                                             */
/*   lv-field-hdl = lv-group-hdl:first-child .                                            */
/*                                                                                        */
/*   do while true:                                                                       */
/*      if not valid-handle(lv-field-hdl) then leave.                                     */
/*      if lookup(lv-field-hdl:private-data,"parm") > 0                                   */
/*         then do:                                                                       */
/*            if lv-field-hdl:label <> ? then                                             */
/*               assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","   */
/*                      parm-lbl-list = parm-lbl-list + lv-field-hdl:label + ","          */
/*                      .                                                                 */
/*            else do:  /* radio set */                                                   */
/*            MESSAGE lv-field-hdl:NAME lv-field-hdl:LABEL                                */
/*              VIEW-AS ALERT-BOX INFO BUTTONS OK.                                        */
/*               assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","   */
/*                      .                                                                 */
/*               lv-field2-hdl = lv-group-hdl:first-child.                                */
/*               repeat:                                                                  */
/*                   if not valid-handle(lv-field2-hdl) then leave.                       */
/*                   if lv-field2-hdl:private-data = lv-field-hdl:name then do:           */
/*                      parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",". */
/*                   end.                                                                 */
/*                   lv-field2-hdl = lv-field2-hdl:next-sibling.                          */
/*               end.                                                                     */
/*            end.                                                                        */
/*         end.                                                                           */
/*      lv-field-hdl = lv-field-hdl:next-sibling.                                         */
/*   end.                                                                                 */

  PUT SPACE(28)
      "< Selection Parameters >"
      SKIP(1).

  DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
    IF ENTRY(i,parm-fld-list) NE "" OR
       entry(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                 trim(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.

  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

