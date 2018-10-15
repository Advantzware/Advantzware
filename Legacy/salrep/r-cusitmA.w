&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-cusitm.w

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
DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

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

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

def var fcust as ch init "" NO-UNDO.
def var tcust like fcust init "zzzzzzzz" NO-UNDO.
def var fship as ch init "" NO-UNDO.
def var tship like fcust init "zzzzzzzz" NO-UNDO.
def var fshpz as ch format "x(10)" init "" NO-UNDO.
def var tshpz like fcust init "zzzzzzzzzz" NO-UNDO.
def var fitem like itemfg.i-no init " " NO-UNDO.
def var titem like fitem init "zzzzzzzzzzzzzzzzzzz" NO-UNDO.
def var fsman as char format "x(3)" init "" NO-UNDO.
def var tsman like fsman init "zzz" NO-UNDO.
def var fdate as date format "99/99/9999" NO-UNDO.
def var tdate like fdate NO-UNDO.
def var fpo as char format "x(15)" init " " NO-UNDO.
def var tpo like fpo init "zzzzzzzzzzzzzzz" NO-UNDO.
def var v-det as log format "Detail/Summary" init YES NO-UNDO.
def var v-sort1 as char format "!" init "I" NO-UNDO.
def var v-print1 as log format "Cust/Item" init YES NO-UNDO.
def var v-disc-p as log init NO NO-UNDO.
def var v-freight as log init NO NO-UNDO.
def var v-inc-fc as log init NO NO-UNDO.

def var v-date like ar-inv.inv-date column-label "Invoice!Date" NO-UNDO.
def var v-ord  like ar-invl.ord-no column-label "Order!Number" NO-UNDO.
def var v-pric like ar-invl.unit-pr column-label "Unit Price" NO-UNDO.
def var v-uom  like ar-invl.pr-uom column-label "UOM" NO-UNDO.

def var v-sman-no as   char format "x(3)" no-undo.

def var v-exc     as   LOG NO-UNDO.
def var v-name    like cust.name format "x(21)" NO-UNDO.

def var v-pct as dec format "99.99" no-undo.
def var v-fac as int no-undo.
def var v-ship like ar-inv.ship-id no-undo.
def var v-shpz like ar-inv.sold-zip no-undo.
def var v-disc like ar-invl.disc no-undo.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD v-po AS CHAR .

DEF BUFFER xreport FOR tt-report.

def TEMP-TABLE w-data NO-UNDO
  field i-no      like ar-invl.i-no column-label "FG Item"
  field inv-no    like ar-invl.inv-no column-label "Invoice!Number"
  field rec-id    as recid.

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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tb_cust-list btnCustList ~
begin_cust-no end_cust-no begin_cust-type end_cust-type begin_ship-to ~
end_ship-to begin_slsmn end_slsmn begin_i-no end_i-no begin_inv-date ~
end_inv-date begin_po end_po tb_detailed rd_show rd_sort tb_disprice ~
tb_fin-chg rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel ~
tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust-no end_cust-no ~
begin_cust-type end_cust-type begin_ship-to end_ship-to begin_slsmn ~
end_slsmn begin_i-no end_i-no begin_inv-date end_inv-date begin_po end_po ~
tb_detailed lbl_show1 rd_show lbl_sort rd_sort tb_disprice tb_fin-chg ~
rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm ~
tb_excel tb_runExcel fi_file 

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

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_cust-type AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Cust Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_inv-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_po AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ship-to AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Ship-to#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-type AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Cust Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_inv-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ship-to AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Ship-to #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-cusitm.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_show1 AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_show AS CHARACTER INITIAL "Customer Name" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer Name", "Customer Name",
"Item", "Item"
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Item#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer#", "Customer#",
"Ship-to#", "Ship-to#",
"Item#", "Item#",
"Order#", "Order#"
     SIZE 50 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 8.81.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 13.57.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL yes 
     LABEL "Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tb_disprice AS LOGICAL INITIAL no 
     LABEL "Show Discounted Prices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_fin-chg AS LOGICAL INITIAL no 
     LABEL "Include Finance Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

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
     tb_cust-list AT ROW 1.52 COL 30.8 WIDGET-ID 6
     btnCustList AT ROW 1.57 COL 62.8 WIDGET-ID 8
     begin_cust-no AT ROW 2.62 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.62 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_cust-type AT ROW 3.57 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Type"
     end_cust-type AT ROW 3.57 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Type"
     begin_ship-to AT ROW 4.52 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Ship-to Number"
     end_ship-to AT ROW 4.52 COL 70 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_slsmn AT ROW 5.48 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 5.48 COL 70 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_i-no AT ROW 6.43 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 6.43 COL 70 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_inv-date AT ROW 7.38 COL 27 COLON-ALIGNED
     end_inv-date AT ROW 7.38 COL 70 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     begin_po AT ROW 8.33 COL 27 COLON-ALIGNED HELP
          "Enter Beginning PO#"
     end_po AT ROW 8.33 COL 70 COLON-ALIGNED HELP
          "Enter Ending PO#"
     tb_detailed AT ROW 9.62 COL 56 RIGHT-ALIGNED
     lbl_show1 AT ROW 10.57 COL 32 COLON-ALIGNED NO-LABEL
     rd_show AT ROW 10.57 COL 41 NO-LABEL
     lbl_sort AT ROW 11.52 COL 32 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 11.52 COL 41 NO-LABEL
     tb_disprice AT ROW 12.48 COL 41
     tb_fin-chg AT ROW 13.43 COL 41
     rd-dest AT ROW 15.91 COL 5 NO-LABEL
     lv-ornt AT ROW 16.38 COL 31 NO-LABEL
     lines-per-page AT ROW 16.38 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 18.24 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 19.24 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 20.29 COL 30
     tb_excel AT ROW 21.52 COL 50 RIGHT-ALIGNED
     tb_runExcel AT ROW 21.52 COL 71 RIGHT-ALIGNED
     fi_file AT ROW 22.33 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 24 COL 19
     btn-cancel AT ROW 24 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.14 COL 3
     RECT-6 AT ROW 14.81 COL 1
     RECT-7 AT ROW 1.1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.24
         SIZE 96.4 BY 25.43.


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
         TITLE              = "Sales Analysis - By Customer/Item"
         HEIGHT             = 25.67
         WIDTH              = 96.4
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
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_inv-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_po:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ship-to:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_inv-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_po:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ship-to:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_show1 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_show1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_show".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_show:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_detailed IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_disprice:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_fin-chg:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Sales Analysis - By Customer/Item */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sales Analysis - By Customer/Item */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-type C-Win
ON LEAVE OF begin_cust-type IN FRAME FRAME-A /* Beginning Cust Type */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv-date C-Win
ON LEAVE OF begin_inv-date IN FRAME FRAME-A /* Beginning Invoice Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship-to C-Win
ON LEAVE OF begin_ship-to IN FRAME FRAME-A /* Beginning Ship-to# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Salesrep# */
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

  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT tb_cust-list OR  NOT AVAIL ttCustList THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust-no,
                    INPUT end_cust-no).
  END.

  RUN run-report.
  STATUS DEFAULT "Processing Complete".
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type="Salesman"
                            &begin_cust=begin_slsmn
                            &END_cust= begin_slsmn
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:TITLE 
                            &fax-file=list-name }
       END. 
       when 5 then do:

           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Salesman"
                             &begin_cust= begin_slsmn
                             &END_cust=begin_slsmn
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Salesman"
                                  &begin_cust= begin_slsmn
                                  &END_cust=begin_slsmn
                                  &mail-subject= c-win:TITLE   
                                  &mail-body=  c-win:TITLE  
                                  &mail-file=list-name }
     END.
       END.
       WHEN 6 THEN RUN OUTPUT-to-port.

  end case.
  SESSION:SET-WAIT-STATE (""). 
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


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-type C-Win
ON LEAVE OF end_cust-type IN FRAME FRAME-A /* Ending Cust Type */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv-date C-Win
ON LEAVE OF end_inv-date IN FRAME FRAME-A /* Ending Invoice Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ship-to C-Win
ON LEAVE OF end_ship-to IN FRAME FRAME-A /* Ending Ship-to # */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Salesrep# */
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


&Scoped-define SELF-NAME rd_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_show C-Win
ON VALUE-CHANGED OF rd_show IN FRAME FRAME-A
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
DO:
  assign {&self-name}.
  EMPTY TEMP-TABLE ttCustList.
  RUN SetCustRange(INPUT tb_cust-list).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_disprice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_disprice C-Win
ON VALUE-CHANGED OF tb_disprice IN FRAME FRAME-A /* Show Discounted Prices? */
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


&Scoped-define SELF-NAME tb_fin-chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fin-chg C-Win
ON VALUE-CHANGED OF tb_fin-chg IN FRAME FRAME-A /* Include Finance Charges? */
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
   begin_inv-date = date(1,1,year(today))
   end_inv-date   = today.

  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "HZ",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust-no.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'HZ',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""HZ""}

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

   IF ou-log AND ou-cust-int = 0 THEN do:
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
                            INPUT 'HZ',
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report C-Win 
PROCEDURE create-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input parameter ip-recid  as   recid            no-undo.
def input parameter ip-key-03 like tt-report.key-03 no-undo.
def input parameter ip-key-04 like tt-report.key-04 no-undo.
def input parameter ip-key-10 like tt-report.key-10 no-undo.


create xreport.

assign
 v-exc           = no
 xreport.term-id = ""
 xreport.rec-id  = ip-recid
 xreport.key-01  = trim(if v-sort1 eq "Z" then cust.zip else "") +
                   tt-report.key-09
 xreport.key-02  = if v-sort1 eq "H" then (v-shpz + v-ship) else
                   if v-sort1 eq "S" then v-ship else
                   if v-sort1 eq "O" then
                     string(ar-invl.ord-no,"999999") else ""
 xreport.key-03  = ip-key-03
 xreport.key-04  = ip-key-04
 xreport.key-05  = v-ship
 xreport.key-06  = v-sman-no
 xreport.key-07  = xreport.key-03
 xreport.key-09  = tt-report.key-09
 xreport.key-10  = ip-key-10 
 xreport.v-po    = ar-invl.po-no .

if xreport.key-02 eq "" and (v-det or v-sort1 ne "I") then
  if v-sort1 eq "C" then
    xreport.key-02 = xreport.key-05.
  else
    assign
     xreport.key-02 = xreport.key-03
     xreport.key-03 = xreport.key-05.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report1 C-Win 
PROCEDURE create-report1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input parameter ip-recid  as   recid         no-undo.
def input parameter ip-key-03 like report.key-03 no-undo.
def input parameter ip-key-04 like report.key-04 no-undo.
def input parameter ip-key-10 like report.key-10 no-undo.

do i = 1 to 3:
  v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
              else ar-invl.sman[i].

  if v-sman-no   lt fsman                         or
     v-sman-no   gt tsman                         or
     (i ne 1 and
      (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.
    run create-report (ip-recid, ip-key-03, ip-key-04, ip-key-10).
    LEAVE.
end.

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
                                  INPUT 'HZ').


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
  DISPLAY tb_cust-list begin_cust-no end_cust-no begin_cust-type end_cust-type 
          begin_ship-to end_ship-to begin_slsmn end_slsmn begin_i-no end_i-no 
          begin_inv-date end_inv-date begin_po end_po tb_detailed lbl_show1 
          rd_show lbl_sort rd_sort tb_disprice tb_fin-chg rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tb_cust-list btnCustList begin_cust-no end_cust-no 
         begin_cust-type end_cust-type begin_ship-to end_ship-to begin_slsmn 
         end_slsmn begin_i-no end_i-no begin_inv-date end_inv-date begin_po 
         end_po tb_detailed rd_show rd_sort tb_disprice tb_fin-chg rd-dest 
         lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel 
         fi_file btn-ok btn-cancel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-excel-1 C-Win 
PROCEDURE print-excel-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-log AS LOG NO-UNDO.
  DEFINE INPUT PARAMETER ip-qty AS DEC NO-UNDO.
  DEFINE INPUT PARAMETER ip-amt AS DEC NO-UNDO.

  PUT STREAM excel UNFORMATTED
      SKIP
      '"' ""  '",'
      '"' ""  '",'
      '"' ""  '",'
      '"' ""  '",'
      '"' ""  '",'
      '"' ""  '",'
      '"' ""  '",'
      '"' ""  '",'
      '"' ""  '",'
      '"' ""  '",'
      '"' ""  '",'.

  IF ip-log THEN
     PUT STREAM excel UNFORMATTED
         '"' STRING(ip-qty,"->>>,>>>,>>>") '",'
         '"' STRING(ip-amt,"->,>>>,>>>,>>9.99") '",'
         '"' "" '",'
         '"' "" '",'
         SKIP.
  ELSE
     PUT STREAM excel UNFORMATTED
         '"' "" '",'
         '"' "" '",'
         '"' STRING(ip-qty,"->>>,>>>,>>>") '",'
         '"' STRING(ip-amt,"->,>>>,>>>,>>9.99") '",'
         SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* -------------------------------------------------------------------------- */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def var v-qty     as   int extent 5 column-label "Qty Shipped" NO-UNDO.
def var v-amt     as   dec extent 5 column-label "Invoice Amt" NO-UNDO.
DEF VAR lv-r-no   LIKE oe-retl.r-no NO-UNDO.
DEF VAR lv-type   AS   CHAR NO-UNDO.

DEF VAR li-seq AS INT NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR lv-quotes AS CHAR NO-UNDO.

{sys/form/r-topw.f}

form cust.cust-no       column-label "Customer"
     v-name             column-label "Customer/Item Name"
     tt-report.key-05   column-label "Ship-to"                format "x(8)"
     w-data.inv-no
     v-date             FORMAT "99/99/99"
     w-data.i-no
     v-ord              format ">>>>>>"
     v-qty[1]           format "->>>,>>>,>>>"
     v-pric             format "->>>,>>>,>>9.99<<"
     v-uom
     v-amt[1]           format "->,>>>,>>>,>>9.99"
   with no-box frame itemx down STREAM-IO width 132.

SESSION:SET-WAIT-STATE ("general").

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 fcust      = begin_cust-no
 tcust      = end_cust-no
 fship      = begin_ship-to
 tship      = end_ship-to
 fsman      = begin_slsmn
 tsman      = END_slsmn
 fitem      = begin_i-no
 titem      = end_i-no
 fdate      = begin_inv-date
 tdate      = end_inv-date
 fpo        = begin_po
 tpo        = end_po
 v-det      = tb_detailed
 v-print1   = rd_show EQ "Customer Name"
 v-sort1    = SUBSTR(rd_sort,1,1) 
 v-disc-p   = tb_disprice              
 v-freight  = NO
 v-inc-fc   = tb_fin-chg
 lv-quotes  = CHR(34).

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   excelheader = "Customer,Customer/Item Name,Ship-To,Invoice Number,Invoice Date,"
               + "FG Item,Order Number,Qty Shipped,Unit Price,UOM,Invoice Amt,"
               + (IF v-sort1 EQ "O" THEN "ORDER QTY. TOTALS,ORDER AMT. TOTALS,"
                  ELSE IF v-sort1 EQ "I" THEN "ITEM QTY. TOTALS,ITEM AMT. TOTALS,"
                  ELSE "SHIP-TO QTY. TOTALS,SHIP-TO AMT. TOTALS,")
               + "CUSTOMER QTY. TOTALS,CUSTOMER AMT. TOTALS".

   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

display "" with frame r-top.

EMPTY TEMP-TABLE tt-report.

FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
    each cust
      where cust.company eq cocode
        and cust.cust-no EQ ttCustList.cust-no /*fcust*/
       /* and cust.cust-no le tcust*/
        and cust.type    ge begin_cust-type
        and cust.type    le end_cust-type
      use-index cust no-lock:
      {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
   {sa/sa-sls03.i "fdate" "tdate"}    
  end.

  for each tt-report
      where tt-report.term-id eq ""
        and tt-report.key-01  eq ""
        and tt-report.key-02  eq ""
        and tt-report.key-03  eq ""
        and tt-report.key-04  eq ""
        and tt-report.key-05  eq ""
        and tt-report.key-06  eq ""
        and tt-report.key-07  eq ""
        and tt-report.key-08  eq "",

      first cust
      where cust.company eq cocode
        and cust.cust-no eq tt-report.key-09
      no-lock

      transaction:
      {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}

    if tt-report.key-10 eq "ar-inv" then do:       
       find ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock.       

       run ship-data.

       if v-ship ge fship and
          v-ship le tship and
          v-shpz ge fshpz and
          v-shpz le tshpz then do:

         for each ar-invl
             where ar-invl.x-no    eq ar-inv.x-no
               and ar-invl.i-no    ge fitem
               and ar-invl.i-no    le titem
               and ar-invl.po-no   ge fpo
               and ar-invl.po-no   le tpo
               and (ar-invl.billable or not ar-invl.misc)
             use-index x-no no-lock:

           run create-report1 (recid(ar-invl),
                               if ar-invl.misc then ar-invl.i-name else
                               if ar-invl.i-no ne "" then ar-invl.i-no else
                               "AR SALE",
                               string(ar-inv.inv-no,"999999"), "").
         end.

         if v-freight and ar-inv.f-bill then do:
            find first ar-invl where ar-invl.x-no eq ar-inv.x-no
                use-index x-no no-lock no-error.

            if avail ar-invl then do:
               v-sman-no = "".

               do i = 1 to 3:
                 if ar-invl.sman[i] ne "" then do:
                   v-sman-no = ar-invl.sman[i].
                   leave.
                 end.
               end.

               if v-sman-no eq "" then v-sman-no = cust.sman.

               if "freight" ge fitem                      and
                  "freight" le titem                      and
                  v-sman-no ge fsman                      and
                  v-sman-no le tsman                      THEN do:

                 run create-report (recid(ar-invl), "FREIGHT",
                                    string(ar-inv.inv-no,"999999"), "FREIGHT").
               END.
            end.
         end.
       end.

       delete tt-report.
    end.

    else
    if tt-report.key-10 eq "ar-cashl" then do:      
       find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
       find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.

       assign
        v-exc            = yes
        tt-report.key-01 = trim(if v-sort1 eq "Z" then cust.zip else "") +
                           tt-report.key-09
        tt-report.key-02 = if v-sort1 ne "I" then
                             (if v-sort1 eq "H" then cust.zip else "") +
                              tt-report.key-09
                           else ""
        tt-report.key-03 = "MEMO"
        tt-report.key-04 = string(ar-cashl.inv-no,"999999")
        tt-report.key-05 = tt-report.key-09
        tt-report.key-06 = cust.sman
        tt-report.key-07 = tt-report.key-03.

       release ar-inv.

       RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

       ASSIGN
        lv-r-no = 0
        lv-type = "".

       IF AVAIL reftable THEN
          ASSIGN
             lv-r-no = reftable.val[1]
             lv-type = reftable.dscr.
       ELSE
       IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
         ASSIGN
            lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
            lv-type = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).

       IF lv-r-no NE 0 THEN DO:
         find first oe-reth
             where oe-reth.company eq cocode
               and oe-reth.r-no    eq lv-r-no
             no-lock no-error.
         if avail oe-reth then
         find first ar-inv
              where ar-inv.company eq cocode
                and ar-inv.cust-no eq oe-reth.cust-no
                and ar-inv.inv-no  eq oe-reth.inv-no
              no-lock no-error.
       end.       

       if avail ar-inv then do:
          run ship-data.

          if v-ship ge fship and
             v-ship le tship and
             v-shpz ge fshpz and
             v-shpz le tshpz then
          if lv-type eq "items" then do:
            release ar-invl.
            find first oe-retl
                where oe-retl.company eq cocode
                  and oe-retl.r-no    eq oe-reth.r-no
                  and oe-retl.line    eq ar-cashl.line
                  and oe-retl.i-no    ge fitem
                  and oe-retl.i-no    le titem
                no-lock no-error.
            if avail oe-retl then
            find first ar-invl
                where ar-invl.company eq cocode
                  and ar-invl.cust-no eq ar-cash.cust-no
                  and ar-invl.inv-no  eq ar-cashl.inv-no
                  and ar-invl.i-no    eq oe-retl.i-no
                  and ar-invl.po-no   ge fpo
                  and ar-invl.po-no   le tpo
                  and (ar-invl.billable or not ar-invl.misc)
                no-lock no-error.
            if avail ar-invl then do:
               run create-report1 (recid(ar-cashl), oe-retl.i-no,
                                   tt-report.key-04, "").
               delete tt-report.
            end.
          end.

          else
          if lv-type   eq "freight"                  and
             "freight" ge fitem                      and
             "freight" le titem                      and
             cust.sman ge fsman                      and
             cust.sman le tsman                      and
             v-freight                               then
            assign
             v-exc            = no
             tt-report.key-02 = if v-sort1 ne "I" then v-ship else ""
             tt-report.key-03 = "FREIGHT"
             tt-report.key-05 = v-ship.

          else
          if lv-type   eq "tax"                  and
             "tax"     ge fitem                  and
             "tax"     le titem                  and
             cust.sman ge fsman                  and
             cust.sman le tsman                  then
            assign
             v-exc            = no
             tt-report.key-02 = if v-sort1 ne "I" then v-ship else ""
             tt-report.key-03 = "TAX"
             tt-report.key-05 = v-ship.

          else
          if ""        ge fitem and
             ""        le titem and
             cust.sman ge fsman and
             cust.sman le tsman then v-exc = no.
       end.

       else
       if ""               ge fitem and
          ""               le titem and
          cust.sman        ge fsman and
          cust.sman        le tsman and
          ar-cashl.cust-no ge fship and
          ar-cashl.cust-no le tship and
          cust.zip         ge fshpz and
          cust.zip         le tshpz then v-exc = no.

       if avail tt-report then do:
          tt-report.key-07 = tt-report.key-03.

          if v-exc then delete tt-report.

          else
          if tt-report.key-02 eq "" and (v-det or v-sort1 ne "I") then
            if v-sort1 eq "C" then
              tt-report.key-02 = tt-report.key-05.
            else
              assign
               tt-report.key-02 = tt-report.key-03
               tt-report.key-03 = tt-report.key-05.
       end.     
    end.
  end.
  FOR EACH xreport NO-LOCK:
  END.
  for each tt-report where tt-report.term-id eq "",
      first cust
      where cust.company eq cocode
        and cust.cust-no eq tt-report.key-09
      no-lock

      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            by tt-report.key-04
            by tt-report.key-05

      with frame itemx down

      transaction:

    create w-data.
    assign
     w-data.i-no   = tt-report.key-07
     w-data.inv-no = int(tt-report.key-04)
     w-data.rec-id = tt-report.rec-id.

    find first ar-invl
        where recid(ar-invl) eq w-data.rec-id
        no-lock no-error.

    if avail ar-invl then do:
      find ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.
      assign
       v-date   = ar-inv.inv-date
       v-ord    = ar-invl.ord-no
       v-pric   = ar-invl.unit-pr
       v-uom    = ar-invl.pr-uom
       v-qty[1] = ar-invl.ship-qty
       v-amt[1] = ar-invl.amt
       v-disc   = ar-invl.disc
       v-pct    = 1.

      if tt-report.key-10 eq "FREIGHT" then
        assign
         v-pric   = ar-inv.freight
         v-uom    = ""
         v-qty[1] = 0
         v-amt[1] = ar-inv.freight
         v-disc   = 0.

      else do:
        do i = 1 to 3:
          if ar-invl.sman[i] eq tt-report.key-06 then
            assign
             v-pct = ar-invl.s-pct[i] / 100
             i     = 3.
        end.

        if v-pct eq 0 then
        do i = 1 to 3:
          if i eq 1 then j = 0.
          if ar-invl.sman[i] ne "" then j = j + 1.
          if i eq 3 then v-pct = 1 / j.
        end.

        if v-pct le 0 or v-pct eq ? then v-pct = 1.
      end.

      v-amt[1] = v-amt[1] * v-pct.
    end.

    else do:
      find first ar-cashl
          where recid(ar-cashl) eq w-data.rec-id
          no-lock no-error.

      if avail ar-cashl then do:
        find first ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock.

        assign
         v-date   = ar-cash.check-date
         v-ord    = 0
         v-pric   = ar-cashl.amt-paid - ar-cashl.amt-disc
         v-uom    = ""
         v-qty[1] = 0
         v-amt[1] = ar-cashl.amt-paid - ar-cashl.amt-disc
         v-disc   = 0.

        RELEASE ar-invl.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

        if avail oe-retl then do:
          assign
           v-ord    = oe-retl.ord-no
           v-pric   = oe-retl.unit-pr
           v-uom    = oe-retl.uom
           v-qty[1] = - oe-retl.tot-qty-return.

          find first ar-invl
              where ar-invl.company eq cocode
                and ar-invl.cust-no eq ar-cash.cust-no
                and ar-invl.inv-no  eq ar-cashl.inv-no
                and ar-invl.i-no    eq oe-retl.i-no
              no-lock no-error.

          if avail ar-invl then do:
            /* Added for decimal problem */
            assign v-pric   = ar-invl.unit-pr.

            do i = 1 to 3:
              if ar-invl.sman[i] eq tt-report.key-06 then
                assign
                 v-pct = ar-invl.s-pct[i] / 100
                 i     = 3.
            end.

            if v-pct eq 0 then
            do i = 1 to 3:
              if i eq 1 then j = 0.
              if ar-invl.sman[i] ne "" then j = j + 1.
              if i eq 3 then v-pct = 1 / j.
            end.

            if v-pct le 0 or v-pct eq ? then v-pct = 1.
              assign
               v-amt[1] = v-amt[1] * v-pct
               v-disc   = ar-invl.disc.
          end.
        end.
      end.
    end.

    if v-print1 then v-name = cust.name.

    else do:
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq w-data.i-no
          no-lock no-error.
      v-name = if avail itemfg then itemfg.i-name else w-data.i-no.
    end.

    if v-det then do:
      if v-disc-p and v-disc ne 0 then
        v-pric = v-pric * (100 - v-disc) / 100.

      display cust.cust-no         when first-of(tt-report.key-01)
              v-name               when (v-print1 and first-of(tt-report.key-01))
                                     or (not v-print1)
              tt-report.key-05
              w-data.inv-no
              v-date
              w-data.i-no
              v-ord
              v-qty[1]
              v-pric
              v-uom
              v-amt[1].
      down.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' IF first-of(tt-report.key-01) THEN
                    cust.cust-no ELSE ""                           '",'
             '"' IF (v-print1 and first-of(tt-report.key-01))
                    or (not v-print1) THEN v-name ELSE ""          '",'
             '"' tt-report.key-05                                  '",'
             '"' w-data.inv-no                                     '",'
             '"' STRING(v-date)                                    '",'
             '"' REPLACE(w-data.i-no,lv-quotes,"")                 '",'
             '"' STRING(v-ord,">>>>>>")                            '",'
             '"' STRING(v-qty[1],"->>>,>>>,>>>")                   '",'
             '"' STRING(v-pric,"->,>>>,>>9.99<<")                  '",'
             '"' v-uom                                             '",'
             '"' STRING(v-amt[1],"->,>>>,>>>,>>9.99")              '",'
             '"' ""                                                '",'
             '"' ""                                                '",'
             SKIP.
    end.

    else
        display cust.cust-no v-name.

    assign
     v-qty[2] = v-qty[2] + v-qty[1]
     v-amt[2] = v-amt[2] + v-amt[1].

    if last-of(tt-report.key-03) then do:
      if v-det then do:
        if v-sort1 ne "O" then do:
          underline v-name v-qty[1] v-amt[1] with frame itemx.

          display "          ITEM TOTALS"                     @ v-name
                  "       SHIP-TO TOTALS" when v-sort1 eq "I" @ v-name
                  v-qty[2] @ v-qty[1]
                  v-amt[2] @ v-amt[1]

              with frame itemx.

          down with frame itemx.

          IF tb_excel THEN
             RUN print-excel-1(INPUT YES, INPUT v-qty[2], INPUT v-amt[2]).

          put skip(1).
        end.
      end.

      else do:
        display v-name             when not v-print1
                tt-report.key-05   when v-sort1 ne "I"
                w-data.i-no
                v-ord              when v-sort1 eq "O"
                v-qty[2] @ v-qty[1]
                v-amt[2] @ v-amt[1].
        down.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' cust.cust-no  '",'
              '"' v-name '",'
              '"' IF v-sort1 ne "I" THEN tt-report.key-05 ELSE "" '",'
              '"' "" '",'
              '"' "" '",'
              '"' REPLACE(w-data.i-no,lv-quotes,"") '",'
              '"' IF v-sort1 eq "O" THEN STRING(v-ord,">>>>>>") ELSE "" '",'
              '"' STRING(v-qty[2],"->>>,>>>,>>>") '",'
              '"' "" '",'
              '"' "" '",'
              '"' STRING(v-amt[2],"->,>>>,>>>,>>9.99") '",'
              '"' ""                                                '",'
              '"' ""                                                '",' SKIP.
      end.

      assign
       v-qty[3] = v-qty[3] + v-qty[2]
       v-amt[3] = v-amt[3] + v-amt[2]

       v-qty[2] = 0
       v-amt[2] = 0.
    end.

    if last-of(tt-report.key-02) then do:
      if v-det or v-sort1 ne "I" then do:
        underline v-name v-qty[1] v-amt[1] with frame itemx.

        display "       SHIP-TO TOTALS"                     @ v-name
                "         ORDER TOTALS" when v-sort1 eq "O" @ v-name
                "          ITEM TOTALS" when v-sort1 eq "I" @ v-name
                v-qty[3] @ v-qty[1]
                v-amt[3] @ v-amt[1]

            with frame itemx.

        down with frame itemx.
        put skip(1).

        IF tb_excel THEN
           RUN print-excel-1(INPUT YES, INPUT v-qty[3], INPUT v-amt[3]).
      end.

      assign
       v-qty[4] = v-qty[4] + v-qty[3]
       v-amt[4] = v-amt[4] + v-amt[3]

       v-qty[3] = 0
       v-amt[3] = 0.
    end.

    if last-of(tt-report.key-01) then do:
      underline v-name v-qty[1] v-amt[1] with frame itemx.

      display "      CUSTOMER TOTALS" @ v-name
              v-qty[4] @ v-qty[1]
              v-amt[4] @ v-amt[1]

          with frame itemx.
      down with frame itemx.

      put skip(1).

      IF tb_excel THEN
         RUN print-excel-1(INPUT NO, INPUT v-qty[4], INPUT v-amt[4]).


      assign
       v-qty[5] = v-qty[5] + v-qty[4]
       v-amt[5] = v-amt[5] + v-amt[4]

       v-qty[4] = 0
       v-amt[4] = 0.
    end.

    delete w-data.

  end.

  /* display final totals */
  put skip(1).

  underline v-name v-qty[1] v-amt[1] with frame itemx.

  display "         GRAND TOTALS" @ v-name
          v-qty[5] @ v-qty[1]
          v-amt[5] @ v-amt[1]

      with frame itemx.

  IF tb_excel THEN
     RUN print-excel-1(INPUT NO,
                       INPUT v-qty[5], INPUT v-amt[5]).

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

end procedure.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ship-data C-Win 
PROCEDURE ship-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
release shipto.

if ar-inv.ship-id ne "" then
find first shipto
    where shipto.company eq cocode
      and shipto.cust-no eq ar-inv.cust-no
      and shipto.ship-id eq ar-inv.ship-id
    no-lock no-error.

if avail shipto then
  assign
   v-ship = ar-inv.ship-id
   v-shpz = shipto.ship-zip.

else
if ar-inv.sold-id ne "" then
  assign
   v-ship = ar-inv.sold-id
   v-shpz = ar-inv.sold-zip.

else
  assign
   v-ship = ar-inv.cust-no
   v-shpz = cust.zip.

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

