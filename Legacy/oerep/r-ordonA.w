&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-ordopn.w

  Description: Open Order Report
  
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

ASSIGN
 cocode = gcompany
 locode = gloc.

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-report-title AS CHAR NO-UNDO.

DEF VAR v-sort AS CHAR NO-UNDO.
DEF VAR v-ordl AS LOG NO-UNDO.
DEF VAR v-q-onh LIKE itemfg.q-onh NO-UNDO.
DEF VAR lv-stat AS CHAR NO-UNDO.
DEF VAR lv-due-date LIKE oe-ordl.req-date NO-UNDO.
DEF VAR tb_sch AS LOG NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD q-onh    LIKE itemfg.q-onh
    FIELD q-shp    LIKE itemfg.q-onh
    FIELD q-rel    LIKE itemfg.q-onh
    FIELD q-wip    LIKE itemfg.q-onh
    FIELD q-avl    LIKE itemfg.q-onh
    FIELD po-no    LIKE oe-ord.po-no
    FIELD inv      AS   LOG
    FIELD inv-no   LIKE ar-invl.inv-no
    FIELD cad-no   LIKE itemfg.cad-no
    FIELD row-id   AS ROWID
    FIELD due-date LIKE oe-ordl.req-date
    FIELD unit-count LIKE eb.cas-cnt
    FIELD units-pallet LIKE eb.cas-pal
    INDEX row-id row-id.

DEF TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin.

DEF VAR lv-pdf-file AS cha NO-UNDO.
{custom/xprint.i}

DEF STREAM st-excel.

DEF VAR v-prompt-excel AS LOG NO-UNDO.

find first sys-ctrl WHERE
     sys-ctrl.company eq cocode AND
     sys-ctrl.name    eq "OR16"
     no-lock no-error.

if not avail sys-ctrl then
   do transaction:
      create sys-ctrl.
      assign
        sys-ctrl.company = cocode
        sys-ctrl.name    = "OR16"
        sys-ctrl.descrip = "Prompt for Excel Filename?".
   end.

v-prompt-excel = sys-ctrl.log-fld.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust-no end_cust-no begin_ord-date ~
end_ord-date begin_po-no end_po-no begin_job-no begin_job-no2 end_job-no ~
end_job-no2 begin_i-no end_i-no begin_cad-no end_cad-no begin_due-date ~
end_due-date begin_userid end_userid begin_slsmn end_slsmn rd_sort-1 ~
rd_sort rd_jstat tb_job-qty tb_0-bal rd_ostat tb_under tb_0-wip rd_due-date ~
tb_0-avl rd_wip-qty tb_job-qoh rd-dest lv-ornt lv-font-no lines-per-page ~
td-show-parm tb_batch tb_excel tb_runExcel v-excel-file btn-ok btn-cancel ~
RECT-39 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_ord-date ~
end_ord-date begin_po-no end_po-no begin_job-no begin_job-no2 end_job-no ~
end_job-no2 begin_i-no end_i-no begin_cad-no end_cad-no begin_due-date ~
end_due-date begin_userid end_userid begin_slsmn end_slsmn lbl_sort-1 ~
rd_sort-1 lbl_sort rd_sort lbl_jstat rd_jstat tb_job-qty tb_0-bal lbl_ostat ~
rd_ostat tb_under tb_0-wip lbl_due-date rd_due-date tb_0-avl lbl_wip-qty ~
rd_wip-qty tb_job-qoh tb_est-count tb_est-pallets rd-dest lv-ornt ~
lv-font-no lines-per-page lv-font-name td-show-parm tb_batch tb_excel ~
tb_runExcel v-excel-file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReplaceCommas C-Win 
FUNCTION ReplaceCommas RETURNS CHARACTER
  ( ipcString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE begin_cad-no AS CHARACTER FORMAT "X(15)" 
     LABEL "Beginning CAD#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE begin_due-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Due Date" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Order Date" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .95 NO-UNDO.

DEFINE VARIABLE begin_po-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE begin_userid AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning User ID" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_cad-no AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending CAD#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_due-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Due Date" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Order Date" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_userid AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending User ID" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_due-date AS CHARACTER FORMAT "X(256)":U INITIAL "Due Date?" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_jstat AS CHARACTER FORMAT "X(256)":U INITIAL "Job Status?" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_ostat AS CHARACTER FORMAT "X(256)":U INITIAL "Order Status?" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Secondary Sort?" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_sort-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Primary Sort?" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_wip-qty AS CHARACTER FORMAT "X(256)":U INITIAL "WIP Qty?" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .95 NO-UNDO.

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

DEFINE VARIABLE v-excel-file AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-ordopn.csv" 
     LABEL "Save Excel To" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 28 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 20 BY 5.95 NO-UNDO.

DEFINE VARIABLE rd_due-date AS CHARACTER INITIAL "Line" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Line", "Line",
"Release", "Release"
     SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE rd_jstat AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"All", "All"
     SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE rd_ostat AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"All", "All"
     SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "PO#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "PO#", "PO#",
"Item", "Item",
"Cust Part#", "Cust Part#",
"FG Item Name", "FG Item Name",
"Order#", "Order#",
"Due Date", "Due Date",
"CAD#", "CAD#"
     SIZE 92 BY .95 NO-UNDO.

DEFINE VARIABLE rd_sort-1 AS CHARACTER INITIAL "Customer#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer#", "Customer#",
"Due Date", "Due Date",
"Sales Rep", "Salesman"
     SIZE 47 BY .95 NO-UNDO.

DEFINE VARIABLE rd_wip-qty AS CHARACTER INITIAL "1" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Order Qty - OH - Ship", "1",
"Job Qty - Rcpts", "2"
     SIZE 44.4 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY .1.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 117 BY 2.62.

DEFINE VARIABLE tb_0-avl AS LOGICAL INITIAL yes 
     LABEL "Include 0 Qty / Act. Release Qty = 0" 
     VIEW-AS TOGGLE-BOX
     SIZE 39.2 BY 1.05 NO-UNDO.

DEFINE VARIABLE tb_0-bal AS LOGICAL INITIAL yes 
     LABEL "Include 0 Order Balance Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.8 BY 1.05 NO-UNDO.

DEFINE VARIABLE tb_0-wip AS LOGICAL INITIAL yes 
     LABEL "Include 0 Qty WIP Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31.6 BY 1.05 NO-UNDO.

DEFINE VARIABLE tb_batch AS LOGICAL INITIAL no 
     LABEL "Run In Batch Mode?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE tb_est-count AS LOGICAL INITIAL no 
     LABEL "Est Unit (Case) Count" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_est-pallets AS LOGICAL INITIAL no 
     LABEL "Est Units/Pallet" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Output to Excel File?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_job-qoh AS LOGICAL INITIAL no 
     LABEL "Include Jobs w/QOH?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_job-qty AS LOGICAL INITIAL no 
     LABEL "Print Job Qty Details?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.2 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_under AS LOGICAL INITIAL no 
     LABEL "Drop Order Underrun%" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 1.95 COL 33 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 1.95 COL 88 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_ord-date AT ROW 2.91 COL 33 COLON-ALIGNED
     end_ord-date AT ROW 2.91 COL 88 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     begin_po-no AT ROW 3.86 COL 33 COLON-ALIGNED HELP
          "Enter Ending Customer PO Number"
     end_po-no AT ROW 3.86 COL 88 COLON-ALIGNED HELP
          "Enter Ending Customer PO Number"
     begin_job-no AT ROW 4.81 COL 33 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 4.81 COL 49 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 4.81 COL 88 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 4.81 COL 104 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_i-no AT ROW 5.76 COL 33 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_i-no AT ROW 5.76 COL 88 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_cad-no AT ROW 6.71 COL 33 COLON-ALIGNED HELP
          "Enter Beginning CAD Number"
     end_cad-no AT ROW 6.71 COL 88 COLON-ALIGNED HELP
          "Enter Ending CAD Number"
     begin_due-date AT ROW 7.67 COL 33 COLON-ALIGNED
     end_due-date AT ROW 7.67 COL 88 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     begin_userid AT ROW 8.62 COL 33 COLON-ALIGNED HELP
          "Enter the Beginning User ID"
     end_userid AT ROW 8.62 COL 88 COLON-ALIGNED HELP
          "Enter the Ending User ID"
     begin_slsmn AT ROW 9.57 COL 33 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 9.57 COL 88 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     lbl_sort-1 AT ROW 11 COL 6 COLON-ALIGNED NO-LABEL
     rd_sort-1 AT ROW 11 COL 24 NO-LABEL
     lbl_sort AT ROW 12.19 COL 3 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 12.19 COL 24 NO-LABEL
     lbl_jstat AT ROW 13.91 COL 1.8 COLON-ALIGNED NO-LABEL
     rd_jstat AT ROW 13.91 COL 19.8 NO-LABEL
     tb_job-qty AT ROW 13.86 COL 52
     tb_0-bal AT ROW 13.86 COL 80
     lbl_ostat AT ROW 14.86 COL 1.8 COLON-ALIGNED NO-LABEL
     rd_ostat AT ROW 14.86 COL 19.8 NO-LABEL
     tb_under AT ROW 14.81 COL 51.8
     tb_0-wip AT ROW 14.81 COL 79.8
     lbl_due-date AT ROW 15.81 COL 2 COLON-ALIGNED NO-LABEL
     rd_due-date AT ROW 15.81 COL 19.8 NO-LABEL
     tb_0-avl AT ROW 15.76 COL 79.6
     lbl_wip-qty AT ROW 16.86 COL 2 COLON-ALIGNED NO-LABEL
     rd_wip-qty AT ROW 16.76 COL 19.6 NO-LABEL
     tb_job-qoh AT ROW 16.71 COL 79.6
     tb_est-count AT ROW 17.67 COL 51.8
     tb_est-pallets AT ROW 17.67 COL 79.6
     rd-dest AT ROW 19.19 COL 8.6 NO-LABEL
     lv-ornt AT ROW 19.62 COL 50.2 NO-LABEL
     lv-font-no AT ROW 19.57 COL 83.8 COLON-ALIGNED
     lines-per-page AT ROW 19.57 COL 111.8 COLON-ALIGNED
     lv-font-name AT ROW 20.76 COL 55 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22 COL 57
     tb_batch AT ROW 22 COL 82.4
     tb_excel AT ROW 22.91 COL 57
     tb_runExcel AT ROW 22.91 COL 102.2 RIGHT-ALIGNED
     v-excel-file AT ROW 24.1 COL 54 COLON-ALIGNED
     btn-ok AT ROW 25.52 COL 32
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119.6 BY 25.86.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
     btn-cancel AT ROW 25.52 COL 75
     " Sort By" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 10.29 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 23 BY .71 AT ROW 1.24 COL 2
          BGCOLOR 2 
     " Print Selections" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 13.33 COL 3
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 18.48 COL 4.6
     RECT-9 AT ROW 10.81 COL 2
     RECT-39 AT ROW 18.86 COL 29
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119.6 BY 25.86.


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
         TITLE              = "Open Order Report"
         HEIGHT             = 25.86
         WIDTH              = 119.6
         MAX-HEIGHT         = 25.86
         MAX-WIDTH          = 119.6
         VIRTUAL-HEIGHT     = 25.86
         VIRTUAL-WIDTH      = 119.6
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
   FRAME-NAME Custom                                                    */
ASSIGN 
       begin_cad-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_userid:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cad-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_userid:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_due-date IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_due-date".

/* SETTINGS FOR FILL-IN lbl_jstat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_jstat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_jstat".

/* SETTINGS FOR FILL-IN lbl_ostat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_ostat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_ostat".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lbl_sort-1 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort-1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort-1".

/* SETTINGS FOR FILL-IN lbl_wip-qty IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_wip-qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_due-date".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_jstat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_ostat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_sort-1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_wip-qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR RECTANGLE RECT-9 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_0-avl:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_0-bal:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_0-wip:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_est-count IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_est-count:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_est-pallets IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_est-pallets:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_job-qoh:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_job-qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_under:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Open Order Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Open Order Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cad-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cad-no C-Win
ON LEAVE OF begin_cad-no IN FRAME FRAME-A /* Beginning CAD# */
DO:
   assign {&self-name}.
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


&Scoped-define SELF-NAME begin_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_due-date C-Win
ON LEAVE OF begin_due-date IN FRAME FRAME-A /* Beginning Due Date */
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


&Scoped-define SELF-NAME begin_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-date C-Win
ON LEAVE OF begin_ord-date IN FRAME FRAME-A /* Beginning Order Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-no C-Win
ON LEAVE OF begin_po-no IN FRAME FRAME-A /* Beginning Customer PO# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Sales Rep# */
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

  ASSIGN
    lv-pdf-file = init-dir + "\OpnOrder"
    is-xprint-form = NO.
  
  IF g_batch THEN tb_batch = YES.
  IF v-prompt-excel AND tb_excel THEN DO:
     DEF VAR v-excel-file2 AS cha NO-UNDO.
     RUN oerep/d-ordexl.w (OUTPUT v-excel-file2).
     v-excel-file = v-excel-file + v-excel-file2 + ".csv".
     IF tb_batch THEN DISPLAY v-excel-file WITH FRAME {&FRAME-NAME}.
  END.
  
  IF tb_batch THEN DO:
     RUN run-batch.
     RETURN NO-APPLY.
  END.

  SESSION:SET-WAIT-STATE("general").
  run run-report. 

  STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= "Customer"
                            &begin_cust=begin_cust-no
                            &END_cust= begin_cust-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           is-xprint-form = YES.
           
            IF is-xprint-form THEN DO:
               RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
               {custom/asimail2.i &TYPE="CUSTOMER"
                             &group-title=v-prgmname
                             &begin_cust=begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject="Open Order Report"
                             &mail-body="Open Order Report"
                             &mail-file=lv-pdf-file + ".pdf" }
           END.
           ELSE DO:
               {custom/asimailr2.i &TYPE="Customer"
                                  &group-title=v-prgmname
                                  &begin_cust=begin_cust-no
                                  &END_cust=begin_cust-no
                                  &mail-subject=c-win:TITLE
                                  &mail-body=c-win:TITLE
                                  &mail-file=list-name }
           END.
       END. 
      WHEN 6 THEN RUN output-to-port.
  end case. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cad-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cad-no C-Win
ON LEAVE OF end_cad-no IN FRAME FRAME-A /* Ending CAD# */
DO:
     assign {&self-name}.
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


&Scoped-define SELF-NAME end_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_due-date C-Win
ON LEAVE OF end_due-date IN FRAME FRAME-A /* Ending Due Date */
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


&Scoped-define SELF-NAME end_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-date C-Win
ON LEAVE OF end_ord-date IN FRAME FRAME-A /* Ending Order Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-no C-Win
ON LEAVE OF end_po-no IN FRAME FRAME-A /* Ending Customer PO# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Sales Rep# */
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

  ASSIGN rd-dest lv-ornt.
  IF rd-dest = 5 THEN DO:
     IF lv-ornt = "p" THEN lines-per-page:SCREEN-VALUE = "60".
     ELSE lines-per-page:SCREEN-VALUE = "45".     
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  assign {&self-name}.
  IF rd-dest = 5 THEN do:
     IF lv-ornt:SCREEN-VALUE BEGINS "p" THEN lines-per-page:SCREEN-VALUE = "60".
     ELSE lines-per-page:SCREEN-VALUE = "65".     
  END.
  ELSE DO:
     IF lv-ornt:SCREEN-VALUE BEGINS "p" THEN lines-per-page:SCREEN-VALUE = "99".
     ELSE lines-per-page:SCREEN-VALUE = "65".     
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_due-date C-Win
ON VALUE-CHANGED OF rd_due-date IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_jstat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_jstat C-Win
ON VALUE-CHANGED OF rd_jstat IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_ostat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_ostat C-Win
ON VALUE-CHANGED OF rd_ostat IN FRAME FRAME-A
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


&Scoped-define SELF-NAME rd_sort-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort-1 C-Win
ON VALUE-CHANGED OF rd_sort-1 IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_wip-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_wip-qty C-Win
ON VALUE-CHANGED OF rd_wip-qty IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_0-avl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_0-avl C-Win
ON VALUE-CHANGED OF tb_0-avl IN FRAME FRAME-A /* Include 0 Qty / Act. Release Qty = 0 */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_0-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_0-bal C-Win
ON VALUE-CHANGED OF tb_0-bal IN FRAME FRAME-A /* Include 0 Order Balance Items? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_0-wip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_0-wip C-Win
ON VALUE-CHANGED OF tb_0-wip IN FRAME FRAME-A /* Include 0 Qty WIP Items? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_est-count
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_est-count C-Win
ON VALUE-CHANGED OF tb_est-count IN FRAME FRAME-A /* Est Unit (Case) Count */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_est-pallets
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_est-pallets C-Win
ON VALUE-CHANGED OF tb_est-pallets IN FRAME FRAME-A /* Est Units/Pallet */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Output to Excel File? */
DO:
  assign {&self-name}
         tb_est-count:SENSITIVE = tb_excel
         tb_est-pallets:SENSITIVE = tb_excel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_job-qoh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_job-qoh C-Win
ON VALUE-CHANGED OF tb_job-qoh IN FRAME FRAME-A /* Include Jobs w/QOH? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_job-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_job-qty C-Win
ON VALUE-CHANGED OF tb_job-qty IN FRAME FRAME-A /* Print Job Qty Details? */
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


&Scoped-define SELF-NAME tb_under
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_under C-Win
ON VALUE-CHANGED OF tb_under IN FRAME FRAME-A /* Drop Order Underrun% */
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


&Scoped-define SELF-NAME v-excel-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-excel-file C-Win
ON LEAVE OF v-excel-file IN FRAME FRAME-A /* Save Excel To */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
DEF VAR lv-title AS CHAR NO-UNDO.

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
   
  begin_ord-date = TODAY.
  IF g_batch THEN tb_batch = YES.

  FIND FIRST users WHERE
       users.user_id EQ USERID("NOSWEAT")
       NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     init-dir = users.user_program[2].
  ELSE
     init-dir = "c:\tmp".

  RUN enable_UI.
  
  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    lv-title = c-win:TITLE.
    {custom/usrprint.i}
    c-win:TITLE = lv-title.
    APPLY "entry" TO begin_cust-no.
    
    IF tb_excel:SCREEN-VALUE = "Yes" THEN
       ASSIGN
         tb_est-count:SENSITIVE = YES
         tb_est-pallets:SENSITIVE = YES.
    ELSE
       ASSIGN
          tb_est-count:SENSITIVE = NO
          tb_est-pallets:SENSITIVE = NO.
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-tt C-Win 
PROCEDURE build-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-date AS DATE NO-UNDO.
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.

  DEF BUFFER b-ar-invl FOR ar-invl.
  DEF BUFFER b-inv-head FOR inv-head.
  DEF BUFFER b-inv-line FOR inv-line.
  DEF BUFFER b-oe-rell FOR oe-rell.
  DEF BUFFER bf-oe-boll FOR oe-boll.

  DEF VAR v-po-no LIKE oe-ord.po-no NO-UNDO.
  DEF VAR dShipQty AS DECIMAL NO-UNDO.
  
  v-po-no = oe-ordl.po-no.

  CREATE tt-report.

  FIND FIRST itemfg
      WHERE itemfg.company EQ oe-ordl.company
        AND itemfg.i-no    EQ oe-ordl.i-no
      NO-LOCK NO-ERROR.
  IF AVAIL itemfg THEN tt-report.cad-no = itemfg.cad-no.

  IF tt-report.cad-no EQ "" THEN DO:
    RELEASE eb.
    IF TRIM(oe-ordl.est-no) NE "" THEN
    FIND FIRST eb
        WHERE eb.company  EQ oe-ordl.company
          AND eb.est-no   EQ oe-ordl.est-no
          AND eb.stock-no EQ oe-ordl.i-no
          AND eb.cad-no   NE ""
        USE-INDEX est-no NO-LOCK NO-ERROR.
    IF NOT AVAIL eb THEN
    FIND FIRST eb
        WHERE eb.company  EQ oe-ordl.company
          AND eb.stock-no EQ oe-ordl.i-no
          AND eb.cad-no   NE ""
        USE-INDEX stock NO-LOCK NO-ERROR.
    IF AVAIL eb THEN tt-report.cad-no = eb.cad-no.
  END.

  RELEASE eb.

  IF TRIM(oe-ordl.est-no) NE "" THEN
  DO:
     FIND FIRST eb WHERE
          eb.company  EQ oe-ordl.company AND
          eb.est-no   EQ oe-ordl.est-no AND
          eb.stock-no EQ oe-ordl.i-no AND
          eb.form-no  EQ oe-ordl.form-no AND
          eb.blank-no EQ oe-ordl.blank-no
          NO-LOCK NO-ERROR.

     IF AVAIL eb THEN
     DO:
        ASSIGN
          tt-report.unit-count = eb.cas-cnt
          tt-report.units-pallet = eb.cas-pal.
        RELEASE eb.
     END.
  END.

  ASSIGN
   tt-report.term-id  = ""
   tt-report.key-01   = IF rd_sort-1 EQ "Due Date" THEN
                          STRING(YEAR(lv-due-date),"9999") +
                          STRING(MONTH(lv-due-date),"99")  +
                          STRING(DAY(lv-due-date),"99")
                        ELSE
                        IF rd_sort-1 EQ "Salesman" THEN
                          oe-ordl.s-man[1]         ELSE ""
   tt-report.key-02   = oe-ord.cust-no
   tt-report.key-03   = IF v-sort EQ "PO" THEN v-po-no
                        ELSE 
                        IF v-sort EQ "It" THEN
                          (STRING(oe-ordl.i-no,"x(15)") + v-po-no)
                        ELSE
                        IF v-sort EQ "Cu" THEN
                          (STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE
                        IF v-sort EQ "FG" THEN
                          (STRING(oe-ordl.i-name,"x(30)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE
                        IF v-sort EQ "Or" THEN
                          (STRING(oe-ord.ord-no,"99999999999") + oe-ordl.part-no)
                        ELSE
                        IF v-sort EQ "CA" THEN
                          (STRING(tt-report.cad-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE
                          (STRING(YEAR(lv-due-date),"9999") +
                           STRING(MONTH(lv-due-date),"99")  +
                           STRING(DAY(lv-due-date),"99")    +
                           STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))              
   tt-report.key-04   = FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                        TRIM(oe-ordl.job-no) + "-" +
                        STRING(oe-ordl.job-no2,"99")
   tt-report.key-05   = STRING(oe-ord.ord-no,"99999999999")
   tt-report.key-06   = oe-ordl.i-no
   tt-report.key-07   = STRING(YEAR(ip-date),"9999") +
                        STRING(MONTH(ip-date),"99")  +
                        STRING(DAY(ip-date),"99")
   tt-report.po-no    = v-po-no
   tt-report.rec-id   = ip-recid
   tt-report.row-id   = ROWID(oe-ordl)
   tt-report.due-date = lv-due-date
   v-ordl             = NO.

  FIND b-ar-invl WHERE RECID(b-ar-invl) EQ ip-recid NO-LOCK NO-ERROR.
  IF AVAIL b-ar-invl THEN
    ASSIGN
     tt-report.q-shp  = b-ar-invl.ship-qty
     tt-report.inv    = YES
     tt-report.inv-no = b-ar-invl.inv-no.

  FIND b-inv-line WHERE RECID(b-inv-line) EQ ip-recid NO-LOCK NO-ERROR.
  IF AVAIL b-inv-line THEN DO:
    FIND FIRST b-inv-head WHERE b-inv-head.r-no EQ b-inv-line.r-no NO-LOCK.
    ASSIGN
     tt-report.q-shp  = b-inv-line.ship-qty
     tt-report.inv    = YES
     tt-report.inv-no = b-inv-head.inv-no.
  END.

  FIND b-oe-rell WHERE RECID(b-oe-rell) EQ ip-recid NO-LOCK NO-ERROR.
  IF NOT tt-report.inv THEN DO:
/*     dShipQty = 0.                                             */
/*     FOR EACH bf-oe-boll                                       */
/*               WHERE bf-oe-boll.company  EQ b-oe-rell.company  */
/*                 AND bf-oe-boll.ord-no   EQ b-oe-rell.ord-no   */
/*                 AND bf-oe-boll.line     EQ b-oe-rell.line     */
/*                 AND bf-oe-boll.i-no     EQ b-oe-rell.i-no     */
/*                 AND bf-oe-boll.r-no     EQ b-oe-rell.r-no     */
/*                 AND bf-oe-boll.rel-no   EQ b-oe-rell.rel-no   */
/*                 AND bf-oe-boll.b-ord-no EQ b-oe-rell.b-ord-no */
/*                 AND bf-oe-boll.po-no    EQ b-oe-rell.po-no    */
/*              NO-LOCK:                                         */
/*         dShipQty = dShipQty + bf-oe-boll.qty.                 */
/*     END.                                                      */
/*     IF dShipQty > 0 THEN                                      */
/*         tt-report.q-rel = dShipQty.                           */
/*     ELSE                                                      */
        IF AVAIL b-oe-rell   THEN tt-report.q-rel = b-oe-rell.qty.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-qoh C-Win 
PROCEDURE calc-qoh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR vdat          AS   DATE.
  DEF VAR v-curr        AS   LOG.
  DEF VAR v-q-or-v      AS   LOG.

  DEF VAR v-qohj        AS   DEC                EXTENT 6.
  DEF VAR v-qohi        LIKE v-qohj.
  DEF VAR v-qty         AS   INT.
  DEF VAR v-qty1        LIKE v-qty.
  DEF VAR v-qtyc        LIKE v-qty.
  DEF VAR v-red         LIKE v-qty.
  DEF VAR v             AS   INT.
  DEF VAR v-val         AS   DEC                EXTENT 4.
  DEF VAR v-cst         AS   DEC                EXTENT 4.
  DEF VAR v-u-val       AS   DEC.
  DEF VAR v-u-cst       AS   DEC.
  DEF VAR v-date        AS   DATE.

  DEF BUFFER b-f-rc for fg-rcpth.
  DEF BUFFER b-f-rd for fg-rdtlh.


  ASSIGN
   vdat     = TODAY
   v-curr   = YES
   v-q-or-v = YES.
  
  FOR EACH itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ oe-ordl.i-no
      NO-LOCK,
      EACH fg-bin
      WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
      NO-LOCK:

    CREATE tt-fg-bin.
    BUFFER-COPY fg-bin EXCEPT rec_key TO tt-fg-bin.
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
  DISPLAY begin_cust-no end_cust-no begin_ord-date end_ord-date begin_po-no 
          end_po-no begin_job-no begin_job-no2 end_job-no end_job-no2 begin_i-no 
          end_i-no begin_cad-no end_cad-no begin_due-date end_due-date 
          begin_userid end_userid begin_slsmn end_slsmn lbl_sort-1 rd_sort-1 
          lbl_sort rd_sort lbl_jstat rd_jstat tb_job-qty tb_0-bal lbl_ostat 
          rd_ostat tb_under tb_0-wip lbl_due-date rd_due-date tb_0-avl 
          lbl_wip-qty rd_wip-qty tb_job-qoh tb_est-count tb_est-pallets rd-dest 
          lv-ornt lv-font-no lines-per-page lv-font-name td-show-parm tb_batch 
          tb_excel tb_runExcel v-excel-file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_cust-no end_cust-no begin_ord-date end_ord-date begin_po-no 
         end_po-no begin_job-no begin_job-no2 end_job-no end_job-no2 begin_i-no 
         end_i-no begin_cad-no end_cad-no begin_due-date end_due-date 
         begin_userid end_userid begin_slsmn end_slsmn rd_sort-1 rd_sort 
         rd_jstat tb_job-qty tb_0-bal rd_ostat tb_under tb_0-wip rd_due-date 
         tb_0-avl rd_wip-qty tb_job-qoh rd-dest lv-ornt lv-font-no 
         lines-per-page td-show-parm tb_batch tb_excel tb_runExcel v-excel-file 
         btn-ok btn-cancel RECT-39 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-batch C-Win 
PROCEDURE run-batch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {BATCH/runbatch.i "oerep\s-ordopn.r"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ oe/rep/backlog.p 10/94 gb */
/* Order Backlog Summary / Detail Report                                      */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

DEF BUFFER b-tt-report FOR tt-report.
DEF BUFFER b-oe-rell FOR oe-rell.

def var v-cust  like oe-ord.cust-no  extent 2 init ["","zzzzzzzz"].
def var v-date  like ar-inv.inv-date format "99/99/9999"
                                     extent 2 init [today, 12/31/9999].
def var v-po    like oe-relh.po-no   extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-job   like oe-ord.job-no   extent 2 init ["","zzzzzz"].
def var v-job2  like oe-ord.job-no2  format "99" extent 2 init [0,99].
def var v-item  like oe-ordl.i-no    extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-inc   as   log             format "Yes/No" init yes.
def var v-stat  as   char format "!" init "A".
def var v-ostat as   char format "!" init "A".
def var v-jobq  as   log             format "Yes/No" init no.

def var v-dat     as   date format "99/99/99".
def var v-bal-qty as   int format ">>>,>>>,>>9".
def var v-inv-amt as   int format ">>>>,>>9.99".
def var v-cust-no like cust.cust-no.
def var v-name    like cust.name.
def var v-sman    like sman.sman.
def var v-sname   like sman.sname.
def var v-field1  as   char format "x(36)".
def var v-ord-no  as   char.
def var v-q-onh   like itemfg.q-onh NO-UNDO.
def var v-q-shp   like v-q-onh NO-UNDO.
def var v-q-rel   like v-q-onh NO-UNDO.
def var v-q-wip   like v-q-onh NO-UNDO.
def var v-q-avl   like v-q-onh NO-UNDO.
DEF VAR lv-slsmn  AS CHAR NO-UNDO.

def var v-time as int.
v-time = time.
DEF VAR v-comma AS cha FORM "x" INIT "," NO-UNDO.
DEF VAR lv-job-qty AS DEC NO-UNDO.
DEF VAR lv-rec-qty AS DEC NO-UNDO.

FORMAT HEADER
       SKIP(1)
       "Sales Rep:"
       lv-slsmn
       
    WITH FRAME r-top2 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP STREAM-IO WIDTH 200.

FORMAT HEADER
       SKIP(1)
       "Cust#   "
       "Due Date  "
       "Cust Part#     "
       "Item Description              "
       "FG Item #      "
       "Order#"
       "CAD#         "
       "PO#       "
       " Order Qty"
       "Qty OnHand"
       "Qty Shippd"
       "Qty ActRel"
       "   Qty WIP"
       " Qty Avail"
       SKIP
       "--------"
       "----------"
       "---------------"
       "------------------------------"
       "---------------"
       "------"
       "-------------"
       "----------"
       "----------"
       "----------"
       "----------"
       "----------"
       "----------"
       "----------"
       
    WITH FRAME r-top3 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP STREAM-IO WIDTH 200.


SESSION:SET-WAIT-STATE ("general").

ASSIGN 
 str-tit2 = c-win:TITLE
 str-tit3 = (IF rd_sort-1 EQ "Customer#" THEN ""
                                         ELSE "By " + TRIM(rd_sort-1) + " ") +
            "By Customer# " +
            "By " + TRIM(rd_sort)
 {sys/inc/ctrtext.i str-tit2 112}
 {sys/inc/ctrtext.i str-tit3 132}

 v-cust[1]  = begin_cust-no
 v-cust[2]  = end_cust-no
 v-date[1]  = begin_ord-date
 v-date[2]  = end_ord-date
 v-po[1]    = begin_po-no
 v-po[2]    = end_po-no
 v-job[1]   = fill(" ",6 - length(trim(begin_job-no))) +
              trim(begin_job-no) + string(int(begin_job-no2),"99")
 v-job[2]   = fill(" ",6 - length(trim(end_job-no)))   +
              trim(end_job-no)   + string(int(end_job-no2),"99")
 v-item[1]  = begin_i-no
 v-item[2]  = end_i-no
 v-sort     = substr(rd_sort,1,2)
 v-inc      = tb_0-bal
 v-stat     = substr(rd_jstat,1,1)
 v-ostat    = substr(rd_ostat,1,1)
 v-jobq     = tb_job-qty.

{sys/inc/print1.i}

{sys/inc/outprint.i value(0)}

IF rd-dest = 5 THEN DO:
   IF lv-ornt = "L" THEN PUT "<OLANDSCAPE><PREVIEW>".                   /*<p7><CPI20>*/ 
   ELSE PUT "<PREVIEW>".
   PUT "<PDF-EXCLUDE=MS Mincho></PROGRESS><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><P7><ADJUST=LPI>" FORM "x(165)" SKIP.
END. 
OUTPUT CLOSE.

IF tb_excel THEN DO:
   OUTPUT STREAM st-excel TO VALUE(v-excel-file).
   PUT STREAM st-excel
       "Cust#   ," 
       "Due Date,"
       "Cust Part#,"
       "Item Description,"
       "FG Item #," 
       "Order#,"
       "CAD#,"
       "PO#,"
       " Order Qty,"
       "Qty OnHand,"
       "Qty Shippd,"
       "Qty ActRel,"
       "   Qty WIP,"
       " Qty Avail".

   IF tb_est-count THEN
      PUT STREAM st-excel ",Est Unit (Case) Count".

   IF tb_est-pallets THEN
      PUT STREAM st-excel ",Est Units/Pallet".

   PUT STREAM st-excel SKIP.
END.

{sys/inc/outprint.i "value(lines-per-page) append" }                                     /**/

IF td-show-parm THEN RUN show-param.

VIEW FRAME r-top.

EMPTY TEMP-TABLE tt-report.
EMPTY TEMP-TABLE tt-fg-bin.

{oerep/r-ordopn.i}

IF tb_excel THEN
DO:
  OUTPUT STREAM st-excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(v-excel-file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReplaceCommas C-Win 
FUNCTION ReplaceCommas RETURNS CHARACTER
  ( ipcString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   

  RETURN REPLACE(ipcString,","," ").
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

