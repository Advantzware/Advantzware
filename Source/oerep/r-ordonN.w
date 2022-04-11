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
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No.        */
     
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

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

{sys/ref/CustList.i NEW}

DEFINE VARIABLE v-program       AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-fax-file     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-report-title AS CHARACTER NO-UNDO.

DEFINE VARIABLE v-sort          AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-ordl          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE tb_sch          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lv-stat         AS CHARACTER NO-UNDO.

DEFINE VARIABLE lv-due-date  LIKE oe-ordl.req-date NO-UNDO.
DEFINE VARIABLE lv-due-date2 LIKE oe-ordl.req-date NO-UNDO.
DEFINE VARIABLE v-q-onh      LIKE itemfg.q-onh     NO-UNDO.


DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD q-onh        LIKE itemfg.q-onh
    FIELD q-shp        LIKE itemfg.q-onh
    FIELD q-rel        LIKE itemfg.q-onh
    FIELD q-wip        LIKE itemfg.q-onh
    FIELD q-avl        LIKE itemfg.q-onh
    FIELD po-no        LIKE oe-ord.po-no
    FIELD inv          AS   LOG
    FIELD inv-no       LIKE ar-invl.inv-no
    FIELD cad-no       LIKE itemfg.cad-no
    FIELD row-id       AS ROWID
    FIELD due-date     LIKE oe-ordl.req-date
    FIELD unit-count   LIKE eb.cas-cnt
    FIELD units-pallet LIKE eb.cas-pal
    FIELD job-no       AS CHARACTER
    FIELD die          LIKE eb.die-no
    FIELD styl         LIKE eb.style
    FIELD due-dt       AS DATE
    FIELD run-end-date AS DATE
    FIELD sht          AS CHARACTER
    FIELD prntd        AS CHARACTER
    FIELD die-cut      AS CHARACTER
    FIELD glue         AS CHARACTER
    FIELD cust-lot     LIKE oe-rel.lot-no
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


DEFINE VARIABLE ldummy             AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFieldType         AS character   NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER     NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS character   NO-UNDO.
DEFINE VARIABLE cColumnInit AS LOGICAL INIT YES   NO-UNDO.
DEFINE VARIABLE glCustListActive   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.
DEFINE VARIABLE cFileName   as CHARACTER          NO-UNDO .

ASSIGN cTextListToSelect = "Rep,Cust#,Line Due Dt,Rel Due Dt,Cust Part#,Item Description,FG Item #," +
                           "Order#,CAD#,PO#,Order Qty,Qty OnHand,Qty Shippd,Qty ActRel," +
                           "Qty WIP,Qty Avail,Unit,Pallet,Order Value,Ack Date,Order Start Date,Status,CSR," +
                           "JOB#,DIE#,DUE DATE,COMPLETION DATE,STYLE,SHEETED,PRINTED,DIE CUT,GLUED,Last Ship,Customer Lot #"
       cFieldListToSelect = "rep,cust,l-due-dt,r-due-dt,cust-prt,itm-dscr,fg-itm," +
                            "ord,cad,po,ord-qty,qty-oh,qty-shp,qty-act," +
                            "qty-wip,qty-avl,est-unt,est-palt,ord-value,ack-date,ord-date,status,csr," +
                            "job,die,due-dt,comp-dt,styl,sht,prntd,die-cut,glue,last-date,cust-lot"
       cFieldLength = "3,8,11,10,32,30,20," + "8,14,15,10,10,10,10," + "10,10,5,6,15,8,16,20,8," + "13,20,10,15,7,7,7,7,5,16,15"
       cFieldType = "c,c,c,c,c,c,c," + "c,c,c,i,i,i,i," + "i,i,i,i,i,c,c,c,c," + "c,c,c,c,c,c,c,c,c,c,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Cust#,Line Due Dt,Rel Due Dt,Cust Part#,Item Description,FG Item #," +
                           "Order#,CAD#,PO#,Order Qty,Qty OnHand,Qty Shippd,Qty ActRel," +
                           "Qty WIP,Qty Avail" .

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
rd_sort rd_jstat tb_job-qty tb_0-bal rd_ostat tb_under tb_0-wip tb_0-avl ~
rd_wip-qty tb_job-qoh tb_itm-act btn_SelectColumns rd-dest td-show-parm ~
tb_batch tb_OpenCSV fi_file btn-ok btn-cancel tb_cust-list ~
btnCustList tbAutoClose RECT-40 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_ord-date ~
end_ord-date begin_po-no end_po-no begin_job-no begin_job-no2 end_job-no ~
end_job-no2 begin_i-no end_i-no begin_cad-no end_cad-no begin_due-date ~
end_due-date begin_userid end_userid begin_slsmn end_slsmn lbl_sort-1 ~
rd_sort-1 lbl_sort rd_sort lbl_jstat rd_jstat tb_job-qty tb_0-bal lbl_ostat ~
rd_ostat tb_under tb_0-wip tb_0-avl lbl_wip-qty rd_wip-qty tb_job-qoh ~
tb_itm-act rd-dest td-show-parm tb_batch tb_OpenCSV fi_file ~
tb_cust-list tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
     SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 16 BY 1.29.

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

DEFINE BUTTON Btn_Add 
     LABEL "&Add >>" 
     SIZE 16 BY 1.

DEFINE BUTTON btn_down 
     LABEL "Move Down" 
     SIZE 16 BY 1.

DEFINE BUTTON Btn_Remove 
     LABEL "<< &Remove" 
     SIZE 16 BY 1.

DEFINE BUTTON btn_SelectColumns 
     LABEL "Select Columns" 
     SIZE 40.4 BY 1.48.

DEFINE BUTTON btn_Up 
     LABEL "Move Up" 
     SIZE 16 BY 1.

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

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(9)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-999":U INITIAL "000" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5.5 BY 1 NO-UNDO.

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

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(9)":U INITIAL "zzzzzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-999":U INITIAL "999" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5.5 BY 1 NO-UNDO.

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

DEFINE VARIABLE lbl_jstat AS CHARACTER FORMAT "X(256)":U INITIAL "Job Status?" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE lbl_ostat AS CHARACTER FORMAT "X(256)":U INITIAL "Order Status?" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Secondary Sort?" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE lbl_sort-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Primary Sort?" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE lbl_wip-qty AS CHARACTER FORMAT "X(256)":U INITIAL "WIP Qty?" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81 NO-UNDO.

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

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)":U INITIAL "c:~\tmp~\r-ordopn.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE
     SIZE 43 BY 1 NO-UNDO.

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
"To Email", 5,
"To CSV", 3
     SIZE 16 BY 3.81 NO-UNDO.

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
"Line Due Date", "Due Date",
"Release Due Date", "Rel Date",
"Sales Rep", "Salesman"
     SIZE 75 BY .95 NO-UNDO.

DEFINE VARIABLE rd_wip-qty AS CHARACTER INITIAL "1" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Order Qty - OH - Ship", "1",
"Job Qty - Rcpts", "2"
     SIZE 44.4 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 115 BY 5.43.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 115 BY 4.52.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 115 BY 11.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 115 BY 2.33.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 27 BY .95 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_0-avl AS LOGICAL INITIAL yes 
     LABEL "Include 0 Qty / Act. Release Qty = 0" 
     VIEW-AS TOGGLE-BOX
     SIZE 39.2 BY 1 NO-UNDO.

DEFINE VARIABLE tb_0-bal AS LOGICAL INITIAL yes 
     LABEL "Include 0 Order Balance Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_0-wip AS LOGICAL INITIAL yes 
     LABEL "Include 0 Qty WIP Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_batch AS LOGICAL INITIAL no 
     LABEL "Run In Batch Mode?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.8 BY .71 NO-UNDO.

DEFINE VARIABLE tb_itm-act AS LOGICAL INITIAL no 
     LABEL "Include Inactive Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_job-qoh AS LOGICAL INITIAL no 
     LABEL "Include Jobs w/QOH?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_job-qty AS LOGICAL INITIAL no 
     LABEL "Print Job Qty Details?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.2 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV AS LOGICAL INITIAL no 
     LABEL "Open CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.8 BY .81
     BGCOLOR 15  NO-UNDO.

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
     begin_cust-no AT ROW 2.38 COL 33 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.38 COL 88 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_ord-date AT ROW 3.48 COL 33 COLON-ALIGNED
     end_ord-date AT ROW 3.48 COL 88 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     begin_po-no AT ROW 4.57 COL 33 COLON-ALIGNED HELP
          "Enter Ending Customer PO Number"
     end_po-no AT ROW 4.57 COL 88 COLON-ALIGNED HELP
          "Enter Ending Customer PO Number"
     begin_job-no AT ROW 5.67 COL 33 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 5.67 COL 48.4 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 5.67 COL 88 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 5.67 COL 103.4 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_i-no AT ROW 6.76 COL 33 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_i-no AT ROW 6.76 COL 88 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_cad-no AT ROW 7.86 COL 33 COLON-ALIGNED HELP
          "Enter Beginning CAD Number"
     end_cad-no AT ROW 7.86 COL 88 COLON-ALIGNED HELP
          "Enter Ending CAD Number"
     begin_due-date AT ROW 8.95 COL 33 COLON-ALIGNED
     end_due-date AT ROW 8.95 COL 88 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     begin_userid AT ROW 10.05 COL 33 COLON-ALIGNED HELP
          "Enter the Beginning User ID"
     end_userid AT ROW 10.05 COL 88 COLON-ALIGNED HELP
          "Enter the Ending User ID"
     begin_slsmn AT ROW 11.14 COL 33 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 11.14 COL 88 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     lbl_sort-1 AT ROW 13.14 COL 6.6 COLON-ALIGNED NO-LABEL
     rd_sort-1 AT ROW 13.05 COL 23.6 NO-LABEL
     lbl_sort AT ROW 14.05 COL 3.6 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 13.95 COL 23.6 NO-LABEL
     lbl_jstat AT ROW 16 COL 8.8 COLON-ALIGNED NO-LABEL
     rd_jstat AT ROW 15.86 COL 24 NO-LABEL
     tb_job-qty AT ROW 19 COL 24.2
     tb_0-bal AT ROW 16 COL 74
     lbl_ostat AT ROW 16.91 COL 6.8 COLON-ALIGNED NO-LABEL
     rd_ostat AT ROW 16.81 COL 24 NO-LABEL
     tb_under AT ROW 19.95 COL 24.2
     tb_0-wip AT ROW 16.95 COL 74
     tb_0-avl AT ROW 17.91 COL 74
     lbl_wip-qty AT ROW 17.81 COL 10.8 COLON-ALIGNED NO-LABEL
     rd_wip-qty AT ROW 17.81 COL 24 NO-LABEL
     tb_job-qoh AT ROW 18.86 COL 74
     tb_itm-act AT ROW 19.86 COL 74
     btn_SelectColumns AT ROW 21.24 COL 42 WIDGET-ID 10
     sl_avail AT ROW 23.57 COL 46.2 NO-LABEL WIDGET-ID 26
     Btn_Add AT ROW 23.67 COL 30.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     sl_selected AT ROW 23.43 COL 27.2 NO-LABEL WIDGET-ID 28
     Btn_Remove AT ROW 23.67 COL 43.2 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 23.67 COL 62.2 WIDGET-ID 40
     btn_down AT ROW 23.91 COL 77.2 WIDGET-ID 42
     rd-dest AT ROW 23.43 COL 6.2 NO-LABEL
     lv-ornt AT ROW 23.57 COL 30.4 NO-LABEL
     lv-font-no AT ROW 23.52 COL 64 COLON-ALIGNED
     lines-per-page AT ROW 23.52 COL 92 COLON-ALIGNED
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119.6 BY 29.19
         BGCOLOR 15 .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
     lv-font-name AT ROW 23.67 COL 30.2 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 25.33 COL 44.2
     tb_batch AT ROW 24.62 COL 44.2
     tb_OpenCSV AT ROW 26.29 COL 88 RIGHT-ALIGNED
     fi_file AT ROW 26.19 COL 27.8 COLON-ALIGNED
     btn-ok AT ROW 28.62 COL 41
     btn-cancel AT ROW 28.62 COL 65.8
     tb_cust-list AT ROW 1.62 COL 37.6 WIDGET-ID 6
     btnCustList AT ROW 1.57 COL 71.2 WIDGET-ID 8
     tbAutoClose AT ROW 27.81 COL 41.6 WIDGET-ID 16
     " Sort By" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.48 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 23 BY .71 AT ROW 1.24 COL 4.4
          BGCOLOR 15 
     " Print Selections" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 15.24 COL 4.2
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 22.67 COL 4.4
     RECT-9 AT ROW 12.81 COL 3.2
     RECT-39 AT ROW 23 COL 3.2
     RECT-40 AT ROW 1.48 COL 3.2 WIDGET-ID 44
     RECT-10 AT ROW 15.57 COL 3.2 WIDGET-ID 46
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119.6 BY 29.19
         BGCOLOR 15 .


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
         HEIGHT             = 29.19
         WIDTH              = 119.6
         MAX-HEIGHT         = 29.19
         MAX-WIDTH          = 119.6
         VIRTUAL-HEIGHT     = 29.19
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

/* SETTINGS FOR BUTTON Btn_Add IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Add:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON btn_down IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       btn_down:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON Btn_Remove IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Remove:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON btn_Up IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       btn_Up:HIDDEN IN FRAME FRAME-A           = TRUE.

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

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lines-per-page:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-name:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-no:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-ornt:HIDDEN IN FRAME FRAME-A           = TRUE.

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

/* SETTINGS FOR RECTANGLE RECT-10 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-39 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-9 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST sl_avail IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       sl_avail:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_selected IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       sl_selected:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tb_0-avl:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_0-bal:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_0-wip:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_job-qoh:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_job-qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A     = 
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
  IF rd-dest EQ 3 THEN
  DO:
    ASSIGN fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
    RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
    fi_file:SCREEN-VALUE =  cFileName.
  END.
  ASSIGN
    lv-pdf-file = init-dir + "\OpnOrder"
    is-xprint-form = NO.

  IF g_batch THEN tb_batch = YES.
  IF v-prompt-excel AND rd-dest EQ 3 THEN DO:
     DEF VAR v-excel-file2 AS cha NO-UNDO.
     RUN oerep/d-ordexl.w (OUTPUT v-excel-file2).
     fi_file = fi_file + v-excel-file2 + ".csv".
     IF tb_batch THEN DISPLAY fi_file WITH FRAME {&FRAME-NAME}.
  END.

  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
      EMPTY TEMP-TABLE ttCustList.
      RUN BuildCustList(INPUT cocode,
                        INPUT tb_cust-list AND glCustListActive,
                        INPUT begin_cust-no,
                        INPUT END_cust-no).
  END.

  IF tb_batch THEN DO:
     RUN run-batch.
     RETURN NO-APPLY.
  END.

  SESSION:SET-WAIT-STATE("general").
  RUN GetSelectionList.
  run run-report. 

  STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE("").

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN DO:
           IF NOT tb_OpenCSV THEN DO:        
               MESSAGE  "CSV file have been created." SKIP(1)
               "~"OK~" to open CSV file?"
               VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
               TITLE "" UPDATE lChoice AS LOGICAL.
               
               IF lChoice THEN
               DO:
                  OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)). 
               END.
           END.
           ELSE DO:
                  OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)). 
           END.
       END. /* WHEN 3 THEN DO: */
       
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &type= "Customer"
                            &begin_cust=begin_cust-no
                            &END_cust= begin_cust-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       WHEN 5 THEN DO:
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
  END CASE. 
  IF tbAutoClose:CHECKED THEN 
     APPLY 'CLOSE' TO THIS-PROCEDURE.
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


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

  APPLY "DEFAULT-ACTION" TO sl_avail.

  /*
  DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
    IF sl_avail:IS-SELECTED(i) AND
      (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR sl_selected:NUM-ITEMS = 0) THEN
    /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
        cSelectedList = cSelectedList +
                        entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
  END.
  cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
  sl_selected:LIST-ITEM-PAIRS = cSelectedList.
  sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
DO:
  RUN Move-Field ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove C-Win
ON CHOOSE OF Btn_Remove IN FRAME FRAME-A /* << Remove */
DO:
 /* DO i = sl_selected:NUM-ITEMS TO 1 BY -1 WITH FRAME {&FRAME-NAME}:
    IF sl_selected:IS-SELECTED(i) THEN
    ldummy = sl_selected:DELETE(i).
  END
  */
  APPLY "DEFAULT-ACTION" TO sl_selected  .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_SelectColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_SelectColumns C-Win
ON CHOOSE OF btn_SelectColumns IN FRAME FRAME-A /* Select Columns */
DO:
    DEF VAR cTextSelected AS cha NO-UNDO.
    DEF VAR cTextListed AS cha NO-UNDO.

    RUN displaySelectionList2.

    ASSIGN cTextSelected = sl_selected:LIST-ITEMS
           cTextListed = sl_avail:LIST-ITEMS.

    IF NOT cColumnInit THEN RUN custom/d-rptsel.w (INPUT-OUTPUT cTextListed, INPUT-OUTPUT cTextSelected, INPUT-OUTPUT cTextListToDefault, INPUT-OUTPUT cTextListToSelect).

    ASSIGN sl_selected:LIST-ITEMS = cTextSelected
           sl_avail:LIST-ITEMS = cTextListed.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
DO:
  RUN Move-Field ("Up").
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
  ASSIGN {&self-name}.
  RUN pChangeDest.
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


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail C-Win
ON DEFAULT-ACTION OF sl_avail IN FRAME FRAME-A
DO:

   IF (NOT CAN-DO(sl_selected:LIST-ITEMs,{&SELF-NAME}:SCREEN-VALUE) OR
       sl_selected:NUM-ITEMS = 0)
   THEN ASSIGN ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
               ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
              /* sl_selected:SCREEN-VALUE = sl_selected:ENTRY(sl_selected:NUM-ITEMS) */
               .


/* for pairs
    DEF VAR cSelectedList AS cha NO-UNDO.
    cSelectedList = sl_Selected:LIST-ITEM-PAIRS.
    DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
    IF sl_avail:IS-SELECTED(i) AND
      (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR
         sl_selected:NUM-ITEMS = 0) THEN
    /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
        cSelectedList = cSelectedList +
                        entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
    MESSAGE i sl_avail:IS-SELECTED(i) NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i))
        sl_selected:NUM-ITEMS
        SKIP cSelectedList
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
  sl_selected:LIST-ITEM-PAIRS = cSelectedList.
  sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected C-Win
ON DEFAULT-ACTION OF sl_selected IN FRAME FRAME-A
DO:
   DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
    IF {&SELF-NAME}:IS-SELECTED(i) THEN DO:
       ASSIGN ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
              ldummy = /*{&SELF-NAME}:DELETE(i)*/
                       {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
              .
    END.           
  END.
  IF {&SELF-NAME}:NUM-ITEMS NE 0 THEN
  ASSIGN
    {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(1)
    .


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



&Scoped-define SELF-NAME tb_itm-act
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_itm-act C-Win
ON VALUE-CHANGED OF tb_itm-act IN FRAME FRAME-A /* Include Inactive Items? */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
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


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Name */
DO:
    fi_file = ''.
 //  assign {&self-name}.
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
    btn_SelectColumns:load-image("Graphics/32x32/selectColumns.png").
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
  RUN enable_UI.
  {methods/nowait.i}
  {sys/inc/reportsConfigNK1.i "OR16" }
  ASSIGN
  td-show-parm:SENSITIVE = lShowParameters
  tb_batch:SENSITIVE = lShowBatchMode
  td-show-parm:HIDDEN = NOT lShowParameters
  tb_batch:HIDDEN = NOT lShowBatchMode
  td-show-parm:VISIBLE = lShowParameters
  tb_batch:VISIBLE = lShowBatchMode
  .

   RUN sys/inc/CustListForm.p ( "OR16",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    lv-title = c-win:TITLE.
    {custom/usrprint.i}
    c-win:TITLE = lv-title.
    APPLY "entry" TO begin_cust-no.
    APPLY 'choose' TO btn_SelectColumns IN FRAME {&FRAME-NAME}.
    cColumnInit = NO.

  END.

  RUN pChangeDest.
  
  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'OR16',
                          INPUT NO,
                          OUTPUT glCustListActive).

 {sys/inc/chblankcust.i ""OR16""}

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
          tt-report.unit-count   = eb.cas-cnt
          tt-report.units-pallet = eb.cas-pal
          tt-report.die          = eb.die-no
          tt-report.styl         = eb.style
          .
          
        RELEASE eb.
     END.
  END.

  ASSIGN
   tt-report.term-id  = ""
   tt-report.key-01   = IF rd_sort-1 EQ "Due Date" OR rd_sort-1 EQ "Rel Date" THEN
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
   tt-report.key-04   = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', oe-ordl.job-no, oe-ordl.job-no2))
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
                            INPUT 'OR16',
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-qoh C-Win 
PROCEDURE calc-qoh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  DEF VAR vdat          AS   DATE.                       */
/*  DEF VAR v-curr        AS   LOG.                        */
/*  DEF VAR v-q-or-v      AS   LOG.                        */
/*                                                         */
/*  DEF VAR v-qohj        AS   DEC                EXTENT 6.*/
/*  DEF VAR v-qohi        LIKE v-qohj.                     */
/*  DEF VAR v-qty         AS   INT.                        */
/*  DEF VAR v-qty1        LIKE v-qty.                      */
/*  DEF VAR v-qtyc        LIKE v-qty.                      */
/*  DEF VAR v-red         LIKE v-qty.                      */
/*  DEF VAR v             AS   INT.                        */
/*  DEF VAR v-val         AS   DEC                EXTENT 4.*/
/*  DEF VAR v-cst         AS   DEC                EXTENT 4.*/
/*  DEF VAR v-u-val       AS   DEC.                        */
/*  DEF VAR v-u-cst       AS   DEC.                        */
/*  DEF VAR v-date        AS   DATE.                       */
/*                                                         */
/*  DEF BUFFER b-f-rc for fg-rcpth.                        */
/*  DEF BUFFER b-f-rd for fg-rdtlh.                        */
/*                                                         */
/*                                                         */
/*  ASSIGN                                                 */
/*   vdat     = TODAY                                      */
/*   v-curr   = YES                                        */
/*   v-q-or-v = YES.                                       */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustList C-Win 
PROCEDURE CustList :
/*------------------------------------------------------------------------------
  Purpose:  Display a UI of selected customers   
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    RUN sys/ref/CustListManager.w(INPUT cocode,
                                  INPUT 'OR16').


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault C-Win 
PROCEDURE DisplaySelectionDefault :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToDefault):

     cListContents = cListContents +                   
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToDefault)   .
  END.            
  sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList C-Win 
PROCEDURE DisplaySelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

/*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:

     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    /* (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect) + "," +
                     ENTRY(1,cFieldListToSelect)
                     paris */

                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .
  END.

 /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */

  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 C-Win 
PROCEDURE DisplaySelectionList2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.
  
  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:
    RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    /* (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect) + "," +
                     ENTRY(1,cFieldListToSelect)
                     paris */

                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .
  END.

 /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */

  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
  END.

  {sys/ref/SelColCorrect.i}

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
          rd_ostat tb_under tb_0-wip tb_0-avl lbl_wip-qty rd_wip-qty tb_job-qoh 
          tb_itm-act rd-dest td-show-parm tb_batch tb_OpenCSV fi_file 
          tb_cust-list tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_cust-no end_cust-no begin_ord-date end_ord-date begin_po-no 
         end_po-no begin_job-no begin_job-no2 end_job-no end_job-no2 begin_i-no 
         end_i-no begin_cad-no end_cad-no begin_due-date end_due-date 
         begin_userid end_userid begin_slsmn end_slsmn rd_sort-1 rd_sort 
         rd_jstat tb_job-qty tb_0-bal rd_ostat tb_under tb_0-wip tb_0-avl 
         rd_wip-qty tb_job-qoh tb_itm-act btn_SelectColumns rd-dest 
         td-show-parm tb_batch tb_OpenCSV fi_file btn-ok btn-cancel 
         tb_cust-list btnCustList tbAutoClose RECT-40 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList C-Win 
PROCEDURE GetSelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR cTmpList AS cha NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
 iColumnLength = 0.

 DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     

    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
           ttRptSelected.DisplayOrder = i
           ttRptSelected.HeadingFromLeft = IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
           iColumnLength = iColumnLength + ttRptSelected.FieldLength + 1.
           .        

 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetVarValue C-Win 
PROCEDURE GetVarValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ipVarName AS cha NO-UNDO.
  DEF OUTPUT PARAM opVarValue AS cha NO-UNDO.

  opVarValue = ipVarName.

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

/*{sys/form/r-top3w.f}*/

DEF BUFFER b-tt-report FOR tt-report.
DEF BUFFER b-oe-rell FOR oe-rell.
DEF BUFFER b-job-mch FOR job-mch.

def var v-cust  like oe-ord.cust-no  extent 2 init ["","zzzzzzzz"].
def var v-date  like ar-inv.inv-date format "99/99/9999"
                                     extent 2 init [today, 12/31/9999].
def var v-po    like oe-relh.po-no   extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-job   like oe-ord.job-no   extent 2 init ["","zzzzzzzzz"].
def var v-job2  like oe-ord.job-no2  format "999" extent 2 init [0,999].
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
def var v-rs      as   char format "x(7)" NO-UNDO.
def var v-dc      as   char format "x(7)" NO-UNDO.
def var v-pr      as   char format "x(7)" NO-UNDO.
def var v-gl      as   char format "x(5)" NO-UNDO.

def var v-time as int.
v-time = time.
DEF VAR v-comma AS cha FORM "x" INIT "," NO-UNDO.
DEF VAR lv-job-qty AS DEC NO-UNDO.
DEF VAR lv-rec-qty AS DEC NO-UNDO.

DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-line AS cha FORM "x(300)" NO-UNDO.
DEFINE VARIABLE dOrdVal AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.

{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lSelected AS LOGICAL INIT YES NO-UNDO.
DEFINE VARIABLE lc-result AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
//DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

//RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

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
 v-job[1]   = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', begin_job-no, begin_job-no2))
 v-job[2]   = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', end_job-no, end_job-no2))
 v-item[1]  = begin_i-no
 v-item[2]  = end_i-no
 v-sort     = substr(rd_sort,1,2)
 v-inc      = tb_0-bal
 v-stat     = substr(rd_jstat,1,1)
 v-ostat    = substr(rd_ostat,1,1)
 v-jobq     = tb_job-qty
 lSelected  = tb_cust-list .

{sys/inc/print1.i}

{sys/inc/outprint.i value(0)}

DEF VAR cslist AS cha NO-UNDO.
 FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

   IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
   THEN ASSIGN str-tit4 = str-tit4 + ttRptSelected.TextList + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + "," .        
   ELSE 
   ASSIGN str-tit4 = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
          str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
          excelheader = excelHeader + ttRptSelected.TextList + ","
          .        
          cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

IF rd-dest = 5 THEN DO:
   IF lv-ornt = "L" THEN PUT "<OLANDSCAPE><PREVIEW>".                   /*<p7><CPI20>*/ 
   ELSE PUT "<PREVIEW>".
   PUT "<PDF-EXCLUDE=MS Mincho></PROGRESS><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><P7><ADJUST=LPI>" FORM "x(165)" SKIP.
END. 
OUTPUT CLOSE.

IF rd-dest EQ 3 THEN DO:
   OUTPUT STREAM st-excel TO VALUE(cFileName).
  /* PUT STREAM st-excel
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
       " Qty Avail".*/

  /* IF tb_est-count THEN
      PUT STREAM st-excel ",Est Unit (Case) Count".

   IF tb_est-pallets THEN
      PUT STREAM st-excel ",Est Units/Pallet". */

   PUT STREAM st-excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN v-cust[1] = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN v-cust[2] = ttCustList.cust-no .
END.

{sys/inc/outprint.i "value(lines-per-page) append" }                                     /**/

IF td-show-parm THEN RUN show-param.

VIEW FRAME r-top.

EMPTY TEMP-TABLE tt-report.
EMPTY TEMP-TABLE tt-fg-bin.

{oerep/r-ordonN.i}

IF rd-dest EQ 3 THEN
DO:
  OUTPUT STREAM st-excel CLOSE.
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChangeDest C-Win 
PROCEDURE pChangeDest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      IF rd-dest:SCREEN-VALUE EQ "3" THEN
       ASSIGN
        tb_OpenCSV:SCREEN-VALUE = "Yes"
        fi_file:SENSITIVE = YES
        tb_OpenCSV:SENSITIVE = YES       
       .
      ELSE 
        ASSIGN
        tb_OpenCSV:SCREEN-VALUE = "NO"
        fi_file:SENSITIVE = NO
        tb_OpenCSV:SENSITIVE = NO       
       .
     ASSIGN fi_file:SCREEN-VALUE = "c:\tmp\r-ordopn.csv".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*RETURN string(hField:BUFFER-VALUE, hField:FORMAT) */
  RETURN string(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

