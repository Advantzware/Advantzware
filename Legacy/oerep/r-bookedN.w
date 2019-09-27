&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-booked.w

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
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecdt.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}

ASSIGN 
 cocode = gcompany
 locode = gloc.

{sys/ref/CustList.i NEW}

DEFINE TEMP-TABLE w-data NO-UNDO 
  FIELD ord-no LIKE oe-ord.ord-no
  FIELD LINE LIKE oe-ordl.LINE 
  FIELD sman AS CHARACTER FORMAT "x(3)"
  FIELD item-n LIKE itemfg.i-name COLUMN-LABEL "Item Description"
        FORMAT "x(27)"
  FIELD procat LIKE itemfg.procat COLUMN-LABEL "Prod!Code"
  FIELD qty LIKE oe-ordl.qty COLUMN-LABEL "Quantity!Ordered/EA"
        FORMAT ">,>>>,>>>"
  FIELD sqft LIKE itemfg.t-sqft COLUMN-LABEL "Sq Ft" FORMAT ">>,>>>.999"
  FIELD t-sqft LIKE itemfg.t-sqft COLUMN-LABEL "Total!Sq Ft/M" FORMAT "->,>>>.999"
  FIELD t-tons AS DECIMAL COLUMN-LABEL "Total!  Tons" FORMAT "->,>>>.9"
  FIELD price LIKE oe-ordl.price FORMAT ">>>,>>9.99<<<<"
  FIELD revenue LIKE oe-ordl.t-price COLUMN-LABEL "Order!Amount"
  FIELD misc AS LOGICAL
  FIELD cost AS DECIMAL
  FIELD comm AS DECIMAL LABEL "Comm %"
  FIELD margin AS DECIMAL
  FIELD shp-qty LIKE oe-ordl.ship-qty  
  FIELD cShip-from LIKE oe-rel.spare-char-1  .

DEFINE TEMP-TABLE wkrecap NO-UNDO     /* recap by product category */
  FIELD procat LIKE itemfg.procat COLUMN-LABEL "Cat"
  FIELD t-sqft LIKE itemfg.t-sqft  EXTENT 2 COLUMN-LABEL "Sq Ft" FORMAT ">>,>>>.999"
  FIELD t-tons AS DECIMAL COLUMN-LABEL "Tons" EXTENT 2 FORMAT "->,>>>.9"
  FIELD revenue LIKE oe-ordl.t-price   EXTENT 2 COLUMN-LABEL "Amount"
  FIELD price-per-m  AS DECIMAL COLUMN-LABEL "$/MSF" EXTENT 2
  FIELD price-per-t  AS DECIMAL COLUMN-LABEL "$/TON" EXTENT 2
  FIELD num-of-ord AS INTEGER COLUMN-LABEL "#Orders".

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

DEFINE VARIABLE v-print-fmt AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
DEFINE VARIABLE security-flag AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-code AS CHARACTER NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg.

DEF STREAM excel.

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE cTextListToSelect AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength AS INTEGER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

/*
(IF {sys/inc/rptDisp.i "oe-ord.due-date"} THEN "DUE DATE " ELSE "" ) +   8
    (IF {sys/inc/rptDisp.i "w-data.ord-no"} THEN     "ORDER#         " ELSE "" ) +  14
    (IF {sys/inc/rptDisp.i "cust.name"} THEN    "CUSTOMER NAME " ELSE "" ) + 13
    (IF {sys/inc/rptDisp.i "w-data.comm"} THEN  "COMM % " ELSE "" ) +    6
    (IF {sys/inc/rptDisp.i "w-data.procat"} THEN  "PROD!Code " ELSE "" ) +    5

    (IF {sys/inc/rptDisp.i "w-data.item-n"} THEN "ITEM DESCRIPTION " ELSE "")  +  16
    (IF {sys/inc/rptDisp.i "w-data.qty"} THEN    "QTY ORDERED/EA " ELSE "" ) +  14
    (IF {sys/inc/rptDisp.i "w-data.sqft"} THEN "Sq Ft " ELSE "" ) +    10
    (IF {sys/inc/rptDisp.i "w-data.t-sqft"} THEN "Total!Sq Ft/M " ELSE "" ) +    10

    (IF {sys/inc/rptDisp.i "v-price-per-m"} THEN "$/MSF    " ELSE "" ) + 10
    (IF {sys/inc/rptDisp.i "w-data.price"} THEN "PRICE " ELSE "" ) + 10
    (IF {sys/inc/rptDisp.i "v-revenue"} THEN     ""Order!Amount      " ELSE "" ) +  13
    (IF {sys/inc/rptDisp.i "v-margin"} THEN     ""% Margin      " ELSE "" ) +  9
    (IF {sys/inc/rptDisp.i "v-profit"}  THEN     "% Profit " ELSE "" ) +  9
    (IF {sys/inc/rptDisp.i "w-data.t-tons"}  THEN "Total!  Ton   " ELSE "" ) + 8
    (IF {sys/inc/rptDisp.i "v-price-per-t"}  THEN "$/TON      " ELSE "" ) +  10    

    (IF {sys/inc/rptDisp.i "oe-ordl.part-no"}  THEN "Customer Part#" ELSE "" ) +  15    
  */
ASSIGN cTextListToSelect  = "DUE DATE,ORDER#,CUSTOMER,CUSTOMER NAME,PROD CODE," +
                            "FG ITEM NAME,QTY ORDERED/EA,SQ FT,TOTAL Sq Ft/M," +
                            "$/MSF,PRICE,ORDER AMOUNT,% PROFIT,TOTAL TONS,$/TON," +
                            "FG ITEM#,ID,CUSTOMER PART#,CUSTOMER PO#,DIE#,ORDER DATE,COMM %,SHIPPED QTY,CSR,ACK. DATE," +
                            "UOM,SHIP FROM,MACHINE,INKS,PRINT SHEET#,COST/$M,TOTAL STD COST,FULL COST,ENTERED BY,STATUS,PO RECEIVED,PREV ORDER#," +
                            "APPROVED DATE"
       cFieldListToSelect = "oe-ord.due-date,w-data.ord-no,cust.cust-no,cust.name,w-data.procat," +
                            "w-data.item-n,w-data.qty,w-data.sqft,t-sqft," +
                            "v-price-per-m,price,v-revenue,v-profit,t-tons,v-price-per-t," +
                            "oe-ordl.i-no,oe-ord.user-id,oe-ordl.part-no,cust-po,die-no,oe-ord.ord-date,v-net-prct,w-data.shp-qty,csrUser_id,ack-date," +
                            "oe-ordl.pr-uom,Ship-from,v-mach,v-ink,print-sheet,v-cost,v-t-cost,full-cost,oe-ord.entered-id,status,po-recvdt,prev-order," +
                            "approved-date"

       cFieldLength = "8,14,8,13,9," + "16,14,10,13," + "10,10,13,9,10,14," + "15,8,15,15,15,10,7,14,8,10," + "6,9,30,40,20,14,14,14,10,20,11,11," + "13"
       .

{sys/inc/ttRptSel.i}
 ASSIGN cTextListToDefault  = "DUE DATE,ORDER#,CUST#,CUSTOMER NAME,COMM %,PROD CODE," +
                              "QTY ORDERED/EA,CUSTOMER PART#,FG ITEM NAME,SQ FT,TOTAL Sq Ft/M," +
                              "$/MSF,PRICE,ORDER AMOUNT,% PROFIT,TOTAL TONS,$/TON,SHIPPED QTY" .
                              /* "FG ITEM#,ID" */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 tb_cust-list btnCustList ~
begin_cust-no end_cust-no begin_ord-date end_ord-date begin_due-date ~
end_due-date begin_slsmn end_slsmn begin_fg-cat end_fg-cat begin_shipfrom ~
end_shipfrom tb_prepmisc tb_smn-no tb_exclude-set-comps tb_rep-tot ~
tb_exclude-transfer tb_include-ordrel tb_Under% tb_Over% Btn_Def sl_avail ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
tb_batch btn-ok btn-cancel begin_cust-part end_cust-part
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust-no end_cust-no ~
begin_ord-date end_ord-date lbl_sqft begin_due-date end_due-date ~
begin_slsmn end_slsmn begin_fg-cat end_fg-cat begin_shipfrom end_shipfrom ~
tb_prepmisc tb_smn-no tb_exclude-set-comps tb_rep-tot tb_exclude-transfer ~
tb_include-ordrel tb_Under% fUnder% fOver% tb_Over% sl_avail sl_selected ~
rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm ~
tb_excel tb_runExcel fi_file tb_batch begin_cust-part end_cust-part

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetInksForJob C-Win 
FUNCTION fGetInksForJob RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetRoutingForJob C-Win 
FUNCTION fGetRoutingForJob RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE BUTTON Btn_Add 
     LABEL "&Add >>" 
     SIZE 16 BY 1.

DEFINE BUTTON Btn_Def 
     LABEL "&Default" 
     SIZE 16 BY 1.

DEFINE BUTTON btn_down 
     LABEL "Move Down" 
     SIZE 16 BY 1.

DEFINE BUTTON Btn_Remove 
     LABEL "<< &Remove" 
     SIZE 16 BY 1.

DEFINE BUTTON btn_Up 
     LABEL "Move Up" 
     SIZE 16 BY 1.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_cust-part AS CHARACTER FORMAT "X(15)" 
     LABEL "Beginning Customer Part#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_due-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Due Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_fg-cat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Product Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_shipfrom AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Ship From WH" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning SalesRep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-part AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Customer Part#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_due-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Due Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_fg-cat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Product Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_shipfrom AS CHARACTER FORMAT "X(5)":U INITIAL "zzzz" 
     LABEL "Ending Ship From WH" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending SalesRep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-booked.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE fOver% AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.2 BY 1 NO-UNDO.

DEFINE VARIABLE fUnder% AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.2 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sqft AS CHARACTER FORMAT "X(256)":U INITIAL "Print SqFt or Part#?" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .95 NO-UNDO.

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

DEFINE VARIABLE rd_sqft AS CHARACTER INITIAL "Square Ft" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Square Ft", "Square Ft",
"Part#", "Part#"
     SIZE 26 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 13.57.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.05.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5.24 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 35 BY 5.24 NO-UNDO.

DEFINE VARIABLE tb_batch AS LOGICAL INITIAL no 
     LABEL "Run In Batch Mode?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE tb_comm AS LOGICAL INITIAL yes 
     LABEL "Print Commission?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.8 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_exclude-set-comps AS LOGICAL INITIAL no 
     LABEL "Exclude Set Components" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE tb_exclude-transfer AS LOGICAL INITIAL no 
     LABEL "Exclude Transfer Releases/Orders" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .95 NO-UNDO.

DEFINE VARIABLE tb_include-ordrel AS LOGICAL INITIAL no 
     LABEL "Include Orders with no Release?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .95 NO-UNDO.

DEFINE VARIABLE tb_margin AS LOGICAL INITIAL no 
     LABEL "Print Avail Margin?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE tb_Over% AS LOGICAL INITIAL no 
     LABEL "Print Order Over(%) +" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prepmisc AS LOGICAL INITIAL no 
     LABEL "Include Prep / Misc Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .95 NO-UNDO.

DEFINE VARIABLE tb_prft AS LOGICAL INITIAL yes 
     LABEL "Print Profit?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE tb_rep-tot AS LOGICAL INITIAL no 
     LABEL "Rep Sub Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_smn-no AS LOGICAL INITIAL no 
     LABEL "Page By SalesRep?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE tb_sortby AS LOGICAL INITIAL no 
     LABEL "Sort by Order#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE tb_ton AS LOGICAL INITIAL no 
     LABEL "Print $/Ton?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE tb_Under% AS LOGICAL INITIAL no 
     LABEL "Print Order Under(%) -" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_cust-list AT ROW 1.43 COL 31.8 WIDGET-ID 162
     btnCustList AT ROW 1.52 COL 65.4 WIDGET-ID 8
     begin_cust-no AT ROW 2.43 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.43 COL 73 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_ord-date AT ROW 3.38 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Order Date"
     end_ord-date AT ROW 3.38 COL 73 COLON-ALIGNED HELP
          "Enter Ending Order Date"
     lbl_sqft AT ROW 3.86 COL 103 COLON-ALIGNED NO-LABEL
     begin_due-date AT ROW 4.33 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Order Date" WIDGET-ID 164
     end_due-date AT ROW 4.33 COL 73 COLON-ALIGNED HELP
          "Enter Ending Order Date" WIDGET-ID 166
     rd_sqft AT ROW 4.57 COL 122 NO-LABEL
     begin_slsmn AT ROW 5.29 COL 30 COLON-ALIGNED HELP
          "Enter Beginning SalesRep Number"
     end_slsmn AT ROW 5.29 COL 73 COLON-ALIGNED HELP
          "Enter Ending SalesRep Number"
     tb_prft AT ROW 5.76 COL 116
     begin_fg-cat AT ROW 6.24 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Product Category"
     end_fg-cat AT ROW 6.24 COL 73 COLON-ALIGNED HELP
          "Enter Ending Product Category"
     tb_ton AT ROW 6.95 COL 114
     begin_shipfrom AT ROW 7.19 COL 30 COLON-ALIGNED HELP
          "Enter starting ship from location." WIDGET-ID 158
     end_shipfrom AT ROW 7.19 COL 73 COLON-ALIGNED HELP
          "Enter ending ship from location." WIDGET-ID 160
     begin_cust-part AT ROW 8.05 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Customer Part Number"
     end_cust-part AT ROW 8.05 COL 73 COLON-ALIGNED HELP
          "Enter Ending Customer part Number"
     tb_sortby AT ROW 8.38 COL 122
     tb_prepmisc AT ROW 9.18 COL 15
     tb_smn-no AT ROW 9.14 COL 58
     tb_exclude-set-comps AT ROW 9.98 COL 15 WIDGET-ID 4
     tb_rep-tot AT ROW 9.95 COL 58 WIDGET-ID 54
     tb_exclude-transfer AT ROW 10.76 COL 15 WIDGET-ID 6
     tb_include-ordrel AT ROW 10.77 COL 58 WIDGET-ID 6
     tb_comm AT ROW 11.24 COL 118
     tb_Under% AT ROW 11.62 COL 15 WIDGET-ID 46
     fUnder% AT ROW 11.62 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     fOver% AT ROW 11.62 COL 73.8 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     tb_Over% AT ROW 11.67 COL 52 WIDGET-ID 52
     tb_margin AT ROW 13.86 COL 119 WIDGET-ID 2
     Btn_Def AT ROW 15.29 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_avail AT ROW 15.62 COL 4 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 15.62 COL 60 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 16.48 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 17.67 COL 40 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 18.86 COL 40 WIDGET-ID 40
     btn_down AT ROW 20.05 COL 40 WIDGET-ID 42
     rd-dest AT ROW 22.1 COL 5 NO-LABEL
     lv-ornt AT ROW 22.81 COL 31 NO-LABEL
     lines-per-page AT ROW 22.81 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 24.71 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 25.67 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 26.86 COL 30
     tb_excel AT ROW 28.05 COL 72 RIGHT-ALIGNED
     tb_runExcel AT ROW 28.05 COL 93 RIGHT-ALIGNED
     fi_file AT ROW 28.86 COL 49 COLON-ALIGNED HELP
          "Enter File Name"
     tb_batch AT ROW 29 COL 6
     btn-ok AT ROW 30.33 COL 26
     btn-cancel AT ROW 30.33 COL 56
     "Note: Profit Includes Estimate Markups and Commissions." VIEW-AS TEXT
          SIZE 55 BY .95 AT ROW 13.43 COL 17
          FGCOLOR 1 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 151.4 BY 31.38.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 14.91 COL 60.2 WIDGET-ID 44
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 14.91 COL 4.4 WIDGET-ID 38
     "(Prep / Misc Charges will Display 'P' or 'M' for Product Code)" VIEW-AS TEXT
          SIZE 57 BY .95 AT ROW 12.67 COL 15
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 21.38 COL 2
     RECT-7 AT ROW 1 COL 1
     RECT-8 AT ROW 21.14 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 151.4 BY 31.38.


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
         TITLE              = "Orders Booked"
         HEIGHT             = 31.38
         WIDTH              = 95
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
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_fg-cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_shipfrom:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-part:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_fg-cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_shipfrom:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-part:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN fOver% IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fUnder% IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_sqft IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sqft:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sqft".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rd_sqft IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rd_sqft:HIDDEN IN FRAME FRAME-A           = TRUE
       rd_sqft:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_comm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_comm:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_comm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_exclude-set-comps:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_exclude-transfer:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_include-ordrel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_margin IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_margin:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_margin:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prepmisc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prft IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_prft:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_prft:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_rep-tot:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_smn-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_sortby IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_sortby:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_sortby:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_ton IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_ton:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_ton:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Orders Booked */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Orders Booked */
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
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_due-date C-Win
ON LEAVE OF begin_due-date IN FRAME FRAME-A /* Beginning Due Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_fg-cat C-Win
ON LEAVE OF begin_fg-cat IN FRAME FRAME-A /* Beginning Product Category */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-date C-Win
ON LEAVE OF begin_ord-date IN FRAME FRAME-A /* Beginning Order Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_shipfrom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_shipfrom C-Win
ON HELP OF begin_shipfrom IN FRAME FRAME-A /* Beginning Ship From WH */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    run windows/l-loc.w  (cocode,{&SELF-NAME}:SCREEN-VALUE, output char-val). 
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_shipfrom C-Win
ON LEAVE OF begin_shipfrom IN FRAME FRAME-A /* Beginning Ship From WH */
DO:
  assign {&self-name}.
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

&Scoped-define SELF-NAME begin_cust-part
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-part C-Win
ON LEAVE OF begin_cust-part IN FRAME FRAME-A /* Beginning Cust Part */
DO:
   assign {&self-name}.
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
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  IF g_batch THEN tb_batch = YES.
  IF tb_batch THEN DO:
     RUN run-batch.
     RETURN NO-APPLY.
  END.

  RUN GetSelectionList.
  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
      EMPTY TEMP-TABLE ttCustList.
      RUN BuildCustList(INPUT cocode,
                        INPUT tb_cust-list AND glCustListActive,
                        INPUT begin_cust-no,
                        INPUT END_cust-no).
  END.
  RUN run-report.
  STATUS DEFAULT "Processing Complete".
  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &TYPE= "Customer "
                            &begin_cust= "begin_cust-no"
                            &END_cust= "begin_cust-no" 
                            &fax-subject=c-win:TITLE 
                            &fax-body=c-win:TITLE 
                            &fax-file=list-name }
       END. 
       WHEN 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Customer "
                                &begin_cust= "begin_cust-no"
                                &END_cust= "begin_cust-no"
                                &mail-subject=c-win:TITLE 
                                &mail-body=c-win:TITLE 
                                &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Customer "
                                  &begin_cust= "begin_cust-no"
                                  &END_cust= "begin_cust-no"
                                  &mail-subject=c-win:TITLE 
                                  &mail-body=c-win:TITLE 
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  END CASE.
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
  DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

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


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def C-Win
ON CHOOSE OF Btn_Def IN FRAME FRAME-A /* Default */
DO:
  DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

  RUN DisplaySelectionDefault.  /* task 04041406 */ 
  RUN DisplaySelectionList2 .

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


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
DO:
  RUN Move-Field ("Up").
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


&Scoped-define SELF-NAME end_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_due-date C-Win
ON LEAVE OF end_due-date IN FRAME FRAME-A /* Ending Due Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_fg-cat C-Win
ON LEAVE OF end_fg-cat IN FRAME FRAME-A /* Ending Product Category */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-date C-Win
ON LEAVE OF end_ord-date IN FRAME FRAME-A /* Ending Order Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_shipfrom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_shipfrom C-Win
ON HELP OF end_shipfrom IN FRAME FRAME-A /* Ending Ship From WH */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    run windows/l-loc.w  (cocode,{&SELF-NAME}:SCREEN-VALUE, output char-val). 
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_shipfrom C-Win
ON LEAVE OF end_shipfrom IN FRAME FRAME-A /* Ending Ship From WH */
DO:
  assign {&self-name}.
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

&Scoped-define SELF-NAME end_cust-part
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-part C-Win
ON LEAVE OF end_cust-part IN FRAME FRAME-A /* Ending Customer Part# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
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
    DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val NE "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
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
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_sqft
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sqft C-Win
ON VALUE-CHANGED OF rd_sqft IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
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
    DEFINE VARIABLE cSelectedList AS cha NO-UNDO.
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
       ASSIGN ldummy = sl_Avail:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
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


&Scoped-define SELF-NAME tb_comm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_comm C-Win
ON VALUE-CHANGED OF tb_comm IN FRAME FRAME-A /* Print Commission? */
DO:
  ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_exclude-set-comps
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_exclude-set-comps C-Win
ON VALUE-CHANGED OF tb_exclude-set-comps IN FRAME FRAME-A /* Exclude Set Components */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_exclude-transfer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_exclude-transfer C-Win
ON VALUE-CHANGED OF tb_exclude-transfer IN FRAME FRAME-A /* Exclude Transfer Releases/Orders */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_include-ordrel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_include-ordrel C-Win
ON VALUE-CHANGED OF tb_include-ordrel IN FRAME FRAME-A /* Include Orders with no Release? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_margin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_margin C-Win
ON VALUE-CHANGED OF tb_margin IN FRAME FRAME-A /* Print Avail Margin? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_Over%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_Over% C-Win
ON VALUE-CHANGED OF tb_Over% IN FRAME FRAME-A /* Print Order Over(%) + */
DO:
  IF SELF:SCREEN-VALUE = "yes" THEN DO:
      fOver%:SENSITIVE = YES.
      APPLY "entry" TO fOver%.
   END.
   ELSE DO:
      fOver%:SENSITIVE = NO.
      fOver%:SCREEN-VALUE = "0".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prepmisc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prepmisc C-Win
ON VALUE-CHANGED OF tb_prepmisc IN FRAME FRAME-A /* Include Prep / Misc Charges? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prft
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prft C-Win
ON VALUE-CHANGED OF tb_prft IN FRAME FRAME-A /* Print Profit? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rep-tot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rep-tot C-Win
ON VALUE-CHANGED OF tb_rep-tot IN FRAME FRAME-A /* Rep Sub Totals? */
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


&Scoped-define SELF-NAME tb_smn-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_smn-no C-Win
ON VALUE-CHANGED OF tb_smn-no IN FRAME FRAME-A /* Page By SalesRep? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sortby
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sortby C-Win
ON VALUE-CHANGED OF tb_sortby IN FRAME FRAME-A /* Sort by Order#? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ton
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ton C-Win
ON VALUE-CHANGED OF tb_ton IN FRAME FRAME-A /* Print $/Ton? */
DO:
  IF {&self-name}:SCREEN-VALUE EQ "Yes" THEN
    lv-ornt:SCREEN-VALUE = "L".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_Under%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_Under% C-Win
ON VALUE-CHANGED OF tb_Under% IN FRAME FRAME-A /* Print Order Under(%) - */
DO:
   IF SELF:SCREEN-VALUE = "yes" THEN DO:
      fUnder%:SENSITIVE = YES.
      APPLY "entry" TO fUnder%.
   END.
   ELSE DO:
      fUnder%:SENSITIVE = NO.
      fUnder%:SCREEN-VALUE = "0".
   END.
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
   begin_ord-date = TODAY 
   end_ord-date   = TODAY.

  IF g_batch THEN tb_batch = YES.

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "OR5",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}        
    RUN DisplaySelectionList2.    

    APPLY "entry" TO begin_ord-date.
  END.
 
  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'OR5',
                          INPUT NO,
                          OUTPUT glCustListActive).

 {sys/inc/chblankcust.i ""OR5""}

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
                            INPUT 'OR5',
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
                                  INPUT 'OR5').


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
  DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

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

  DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) NE NUM-ENTRIES(cFieldListToSelect) THEN DO:   
     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +                   
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .    
  END.

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
  DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
  
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
  DISPLAY tb_cust-list begin_cust-no end_cust-no begin_ord-date end_ord-date 
          lbl_sqft begin_due-date end_due-date begin_slsmn end_slsmn 
          begin_fg-cat end_fg-cat begin_shipfrom end_shipfrom tb_prepmisc 
          tb_smn-no tb_exclude-set-comps tb_rep-tot tb_exclude-transfer 
          tb_include-ordrel tb_Under% fUnder% fOver% tb_Over% sl_avail 
          sl_selected rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file tb_batch begin_cust-part
          end_cust-part
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 RECT-8 tb_cust-list btnCustList begin_cust-no end_cust-no 
         begin_ord-date end_ord-date begin_due-date end_due-date begin_slsmn 
         end_slsmn begin_fg-cat end_fg-cat begin_shipfrom end_shipfrom 
         tb_prepmisc tb_smn-no tb_exclude-set-comps tb_rep-tot 
         tb_exclude-transfer tb_include-ordrel tb_Under% tb_Over% Btn_Def 
         sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest 
         lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel 
         fi_file tb_batch btn-ok btn-cancel begin_cust-part end_cust-part 
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
 DEFINE VARIABLE cTmpList AS CHARACTER NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
 iColumnLength = 0.

 DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     
    IF NOT AVAILABLE ttRptList THEN
        MESSAGE "no " i ENTRY(i,ctmplist) SKIP
        ctmplist
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
           ttRptSelected.DisplayOrder = i
           iColumnLength = iColumnLength + ttRptSelected.FieldLength + 1.
           .        
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field C-Win 
PROCEDURE Move-Field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER move AS CHARACTER NO-UNDO.

  DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME}
      WITH FRAME {&FRAME-NAME}:
    IF sl_selected:IS-SELECTED(i) THEN
    DO:
      IF move = "Down" AND i NE sl_selected:NUM-ITEMS THEN
      ASSIGN
        ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
        ldummy = sl_selected:DELETE(i)
        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
        .
      ELSE
      IF move = "Up" AND i NE 1 THEN
      ASSIGN
        ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i - 1)
        ldummy = sl_selected:DELETE(i + 1)
        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i - 1)
        .
      LEAVE.
    END.
  END.

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
  RUN scr-rpt.w (list-name,c-win:TITLE,INTEGER(lv-font-no),lv-ornt). /* open file-name, title */ 
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
 /* DEFINE VARIABLE printok AS LOG.

  SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.

  RUN custom/usrprtb.p
      ("oerep\s-booked.r", FRAME {&FRAME-NAME}:HANDLE, SESSION:PRINTER-NAME, SESSION:PRINTER-PORT, CURRENT-WINDOW:TITLE).
  */

  {BATCH/runbatch.i "oerep\s-booked.r"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/***************************************************************************\
*****************************************************************************
**  Program: /u2/fold/all/dev/asi/oe/rep
**       by: Christopher A. Heins, 07.14.95
** Descript: SalesRep Performance daily, period and year to date.
**
*****************************************************************************
\***************************************************************************/
DEFINE VARIABLE str-tit4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE str-tit5 AS CHARACTER NO-UNDO.
DEFINE VARIABLE str-line AS CHARACTER FORM "x(300)" NO-UNDO.

{sys/form/r-top5DL.f}

DEFINE VARIABLE fdate as DATE FORMAT "99/99/9999" INITIAL 01/01/0001 NO-UNDO.
DEFINE VARIABLE tdate LIKE fdate INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE v-break AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE prt-sqft AS LOGICAL INITIAL YES FORMAT "SqFt/PartNo" NO-UNDO.
DEFINE VARIABLE p-m-chg AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE prt-profit AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE item-dscr AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE mdate AS DATE NO-UNDO.
DEFINE VARIABLE lo_trandate LIKE fdate NO-UNDO.
DEFINE VARIABLE v-per-days AS INTEGER EXTENT 2 NO-UNDO INITIAL 0.
DEFINE VARIABLE v-n-lines  AS INTEGER NO-UNDO.
DEFINE VARIABLE fsman AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE tsman AS CHARACTER FORMAT "x(3)" INITIAL "zzz" NO-UNDO.
DEFINE VARIABLE v-sman LIKE w-data.sman NO-UNDO.
DEFINE VARIABLE v-exclude AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-misc AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-amt  LIKE oe-ord.t-revenue NO-UNDO.
DEFINE VARIABLE v-pct AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE v-sqft LIKE itemfg.t-sqft  FORMAT ">,>>9.999" NO-UNDO.
DEFINE VARIABLE v-tons AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-qty LIKE oe-ordl.qty FORMAT "->>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-price-per-m AS DECIMAL COLUMN-LABEL "$/MSF" NO-UNDO.
DEFINE VARIABLE v-price-per-t AS DECIMAL COLUMN-LABEL "$/TON" NO-UNDO.
DEFINE VARIABLE v-msf LIKE v-price-per-m EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-ton LIKE v-price-per-t EXTENT 2 NO-UNDO.
DEFINE VARIABLE tot-sqft AS DECIMAL NO-UNDO.
DEFINE VARIABLE tot-renv AS DECIMAL NO-UNDO.
DEFINE VARIABLE tot-ton AS DECIMAL NO-UNDO.
DEFINE VARIABLE cPrevOrder AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOrdWithNoRel AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE fcpart like itemfg.part-no INITIAL " " NO-UNDO.
DEFINE VARIABLE tcpart like fcpart INITIAL "zzzzzzzzzzzzzzzzzzz" NO-UNDO.

DEFINE VARIABLE v-revenue LIKE oe-ordl.t-price FORMAT "->,>>>,>>9.99" NO-UNDO
  COLUMN-LABEL "Order!Amount".
DEFINE VARIABLE v-cost LIKE oe-ordl.cost FORMAT "->>>,>>>,>>9.99" NO-UNDO
  COLUMN-LABEL "COST/$M". 
DEFINE VARIABLE v-t-cost LIKE oe-ordl.t-cost FORMAT "->>,>>>,>>9.99" NO-UNDO
  COLUMN-LABEL "TOTAL STD COST".
DEFINE VARIABLE v-profit AS DECIMAL FORMAT "->>,>>9.9" NO-UNDO
  COLUMN-LABEL "% Profit".
DEFINE VARIABLE v-margin AS DECIMAL FORMAT "->>,>>9.9" NO-UNDO COLUMN-LABEL "% Margin".
DEFINE VARIABLE v-sname LIKE sman.sname.

DEFINE VARIABLE v AS INTEGER NO-UNDO.
DEFINE VARIABLE qm AS DECIMAL NO-UNDO.
DEFINE VARIABLE mat AS DECIMAL NO-UNDO.
DEFINE VARIABLE lab AS DECIMAL NO-UNDO.

DEFINE VARIABLE ii LIKE i NO-UNDO.
DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.

DEFINE VARIABLE cDisplay AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcelDisplay AS CHARACTER NO-UNDO.
DEFINE VARIABLE hField AS HANDLE NO-UNDO.
DEFINE VARIABLE cTmpField AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVarValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSelected AS LOGICAL INIT YES NO-UNDO.
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF BUFFER boe-ord FOR oe-ord.
DEF BUFFER boe-ordl FOR oe-ordl.
DEF BUFFER bcust FOR cust.
DEFINE VARIABLE dSDueDate as DATE FORMAT "99/99/9999" INITIAL 01/01/0001 NO-UNDO.
DEFINE VARIABLE dEDueDate LIKE dSDueDate INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE c-result  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResult    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCheckCostlist AS CHARACTER INIT "% Profit,ORDER AMOUNT,COST/$M,TOTAL STD COST,FULL COST" NO-UNDO.
DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

DEF BUFFER bw-data FOR w-data.

FIND FIRST w-data NO-ERROR.

FORM HEADER "Sales Rep:"
            w-data.sman
            "-"
            v-sname
    WITH FRAME r-top1 NO-BOX NO-ATTR-SPACE PAGE-TOP STREAM-IO WIDTH 180.

ASSIGN 
 str-tit2 = c-win:TITLE + "   (O-R-5)"
 {sys/inc/ctrtext.i str-tit2 112}

 fdate      = begin_ord-date
 tdate      = end_ord-date
 fsman      = begin_slsmn
 tsman      = end_slsmn
 v-break    = tb_smn-no
 /*prt-sqft   = rd_sqft eq "Square Ft"
 /* item-dscr  = tb_desc */
 prt-profit = tb_prft
 */
 p-m-chg    = tb_prepmisc
 lSelected  = tb_cust-list
 dSDueDate      = begin_due-date
 dEDueDate      = end_due-date    
 lOrdWithNoRel  = tb_include-ordrel 
 fcpart      = begin_cust-part
 tcpart      = END_cust-part.

/*IF tb_margin THEN prt-profit = NO.*/
prt-profit = NO.
DO i = 1 TO NUM-ENTRIES(cCheckCostlist,","):
    IF LOOKUP(ENTRY(i,cCheckCostlist),cSelectedlist) NE 0
    THEN prt-profit = YES.
END.
prt-sqft = NO.

IF prt-profit THEN DO:
  IF NOT security-flag THEN RUN sys/ref/d-passwd.w (3, OUTPUT security-flag).
  prt-profit = security-flag.
END.

RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:
    IF ttRptSelected.TextList MATCHES "*SQ FT*" THEN prt-sqft = YES.

    IF ttRptSelected.TextList = "% Profit" AND NOT prt-profit THEN NEXT. 
    IF ttRptSelected.TextList = "ORDER AMOUNT" AND NOT prt-profit THEN NEXT.
    IF ttRptSelected.TextList = "COST/$M" AND NOT prt-profit THEN NEXT.
    IF ttRptSelected.TextList = "TOTAL STD COST" AND NOT prt-profit THEN NEXT.
    IF ttRptSelected.TextList = "FULL COST" AND NOT prt-profit THEN NEXT.

    ASSIGN str-tit4 = str-tit4 + 
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength + 1 - LENGTH(ttRptSelected.TextList))
            str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + ",".        

    IF LOOKUP(ttRptSelected.TextList, "TOTAL Sq Ft/M,$/MSF,ORDER AMOUNT,TOTAL TONS,$/TON,% PROFIT") NE 0    THEN
        ASSIGN
        str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
    ELSE
        str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " .
END.
excelheader = "Sales Rep,Sales Name," + excelheader.

IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN begin_cust-no = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN end_cust-no = ttCustList.cust-no .
END.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(cFileName).

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

EMPTY TEMP-TABLE tt-report.

EMPTY TEMP-TABLE w-data.

EMPTY TEMP-TABLE wkrecap.

{oerep/r-bookedN.i} 

SESSION:SET-WAIT-STATE ("").

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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
  DEFINE VARIABLE lv-frame-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-group-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field2-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
  DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-label as CHARACTER.

  ASSIGN
  lv-frame-hdl = FRAME {&FRAME-NAME}:HANDLE
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD
  lv-field-hdl = lv-group-hdl:FIRST-CHILD.

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") GT 0
        THEN DO:
           IF lv-field-hdl:label NE ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                     .
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     .
              lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN DO:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                  END.
                  lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
              END.       
           END.                 
        END.            
     lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
  END.

  PUT SPACE(28)
      "< Selection Parameters >"
      skip(1).

  DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
    IF ENTRY(i,parm-fld-list) NE "" OR 
       ENTRY(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - LENGTH(TRIM(ENTRY(i,parm-lbl-list)))) +
                 TRIM(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetInksForJob C-Win 
FUNCTION fGetInksForJob RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE dResult    AS CHARACTER NO-UNDO.
    IF AVAIL job THEN DO:
        IF AVAIL eb THEN
            for each job-mat where job-mat.company eq cocode
                and job-mat.job     eq job.job  
                and job-mat.frm     eq eb.form-no
                NO-LOCK ,
                first item
                {sys/look/itemivW.i}
                and item.i-no eq job-mat.i-no:
                    IF eb.est-type LE 4 THEN do:
                        do i = 1 to 20:
                            if eb.i-code2[i] eq job-mat.i-no then do:
                                IF LOOKUP(job-mat.i-no,dResult) EQ 0 THEN
                                 dResult = dResult + job-mat.i-no + "," .
                            end.
                        end. /* loop i */
                    END.
                    ELSE do:
                        do i = 1 to 10:
                            if eb.i-code[i] eq job-mat.i-no then do:
                                IF LOOKUP(job-mat.i-no,dResult) EQ 0 THEN
                                 dResult = dResult + job-mat.i-no + "," . 
                            end.
                        end. /* loop i */
                    END.
            END.
    END.                

    RETURN dResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetRoutingForJob C-Win 
FUNCTION fGetRoutingForJob RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dResult    AS CHARACTER NO-UNDO.

    IF AVAIL job THEN DO:
        FOR EACH job-mch WHERE job-mch.company = job.company 
            AND job-mch.job = job.job 
            AND job-mch.job-no = job.job-no 
            AND job-mch.job-no2 = job.job-no2 
            use-index line-idx NO-LOCK BREAK BY job-mch.job :
            IF NOT LAST(job-mch.job) THEN
                dResult = dResult + job-mch.m-code + "," .
            ELSE dResult = dResult + job-mch.m-code .
        END.
    END.                

    RETURN dResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*RETURN string(hField:BUFFER-VALUE, hField:FORMAT) */
  RETURN STRING(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

