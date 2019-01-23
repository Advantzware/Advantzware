&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

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

/*{sys/inc/custlistform.i ""IR2"" }*/

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

{fg/rep/fg-ibtg1.i NEW SHARED}

DEF VAR ll-secure AS LOG NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR excel-header-var-1 AS CHAR NO-UNDO.
DEF VAR excel-header-var-2 AS CHAR NO-UNDO.
DEF VAR excel-header-var-3 AS CHAR NO-UNDO.
DEF VAR v-msf-oh AS DECIMAL NO-UNDO.
DEF VAR v-ord-price AS DECIMAL NO-UNDO.
DEF VAR v-po-ord AS CHAR NO-UNDO.
DEF VAR v-last-inv AS CHAR NO-UNDO.
/*DEF var v-tot-msf as dec format "->>>,>>9.99"  extent 3 no-undo.*/
def var v-tot-mat as dec format "->>,>>>,>>9.99"  extent 3 no-undo.
def var v-tot-lab as dec format "->>,>>>,>>9.99" extent 3 no-undo.
DEF VAR v-bin-msf AS DECIMAL NO-UNDO.
DEF VAR v-po-rel AS CHAR NO-UNDO.
DEF VAR v-sales-rep AS CHAR NO-UNDO.
DEF VAR v-cust-name AS CHAR NO-UNDO.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "CUSTOMER,CUST NAME,ITEM #,TAG#,FULL TAG#,FG LOT#,CUST PART#,DESCRIPTION,JOB#,REC DATE," +
                           "WHSE,BIN,MSF OH,C-UOM,REL QTY,QTY ON HAND,LAST SALE,FG CAT," +
                           "VIEW PO,LINE PO#,REL PO#,FG PRICE,ORDER PRICE,UOM COST,TOTAL COST,MAT COST,LABOR COST,REP," +
                           "SELL VAL(FG),SELL VAL(ORD),DAYS OLD,CUST OWN,SET HEADER,QTY PER SET,UNITS,UNIT COUNT,PARTIAL"
           cFieldListToSelect = "itemfg.cust-no,cust-name,itemfg.i-no,tag#,tag,fg-lot-val,itemfg.part-no,itemfg.i-name,v-job-no,recdate," +
                                "loc,bin,msf-on-hand,cost-uom,rel-qty,qty-on-hand,last-sale,itemfg.procat," +
                                "view-po,line-po,rel-po,sell-price,ord-pr,uom-cost,v-tot-cost,mat-cost,lab-cost,sale-rep," + 
                                "sell-value-fg,sell-value-ord,days-old,custno,set-header,qty-per-set,units,unit-count,partial"
           cFieldLength = "8,30,15,6,24,20,15,30,10,8," + "5,8,8,5,11,11,9,7," + "11,11,11,11,11,11,11,11,11,3," + "14,14,8,8,15,14,8,11,9"
           cFieldType = "c,c,c,c,c,c,c,c,c,c,c," + "c,c,i,c,i,i,c,c," + "c,c,c,i,i,i,i,i,i,c," + "i,i,i,c,c,i,i,i,i"
           .

{sys/inc/ttRptSel.i}

ASSIGN cTextListToDefault  = "CUSTOMER,CUST NAME,ITEM #,FULL TAG#,FG LOT#,REC DATE,CUST PART#,DESCRIPTION,WHSE,BIN,JOB#," +
                             "QTY ON HAND,MSF OH,C-UOM,UOM COST,TOTAL COST,LINE PO#,REL QTY" .


{sys/inc/oereordr.i}



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-10 RECT-12 RECT-13 ~
RECT-15 fi_days-old as-of-date tb_cust-list btnCustList begin_cust-no ~
end_cust-no begin_whse end_whse begin_loc-bin end_loc-bin begin_i-no ~
end_i-no begin_cat end_cat begin_slm end_slm rd_sort tb_sets tb_zero ~
rd_i-code tb_cust-whse tb_subt tb_cust-whse-2 tb_summ-bin tb_inactive ~
Btn_Def sl_avail sl_selected Btn_Add Btn_Remove btn_Up tb_page btn_down ~
rd-dest td-show-parm lv-ornt lines-per-page tb_excel lv-font-no tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_days-old as-of-date tb_cust-list ~
begin_cust-no end_cust-no begin_whse end_whse begin_loc-bin end_loc-bin ~
begin_i-no end_i-no begin_cat end_cat begin_slm end_slm lbl_sort lbl_i-code ~
lbl_prt-op rd_sort tb_sets tb_zero rd_i-code tb_cust-whse tb_subt ~
tb_cust-whse-2 tb_summ-bin tb_inactive sl_avail sl_selected tb_page rd-dest ~
td-show-parm lv-ornt lines-per-page tb_excel lv-font-no tb_runExcel ~
lv-font-name fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel /*AUTO-END-KEY */
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

DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/01 
     LABEL "As of" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc-bin AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Bin" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_slm AS CHARACTER FORMAT "XXX":U 
     LABEL "Beginning Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_whse AS CHARACTER FORMAT "X(5)" 
     LABEL "Beginning Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc-bin AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Bin" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_slm AS CHARACTER FORMAT "XXX":U INITIAL "zzz" 
     LABEL "Ending Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_whse AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
     LABEL "Ending Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_days-old AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Only Show QOH that is Older Than" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-detail.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE lbl_i-code AS CHARACTER FORMAT "X(256)":U INITIAL "Item Code?" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_prt-op AS CHARACTER FORMAT "X(256)":U INITIAL "Print Options:" 
     VIEW-AS FILL-IN 
     SIZE 14.2 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 45.4 BY 1 NO-UNDO.

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
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 85.4 BY 1.05 NO-UNDO.

DEFINE VARIABLE rd_i-code AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Stock", "Stock",
"Custom", "Custom",
"All", "All"
     SIZE 11 BY 3.14 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Customer#", "Customer#",
"FG Item#", "FG Item#",
"Part#", "Part#",
"Product Category", "Product Category",
"Whs/Bin", "Whs/Bin"
     SIZE 21.2 BY 8.1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13.6 BY 3.33.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 6.19.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 1.4 BY 4.52.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE .8 BY 10.95.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113.6 BY 6.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 114 BY 20.24.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5.71 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5.71 NO-UNDO.

DEFINE VARIABLE tb_cost AS LOGICAL INITIAL yes 
     LABEL "Print Cost?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cost-2 AS LOGICAL INITIAL no 
     LABEL "If Yes - DL/MAT Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_cust-whse AS LOGICAL INITIAL no 
     LABEL "Include Customer Owned Warehouse?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-whse-2 AS LOGICAL INITIAL no 
     LABEL "Only Customer Owned Warehouse?" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_inactive AS LOGICAL INITIAL no 
     LABEL "Include Inactive Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_page AS LOGICAL INITIAL no 
     LABEL "Page Break?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_sets AS LOGICAL INITIAL no 
     LABEL "Print Set and Components Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_subt AS LOGICAL INITIAL yes 
     LABEL "Print Subtotals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE tb_summ-bin AS LOGICAL INITIAL no 
     LABEL "Print Summary by Bin Qty" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_zero AS LOGICAL INITIAL yes 
     LABEL "Include Zero Balances?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_days-old AT ROW 1.86 COL 80.8 COLON-ALIGNED
     as-of-date AT ROW 1.95 COL 27 COLON-ALIGNED
     tb_cust-list AT ROW 3.14 COL 33 WIDGET-ID 6
     btnCustList AT ROW 3.24 COL 64.8 WIDGET-ID 8
     begin_cust-no AT ROW 4.24 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 4.24 COL 80.8 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_whse AT ROW 5.19 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Warehouse"
     end_whse AT ROW 5.19 COL 80.8 COLON-ALIGNED HELP
          "Enter Ending Warehouse Number"
     begin_loc-bin AT ROW 6.14 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Warehouse"
     end_loc-bin AT ROW 6.14 COL 80.8 COLON-ALIGNED HELP
          "Enter Ending Warehouse Number"
     begin_i-no AT ROW 7.1 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 7.1 COL 80.8 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_cat AT ROW 8.05 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Category"
     end_cat AT ROW 8.05 COL 80.8 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_slm AT ROW 9.05 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Category" WIDGET-ID 48
     end_slm AT ROW 9.05 COL 80.8 COLON-ALIGNED HELP
          "Enter Ending Order Number" WIDGET-ID 50
     lbl_sort AT ROW 10.33 COL 2 COLON-ALIGNED NO-LABEL
     lbl_i-code AT ROW 10.62 COL 24.4 COLON-ALIGNED NO-LABEL
     lbl_prt-op AT ROW 10.62 COL 41.8 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     rd_sort AT ROW 11.48 COL 3 NO-LABEL
     tb_sets AT ROW 11.48 COL 44
     tb_zero AT ROW 11.48 COL 86.8
     rd_i-code AT ROW 11.95 COL 27.8 NO-LABEL
     tb_cust-whse AT ROW 12.38 COL 44
     tb_subt AT ROW 12.38 COL 86.8
     tb_cust-whse-2 AT ROW 13.29 COL 44
     tb_summ-bin AT ROW 13.29 COL 86.8 WIDGET-ID 2
     tb_cost AT ROW 14.19 COL 44
     tb_cost-2 AT ROW 14.19 COL 59.8
     tb_inactive AT ROW 14.24 COL 86.8 WIDGET-ID 46
     Btn_Def AT ROW 15.48 COL 62.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_avail AT ROW 15.52 COL 29.4 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 15.52 COL 80.8 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 16.62 COL 62.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 17.81 COL 62.6 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 19 COL 62.6 WIDGET-ID 40
     tb_page AT ROW 19.95 COL 3.8 WIDGET-ID 44
     btn_down AT ROW 20.19 COL 62.6 WIDGET-ID 42
     rd-dest AT ROW 22.95 COL 3.2 NO-LABEL
     td-show-parm AT ROW 23.14 COL 90.4
     lv-ornt AT ROW 24.29 COL 3 NO-LABEL
     lines-per-page AT ROW 25.38 COL 33.2 COLON-ALIGNED
     tb_excel AT ROW 25.43 COL 88.2 RIGHT-ALIGNED
     lv-font-no AT ROW 25.48 COL 7 COLON-ALIGNED
     tb_runExcel AT ROW 25.48 COL 112.2 RIGHT-ALIGNED
     lv-font-name AT ROW 26.67 COL 1.2 COLON-ALIGNED NO-LABEL
     fi_file AT ROW 26.67 COL 66 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 28.33 COL 26.6
     btn-cancel AT ROW 28.33 COL 75.8
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 115.4 BY 28.95.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 21.91 COL 2.6
     RECT-6 AT ROW 21.76 COL 2
     RECT-7 AT ROW 1.48 COL 2
     RECT-10 AT ROW 11.76 COL 26.4 WIDGET-ID 12
     RECT-12 AT ROW 15.29 COL 26.2 WIDGET-ID 16
     RECT-13 AT ROW 10.52 COL 40.8 WIDGET-ID 18
     RECT-15 AT ROW 10.29 COL 24.6 WIDGET-ID 22
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 115.4 BY 28.95.


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
         TITLE              = "FG Value/Cost by Whs/Bin/Tag"
         HEIGHT             = 28.95
         WIDTH              = 116.2
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
       as-of-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_loc-bin:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_whse:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_loc-bin:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_whse:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_days-old:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_i-code IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_i-code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_i-code".

/* SETTINGS FOR FILL-IN lbl_prt-op IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_prt-op:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_i-code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_cost IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_cost:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_cost:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_cost-2 IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_cost-2:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_cost-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-whse:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-whse-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_inactive:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_page:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sets:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_subt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_summ-bin:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_zero:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* FG Value/Cost by Whs/Bin/Tag */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* FG Value/Cost by Whs/Bin/Tag */
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
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cat C-Win
ON LEAVE OF begin_cat IN FRAME FRAME-A /* Beginning Category */
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


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_loc-bin C-Win
ON LEAVE OF begin_loc-bin IN FRAME FRAME-A /* Beginning Bin */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slm C-Win
ON LEAVE OF begin_slm IN FRAME FRAME-A /* Beginning Sales Rep */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_whse C-Win
ON LEAVE OF begin_whse IN FRAME FRAME-A /* Beginning Warehouse */
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

 SESSION:SET-WAIT-STATE("general").
 RUN GetSelectionList.
 FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust-no,
                    INPUT end_cust-no).
  END.
  run run-report. 

  STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_cust-no
                            &END_cust=END_cust-no
                            &fax-subject=c-win:title 
                            &fax-body=c-win:title 
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_cust-no
                             &END_cust=end_cust-no
                             &mail-subject=c-win:title 
                             &mail-body=c-win:title 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_cust-no
                                  &END_cust=end_cust-no
                                  &mail-subject=c-win:title 
                                  &mail-body=c-win:title 
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case.
  SESSION:SET-WAIT-STATE("").
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


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def C-Win
ON CHOOSE OF Btn_Def IN FRAME FRAME-A /* Default */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

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


&Scoped-define SELF-NAME end_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cat C-Win
ON LEAVE OF end_cat IN FRAME FRAME-A /* Ending Category */
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


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_loc-bin C-Win
ON LEAVE OF end_loc-bin IN FRAME FRAME-A /* Ending Bin */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slm C-Win
ON LEAVE OF end_slm IN FRAME FRAME-A /* Ending Sales Rep */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_whse C-Win
ON LEAVE OF end_whse IN FRAME FRAME-A /* Ending Warehouse */
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


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode, {&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode, {&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

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


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
DO:
  assign {&self-name}.

  IF LOOKUP(rd_sort,"Customer#,FG Item#,Part#") <> 0 THEN do:
   tb_page:SENSITIVE = NO .
   tb_page:SCREEN-VALUE = "NO" .
  END.
  ELSE do:
    tb_page:SENSITIVE = YES .
    tb_page:SCREEN-VALUE = "YES" .
  END.
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


&Scoped-define SELF-NAME tb_cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cost C-Win
ON VALUE-CHANGED OF tb_cost IN FRAME FRAME-A /* Print Cost? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cost-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cost-2 C-Win
ON VALUE-CHANGED OF tb_cost-2 IN FRAME FRAME-A /* If Yes - DL/MAT Only? */
DO:
  ASSIGN {&self-name}.
  IF {&self-name}:SCREEN-VALUE EQ "YES" THEN

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
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inactive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inactive C-Win
ON VALUE-CHANGED OF tb_inactive IN FRAME FRAME-A /* Include Inactive Items? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_page C-Win
ON VALUE-CHANGED OF tb_page IN FRAME FRAME-A /* Page Break? */
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


&Scoped-define SELF-NAME tb_sets
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sets C-Win
ON VALUE-CHANGED OF tb_sets IN FRAME FRAME-A /* Print Set and Components Only? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_subt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_subt C-Win
ON VALUE-CHANGED OF tb_subt IN FRAME FRAME-A /* Print Subtotals? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_summ-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_summ-bin C-Win
ON VALUE-CHANGED OF tb_summ-bin IN FRAME FRAME-A /* Print Summary by Bin Qty */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_zero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zero C-Win
ON VALUE-CHANGED OF tb_zero IN FRAME FRAME-A /* Include Zero Balances? */
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
 RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "IR2",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i "AND lv-field-hdl:SENSITIVE"}

    ASSIGN
     as-of-date              = TODAY
     as-of-date:SCREEN-VALUE = STRING(TODAY).
    IF LOOKUP(rd_sort:SCREEN-VALUE,"Customer#,FG Item#,Part#") <> 0 THEN do:
        tb_page:SENSITIVE = NO .
        tb_page:SCREEN-VALUE = "NO" .
    END.
    ELSE do:
        tb_page:SENSITIVE = YES .
     END.
    RUN DisplaySelectionList2.
    /*APPLY "value-changed" TO tb_po-num.*/
    APPLY "entry" TO as-of-date.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'IR2',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""IR2""}

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
                            INPUT 'IR2',
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
                                  INPUT 'IR2').


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
  DEF VAR cTmpList AS cha NO-UNDO.

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

  cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

   DO iCount = 1 TO sl_selected:NUM-ITEMS:
       IF LOOKUP(ENTRY(iCount,cTmpList), cTextListToSelect) = 0 THEN
        ldummy = sl_selected:DELETE(ENTRY(iCount,cTmpList)).
  END.

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
  DISPLAY fi_days-old as-of-date tb_cust-list begin_cust-no end_cust-no 
          begin_whse end_whse begin_loc-bin end_loc-bin begin_i-no end_i-no 
          begin_cat end_cat begin_slm end_slm lbl_sort lbl_i-code lbl_prt-op 
          rd_sort tb_sets tb_zero rd_i-code tb_cust-whse tb_subt tb_cust-whse-2 
          tb_summ-bin tb_inactive sl_avail sl_selected tb_page rd-dest 
          td-show-parm lv-ornt lines-per-page tb_excel lv-font-no tb_runExcel 
          lv-font-name fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 RECT-10 RECT-12 RECT-13 RECT-15 fi_days-old as-of-date 
         tb_cust-list btnCustList begin_cust-no end_cust-no begin_whse end_whse 
         begin_loc-bin end_loc-bin begin_i-no end_i-no begin_cat end_cat 
         begin_slm end_slm rd_sort tb_sets tb_zero rd_i-code tb_cust-whse 
         tb_subt tb_cust-whse-2 tb_summ-bin tb_inactive Btn_Def sl_avail 
         sl_selected Btn_Add Btn_Remove btn_Up tb_page btn_down rd-dest 
         td-show-parm lv-ornt lines-per-page tb_excel lv-font-no tb_runExcel 
         fi_file btn-ok btn-cancel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ fg/rep/fg-ibtag.p 9/91 cd */
/* FINISHED GOODS - INVENTORY ON HAND BY BIN / TAG".                          */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-topw.f}*/
DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-line AS cha FORM "x(300)" NO-UNDO.
DEF VAR v-page AS LOGICAL NO-UNDO.
DEF VAR lProcessRel AS LOGICAL NO-UNDO.
DEF VAR lProcessLastSale AS LOGICAL NO-UNDO.
DEF VAR v-tot-fgsell AS DEC EXTENT 4 NO-UNDO.
DEF VAR v-tot-ordsell AS DEC EXTENT 4 NO-UNDO.

{sys/form/r-top5DL.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.
form header
     "        "
     "               "
     "                         "
     v-label4[3]
     "     "
     "        "
     "        "
     "         "
     v-label1[4]
     "COST"
     v-label1[1]
     v-label2[1] space(2)
     v-label2[2] format "x(27)"
     v-label3[1]
     skip
     "CUSTOMER"
     "ITEM #         "
     "TAG #   "
     v-label4[1]
     "DESCRIPTION              "
     "WHSE "
     "BIN     "
     "JOB #    "
     " ON HAND"
     "UOM "
     v-label1[2]
     v-label2[3] space(2)
     v-label2[4] format "x(27)"
     v-label3[2]
     skip
     "--------"
     "---------------"
     "--------"
     v-label4[2]
     "-------------------------"
     "-----"
     "--------"
     "---------"
     "--------"
     "----"
     v-label1[3]
     v-label2[5] space(2)
     v-label2[6] format "x(27)"
     v-label3[3]

    with frame r-top1 stream-io width 200
         no-labels no-box no-underline page-top.

form header
     "        "
     "               "
     "               "
     v-label4[3]
     "                         "
     "     "
     "        "
     "        "
     "         "
     v-label1[4]
     "COST"
     v-label1[1]
     v-label2[1] space(2)
     v-label2[2] format "x(27)"
     v-label3[1]
     skip
     "CUSTOMER"
     "ITEM #         "
     "TAG #   "
     v-label4[1]
     "CUST PART #    "
     "DESCRIPTION              "
     "WHSE "
     "BIN     "
     "JOB #    "
     " ON HAND"
     "UOM "
     v-label1[2]
     v-label2[3] space(2)
     v-label2[4] format "x(27)"
     v-label3[2]
     skip
     "--------"
     "---------------"
     "--------"
     v-label4[2]
     "---------------"
     "-------------------------"
     "-----"
     "--------"
     "--------"
     "---------"
     "----"
     v-label1[3]
     v-label2[5] space(2)
     v-label2[6] format "x(27)"
     v-label3[3]

    with frame r-top2 stream-io width 200
         no-labels no-box no-underline page-top.

form header
     "        "
     "               "
     "        "
     v-label4[3]
     "RECEIPT "
     "                         "
     "     "
     "        "
     "         "
     v-label1[4]
     "COST"
     v-label1[1]
     v-label2[1] space(2)
     v-label2[2] format "x(27)"
     v-label3[1]
     skip
     "CUSTOMER"
     "ITEM #         "
     "TAG #   "
     v-label4[1]
     "  DATE  "
     "DESCRIPTION              "
     "WHSE "
     "BIN     "
     "JOB #    "
     " ON HAND"
     "UOM "
     v-label1[2]
     v-label2[3] space(2)
     v-label2[4] format "x(27)"
     v-label3[2]
     skip
     "--------"
     "---------------"
     "--------"
     v-label4[2]
     "--------"
     "-------------------------"
     "-----"
     "--------"
     "---------"
     "--------"
     "----"
     v-label1[3]
     v-label2[5] space(2)
     v-label2[6] format "x(27)"
     v-label3[3]

    with frame r-top3 stream-io width 200
         no-labels no-box no-underline page-top.

form header
     "        "
     "               "
     "        "
      v-label4[3]
     "RECEIPT "
     "               "
     "                         "
     "     "
     "        "
     "         "
     v-label1[4]
     "COST"
     v-label1[1]
     v-label2[1] space(2)
     v-label2[2] format "x(27)"
     v-label3[1]
     skip
     "CUSTOMER"
     "ITEM #         "
     "TAG #   "
      v-label4[1]
     "  DATE  "
     "CUST PART #    "
     "DESCRIPTION              "
     "WHSE "
     "BIN     "
     "JOB #    "
     " ON HAND"
     "UOM "
     v-label1[2]
     v-label2[3] space(2)
     v-label2[4] format "x(27)"
     v-label3[2]
     skip
     "--------"
     "---------------"
     "--------"
      v-label4[2]
     "--------"
     "---------------"
     "-------------------------"
     "-----"
     "--------"
     "---------"
     "--------"
     "----"
     v-label1[3]
     v-label2[5] space(2)
     v-label2[6] format "x(27)"
     v-label3[3]

    with frame r-top4 stream-io width 200
         no-labels no-box no-underline page-top.

/*
IF NOT rd_sort BEGINS "W" THEN tb_subt = YES.
*/

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 vdat           = as-of-date
 fcus           = begin_cust-no
 tcus           = end_cust-no
 fslm           = begin_slm                     /*Task# 01221409*/
 tslm           = end_slm
 v-loc[1]       = begin_whse
 v-loc[2]       = end_whse
 v-loc-bin[1]   = begin_loc-bin
 v-loc-bin[2]   = end_loc-bin
 fino           = begin_i-no
 tino           = end_i-no
 fcat           = begin_cat
 tcat           = END_cat
 v-type         = SUBSTR(rd_i-code,1,1)
 v-sort-by-cust = SUBSTR(rd_sort,1,2)
 zbal           = tb_zero
 v-custown      = tb_cust-whse
 lIncludeInactive = tb_Inactive 
/*  v-prt-c        = tb_cost   */
/*  v-dl-mat       = tb_cost-2 */
 v-page         = tb_page
 /*v-prt-p        = tb_sell-pr*/
 /*v-prt-cpn      = tb_cust-pt*/
 /*v-prt-po       = tb_po-num*/
 /*v-fg-lot       = tb_fg-lot*/
 /*v-prt-arqty    = tb_actrel*/
 /*v-po-type      = SUBSTR(rd_po-type,1,1)*/
 /*v-prt-msf      = rd_msf EQ "MSF"*/
 v-subt         = tb_subt
 /*v-fgprice      = rd_price EQ "FG"*/
 v-sets         = tb_sets
 /*v-rct-date     = tb_rct-date*/
 v-summ-bin     = tb_summ-bin
 lSelected      = tb_cust-list

 v-tot-qty      = 0
 v-tot-cst      = 0
 v-tot-ext      = 0
 v-tot-mat      = 0
 v-tot-lab      = 0
 v-tot-ordsell  = 0
 v-tot-fgsell   = 0
 v-label1       = ""
 v-label2       = ""
 v-label3       = "".

ASSIGN
    v-tot-qty  = 0
    v-tot-cst  = 0 
    v-tot-ext  = 0
    v-tot-msf = 0
    excel-header-var-1  = "" 
    excel-header-var-2  = ""
    excel-header-var-3  = "" .

assign
  v-file         = fi_file
  v-excel        = tb_excel
  v-runexcel     = tb_runexcel.

/* If 'only customer owned' is checked, no need to check 'include cust owned' */
IF tb_cust-whse-2 THEN
    v-custown = TRUE.

IF (LOOKUP("TOTAL COST", cSelectedList) > 0 
    OR LOOKUP("UOM COST", cSelectedList) > 0
    OR LOOKUP("LABOR COST", cSelectedList) > 0 
    OR LOOKUP("MAT COST", cSelectedList) > 0) AND NOT ll-secure THEN
    RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).
IF LOOKUP("REL QTY", cSelectedList) > 0 
    OR LOOKUP("REL PO#", cSelectedList) > 0 THEN
    lProcessRel = YES.
IF LOOKUP("LAST SALE", cSelectedList) > 0 THEN
    lProcessLastSale = YES.

/* IF v-prt-c THEN DO:                                                   */
/*   IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure). */
/*   ASSIGN                                                              */
/*    v-prt-c = ll-secure                                                */
/*    v-prt-p = (v-prt-c and v-dl-mat) .                                 */
/* END.                                                                  */
IF lselected THEN DO:
      FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
      IF AVAIL ttCustList THEN ASSIGN fcus = ttCustList.cust-no .
      FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
      IF AVAIL ttCustList THEN ASSIGN tcus = ttCustList.cust-no .
END.
SESSION:SET-WAIT-STATE ("general").


 IF v-fg-lot THEN DO:
    assign
        v-label4[1] = "FG Lot#        "
        v-label4[2] = "---------------"
        v-label4[3] = "               ".
 END.
 ELSE DO:
    assign
        v-label4[1] = ""
        v-label4[2] = ""
        v-label4[3] = "".
 END.

if v-prt-c then do:
  assign
   v-label1[2] = "UOM COST"
   v-label1[3] = "--------"
   v-label2[5] = "-----------".

  if v-dl-mat then
    assign
     v-label2[1] = "     DIRECT"
     v-label2[3] = " LABOR COST".
  else
    assign
     v-label2[1] = "      TOTAL"
     v-label2[3] = "       COST".
end.    


if v-prt-p then do:
  v-label2[6] = "-----------" + " ---------------".

  if v-dl-mat THEN DO:
    assign
      v-label2[2] = "   MATERIAL" + if v-po-type eq "L"
                                   then " LINE"
                                   else " ORDER"
      v-label2[4] = "       COST" + " PO"
      excel-header-var-1 = "MATERIAL"
      excel-header-var-2 = IF v-po-type EQ "L" THEN "LINE"
                           ELSE "ORDER"
      excel-header-var-3 = "COST".
  END.
  ELSE DO:

    assign
     v-label2[2] = "    SELLING" + if v-po-type eq "L"
                                   then " LINE"
                                   else " ORDER"
     v-label2[4] = "      VALUE" + " PO"
     excel-header-var-1 = "SELLING"
     excel-header-var-2 = IF v-po-type EQ "L" THEN "LINE"
                          ELSE "ORDER"
     excel-header-var-3 = "VALUE".
  END.
end.

ELSE v-prt-po = NO.

IF v-prt-msf THEN
  ASSIGN
   v-label1[4] = "     MSF"
   v-qoh-f     = "->>9.999".
ELSE
  ASSIGN
   v-label1[4] = "QUANTITY"
   v-qoh-f     = "->>>,>>9".

IF v-prt-arqty THEN
  ASSIGN
   v-label3[1] = "ACTUAL REL"
   v-label3[2] = "  QUANTITY"
   v-label3[3] = "----------".

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

        IF LOOKUP(ttRptSelected.TextList, "QTY ON HAND,MSF OH,TOTAL COST,MSF Remain,MAT COST,LABOR COST,SELL VAL(FG),SELL VAL(ORD)") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " .
 END.

 IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
 END.


{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then do:
  run show-param.
  PAGE.
END.

/*VIEW FRAME r-top.

IF v-rct-date THEN DO:
  IF v-prt-cpn THEN VIEW FRAME r-top4.
               ELSE VIEW FRAME r-top3.
END.
ELSE
  IF v-prt-cpn THEN VIEW FRAME r-top2.
               ELSE VIEW FRAME r-top1.*/
display "" with frame r-top.


    STATUS DEFAULT "Processing...".

    EMPTY TEMP-TABLE tt-fg-bin.
    EMPTY TEMP-TABLE tt-itemfg.

    FOR EACH itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
          AND itemfg.cust-no GE fcus
          AND itemfg.cust-no LE tcus
          AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq itemfg.cust-no
          AND ttCustList.log-fld no-lock) else true)
          AND itemfg.i-no    GE fino
          AND itemfg.i-no    LE tino
          AND itemfg.procat  GE fcat
          AND itemfg.procat  LE tcat
          AND (itemfg.i-code EQ v-type or v-type eq "A")
          AND (itemfg.stat EQ "A" OR lIncludeInactive)
          AND (NOT v-sets    OR
               itemfg.isaset OR
               CAN-FIND(FIRST fg-set
                        WHERE fg-set.company EQ itemfg.company
                          AND fg-set.part-no EQ itemfg.i-no))
        USE-INDEX customer
/*         , */
/*         FIRST reftable NO-LOCK                              */
/*             {&where-fgstatus}                               */
/*             AND (reftable.code2 EQ "A" OR lIncludeInactive) */
         :

      RUN fg/rep/tt-fgbin.p (BUFFER itemfg, vdat, "", "zzzzzzzzzz",
                             v-loc[1], v-loc[2], v-loc-bin[1], v-loc-bin[2],
                             zbal, fi_days-old, YES, v-custown).

      FOR EACH tt-fg-bin
          WHERE tt-fg-bin.company EQ itemfg.company
            AND tt-fg-bin.i-no    EQ itemfg.i-no
            AND (v-custown OR tb_cust-whse-2 OR
                 (tt-fg-bin.cust-no EQ "" AND tt-fg-bin.loc NE "CUST"))
            AND (NOT tb_cust-whse-2 OR
                 (tt-fg-bin.cust-no NE "" OR tt-fg-bin.loc EQ "CUST"))
          USE-INDEX co-ino:

        IF tt-fg-bin.qty NE 0 OR zbal THEN DO:
          CREATE tt-itemfg.
          BUFFER-COPY itemfg TO tt-itemfg
          ASSIGN
           tt-itemfg.row-id      = ROWID(itemfg)
           tt-itemfg.job-no      = tt-fg-bin.job-no
           tt-itemfg.job-no2     = tt-fg-bin.job-no2
           tt-itemfg.loc         = tt-fg-bin.loc
           tt-itemfg.loc-bin     = tt-fg-bin.loc-bin
           tt-itemfg.tag         = tt-fg-bin.tag
           tt-itemfg.bin-cust-no = tt-fg-bin.cust-no
           tt-itemfg.part-cust   = STRING(tt-itemfg.part-no,"x(20)") +
                                   STRING(tt-itemfg.cust-no,"x(20)")
           tt-itemfg.loc-bin-tag = STRING(tt-itemfg.loc,"x(10)")         +
                                   STRING(tt-itemfg.loc-bin,"x(10)")     +
                                   STRING(tt-itemfg.tag,"x(20)").
        END.

        ELSE DELETE tt-fg-bin.
      END.
    END.

    v-cust-name = "" .

   if v-sort-by-cust eq "Cu" THEN do:
        for each tt-itemfg use-index cust-no no-lock,
        {fg/rep/fg-ibtagn.i tt-itemfg.cust-no tt-itemfg.i-no}
        end.
   END.
   ELSE if v-sort-by-cust eq "FG" THEN do:
        for each tt-itemfg use-index i-no no-lock,
            {fg/rep/fg-ibtagn.i tt-itemfg.i-no 1}
        end.
   END.
   ELSE if v-sort-by-cust eq "Pr" THEN do:
        for each tt-itemfg use-index procat no-lock,
        {fg/rep/fg-ibtagn.i tt-itemfg.procat tt-itemfg.i-no}
        end.
   END.
   ELSE if v-sort-by-cust eq "Pa" THEN do:
        for each tt-itemfg use-index part-no no-lock,
         {fg/rep/fg-ibtagn.i tt-itemfg.part-cust tt-itemfg.i-no}
         end.
   END.
   ELSE DO:
       for each tt-itemfg use-index loc-bin-tag no-lock,
        {fg/rep/fg-ibtagn.i tt-itemfg.loc-bin-tag tt-itemfg.i-no "by tt-itemfg.tag"}
        end.
   END.       
    /*if v-sort-by-cust eq "Cu" then run fg/rep/fg-ibtg1.p. else
    if v-sort-by-cust eq "FG" then run fg/rep/fg-ibtg2.p. else
    if v-sort-by-cust eq "Pr" then run fg/rep/fg-ibtg3.p. else
    if v-sort-by-cust eq "Pa" then run fg/rep/fg-ibtg4.p. else
                                   run fg/rep/fg-ibtg5.p.*/


    put skip(1).

    PUT    SKIP  str-line SKIP .
          ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".

     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:    
                WHEN "itemfg.cust-no" THEN cVarValue = "" .
                WHEN "itemfg.i-no" THEN cvarValue = "" .
                WHEN "itemfg.part-no" THEN cVarValue = "" .
                WHEN "itemfg.i-name" THEN cVarValue = "" .
                WHEN "itemfg.procat" THEN cVarValue = "" .
                WHEN "tag" THEN cVarValue = "" .
                WHEN "tag#" THEN cVarValue = "" .
                WHEN "fg-lot-val" THEN cvarValue = "" .
                WHEN "v-job-no" THEN cVarValue = "" .
                WHEN "recdate" THEN cVarValue = "" .
                WHEN "days-old" THEN cVarValue = "".
                WHEN "loc" THEN cVarValue = "".
                WHEN "bin" THEN cVarValue =    "".
                WHEN "msf-on-hand" THEN cVarValue = STRING(v-tot-msf[3],"->>9.999").
                WHEN "cost-uom" THEN cVarValue = "".
                WHEN "rel-qty" THEN cVarValue = "" .
                WHEN "qty-on-hand" THEN cVarValue = STRING(v-tot-qty[3],"->>,>>>,>>9") .
                WHEN "last-sale" THEN cVarValue = "" .
                WHEN "view-po" THEN cVarValue = "" .
                WHEN "line-po" THEN cVarValue = "" .
                WHEN "rel-po" THEN cVarValue = "" .
                WHEN "ord-pr" THEN cVarValue = "".
                WHEN "sell-price" THEN cVarValue = "".
                WHEN "uom-cost" THEN cVarValue = "" .
                WHEN "v-tot-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-tot-cst[3],"->>>,>>9.99") ELSE "").
                WHEN "lab-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-tot-lab[3],"->>>,>>9.99") ELSE "") .
                WHEN "mat-cost" THEN cVarValue = (IF ll-secure THEN STRING(v-tot-mat[3],"->>>,>>9.99") ELSE "") .
                WHEN "sale-rep" THEN cVarValue = "" .
                WHEN "sell-value-ord" THEN cVarValue = STRING(v-tot-ordsell[3],"->>,>>>,>>9.99") .    
                WHEN "sell-value-fg" THEN cVarValue = STRING(v-tot-fgsell[3],"->>,>>>,>>9.99") .
                WHEN "custno" THEN cVarValue = "" .
                WHEN "set-header" THEN cVarValue = "" .
                WHEN "qty-per-set" THEN cVarValue = "" .                 
                WHEN "cust-name" THEN cVarValue = "" . 
                WHEN "units" THEN cVarValue = "" . 
                WHEN "unit-count" THEN cVarValue = "" . 
                WHEN "partial" THEN cVarValue = "" . 
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
    END.
    PUT UNFORMATTED  "       GRAND TOTALS" substring(cDisplay,20,300) SKIP.
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
              "GRAND TOTALS " + substring(cExcelDisplay,3,300) SKIP.
     END.




    /* PUT "GRAND TOTALS" TO 20.


        PUT v-tot-qty[3] FORMAT "->>>,>>>,>>9.999" TO 42.
        PUT "MSF  " TO 55 .
        PUT v-tot-msf[3] TO 70.

      if v-prt-c THEN do:
           PUT "COST" AT 82 .
           put v-tot-cst[3] to 100.
      if v-prt-p then PUT "     MAT COST" v-tot-mat[3] to 130 "     LAB COST   " v-tot-lab[3] .
      END.*/

    STATUS DEFAULT "".

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

 /*IF v-excel = TRUE THEN DO:

      EXPORT STREAM excel DELIMITER ","
         " "
         "GRAND TOTALS"
         ""
         "QTY "
         v-tot-qty[3] FORMAT "->>>,>>>,>>9.99"
         ""
         "MSF "
         v-tot-msf[3] FORMAT "->>>,>>9.99"
         ""
         (IF v-prt-c THEN "COST " ELSE "")
         (if v-prt-c then v-tot-cst[3] ELSE 0)
          ""
         (IF v-prt-p THEN "MAT Cost " ELSE "")
         (if v-prt-p then v-tot-mat[3] ELSE 0)
         (IF v-prt-p THEN "LAB Cost " ELSE "")
         (if v-prt-p then v-tot-lab[3] ELSE 0)
        /* (if v-prt-p THEN v-tot-ext[3] ELSE 0)*/.
    end.*/


 IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
 END.

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
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


