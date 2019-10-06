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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-10 RECT-11 RECT-12 ~
RECT-13 RECT-15 fi_days-old as-of-date tb_cust-list btnCustList ~
begin_cust-no end_cust-no begin_whse end_whse begin_loc-bin end_loc-bin ~
begin_i-no end_i-no begin_cat end_cat rd_sort tb_rct-date tb_zero rd_i-code ~
tb_cust-whse tb_subt tb_actrel tb_cust-whse-2 tb_cust-pt tb_cost tb_cost-2 ~
tb_summ-bin tb_sets tb_sell-pr rd_price rd_msf tb_po-num rd_po-type ~
tb_fg-lot rd-dest td-show-parm lv-ornt lines-per-page tb_excel lv-font-no ~
tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_days-old as-of-date tb_cust-list ~
begin_cust-no end_cust-no begin_whse end_whse begin_loc-bin end_loc-bin ~
begin_i-no end_i-no begin_cat end_cat lbl_sort lbl_prt-op lbl_i-code ~
rd_sort tb_rct-date tb_zero rd_i-code tb_cust-whse tb_subt tb_actrel ~
tb_cust-whse-2 tb_cust-pt tb_cost tb_cost-2 tb_summ-bin tb_sets lbl_msf ~
tb_sell-pr lbl_price2 rd_price rd_msf tb_po-num rd_po-type lbl_po-type ~
tb_fg-lot rd-dest td-show-parm lv-ornt lines-per-page tb_excel lv-font-no ~
tb_runExcel lv-font-name fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel /*AUTO-END-KEY*/
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

DEFINE VARIABLE lbl_msf AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_po-type AS CHARACTER FORMAT "X(256)":U INITIAL "If Yes - PO Type?" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_price2 AS CHARACTER FORMAT "X(256)":U INITIAL "From" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_prt-op AS CHARACTER FORMAT "X(256)":U INITIAL "Print Option:" 
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
     SIZE 11 BY 3.38 NO-UNDO.

DEFINE VARIABLE rd_msf AS CHARACTER INITIAL "Qty" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Qty", "Qty",
"MSF", "MSF"
     SIZE 9.2 BY 2.29 NO-UNDO.

DEFINE VARIABLE rd_po-type AS CHARACTER INITIAL "Line" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Line", "Line",
"Header", "Header"
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE rd_price AS CHARACTER INITIAL "Order" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "FG", "FG",
"Order", "Order"
     SIZE 18 BY 1 NO-UNDO.

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
     SIZE 13.6 BY 5.24.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13.6 BY 3.81.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 57 BY 3.67.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 1.4 BY 9.52.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 1.4 BY 9.52.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113.6 BY 6.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 114 BY 18.1.

DEFINE VARIABLE tb_actrel AS LOGICAL INITIAL no 
     LABEL "Print Actual Release Qty?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

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

DEFINE VARIABLE tb_cust-pt AS LOGICAL INITIAL no 
     LABEL "Print Customer Part#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

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

DEFINE VARIABLE tb_fg-lot AS LOGICAL INITIAL no 
     LABEL "Print FG Lot#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE tb_po-num AS LOGICAL INITIAL no 
     LABEL "Print PO#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_rct-date AS LOGICAL INITIAL no 
     LABEL "Print Receipt Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_sell-pr AS LOGICAL INITIAL yes 
     LABEL "Print Sell Price?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1 NO-UNDO.

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
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_days-old AT ROW 1.91 COL 80.8 COLON-ALIGNED
     as-of-date AT ROW 2 COL 27 COLON-ALIGNED
     tb_cust-list AT ROW 3.24 COL 34.2 WIDGET-ID 6
     btnCustList AT ROW 3.33 COL 66 WIDGET-ID 8
     begin_cust-no AT ROW 4.48 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 4.48 COL 80.8 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_whse AT ROW 5.43 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Warehouse"
     end_whse AT ROW 5.43 COL 80.8 COLON-ALIGNED HELP
          "Enter Ending Warehouse Number"
     begin_loc-bin AT ROW 6.38 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Warehouse"
     end_loc-bin AT ROW 6.38 COL 80.8 COLON-ALIGNED HELP
          "Enter Ending Warehouse Number"
     begin_i-no AT ROW 7.33 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 7.33 COL 80.8 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_cat AT ROW 8.29 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Category"
     end_cat AT ROW 8.29 COL 80.8 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     lbl_sort AT ROW 9.76 COL 2 COLON-ALIGNED NO-LABEL
     lbl_prt-op AT ROW 9.76 COL 41.8 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     lbl_i-code AT ROW 9.91 COL 24.4 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 10.91 COL 3 NO-LABEL
     tb_rct-date AT ROW 10.91 COL 86.8
     tb_zero AT ROW 10.95 COL 44
     rd_i-code AT ROW 11.1 COL 27.8 NO-LABEL
     tb_cust-whse AT ROW 11.81 COL 44
     tb_subt AT ROW 12.43 COL 87
     tb_actrel AT ROW 12.62 COL 86.8
     tb_cust-whse-2 AT ROW 12.71 COL 44
     tb_cust-pt AT ROW 13.48 COL 86.8
     tb_cost AT ROW 13.62 COL 44
     tb_cost-2 AT ROW 13.62 COL 59.8
     tb_summ-bin AT ROW 14.33 COL 86.8 WIDGET-ID 2
     tb_sets AT ROW 14.52 COL 43.8
     lbl_msf AT ROW 15.24 COL 25.8 COLON-ALIGNED NO-LABEL
     tb_sell-pr AT ROW 15.81 COL 44.4
     lbl_price2 AT ROW 15.81 COL 63.6 COLON-ALIGNED NO-LABEL
     rd_price AT ROW 15.81 COL 73 NO-LABEL
     rd_msf AT ROW 16.43 COL 27.8 NO-LABEL
     tb_po-num AT ROW 16.91 COL 44.6
     rd_po-type AT ROW 17 COL 79 NO-LABEL
     lbl_po-type AT ROW 17.43 COL 58 COLON-ALIGNED NO-LABEL
     tb_fg-lot AT ROW 18.05 COL 44.6
     rd-dest AT ROW 21 COL 3.2 NO-LABEL
     td-show-parm AT ROW 21.19 COL 90.4
     lv-ornt AT ROW 22.33 COL 3 NO-LABEL
     lines-per-page AT ROW 23.43 COL 33.2 COLON-ALIGNED
     tb_excel AT ROW 23.48 COL 88.2 RIGHT-ALIGNED
     lv-font-no AT ROW 23.52 COL 7 COLON-ALIGNED
     tb_runExcel AT ROW 23.52 COL 112.2 RIGHT-ALIGNED
     lv-font-name AT ROW 24.71 COL 1.2 COLON-ALIGNED NO-LABEL
     fi_file AT ROW 24.71 COL 66 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 26.52 COL 27
     btn-cancel AT ROW 26.52 COL 76.2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 19.95 COL 2.6
     RECT-6 AT ROW 19.43 COL 2
     RECT-7 AT ROW 1.24 COL 2
     RECT-10 AT ROW 9.81 COL 26.2 WIDGET-ID 12
     RECT-11 AT ROW 15.05 COL 26.2 WIDGET-ID 14
     RECT-12 AT ROW 15.57 COL 43.4 WIDGET-ID 16
     RECT-13 AT ROW 9.71 COL 40.8 WIDGET-ID 18
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 115.4 BY 27.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
     RECT-15 AT ROW 9.71 COL 24.4 WIDGET-ID 22
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 115.4 BY 27.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "FG Value/Cost by Whs/Bin/Tag"
         HEIGHT             = 27.1
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

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
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

/* SETTINGS FOR FILL-IN lbl_msf IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_msf:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_msf".

/* SETTINGS FOR FILL-IN lbl_po-type IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_po-type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_po-type".

/* SETTINGS FOR FILL-IN lbl_price2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_price2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_price".

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
       rd_msf:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_po-type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_price:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_actrel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cost:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cost-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-pt:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       tb_fg-lot:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_po-num:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_rct-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sell-pr:PRIVATE-DATA IN FRAME FRAME-A     = 
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
DO:
  RUN CustList.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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


&Scoped-define SELF-NAME tb_actrel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_actrel C-Win
ON VALUE-CHANGED OF tb_actrel IN FRAME FRAME-A /* Print Actual Release Qty? */
DO:
  assign {&self-name}.
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
    ASSIGN
     tb_sell-pr:SCREEN-VALUE = "NO"
     tb_sell-pr              = NO.
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


&Scoped-define SELF-NAME tb_cust-pt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-pt C-Win
ON VALUE-CHANGED OF tb_cust-pt IN FRAME FRAME-A /* Print Customer Part#? */
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


&Scoped-define SELF-NAME tb_fg-lot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fg-lot C-Win
ON VALUE-CHANGED OF tb_fg-lot IN FRAME FRAME-A /* Print FG Lot#? */
DO:
  /*IF tb_sell-pr:SCREEN-VALUE EQ "yes" THEN {&self-name}:SCREEN-VALUE = "no".
  IF tb_po-num:SCREEN-VALUE EQ "yes" THEN {&self-name}:SCREEN-VALUE = "no".*/
  IF {&self-name}:SCREEN-VALUE EQ "yes" THEN tb_sell-pr:SCREEN-VALUE = "no".
  IF {&self-name}:SCREEN-VALUE EQ "yes" THEN tb_po-num:SCREEN-VALUE = "no".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_po-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_po-num C-Win
ON VALUE-CHANGED OF tb_po-num IN FRAME FRAME-A /* Print PO#? */
DO:
  /*IF tb_fg-lot:SCREEN-VALUE EQ "yes" THEN {&self-name}:SCREEN-VALUE = "no".*/
  IF {&self-name}:SCREEN-VALUE EQ "yes" THEN tb_fg-lot:SCREEN-VALUE = "no".
  IF {&self-name}:SCREEN-VALUE EQ "yes" THEN tb_sell-pr:SCREEN-VALUE = "yes".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rct-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rct-date C-Win
ON VALUE-CHANGED OF tb_rct-date IN FRAME FRAME-A /* Print Receipt Date? */
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


&Scoped-define SELF-NAME tb_sell-pr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sell-pr C-Win
ON VALUE-CHANGED OF tb_sell-pr IN FRAME FRAME-A /* Print Sell Price? */
DO:
   /*IF tb_fg-lot:SCREEN-VALUE EQ "yes" THEN {&self-name}:SCREEN-VALUE = "no".*/
   IF tb_po-num:SCREEN-VALUE EQ "yes" THEN {&self-name}:SCREEN-VALUE = "yes".
   IF {&self-name}:SCREEN-VALUE EQ "yes" THEN tb_fg-lot:SCREEN-VALUE = "no".

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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

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

  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "IR2",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i "AND lv-field-hdl:SENSITIVE"}

    ASSIGN
     as-of-date              = TODAY
     as-of-date:SCREEN-VALUE = STRING(TODAY).

    APPLY "value-changed" TO tb_po-num.
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

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
          begin_cat end_cat lbl_sort lbl_prt-op lbl_i-code rd_sort tb_rct-date 
          tb_zero rd_i-code tb_cust-whse tb_subt tb_actrel tb_cust-whse-2 
          tb_cust-pt tb_cost tb_cost-2 tb_summ-bin tb_sets lbl_msf tb_sell-pr 
          lbl_price2 rd_price rd_msf tb_po-num rd_po-type lbl_po-type tb_fg-lot 
          rd-dest td-show-parm lv-ornt lines-per-page tb_excel lv-font-no 
          tb_runExcel lv-font-name fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 RECT-10 RECT-11 RECT-12 RECT-13 RECT-15 fi_days-old 
         as-of-date tb_cust-list btnCustList begin_cust-no end_cust-no 
         begin_whse end_whse begin_loc-bin end_loc-bin begin_i-no end_i-no 
         begin_cat end_cat rd_sort tb_rct-date tb_zero rd_i-code tb_cust-whse 
         tb_subt tb_actrel tb_cust-whse-2 tb_cust-pt tb_cost tb_cost-2 
         tb_summ-bin tb_sets tb_sell-pr rd_price rd_msf tb_po-num rd_po-type 
         tb_fg-lot rd-dest td-show-parm lv-ornt lines-per-page tb_excel 
         lv-font-no tb_runExcel fi_file btn-ok btn-cancel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ fg/rep/fg-ibtag.p 9/91 cd */
/* FINISHED GOODS - INVENTORY ON HAND BY BIN / TAG".                          */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}
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

form header
     "        "
     "               "
     "               "
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
     "CUST PART #    "
     "ITEM #         "
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
     "---------------"
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

    with frame r-top5 stream-io width 200
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
 v-prt-c        = tb_cost
 v-dl-mat       = tb_cost-2
 v-prt-p        = tb_sell-pr
 v-prt-cpn      = tb_cust-pt
 v-prt-po       = tb_po-num
 v-fg-lot       = tb_fg-lot
 v-prt-arqty    = tb_actrel
 v-po-type      = SUBSTR(rd_po-type,1,1)
 v-prt-msf      = rd_msf EQ "MSF"
 v-subt         = tb_subt
 v-fgprice      = rd_price EQ "FG"
 v-sets         = tb_sets
 v-rct-date     = tb_rct-date
 v-summ-bin     = tb_summ-bin
 lSelected      = tb_cust-list

 v-tot-qty      = 0
 v-tot-cst      = 0
 v-tot-ext      = 0
 v-label1       = ""
 v-label2       = ""
 v-label3       = "".

ASSIGN
    v-tot-qty  = 0
    v-tot-cst  = 0 
    v-tot-ext  = 0
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

IF v-prt-c THEN DO: 
  IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).
  ASSIGN
   v-prt-c = ll-secure
   v-prt-p = (v-prt-c and v-dl-mat) or (v-prt-p and not v-dl-mat).
END.
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

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then do:
  run show-param.
  PAGE.
END.

VIEW FRAME r-top.

IF v-rct-date THEN DO:
  IF v-prt-cpn THEN do:
      IF v-summ-bin THEN 
          VIEW FRAME r-top5.
      ELSE
          VIEW FRAME r-top4.
  END.
  ELSE VIEW FRAME r-top3.
END.
ELSE
  IF v-prt-cpn THEN VIEW FRAME r-top2.
               ELSE VIEW FRAME r-top1.

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).

   IF v-rct-date = TRUE AND v-prt-cpn = TRUE THEN DO:
      EXPORT STREAM excel DELIMITER ","   
         " "
         " "
         " "
         " "
         "RECEIPT "
         " "
         " "
         " "
         " "
         " "
         " "
         v-label1[4]
         "COST"
         v-label1[1]
         v-label2[1] 
         excel-header-var-1
         excel-header-var-2
         v-label3[1]
         SKIP.
      EXPORT STREAM excel DELIMITER ","          
         "CUSTOMER" 
         "ITEM #"
         "TAG #"
         v-label4[1]
         "DATE"
         "DAYS"
         "CUST PART #"
         "DESCRIPTION"
         "WHSE "
         "BIN"
         "JOB #"
         "ON HAND"
         "UOM"
         v-label1[2]
         v-label2[3] 
         excel-header-var-3
         "PO"
         v-label3[2]
         SKIP.      
   END. /*IF v-rct-date = TRUE AND v-prt-cpn = TRUE THEN DO:*/


   IF v-rct-date = TRUE AND v-prt-cpn = FALSE THEN DO:
      EXPORT STREAM excel DELIMITER ","    
          " "
          " "
          " "
          " "
          "RECEIPT "
          " "
          " "
          " "
          " "
          " "
          v-label1[4]
          "COST"
          v-label1[1]
          v-label2[1]
          excel-header-var-1
          excel-header-var-2
          v-label3[1]
          SKIP.
      EXPORT STREAM excel DELIMITER ","      
          "CUSTOMER"
          "ITEM #"
          "TAG #"
          v-label4[1]
          "DATE"
          "DAYS"
          "DESCRIPTION"
          "WHSE"
          "BIN"
          "JOB #"
          "ON HAND"
          "UOM"
          v-label1[2]
          v-label2[3]
          excel-header-var-3
          "PO"
          v-label3[2]
          SKIP.
   END. /*IF v-rct-date TRUE AND v-prt-cpn = FALSE THEN DO:*/     

   IF v-rct-date = FALSE AND v-prt-cpn = TRUE THEN DO:
      EXPORT STREAM excel DELIMITER ","   
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          v-label1[4]
          "COST"
          v-label1[1]
          v-label2[1]
          excel-header-var-1
          excel-header-var-2
          v-label3[1]
          SKIP.
      EXPORT STREAM excel DELIMITER ","     
          "CUSTOMER"
          "ITEM #"
          "TAG #"
          v-label4[1]
          "CUST PART #"
          "DESCRIPTION"
          "WHSE"
          "BIN"
          "JOB #"
          "ON HAND"
          "UOM"
          v-label1[2]
          v-label2[3]
          excel-header-var-3
          "PO"
          v-label3[2]
          SKIP.
   END. /* IF v-rct-date = FALSE AND v-prt-cpn = TRUE THEN DO: */

   IF v-rct-date = FALSE AND v-prt-cpn = FALSE THEN DO:
      EXPORT STREAM excel DELIMITER ","
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          " "
          v-label1[4]
          "COST"
          v-label1[1]
          v-label2[1]
          excel-header-var-1
          excel-header-var-2
          v-label3[1]
          SKIP.
      EXPORT STREAM excel DELIMITER ","     
          "CUSTOMER"
          "ITEM #"
          "TAG #"
          v-label4[1]
          "DESCRIPTION"
          "WHSE"
          "BIN"
          "JOB #"
          "ON HAND"
          "UOM"
          v-label1[2]
          v-label2[3]
          excel-header-var-3
          "PO"
          v-label3[2]
          SKIP.
   END. /*IF v-rct-date FALSE AND v-prt-cpn = FALSE THEN DO:*/

END. /* IF tb_excel THEN DO: */ 


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
          AND (NOT v-sets    OR
               itemfg.isaset OR
               CAN-FIND(FIRST fg-set
                        WHERE fg-set.company EQ itemfg.company
                          AND fg-set.part-no EQ itemfg.i-no))
        USE-INDEX customer:

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

    if v-sort-by-cust eq "Cu" then run fg/rep/fg-ibtg1.p. else
    if v-sort-by-cust eq "FG" then run fg/rep/fg-ibtg2.p. else
    if v-sort-by-cust eq "Pr" then run fg/rep/fg-ibtg3.p. else
    if v-sort-by-cust eq "Pa" then run fg/rep/fg-ibtg4.p. else
                                   run fg/rep/fg-ibtg5.p.

    put skip(1).

    IF v-rct-date THEN
    if v-prt-cpn then do:
        IF v-summ-bin THEN DO:
            PUT "GRAND TOTALS" TO 113.

            IF v-prt-msf THEN
                PUT v-tot-qty[3] FORMAT "->>>,>>9.999" TO 125.
            ELSE
                PUT v-tot-qty[3] TO 125.
            if v-prt-c then put v-tot-cst[3] to 151.
            if v-prt-p then put v-tot-ext[3] to 164 skip(1).
        END.
        ELSE do:
            PUT "GRAND TOTALS" TO 120.

            IF v-prt-msf THEN
              PUT v-tot-qty[3] FORMAT "->>>,>>9.999" TO 134.
            ELSE
              PUT v-tot-qty[3] TO 134.

            if v-prt-c then put v-tot-cst[3] to 160.
            if v-prt-p then put v-tot-ext[3] to 173 skip(1).
        END.
    end.

    else do:
       put "GRAND TOTALS" TO 103.

      IF v-prt-msf THEN
        PUT v-tot-qty[3] FORMAT "->>>,>>9.999" TO 118.
      ELSE
        PUT v-tot-qty[3] TO 118.

      if v-prt-c then put v-tot-cst[3] to 144.
      if v-prt-p then put v-tot-ext[3] to 157 skip(1).
    end.

    ELSE
    if v-prt-cpn then do:
      PUT "GRAND TOTALS" TO 110.

      IF v-prt-msf THEN
        PUT v-tot-qty[3] FORMAT "->>>,>>9.999" TO 125.
      ELSE
        PUT v-tot-qty[3] TO 125.

      if v-prt-c then put v-tot-cst[3] to 151.
      if v-prt-p then put v-tot-ext[3] to 164 skip(1).
    end.

    else do:
      put "GRAND TOTALS" TO 90.

      IF v-prt-msf THEN
        PUT v-tot-qty[3] FORMAT "->>>,>>9.999" TO 109.
      ELSE
        PUT v-tot-qty[3] TO 109.

      if v-prt-c then put v-tot-cst[3] to 135.
      if v-prt-p then put v-tot-ext[3] to 146 skip(1).
    end.


    STATUS DEFAULT "".

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

 IF v-excel = TRUE THEN DO:
    IF v-rct-date THEN
    if v-prt-cpn then do:
      EXPORT STREAM excel DELIMITER ","
         " "
          " "
         " "
         " "
         " "
         " "
         " "
         " "
         " "
         "GRAND TOTALS" 
         v-tot-qty[3] FORMAT "->>>,>>9.99"
         " "
         " "
         (if v-prt-c then v-tot-cst[3] ELSE 0)
         (if v-prt-p THEN v-tot-ext[3] ELSE 0).
    end.

    else do:
      EXPORT STREAM excel DELIMITER ","
         " "
         " "
          " "
         " "
         " "
         " "
         " "
         " "
         "GRAND TOTALS"
         v-tot-qty[3] FORMAT "->>>,>>9.99"
         " "
         " "
         (if v-prt-c then v-tot-cst[3] ELSE 0)
         (if v-prt-p THEN v-tot-ext[3] ELSE 0).
    end.

    ELSE
    if v-prt-cpn then do:
       EXPORT STREAM excel DELIMITER ","
         " "
         " "
           " "
         " "
         " "
         " "
         " "
         " "
         "GRAND TOTALS" 
         v-tot-qty[3] FORMAT "->>>,>>9.99"
         " "
         " "
         (if v-prt-c then v-tot-cst[3] ELSE 0)
         (if v-prt-p THEN v-tot-ext[3] ELSE 0).
    end.

    else do:
      EXPORT STREAM excel DELIMITER ","
         " "
         " "
          " "
         " "
         " "
         " "
         " "
         "GRAND TOTALS" 
         v-tot-qty[3] FORMAT "->>>,>>9.99"
         " "
         " "
         (if v-prt-c then v-tot-cst[3] ELSE 0)
         (if v-prt-p THEN v-tot-ext[3] ELSE 0).
    end.
 END.

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

