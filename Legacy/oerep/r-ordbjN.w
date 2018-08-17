&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-ordbal.w

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

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-report-title AS CHAR NO-UNDO.

DEF VAR v-sort AS CHAR NO-UNDO.
DEF VAR v-ordl AS LOG NO-UNDO.
def var v-po like oe-relh.po-no extent 2 init ["","zzzzzzzzzzzzzzz"].
DEFINE VAR job-qty AS INT NO-UNDO.
DEFINE VAR open-job-qty AS INT NO-UNDO.
DEFINE VAR qtyOnHand AS INT NO-UNDO.
DEFINE VAR order-qty AS INT NO-UNDO.
DEFINE VAR wip-qty AS INT NO-UNDO.
DEFINE VAR shipped-qty AS INT NO-UNDO.
DEFINE VAR v-ship-qty AS INT NO-UNDO.
DEFINE VAR produced-qty AS INT NO-UNDO.
DEFINE VAR v-prod-qty AS INT NO-UNDO.
DEFINE VAR on-hand-qty AS INT NO-UNDO.
DEFINE VAR job-qty-rcvd AS INT NO-UNDO.
DEFINE VAR lJob-open AS LOG NO-UNDO.
DEF VAR qoh AS INT NO-UNDO.
DEF VAR qprod AS INT NO-UNDO.
DEF VAR li-qoh AS INTEGER NO-UNDO.
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD q-onh  LIKE itemfg.q-onh
    FIELD q-shp  LIKE itemfg.q-onh
    FIELD q-wip  LIKE itemfg.q-onh
    FIELD po-no  LIKE oe-ord.po-no
    FIELD inv    AS   LOG
    FIELD inv-no  LIKE ar-invl.inv-no
    FIELD prod-qty    AS INT 
    FIELD qty-to-prod AS INT

    FIELD row-id AS ROWID
    INDEX row-id row-id.

DEF TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin.

DEF VAR lv-pdf-file AS cha NO-UNDO.

DEF STREAM excel.

DEF BUFFER b-oe-bolh FOR oe-bolh.
DEF BUFFER b-oe-boll FOR oe-boll.
DEF BUFFER b-oe-rell FOR oe-rell.
DEF BUFFER b-oe-rel  FOR oe-rel.
DEF BUFFER b-oe-ordl FOR oe-ordl.

{custom/xprint.i}

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.
DEF VAR cColumnInit AS LOG INIT YES NO-UNDO.

ASSIGN cTextListToSelect = "Rep,Sales Rep Name,Cust #,Cust Name,Order PO,Item PO,Release PO," +
                           "Order#,Customer P/N,FG Item #,Item Name,Ord Date,Order Qty," +
                           "Inv/Rel Date,Inv#,Qty Produce,Qty To Produce," +
                           "Qty Shipped,Release Qty,Invoice Amt,Balance Due,Qty On-Hand"
       cFieldListToSelect = "sman,sname,cust,cname,ord-po,item-po,rel-po," +
                            "ord-no,cust-part,fgitem,i-name,ord-date,ord-qty," +
                            "invrel-dt,inv-no,qty-pro,qty-to-pro," +
                            "qty-shp,rel-qty,inv-amt,bal-due,qty-on-hand" 
       cFieldLength = "3,25,8,30,15,15,15," +  "8,15,15,30,10,11," + "12,6,11,14," + "11,11,11,11,11" 
       cFieldType = "c,c,c,c,c,c,c," + "c,c,c,c,c,i," + "c,c,i,i," + "i,i,i,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Rep,Sales Rep Name,Cust #,Cust Name,Order PO,Item PO,Release PO," +
                           "Order#,Customer P/N,FG Item #,Item Name,Ord Date,Order Qty," +
                           "Inv/Rel Date,Inv#,Qty Produce,Qty To Produce," +
                           "Qty Shipped,Release Qty,Invoice Amt,Balance Due,Qty On-Hand" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 tb_cust-list ~
btnCustList begin_cust-no end_cust-no begin_ord-date end_ord-date ~
begin_po-no end_po-no begin_job-no begin_job-no2 end_job-no end_job-no2 ~
begin_i-no end_i-no begin_slmn end_slmn rd_sort tb_break rd_jstat rd_ostat ~
fi_days-old tb_under as-of-date tb_job-qty tb_0-qoh tb_0-bal tb_sch ~
btn_SelectColumns rd-dest lines-per-page lv-ornt ~
lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust-no end_cust-no ~
begin_ord-date end_ord-date begin_po-no end_po-no begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_i-no end_i-no begin_slmn ~
end_slmn lbl_sort rd_sort tb_break lbl_jstat rd_jstat lbl_ostat rd_ostat ~
fi_days-old tb_under as-of-date tb_job-qty tb_0-qoh tb_0-bal tb_sch rd-dest ~
sl_avail lines-per-page lv-ornt lv-font-no lv-font-name td-show-parm ~
tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-bal C-Win 
FUNCTION get-bal RETURNS INTEGER
  (OUTPUT op-qoh AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-wip C-Win 
FUNCTION get-wip RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD orderQty C-Win 
FUNCTION orderQty RETURNS INTEGER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD producedQty C-Win 
FUNCTION producedQty RETURNS INTEGER
    (OUTPUT opBalance AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD shipQty C-Win 
FUNCTION shipQty RETURNS INTEGER
    (OUTPUT opBalance AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD wipQty C-Win 
FUNCTION wipQty RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

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

DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "As of" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

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

DEFINE VARIABLE begin_slmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

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

DEFINE VARIABLE end_slmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE fi_days-old AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Older Than" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-ordbal.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_jstat AS CHARACTER FORMAT "X(256)":U INITIAL "Job Status?" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_ostat AS CHARACTER FORMAT "X(256)":U INITIAL "Order Status?" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

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
"Due Date", "Due Date"
     SIZE 84 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 16.67.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 4.76.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 4.52 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 4.52 NO-UNDO.

DEFINE VARIABLE tb_0-bal AS LOGICAL INITIAL yes 
     LABEL "Include Zero Order Balance Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY 1.05 NO-UNDO.

DEFINE VARIABLE tb_0-qoh AS LOGICAL INITIAL yes 
     LABEL "Include Items with Zero QOH?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1.05 NO-UNDO.

DEFINE VARIABLE tb_break AS LOGICAL INITIAL no 
     LABEL "Page Break by Sales Rep?" 
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

DEFINE VARIABLE tb_job-qty AS LOGICAL INITIAL no 
     LABEL "Print Job Qty Details?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .95 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_sch AS LOGICAL INITIAL no 
     LABEL "Show Scheduled Releases?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .95 NO-UNDO.

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
     tb_cust-list AT ROW 1.76 COL 34.4 WIDGET-ID 6
     btnCustList AT ROW 1.86 COL 68 WIDGET-ID 8
     begin_cust-no AT ROW 2.81 COL 32 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.81 COL 80 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_ord-date AT ROW 3.76 COL 32 COLON-ALIGNED
     end_ord-date AT ROW 3.76 COL 80 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     begin_po-no AT ROW 4.71 COL 32 COLON-ALIGNED HELP
          "Enter Ending Customer PO Number"
     end_po-no AT ROW 4.71 COL 80 COLON-ALIGNED HELP
          "Enter Ending Customer PO Number"
     begin_job-no AT ROW 5.67 COL 32 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 5.67 COL 48 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 5.67 COL 80 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 5.67 COL 96 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_i-no AT ROW 6.62 COL 32 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_i-no AT ROW 6.62 COL 80 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_slmn AT ROW 7.57 COL 32 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slmn AT ROW 7.57 COL 80 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     lbl_sort AT ROW 8.76 COL 8 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 8.76 COL 20 NO-LABEL
     tb_break AT ROW 10.05 COL 66
     lbl_jstat AT ROW 10.14 COL 5 COLON-ALIGNED NO-LABEL
     rd_jstat AT ROW 10.14 COL 20 NO-LABEL
     lbl_ostat AT ROW 11.43 COL 3 COLON-ALIGNED NO-LABEL
     rd_ostat AT ROW 11.43 COL 20 NO-LABEL
     fi_days-old AT ROW 12.29 COL 81 COLON-ALIGNED
     tb_under AT ROW 12.91 COL 6
     as-of-date AT ROW 13.48 COL 76 COLON-ALIGNED
     tb_job-qty AT ROW 13.95 COL 6
     tb_0-qoh AT ROW 14.67 COL 68
     tb_0-bal AT ROW 15 COL 6
     tb_sch AT ROW 16.05 COL 6 WIDGET-ID 2
     btn_SelectColumns AT ROW 16.14 COL 63.8 WIDGET-ID 10
     rd-dest AT ROW 19.38 COL 13 NO-LABEL
     sl_avail AT ROW 19.52 COL 10 NO-LABEL WIDGET-ID 26
     lines-per-page AT ROW 19.62 COL 91 COLON-ALIGNED
     lv-ornt AT ROW 19.86 COL 38 NO-LABEL
     Btn_Add AT ROW 20.67 COL 13 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     sl_selected AT ROW 20.67 COL 15 NO-LABEL WIDGET-ID 28
     lv-font-no AT ROW 21.52 COL 41 COLON-ALIGNED
     Btn_Remove AT ROW 21.86 COL 13 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     lv-font-name AT ROW 22.48 COL 35 COLON-ALIGNED NO-LABEL
     btn_Up AT ROW 23.05 COL 13 WIDGET-ID 40
     td-show-parm AT ROW 23.67 COL 37
     tb_excel AT ROW 24.14 COL 80 RIGHT-ALIGNED
     tb_runExcel AT ROW 24.14 COL 102 RIGHT-ALIGNED
     btn_down AT ROW 24.24 COL 13 WIDGET-ID 42
     fi_file AT ROW 25.1 COL 58 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 26.76 COL 35
     btn-cancel AT ROW 26.76 COL 65
     "Days" VIEW-AS TEXT
          SIZE 6 BY 1 AT ROW 12.29 COL 90
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Only Show QOH that is..." VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 11.33 COL 68
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 109.8 BY 27.43.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 18.67 COL 12
     RECT-6 AT ROW 18.43 COL 1
     RECT-7 AT ROW 1.48 COL 1
     RECT-8 AT ROW 11.1 COL 66
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 109.8 BY 27.43.


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
         TITLE              = "Order Balance by PO# / Job"
         HEIGHT             = 27.43
         WIDTH              = 109.8
         MAX-HEIGHT         = 47.91
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 47.91
         VIRTUAL-WIDTH      = 256
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
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       begin_slmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

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
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       end_slmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_days-old:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
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

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_jstat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_ostat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR SELECTION-LIST sl_avail IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */

ASSIGN 
       sl_avail:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_selected IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       sl_selected:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tb_0-bal:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_0-qoh:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_break:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       tb_job-qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sch:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Order Balance by PO# / Job */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Order Balance by PO# / Job */
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


&Scoped-define SELF-NAME begin_slmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slmn C-Win
ON LEAVE OF begin_slmn IN FRAME FRAME-A /* Beginning Sales Rep# */
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
    lv-pdf-file = init-dir + "\OrderBal"
    is-xprint-form = NO.

  RUN GetSelectionList.
  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
      EMPTY TEMP-TABLE ttCustList.
      RUN BuildCustList(INPUT cocode,
                        INPUT tb_cust-list AND glCustListActive,
                        INPUT begin_cust-no,
                        INPUT END_cust-no).
  END.
  run run-report.
  STATUS DEFAULT "Processing Complete". 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= "Customer"
                            &begin_cust=begin_cust-no
                            &END_cust= begin_cust-no
                            &fax-subject=lv-report-title
                            &fax-body=lv-report-title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           is-xprint-form = YES.
           /*IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Customer"
                             &begin_cust= begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject=lv-report-title
                             &mail-body=lv-report-title
                             &mail-file=list-name }
                             */
            IF is-xprint-form THEN DO:
               RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
               {custom/asimail2.i &TYPE="CUSTOMER"
                             &group-title=v-prgmname
                             &begin_cust=begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject=lv-report-title
                             &mail-body=lv-report-title
                             &mail-file=lv-pdf-file + ".pdf" }

           END.
           ELSE DO:
               {custom/asimailr2.i &TYPE = "Customer"
                                  &group-title=v-prgmname
                                  &begin_cust= begin_cust-no
                                  &END_cust=begin_cust-no
                                  &mail-subject=lv-report-title
                                  &mail-body=lv-report-title
                                  &mail-file=list-name }

           END.
       END. 
      WHEN 6 THEN RUN output-to-port.
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


&Scoped-define SELF-NAME end_slmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slmn C-Win
ON LEAVE OF end_slmn IN FRAME FRAME-A /* Ending Sales Rep# */
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
     ELSE lines-per-page:SCREEN-VALUE = "45".     
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


&Scoped-define SELF-NAME tb_0-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_0-bal C-Win
ON VALUE-CHANGED OF tb_0-bal IN FRAME FRAME-A /* Include Zero Order Balance Items? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_0-qoh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_0-qoh C-Win
ON VALUE-CHANGED OF tb_0-qoh IN FRAME FRAME-A /* Include Items with Zero QOH? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_break
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_break C-Win
ON VALUE-CHANGED OF tb_break IN FRAME FRAME-A /* Page Break by Sales Rep? */
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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
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


&Scoped-define SELF-NAME tb_sch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sch C-Win
ON VALUE-CHANGED OF tb_sch IN FRAME FRAME-A /* Show Scheduled Releases? */
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
   as-of-date     = TODAY.

  FIND FIRST users WHERE
       users.user_id EQ USERID("NOSWEAT")
       NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     init-dir = users.user_program[2].
  ELSE
     init-dir = "c:\tmp".

  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "OZ9",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust-no.
    APPLY 'choose' TO btn_SelectColumns IN FRAME {&FRAME-NAME}.
    cColumnInit = NO.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'OZ9',
                          INPUT NO,
                          OUTPUT glCustListActive).

 {sys/inc/chblankcust.i ""OZ9""}

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
                            INPUT 'OZ9',
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
                                  INPUT 'OZ9').


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

  DEF VAR v-po-no LIKE oe-ord.po-no NO-UNDO.
  def var v-job   like oe-ord.job-no   extent 2 init ["","zzzzzz"].
  def var v-job2  like oe-ord.job-no2  format "99" extent 2 init [0,99].
  DEF VAR temp-job1 AS CHAR NO-UNDO.
  temp-job1 = TRIM(END_job-no).
  IF temp-job1 = "" THEN
      temp-job1 = "zzzzzz".
  ASSIGN
  v-job[1]   = fill(" ",6 - length(trim(begin_job-no))) +
              trim(begin_job-no) + string(int(begin_job-no2),"99")
  v-job[2]   = fill(" ",6 - length(trim(temp-job1)))   +
              temp-job1   + string(int(end_job-no2),"99").

 /* IF rd_prt-po EQ "Line" THEN
     v-po-no = oe-ordl.po-no.
  ELSE IF rd_prt-po EQ "Header" THEN
     v-po-no = oe-ord.po-no.
  ELSE
  DO: */
     IF AVAIL oe-rell THEN
        v-po-no = oe-rell.po-no.
     ELSE
     DO: 
        if avail ar-invl then do:
           find first b-oe-bolh
               where b-oe-bolh.company eq cocode
                 and b-oe-bolh.bol-no  eq ar-invl.bol-no
               no-lock no-error.
           if avail b-oe-bolh then do:
              find first b-oe-boll
                  where b-oe-boll.company eq cocode
                    and b-oe-boll.b-no    eq b-oe-bolh.b-no
                    and b-oe-boll.i-no    eq ar-invl.i-no
                  no-lock no-error.
              if avail b-oe-boll then do:
                find first b-oe-rell
                    where b-oe-rell.company eq cocode
                      and b-oe-rell.r-no    eq b-oe-boll.r-no
                      AND b-oe-rell.ord-no  EQ b-oe-boll.ord-no
                      and b-oe-rell.i-no    eq b-oe-boll.i-no
                      and b-oe-rell.line    eq b-oe-boll.line
                      no-lock no-error.

                IF AVAIL b-oe-rell THEN 
                   v-po-no = b-oe-rell.po-no. 
             END.
           END.
        END.
        ELSE IF AVAIL inv-line THEN DO:
           find first b-oe-bolh
               where b-oe-bolh.company eq cocode
                 and b-oe-bolh.b-no    eq inv-line.b-no
               no-lock no-error.
           if avail b-oe-bolh then do:
              find first b-oe-boll
                  where b-oe-boll.company eq cocode
                    and b-oe-boll.b-no    eq b-oe-bolh.b-no
                    and b-oe-boll.i-no    eq inv-line.i-no
                  no-lock no-error.
              if avail b-oe-boll then do:
                 find first b-oe-rell
                     where b-oe-rell.company eq cocode
                       and b-oe-rell.r-no    eq b-oe-boll.r-no
                       and b-oe-rell.i-no    eq b-oe-boll.i-no
                       and b-oe-rell.line    eq b-oe-boll.line
                     USE-INDEX r-no no-lock no-error.
                 if avail b-oe-rell THEN 
                    v-po-no = b-oe-rell.po-no.
              end.
           end.
        END.
        ELSE IF AVAIL oe-rel THEN
             v-po-no = oe-rel.po-no.

        ELSE IF AVAIL oe-ordl THEN
        DO:
           FIND FIRST b-oe-rel WHERE
                b-oe-rel.company EQ cocode AND
                b-oe-rel.ord-no EQ oe-ordl.ord-no
                NO-LOCK NO-ERROR.

           IF AVAIL b-oe-rel THEN
              v-po-no = b-oe-rel.po-no.
        END.
        else v-po-no = "".

       IF tb_sch THEN do:
        IF AVAIL oe-rel THEN
             v-po-no = oe-rel.po-no.
        ELSE v-po-no = "".
       END.

        IF NOT(v-po-no GE v-po[1] AND
               v-po-no LE v-po[2]) THEN NEXT.
     END.
 /* END.*/

 FIND FIRST job-hdr
        where job-hdr.company eq cocode
          and job-hdr.ord-no  EQ oe-ordl.ord-no
          AND job-hdr.i-no    EQ oe-ordl.i-no
          AND (fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) + string(job-hdr.job-no2,"99") ge v-job[1]
          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) + string(job-hdr.job-no2,"99") le v-job[2])
         use-index opened NO-LOCK NO-ERROR.


/*
IF NOT AVAIL job-hdr THEN DO:
    FIND FIRST job-hdr
           where job-hdr.company eq cocode
             and job-hdr.ord-no  EQ oe-ordl.ord-no
             AND job-hdr.i-no    EQ oe-ordl.i-no NO-LOCK NO-ERROR.
    MESSAGE "here2" AVAIL(job-hdr) oe-ordl.ord-no oe-ordl.i-no VIEW-AS ALERT-BOX.

END.
*/
  IF NOT AVAIL job-hdr THEN
      NEXT.

  CREATE tt-report.
  ASSIGN
   tt-report.term-id = ""
   tt-report.key-01  = IF tb_break THEN oe-ordl.s-man[1] ELSE ""
   tt-report.key-02  = oe-ord.cust-no
   tt-report.key-03  = IF v-sort EQ "P" THEN v-po-no
                       ELSE 
                       IF v-sort EQ "I" THEN
                         (STRING(oe-ordl.i-no,"x(15)") + v-po-no)
                       ELSE
                       IF v-sort EQ "C" THEN
                         (STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))
                       ELSE
                       IF v-sort EQ "F" THEN
                         (STRING(oe-ordl.i-name,"x(30)") + STRING(oe-ord.ord-no,"99999999999"))
                       ELSE
                       IF v-sort EQ "O" THEN
                         (STRING(oe-ord.ord-no,"99999999999") + oe-ordl.part-no)
                       ELSE  
                         (STRING(YEAR(oe-ordl.req-date),"9999") +
                          STRING(MONTH(oe-ordl.req-date),"99")  +
                          STRING(DAY(oe-ordl.req-date),"99")    +
                          STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))              
   tt-report.key-04  = FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +
                       TRIM(job-hdr.job-no) + "-" +
                       STRING(job-hdr.job-no2,"99")
   tt-report.key-05  = STRING(oe-ord.ord-no,"99999999999")
   tt-report.key-06  = oe-ordl.i-no
   tt-report.key-07  = STRING(YEAR(ip-date),"9999") +
                       STRING(MONTH(ip-date),"99")  +
                       STRING(DAY(ip-date),"99")
   tt-report.po-no   = v-po-no
   tt-report.rec-id  = ip-recid
   tt-report.row-id  = ROWID(oe-ordl)
   v-ordl            = no
     .

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

  DEF VAR li AS INT NO-UNDO.

  IF AVAIL oe-ordl THEN
  DO:
     IF oe-ordl.job-no NE "" THEN
        FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
           WHERE fg-rcpth.company   EQ cocode
             AND fg-rcpth.job-no    EQ oe-ordl.job-no
             AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
             AND fg-rcpth.i-no      EQ oe-ordl.i-no
             AND fg-rcpth.rita-code EQ "R"
           USE-INDEX job,
           EACH fg-rdtlh FIELDS(qty) NO-LOCK
           WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
             AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
             li = li + fg-rdtlh.qty.
        END.
     ELSE
     DO:
        FOR EACH job-hdr FIELDS(job-no job-no2) WHERE
            job-hdr.company EQ cocode AND
            job-hdr.ord-no EQ oe-ordl.ord-no AND
            job-hdr.i-no EQ oe-ordl.i-no
            USE-INDEX ord-no
            NO-LOCK,
            EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
           WHERE fg-rcpth.company   EQ cocode
             AND fg-rcpth.job-no    EQ job-hdr.job-no
             AND fg-rcpth.job-no2   EQ job-hdr.job-no2
             AND fg-rcpth.i-no      EQ oe-ordl.i-no
             AND fg-rcpth.rita-code EQ "R"
           USE-INDEX job,
           EACH fg-rdtlh FIELDS(qty) NO-LOCK
           WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
             AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
             li = li + fg-rdtlh.qty.
        END.
     END.

     IF oe-ordl.po-no-po NE 0 THEN
        FOR EACH fg-rcpth FIELDS(r-no rita-code) WHERE
            fg-rcpth.company   EQ cocode AND
            fg-rcpth.po-no     EQ STRING(oe-ordl.po-no-po) AND
            fg-rcpth.i-no      EQ oe-ordl.i-no AND
            fg-rcpth.rita-code EQ "R"
            NO-LOCK,
            EACH fg-rdtlh FIELDS(qty) WHERE
                 fg-rdtlh.r-no EQ fg-rcpth.r-no AND
                 fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                 NO-LOCK:
                 li = li + fg-rdtlh.qty.
        END.
  END.

  ASSIGN  tt-report.prod-qty    = li .

  IF NOT CAN-FIND(FIRST tt-fg-bin WHERE tt-fg-bin.i-no EQ tt-report.key-06) THEN RUN calc-qoh.

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
  DEF VAR lv-tag        LIKE fg-rdtlh.tag NO-UNDO.
  DEF VAR ld-last       AS   DATE NO-UNDO.

  DEF BUFFER b-f-rc for fg-rcpth.
  DEF BUFFER b-f-rd for fg-rdtlh.


  ASSIGN
   vdat     = as-of-date
   v-curr   = YES
   v-q-or-v = YES.

  FOR EACH itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ tt-report.key-06
      NO-LOCK,
      EACH fg-bin
      WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
      NO-LOCK:

    CREATE tt-fg-bin.
    BUFFER-COPY fg-bin EXCEPT rec_key TO tt-fg-bin.

    IF fi_days-old NE 0 THEN DO:
      tt-fg-bin.qty = 0.

      FOR EACH fg-rcpth
          WHERE fg-rcpth.company    EQ cocode
            AND fg-rcpth.i-no       EQ fg-bin.i-no
            AND fg-rcpth.job-no     EQ fg-bin.job-no
            AND fg-rcpth.job-no2    EQ fg-bin.job-no2
            AND fg-rcpth.trans-date LE as-of-date
          NO-LOCK USE-INDEX tran,

          EACH fg-rdtlh
          WHERE fg-rdtlh.r-no       EQ fg-rcpth.r-no
            AND fg-rdtlh.loc        EQ fg-bin.loc
            AND fg-rdtlh.loc-bin    EQ fg-bin.loc-bin
            AND fg-rdtlh.tag        EQ fg-bin.tag
            AND fg-rdtlh.cust-no    EQ fg-bin.cust-no
            AND fg-rdtlh.rita-code  EQ fg-rcpth.rita-code
          NO-LOCK

          BREAK BY fg-bin.i-no
                BY fg-bin.job-no
                BY fg-bin.job-no2
                BY fg-bin.loc
                BY fg-bin.loc-bin
                BY fg-bin.tag
                BY fg-rcpth.trans-date
                BY fg-rdtlh.trans-time
                BY fg-rcpth.r-no:

        IF FIRST(fg-bin.i-no) THEN
          ASSIGN
           v-cst[1] = 0
           v-val[1] = 0
           v-qohi   = 0.

        {fg/rep/fg-aging.i fi_days-old}

        if last-of(fg-bin.tag) then do:
          v-qtyc = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                   v-qohj[4] + v-qohj[5] + v-qohj[6].

          if v-qohj[6] lt 0 then do:
            v-qty = v-qohj[6] * -1.

            do v = 5 to 1 by -1:
              if v-qohj[v] gt 0 then
                assign
                 v-red     = min(v-qty,v-qohj[v])
                 v-qohj[v] = v-qohj[v] - v-red
                 v-qty     = v-qty     - v-red.

              if v-qty le 0 then leave.
            end.

            if v-qty gt 0 then v-qohi[6] = v-qohi[6] - v-qty.
          end.

          /*release oe-ordl.*/ 
          if fg-bin.job-no ne "" then
          find last b-oe-ordl
              where b-oe-ordl.company eq cocode
                and b-oe-ordl.job-no  eq fg-bin.job-no
                and b-oe-ordl.job-no2 eq fg-bin.job-no2
                and b-oe-ordl.i-no    eq fg-rcpth.i-no
              use-index job no-lock no-error.

          if not v-curr then
            assign
             v-qohj[1] = 0
             v-qohj[2] = 0
             v-qohj[3] = 0.

          assign
           v-qty     = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                       v-qohj[4] + v-qohj[5]
           v-qohi[1] = v-qohi[1] + v-qohj[1]
           v-qohi[2] = v-qohi[2] + v-qohj[2]
           v-qohi[3] = v-qohi[3] + v-qohj[3]
           v-qohi[4] = v-qohi[4] + v-qohj[4]
           v-qohi[5] = v-qohi[5] + v-qohj[5]
           v-qohj    = 0.

          if avail b-oe-ordl then
            assign
             v-u-cst  = b-oe-ordl.t-cost / b-oe-ordl.qty
             v-u-val  = b-oe-ordl.t-price / b-oe-ordl.qty.

          else do:
            if itemfg.prod-uom eq "EA" then
              v-u-cst = itemfg.total-std-cost.
            else
              run sys/ref/convcuom.p(itemfg.prod-uom, "EA", 0, 0, 0, 0,
                                   itemfg.total-std-cost, output v-u-cst).

            if itemfg.sell-uom eq "EA" then
              v-u-val = itemfg.sell-price.
            else
              run sys/ref/convcuom.p(itemfg.sell-uom, "EA", 0, 0, 0, 0,
                                     itemfg.sell-price, output v-u-val).
          end.

          if v-u-cst eq ? then v-u-cst = 0.
          if v-u-val eq ? then v-u-val = 0.

          assign
           v-cst[1] = v-cst[1] + (v-qty * v-u-cst)
           v-val[1] = v-val[1] + (v-qty * v-u-val).
        end.

        if last-of(fg-bin.i-no) then do:
          if v-qohi[6] lt 0 then do:
            v-qty = v-qohi[6] * -1.

            do v = 5 to 1 by -1:
              if v-qohi[v] gt 0 then
                assign
                 v-red     = min(v-qty,v-qohi[v])
                 v-qohi[v] = v-qohi[v] - v-red
                 v-qty     = v-qty     - v-red.

              if v-qty le 0 then leave.
            end.

            if v-qty gt 0 then
              assign
               v-qohi   = 0
               v-cst[1] = 0
               v-val[1] = 0.
          end.

          if v-cst[1] lt 0 then v-cst[1] = 0.
          if v-val[1] lt 0 then v-val[1] = 0.

          if not v-q-or-v then do:
             v-qty = v-qohi[1] + v-qohi[2] + v-qohi[3] + v-qohi[4] + v-qohi[5].

            do v = 1 to 5:
               v-qohi[v] = v-val[1] / v-qty * v-qohi[v].

               if v-qohi[v] eq ? then v-qohi[v] = 0.
            end.
          end.

          tt-fg-bin.qty = v-qohi[2] + v-qohi[3] + v-qohi[4] + v-qohi[5].
        end.
      END.
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
  DISPLAY tb_cust-list begin_cust-no end_cust-no begin_ord-date end_ord-date 
          begin_po-no end_po-no begin_job-no begin_job-no2 end_job-no 
          end_job-no2 begin_i-no end_i-no begin_slmn end_slmn lbl_sort rd_sort 
          tb_break lbl_jstat rd_jstat lbl_ostat rd_ostat fi_days-old tb_under 
          as-of-date tb_job-qty tb_0-qoh tb_0-bal tb_sch rd-dest  
          lines-per-page lv-ornt lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 RECT-8 tb_cust-list btnCustList begin_cust-no 
         end_cust-no begin_ord-date end_ord-date begin_po-no end_po-no 
         begin_job-no begin_job-no2 end_job-no end_job-no2 begin_i-no end_i-no 
         begin_slmn end_slmn rd_sort tb_break rd_jstat rd_ostat fi_days-old 
         tb_under as-of-date tb_job-qty tb_0-qoh tb_0-bal tb_sch 
         btn_SelectColumns rd-dest lines-per-page lv-ornt 
         lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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

 DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.  
    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
           ttRptSelected.DisplayOrder = i
           ttRptSelected.HeadingFromLeft = IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
           iColumnLength = iColumnLength + ttRptSelected.FieldLength + 1.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ oe/rep/backlog.p 10/94 gb */
/* Order Backlog Summary / Detail Report                                      */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-top3w.f "Hot Keys O-Z-9"}*/

DEF BUFFER b-tt-report FOR tt-report.
DEF BUFFER b-oe-rell FOR oe-rell.

def var v-cust  like oe-ord.cust-no  extent 2 init ["","zzzzzzzz"].
def var v-date  like ar-inv.inv-date format "99/99/9999"
                                     extent 2 init [today, 12/31/9999].
def var v-job   like oe-ord.job-no   extent 2 init ["","zzzzzz"].
def var v-job2  like oe-ord.job-no2  format "99" extent 2 init [0,99].
def var v-item  like oe-ordl.i-no    extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-inc   as   log             format "Yes/No" init yes.
def var v-stat  as   char format "!" init "A".
def var v-ostat as   char format "!" init "A".
def var v-bal   as   log format "BalDue/InvAmt" init yes.
def var v-jobq  as   log             format "Yes/No" init no.

def var v-dat     as   date format "99/99/99".
def var v-bal-qty as   int format ">>>,>>>,>>9".
def var v-inv-amt as   int format ">>>>,>>9.99".
def var v-cust-no like cust.cust-no.
def var v-name    like cust.name.
def var v-sman    like sman.sman.
def var v-sname   like sman.sname.
def var v-field1  as   char format "x(52)".
def var v-label   as   char format "x(11)" init "Invoice Amt".
def var v-field2  like v-label.
def var v-ord-no  as   char.
def var v-q-onh   like itemfg.q-onh NO-UNDO.
def var v-q-shp   like v-q-onh NO-UNDO.
def var v-q-rel   like v-q-onh NO-UNDO.
def var v-q-wip   like v-q-onh NO-UNDO.
def var v-q-avl   like v-q-onh NO-UNDO.
DEF VAR begin_due-date AS DATE NO-UNDO.
DEF VAR end_due-date AS DATE NO-UNDO.
DEF VAR rd_due-date AS CHAR NO-UNDO.
DEF VAR lv-stat AS CHAR NO-UNDO.
DEF VAR lv-due-date LIKE oe-ordl.req-date NO-UNDO.
DEF VAR act-rel-qty AS INT NO-UNDO.

def var v-time as int.
v-time = time.
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
DEFINE VARIABLE lSelected AS LOGICAL INIT YES NO-UNDO.

{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF VAR excelheader AS CHAR NO-UNDO.

FORMAT HEADER
       SKIP(1)
       "Sales Rep:"
       v-sman FORMAT "x(8)"
       v-sname

    WITH FRAME r-top1 STREAM-IO WIDTH 220 NO-BOX PAGE-TOP.

FORMAT HEADER
       SKIP(1)
       "Customer:"
       v-cust-no
       v-name
       SKIP(1)
       "PO Number      "
       "Order#/Job#"
       "Customer P/N   "
       "FG Item #      "
       "Item Name #    "
       "Ord Date"
       "  Order Qty"
       "Inv/Rel Date"
       "Inv#  "
       "QtyProduced"
       "Qty to Produce"
       "Qty Shipped"
       "Release Qty"
       v-label
       "Qty On-Hand"
       SKIP
       "---------------"
       "-----------"
       "---------------"
       "---------------"
       "---------------"
       "--------"
       "-----------"
       "------------"
       "------"
       "-----------"
       "--------------"
       "-----------"
       "-----------"
       "-----------"
       "-----------"

    WITH FRAME r-top2 STREAM-IO WIDTH 220 NO-BOX PAGE-TOP.


SESSION:SET-WAIT-STATE ("general").

ASSIGN
 str-tit2 = c-win:TITLE 
 str-tit3 = "By Customer" 
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
 v-sort     = substr(rd_sort,1,1)
 v-inc      = tb_0-bal
 v-stat     = substr(rd_jstat,1,1)
 v-ostat    = substr(rd_ostat,1,1)
 v-jobq     = tb_job-qty
 /*v-bal      = rd_prt-baldue eq "Balance Due"*/ 
 lSelected  = tb_cust-list .

if v-bal then v-label = "Balance Due".
IF v-item[2] = "" THEN
    v-item[2] = "zzzzzzzzzzzzz".

IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN v-cust[1] = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN v-cust[2] = ttCustList.cust-no .
END.

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
   PUT "<PDF-EXCLUDE=MS Mincho></PROGRESS><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><P7>" FORM "x(150)" SKIP.
END. 
OUTPUT CLOSE.
{sys/inc/outprint.i "value(lines-per-page) append" }                                     /**/

VIEW FRAME r-top.

IF td-show-parm THEN RUN show-param.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
 /* IF tb_break THEN
     excelheader = "Sales Rep ID,Sales Rep Name,".
  /* wfk - took out job# after order # */
  excelheader = excelheader + "Cust #,Cust Name,PO Number,Order#,Customer P/N,"
               + "FG Item #,Item Name,Ord Date,Order Qty,Inv/Rel Date,Inv#,Qty Produce,Qty To Produce,"
               + "Qty Shipped,Release Qty," + v-label + ",Qty On-Hand". */

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

/*IF tb_break THEN VIEW FRAME r-top1.*/

VIEW FRAME r-top.

EMPTY TEMP-TABLE tt-report.
EMPTY TEMP-TABLE tt-fg-bin.

{oerep/r-ordbjN.i}

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
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


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-bal C-Win 
FUNCTION get-bal RETURNS INTEGER
  (OUTPUT op-qoh AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE iTotalJobOnHandQty AS INTEGER     NO-UNDO.
DEFINE BUFFER bf-job-hdr FOR job-hdr .
/*   IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN */

    FOR EACH bf-job-hdr FIELDS(company job-no job-no2 i-no)
        WHERE bf-job-hdr.company EQ cocode
        AND bf-job-hdr.ord-no EQ oe-ordl.ord-no 
        AND bf-job-hdr.i-no EQ oe-ordl.i-no
        USE-INDEX ord-no
        NO-LOCK
        BREAK BY bf-job-hdr.job-no BY bf-job-hdr.job-no2 BY bf-job-hdr.i-no:
        IF LAST-OF(bf-job-hdr.i-no) THEN 
        DO:    
            FOR EACH fg-bin FIELDS (qty)
                WHERE fg-bin.company EQ bf-job-hdr.company
                AND fg-bin.job-no  EQ bf-job-hdr.job-no
                AND fg-bin.job-no2 EQ bf-job-hdr.job-no2
                AND fg-bin.i-no    EQ bf-job-hdr.i-no
                NO-LOCK:
                iTotalJobOnHandQty = iTotalJobOnHandQty + fg-bin.qty.
            END.
        END.
    END.
    op-qoh = iTotalJobOnHandQty.
RETURN iTotalJobOnHandQty.    /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-wip C-Win 
FUNCTION get-wip RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  DEF BUFFER b-oe-ordl FOR oe-ordl.


  FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK NO-ERROR.

  rtnValue = oe-ordl.qty - (get-bal(li-qoh) + oe-ordl.ship-qty).
  IF rtnValue LT 0 OR
     rtnValue LT oe-ordl.qty * b-oe-ordl.under-pct / 100 THEN
  rtnValue = 0.
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION orderQty C-Win 
FUNCTION orderQty RETURNS INTEGER
    ( /* parameter-definitions */ ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

    IF AVAILABLE job-hdr /* AND AVAILABLE job AND job.opened = TRUE */ THEN DO:
      FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                   AND oe-ordl.i-no EQ job-hdr.i-no
                                   AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
      IF AVAILABLE oe-ordl THEN
      rtnValue = oe-ordl.qty.
    END. /* avail job-hdr */
    RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION producedQty C-Win 
FUNCTION producedQty RETURNS INTEGER
    (OUTPUT opBalance AS INTEGER) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE job-hdr /*AND AVAILABLE job AND job.opened = TRUE */ THEN DO:

    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.i-no EQ job-hdr.i-no
                                 AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN
    DO:
       IF oe-ordl.job-no NE '' THEN
          FOR EACH fg-rcpth fields(r-no rita-code) NO-LOCK
             WHERE fg-rcpth.company EQ oe-ordl.company
               AND fg-rcpth.job-no EQ oe-ordl.job-no
               AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
               AND fg-rcpth.i-no EQ oe-ordl.i-no
               AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
              EACH fg-rdtlh FIELDS(qty) NO-LOCK
             WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
               AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
              rtnValue = rtnValue + fg-rdtlh.qty.

       END.
      ELSE
         FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
             WHERE fg-rcpth.company   EQ cocode
               AND fg-rcpth.job-no    EQ job-hdr.job-no
               AND fg-rcpth.job-no2   EQ job-hdr.job-no2
               AND fg-rcpth.i-no      EQ oe-ordl.i-no
               AND fg-rcpth.rita-code EQ "R"
               USE-INDEX job,
             EACH fg-rdtlh FIELDS(qty) NO-LOCK WHERE
                  fg-rdtlh.r-no      EQ fg-rcpth.r-no AND
                  fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                  rtnValue = rtnValue + fg-rdtlh.qty.

         END.
    END.
  END. /* avail job-hdr */
  opBalance = rtnValue.
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION shipQty C-Win 
FUNCTION shipQty RETURNS INTEGER
    (OUTPUT opBalance AS INTEGER) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.
  DEF VAR li-inv-qty LIKE oe-ordl.inv-qty NO-UNDO.
  DEF VAR li-ship-qty LIKE oe-ordl.ship-qty NO-UNDO.

  IF AVAILABLE job-hdr /* AND AVAILABLE job AND job.opened = TRUE */ THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.i-no EQ job-hdr.i-no
                                 AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN DO:
      RUN oe/ordlsqty.p (ROWID(oe-ordl),
                         OUTPUT li-inv-qty, OUTPUT li-ship-qty).

      rtnValue = li-ship-qty.
    END.
  END. /* avail job-hdr */
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION wipQty C-Win 
FUNCTION wipQty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE job-hdr /* AND AVAILABLE job AND job.opened = TRUE */ THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.i-no EQ job-hdr.i-no
                                 AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN DO:
      FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
      rtnValue = oe-ordl.qty - (qtyOnHand + oe-ordl.ship-qty).
      IF rtnValue LT 0 OR
         rtnValue LT oe-ordl.qty *
                     (IF AVAIL oe-ord THEN oe-ordl.under-pct ELSE 100) / 100 THEN
        rtnValue = 0.
    END. /* avail oe-ordl */
  END. /* avail job-hdr */
  RETURN rtnValue.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

