&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-sched3.w

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

def var v-fcust like cust.cust-no extent 2 init ["","zzzzzzzz"] no-undo.
def var v-ford-no as int format ">>>>>>" extent 2 init [0,999999] no-undo.
def var v-fdate as date extent 2 format "99/99/9999" init [today, 12/31/9999] no-undo.
def var v-fitem as char format "x(15)" extent 2 init ["","zzzzzzzzzzzzzzz"] no-undo.
def var v-fsman as char format "xxx" extent 2 init ["","zzz"] no-undo.
def var v-fcarr as char format "x(5)" extent 2 init ["","zzzzz"] no-undo.
DEF VAR v-floc AS CHAR FORMAT 'X(5)' EXTENT 2 INIT ['','zzzzz'] NO-UNDO.
def var v-ponum as log init yes no-undo.
def var v-sort as char format "!" init "C" no-undo.
def var v-print as char format "!" init "I" no-undo.
def var v-types as char format "x(7)" init "PALSBIC" no-undo.
def var v-comps as log init no no-undo.
def var v-by-job as log init no no-undo.
def var chosen as int init 3 no-undo.
def var v-qty like oe-rel.qty no-undo.
def var v-date like oe-rel.rel-date no-undo.
def var v-po-no like oe-rel.po-no no-undo.
def var v-rel-no like oe-rel.rel-no no-undo.
def var v-ship-id like oe-rel.ship-id no-undo.
def var v-carrier like oe-rel.carrier no-undo.  
def var v-type as char no-undo.
def var v-tot-qty as int format "->>>,>>>,>>9" EXTENT 2 no-undo.
def var v-tot-val as DEC format "->>>,>>>,>>9" extent 2 no-undo.
def var v-tot-msf as dec format "->>>,>>9.999" extent 2 no-undo.
DEF VAR ld-qty-ord AS DEC FORMAT ">>>,>>>,>>9" NO-UNDO.
DEF VAR ld-qty-rec AS DEC FORMAT ">>>,>>>,>>9" NO-UNDO.
DEF VAR ll-po AS LOG NO-UNDO.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR v-qty-opt AS CHAR NO-UNDO.

def TEMP-TABLE w-ord
  field ord-no like oe-ord.ord-no
  field est-no like oe-ord.est-no
  field onh-qty like itemfg.q-onh
  field cust-no like oe-ord.cust-no
  field cust-name like oe-ord.cust-name
  field part-no like oe-ordl.part-no
  field i-no like oe-ordl.i-no
  field i-name like oe-ordl.i-name
  field qty like oe-ordl.qty
  field cost like oe-ordl.cost
  field price like oe-ordl.price
  field t-price like oe-ordl.t-price format "->>,>>>,>>9"
  field rel-qty like oe-rel.qty
  field rel-date as char format "x(9)"
  field job as char format "x(9)"
  field job-no like oe-ordl.job-no
  field job-no2 like oe-ordl.job-no2
  field rel-no like oe-rel.rel-no
  field ship-id like oe-rel.ship-id
  field po-num like oe-ordl.po-no
  field ord-qty like oe-ordl.qty
  field shp-qty like oe-ordl.ship-qty
  field msf as dec format "->>9.999"
  field component as int
  field prom-code like oe-ordl.prom-code FORMAT 'X(5)'
  field last-date like oe-ord.last-date format "99/99/99"
  field carrier like oe-relh.carrier
  field is-a-component like oe-ordl.is-a-component
  field palls as int format "->>,>>>,>>9"
  FIELD xls-rel-date  LIKE oe-rel.rel-date format "99/99/99"
  FIELD xls-status    AS CHAR
  FIELD iPro-qty  AS INTEGER .

def buffer b-w-ord for w-ord.

{fg/fullset.i new}

{custom/formtext.i NEW}

DEF VAR tb_prt-qoh AS LOG NO-UNDO.
DEF VAR tb_prt-last AS LOG NO-UNDO.
DEF VAR rd_print2 AS CHAR NO-UNDO.
DEF VAR rd_print3 AS CHAR NO-UNDO.

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
def {1} SHARED var v-print-fmt  as char NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR ll-secure AS LOG NO-UNDO.

DEF TEMP-TABLE tt-report LIKE report FIELD qty LIKE oe-rell.qty
                                     FIELD onh LIKE fg-bin.qty
                                     FIELD ord-no  AS INTEGER 
                                     FIELD i-no    AS CHARACTER 
                                     FIELD rel-date AS DATE
                                     FIELD pro-qty AS INTEGER  .
DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.


ASSIGN cTextListToSelect = "Customer#,Rel Date,Rel Num,Ship To,Carrier,Order #,Cust Part#,Descrption,Fg Item#," +
                           "Po Number,Qty On Hand,Release Qty,Sales Value,Skids,Status,Projected Qty"
       cFieldListToSelect = "cust,rel-date,rel-num,ship,carr,ord,cust-part,desc,fg-item," +
                            "po-num,Qty-hand,rel-qty,sales,skid,stat,proj-qty"
       cFieldLength = "9,8,7,8,7,8,15,25,15," + "15,11,11,15,7,6,13"
       cFieldType = "c,c,i,c,c,i,c,c,c," + "c,i,i,i,i,c,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Customer#,Rel Date,Rel Num,Ship To,Carrier,Order #,Cust Part#,Descrption,Fg Item#," +
                           "Po Number,Qty On Hand,Projected Qty,Release Qty,Sales Value,Skids,Status" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust-no end_cust-no begin_ord-no ~
end_ord-no begin_i-no end_i-no begin_loc end_loc begin_slsmn end_slsmn ~
begin_date end_date begin_carr end_carr tb_actual tb_backordered rd_sort ~
tb_show-only sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up ~
btn_down rd-dest lv-ornt lv-font-no lines-per-page td-show-parm tb_excel ~
tb_runExcel fi_file btn-ok btn-cancel tb_pro-qty RECT-6 RECT-7 RECT-11 ~
tb_cust-list btnCustList 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_ord-no ~
end_ord-no begin_i-no end_i-no begin_loc end_loc begin_slsmn end_slsmn ~
begin_date end_date begin_carr end_carr tb_actual tb_backordered rd_sort ~
tb_show-only sl_avail sl_selected rd-dest lv-ornt lv-font-no lines-per-page ~
lv-font-name td-show-parm tb_excel tb_runExcel fi_file tb_pro-qty ~
tb_cust-list 

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

DEFINE VARIABLE begin_carr AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Carrier#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_carr AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Carrier#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-sched3.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 55.2 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "L" 
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
     SIZE 99 BY 1.19 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer#", "Customer#",
"Release Date", "Release Date",
"Item#", "Item#",
"Item Name", "Item Name",
"Territory", "Territory",
"Carrier", "Carrier",
"Credit Rating", "Credit Rating"
     SIZE 95 BY 1.05 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96.4 BY 6.43.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 123 BY 6.43.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 123 BY 18.81.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_actual AS LOGICAL INITIAL yes 
     LABEL "Actual" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.2 BY 1.33 NO-UNDO.

DEFINE VARIABLE tb_backordered AS LOGICAL INITIAL yes 
     LABEL "Backorder" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.2 BY 1.33 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.8 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_pro-qty AS LOGICAL INITIAL yes 
     LABEL "Projected Qty?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.2 BY 1.33 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_show-only AS LOGICAL INITIAL no 
     LABEL "Show only releases with Qty > On Hand?" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 2.81 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.81 COL 90 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_ord-no AT ROW 3.76 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord-no AT ROW 3.76 COL 90 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_i-no AT ROW 4.71 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 4.71 COL 90 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_loc AT ROW 5.67 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Warehouse"
     end_loc AT ROW 5.67 COL 90 COLON-ALIGNED HELP
          "Enter Ending Warehouse"
     begin_slsmn AT ROW 6.62 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 6.62 COL 90 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_date AT ROW 7.57 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 7.57 COL 90 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_carr AT ROW 8.52 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Carrier Number"
     end_carr AT ROW 8.52 COL 90 COLON-ALIGNED HELP
          "Enter Ending Carrier Number"
     tb_actual AT ROW 10.67 COL 46
     tb_backordered AT ROW 10.71 COL 58.2
     rd_sort AT ROW 11.76 COL 25 NO-LABEL
     tb_show-only AT ROW 9.86 COL 45.8
     sl_avail AT ROW 13.86 COL 26 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 13.71 COL 62 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 13.86 COL 81.4 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 14.71 COL 62 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 15.71 COL 62 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 16.76 COL 62 WIDGET-ID 40
     btn_down AT ROW 17.76 COL 62 WIDGET-ID 42
     rd-dest AT ROW 21.14 COL 3.6 NO-LABEL
     lv-ornt AT ROW 22.62 COL 3 NO-LABEL
     lv-font-no AT ROW 23.91 COL 6.6 COLON-ALIGNED
     lines-per-page AT ROW 23.91 COL 30.8 COLON-ALIGNED
     lv-font-name AT ROW 25.19 COL 2.8 NO-LABEL
     td-show-parm AT ROW 23.91 COL 41
     tb_excel AT ROW 23.95 COL 99.6 RIGHT-ALIGNED
     tb_runExcel AT ROW 23.95 COL 121.6 RIGHT-ALIGNED
     fi_file AT ROW 25.05 COL 77.6 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 26.52 COL 29.4
     btn-cancel AT ROW 26.52 COL 77.4
     tb_pro-qty AT ROW 9.76 COL 91.6 WIDGET-ID 58
     tb_cust-list AT ROW 1.81 COL 42 WIDGET-ID 6
     btnCustList AT ROW 1.91 COL 75.6 WIDGET-ID 60
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 20.14 COL 3.8 WIDGET-ID 38
     "Release Types:" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 10.71 COL 28
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.29 COL 26
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     "Sort Options:" VIEW-AS TEXT
          SIZE 13 BY 1 AT ROW 11.71 COL 10.8 WIDGET-ID 12
     "Print Options" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 9.76 COL 30.4 WIDGET-ID 14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 123.4 BY 27.19.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 13.14 COL 81.4 WIDGET-ID 44
     RECT-6 AT ROW 19.95 COL 1
     RECT-7 AT ROW 1 COL 1
     RECT-11 AT ROW 13 COL 23.6 WIDGET-ID 8
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 123.4 BY 27.19.


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
         TITLE              = "Actual Releases"
         HEIGHT             = 27.19
         WIDTH              = 123.4
         MAX-HEIGHT         = 27.19
         MAX-WIDTH          = 123.4
         VIRTUAL-HEIGHT     = 27.19
         VIRTUAL-WIDTH      = 123.4
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
       begin_carr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_loc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       end_carr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_loc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_actual:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_backordered:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       tb_pro-qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_show-only:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Actual Releases */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Actual Releases */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_carr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_carr C-Win
ON LEAVE OF begin_carr IN FRAME FRAME-A /* Beginning Carrier# */
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


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
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


&Scoped-define SELF-NAME begin_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_loc C-Win
ON LEAVE OF begin_loc IN FRAME FRAME-A /* Beginning Warehouse */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Beginning Order# */
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
    ASSIGN {&DISPLAYED-OBJECTS}.
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
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Customer"
                             &begin_cust= begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Customer"
                                  &begin_cust= begin_cust-no
                                  &END_cust=begin_cust-no
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
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


&Scoped-define SELF-NAME end_carr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_carr C-Win
ON LEAVE OF end_carr IN FRAME FRAME-A /* Ending Carrier# */
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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
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


&Scoped-define SELF-NAME end_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_loc C-Win
ON LEAVE OF end_loc IN FRAME FRAME-A /* Ending Warehouse */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* Ending Order# */
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


&Scoped-define SELF-NAME tb_backordered
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_backordered C-Win
ON VALUE-CHANGED OF tb_backordered IN FRAME FRAME-A /* Backorder */
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


&Scoped-define SELF-NAME tb_pro-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_pro-qty C-Win
ON VALUE-CHANGED OF tb_pro-qty IN FRAME FRAME-A /* Projected Qty? */
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


&Scoped-define SELF-NAME tb_show-only
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-only C-Win
ON VALUE-CHANGED OF tb_show-only IN FRAME FRAME-A /* Show only releases with Qty > On Hand? */
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

  begin_date = today.
  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "OZ4",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_cust-no.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'OZ4',
                          INPUT NO,
                          OUTPUT glCustListActive).

 {sys/inc/chblankcust.i ""OZ4""}

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
                            INPUT 'OZ4',
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
                                  INPUT 'OZ4').


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
  DISPLAY begin_cust-no end_cust-no begin_ord-no end_ord-no begin_i-no end_i-no 
          begin_loc end_loc begin_slsmn end_slsmn begin_date end_date begin_carr 
          end_carr tb_actual tb_backordered rd_sort tb_show-only sl_avail 
          sl_selected rd-dest lv-ornt lv-font-no lines-per-page lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file tb_pro-qty tb_cust-list 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_cust-no end_cust-no begin_ord-no end_ord-no begin_i-no end_i-no 
         begin_loc end_loc begin_slsmn end_slsmn begin_date end_date begin_carr 
         end_carr tb_actual tb_backordered rd_sort tb_show-only sl_avail 
         Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt 
         lv-font-no lines-per-page td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel tb_pro-qty RECT-6 RECT-7 RECT-11 tb_cust-list 
         btnCustList 
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
   /*   DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
      /*   CREATE-TEST-FILE*/
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
  RUN custom\d-print.w (list-name).
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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
DEF BUFFER b-oe-ordl FOR oe-ordl.

  DEF VAR lv-qty LIKE oe-rell.qty NO-UNDO.
  DEF VAR lv-subt AS CHAR NO-UNDO.
  DEF VAR lv-cr-rating LIKE cust.cr-rating NO-UNDO.
  DEF VAR ll-show-top-only AS LOG NO-UNDO.
  DEF VAR ld-palls AS DEC NO-UNDO.
  DEF VAR tb_notes AS LOG NO-UNDO.
  DEF VAR begin_spec AS CHAR NO-UNDO.
  DEF VAR end_spec AS CHAR NO-UNDO.
  DEF VAR tb_stats AS LOG INIT NO NO-UNDO.
  DEF VAR tb_subt AS LOG INIT YES NO-UNDO.
  DEF VAR v-tot-pal LIKE v-tot-qty NO-UNDO.

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
DEFINE VARIABLE iProjQty AS INTEGER NO-UNDO .
DEFINE VARIABLE iRelqty AS INTEGER NO-UNDO .
/*{sys/form/r-top5DL3.f} */
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lSelected AS LOGICAL INIT YES NO-UNDO.

  {sys/form/r-top.i}

  {sys/inc/ctrtext.i str-tit 152}.

  FORM HEADER SKIP(1)
       day_str str-tit FORMAT "x(152)" "Page" AT 164 PAGE-NUMBER FORMAT ">>9"
       SKIP
       tim_str str-tit2 FORMAT "x(152)" "{1}" AT 164 SKIP(1) 
       str-tit3 FORMAT "x(172)"
       SKIP(1)
        str-tit4 SKIP
        str-tit5 SKIP 

       WITH FRAME r-top ROW 1 COLUMN 1 STREAM-IO WIDTH 200
            NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.

  FORM HEADER
       "Credit Rating:"
       lv-cr-rating
       SKIP(1)

      WITH FRAME r-top2 PAGE-TOP NO-ATTR-SPACE NO-BOX WIDTH 200 STREAM-IO.



  ASSIGN v-tot-qty = 0
         v-tot-msf = 0
         v-tot-val = 0.

  ASSIGN
   str-tit2 = c-win:TITLE
   {sys/inc/ctrtext.i str-tit2 152}

   v-fcust[1]   = begin_cust-no
   v-fcust[2]   = end_cust-no
   v-fsman[1]   = begin_slsmn
   v-fsman[2]   = end_slsmn
   v-ford-no[1] = begin_ord-no
   v-ford-no[2] = end_ord-no
   v-fitem[1]   = begin_i-no
   v-fitem[2]   = end_i-no
   v-floc[1]    = begin_loc
   v-floc[2]    = end_loc
   v-fdate[1]   = begin_date
   v-fdate[2]   = end_date
   v-fcarr[1]   = begin_carr
   v-fcarr[2]   = end_carr
   lSelected  = tb_cust-list
   v-sort       = IF rd_sort EQ "Customer#"     THEN "C"  ELSE
                  IF rd_sort EQ "Release Date"  THEN "R"  ELSE
                  IF rd_sort EQ "Item#"         THEN "I"  ELSE
                  IF rd_sort EQ "Item Name"     THEN "N"  ELSE
                  IF rd_sort EQ "Territory"     THEN "T"  ELSE
                  IF rd_sort EQ "Credit Rating" THEN "CR" ELSE "A"
   v-types      = STRING(tb_actual,"A/")      + STRING(tb_backordered,"B/")

   str-tit3 = (IF v-sort EQ "C"  THEN "By Customer By Date"      ELSE
               IF v-sort EQ "R"  THEN "By Release Date"          ELSE
               IF v-sort EQ "I"  THEN "By Item By Date"          ELSE
               IF v-sort EQ "N"  THEN "By Item Name By Date"     ELSE
               IF v-sort EQ "A"  THEN "By Carrier By Date"       ELSE
               IF v-sort EQ "CR" THEN "By Credit Rating By Date" ELSE
                                      "By Territory By Date")    + "  "   +
               STRING(v-fdate[1],"99/99/9999")                   + " to " +
               STRING(v-fdate[2],"99/99/9999")

   {sys/inc/ctrtext.i str-tit3 172}.

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

        IF LOOKUP(ttRptSelected.TextList, "Sales Value,Skids") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

 FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:
        IF LOOKUP(ttRptSelected.TextList, "Sales Value") <> 0 AND NOT ll-secure   THEN
        RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).
 END.

  IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
  END.

  IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN v-fcust[1] = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN v-fcust[2] = ttCustList.cust-no .
  END.

  {sys/inc/print1.i}

  {sys/inc/outprint.i VALUE(lines-per-page)}
  IF td-show-parm THEN RUN show-param.

  VIEW FRAME r-top.

SESSION:SET-WAIT-STATE ("general").

  FOR EACH tt-report:
    DELETE tt-report.
  END.

  FOR EACH w-ord:
    DELETE w-ord.
  END.

  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ cocode
        AND oe-ordl.opened  EQ YES
        AND oe-ordl.ord-no  GE v-ford-no[1]
        AND oe-ordl.ord-no  LE v-ford-no[2]
        AND oe-ordl.i-no    GE v-fitem[1]
        AND oe-ordl.i-no    LE v-fitem[2]
        AND ((oe-ordl.s-man[1] GE v-fsman[1] AND
              oe-ordl.s-man[1] LE v-fsman[2]) OR
             (oe-ordl.s-man[2] GE v-fsman[1] AND
              oe-ordl.s-man[2] LE v-fsman[2]) OR
             (oe-ordl.s-man[3] GE v-fsman[1] AND
              oe-ordl.s-man[3] LE v-fsman[2]))
        AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}
                         USE-INDEX ord-no)
      USE-INDEX opened NO-LOCK,

      FIRST oe-ord
      WHERE oe-ord.company EQ oe-ordl.company
        AND oe-ord.ord-no  EQ oe-ordl.ord-no
        AND oe-ord.cust-no GE v-fcust[1]
        AND oe-ord.cust-no LE v-fcust[2]
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq oe-ord.cust-no
        AND ttCustList.log-fld no-lock) else true)
      NO-LOCK,

      FIRST cust
      WHERE cust.company EQ oe-ord.company
        AND cust.cust-no EQ oe-ord.cust-no
      NO-LOCK:

    STATUS DEFAULT "Processing Order#/FG#: " +
                   TRIM(STRING(oe-ordl.ord-no,">>>>>>>>")) + "/" +
                   TRIM(oe-ordl.i-no).
    

    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.company EQ oe-ordl.company
          AND oe-rell.ord-no  EQ oe-ordl.ord-no
          AND oe-rell.i-no    EQ oe-ordl.i-no
          AND oe-rell.line    EQ oe-ordl.line
          AND ((oe-rell.b-ord-no NE 0 AND INDEX(v-types,"B") GT 0) OR
               (oe-rell.b-ord-no EQ 0 AND INDEX(v-types,"A") GT 0))
        USE-INDEX ord-no,

        FIRST oe-relh NO-LOCK
        WHERE oe-relh.r-no     EQ oe-rell.r-no
          AND oe-relh.posted   EQ NO
          AND oe-relh.deleted  EQ NO
          /*AND oe-relh.rel-date GE v-fdate[1]
          AND oe-relh.rel-date LE v-fdate[2]*/
          AND oe-relh.carrier  GE v-fcarr[1]
          AND oe-relh.carrier  LE v-fcarr[2]
        USE-INDEX r-no

        BREAK BY oe-rell.r-no
              BY oe-rell.ord-no
              BY oe-rell.i-no
              BY oe-rell.line
              BY oe-rell.rel-no
              BY oe-rell.b-ord-no
              BY oe-rell.po-no:

      IF FIRST-OF(oe-rell.po-no) THEN lv-qty = 0.

      lv-qty = lv-qty + oe-rell.qty.

      IF LAST-OF(oe-rell.po-no) THEN DO:
        CREATE tt-report.
        ASSIGN
         tt-report.term-id = ""
         tt-report.key-01  = IF v-sort EQ "R" THEN
                               (STRING(YEAR(oe-relh.rel-date),"9999") +
                                STRING(MONTH(oe-relh.rel-date),"99")  +
                                STRING(DAY(oe-relh.rel-date),"99"))
                             ELSE
                             IF v-sort EQ "N" THEN oe-ordl.i-name
                             ELSE
                             IF v-sort EQ "C" THEN oe-relh.cust-no
                             ELSE
                             IF v-sort EQ "I" OR v-sort EQ "D" THEN oe-rell.i-no
                             ELSE
                             IF v-sort EQ "T" THEN cust.terr
                             ELSE
                             IF v-sort EQ "A" THEN oe-relh.carrier
                             ELSE
                             IF v-sort EQ "CR" THEN cust.cr-rating
                             ELSE ""
         tt-report.key-02  = IF v-sort EQ "I" OR v-sort EQ "D" THEN oe-rell.i-no
                             ELSE
                             IF v-sort EQ "T" THEN cust.terr
                             ELSE
                             IF v-sort EQ "A" THEN oe-relh.carrier
                             ELSE
                             IF v-sort EQ "CR" THEN cust.cr-rating
                             ELSE
                              IF v-sort EQ "R" THEN  string(oe-relh.release#,"9999999999")
                             ELSE oe-relh.cust-no
         tt-report.key-03  = IF v-sort NE "R" THEN
                               (STRING(YEAR(oe-relh.rel-date),"9999") +
                                STRING(MONTH(oe-relh.rel-date),"99")  +
                                STRING(DAY(oe-relh.rel-date),"99"))
                             ELSE STRING(oe-ord.ord-no,"9999999999")
         tt-report.key-04  = STRING(IF v-sort EQ "A" THEN oe-relh.cust-no
                                                     ELSE " ","x(10)") +
                             STRING(oe-ord.ord-no,"9999999999")
         tt-report.key-05  = STRING(INDEX(v-types,v-type),"99")
         tt-report.key-06  = IF oe-rell.b-ord-no EQ 0 THEN "A" ELSE "B"
         tt-report.qty     = lv-qty
         tt-report.rec-id  = RECID(oe-rell)
         tt-report.ord-no     = oe-ord.ord-no
         tt-report.i-no  = oe-rell.i-no 
         tt-report.rel-date   = oe-relh.rel-date   .

        FOR EACH fg-bin
            WHERE fg-bin.company EQ oe-ordl.company
              AND fg-bin.i-no    EQ oe-ordl.i-no
              AND fg-bin.job-no  EQ oe-ordl.job-no
              AND fg-bin.job-no2 EQ oe-ordl.job-no2
              AND fg-bin.loc     GE v-floc[1]
              AND fg-bin.loc     LE v-floc[2]
            USE-INDEX job no-lock:
          tt-report.onh = tt-report.onh + fg-bin.qty.
        END.
      END.
    END.
  END.

  STATUS DEFAULT "Printing...".

  
  RELEASE tt-report.
    iRelqty = 0  .
  for each tt-report where tt-report.term-id eq "",
      FIRST oe-rell where recid(oe-rell) eq tt-report.rec-id no-lock,
      first oe-relh
      where oe-relh.company eq oe-rell.company
        and oe-relh.r-no    eq oe-rell.r-no
      use-index r-no NO-LOCK,
      first oe-ordl
      where oe-ordl.company eq oe-rell.company
        and oe-ordl.ord-no  eq oe-rell.ord-no
        and oe-ordl.i-no    eq oe-rell.i-no
        and oe-ordl.line    eq oe-rell.line
      NO-LOCK,
      first oe-ord of oe-ordl no-lock,
      first cust
      where cust.company eq oe-ord.company
        and cust.cust-no eq oe-ord.cust-no
      no-lock
      break 
            by tt-report.i-no
            BY tt-report.rel-date
            :
       
      IF FIRST-OF(tt-report.i-no) THEN
             iRelqty = 0.
  
      tt-report.pro-qty  = tt-report.onh - iRelqty  .

      IF FIRST-OF(tt-report.i-no) THEN
                iRelqty = tt-report.qty  .
      ELSE iRelqty = iRelqty + tt-report.qty .

     IF tt-report.rel-date LT v-fdate[1] 
         OR tt-report.rel-date GT v-fdate[2]  THEN
         DELETE tt-report .

    END.

    IF tb_show-only THEN
  FOR EACH tt-report
      WHERE tt-report.term-id EQ ""
        AND tt-report.qty     LE tt-report.pro-qty /*tt-report.onh*/:
    DELETE tt-report.
  END.

  IF NOT CAN-FIND(FIRST tt-report WHERE tt-report.term-id EQ "") THEN DO:
    CREATE tt-report.
    ASSIGN
     tt-report.term-id = ""
     ll-show-top-only  = YES.
  END.
  
  for each tt-report where tt-report.term-id eq "",
      FIRST oe-rell where recid(oe-rell) eq tt-report.rec-id no-lock,
      first oe-relh
      where oe-relh.company eq oe-rell.company
        and oe-relh.r-no    eq oe-rell.r-no
      use-index r-no NO-LOCK,
      first oe-ordl
      where oe-ordl.company eq oe-rell.company
        and oe-ordl.ord-no  eq oe-rell.ord-no
        and oe-ordl.i-no    eq oe-rell.i-no
        and oe-ordl.line    eq oe-rell.line
      NO-LOCK,
      first oe-ord of oe-ordl no-lock,
      first cust
      where cust.company eq oe-ord.company
        and cust.cust-no eq oe-ord.cust-no
      no-lock
      break by tt-report.key-01
            by tt-report.key-02 
            by tt-report.key-03 DESC
            by tt-report.key-04:

    IF v-sort EQ "CR" AND FIRST-OF(tt-report.key-02) THEN DO:
      lv-cr-rating = tt-report.key-02.
      IF FIRST(tt-report.key-02) THEN VIEW FRAME r-top2.
      PAGE.
    END.

    ELSE
      IF FIRST(tt-report.key-01) THEN PAGE.

    v-qty = IF tt-report.qty NE 0 THEN tt-report.qty ELSE oe-rell.qty.

    create w-ord.

    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq oe-ordl.i-no
        NO-LOCK NO-ERROR.

    assign
     w-ord.ord-no    = oe-ord.ord-no
     w-ord.cust-no   = oe-ord.cust-no
     w-ord.cust-name = oe-ord.cust-name
     w-ord.part-no   = oe-ordl.part-no
     w-ord.i-no      = oe-ordl.i-no
     w-ord.i-name    = oe-ordl.i-name
     w-ord.qty       = oe-ordl.qty
     w-ord.cost      = oe-ordl.cost
     w-ord.price     = oe-ordl.t-price / oe-ordl.qty
     w-ord.rel-qty   = v-qty
     w-ord.onh-qty   = tt-report.onh
     w-ord.t-price   = w-ord.price * w-ord.rel-qty
     w-ord.rel-date  = STRING(oe-relh.rel-date) + tt-report.key-06
     w-ord.rel-no    = oe-relh.release#
     w-ord.ship-id   = oe-relh.ship-id
     w-ord.job-no    = oe-ordl.job-no
     w-ord.job-no2   = oe-ordl.job-no2
     w-ord.job       = IF w-ord.job-no eq "" then "" ELSE
                       (trim(w-ord.job-no) + "-" +
                        STRING(w-ord.job-no2,"99"))
     w-ord.po-num    = oe-rell.po-no
     w-ord.ord-qty   = oe-ordl.qty
     w-ord.shp-qty   = oe-ordl.ship-qty
     w-ord.msf       = w-ord.rel-qty * ( IF AVAIL itemfg THEN itemfg.t-sqft ELSE 0) / 1000
     w-ord.prom-code = oe-ordl.prom-code
     w-ord.last-date = oe-ord.last-date
     w-ord.carrier   = oe-relh.carrier
     w-ord.is-a-component = oe-ordl.is-a-component
     ld-palls        = w-ord.rel-qty /
                       ((IF oe-ordl.cas-cnt    EQ 0 THEN 1 ELSE oe-ordl.cas-cnt) *
                        (IF oe-ordl.cases-unit EQ 0 THEN 1 ELSE oe-ordl.cases-unit)).

    {sys/inc/roundup.i ld-palls}

    IF ld-palls LT 0 THEN ld-palls = ld-palls * -1.

    w-ord.palls = w-ord.palls + ld-palls.

    w-ord.iPro-qty = tt-report.pro-qty.

    IF NOT FIRST-OF(tt-report.key-02) AND v-sort EQ "C" THEN w-ord.cust-name = "".

    IF v-comps AND AVAIL itemfg AND itemfg.isaset THEN DO:
      RUN fg/fullset.p (ROWID(itemfg)).

      FOR EACH tt-fg-set,
          FIRST itemfg
          WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ tt-fg-set.part-no
          NO-LOCK:

        CREATE b-w-ord.
        BUFFER-COPY w-ord TO b-w-ord
        ASSIGN
         b-w-ord.component = 1
         b-w-ord.cust-name = ""
         b-w-ord.part-no   = itemfg.part-no
         b-w-ord.i-no      = tt-fg-set.part-no
         b-w-ord.i-name    = itemfg.i-name
         b-w-ord.price     = 0
         b-w-ord.cost      = 0
         b-w-ord.t-price   = 0
         b-w-ord.job       = ""
         b-w-ord.po-num    = ""
         b-w-ord.qty       = w-ord.qty     * tt-fg-set.part-qty-dec
         b-w-ord.rel-qty   = w-ord.rel-qty * tt-fg-set.part-qty-dec
         b-w-ord.ord-qty   = w-ord.ord-qty * tt-fg-set.part-qty-dec
         b-w-ord.shp-qty   = w-ord.shp-qty * tt-fg-set.part-qty-dec
         b-w-ord.msf       = b-w-ord.rel-qty * itemfg.t-sqft / 1000.
      END.
    END.

    FOR EACH w-ord
        BREAK BY w-ord.component
              BY w-ord.i-no:

     /* IF NOT tb_show-val THEN w-ord.t-price = 0.*/

      FIND FIRST itemfg
          WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ w-ord.i-no
          NO-LOCK NO-ERROR.

      IF AVAIL itemfg OR ll-show-top-only THEN DO:
        /*{oe/rep/schdrel.i}*/
          DEF VAR lv-issue AS CHAR FORMAT "x(5)" NO-UNDO.
          lv-issue = IF w-ord.rel-qty GT /*w-ord.onh-qty*/ w-ord.iPro-qty THEN "ISSUE" ELSE "OK".

             ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = string(w-ord.cust-no,"x(8)") .
                         WHEN "rel-date"   THEN cVarValue = IF w-ord.rel-date NE ? THEN string(w-ord.rel-date,"x(8)") ELSE "".
                         WHEN "rel-num"   THEN cVarValue = STRING(w-ord.rel-no,">>>>>9").
                         WHEN "ship"  THEN cVarValue = STRING(w-ord.ship-id,"x(8)") .
                         WHEN "carr"   THEN cVarValue = STRING(w-ord.carrier ,"x(8)") .
                         WHEN "ord"  THEN cVarValue = STRING(w-ord.ord-no,">>>>>>>>") .
                         WHEN "cust-part"   THEN cVarValue = STRING(w-ord.part-no,"x(15)") .
                         WHEN "desc"  THEN cVarValue = STRING(w-ord.i-name,"x(25)") .

                         WHEN "fg-item"    THEN cVarValue = string(w-ord.i-no,"x(15)") .
                         WHEN "po-num"   THEN cVarValue = string(w-ord.po-num,"x(15)").
                         WHEN "Qty-hand"   THEN cVarValue = STRING(w-ord.onh-qty,"->>,>>>,>>9").
                         WHEN "rel-qty"  THEN cVarValue = STRING(w-ord.rel-qty,"->>,>>>,>>9") .
                         WHEN "sales"   THEN cVarValue = IF ll-secure THEN STRING(w-ord.t-price,"$->>,>>>,>>9.99") ELSE "" .
                         WHEN "skid"  THEN cVarValue = STRING(w-ord.palls,">>>,>>9") .
                         WHEN "stat"   THEN cVarValue = STRING(lv-issue,"x(6)") .
                         WHEN "proj-qty"   THEN cVarValue = IF tb_pro-qty THEN STRING(w-ord.iPro-qty,"->>>>,>>>,>>9") ELSE "" .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.


      END.
    END.

    IF NOT ll-show-top-only THEN DO:
      FIND FIRST w-ord.

      ASSIGN
       v-tot-qty[1] = v-tot-qty[1] + 1
       v-tot-msf[1] = v-tot-msf[1] + w-ord.msf.
    IF w-ord.t-price NE ? THEN
       v-tot-val[1] = v-tot-val[1] + w-ord.t-price .
    IF w-ord.palls NE ? THEN
       v-tot-pal[1] = v-tot-pal[1] + w-ord.palls.

      IF LAST-OF(tt-report.key-02) THEN DO:
        /*IF v-sort EQ "C" THEN
          PUT "Customer Totals:" TO 140.
        ELSE
          PUT "      Subtotals:" TO 140.*/

       /* PUT v-tot-val[1] TO 156 FORMAT "$->>,>>>,>>9.99"
            v-tot-pal[1] TO 164 FORMAT ">>>,>>9"
            SKIP(2).*/

          PUT str-line SKIP .
          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = "" .
                         WHEN "rel-date"   THEN cVarValue = "".
                         WHEN "rel-num"   THEN cVarValue = "".
                         WHEN "ship"  THEN cVarValue = "".
                         WHEN "carr"   THEN cVarValue = "" .
                         WHEN "ord"  THEN cVarValue = "" .
                         WHEN "cust-part"   THEN cVarValue = "" .
                         WHEN "desc"  THEN cVarValue = "" .

                         WHEN "fg-item"    THEN cVarValue = "" .
                         WHEN "po-num"   THEN cVarValue =  "".
                         WHEN "Qty-hand"   THEN cVarValue = "".
                         WHEN "rel-qty"  THEN cVarValue = "" .
                         WHEN "sales"   THEN cVarValue = IF ll-secure THEN STRING(v-tot-val[1],"$->>,>>>,>>9.99") ELSE "" .
                         WHEN "skid"  THEN cVarValue = STRING(v-tot-pal[1],">>>,>>9") .
                         WHEN "stat"   THEN cVarValue = "" .
                         WHEN "proj-qty"   THEN cVarValue = "" .
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
       IF trim(cDisplay) NE "" THEN do:
        IF v-sort EQ "C" THEN
          PUT UNFORMATTED "          Customer Totals:"  substring(cDisplay,27,350) SKIP(1).  
        ELSE
          PUT UNFORMATTED "          Sub Totals:" substring(cDisplay,22,350) SKIP(1).
       END.

            /*IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.*/

        ASSIGN
         v-tot-qty[2] = v-tot-qty[2] + v-tot-qty[1]
         v-tot-val[2] = v-tot-val[2] + v-tot-val[1]
         v-tot-msf[2] = v-tot-msf[2] + v-tot-msf[1]
         v-tot-pal[2] = v-tot-pal[2] + v-tot-pal[1]
         v-tot-qty[1] = 0
         v-tot-val[1] = 0
         v-tot-msf[1] = 0
         v-tot-pal[1] = 0.
      END.

      IF LAST(tt-report.key-01) THEN DO:
        /*PUT SKIP(1)
            "Report Totals:" TO 140
            v-tot-val[2]     TO 156 FORMAT "$->>,>>>,>>9.99"
            v-tot-pal[2]     TO 164 FORMAT ">>>,>>9".*/
          PUT str-line SKIP .
          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = "" .
                         WHEN "rel-date"   THEN cVarValue = "".
                         WHEN "rel-num"   THEN cVarValue = "".
                         WHEN "ship"  THEN cVarValue = "".
                         WHEN "carr"   THEN cVarValue = "" .
                         WHEN "ord"  THEN cVarValue = "" .
                         WHEN "cust-part"   THEN cVarValue = "" .
                         WHEN "desc"  THEN cVarValue = "" .

                         WHEN "fg-item"    THEN cVarValue = "" .
                         WHEN "po-num"   THEN cVarValue =  "".
                         WHEN "Qty-hand"   THEN cVarValue = "".
                         WHEN "rel-qty"  THEN cVarValue = "" .
                         WHEN "sales"   THEN cVarValue = IF ll-secure THEN STRING(v-tot-val[2],"$->>,>>>,>>9.99") ELSE "" .
                         WHEN "skid"  THEN cVarValue = STRING(v-tot-pal[2],">>>,>>9") .
                         WHEN "stat"   THEN cVarValue = "" .
                          WHEN "proj-qty"   THEN cVarValue = "" .
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

          IF trim(cDisplay) NE "" THEN 
          PUT UNFORMATTED "          Report Totals:  "  substring(cDisplay,27,350) SKIP(1).  


      END.
    END.

    FOR EACH w-ord:
      DELETE w-ord.
    END.
  END. /* each tt-report */

  STATUS DEFAULT "".

  SESSION:SET-WAIT-STATE ("").

  IF tb_excel THEN DO:
    OUTPUT STREAM excel CLOSE.
    IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
  END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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

