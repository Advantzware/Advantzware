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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecdt.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def TEMP-TABLE w-data no-undo
  field ord-no like oe-ord.ord-no
  field line   like oe-ordl.line
  field sman   as char format "x(3)"
  field item-n like itemfg.i-name column-label "Item Description"
        format "x(27)"
  field procat like itemfg.procat column-label "Prod!Code"
  field qty like oe-ordl.qty column-label "Quantity!Ordered/EA"
        format ">,>>>,>>>"
  field sqft like itemfg.t-sqft column-label "Sq Ft" format ">>,>>>.999"
  field t-sqft like itemfg.t-sqft column-label "Total!Sq Ft/M" format "->,>>>.999"
  field t-tons as dec column-label "Total!  Tons" format "->,>>>.9"
  field price like oe-ordl.price format ">>>,>>9.99<<<<"
  field revenue like oe-ordl.t-price column-label "Order!Amount"
  field misc as log
  field cost as dec
  field comm as dec label "Comm %"
  FIELD margin AS DEC.

def TEMP-TABLE wkrecap no-undo    /* recap by product category */
  field procat like itemfg.procat column-label "Cat"
  field t-sqft like itemfg.t-sqft  extent 2 column-label "Sq Ft" format ">>,>>>.999"
  field t-tons as dec column-label "Tons" extent 2 format "->,>>>.9"
  field revenue like oe-ordl.t-price   extent 2 column-label "Amount"
  field price-per-m  as dec column-label "$/MSF" extent 2
  field price-per-t  as dec column-label "$/TON" extent 2
  field num-of-ord as int column-label "#Orders".

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
def var security-flag as log no-undo.
DEF VAR v-code AS CHAR NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg.

DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO. 
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

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
ASSIGN cTextListToSelect  = "DUE DATE,ORDER#,CUSTOMER NAME,COMM %,PROD CODE," +
                            "FG ITEM NAME,QTY ORDERED/EA,SQ FT,TOTAL Sq Ft/M," +
                            "$/MSF,PRICE,ORDER AMOUNT,% PROFIT,TOTAL TONS,$/TON," +
                            "FG ITEM#,ID,CUSTOMER PART#,CUSTOMER PO#"  
       cFieldListToSelect = "oe-ord.due-date,w-data.ord-no,cust.name,w-data.comm,w-data.procat," +
                            "w-data.item-n,w-data.qty,w-data.sqft,t-sqft," +
                            "v-price-per-m,w-data.price,v-revenue,v-profit,t-tons,v-price-per-t," +
                            "oe-ordl.i-no,oe-ord.user-id,oe-ordl.part-no,cust-po" 
                            
       cFieldLength = "8,14,13,6,9," + "16,14,10,13," + "10,10,13,9,10,10," + "15,8,15,15"
       .

{sys/inc/ttRptSel.i}
 ASSIGN cTextListToDefault  = "DUE DATE,ORDER#,CUSTOMER NAME,COMM %,PROD CODE," +
                              "QTY ORDERED/EA,CUSTOMER PART#,FG ITEM NAME,SQ FT,TOTAL Sq Ft/M," +
                              "$/MSF,PRICE,ORDER AMOUNT,% PROFIT,TOTAL TONS,$/TON" .
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
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 begin_cust-no end_cust-no ~
begin_ord-date end_ord-date begin_slsmn end_slsmn begin_fg-cat end_fg-cat ~
tb_prepmisc tb_smn-no tb_exclude-set-comps tb_rep-tot tb_exclude-transfer ~
tb_Under% tb_Over% Btn_Def sl_avail sl_selected Btn_Add Btn_Remove btn_Up ~
btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel ~
tb_runExcel fi_file tb_batch btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_ord-date ~
end_ord-date lbl_sqft begin_slsmn end_slsmn begin_fg-cat end_fg-cat ~
tb_prepmisc tb_smn-no tb_exclude-set-comps tb_rep-tot tb_exclude-transfer ~
tb_Under% fUnder% fOver% tb_Over% sl_avail sl_selected rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel ~
fi_file tb_batch 

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

DEFINE VARIABLE begin_fg-cat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Product Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Order Date" 
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

DEFINE VARIABLE end_fg-cat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Product Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Order Date" 
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
     SIZE 94 BY 11.67.

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
     SIZE 30 BY .95 NO-UNDO.

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
     begin_cust-no AT ROW 2.43 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.43 COL 73 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_ord-date AT ROW 3.38 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Order Date"
     end_ord-date AT ROW 3.38 COL 73 COLON-ALIGNED HELP
          "Enter Ending Order Date"
     lbl_sqft AT ROW 3.86 COL 103 COLON-ALIGNED NO-LABEL
     begin_slsmn AT ROW 4.33 COL 30 COLON-ALIGNED HELP
          "Enter Beginning SalesRep Number"
     end_slsmn AT ROW 4.33 COL 73 COLON-ALIGNED HELP
          "Enter Ending SalesRep Number"
     rd_sqft AT ROW 4.57 COL 122 NO-LABEL
     begin_fg-cat AT ROW 5.29 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Product Category"
     end_fg-cat AT ROW 5.29 COL 73 COLON-ALIGNED HELP
          "Enter Ending Product Category"
     tb_prft AT ROW 5.76 COL 116
     tb_prepmisc AT ROW 6.62 COL 15
     tb_smn-no AT ROW 6.62 COL 58
     tb_ton AT ROW 6.95 COL 114
     tb_exclude-set-comps AT ROW 7.57 COL 15 WIDGET-ID 4
     tb_rep-tot AT ROW 7.71 COL 58 WIDGET-ID 54
     tb_sortby AT ROW 8.38 COL 122
     tb_exclude-transfer AT ROW 8.52 COL 15 WIDGET-ID 6
     tb_Under% AT ROW 9.57 COL 15 WIDGET-ID 46
     fUnder% AT ROW 9.57 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     fOver% AT ROW 9.57 COL 73.8 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     tb_Over% AT ROW 9.62 COL 52 WIDGET-ID 52
     tb_comm AT ROW 11.24 COL 118
     Btn_Def AT ROW 13.1 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_avail AT ROW 13.43 COL 4 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 13.43 COL 60 NO-LABEL WIDGET-ID 28
     tb_margin AT ROW 13.86 COL 119 WIDGET-ID 2
     Btn_Add AT ROW 14.29 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 15.48 COL 40 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 16.67 COL 40 WIDGET-ID 40
     btn_down AT ROW 17.86 COL 40 WIDGET-ID 42
     rd-dest AT ROW 19.91 COL 5 NO-LABEL
     lv-ornt AT ROW 20.62 COL 31 NO-LABEL
     lines-per-page AT ROW 20.62 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 22.52 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 23.48 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 24.67 COL 30
     tb_excel AT ROW 25.86 COL 72 RIGHT-ALIGNED
     tb_runExcel AT ROW 25.86 COL 93 RIGHT-ALIGNED
     fi_file AT ROW 26.67 COL 49 COLON-ALIGNED HELP
          "Enter File Name"
     tb_batch AT ROW 26.81 COL 6
     btn-ok AT ROW 28.14 COL 26
     btn-cancel AT ROW 28.14 COL 56
     "Note: Profit Includes Estimate Markups and Commissions." VIEW-AS TEXT
          SIZE 55 BY .95 AT ROW 11.57 COL 17
          FGCOLOR 1 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 12.71 COL 60.2 WIDGET-ID 44
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 12.71 COL 4.4 WIDGET-ID 38
     "(Prep / Misc Charges will Display 'P' or 'M' for Product Code)" VIEW-AS TEXT
          SIZE 57 BY .95 AT ROW 10.67 COL 15
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 19.19 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 151.4 BY 28.62.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
     RECT-7 AT ROW 1 COL 1
     RECT-8 AT ROW 18.95 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 151.4 BY 28.62.


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
         HEIGHT             = 28.67
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
IF NOT C-Win:LOAD-ICON("images\progress":U) THEN
    MESSAGE "Unable to load icon: images\progress"
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
       begin_fg-cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_fg-cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
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
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_fg-cat C-Win
ON LEAVE OF begin_fg-cat IN FRAME FRAME-A /* Beginning Product Category */
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

  IF g_batch THEN tb_batch = YES.
  IF tb_batch THEN DO:
     RUN run-batch.
     RETURN NO-APPLY.
  END.

  RUN GetSelectionList.
  RUN run-report.
  STATUS DEFAULT "Processing Complete".
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= "Customer "
                            &begin_cust= "begin_cust-no"
                            &END_cust= "begin_cust-no" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Customer "
                                &begin_cust= "begin_cust-no"
                                &END_cust= "begin_cust-no"
                                &mail-subject=c-win:title
                                &mail-body=c-win:title
                                &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Customer "
                                  &begin_cust= "begin_cust-no"
                                  &END_cust= "begin_cust-no"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  end case.
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


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_fg-cat C-Win
ON LEAVE OF end_fg-cat IN FRAME FRAME-A /* Ending Product Category */
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


&Scoped-define SELF-NAME rd_sqft
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sqft C-Win
ON VALUE-CHANGED OF rd_sqft IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_comm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_comm C-Win
ON VALUE-CHANGED OF tb_comm IN FRAME FRAME-A /* Print Commission? */
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


&Scoped-define SELF-NAME tb_exclude-set-comps
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_exclude-set-comps C-Win
ON VALUE-CHANGED OF tb_exclude-set-comps IN FRAME FRAME-A /* Exclude Set Components */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_exclude-transfer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_exclude-transfer C-Win
ON VALUE-CHANGED OF tb_exclude-transfer IN FRAME FRAME-A /* Exclude Transfer Releases/Orders */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_margin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_margin C-Win
ON VALUE-CHANGED OF tb_margin IN FRAME FRAME-A /* Print Avail Margin? */
DO:
  assign {&self-name}.
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
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prft
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prft C-Win
ON VALUE-CHANGED OF tb_prft IN FRAME FRAME-A /* Print Profit? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rep-tot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rep-tot C-Win
ON VALUE-CHANGED OF tb_rep-tot IN FRAME FRAME-A /* Rep Sub Totals? */
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


&Scoped-define SELF-NAME tb_smn-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_smn-no C-Win
ON VALUE-CHANGED OF tb_smn-no IN FRAME FRAME-A /* Page By SalesRep? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sortby
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sortby C-Win
ON VALUE-CHANGED OF tb_sortby IN FRAME FRAME-A /* Sort by Order#? */
DO:
  assign {&self-name}.
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
   begin_ord-date = today
   end_ord-date   = today.

  IF g_batch THEN tb_batch = YES.

  RUN DisplaySelectionList.
  RUN enable_UI.
  
  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}        
    RUN DisplaySelectionList2.    

    APPLY "entry" TO begin_ord-date.
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
  DISPLAY begin_cust-no end_cust-no begin_ord-date end_ord-date lbl_sqft 
          begin_slsmn end_slsmn begin_fg-cat end_fg-cat tb_prepmisc tb_smn-no 
          tb_exclude-set-comps tb_rep-tot tb_exclude-transfer tb_Under% fUnder% 
          fOver% tb_Over% sl_avail sl_selected rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
          tb_batch 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 RECT-8 begin_cust-no end_cust-no begin_ord-date end_ord-date 
         begin_slsmn end_slsmn begin_fg-cat end_fg-cat tb_prepmisc tb_smn-no 
         tb_exclude-set-comps tb_rep-tot tb_exclude-transfer tb_Under% tb_Over% 
         Btn_Def sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel 
         tb_runExcel fi_file tb_batch btn-ok btn-cancel 
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
    IF NOT AVAIL ttRptList THEN
        MESSAGE "no " i ENTRY(i,ctmplist) SKIP
        ctmplist
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
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
 /* DEF VAR printok AS LOG.
  
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
DEF VAR str-tit4 AS cha NO-UNDO.
DEF VAR str-tit5 AS cha NO-UNDO.
DEF VAR str-line AS cha FORM "x(300)" NO-UNDO.

{sys/form/r-top5DL.f}

def var fdate as date format "99/99/9999" init 01/01/0001 no-undo.
def var tdate like fdate init 12/31/9999 no-undo.
def var v-break as log init no no-undo.
def var prt-sqft as log init yes format "SqFt/PartNo" no-undo.
def var p-m-chg as log init no no-undo.
def var prt-profit as log init yes no-undo.
def var item-dscr as log init no no-undo.
def var mdate as date no-undo.
def var lo_trandate like fdate no-undo.
def var v-per-days as int extent 2 no-undo init 0.
def var v-n-lines  as int no-undo.
def var fsman as char format "x(3)" no-undo.
def var tsman as char format "x(3)" init "zzz" no-undo.
def var v-sman like w-data.sman no-undo.
def var v-exclude as log no-undo.
def var v-misc as LOG NO-UNDO.
def var v-amt  like oe-ord.t-revenue NO-UNDO.
def var v-pct as dec format "99.99" NO-UNDO.
def var v-sqft like itemfg.t-sqft  format ">,>>9.999" NO-UNDO.
def var v-tons as DEC NO-UNDO.
def var v-qty like oe-ordl.qty format "->>>,>>9.99" NO-UNDO.
def var v-price-per-m as dec column-label "$/MSF" no-undo.
def var v-price-per-t as dec column-label "$/TON" no-undo.
def var v-msf like v-price-per-m extent 2 no-undo.
def var v-ton like v-price-per-t extent 2 no-undo.
DEF VAR tot-sqft AS DEC NO-UNDO.
DEF VAR tot-renv AS DEC NO-UNDO.
DEF VAR tot-ton AS DEC NO-UNDO.

def var v-revenue like oe-ordl.t-price format "->,>>>,>>9.99" no-undo
  column-label "Order!Amount".
def var v-profit as dec format "->>,>>9.9" no-undo
  column-label "% Profit".
DEF VAR v-margin AS DEC FORMAT "->>,>>9.9" NO-UNDO COLUMN-LABEL "% Margin".
def var v-sname like sman.sname.

def var v as INT NO-UNDO.
def var qm as DEC NO-UNDO.
def var mat as DEC NO-UNDO.
def var lab as DEC NO-UNDO.

def var ii like i no-undo.
DEF VAR excelheader AS CHAR NO-UNDO.

DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF BUFFER boe-ord FOR oe-ord.
DEF BUFFER boe-ordl FOR oe-ordl.
DEF BUFFER bcust FOR cust.
DEF BUFFER bw-data FOR w-data.

find first w-data no-error.

form header "Sales Rep:"
            w-data.sman
            "-"
            v-sname
    with frame r-top1 no-box no-attr-space page-top stream-io width 180.

assign
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
 p-m-chg    = tb_prepmisc.

/*IF tb_margin THEN prt-profit = NO.*/
prt-profit = CAN-DO(cSelectedlist,"% PROFIT").
prt-sqft = NO.
if prt-profit then do:
  IF NOT security-flag THEN RUN sys/ref/d-passwd.w (3, OUTPUT security-flag).
  prt-profit = security-flag.
end.

FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:
    IF ttRptSelected.TextList MATCHES "*SQ FT*" THEN prt-sqft = YES.

    IF ttRptSelected.TextList = "% Profit" AND NOT prt-profit THEN NEXT.

    ASSIGN str-tit4 = str-tit4 + 
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength + 1 - LENGTH(ttRptSelected.TextList))
            str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + ",".        
    
    IF LOOKUP(ttRptSelected.TextList, "TOTAL Sq Ft/M,$/MSF,ORDER AMOUNT,TOTAL TONS,$/TON,% PROFIT") <> 0    THEN
        ASSIGN
        str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
    ELSE
        str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " .
END.
excelheader = "Sales Rep,Sales Name," + excelheader.




/*
ASSIGN str-tit4 = 
      (IF {sys/inc/rptDisp.i "oe-ord.due-date"} THEN "DUE DATE " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "w-data.ord-no"} THEN     "ORDER#         " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "cust.name"} THEN    "CUSTOMER NAME " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "w-data.comm"} THEN  "COMM % " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "w-data.item-n"} THEN "ITEM DESCRIPTION " ELSE "")  +
    (IF {sys/inc/rptDisp.i "w-data.qty"} THEN    "QTY ORDERED/EA " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "w-data.t-sqft"} THEN "T " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "v-price-per-m"} THEN "RFID TAG #    " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "v-revenue"} THEN     "TAG #         " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "v-profit"}  THEN     "UNITS    " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "w-data.t-tons"}  THEN "COUNT    " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "v-price-per-t"}  THEN "BIN       " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "lv-cost-uom"}       THEN "UOM       " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "v-fg-qty"}  THEN         "TOT QTY    " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "v-fg-cost"}   THEN       "TOT COST  " ELSE "" ) +
    (IF {sys/inc/rptDisp.i "v-fg-value"}  THEN       "TOT SELL VALUE" ELSE ""  )
  str-tit5 = ""
     
    .
*/

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
/* ===
  excelheader = "Sales Rep,Sales Name,".
      
 /* if item-dscr then do:
    excelheader = excelheader + "Due Date,Order#,Customer Name,Comm %,"
                + "Item Description,Quantity Ordered/EA,"
                + "Total Sq Ft/M,$/MSF,Order Amount,".

    IF tb_margin THEN
       excelheader = excelheader +  "Avail Margin,".
    ELSE
       excelheader = excelheader + "% Profit,".

    if tb_ton THEN
      excelheader = excelheader + "Total Tons,$/TON".
  END.
  ELSE */
  IF prt-sqft THEN DO:

    excelheader = excelheader + "Due Date,Order#,Customer Name,Comm %,"
                + "Prod Code,Item Description,Quantity Ordered/EA,Sq Ft,"
                + "Total Sq Ft/M,$/MSF,Price,Order Amount,".

    IF tb_margin THEN
       excelheader = excelheader +  "Avail Margin,".
    ELSE
       excelheader = excelheader + "% Profit,".

    if tb_ton THEN
      excelheader = excelheader + "Total Tons,$/TON".
  END.
  ELSE DO:
    excelheader = excelheader + "Due Date,Order#,Customer Name,Comm %,"
                + "Prod Code,Item Description,Quantity Ordered/EA,Customer Part Number,"
                + "$/MSF,Price,Order Amount,".

    IF tb_margin THEN
       excelheader = excelheader +  "Avail Margin,".
    ELSE
       excelheader = excelheader + "% Profit,".

    if tb_ton THEN
      excelheader = excelheader + "Total Tons,$/TON".
  END.
====*/  
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
  
  ASSIGN
  lv-frame-hdl = frame {&frame-name}:HANDLE
  lv-group-hdl = lv-frame-hdl:first-child
  lv-field-hdl = lv-group-hdl:first-child.
  
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

