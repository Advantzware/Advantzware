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

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
def new shared var v-fr-tax like oe-ctrl.f-tax.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
def var security-flag as log no-undo.

DEF STREAM excel.

DEFINE TEMP-TABLE ExtList NO-UNDO
  FIELD ord-no       LIKE oe-ord.ord-no
  FIELD est-no       LIKE oe-ord.est-no 
  FIELD job-no       AS CHARACTER
  FIELD ord-date     LIKE oe-ord.ord-date
  FIELD cust-no      LIKE oe-ord.cust-no 
  FIELD cust-name    LIKE oe-ord.cust-name 
  FIELD i-no         LIKE oe-ordl.i-no     /* misc charge */
  FIELD i-name       LIKE oe-ordl.i-name   /* description */
  FIELD qty-lft      LIKE oe-ordl.qty 
  FIELD cost         LIKE oe-ordl.cost 
  FIELD price        LIKE oe-ordl.price
  FIELD pr-uom       LIKE oe-ordl.pr-uom
  FIELD ext-price    AS CHARACTER  /* price */
  FIELD margin       AS CHARACTER.



DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
/*
   oe-ord.est-no format "x(8)"
   oe-ord.job-no space(0) "-" space(0)
   oe-ord.job-no2 format "99"
   space(3) 
   oe-ord.ord-date
   space(3) 
   oe-ord.cust-no 
   oe-ord.cust-name skip
  WITH frame ord no-labels no-box no-underline stream-io width 132.

FORMAT  /* frame ord1 */
    oe-ordl.i-no                          label "Item"  at 10
    oe-ordl.i-name format "x(25)"         label "Description"
    v-qty-lft      format "->>>,>>9"      label "Quantity"
    oe-ordl.cost   format "->>>,>>9.99"   label "Cost/M"
    oe-ordl.price  format "->>>,>>9.99"   label "Price"
    oe-ordl.pr-uom   label "UOM" 4
    v-qty-lft 
    v-ext-price    format "->,>>>,>>9.99" label "Ext Price"
    v-margin       format "->,>>>,>>9.99" 
*/
DEF VAR v-orderedMsf AS DEC FORM "->>>,>>9.99" NO-UNDO.
DEF VAR v-jobShipQty AS INT FORM "->>>>>>9" NO-UNDO.
DEF VAR v-boardProfit AS DEC FORM "->>>,>>9.99" NO-UNDO.
DEF VAR v-boardPO AS INT FORM "->>>>>>9" NO-UNDO.
DEF VAR v-boardpoQty AS INT FORM "->>>>>>9" NO-UNDO.
DEF VAR v-boardCost AS DEC FORM "->>>,>>9.99" NO-UNDO.
DEF VAR v-boardTotalCost AS DEC FORM "->>>,>>9.99" NO-UNDO.
DEF VAR v-boardTotalQty AS INT FORM "->>>>>>9" NO-UNDO.

DEF VAR v-Order%Profit AS DEC NO-UNDO.
DEF VAR v-MSFRec AS DEC NO-UNDO.
DEF var v-FGShipDate AS DATE NO-UNDO.
DEF VAR v-PORecDate AS DATE NO-UNDO.
DEF VAR v-FGExtPrice AS DEC NO-UNDO.
DEF VAR v-PORecCost AS DEC NO-UNDO.
DEF VAR v-ProfitSold$ AS DEC NO-UNDO.
DEF VAR v-ProfitSold% AS DEC NO-UNDO.
DEF VAR v-UnitsBoard AS INT NO-UNDO.
DEF VAR v-UnitLoss$ AS DEC NO-UNDO.
DEF VAR v-Loss% AS DEC NO-UNDO.
DEF VAR v-bol# AS INT FORM ">>>>>>>9" NO-UNDO.
DEF VAR v-inv# AS INT FORM ">>>>>9" NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "Order#,Est#,Job#,Date,Cust#,Name," + 
                           "Item,Description,FG Order Qty,FG Cost,Price,UOM,Ext Price,FG Item Profit," + 
                           "PO MSF,FG Shipped,PO Profit,PO#,PO Qty,PO Cost,PO Total Cost,PO Received,"  +
                           "Order% Profit,MSF Recvd,FG Ship Date,PO Rec Date,FG Ext Price," +
                           "PO Rec Cost,Profit $ Sold,Profit % Sold,Units/Board,Unit Waste, % Loss,BOL#,Invoice#"
       cFieldListToSelect = "oe-ord.ord-no,oe-ordl.est-no,oe-ordl.job-no,oe-ord.ord-date,oe-ord.cust-no,oe-ord.cust-name," +
                            "oe-ordl.i-no,oe-ordl.i-name,v-qty-lft,oe-ordl.cost,oe-ordl.price,oe-ordl.pr-uom,v-ext-price,v-margin," +
                            "v-orderedMsf,v-jobShipQty,v-boardProfit,v-boardPO,v-boardpoQty,v-boardCost,v-boardTotalCost,v-boardTotalQty," +
                            "v-Order%Profit,v-MSFRec,v-FGShipDate,v-PORecDate,v-FGExtPrice," +
                            "v-PORecCost,v-ProfitSold$,v-ProfitSold%,v-UnitsBoard,v-UnitLoss$,v-Loss%,v-bol#,v-inv#"
       cFieldLength = "6,8,9,10,8,30," + "15,25,12,11,11,4,13,14," + "7,11,12,9,12,11,13,11," +
                      "13,9,12,11,12," + "11,13,13,11,12,7,8,8"
       cFieldType   = "i,c,c,c,c,c," + "c,c,i,i,i,c,i,i," + "i,i,i,i,i,i,i,i," + "i,i,i,i,i,i,i,i,i,i,i,i,i"
       .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Order#,Est#,Job#,Date,Cust#,Name," + 
                           "Item,Description,FG Order Qty,FG Cost,Price,UOM,Ext Price,FG Item Profit".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_ord-no end_ord-no ~
begin_cust-no end_cust-no begin_i-no end_i-no begin_ord-date end_ord-date ~
begin_rct-date end_rct-date tb_UseRcptDate begin_ship-date end_ship-date ~
tb_UseShipDate tb_PrtMisc sl_avail Btn_Add sl_selected Btn_Remove btn_Up ~
btn_down rd-dest lv-ornt lv-font-no lines-per-page td-show-parm tb_excel ~
tb_runExcel fi_file btn-ok btn-cancel Btn_Def
&Scoped-Define DISPLAYED-OBJECTS begin_ord-no end_ord-no begin_cust-no ~
end_cust-no begin_i-no end_i-no begin_ord-date end_ord-date begin_rct-date ~
end_rct-date tb_UseRcptDate begin_ship-date end_ship-date tb_UseShipDate ~
tb_PrtMisc sl_avail sl_selected rd-dest lv-ornt lv-font-no lines-per-page ~
lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldvalue C-Win 
FUNCTION GetFieldvalue RETURNS CHARACTER
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

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rct-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_ship-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Ship Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rct-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ship-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Ship Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-booko#.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_qty AS CHARACTER FORMAT "X(256)":U INITIAL "Print Quantity Ordered or Remaining?" 
     VIEW-AS FILL-IN 
     SIZE 37 BY .95 NO-UNDO.

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

DEFINE VARIABLE rd_qty AS CHARACTER INITIAL "Ordered" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ordered", "Ordered",
"Remaining", "Remaining"
     SIZE 28 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY 11.1.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 35 BY 5 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 35 BY 5 NO-UNDO.

DEFINE VARIABLE tb_contr AS LOGICAL INITIAL no 
     LABEL "Print Contribution?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_PrtMisc AS LOGICAL INITIAL no 
     LABEL "Print Misc. Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_UseRcptDate AS LOGICAL INITIAL no 
     LABEL "Use ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE tb_UseShipDate AS LOGICAL INITIAL no 
     LABEL "Use ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_ord-no AT ROW 2.43 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord-no AT ROW 2.43 COL 66 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_cust-no AT ROW 3.38 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 3.38 COL 66 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_i-no AT ROW 4.33 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 4.33 COL 66 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_ord-date AT ROW 5.29 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Order Date"
     end_ord-date AT ROW 5.29 COL 66 COLON-ALIGNED HELP
          "Enter Ending Order Date"
     begin_rct-date AT ROW 6.24 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Order Date" WIDGET-ID 52
     end_rct-date AT ROW 6.24 COL 66 COLON-ALIGNED HELP
          "Enter Ending Order Date" WIDGET-ID 54
     tb_UseRcptDate AT ROW 6.48 COL 87 WIDGET-ID 56
     begin_ship-date AT ROW 7.19 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Order Date" WIDGET-ID 48
     end_ship-date AT ROW 7.19 COL 66 COLON-ALIGNED HELP
          "Enter Ending Order Date" WIDGET-ID 50
     tb_UseShipDate AT ROW 7.43 COL 87 WIDGET-ID 58
     lbl_qty AT ROW 8.57 COL 14 COLON-ALIGNED NO-LABEL
     rd_qty AT ROW 8.57 COL 56 NO-LABEL
     tb_PrtMisc AT ROW 9.76 COL 36 WIDGET-ID 46
     tb_contr AT ROW 10.95 COL 36
     sl_avail AT ROW 12.91 COL 2 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 12.91 COL 41 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 13.91 COL 41 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     sl_selected AT ROW 12.91 COL 61.4 NO-LABEL WIDGET-ID 28
     Btn_Remove AT ROW 14.91 COL 41 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 15.91 COL 41 WIDGET-ID 40
     btn_down AT ROW 16.91 COL 41 WIDGET-ID 42
     rd-dest AT ROW 19.1 COL 5 NO-LABEL
     lv-ornt AT ROW 19.33 COL 27 NO-LABEL
     lv-font-no AT ROW 21.24 COL 29 COLON-ALIGNED
     lines-per-page AT ROW 21.24 COL 81 COLON-ALIGNED
     lv-font-name AT ROW 22.43 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 23.62 COL 31
     tb_excel AT ROW 24.81 COL 51.2 RIGHT-ALIGNED
     tb_runExcel AT ROW 24.81 COL 73.2 RIGHT-ALIGNED
     fi_file AT ROW 25.76 COL 29.2 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 27.29 COL 18
     btn-cancel AT ROW 27.33 COL 58
     "Available Columns" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 12.24 COL 2 WIDGET-ID 38
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 12.24 COL 61.60 WIDGET-ID 44
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 18.38 COL 2
     RECT-6 AT ROW 18.14 COL 1
     RECT-7 AT ROW 1.1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 98.6 BY 27.81.


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
         TITLE              = "Orders Booked By Order No"
         HEIGHT             = 28.14
         WIDTH              = 99.8
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
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rct-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ship-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rct-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ship-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_qty IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lbl_qty:HIDDEN IN FRAME FRAME-A           = TRUE
       lbl_qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_qty".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rd_qty IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rd_qty:HIDDEN IN FRAME FRAME-A           = TRUE
       rd_qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_contr IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_contr:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_contr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_PrtMisc:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Orders Booked By Order No */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Orders Booked By Order No */
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


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
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


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Beginning Order# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rct-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rct-date C-Win
ON LEAVE OF begin_rct-date IN FRAME FRAME-A /* Beginning Receipt Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ship-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship-date C-Win
ON LEAVE OF begin_ship-date IN FRAME FRAME-A /* Beginning Ship Date */
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

  RUN GetSelectionList.
  IF iColumnLength > 300 THEN
     MESSAGE "Report may not show all selected columns appropriately. " SKIP
             "Use Excel Output for all selected column values. (" iColumnLength ")"
         VIEW-AS ALERT-BOX WARNING BUTTONS OK.

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
      WHEN 6 THEN RUN output-to=port.
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


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
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


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* Ending Order# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rct-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rct-date C-Win
ON LEAVE OF end_rct-date IN FRAME FRAME-A /* Ending Receipt Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ship-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ship-date C-Win
ON LEAVE OF end_ship-date IN FRAME FRAME-A /* Ending Ship Date */
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


&Scoped-define SELF-NAME rd_qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_qty C-Win
ON VALUE-CHANGED OF rd_qty IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_contr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_contr C-Win
ON VALUE-CHANGED OF tb_contr IN FRAME FRAME-A /* Print Contribution? */
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


&Scoped-define SELF-NAME tb_PrtMisc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_PrtMisc C-Win
ON VALUE-CHANGED OF tb_PrtMisc IN FRAME FRAME-A /* Print Misc. Charges? */
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
   begin_ord-date = today
   end_ord-date   = today.

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_ord-no.
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalcPoMSF C-Win 
PROCEDURE CalcPoMSF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM opTotalMsf AS DEC NO-UNDO.

  DEF VAR v-basis-w AS DEC NO-UNDO. /* for po/po-adder2.p */
  DEF VAR v-len LIKE po-ordl.s-len NO-UNDO.
  DEF VAR v-wid LIKE po-ordl.s-wid NO-UNDO.
  DEF VAR v-dep LIKE po-ordl.s-len NO-UNDO.
  DEF VAR v-ord-qty LIKE po-ordl.ord-qty NO-UNDO.
  DEF VAR lv-orig-uom AS cha NO-UNDO.
  def var factor# as decimal no-undo.
  DEF VAR ll-ea AS LOG INIT NO NO-UNDO.
  DEF VAR lv-uom LIKE po-ordl.pr-qty-uom INIT NO NO-UNDO.
  DEF VAR fg-uom-list AS CHAR NO-UNDO.
  DEF VAR v-out-qty AS INT NO-UNDO.


  find sys-ctrl where sys-ctrl.company = cocode
                    and sys-ctrl.name = "poprint" 
                no-lock no-error.

  factor# = if avail sys-ctrl and can-do("Premier,Middlesx,16th's",sys-ctrl.char-fld) then .16 else 1.

  {ce/msfcalc.i}

    FIND FIRST item
          WHERE item.company EQ oe-ordl.company
            AND item.i-no    EQ po-ordl.i-no
          NO-LOCK NO-ERROR.

      ASSIGN
        v-basis-w = IF AVAIL item THEN item.basis-w ELSE v-basis-w
        v-dep = IF AVAIL item THEN item.s-dep ELSE v-dep
        v-len = (po-ordl.s-len)
        v-wid = (po-ordl.s-wid)
        v-ord-qty = (po-ordl.ord-qty)
        lv-orig-uom = po-ordl.pr-qty-uom 
        {po/calc10.i v-len} 
        {po/calc10.i v-wid}.

      IF NOT AVAIL item THEN
      FIND FIRST itemfg
          WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ po-ordl.i-no
          NO-LOCK NO-ERROR.

      IF AVAIL itemfg THEN
        RUN sys/ref/ea-um-fg.p (po-ordl.pr-qty-uom, OUTPUT ll-ea).

      IF ll-ea THEN ASSIGN lv-uom = po-ordl.pr-qty-uom.     

      IF v-len EQ 0 AND AVAIL ITEM AND
         ITEM.i-code EQ "R" AND item.r-wid GT 0 THEN
         DO:
            v-len = 12.
            IF lv-orig-uom EQ "ROLL" THEN
            DO:
              FIND FIRST uom WHERE uom.uom EQ "ROLL" NO-LOCK NO-ERROR.
              IF AVAIL uom THEN ASSIGN v-ord-qty = v-ord-qty * uom.mult.
            END.
         END.

      RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

      IF po-ordl.pr-qty-uom{2} EQ "EA"       OR
        (NOT po-ordl.item-type AND
         LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) GT 0) THEN
        opTotalMsf = IF v-corr THEN ((v-len * v-wid * .007 * dec(po-ordl.ord-qty{2})) / 1000)
                           ELSE ((((v-len * v-wid) / 144) * dec(po-ordl.ord-qty{2})) / 1000).
      else do:
        /*convert whatever the UOM is into "EACH" first*/

        opTotalMsf = 0.
        if po-ordl.pr-qty-uom NE "EA" then do:
           opTotalMsf = 0.
           run sys/ref/convquom.p(po-ordl.pr-qty-uom,
                               "EA",
                               v-basis-w,
                               v-len,
                               v-wid,
                               v-dep,
                               v-ord-qty,
                               output v-out-qty).

          /*now convert from "EACH" into MSF*/   
          opTotalMsf = if v-corr THEN
                       ((v-len * v-wid * .007 * v-out-qty) / 1000)
                    else
                       ((((v-len * v-wid) / 144) * v-out-qty) / 1000).
          IF po-ordl.pr-qty-uom EQ "ROLL" THEN
             opTotalMsf = OpTotalMsf * (12 / v-len).
        end. 
      end.

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList C-Win 
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
  DISPLAY begin_ord-no end_ord-no begin_cust-no end_cust-no begin_i-no end_i-no 
          begin_ord-date end_ord-date begin_rct-date end_rct-date tb_UseRcptDate 
          begin_ship-date end_ship-date tb_UseShipDate tb_PrtMisc sl_avail 
          sl_selected rd-dest lv-ornt lv-font-no lines-per-page lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_ord-no end_ord-no begin_cust-no end_cust-no 
         begin_i-no end_i-no begin_ord-date end_ord-date begin_rct-date 
         end_rct-date tb_UseRcptDate begin_ship-date end_ship-date 
         tb_UseShipDate tb_PrtMisc sl_avail Btn_Add sl_selected Btn_Remove 
         btn_Up btn_down rd-dest lv-ornt lv-font-no lines-per-page td-show-parm 
         tb_excel tb_runExcel fi_file btn-ok btn-cancel Btn_Def
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
/* ------------------------------------------------ oe/rep/orders.p 11/99 JLF */
/* Orders Report                                                              */
/* -------------------------------------------------------------------------- */
DEF VAR str-tit4 AS cha NO-UNDO.
DEF VAR str-tit5 AS cha NO-UNDO.
/*{sys/form/r-top5DL2.f &TitleLendth="390" &FrameWidth="400"} */
{sys/form/r-top5DL3.f}
def buffer b-oe-ordl for oe-ordl.

def var v-cust like oe-ord.cust-no extent 2 init ["","zzzzzzzz"].
def var v-ord-no as int format ">>>>>>" extent 2 init [0,999999].
def var v-date as date format "99/99/9999" extent 2 init [today,today].
def var v-item as char format "x(15)" extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-ord-qty as log format "Ordered/Remaining" init yes.

def var v-unline as char format "x(80)" init
  "--------------- ------------------------- ------- ----------- ---".
def var v-tot-ord as dec format "->,>>>,>>9.99" extent 2.
def var v-tax-rate     as dec format ">,>>9.99<<<".
def var v-inv as log init no.
def var v-ship as log init no.
def var v-tot-tax like oe-ord.tax.
def var v-tot-freight like oe-ord.t-freight.
def var v-qty-lft like oe-ordl.qty.
def var v-ext-price like oe-ordl.t-price.
def var v-prt-cont as log init no.
def var v-margin as dec.
def var v-margin-tot as dec format "->>>,>>>,>>>,>>9.99".
def var v-password like sys-ctrl.char-fld label "Please Enter Password".
def var v-ext-cost as dec.
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

format  /* frame ord */
   oe-ord.ord-no
   oe-ord.est-no format "x(8)"
   oe-ord.job-no space(0) "-" space(0)
   oe-ord.job-no2 format "99"
   space(3) 
   oe-ord.ord-date
   space(3) 
   oe-ord.cust-no 
   oe-ord.cust-name skip
  WITH frame ord no-labels no-box no-underline stream-io width 132.

format /* frame ordm */
    oe-ordm.charge                       label "Charge"      at 10
    oe-ordm.dscr                         label "Description"
    oe-ordm.amt    format "->>>,>>9.99"  label "Price"       to 102 skip
  WITH frame ordm no-labels no-box down stream-io width 132.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
v-fr-tax = oe-ctrl.f-tax.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-ord-no[1] = begin_ord-no
 v-ord-no[2] = end_ord-no
 v-cust[1]   = begin_cust-no
 v-cust[2]   = end_cust-no
 v-item[1]   = begin_i-no
 v-item[2]   = end_i-no
 v-date[1]   = begin_ord-date
 v-date[2]   = end_ord-date
 v-ord-qty   = YES /*rd_qty eq "Ordered"*/
 v-prt-cont  = can-do(cSelectedList,"Margin$")  /*tb_contr*/.

IF v-prt-cont AND NOT security-flag THEN DO:
  RUN sys/ref/d-passwd.w (3, OUTPUT security-flag).
  v-prt-cont = security-flag.
END.


FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

  IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
  THEN ASSIGN str-tit4 = str-tit4 + ttRptSelected.TextList + " "
              str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + ",".        
  ELSE 
  ASSIGN str-tit4 = str-tit4 + 
           (IF ttRptSelected.HeadingFromLeft THEN
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
           ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
            str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + ",".        
END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

IF tb_excel THEN 
DO:
   /*excelheader = "Order#,Est#,Job#,Date,Cust,Name,"
   IF v-prt-cont THEN
     excelheader = excelheader + "Item/Misc Chg,Description,Quantity,Cost/M,Price,UOM,Ext Price,Margin$".
   ELSE
     excelheader = excelheader + "Item/Misc Chg,Description,Quantity,Cost/M,Price,UOM,Ext Price".
   */
  OUTPUT STREAM excel TO VALUE(fi_file).

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

SESSION:SET-WAIT-STATE ("general").

display "" /*str-tit4 str-tit5*/ with frame r-top.
/*VIEW FRAME r-top.*/
EMPTY TEMP-TABLE ExtList.

for each oe-ord
    where oe-ord.company  eq cocode
      and oe-ord.ord-no   ge v-ord-no[1]
      and oe-ord.ord-no   le v-ord-no[2]
      and oe-ord.cust-no  ge v-cust[1]
      and oe-ord.cust-no  le v-cust[2]
      and oe-ord.ord-date ge v-date[1]
      and oe-ord.ord-date le v-date[2]
      and oe-ord.stat 	  ne "D"
      and oe-ord.type     ne "T"
    use-index ord-no no-lock,

    first b-oe-ordl
    where b-oe-ordl.company eq cocode
      and b-oe-ordl.ord-no  eq oe-ord.ord-no
      and b-oe-ordl.i-no    ge v-item[1]
      and b-oe-ordl.i-no    le v-item[2]
    no-lock,

    first cust
    {sys/ref/custW.i}
      and cust.cust-no eq oe-ord.cust-no
    no-lock

    break by oe-ord.ord-no:
    {custom/statusMsg.i "'Processing Order # ' + string(b-oe-ordl.ord-no)"} 

    v-tot-ord[1] = 0.
    for each oe-ordl
       where oe-ordl.company eq oe-ord.company
         and oe-ordl.ord-no  eq oe-ord.ord-no
         and oe-ordl.i-no    ge v-item[1]
         and oe-ordl.i-no    le v-item[2]
      no-lock
      break by oe-ordl.ord-no:

        if NOT first(oe-ordl.ord-no) AND first-of(oe-ordl.ord-no) then put skip(1).
        assign
          v-ship      = oe-ordl.stat ne "I" and oe-ordl.stat ne "B"
          v-qty-lft   = oe-ordl.qty - (if v-ord-qty then 0 else oe-ordl.inv-qty)
          v-ext-price = 0.
        if v-qty-lft lt 0 then v-qty-lft = 0.

        find first itemfg {sys/look/itemfgrlW.i} 
                   and itemfg.i-no eq oe-ordl.i-no no-lock no-error.

        RUN  oe/GetPriceTotal.p(
          INPUT oe-ordl.qty,
          INPUT oe-ordl.price,
          INPUT oe-ordl.pr-uom,
          INPUT ( IF AVAIL itemfg THEN itemfg.case-count ELSE 0),
          INPUT oe-ordl.disc,
          OUTPUT v-ext-price
          ).  /* task 01241601 */
        /*if oe-ordl.pr-uom begins "L" then
          v-ext-price = 
            oe-ordl.price - round((oe-ordl.price * oe-ordl.disc) / 100, 2).
        else if oe-ordl.pr-uom eq "CS" then 
        do:
            find first itemfg {sys/look/itemfgrlW.i} 
                   and itemfg.i-no eq oe-ordl.i-no no-lock no-error.

            if avail itemfg and itemfg.case-count ne 0 then
              v-ext-price = ((v-qty-lft / itemfg.case-count) * oe-ordl.price) -
                      round((((v-qty-lft / itemfg.case-count) *
                              oe-ordl.price) * oe-ordl.disc) / 100, 2).
            else
              v-ext-price = (v-qty-lft * oe-ordl.price) -
                      round(((v-qty-lft * oe-ordl.price) *
                             oe-ordl.disc) / 100, 2).
        end.
        else if oe-ordl.pr-uom eq "C" then
          v-ext-price = ((v-qty-lft / 100) * oe-ordl.price) -
                    round((((v-qty-lft / 100) *
                            oe-ordl.price) * oe-ordl.disc) / 100, 2).

        else if oe-ordl.pr-uom eq "EA" then
          v-ext-price = (v-qty-lft * oe-ordl.price) -
                    round(((v-qty-lft * oe-ordl.price) *
                           oe-ordl.disc) / 100, 2).

        else /** DEFAULT PER THOUSAND **/
          v-ext-price = ((v-qty-lft / 1000) * oe-ordl.price) -
                    round((((v-qty-lft / 1000) *
                            oe-ordl.price) * oe-ordl.disc) / 100, 2).*/

       /** CALCULATE FREIGHT CHARGES **/
        v-tot-freight = v-tot-freight +
                    (round(oe-ordl.t-freight / oe-ordl.qty, 2) * v-qty-lft).

       /** CALCULATE TAX CHARGES **/
        if oe-ordl.tax and v-tax-rate gt 0 then
          v-tot-tax = v-tot-tax + round((v-ext-price * v-tax-rate) / 100,2).

        /*if v-prt-cont then */
          assign
            v-ext-cost = (oe-ordl.cost * oe-ordl.qty) / 1000
            v-margin = v-ext-price - v-ext-cost.

        ASSIGN v-boardTotalQty = 0
               v-boardPo = 0
               v-boardPOQty = 0
               v-boardCost = 0
               v-boardTotalCost = 0
               v-boardProfit = 0
               v-OrderedMSF = 0
               .
        IF oe-ordl.po-no-po <> 0 THEN DO:
           FIND first po-ordl WHERE po-ordl.company        EQ oe-ordl.company
                    AND po-ordl.po-no          EQ oe-ordl.po-no-po
                    AND ((po-ordl.item-type    EQ YES AND TRIM(oe-ordl.job-no) NE "" AND
                          po-ordl.job-no       EQ oe-ordl.job-no AND po-ordl.job-no2 EQ oe-ordl.job-no2)   
                    OR (po-ordl.item-type    EQ NO AND po-ordl.i-no EQ oe-ordl.i-no))
                    NO-LOCK NO-ERROR.
           ASSIGN v-boardTotalQty = IF AVAIL po-ordl THEN po-ordl.t-rec-qty ELSE 0
                 v-boardPO = IF AVAIL oe-ordl THEN oe-ordl.po-no-po ELSE 0
                 v-boardPoQty = IF AVAIL po-ordl THEN po-ordl.ord-qty ELSE 0
                 v-boardCost = IF AVAIL po-ordl THEN po-ordl.cost ELSE 0
                 v-boardTotalCost = IF AVAIL po-ordl THEN po-ordl.t-cost ELSE 0
                 .

           RUN CalcPOMSF (OUTPUT v-OrderedMSF).
           IF v-OrderedMSF = ? THEN v-OrderedMSF = 0.
           v-PORecDate = ?.
           IF oe-ordl.po-no-po <> 0 THEN
           FOR EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company = oe-ordl.company 
                         AND fg-rcpth.rita-code = "R"
                         AND ( (fg-rcpth.trans-date >= begin_rct-date 
                         AND fg-rcpth.trans-date <= END_rct-date AND tb_UseRcptDate) OR NOT tb_UseRcptDate)
                         AND fg-rcpth.po-no = string(oe-ordl.po-no-po),
               EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code NO-LOCK
               BY fg-rcpth.trans-date:      
               ASSIGN v-PORecDate = fg-rcpth.trans-date.
               LEAVE.
            END.

        END.
        v-boardProfit = v-ext-price - v-boardTotalCost .

        FIND itemfg where itemfg.company  = cocode 
                        and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
        ASSIGN v-jobShipQty = 0
               v-FGShipDate = ?.
        IF oe-ordl.job-no <> "" THEN
        FOR EACH fg-rcpth OF itemfg NO-LOCK WHERE fg-rcpth.rita-code = "S"
                         AND fg-rcpth.job-no = oe-ordl.job-no 
                         AND fg-rcpth.job-no2 = oe-ordl.job-no2
                         AND ((fg-rcpth.trans-date >= begin_ship-date
                         AND fg-rcpth.trans-date <= END_ship-date AND tb_UseShipDate)
                              OR NOT tb_UseShipDate)
                      ,
            EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code NO-LOCK
            BY fg-rcpth.trans-date:      
            ASSIGN v-jobShipQty = v-jobShipQty + fg-rdtlh.qty
                   v-FGShipDate = IF v-FGShipDate = ? THEN fg-rcpth.trans-date ELSE v-FGShipDate.
        END.        
        IF (oe-ordl.po-no-po <> 0 AND
           NOT CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.company = oe-ordl.company 
                         AND fg-rcpth.rita-code = "R"
                         AND fg-rcpth.trans-date >= begin_rct-date 
                         AND fg-rcpth.trans-date <= END_rct-date
                         AND fg-rcpth.po-no = string(oe-ordl.po-no-po) )
           AND tb_UseRcptDate) OR
            (oe-ordl.job-no <> "" AND 
            NOT CAN-FIND(FIRST fg-rcpth OF itemfg NO-LOCK WHERE fg-rcpth.rita-code = "S"
                         AND fg-rcpth.job-no = oe-ordl.job-no 
                         AND fg-rcpth.job-no2 = oe-ordl.job-no2
                         AND fg-rcpth.trans-date >= begin_ship-date
                         AND fg-rcpth.trans-date <= END_ship-date)
            AND tb_UseShipDate )
       THEN NEXT.

       /* ==== new for Selectable columns =====*/
       FIND FIRST oe-boll WHERE oe-boll.company = oe-ordl.company 
                            AND oe-boll.ord-no = oe-ordl.ord-no
                            AND oe-boll.i-no = oe-ordl.i-no NO-LOCK NO-ERROR.
       v-bol# = IF AVAIL oe-boll THEN oe-boll.bol-no ELSE 0.
       FIND FIRST ar-invl WHERE ar-invl.company = oe-ordl.company
                            AND ar-invl.ord-no = oe-ordl.ord-no
                            AND ar-invl.i-no = oe-ordl.i-no NO-LOCK NO-ERROR.
       v-inv# = IF AVAIL ar-invl THEN ar-invl.inv-no ELSE 0.
       IF v-inv# = 0  THEN DO:
          FIND FIRST inv-line WHERE inv-line.company = oe-ordl.company
                            AND inv-line.ord-no = oe-ordl.ord-no
                            AND inv-line.i-no = oe-ordl.i-no NO-LOCK NO-ERROR.
          v-inv# = IF AVAIL inv-line THEN inv-line.inv-no ELSE 0.
       END.
       BUFFER boe-ord:FIND-BY-ROWID(ROWID(oe-ord),NO-LOCK) .
       BUFFER boe-ordl:FIND-BY-ROWID(ROWID(oe-ordl),NO-LOCK) .
       ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".    
       ASSIGN v-Order%Profit = v-boardProfit / v-ext-price
              v-MSFRec = v-orderedMsf / v-boardpoQty * v-boardTotalQty              
              v-FGExtPrice = oe-ordl.price / 1000 * v-jobShipQty
              v-PORecCost = v-boardTotalCost / v-boardpoQty * v-boardTotalQty
              v-ProfitSold$ = v-FGExtPrice - v-PORecCost 
              v-ProfitSold% = v-ProfitSold$ / v-FGExtPrice
              v-UnitsBoard = v-qty-lft / v-boardpoQty
              v-UnitLoss$ = v-boardTotalQty * v-UnitsBoard - v-jobShipQty
              v-Loss% = v-UnitLoss$ / (v-boardTotalQty * v-UnitsBoard )
              .
       IF v-Order%Profit = ? THEN v-Order%Profit = 0.
       IF v-MSFRec = ? THEN v-MSFRec = 0.
       IF v-PORecCost = ? THEN v-PORecCost = 0.
       IF v-ProfitSold$ = ? THEN v-ProfitSold$ = 0.
       IF v-ProfitSold% = ? THEN v-ProfitSold% = 0.
       IF v-UnitsBoard = ? THEN v-UnitsBoard = 0.
       IF v-UnitLoss$ = ? THEN v-UnitLoss$ = 0.
       IF v-Loss% = ? THEN v-Loss% = 0.
       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
         cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

         IF INDEX(cTmpField,".") > 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 IF cFieldName BEGINS "oe-ordl" THEN hField = BUFFER boe-ordl:BUFFER-FIELD(cTmpField) .
                 ELSE IF cFieldName BEGINS "oe-ord" THEN hField = BUFFER boe-ord:BUFFER-FIELD(cTmpField).
                 IF hField <> ? THEN DO:                 
                     cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                     IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = cTmpField + IF cTmpField <> "" THEN "-" + string(oe-ordl.job-no2,"99") ELSE "".                  

                     IF cFieldName = "oe-ordl.price" OR cFieldName = "oe-ordl.cost"
                         THEN cTmpField = STRING(decimal(cTmpField),"->>>,>>9.99").

                     cDisplay = cDisplay + cTmpField + 
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                               .
                     cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".                    
                 END.
                 ELSE DO:
                    cTmpField = substring(cFieldName,1,int( entry( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + quoter(" ") + ",".
                 END.
         END.
         ELSE DO:       
            CASE cTmpField:                                   
                 {oerep/r-booko#N1.i} 
            END CASE.
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
         END.

      END.
      PUT UNFORMATTED cDisplay SKIP.
      IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
      END.
      /*===== end of new ===== */
      if v-prt-cont then do:
         if v-margin ne ? then v-margin-tot = v-margin-tot + v-margin.
      end.
      v-tot-ord[1] = v-tot-ord[1] + v-ext-price.
    end. /* each oe-ordl */
    IF tb_PrtMisc THEN
    for each oe-ordm
       where oe-ordm.company eq oe-ord.company
         and oe-ordm.ord-no  eq oe-ord.ord-no
       no-lock
      break by oe-ordm.ord-no:

        if first-of(oe-ordm.ord-no) then put skip(1) "Miscellaneous" at 10 skip.

        display oe-ordm.charge oe-ordm.dscr oe-ordm.amt with frame ordm.
        if oe-ordm.bill eq "N" then
          display "       N/C" @ oe-ordm.amt with frame ordm.

        down with frame ordm.
        IF tb_excel THEN
        DO:
            CREATE ExtList.
            ASSIGN
              ExtList.ord-no    = oe-ord.ord-no
              ExtList.est-no    = TRIM(oe-ord.est-no)
              ExtList.job-no    = TRIM(oe-ord.job-no) + "-" 
                                  + TRIM(STRING(oe-ord.job-no2, "99"))
              ExtList.ord-date  = oe-ord.ord-date
              ExtList.cust-no   = oe-ord.cust-no
              ExtList.cust-name = oe-ord.cust-name
              ExtList.i-no      = oe-ordm.charge
              ExtList.i-name    = oe-ordm.dscr
              ExtList.ext-price = IF oe-ordm.bill eq "N"
                                  THEN "N/C"  
                                  ELSE TRIM(STRING(oe-ordm.amt)).
           /*   MESSAGE "cr2"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK. */
        END.

        if oe-ordm.bill eq "Y" then 
        do:
            v-tot-ord[1] = v-tot-ord[1] + oe-ordm.amt.

            if oe-ordm.tax and v-tax-rate eq 0 then
              v-tot-tax = v-tot-tax + round((oe-ordm.amt * v-tax-rate) / 100,2).
        end.
    end. /* each oe-ordm */    

    /*put "Total Order" at 75 v-tot-ord[1] to 102 skip(1).*/
    v-tot-ord[2] = v-tot-ord[2] + v-tot-ord[1].    
    if last(oe-ord.ord-no) then 
    do:
        if v-prt-cont then 
        do:
            put skip(2) "Grand Total" at 75 v-tot-ord[2] to 112 skip.
            put "Grand Total Contribution" at 62 v-margin-tot to 112 skip(1).
        end.
        else 
        do:
            put skip(2) "Grand Total" at 75 v-tot-ord[2] to 102 skip(1).
        end.
    end.
end. /* each oe-ord */

FOR EACH ExtList:
    EXPORT STREAM excel DELIMITER "," Extlist.    
END.

IF tb_excel THEN 
DO:
    OUTPUT STREAM excel CLOSE.
    IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.
RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
SESSION:SET-WAIT-STATE ("").
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-old C-Win 
PROCEDURE run-report-old :
/*============
/* Orders Report                                                              */
/* -------------------------------------------------------------------------- */
DEF VAR str-tit4 AS cha NO-UNDO.
DEF VAR str-tit5 AS cha NO-UNDO.
{sys/form/r-top5DL.f}
/*{sys/form/r-topw.f}*/


def buffer b-oe-ordl for oe-ordl.

def var v-cust like oe-ord.cust-no extent 2 init ["","zzzzzzzz"].
def var v-ord-no as int format ">>>>>>" extent 2 init [0,999999].
def var v-date as date format "99/99/9999" extent 2 init [today,today].
def var v-item as char format "x(15)" extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-ord-qty as log format "Ordered/Remaining" init yes.

def var v-unline as char format "x(80)" init
  "--------------- ------------------------- ------- ----------- ---".
def var v-tot-ord as dec format "->,>>>,>>9.99" extent 2.
def var v-tax-rate     as dec format ">,>>9.99<<<".
def var v-inv as log init no.
def var v-ship as log init no.
def var v-tot-tax like oe-ord.tax.
def var v-tot-freight like oe-ord.t-freight.
def var v-qty-lft like oe-ordl.qty.
def var v-ext-price like oe-ordl.t-price.
def var v-prt-cont as log init no.
def var v-margin as dec.
def var v-margin-tot as dec format "->>>,>>>,>>>,>>9.99".
def var v-password like sys-ctrl.char-fld label "Please Enter Password".
def var v-ext-cost as dec.
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


format header
  "Order#  Est#         Job#     Date       Cust#    Name" skip
  fill("-",105) format "x(105)"
  with frame r-top .

format  /* frame ord */
   oe-ord.ord-no
   oe-ord.est-no format "x(8)"
   oe-ord.job-no space(0) "-" space(0)
   oe-ord.job-no2 format "99"
   space(3) 
   oe-ord.ord-date
   space(3) 
   oe-ord.cust-no 
   oe-ord.cust-name skip
  WITH frame ord no-labels no-box no-underline stream-io width 132.

FORMAT  /* frame ord1 */
    oe-ordl.i-no                          label "Item"  at 10
    oe-ordl.i-name format "x(25)"         label "Description"
    v-qty-lft      format "->>>,>>9"      label "Quantity"
    oe-ordl.cost   format "->>>,>>9.99"   label "Cost/M"
    oe-ordl.price  format "->>>,>>9.99"   label "Price"
    oe-ordl.pr-uom                        label "UOM"
    v-ext-price    format "->,>>>,>>9.99" label "Ext Price"
    v-margin       format "->,>>>,>>9.99" label "Margin$"
  WITH frame ordl no-labels no-box down stream-io width 132.

format /* frame ordl2 */
    oe-ordl.i-no                          label "Item"  at 10
    oe-ordl.i-name format "x(25)"         label "Description"
    v-qty-lft      format "->>>,>>9"      label "Quantity"
    oe-ordl.cost   format "->>>,>>9.99"   label "Cost/M"
    oe-ordl.price  format "->>>,>>9.99"   label "Price"
    oe-ordl.pr-uom                        label "UOM"
    v-ext-price    format "->,>>>,>>9.99" label "Ext Price"
  WITH frame ordl2  no-labels no-box down stream-io width 132.

format /* frame ordm */
    oe-ordm.charge                       label "Charge"      at 10
    oe-ordm.dscr                         label "Description"
    oe-ordm.amt    format "->>>,>>9.99"  label "Price"       to 102 skip
  WITH frame ordm no-labels no-box down stream-io width 132.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
v-fr-tax = oe-ctrl.f-tax.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-ord-no[1] = begin_ord-no
 v-ord-no[2] = end_ord-no
 v-cust[1]   = begin_cust-no
 v-cust[2]   = end_cust-no
 v-item[1]   = begin_i-no
 v-item[2]   = end_i-no
 v-date[1]   = begin_ord-date
 v-date[2]   = end_ord-date
 v-ord-qty   = rd_qty eq "Ordered"
 v-prt-cont  = can-do(cSelectedList,"Margin$")  /*tb_contr*/.

IF v-prt-cont AND NOT security-flag THEN DO:
  RUN sys/ref/d-passwd.w (3, OUTPUT security-flag).
  v-prt-cont = security-flag.
END.


FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

    ASSIGN str-tit4 = str-tit4 + 
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength + 1 - LENGTH(ttRptSelected.TextList))
            str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + ",".        
END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

IF tb_excel THEN 
DO:
   excelheader = "Order#,Est#,Job#,Date,Cust,Name,".
   IF v-prt-cont THEN
     excelheader = excelheader + "Item/Misc Chg,Description,Quantity,Cost/M,Price,UOM,Ext Price,Margin$".
   ELSE
     excelheader = excelheader + "Item/Misc Chg,Description,Quantity,Cost/M,Price,UOM,Ext Price".

  OUTPUT STREAM excel TO VALUE(fi_file).

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

SESSION:SET-WAIT-STATE ("general").

display "" with frame r-top.

EMPTY TEMP-TABLE ExtList.

for each oe-ord
    where oe-ord.company  eq cocode
      and oe-ord.ord-no   ge v-ord-no[1]
      and oe-ord.ord-no   le v-ord-no[2]
      and oe-ord.cust-no  ge v-cust[1]
      and oe-ord.cust-no  le v-cust[2]
      and oe-ord.ord-date ge v-date[1]
      and oe-ord.ord-date le v-date[2]
      and (oe-ord.stat    eq "N" or
           oe-ord.stat    eq "A" or
           oe-ord.stat    eq "U" or
           oe-ord.stat    eq "H" or
           oe-ord.stat    eq "D" or
           oe-ord.stat    eq "C" or
           oe-ord.stat    eq "P")
      and oe-ord.type     ne "T"
    use-index ord-no no-lock,

    first b-oe-ordl
    where b-oe-ordl.company eq cocode
      and b-oe-ordl.ord-no  eq oe-ord.ord-no
      and b-oe-ordl.i-no    ge v-item[1]
      and b-oe-ordl.i-no    le v-item[2]
    no-lock,

    first cust
    {sys/ref/custW.i}
      and cust.cust-no eq oe-ord.cust-no
    no-lock

    break by oe-ord.ord-no:

    display 
        oe-ord.ord-no
        trim(oe-ord.est-no)   @ oe-ord.est-no
        oe-ord.job-no
        oe-ord.job-no2
        oe-ord.ord-date       format "99/99/9999"
        oe-ord.cust-no
        oe-ord.cust-name
      with frame ord.

    v-tot-ord[1] = 0.

    for each oe-ordl
       where oe-ordl.company eq oe-ord.company
         and oe-ordl.ord-no  eq oe-ord.ord-no
         and oe-ordl.i-no    ge v-item[1]
         and oe-ordl.i-no    le v-item[2]
      no-lock
      break by oe-ordl.ord-no:

        if first-of(oe-ordl.ord-no) then put skip(1).

        assign
          v-ship      = oe-ordl.stat ne "I" and oe-ordl.stat ne "B"
          v-qty-lft   = oe-ordl.qty - (if v-ord-qty then 0 else oe-ordl.inv-qty)
          v-ext-price = 0.

        if v-qty-lft lt 0 then v-qty-lft = 0.  

        if oe-ordl.pr-uom begins "L" then
          v-ext-price = 
            oe-ordl.price - round((oe-ordl.price * oe-ordl.disc) / 100, 2).
        else if oe-ordl.pr-uom eq "CS" then 
        do:
            find first itemfg {sys/look/itemfgrlW.i} 
                   and itemfg.i-no eq oe-ordl.i-no
                no-lock no-error.

            if avail itemfg and itemfg.case-count ne 0 then
              v-ext-price = ((v-qty-lft / itemfg.case-count) * oe-ordl.price) -
                      round((((v-qty-lft / itemfg.case-count) *
                              oe-ordl.price) * oe-ordl.disc) / 100, 2).
            else
              v-ext-price = (v-qty-lft * oe-ordl.price) -
                      round(((v-qty-lft * oe-ordl.price) *
                             oe-ordl.disc) / 100, 2).
        end.
        else if oe-ordl.pr-uom eq "C" then
          v-ext-price = ((v-qty-lft / 100) * oe-ordl.price) -
                    round((((v-qty-lft / 100) *
                            oe-ordl.price) * oe-ordl.disc) / 100, 2).

        else if oe-ordl.pr-uom eq "EA" then
          v-ext-price = (v-qty-lft * oe-ordl.price) -
                    round(((v-qty-lft * oe-ordl.price) *
                           oe-ordl.disc) / 100, 2).

        else /** DEFAULT PER THOUSAND **/
          v-ext-price = ((v-qty-lft / 1000) * oe-ordl.price) -
                    round((((v-qty-lft / 1000) *
                            oe-ordl.price) * oe-ordl.disc) / 100, 2).

       /** CALCULATE FREIGHT CHARGES **/
        v-tot-freight = v-tot-freight +
                    (round(oe-ordl.t-freight / oe-ordl.qty, 2) * v-qty-lft).

       /** CALCULATE TAX CHARGES **/
        if oe-ordl.tax and v-tax-rate gt 0 then
          v-tot-tax = v-tot-tax + round((v-ext-price * v-tax-rate) / 100,2).

        if v-prt-cont then 
          assign
            v-ext-cost = (oe-ordl.cost * oe-ordl.qty) / 1000
            v-margin = v-ext-price - v-ext-cost.

        /*===========
        if v-prt-cont then 
        do:
            display 
                oe-ordl.i-no
                oe-ordl.i-name
                v-qty-lft
                oe-ordl.cost
                oe-ordl.price
                oe-ordl.pr-uom
                v-ext-price
                v-margin 
              with frame ordl.
            down with frame ordl.

            IF tb_excel THEN
            DO:

               CREATE ExtList.
               ASSIGN
                 ExtList.ord-no    = oe-ord.ord-no
                 ExtList.est-no    = TRIM(oe-ord.est-no)
                 ExtList.job-no    = TRIM(oe-ord.job-no) + "-" 
                                    + TRIM(STRING(oe-ord.job-no2, "99"))
                 ExtList.ord-date  = oe-ord.ord-date
                 ExtList.cust-no   = oe-ord.cust-no
                 ExtList.cust-name = oe-ord.cust-name
                 ExtList.i-no      = oe-ordl.i-no
                 ExtList.i-name    = oe-ordl.i-name
                 ExtList.qty-lft   = v-qty-lft
                 ExtList.cost      = oe-ordl.cost
                 ExtList.price     = oe-ordl.price
                 ExtList.pr-uom    = oe-ordl.pr-uom
                 ExtList.ext-price = TRIM(STRING(v-ext-price))
                 ExtList.margin    = TRIM(STRING(v-margin)).
            END.
        end.
        else 
        do:
            display 
                oe-ordl.i-no
                oe-ordl.i-name
                v-qty-lft
                oe-ordl.cost
                oe-ordl.price
                oe-ordl.pr-uom
                v-ext-price
              with frame ordl2.
            down with frame ordl2.

            IF tb_excel THEN
            DO:
               CREATE ExtList.
               ASSIGN
                 ExtList.ord-no    = oe-ord.ord-no
                 ExtList.est-no    = TRIM(oe-ord.est-no)
                 ExtList.job-no    = TRIM(oe-ord.job-no) + "-" 
                                    + TRIM(STRING(oe-ord.job-no2, "99"))
                 ExtList.ord-date  = oe-ord.ord-date
                 ExtList.cust-no   = oe-ord.cust-no
                 ExtList.cust-name = oe-ord.cust-name
                 ExtList.i-no      = oe-ordl.i-no
                 ExtList.i-name    = oe-ordl.i-name
                 ExtList.qty-lft   = v-qty-lft
                 ExtList.cost      = oe-ordl.cost
                 ExtList.price     = oe-ordl.price
                 ExtList.pr-uom    = oe-ordl.pr-uom
                 ExtList.ext-price = TRIM(STRING(v-ext-price)).
            END.
        end.     
        ======= */
        IF tb_excel THEN
            DO:
               CREATE ExtList.
               ASSIGN
                 ExtList.ord-no    = oe-ord.ord-no
                 ExtList.est-no    = TRIM(oe-ord.est-no)
                 ExtList.job-no    = TRIM(oe-ord.job-no) + "-" 
                                    + TRIM(STRING(oe-ord.job-no2, "99"))
                 ExtList.ord-date  = oe-ord.ord-date
                 ExtList.cust-no   = oe-ord.cust-no
                 ExtList.cust-name = oe-ord.cust-name
                 ExtList.i-no      = oe-ordl.i-no
                 ExtList.i-name    = oe-ordl.i-name
                 ExtList.qty-lft   = v-qty-lft
                 ExtList.cost      = oe-ordl.cost
                 ExtList.price     = oe-ordl.price
                 ExtList.pr-uom    = oe-ordl.pr-uom
                 ExtList.ext-price = TRIM(STRING(v-ext-price)).
        END.

       /* ==== new for Selectable columns =====*/
       BUFFER boe-ord:FIND-BY-ROWID(ROWID(oe-ord)) .
       BUFFER boe-ordl:FIND-BY-ROWID(ROWID(oe-ordl)) .
       ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".    

       DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
         cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

         IF INDEX(cTmpField,".") > 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 IF cFieldName BEGINS "oe-ordl" THEN hField = BUFFER boe-ordl:BUFFER-FIELD(cTmpField) .
                 ELSE IF cFieldName BEGINS "oe-ord" THEN hField = BUFFER boe-ord:BUFFER-FIELD(cTmpField).
                 IF hField <> ? THEN DO:                 
                     cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                     IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = cTmpField + IF cTmpField <> "" THEN "-" + string(oe-ord.job-no2,"99") ELSE "".                  

                     cDisplay = cDisplay + cTmpField + 
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                               .
                     cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".
                 END.
                 ELSE DO:
                    cTmpField = substring(cFieldName,1,int( entry( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + quoter(" ")  /*GetFieldValue(hField))*/ + ",".
                 END.
         END.
         ELSE DO:       
            CASE cTmpField:                                   
                 WHEN "v-qty-lft" THEN cVarValue = string(v-qty-lft,"->>>,>>9").
                 WHEN "v-ext-price" THEN cVarValue = string(v-ext-price,"->,>>>,>>9.99").
                 WHEN "v-margin" THEN cVarValue = STRING(v-margin,"->,>>>,>>9.99").
                 WHEN "v-orderedMsf" THEN
                 WHEN "v-jobShipQty" THEN
                 WHEN "v-boardProfit" THEN 
                 WHEN "v-boardPO" THEN ,v-boardpoQty,v-boardCost,v-boardTotalCost,v-boardTotalQty"

            END CASE.

            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
         END.
      END.
      PUT UNFORMATTED cDisplay SKIP.
      IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
      END.
      /*===== end of new ===== */

        if v-prt-cont then 
        do:
            if v-margin ne ? then 
              v-margin-tot = v-margin-tot + v-margin.
        end.

        v-tot-ord[1] = v-tot-ord[1] + v-ext-price.
    end. /* each oe-ordl */

    for each oe-ordm
       where oe-ordm.company eq oe-ord.company
         and oe-ordm.ord-no  eq oe-ord.ord-no
       no-lock
      break by oe-ordm.ord-no:

        if first-of(oe-ordm.ord-no) then put skip(1) "Miscellaneous" at 10 skip.

        display 
            oe-ordm.charge oe-ordm.dscr oe-ordm.amt
          with frame ordm.

        if oe-ordm.bill eq "N" then
          display "       N/C" @ oe-ordm.amt with frame ordm.

        down with frame ordm.

        IF tb_excel THEN
        DO:
            CREATE ExtList.
            ASSIGN
              ExtList.ord-no    = oe-ord.ord-no
              ExtList.est-no    = TRIM(oe-ord.est-no)
              ExtList.job-no    = TRIM(oe-ord.job-no) + "-" 
                                  + TRIM(STRING(oe-ord.job-no2, "99"))
              ExtList.ord-date  = oe-ord.ord-date
              ExtList.cust-no   = oe-ord.cust-no
              ExtList.cust-name = oe-ord.cust-name
              ExtList.i-no      = oe-ordm.charge
              ExtList.i-name    = oe-ordm.dscr
              ExtList.ext-price = IF oe-ordm.bill eq "N"
                                  THEN "N/C"  
                                  ELSE TRIM(STRING(oe-ordm.amt)).
        END.

        if oe-ordm.bill eq "Y" then 
        do:
            v-tot-ord[1] = v-tot-ord[1] + oe-ordm.amt.

            if oe-ordm.tax and v-tax-rate eq 0 then
              v-tot-tax = v-tot-tax + round((oe-ordm.amt * v-tax-rate) / 100,2).
        end.
    end. /* each oe-ordm */

    put skip "------------" to 102 skip.

    if oe-ord.stat eq "H" then
      put "** THIS ORDER IS ON CREDIT HOLD **" to 50 .
    else if oe-ord.stat eq "D" then
      put "** THIS ORDER IS DELETED **" to 50 .
    else if oe-ord.stat eq "C" then
      put "** THIS ORDER IS CLOSED **" to 50 .

    IF tb_excel AND LOOKUP(oe-ord.stat, "H,D,C") NE 0 THEN
    DO:
        CREATE ExtList.
        ASSIGN
          ExtList.ord-no    = oe-ord.ord-no
          ExtList.est-no    = TRIM(oe-ord.est-no)
          ExtList.job-no    = TRIM(oe-ord.job-no) + "-" 
                              + TRIM(STRING(oe-ord.job-no2, "99"))
          ExtList.ord-date  = oe-ord.ord-date
          ExtList.cust-no   = oe-ord.cust-no
          ExtList.cust-name = oe-ord.cust-name.

        CASE oe-ord.stat:
            WHEN "H" THEN
              ExtList.i-name  = "** THIS ORDER IS ON CREDIT HOLD **".
            WHEN "D" THEN
               ExtList.i-name = "** THIS ORDER IS DELETED **".
            WHEN "C" THEN
               ExtList.i-name = "** THIS ORDER IS CLOSED **".
        END CASE.
    END.

    put "Total Order" at 75 v-tot-ord[1] to 102 skip(1).

    v-tot-ord[2] = v-tot-ord[2] + v-tot-ord[1].

    if last(oe-ord.ord-no) then 
    do:
        if v-prt-cont then 
        do:
            put skip(2) "Grand Total" at 75 v-tot-ord[2] to 112 skip.
            put "Grand Total Contribution" at 62 v-margin-tot to 112 skip(1).
        end.
        else 
        do:
            put skip(2) "Grand Total" at 75 v-tot-ord[2] to 102 skip(1).
        end.
    end.
end. /* each oe-ord */

/* if they're not here, they don't go.. */
FOR EACH ExtList:
    EXPORT STREAM excel DELIMITER "," Extlist.
END.

IF tb_excel THEN 
DO:
    OUTPUT STREAM excel CLOSE.

    IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").
=*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldvalue C-Win 
FUNCTION GetFieldvalue RETURNS CHARACTER
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

