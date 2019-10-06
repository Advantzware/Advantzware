&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-mulsrt.w

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

def var fcust as ch init "" NO-UNDO.
def var tcust like fcust init "zzzzzzzz" NO-UNDO.
def var fship as ch init "" NO-UNDO.
def var tship like fcust init "zzzzzzzz" NO-UNDO.
def var fshpz like ar-inv.zip init "" NO-UNDO.
def var tshpz like fshpz init "zzzzzzzzzz" NO-UNDO.
def var fsman as char format "x(3)" init "" NO-UNDO.
def var tsman like fsman init "zzz" NO-UNDO.
def var fdate as date format "99/99/9999" NO-UNDO.
def var tdate like fdate NO-UNDO.
def var v-det as log format "Detail/Summary" init YES NO-UNDO.
def var v-sort as char format "x(3)" init "" NO-UNDO.
def var v-inc-fc as log init NO NO-UNDO.

def var v-sort-list as char init "CIDSZOM" NO-UNDO.
def var v-sort-desc as char init
     "Customer,Invoice#,Invoice Date,Ship To,Ship To Zip,Order#,SalesRep" NO-UNDO.

def var v-date like ar-inv.inv-date NO-UNDO.
def var v-ord  like ar-invl.ord-no NO-UNDO.
def var v-pric like ar-invl.unit-pr NO-UNDO.
def var v-uom  like ar-invl.pr-uom NO-UNDO.

def var v-sman-no as   char format "x(3)" no-undo.
def var v-qty     as   int extent 6 NO-UNDO.
def var v-amt     as   dec extent 6 NO-UNDO.
def var v-exc     as   LOG NO-UNDO.
def var v-name    like cust.name format "x(21)" NO-UNDO.
def var v-hdr     as   char extent 10 NO-UNDO.
def var v-tot-wght     as   DEC extent 6 NO-UNDO.

def var v-pct as dec format "99.99" no-undo.
def var v-fac as int no-undo.
def var v-ship like ar-inv.ship-id no-undo.
def var v-shpz like ar-inv.sold-zip no-undo.
def var v-total as char no-undo.

def TEMP-TABLE w-data NO-UNDO
  field i-no      like ar-invl.i-no
  field inv-no    like ar-invl.inv-no
  field rec-id    as recid.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

DEF BUFFER xtt-report FOR tt-report.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF STREAM excel.
DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.




ASSIGN cTextListToSelect = "Customer,Name,SRep,Ship To#,Ship Zip,Inv#,Inv Date," 
                           + "Order#,Item No,Item Name,Item Description,Cust Part#,"
                           + "Price,UOM,Qty,Inv Amt,Inv Weight"
       cFieldListToSelect = "cust,name,rep,shipto,ship-zip,inv#,inv-date," +
                            "ord,item-no,item-name,desc,cust-part," +
                            "price,uom,qty,inv-amt,inv-wt"
       cFieldLength = "8,30,4,8,8,6,8," + "8,15,30,30,15," + "13,4,11,14,10"
       cFieldType = "c,c,c,c,c,i,c," + "i,c,c,c,c," + "i,c,i,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Customer,Name,SRep,Ship To#,Ship Zip,Inv#,Inv Date," 
                           + "Order#,Item No,Item Name,Item Description,Cust Part#,"
                           + "Price,UOM,Qty,Inv Amt" .

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
begin_cust-no end_cust-no begin_ship-to end_ship-to begin_ship-to-zip ~
end_ship-to-zip begin_slsmn end_slsmn begin_inv-date end_inv-date fi_sort ~
tb_fin-chg sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust-no end_cust-no ~
begin_ship-to end_ship-to begin_ship-to-zip end_ship-to-zip begin_slsmn ~
end_slsmn begin_inv-date end_inv-date tb_cust-total tb_shipto-total fi_sort ~
tb_fin-chg sl_avail sl_selected rd-dest lv-ornt lines-per-page lv-font-no ~
lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getInvSman C-Win 
FUNCTION getInvSman RETURNS CHARACTER
  ( INPUT picust-no AS CHAR,
    INPUT pi-inv-no AS INT)  FORWARD.

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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_inv-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_ship-to AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Ship-To#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ship-to-zip AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Ship-To Zip" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX":U 
     LABEL "Beginning Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_inv-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ship-to AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Ship-To#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ship-to-zip AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending Ship-To Zip" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX":U INITIAL "zzz" 
     LABEL "Ending Salesrep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-mulsrt.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE fi_sort AS CHARACTER FORMAT "XXX" 
     LABEL "Sort By?" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

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

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 8.19.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 11.43.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE tb_cust-total AS LOGICAL INITIAL yes 
     LABEL "Show Customer Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_fin-chg AS LOGICAL INITIAL no 
     LABEL "Include Finance Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_shipto-total AS LOGICAL INITIAL yes 
     LABEL "Show Shipto Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.8 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_cust-list AT ROW 1.91 COL 31.8 WIDGET-ID 6
     btnCustList AT ROW 1.91 COL 64.2 WIDGET-ID 8
     begin_cust-no AT ROW 3.05 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 3.05 COL 72 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_ship-to AT ROW 4 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Ship-To#"
     end_ship-to AT ROW 4 COL 72 COLON-ALIGNED HELP
          "Enter Ending Ship-To#"
     begin_ship-to-zip AT ROW 4.95 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Ship-To Zip Code"
     end_ship-to-zip AT ROW 4.95 COL 72 COLON-ALIGNED HELP
          "Enter Ending Ship-To Zip Code"
     begin_slsmn AT ROW 5.91 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 5.91 COL 72 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_inv-date AT ROW 6.86 COL 29 COLON-ALIGNED
     end_inv-date AT ROW 6.86 COL 72 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     tb_cust-total AT ROW 8.1 COL 60 RIGHT-ALIGNED WIDGET-ID 2
     tb_shipto-total AT ROW 8.1 COL 86 RIGHT-ALIGNED WIDGET-ID 4
     fi_sort AT ROW 9.29 COL 29 COLON-ALIGNED HELP
          "Choose up to three categories to sort by."
     tb_fin-chg AT ROW 11.19 COL 33
     sl_avail AT ROW 13.29 COL 4.4 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 13.29 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 13.29 COL 59.8 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 14.29 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 15.29 COL 40.4 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 16.33 COL 40.4 WIDGET-ID 40
     btn_down AT ROW 17.33 COL 40.4 WIDGET-ID 42
     rd-dest AT ROW 19.24 COL 5 NO-LABEL
     lv-ornt AT ROW 19.24 COL 31 NO-LABEL
     lines-per-page AT ROW 19.24 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 21.86 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 22.81 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 24.05 COL 31
     tb_excel AT ROW 24.95 COL 50.6 RIGHT-ALIGNED
     tb_runExcel AT ROW 24.95 COL 71.6 RIGHT-ALIGNED
     fi_file AT ROW 25.76 COL 28.6 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 27.57 COL 17
     btn-cancel AT ROW 27.57 COL 55
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 12.57 COL 59.8 WIDGET-ID 44
     "~"S~"hipto" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 9.76 COL 42
          FGCOLOR 9 
     "~"C~"ustomer" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 9.05 COL 42
          FGCOLOR 9 
     "~"I~"nvoice#" VIEW-AS TEXT
          SIZE 11 BY .67 AT ROW 9.05 COL 56
          FGCOLOR 9 
     "invoice ~"D~"ate" VIEW-AS TEXT
          SIZE 16 BY .67 AT ROW 9.05 COL 70
          FGCOLOR 9 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 18.67 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "sales~"R~"ep" VIEW-AS TEXT
          SIZE 13 BY .67 AT ROW 10.48 COL 42
          FGCOLOR 9 
     "ship-to ~"Z~"ip" VIEW-AS TEXT
          SIZE 14 BY .67 AT ROW 9.76 COL 56
          FGCOLOR 9 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 28.38.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
     "~"O~"rder" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 9.76 COL 70
          FGCOLOR 9 
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 12.57 COL 5.2 WIDGET-ID 38
     RECT-6 AT ROW 19 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 28.38.


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
         TITLE              = "Sales With Multiple Sorts"
         HEIGHT             = 28.62
         WIDTH              = 95.6
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
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_inv-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ship-to:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ship-to-zip:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_inv-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ship-to:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ship-to-zip:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_cust-total IN FRAME FRAME-A
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       tb_cust-total:PRIVATE-DATA IN FRAME FRAME-A     = 
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

/* SETTINGS FOR TOGGLE-BOX tb_shipto-total IN FRAME FRAME-A
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       tb_shipto-total:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sales With Multiple Sorts */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sales With Multiple Sorts */
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
ON LEAVE OF begin_ship-to IN FRAME FRAME-A /* Beginning Ship-To# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ship-to-zip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship-to-zip C-Win
ON LEAVE OF begin_ship-to-zip IN FRAME FRAME-A /* Beginning Ship-To Zip */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  SESSION:SET-WAIT-STATE ("general").

  ASSIGN {&DISPLAYED-OBJECTS}.

  IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "southpak"
       THEN is-xprint-form = YES.     
  ELSE is-xprint-form = NO.

  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust-no,
                    INPUT END_cust-no).
  END.       

  RUN GetSelectionList.
  run run-report. 
  STATUS DEFAULT "Processing Complete".
  SESSION:SET-WAIT-STATE ("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type="Salesman"
                            &begin_cust=begin_slsmn
                            &END_cust= begin_slsmn
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Salesman"
                             &begin_cust= begin_slsmn
                             &END_cust=begin_slsmn
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Salesman"
                                  &begin_cust= begin_slsmn
                                  &END_cust=begin_slsmn
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
     END.
       END.
       WHEN 6 THEN RUN OUTPUT-to-port.

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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
DO:
  RUN Move-Field ("Down").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
DO:
  RUN Move-Field ("Up").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
ON LEAVE OF end_ship-to IN FRAME FRAME-A /* Ending Ship-To# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ship-to-zip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ship-to-zip C-Win
ON LEAVE OF end_ship-to-zip IN FRAME FRAME-A /* Ending Ship-To Zip */
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


&Scoped-define SELF-NAME fi_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_sort C-Win
ON LEAVE OF fi_sort IN FRAME FRAME-A /* Sort By? */
DO:
   assign {&self-name}.

   ASSIGN
     tb_cust-total:SCREEN-VALUE = IF INDEX(fi_sort:SCREEN-VALUE,"C") NE 0 THEN "YES" ELSE "NO"
     tb_shipto-total:SCREEN-VALUE = IF INDEX(fi_sort:SCREEN-VALUE,"S") NE 0 THEN "YES" ELSE "NO".
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

  ASSIGN
     begin_inv-date = date(01,01,year(today))
     END_inv-date   = TODAY
     fi_file = "c:\tmp\multisal.csv".
  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "HR10",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  {custom/usrprint.i}
  RUN DisplaySelectionList2.
  ASSIGN
     tb_cust-total:SCREEN-VALUE = IF INDEX(fi_sort:SCREEN-VALUE,"C") NE 0 THEN "YES" ELSE "NO"
     tb_shipto-total:SCREEN-VALUE = IF INDEX(fi_sort:SCREEN-VALUE,"S") NE 0 THEN "YES" ELSE "NO".

   RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'HR10',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""HR10""}

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
                            INPUT 'HR10',
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
                                  INPUT 'HR10').


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
  DISPLAY tb_cust-list begin_cust-no end_cust-no begin_ship-to end_ship-to 
          begin_ship-to-zip end_ship-to-zip begin_slsmn end_slsmn begin_inv-date 
          end_inv-date tb_cust-total tb_shipto-total fi_sort tb_fin-chg sl_avail 
          sl_selected rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tb_cust-list btnCustList begin_cust-no end_cust-no 
         begin_ship-to end_ship-to begin_ship-to-zip end_ship-to-zip 
         begin_slsmn end_slsmn begin_inv-date end_inv-date fi_sort tb_fin-chg 
         sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-sort-fields1 C-Win 
PROCEDURE get-sort-fields1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input  parameter ip-sort as char no-undo.
  def output parameter op-sort as char no-undo.


  op-sort = if ip-sort eq "C" then ar-inv.cust-no                       else
            if ip-sort eq "I" then string(ar-inv.inv-no,"9999999999")   else
            if ip-sort eq "D" then string(year(ar-inv.inv-date),"9999") +
                                   string(month(ar-inv.inv-date),"99")  +
                                   string(day(ar-inv.inv-date),"99")    else
            if ip-sort eq "S" then v-ship                               else
            if ip-sort eq "Z" then v-shpz                               else
            if ip-sort eq "O" then string(ar-invl.ord-no,"9999999999")  else
            if ip-sort eq "R" then v-sman-no                            else "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-sort-fields2 C-Win 
PROCEDURE get-sort-fields2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input  parameter ip-sort as char no-undo.
  def output parameter op-sort as char no-undo.


  op-sort = if ip-sort eq "C" then ar-cash.cust-no                      else
            if ip-sort eq "I" then string(ar-cashl.inv-no,"9999999999") else
            if ip-sort eq "D" then string(year(ar-cash.check-date),"9999") +
                                   string(month(ar-cash.check-date),"99")  +
                                   string(day(ar-cash.check-date),"99") else
            if ip-sort eq "S" then v-ship                               else
            if ip-sort eq "Z" then v-shpz                               else
            if ip-sort eq "O" then "0000000000"                         else
            if ip-sort eq "R" then cust.sman                            else "".

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
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE report-from-inv C-Win 
PROCEDURE report-from-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  do i = 1 to 3:
     v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman  
                 else ar-invl.sman[i].  
   /*   IF ar-invl.sman[i] <> "" THEN
          ASSIGN v-sman-no = ar-invl.sman[i]. */

    if v-sman-no  lt fsman                          or
       v-sman-no  gt tsman                          or
       (i ne 1 and
        (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.

    create xtt-report.

    assign
     xtt-report.rec-id = recid(ar-invl)
     xtt-report.key-04 = string(ar-invl.inv-no,"9999999999")
     xtt-report.key-05 = v-sman-no
     xtt-report.key-06 = string(ar-invl.ord-no,"9999999999")
     xtt-report.key-07 = if ar-invl.misc then ar-invl.i-name else
                         if ar-invl.i-no ne "" then ar-invl.i-no else
                         "AR SALE"
     xtt-report.key-08 = v-ship
     xtt-report.key-09 = tt-report.key-09
     xtt-report.key-10 = v-shpz.

    run get-sort-fields1 (substr(v-sort,1,1), output xtt-report.key-01).
    run get-sort-fields1 (substr(v-sort,2,1), output xtt-report.key-02).
    run get-sort-fields1 (substr(v-sort,3,1), output xtt-report.key-03).
    LEAVE.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* -------------------------------------------------------------------------- */
/*                                                                            */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-top3lw.f}*/

DEF VAR lv-r-no LIKE oe-retl.r-no NO-UNDO.
DEF VAR lv-type AS CHAR NO-UNDO.
DEF VAR cSman LIKE ar-invl.sman[1] NO-UNDO.

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

{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.

form space(8)
     w-data.i-no
     itemfg.i-name        format "x(20)"
     ar-invl.part-dscr1   format "x(20)"
     ar-invl.part-no
     v-pric               format "->,>>>,>>9.99<<"
     v-uom
     v-qty[1]             format "->>,>>>,>>>"          label "Qty"
     v-amt[1]             format "->>,>>>,>>9.99"       label "Inv Amt"

    with no-box frame detail down STREAM-IO WIDTH 132.

/*form header
     skip(1)
     "Customer"
     "Name                          "
     "SRep"
     "Ship To#"
     "Ship Zip  "
     "  Inv#"
     "Inv Date"
     "Order#"
     space(14)
     "        Qty"
     "       Inv Amt"
     skip
     "--------"
     "------------------------------"
     "----"
     "--------"
     "----------"
     "------"
     "--------"
     "------"
     space(14)
     "-----------"
     "--------------"
     skip(1)

    with frame r-top.*/

form cust.cust-no
     cust.name
     tt-report.key-05     format "x(3)"
     space(2)
     tt-report.key-08     format "x(8)"
     tt-report.key-10     format "x(10)"
     w-data.inv-no
     v-date               FORMAT "99/99/99"
     v-ord                format ">>>>>>"
     space(14)
     v-qty[2]             format "->>,>>>,>>>"
     v-amt[2]             format "->>,>>>,>>9.99"
     skip(1)
    with no-box no-labels frame summary down STREAM-IO WIDTH 132.


  SESSION:SET-WAIT-STATE ("general").

  assign
   str-tit2 = c-win:title
   {sys/inc/ctrtext.i str-tit2 112}

   fcust       = begin_cust-no
   tcust       = end_cust-no
   fship       = begin_ship-to
   tship       = END_ship-to
   fshpz       = begin_ship-to-zip
   tshpz       = END_ship-to-zip
   fsman       = begin_slsmn
   tsman       = end_slsmn
   fdate       = begin_inv-date
   tdate       = END_inv-date
   /*v-det       = tb_detailed*/
   v-inc-fc    = tb_fin-chg
   v-sort      = fi_SORT
   lSelected   = tb_cust-list.

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

        IF LOOKUP(ttRptSelected.TextList, "Qty,Inv Amt,Inv Weight") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.
 IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
 END.

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  IF tb_excel THEN do:
      OUTPUT STREAM excel TO VALUE(fi_file).
      /*IF v-det THEN
         excelheader = "Customer,Name,SRep,Ship To#,Ship Zip,Inv#,Inv Date,"
                     + "Order#,Item No,Item Name,Item Description,Cust Part#,"
                     + "Price,UOM,Qty,Inv Amt".
      ELSE
         excelheader = "Customer,Name,SRep,Ship To#,Ship Zip,Inv#,Inv Date,"
                     + "Order#,Qty,Inv Amt".*/

      PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
  END.

  if td-show-parm then run show-param.

  display "" with frame r-top.

  ASSIGN
   v-qty = 0
   v-amt = 0
   v-tot-wght = 0   .

  FOR EACH tt-report:
    DELETE tt-report.
  END.

FOR each ar-inv
      where ar-inv.company  eq cocode
        and ar-inv.posted   eq yes
        AND ar-inv.cust-no  GE fcust
        AND ar-inv.cust-no  LE tcust
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq ar-inv.cust-no
        AND ttCustList.log-fld no-lock) else true)
        and ar-inv.inv-date ge fdate
        and ar-inv.inv-date le tdate
        and (ar-inv.type    ne "FC" or v-inc-fc)
      NO-LOCK:
      {custom/statusMsg.i " 'Processing Customer#  '  + ar-inv.cust-no "}                          

    create tt-report.


    assign
     tt-report.key-09 = ar-inv.cust-no
     tt-report.key-10 = "ar-inv"
     tt-report.rec-id = recid(ar-inv).

  end.

  FOR each cust
      where cust.company eq cocode
        AND cust.cust-no GE fcust
        AND cust.cust-no LE tcust
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
        AND ttCustList.log-fld no-lock) else true)
      no-lock,

      each ar-cash
      where ar-cash.company    eq cocode
        and ar-cash.cust-no    eq cust.cust-no
        and ar-cash.check-date ge fdate
        and ar-cash.check-date le tdate
        and ar-cash.posted     eq yes
      no-lock,

      EACH ar-cashl
      WHERE ar-cashl.c-no    EQ ar-cash.c-no
        AND ar-cashl.posted  EQ YES
        AND ar-cashl.memo    EQ YES
        AND CAN-FIND(FIRST account
                     WHERE account.company EQ ar-cashl.company
                       AND account.actnum  EQ ar-cashl.actnum
                       AND account.type    EQ "R")
      NO-LOCK:
    {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
    create tt-report.
    assign
     tt-report.key-09 = cust.cust-no
     tt-report.key-10 = "ar-cashl"
     tt-report.rec-id = recid(ar-cashl).

  end.

  for each tt-report
      where tt-report.key-01 eq ""
        and tt-report.key-02 eq ""
        and tt-report.key-03 eq ""
        and tt-report.key-04 eq ""
        and tt-report.key-05 eq ""
        and tt-report.key-06 eq ""
        and tt-report.key-07 eq ""
        and tt-report.key-08 eq "",

      first cust
      where cust.company eq cocode
        and cust.cust-no eq tt-report.key-09
      no-lock

      transaction:
     {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
    if tt-report.key-10 eq "ar-inv" then do:

      find ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock.

      run ship-info.

      if v-ship ge fship and
         v-ship le tship and
         v-shpz ge fshpz and
         v-shpz le tshpz then
      for each ar-invl
          where ar-invl.x-no eq ar-inv.x-no
            and (ar-invl.billable or not ar-invl.misc)
          no-lock:
        run report-from-inv. 
      end.

      delete tt-report.
    end.

    else
    if tt-report.key-10 eq "ar-cashl" then do:
      find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock.
      find ar-cash  where ar-cash.c-no    eq ar-cashl.c-no no-lock.

      ASSIGN cSman = getInvSman(cust.cust-no ,ar-cashl.inv-no).

      assign
       v-exc            = yes
       tt-report.key-04 = string(ar-cashl.inv-no,"9999999999")
       tt-report.key-05 = cSman /*cust.sman  string(ar-inv.sman-no) */
       tt-report.key-06 = "0000000000"
       tt-report.key-07 = "MEMO".

      release ar-inv.

      RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

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

        run ship-info.

        if v-ship ge fship and
           v-ship le tship and
           v-shpz ge fshpz and
           v-shpz le tshpz then

/**  gdm - 07060908***********************************
        if lv-type eq "items" then do:
          release ar-invl.
          find first oe-retl
              where oe-retl.company eq cocode
                and oe-retl.r-no    eq oe-reth.r-no
                and oe-retl.line    eq ar-cashl.line
              no-lock no-error.
          if avail oe-retl then
          find first ar-invl
              where ar-invl.company eq cocode
                and ar-invl.cust-no eq ar-cash.cust-no
                and ar-invl.inv-no  eq ar-cashl.inv-no
                and ar-invl.i-no    eq oe-retl.i-no
                and (ar-invl.billable or not ar-invl.misc)
              no-lock no-error.
          if avail ar-invl then do:
            run report-from-inv.

            delete tt-report.
          end.
        end.
        else
**  gdm - 07060908***********************************/

        if lv-type   eq "freight"                  and
           cust.sman ge fsman                      and
           cust.sman le tsman                      then
          assign
           v-exc         = no
           tt-report.key-07 = "FREIGHT"
           tt-report.key-08 = v-ship
           tt-report.key-10 = v-shpz.

        else
        if lv-type   eq "tax"                  and
           cust.sman ge fsman                  and
           cust.sman le tsman                  then
          assign
           v-exc         = no
           tt-report.key-07 = "TAX"
           tt-report.key-08 = v-ship
           tt-report.key-10 = v-shpz.

        else
        if cust.sman ge fsman and
           cust.sman le tsman then
          assign
           v-exc         = no
           tt-report.key-08 = v-ship
           tt-report.key-10 = v-shpz.
      end.

      else
      if cust.sman    ge fsman and
         cust.sman    le tsman and
         cust.cust-no ge fship and
         cust.cust-no le tship and
         cust.zip     ge fshpz and
         cust.zip     le tshpz then
        assign
         v-exc         = no
         tt-report.key-08 = cust.cust-no
         tt-report.key-10 = cust.zip.

      if avail tt-report then do:
        if v-exc then delete tt-report.

        else do:
          run get-sort-fields2 (substr(v-sort,1,1), output tt-report.key-01).
          run get-sort-fields2 (substr(v-sort,2,1), output tt-report.key-02).
          run get-sort-fields2 (substr(v-sort,3,1), output tt-report.key-03).
        end.
      end.
    end.
  end.

  FOR EACH xtt-report:

  END.

  for each tt-report,

      first cust
      where cust.company eq cocode
        and cust.cust-no eq tt-report.key-09
      no-lock

      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            by tt-report.key-04
            by tt-report.key-05
            by tt-report.key-06

      transaction:

    create w-data.
    assign
     w-data.i-no    = tt-report.key-07
     w-data.inv-no  = int(tt-report.key-04)
     w-data.rec-id  = tt-report.rec-id.

    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq w-data.i-no
        no-lock no-error.

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
       v-tot-wght[1] = ar-invl.t-weight 
       v-pct    = 1.

      do i = 1 to 3:
        if ar-invl.sman[i] eq tt-report.key-05 then
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
         v-amt[1] = ar-cashl.amt-paid - ar-cashl.amt-disc.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

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
            assign v-pric = ar-invl.unit-pr.

            do i = 1 to 3:
              if ar-invl.sman[i] eq tt-report.key-05 then
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

            v-amt[1] = v-amt[1] * v-pct.
          end.
        end.
      end.
    end.

  /*  IF w-data.inv-no NE 0 THEN
        FIND FIRST inv-head WHERE inv-head.company EQ cocode
        AND inv-head.inv-no EQ w-data.inv-no NO-LOCK NO-ERROR .

    IF AVAIL ar-invl THEN
        ASSIGN v-tot-wght[1] = ar-invl.t-weight .                                      */

            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = string(cust.cust-no,"x(8)") .
                         WHEN "name"   THEN cVarValue = string(cust.NAME,"x(30)").
                         WHEN "rep"   THEN cVarValue = STRING(tt-report.key-05,"x(3)").
                         WHEN "shipto"  THEN cVarValue = STRING(tt-report.key-08,"x(8)") .
                         WHEN "ship-zip"   THEN cVarValue = STRING(tt-report.key-10,"x(8)") .
                         WHEN "inv#"  THEN cVarValue = STRING(w-data.inv-no,">>>>>>") .
                         WHEN "inv-date"   THEN cVarValue = STRING(v-date,"99/99/99") .
                         WHEN "ord"  THEN cVarValue = STRING(v-ord,">>>>>>>>") .
                         WHEN "item-no"    THEN cVarValue = IF AVAIL itemfg THEN string(itemfg.i-name,"x(15)")  ELSE STRING(w-data.i-no,"x(15)").
                         WHEN "item-name"   THEN cVarValue = IF AVAIL ar-invl THEN string(ar-invl.part-dscr1,"x(30)") ELSE "".
                         WHEN "desc"   THEN cVarValue = IF AVAIL ar-invl THEN STRING(ar-invl.part-dscr1,"x(30)") ELSE "".
                         WHEN "cust-part"  THEN cVarValue = IF AVAIL ar-invl THEN STRING(ar-invl.part-no,"x(15)") ELSE "".
                         WHEN "price"   THEN cVarValue = STRING(v-pric,"->,>>>,>>9.99<<") .
                         WHEN "uom"  THEN cVarValue = STRING(v-uom,"x(4)") .
                         WHEN "qty"   THEN cVarValue = STRING(v-qty[1],"->>,>>>,>>>") .
                         WHEN "inv-amt"  THEN cVarValue = STRING(v-amt[1],"->>,>>>,>>9.99") .
                         WHEN "inv-wt"  THEN cVarValue = IF AVAIL ar-invl THEN STRING(v-tot-wght[1],">>>,>>9.99") ELSE "" .

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



    assign
     v-qty[2] = v-qty[2] + v-qty[1]
     v-amt[2] = v-amt[2] + v-amt[1]
     v-tot-wght[2] = v-tot-wght[2] + v-tot-wght[1] .

    if last-of(tt-report.key-06) then do with frame summary:
      /*display cust.cust-no
              cust.name
              tt-report.key-05
              tt-report.key-08
              tt-report.key-10
              w-data.inv-no
              STRING(v-date,"99/99/99") @ v-date
              v-ord
              v-qty[2]
              v-amt[2].

      down.

      IF NOT v-det AND tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' cust.cust-no      '",'
             '"' cust.name         '",'
             '"' tt-report.key-05  '",'
             '"' tt-report.key-08  '",'
             '"' tt-report.key-10  '",'
             '"' w-data.inv-no     '",'
             '"' STRING(v-date)    '",'
             '"' STRING(v-ord,">>>>>>") '",'
             '"' STRING(v-qty[2],"->>>>>>>>") '",'
             '"' STRING(v-amt[2],"->>>>>>>9.99") '",'
             SKIP.*/

      assign
       v-qty[3] = v-qty[3] + v-qty[2]
       v-amt[3] = v-amt[3] + v-amt[2]
       v-tot-wght[3] = v-tot-wght[3] + v-tot-wght[2] 

       v-qty[2] = 0
       v-amt[2] = 0
       v-tot-wght[2] = 0   .
    end.

    if last-of(tt-report.key-03) then do:
      if index("CSM",substr(v-sort,3,1)) ne 0 then do with frame summary:
        /*underline cust.name v-qty[2] v-amt[2].*/

        v-total = entry(index(v-sort-list,substr(v-sort,3,1)),v-sort-desc).

       /* display fill(" ",23 - length(trim(v-total))) + trim(v-total) +
                " TOTALS"   @ cust.name
                v-qty[3]    @ v-qty[2]
                v-amt[3]    @ v-amt[2].

        down.*/
        PUT str-line  SKIP.
        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = "" .
                         WHEN "name"   THEN cVarValue = "".
                         WHEN "rep"   THEN cVarValue = "".
                         WHEN "shipto"  THEN cVarValue = "" .
                         WHEN "ship-zip"   THEN cVarValue = "" .
                         WHEN "inv#"  THEN cVarValue = "" .
                         WHEN "inv-date"   THEN cVarValue = "" .
                         WHEN "ord"  THEN cVarValue = "" .
                         WHEN "item-no"    THEN cVarValue = "".
                         WHEN "item-name"   THEN cVarValue = "".
                         WHEN "desc"   THEN cVarValue = "".
                         WHEN "cust-part"  THEN cVarValue = "".
                         WHEN "price"   THEN cVarValue = "".
                         WHEN "uom"  THEN cVarValue = "".
                         WHEN "qty"   THEN cVarValue = STRING(v-qty[3],"->>,>>>,>>>") .
                         WHEN "inv-amt"  THEN cVarValue = STRING(v-amt[3],"->>,>>>,>>9.99") .
                         WHEN "inv-wt"  THEN cVarValue = STRING(v-tot-wght[3],">>>,>>9.99") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED fill(" ",23 - length(trim(v-total))) + trim(v-total) +  " Total"  substring(cDisplay,30,350) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  fill(" ",23 - length(trim(v-total))) + trim(v-total) + ' Totals ,'
                        substring(cExcelDisplay,4,350) SKIP.
             END.


        put skip(1).
      end.

      assign
       v-qty[4] = v-qty[4] + v-qty[3]
       v-amt[4] = v-amt[4] + v-amt[3]
       v-tot-wght[4] = v-tot-wght[4] + v-tot-wght[3] 

       v-qty[3] = 0
       v-amt[3] = 0
       v-tot-wght[3] = 0.
    end.

    if last-of(tt-report.key-02) then do:
      if index("CSM",substr(v-sort,2,1)) ne 0 then do with frame summary:
        /*underline cust.name v-qty[2] v-amt[2].*/

        v-total = entry(index(v-sort-list,substr(v-sort,2,1)),v-sort-desc).

        /*display fill(" ",23 - length(trim(v-total))) + trim(v-total) +
                " TOTALS"   @ cust.name
                v-qty[4]    @ v-qty[2]
                v-amt[4]    @ v-amt[2].*/
        PUT str-line  SKIP.
        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = "" .
                         WHEN "name"   THEN cVarValue = "".
                         WHEN "rep"   THEN cVarValue = "".
                         WHEN "shipto"  THEN cVarValue = "" .
                         WHEN "ship-zip"   THEN cVarValue = "" .
                         WHEN "inv#"  THEN cVarValue = "" .
                         WHEN "inv-date"   THEN cVarValue = "" .
                         WHEN "ord"  THEN cVarValue = "" .
                         WHEN "item-no"    THEN cVarValue = "".
                         WHEN "item-name"   THEN cVarValue = "".
                         WHEN "desc"   THEN cVarValue = "".
                         WHEN "cust-part"  THEN cVarValue = "".
                         WHEN "price"   THEN cVarValue = "".
                         WHEN "uom"  THEN cVarValue = "".
                         WHEN "qty"   THEN cVarValue = STRING(v-qty[4],"->>,>>>,>>>") .
                         WHEN "inv-amt"  THEN cVarValue = STRING(v-amt[4],"->>,>>>,>>9.99") .
                         WHEN "inv-wt"  THEN cVarValue = STRING(v-tot-wght[4],">>>,>>9.99") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED fill(" ",23 - length(trim(v-total))) + trim(v-total) + " Total"  substring(cDisplay,30,350) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  fill(" ",23 - length(trim(v-total))) + trim(v-total) + ' Totals ,'
                        substring(cExcelDisplay,4,350) SKIP.
             END.

        down.
        put skip(1).
      end.

      assign
       v-qty[5] = v-qty[5] + v-qty[4]
       v-amt[5] = v-amt[5] + v-amt[4]
       v-tot-wght[5] = v-tot-wght[5] + v-tot-wght[4] 

       v-qty[4] = 0
       v-amt[4] = 0
       v-tot-wght[4] = 0.
    end.

    if last-of(tt-report.key-01) then do:
      if index("CSM",substr(v-sort,1,1)) ne 0 then do with frame summary:
        /*underline cust.name v-qty[2] v-amt[2].*/

        v-total = entry(index(v-sort-list,substr(v-sort,1,1)),v-sort-desc).

        /*display fill(" ",23 - length(trim(v-total))) + trim(v-total) +
                " TOTALS"   @ cust.name
                v-qty[5]    @ v-qty[2]
                v-amt[5]    @ v-amt[2].

        down.*/
        PUT str-line SKIP.
        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = "" .
                         WHEN "name"   THEN cVarValue = "".
                         WHEN "rep"   THEN cVarValue = "".
                         WHEN "shipto"  THEN cVarValue = "" .
                         WHEN "ship-zip"   THEN cVarValue = "" .
                         WHEN "inv#"  THEN cVarValue = "" .
                         WHEN "inv-date"   THEN cVarValue = "" .
                         WHEN "ord"  THEN cVarValue = "" .
                         WHEN "item-no"    THEN cVarValue = "".
                         WHEN "item-name"   THEN cVarValue = "".
                         WHEN "desc"   THEN cVarValue = "".
                         WHEN "cust-part"  THEN cVarValue = "".
                         WHEN "price"   THEN cVarValue = "".
                         WHEN "uom"  THEN cVarValue = "".
                         WHEN "qty"   THEN cVarValue = STRING(v-qty[5],"->>,>>>,>>>") .
                         WHEN "inv-amt"  THEN cVarValue = STRING(v-amt[5],"->>,>>>,>>9.99") .
                         WHEN "inv-wt"  THEN cVarValue = STRING(v-tot-wght[5],">>>,>>9.99") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED fill(" ",23 - length(trim(v-total))) + trim(v-total) + " Total"  substring(cDisplay,30,350) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  fill(" ",23 - length(trim(v-total))) + trim(v-total) + ' Totals ,'
                        substring(cExcelDisplay,4,350) SKIP.
             END.
        put skip(1).
      end.

      assign
       v-qty[6] = v-qty[6] + v-qty[5]
       v-amt[6] = v-amt[6] + v-amt[5]
       v-tot-wght[6] = v-tot-wght[6] + v-tot-wght[5] 

       v-qty[5] = 0
       v-amt[5] = 0
       v-tot-wght[5] = 0.
    end.

    if last(tt-report.key-01) then do with frame summary:
      put skip(1).
      ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = "" .
                         WHEN "name"   THEN cVarValue = "".
                         WHEN "rep"   THEN cVarValue = "".
                         WHEN "shipto"  THEN cVarValue = "" .
                         WHEN "ship-zip"   THEN cVarValue = "" .
                         WHEN "inv#"  THEN cVarValue = "" .
                         WHEN "inv-date"   THEN cVarValue = "" .
                         WHEN "ord"  THEN cVarValue = "" .
                         WHEN "item-no"    THEN cVarValue = "".
                         WHEN "item-name"   THEN cVarValue = "".
                         WHEN "desc"   THEN cVarValue = "".
                         WHEN "cust-part"  THEN cVarValue = "".
                         WHEN "price"   THEN cVarValue = "".
                         WHEN "uom"  THEN cVarValue = "".
                         WHEN "qty"   THEN cVarValue = STRING(v-qty[6],"->>,>>>,>>>") .
                         WHEN "inv-amt"  THEN cVarValue = STRING(v-amt[6],"->>,>>>,>>9.99") .
                         WHEN "inv-wt"  THEN cVarValue = STRING(v-tot-wght[6],">>>,>>9.99") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            PUT str-line SKIP.
            PUT UNFORMATTED "                Grands Total"  substring(cDisplay,29,350) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  '             Grands Totals ,'
                       substring(cExcelDisplay,4,350) SKIP.
             END.

    end.

    delete w-data.
  end.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ship-info C-Win 
PROCEDURE ship-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  if avail ar-inv then 
    if ar-inv.ship-id ne "" then
      assign
       v-ship = ar-inv.ship-id
       v-shpz = ar-inv.sold-zip.

    else
    if ar-inv.sold-id ne "" then
      assign
       v-ship = ar-inv.sold-id
       v-shpz = ar-inv.sold-zip.

    else
      assign
       v-ship = ar-inv.cust-no
       v-shpz = ar-inv.zip.

  else
  if avail cust then
    assign
     v-ship = cust.cust-no
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
  def var lv-label as cha NO-UNDO.

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
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + ",".
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name THEN
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getInvSman C-Win 
FUNCTION getInvSman RETURNS CHARACTER
  ( INPUT picust-no AS CHAR,
    INPUT pi-inv-no AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 DEFINE BUFFER buf-ar-inv FOR ar-inv.
 DEFINE BUFFER buf-ar-invl FOR ar-invl.
     DEF VAR vi AS INT NO-UNDO INIT 0.

find first buf-ar-inv
     where buf-ar-inv.company eq cocode
       and buf-ar-inv.cust-no eq picust-no
       and buf-ar-inv.inv-no  eq pi-inv-no
     no-lock no-error.

IF NOT AVAIL buf-ar-inv THEN
    RETURN cust.sman .

for each buf-ar-invl NO-LOCK where 
         buf-ar-invl.x-no eq buf-ar-inv.x-no
            and (buf-ar-invl.billable or not buf-ar-invl.misc):


  DO vi = 1 to 3:
/*       if (buf-ar-invl.sman[i]  lt fsman) OR (buf-ar-invl.sman[i]  gt tsman)  then next.  */
      IF buf-ar-invl.sman[vi] EQ "" THEN
          RETURN cust.sman. 
      ELSE
           RETURN buf-ar-invl.sman[vi].
  END. 

END.

RETURN "".


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

