&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fgrep\r-cycnt.w

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
DEFINE VARIABLE cc-codeValue AS CHARACTER NO-UNDO.

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

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
{custom/xprint.i}

DEF VAR lv-multi-faxout AS LOG NO-UNDO.  /*for faxing to multiple receipents */
DEF VAR lv-fax-image AS cha NO-UNDO.  /* fax imge file */
DEF VAR lv-prt-bypass AS LOG NO-UNDO.  /* bypass window's printer driver */

DEF VAR lv-date      AS DATE                 NO-UNDO.
DEF VAR lv-job-no    AS CHAR FORMAT "x(9)"   NO-UNDO.
DEF VAR li-palls     AS DEC FORMAT "->>,>>9" NO-UNDO.
DEF VAR v-cnt        AS INT                  NO-UNDO.
DEF VAR v-item-no    LIKE fg-bin.i-no        NO-UNDO.
DEF VAR v-i-name     LIKE itemfg.i-name      NO-UNDO.
DEF VAR v-itemfg     LIKE itemfg.cust-no     NO-UNDO.
DEF VAR v-tag        LIKE fg-bin.tag         NO-UNDO.
DEF VAR v-qty        AS CHAR                 NO-UNDO.
DEF VAR v-li-palls   AS CHAR                 NO-UNDO.
DEF VAR v-prnt-onh   AS LOG INIT "N"         NO-UNDO.
DEF VAR v-writein    AS CHAR FORMAT "X(21)" INIT "    _________________"   NO-UNDO.


DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR is-xprint AS LOG NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

DEF STREAM excel.

FORM
   fg-bin.i-no     LABEL "ITEM"
   itemfg.i-name   FORMAT "x(25)" LABEL "DESCRIPTION"
   itemfg.cust-no  LABEL "CUSTOMER"
   fg-bin.loc      LABEL "WHSE"
   fg-bin.loc-bin  LABEL "BIN"
   fg-bin.tag      LABEL "TAG" FORMAT "x(8)"
   lv-job-no       LABEL "JOB#"
   lv-date         FORMAT "99/99/99" LABEL "RCT DATE"
   fg-bin.qty      FORMAT "->>,>>>,>>9" LABEL "ON HAND"
   li-palls        LABEL "PALLETS"
   v-writein       LABEL "     QUANTITY COUNTED"
   SKIP         
WITH FRAME itemx NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 200.

FORM
   fg-bin.i-no     LABEL "ITEM"
   itemfg.i-name   FORMAT "x(25)" LABEL "DESCRIPTION"
   itemfg.cust-no  LABEL "CUSTOMER"
   fg-bin.loc      LABEL "WHSE"
   fg-bin.loc-bin  LABEL "BIN"
   fg-bin.tag      LABEL "TAG" FORMAT "x(8)"
   lv-job-no       LABEL "JOB#"
   lv-date         FORMAT "99/99/99" LABEL "RCT DATE"
   fg-bin.qty      FORMAT "->>,>>>,>>9" LABEL "ON HAND"
   /*li-palls        LABEL "PALLETS"*/
   v-writein       LABEL "     QUANTITY COUNTED"
   SKIP         
WITH FRAME itemx2 NO-BOX NO-LABELS DOWN STREAM-IO WIDTH 200.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR v-costM AS DEC FORMAT ">>>,>>9.99<<" NO-UNDO.
DEF VAR v-sellValue LIKE itemfg.sell-price NO-UNDO.
DEF VAR cSellUom LIKE itemfg.sell-uom NO-UNDO.
DEF VAR v-counted-date AS DATE NO-UNDO.
DEF VAR v-cust-no LIKE itemfg.cust-no NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "ITEM,DESCRIPTION,CUSTOMER," +
                      "JOB#,WHSE,BIN,TAG," +
                      "RCT DATE,ON HAND QTY,PALLETS,QUANTITY COUNTED,COUNTED DATE,COST/M,SELL VALUE,CUSTOMER PART #," +
                      "SELL UOM"
       cFieldListToSelect = "fg-bin.i-no,itemfg.i-name,v-cust-no," +
                            "lv-job-no,fg-bin.loc,fg-bin.loc-bin,v-tag," +
                            "lv-date,fg-bin.qty,li-palls,v-writein,v-counted-date,v-costM,v-sellValue,itemfg.part-no," +
                            "v-sellUom" 
       cFieldLength = "15,25,8," + "9,5,8,20," + "10,11,7,21,12,10,10,15," + "8"
       cFieldType   = "c,c,c," + "c,c,c,c," + "c,i,i,i,c,i,i,c," + "c" 
       .
ASSIGN cTextListToDefault  = "ITEM,DESCRIPTION,CUSTOMER," + "WHSE,BIN,TAG,JOB#," +
                             "RCT DATE,ON HAND QTY,PALLETS,QUANTITY COUNTED" .

{sys/inc/ttRptSel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_loc end_loc begin_i-no ~
end_i-no begin_code end_code begin_bin end_bin begin_cat end_cat ~
begin_cust-no end_cust-no rd_sort tb_dblspc tb_zero tb_prt-cust-owned ~
sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest ~
lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_loc end_loc begin_i-no end_i-no ~
begin_code end_code begin_bin end_bin begin_cat end_cat begin_cust-no ~
end_cust-no lbl_sort rd_sort tb_dblspc tb_zero tb_prt-cust-owned sl_avail ~
sl_selected rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_bin AS CHARACTER FORMAT "X(8)" 
     LABEL "From Bin" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(8)":U 
     LABEL "From Category" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE begin_code AS CHARACTER FORMAT "XX" 
     LABEL "From Cycle Count Code" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "From Customer#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "From Item#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc AS CHARACTER FORMAT "X(5)" 
     LABEL "From Location" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE end_bin AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Bin" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "To Category" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE end_code AS CHARACTER FORMAT "XX" INITIAL "zz" 
     LABEL "To Cycle Count Code" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Customer#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To Item#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
     LABEL "To Location" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-cycnt.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=6 (20 cpi for 150 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "10" 
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
     SIZE 23 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Item" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item", "Item",
"Bin", "Bin",
"Name", "Name",
"Customer", "Cust"
     SIZE 43 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 8.1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 12.14.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5.71 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 35 BY 5.71 NO-UNDO.

DEFINE VARIABLE tb_dblspc AS LOGICAL INITIAL no 
     LABEL "Double Space Lines?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_prt-cust-owned AS LOGICAL INITIAL no 
     LABEL "Include Customer Owned?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-pallet AS LOGICAL INITIAL no 
     LABEL "Print Pallets?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE tb_qty-oh AS LOGICAL INITIAL no 
     LABEL "Print Quantity On Hand?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_zero AS LOGICAL INITIAL no 
     LABEL "Print Zero Balance Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.8 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_loc AT ROW 2.67 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Location"
     end_loc AT ROW 2.67 COL 70 COLON-ALIGNED HELP
          "Enter Ending Location"
     begin_i-no AT ROW 3.62 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 3.62 COL 70 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_code AT ROW 4.57 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Cycle Count Code"
     end_code AT ROW 4.57 COL 70 COLON-ALIGNED HELP
          "Enter Ending Cycle Count Code"
     begin_bin AT ROW 5.52 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Bin"
     end_bin AT ROW 5.52 COL 70 COLON-ALIGNED HELP
          "Enter Ending Bin"
     begin_cat AT ROW 6.48 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Category"
     end_cat AT ROW 6.48 COL 70 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_cust-no AT ROW 7.43 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 7.43 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     lbl_sort AT ROW 8.95 COL 21 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 8.95 COL 35 HELP
          "Sort By Item, Bin, or Item Name" NO-LABEL
     tb_dblspc AT ROW 10.19 COL 35.2 WIDGET-ID 46
     tb_zero AT ROW 11.1 COL 35.2
     tb_qty-oh AT ROW 11.1 COL 67
     tb_prt-pallet AT ROW 11.95 COL 74
     tb_prt-cust-owned AT ROW 12 COL 35.2 WIDGET-ID 8
     sl_avail AT ROW 14.1 COL 3 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 14.1 COL 42 HELP
          "Default Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 14.1 COL 62.4 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 15.19 COL 42 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 16.38 COL 42 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 17.57 COL 42 WIDGET-ID 40
     btn_down AT ROW 18.76 COL 42 WIDGET-ID 42
     rd-dest AT ROW 21 COL 6 NO-LABEL
     lv-ornt AT ROW 21 COL 33 NO-LABEL
     lines-per-page AT ROW 21 COL 87 COLON-ALIGNED
     lv-font-no AT ROW 22.14 COL 37 COLON-ALIGNED
     lv-font-name AT ROW 23.1 COL 31 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 24.38 COL 33
     tb_excel AT ROW 25.52 COL 68 RIGHT-ALIGNED WIDGET-ID 2
     tb_runExcel AT ROW 25.52 COL 89.6 RIGHT-ALIGNED WIDGET-ID 4
     fi_file AT ROW 26.48 COL 46 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 6
     btn-ok AT ROW 28.38 COL 24
     btn-cancel AT ROW 28.38 COL 51.2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.48 COL 3
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 20.05 COL 6
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 13.38 COL 63 WIDGET-ID 44
     "Available Columns" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 13.38 COL 4 WIDGET-ID 38
     RECT-6 AT ROW 19.95 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100.4 BY 29.14.


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
         TITLE              = "Finished Goods Cycle Count Report"
         HEIGHT             = 29.14
         WIDTH              = 100.4
         MAX-HEIGHT         = 45.05
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.05
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_bin:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_loc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_bin:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_loc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_dblspc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prt-cust-owned:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-pallet IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_prt-pallet:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_prt-pallet:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_qty-oh IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_qty-oh:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_qty-oh:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Finished Goods Cycle Count Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Finished Goods Cycle Count Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bin C-Win
ON LEAVE OF begin_bin IN FRAME FRAME-A /* From Bin */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cat C-Win
ON LEAVE OF begin_cat IN FRAME FRAME-A /* From Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_code C-Win
ON HELP OF begin_code IN FRAME FRAME-A /* From Cycle Count Code */
DO:
  cc-codeValue = SELF:SCREEN-VALUE.
  RUN lookups/cc-codeFG.p (INPUT-OUTPUT cc-codeValue).
  IF cc-codeValue NE '' THEN
  SELF:SCREEN-VALUE = cc-codeValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_code C-Win
ON LEAVE OF begin_code IN FRAME FRAME-A /* From Cycle Count Code */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* From Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* From Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_loc C-Win
ON LEAVE OF begin_loc IN FRAME FRAME-A /* From Location */
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
  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_loc
                            &END_cust=END_loc
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:title 
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_loc
                             &END_cust=end_loc
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_loc
                                  &END_cust=end_loc
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
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

  RUN DisplaySelectionDefault.  /* task 04141407 */ 
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


&Scoped-define SELF-NAME end_bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bin C-Win
ON LEAVE OF end_bin IN FRAME FRAME-A /* To Bin */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cat C-Win
ON LEAVE OF end_cat IN FRAME FRAME-A /* To Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_code C-Win
ON HELP OF end_code IN FRAME FRAME-A /* To Cycle Count Code */
DO:
  cc-codeValue = SELF:SCREEN-VALUE.
  RUN lookups/cc-codeFG.p (INPUT-OUTPUT cc-codeValue).
  IF cc-codeValue NE '' THEN
  SELF:SCREEN-VALUE = cc-codeValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_code C-Win
ON LEAVE OF end_code IN FRAME FRAME-A /* To Cycle Count Code */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* To Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* To Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_loc C-Win
ON LEAVE OF end_loc IN FRAME FRAME-A /* To Location */
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


&Scoped-define SELF-NAME tb_dblspc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_dblspc C-Win
ON VALUE-CHANGED OF tb_dblspc IN FRAME FRAME-A /* Double Space Lines? */
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


&Scoped-define SELF-NAME tb_prt-cust-owned
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-cust-owned C-Win
ON VALUE-CHANGED OF tb_prt-cust-owned IN FRAME FRAME-A /* Include Customer Owned? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-pallet C-Win
ON VALUE-CHANGED OF tb_prt-pallet IN FRAME FRAME-A /* Print Pallets? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_qty-oh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_qty-oh C-Win
ON VALUE-CHANGED OF tb_qty-oh IN FRAME FRAME-A /* Print Quantity On Hand? */
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


&Scoped-define SELF-NAME tb_zero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zero C-Win
ON VALUE-CHANGED OF tb_zero IN FRAME FRAME-A /* Print Zero Balance Items? */
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

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
     APPLY "entry" TO begin_loc.
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
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect) .
  END.
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
  DISPLAY begin_loc end_loc begin_i-no end_i-no begin_code end_code begin_bin 
          end_bin begin_cat end_cat begin_cust-no end_cust-no lbl_sort rd_sort 
          tb_dblspc tb_zero tb_prt-cust-owned sl_avail sl_selected rd-dest 
          lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_loc end_loc begin_i-no end_i-no begin_code 
         end_code begin_bin end_bin begin_cat end_cat begin_cust-no end_cust-no 
         rd_sort tb_dblspc tb_zero tb_prt-cust-owned sl_avail Btn_Def 
         sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-first-Counted-date C-Win 
PROCEDURE get-first-Counted-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-date AS DATE NO-UNDO.

  op-date = ?.

  FOR EACH fg-rcpth
      WHERE fg-rcpth.company   EQ fg-bin.company
        AND fg-rcpth.i-no      EQ fg-bin.i-no
        AND fg-rcpth.rita-code = "C"
        AND fg-rcpth.job-no    EQ fg-bin.job-no
        AND fg-rcpth.job-no2   EQ fg-bin.job-no2

      /*USE-INDEX tran*/ NO-LOCK,

      EACH fg-rdtlh
      WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        /*AND (TRIM(fg-bin.job-no) NE "" OR
             (fg-rdtlh.loc     EQ fg-bin.loc     AND
              fg-rdtlh.loc-bin EQ fg-bin.loc-bin AND
              fg-rdtlh.tag     EQ fg-bin.tag AND
              fg-rdtlh.cust-no EQ fg-bin.cust-no))
              */
            NO-LOCK
      BY fg-rcpth.trans-date DESCENDING:

    IF (/*fg-rdtlh.loc     EQ fg-bin.loc     AND           /*Task# 01161410 "Solution"*/
        fg-rdtlh.loc-bin EQ fg-bin.loc-bin AND   */
        fg-rdtlh.tag     EQ fg-bin.tag AND
        fg-rdtlh.cust-no EQ fg-bin.cust-no) /*OR fg-bin.job-no <> ""*/ THEN .
    ELSE NEXT.
    op-date = fg-rcpth.trans-date.
    LEAVE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-first-date C-Win 
PROCEDURE get-first-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-date AS DATE NO-UNDO.


  op-date = 01/01/0001 .

  IF fg-bin.tag EQ "" THEN
  FOR EACH fg-rcpth
      WHERE fg-rcpth.company   EQ fg-bin.company
        AND fg-rcpth.rita-code EQ "R"
        AND fg-rcpth.i-no      EQ fg-bin.i-no
        AND fg-rcpth.job-no    EQ fg-bin.job-no
        AND fg-rcpth.job-no2   EQ fg-bin.job-no2
      USE-INDEX tran NO-LOCK,

      EACH fg-rdtlh
      WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        AND fg-rdtlh.tag       EQ ""
        AND (TRIM(fg-bin.job-no) NE "" OR
             (fg-rdtlh.loc     EQ fg-bin.loc     AND
              fg-rdtlh.loc-bin EQ fg-bin.loc-bin ))
      USE-INDEX rm-rdtl NO-LOCK

      BY fg-rcpth.trans-date DESC
      BY fg-rdtlh.trans-time:

    op-date = fg-rcpth.trans-date.
    LEAVE.
  END.

  ELSE
  FOR EACH fg-rdtlh
      WHERE fg-rdtlh.company   EQ fg-bin.company
        AND fg-rdtlh.rita-code EQ "R"
        AND fg-rdtlh.tag       EQ fg-bin.tag
      USE-INDEX tag NO-LOCK,

      EACH fg-rcpth
      WHERE fg-rcpth.r-no      EQ fg-rdtlh.r-no
        AND fg-rcpth.rita-code EQ fg-rdtlh.rita-code
        AND fg-rcpth.i-no      EQ fg-bin.i-no
        AND fg-rcpth.job-no    EQ fg-bin.job-no
        AND fg-rcpth.job-no2   EQ fg-bin.job-no2
      USE-INDEX r-no NO-LOCK

      BY fg-rcpth.trans-date DESC
      BY fg-rdtlh.trans-time:

    op-date = fg-rcpth.trans-date.
    LEAVE.
  END.

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
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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
/* ----------------------------------------------- fg/rep/fg-cyclc.p 10/93 cd */
/* Finished Goods - Cycle Count Code List                                     */
/* -------------------------------------------------------------------------- */

DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF BUFFER bitemfg FOR itemfg.
DEF VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.

{sys/form/r-top5DL.f} 

cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.


DEF VAR v-floc       AS CHAR FORMAT "x(5)" INIT ""                         NO-UNDO.
DEF VAR v-tloc       LIKE v-floc INIT "zzzzz"                              NO-UNDO.
DEF VAR v-cat        AS CHAR FORMAT "x(5)" EXTENT 2 INIT ["","zzzzz"]      NO-UNDO.
DEF VAR v-cust       AS CHAR FORMAT "x(8)" EXTENT 2 INIT ["","zzzzzzzz"]   NO-UNDO.
DEF VAR v-i-no       AS CHAR FORMAT "x(15)" EXTENT 2 INIT ["","zzzzzzzzzzzzzzz"] NO-UNDO.
DEF VAR v-code       AS CHAR FORMAT "XX" EXTENT 2 INIT ["","zz"]           NO-UNDO.
DEF VAR v-loc-bin    AS CHAR FORMAT "x(8)" EXTENT 2 INIT ["", "zzzzzzzz"]  NO-UNDO.
DEF VAR v-item-bin   AS CHAR FORMAT "!" INIT "I" NO-UNDO.
DEF VAR v-prnt-zer   AS LOG INIT "N"         NO-UNDO.
DEF VAR v-include-cust-owned AS LOG NO-UNDO.
DEF VAR excelheader  AS CHAR                 NO-UNDO.
DEF VAR lCountedDateSelected AS LOG NO-UNDO.
DEF BUFFER bfg-bin FOR fg-bin .                                        
DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
ASSIGN
   str-tit2 = c-win:TITLE
   {sys/inc/ctrtext.i str-tit2 112}
   v-floc        = begin_loc
   v-tloc        = end_loc
   v-i-no[1]     = begin_i-no
   v-i-no[2]     = end_i-no
   v-code[1]     = begin_code
   v-code[2]     = END_code
   v-loc-bin[1]  = begin_bin
   v-loc-bin [2] = END_bin
   v-cat[1]      = begin_cat
   v-cat[2]      = END_cat
   v-cust[1]     = begin_cust-no
   v-cust[2]     = end_cust-no
   v-item-bin    = SUBSTR(rd_sort,1,1)
   v-prnt-onh    = tb_qty-oh
   v-prnt-zer    = tb_zero
   v-include-cust-owned = tb_prt-cust-owned.

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
   /*lCountedDateSelected = IF lCountedDateSelected = NO AND ttRptSelected.FieldList = "v-counted-date" THEN YES ELSE NO.
   */
          cSlist = cSlist + ttRptSelected.FieldList + ",".
 END.
 lCountedDateSelected = CAN-DO(cslist,'v-counted-date').
 IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(cFileName).
/*
   IF tb_prt-pallet = TRUE THEN
      excelheader = "ITEM,DESCRIPTION,CUSTOMER,WHSE,BIN,TAG,JOB#,RCT DATE,ON HAND,PALLETS,QUANTITY COUNTED".
   ELSE
      excelheader = "ITEM,DESCRIPTION,CUSTOMER,WHSE,BIN,TAG,JOB#,RCT DATE,ON HAND,QUANTITY COUNTED".
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
*/   
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
 END.
{sys/inc/print1.i}
{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

SESSION:SET-WAIT-STATE ("general").
display "" /*str-tit4 str-tit5*/ with frame r-top.

EMPTY TEMP-TABLE tt-report.

IF v-i-no[1] EQ "" AND v-i-no[2] BEGINS "zzzzzzzzzz" 
  AND v-cust[1] GT "" THEN DO:

     FOR EACH itemfg FIELDS(i-name cust-no) WHERE itemfg.company EQ cocode                    
                    AND itemfg.cc-code GE v-code[1]
                    AND itemfg.cc-code LE v-code[2]
                    AND itemfg.cust-no GE v-cust[1]
                    AND itemfg.cust-no LE v-cust[2]
                    AND itemfg.procat  GE v-cat[1]
                    AND itemfg.procat  LE v-cat[2] NO-LOCK
                        USE-INDEX customer,
     EACH fg-bin NO-LOCK WHERE fg-bin.company EQ itemfg.company
                            AND fg-bin.i-no    EQ itemfg.i-no
                            AND fg-bin.i-no    GE v-i-no[1]
                            AND fg-bin.i-no    LE v-i-no[2]
                            AND fg-bin.loc     GE v-floc
                            AND fg-bin.loc     LE v-tloc
                            AND fg-bin.loc-bin GE v-loc-bin[1]
                            AND fg-bin.loc-bin LE v-loc-bin[2]
                            AND (fg-bin.qty    NE 0 OR v-prnt-zer) 
                            AND (IF fg-bin.cust-no GT "" THEN v-include-cust-owned
                                 ELSE TRUE)
                            USE-INDEX co-ino:


     {fgrep/r-cycntN.i}
  END.

END.
ELSE DO:

  FOR EACH fg-bin NO-LOCK WHERE fg-bin.company EQ cocode
                            AND fg-bin.i-no    GE v-i-no[1]
                            AND fg-bin.i-no    LE v-i-no[2]
                            AND fg-bin.loc     GE v-floc
                            AND fg-bin.loc     LE v-tloc
                            AND fg-bin.loc-bin GE v-loc-bin[1]
                            AND fg-bin.loc-bin LE v-loc-bin[2]
                            AND (fg-bin.qty    NE 0 OR v-prnt-zer) 
                            AND (IF fg-bin.cust-no GT "" THEN v-include-cust-owned
                                 ELSE TRUE)
                            USE-INDEX co-ino,
     FIRST itemfg FIELDS(i-name cust-no) WHERE itemfg.company EQ fg-bin.company
                    AND itemfg.i-no    EQ fg-bin.i-no
                    AND itemfg.cc-code GE v-code[1]
                    AND itemfg.cc-code LE v-code[2]
                    AND itemfg.cust-no GE v-cust[1]
                    AND itemfg.cust-no LE v-cust[2]
                    AND itemfg.procat  GE v-cat[1]
                    AND itemfg.procat  LE v-cat[2] NO-LOCK:

    {fgrep/r-cycntN.i}
  END.
END.
ASSIGN lv-date    = ?
       lv-job-no  = ""
       li-palls   = 0
       v-cnt      = 0
       v-item-no  = ""
       v-i-name   = ""
       v-itemfg   = ""
       v-tag      = ""
       v-qty      = ""
       v-li-palls = ""
       v-cust-no = "".

IF v-item-bin EQ "C" THEN RUN run-reportCust.
ELSE DO:

 FOR EACH tt-report WHERE tt-report.term-id EQ "",
   FIRST fg-bin NO-LOCK WHERE RECID(fg-bin) EQ tt-report.rec-id,
   FIRST itemfg NO-LOCK WHERE itemfg.company EQ fg-bin.company
                          AND itemfg.i-no    EQ fg-bin.i-no
   BREAK BY tt-report.key-01
         BY tt-report.key-02
         BY tt-report.key-03:

   RUN get-first-date (OUTPUT lv-date).

   lv-job-no = TRIM(fg-bin.job-no).

   IF lv-job-no NE "" THEN 
      lv-job-no = lv-job-no + "-" + STRING(fg-bin.job-no2,"99").

   ASSIGN
      li-palls = (IF fg-bin.case-count   EQ 0 THEN 1 ELSE fg-bin.case-count)   *
                 (IF fg-bin.cases-unit   EQ 0 THEN 1 ELSE fg-bin.cases-unit)   *
                 (IF fg-bin.units-pallet EQ 0 THEN 1 ELSE fg-bin.units-pallet)
      li-palls = fg-bin.qty / li-palls.

   {sys/inc/roundup.i li-palls}

   IF FIRST-OF(tt-report.key-02) THEN DO:
      ASSIGN
         v-cnt = 1
         v-item-no   = fg-bin.i-no   
         v-i-name    = itemfg.i-name.
      IF fg-bin.cust-no NE "" THEN
         v-itemfg = fg-bin.cust-no.
      ELSE
         v-itemfg = itemfg.cust-no.
   END.
   ELSE DO:
      ASSIGN
         v-item-no = ""
         v-itemfg  = "".
      IF v-cnt = 2 THEN
         v-i-name = itemfg.part-dscr1.
      ELSE 
         v-i-name = "".
   END.

   v-cnt = v-cnt + 1.

   
     ASSIGN v-tag = fg-bin.tag.

   /*IF v-prnt-onh = TRUE THEN*/
      ASSIGN
         v-qty       = STRING(fg-bin.qty)
         v-li-palls  = STRING(li-palls)
         v-cust-no = IF fg-bin.cust-no <> "" THEN fg-bin.cust-no ELSE itemfg.cust-no.
   /*ELSE
      ASSIGN
         v-qty       = ""
         v-li-palls  = "".
   */
   ASSIGN cDisplay = ""
          cTmpField = ""
          cVarValue = ""
          cExcelDisplay = ""
          cExcelVarValue = ""
          v-costM =       0
          v-sellValue =  0
          v-counted-date = ?
          cSellUom = "" .
   IF lCountedDateSelected THEN
       RUN get-first-counted-date(OUTPUT v-counted-date).
  /* FIND LAST oe-ordl WHERE
                 oe-ordl.company EQ fg-bin.company AND
                 oe-ordl.job-no EQ fg-bin.job-no AND
                 oe-ordl.job-no2 EQ fg-bin.job-no2 AND
                 oe-ordl.i-no EQ fg-bin.i-no AND
                 (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                 NO-LOCK NO-ERROR.
   */
   v-sellValue = IF AVAIL oe-ordl THEN oe-ordl.price * (1 - (oe-ordl.disc / 100)) 
                 ELSE itemfg.sell-price.

  cSellUom = IF AVAIL oe-ordl THEN oe-ordl.pr-uom ELSE itemfg.sell-uom .

   IF fg-bin.pur-uom NE "" THEN
   DO:
       if fg-bin.pur-uom eq "M" THEN v-costM = fg-bin.std-tot-cost.
       ELSE run sys/ref/convcuom.p(fg-bin.pur-uom, "M", 0, 0, 0, 0,fg-bin.std-tot-cost, output v-costM).
   END.
   ELSE if itemfg.prod-uom eq "M" THEN v-costM = fg-bin.std-tot-cost.
   ELSE run sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,fg-bin.std-tot-cost, output v-costM).
   IF v-sellValue = ? THEN v-sellValue = 0.
   IF v-costM = ? THEN v-costM = 0.

   BUFFER bfg-bin:FIND-BY-ROWID(ROWID(fg-bin), NO-LOCK) .
   BUFFER bitemfg:FIND-BY-ROWID(ROWID(itemfg), NO-LOCK) .
   DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       IF INDEX(cTmpField,".") > 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 hField = IF cTmpField BEGINS "i-name" OR cTmpField BEGINS "part-no" THEN BUFFER bitemfg:BUFFER-FIELD(cTmpField)
                          ELSE BUFFER bfg-bin:BUFFER-FIELD(cTmpField).
                 IF hField <> ? THEN DO:                 
                     cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                     /*IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = cTmpField + IF cTmpField <> "" THEN "-" + string(fg-bin.job-no2,"99") ELSE "".                  
                      */
                     /*IF ENTRY(i,cSelectedList) = "Tag" THEN cTmpField = SUBSTRING(cTmpField,16,8).*/

                     cDisplay = cDisplay + 
                               IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) = "C" THEN
                                 (cTmpField + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)))
                               ELSE IF LENGTH(cTmpField) <  int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) THEN
                                 (FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(cTmpField)) + cTmpField) + " "
                               ELSE cTmpField.
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
                /*"lv-job-no,lv-date,li-palls,v-writein,v-counted-date,v-costM,v-sellValue"  */
                 WHEN "lv-job-no" THEN cVarValue = string(lv-job-no).
                 WHEN "lv-date" THEN cVarValue = IF lv-date <> ? THEN string(lv-date,"99/99/9999") ELSE "".
                 WHEN "li-palls" THEN cVarValue = STRING(li-palls,"->>,>>9").
                 WHEN "v-writein" THEN cVarValue = STRING(v-writein).
                 WHEN "v-tag" THEN cVarValue = STRING(v-tag,"x(20)").
                 WHEN "v-cust-no" THEN cVarValue = string(v-cust-no).
                 WHEN "v-counted-date" THEN cVarValue = IF v-counted-date <> ? THEN STRING(v-counted-date) ELSE "".
                 WHEN "v-costM" THEN cVarValue = STRING(v-CostM,">>>,>>9.99<<").
                 WHEN "v-sellValue" THEN cVarValue = STRING(v-sellValue,">>>,>>9.99").
                 WHEN "v-sellUom" THEN cVarValue = STRING(cSellUom).
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       END.
   END.
   PUT UNFORMATTED cDisplay SKIP.
   IF tb_dblspc EQ YES THEN PUT SKIP(1).            /*Task# 01161410*/
   IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
   END.
/*=====
   IF tb_prt-pallet THEN DO :
      DISPLAY 
         fg-bin.i-no    WHEN FIRST-OF(tt-report.key-02)
         itemfg.i-name  WHEN FIRST-OF(tt-report.key-02)
         itemfg.cust-no WHEN FIRST-OF(tt-report.key-02)
         fg-bin.cust-no WHEN fg-bin.cust-no NE "" @ itemfg.cust-no
         lv-job-no
         fg-bin.loc
         fg-bin.loc-bin
         fg-bin.tag
         SUBSTR(fg-bin.tag,16,8) WHEN SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no @ fg-bin.tag
         lv-date
         fg-bin.qty     WHEN v-prnt-onh
         li-palls       WHEN v-prnt-onh
         v-writein
      WITH FRAME itemx.            
      DOWN WITH FRAME itemx.

      DISPLAY 
         itemfg.part-dscr1 WHEN FIRST-OF(tt-report.key-02) @ itemfg.i-name
      WITH FRAME itemx.           
      DOWN WITH FRAME itemx.

      IF tb_excel THEN
      DO:
         PUT STREAM excel UNFORMATTED
             '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(fg-bin.i-no, '"', "") ELSE "") '",'
             '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.i-name, '"', "") ELSE "")  '",'
             '"' (IF fg-bin.cust-no NE "" THEN fg-bin.cust-no ELSE IF first-of(tt-report.key-02) THEN itemfg.cust-no ELSE "")  '",'
             '"' fg-bin.loc       '",'
             '"' fg-bin.loc-bin   '",'
             '"' (IF SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no THEN SUBSTR(fg-bin.tag,16,8) ELSE fg-bin.tag) '",'
             '"' lv-job-no '",'
             '"' lv-date   '",'
             '"' (IF v-prnt-onh THEN STRING(fg-bin.qty) ELSE "") '",'
             '"' (IF v-prnt-onh THEN STRING(li-palls) ELSE "")  '",'
             '"' v-writein '",'
             SKIP
             '"' "" '",'
             '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.part-dscr1, '"', "") ELSE "") '",'
             SKIP.
      END.
   END.
   ELSE DO:
      DISPLAY 
         fg-bin.i-no    WHEN FIRST-OF(tt-report.key-02)
         itemfg.i-name  WHEN FIRST-OF(tt-report.key-02)
         itemfg.cust-no WHEN FIRST-OF(tt-report.key-02)
         fg-bin.cust-no WHEN fg-bin.cust-no NE "" @ itemfg.cust-no
         lv-job-no
         fg-bin.loc
         fg-bin.loc-bin
         fg-bin.tag
         SUBSTR(fg-bin.tag,16,8) WHEN SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no @ fg-bin.tag
         lv-date
         fg-bin.qty     WHEN v-prnt-onh
         /* li-palls       WHEN v-prnt-onh*/
         v-writein
      WITH FRAME itemx2.            
      DOWN WITH FRAME itemx2.

      DISPLAY itemfg.part-dscr1 WHEN FIRST-OF(tt-report.key-02) @ itemfg.i-name
         WITH FRAME itemx2.           
      DOWN WITH FRAME itemx2.

      IF tb_excel THEN 
         PUT STREAM excel UNFORMATTED
             '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(fg-bin.i-no, '"', "") ELSE "") '",'
             '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.i-name, '"', "") ELSE "")  '",'
             '"' (IF fg-bin.cust-no NE "" THEN fg-bin.cust-no ELSE IF first-of(tt-report.key-02) THEN itemfg.cust-no ELSE "")  '",'
             '"' fg-bin.loc       '",'
             '"' fg-bin.loc-bin   '",'
             '"' (IF SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no THEN SUBSTR(fg-bin.tag,16,8) ELSE fg-bin.tag) '",'
             '"' lv-job-no '",'
             '"' lv-date          '",'
             '"' (IF v-prnt-onh THEN STRING(fg-bin.qty) ELSE "") '",'
             '"' v-writein '",'
             SKIP
             '"' "" '",'
             '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.part-dscr1, '"', "") ELSE "") '",'
             SKIP.

   END.
   =====*/  
   IF LAST-OF(tt-report.key-02) THEN PUT SKIP(1).

 END.
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
END.

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-reportCust C-Win 
PROCEDURE run-reportCust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF BUFFER bitemfg FOR itemfg.
DEF BUFFER bfg-bin FOR fg-bin.

cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

FOR EACH tt-report 
  WHERE tt-report.term-id EQ "",
 FIRST fg-bin NO-LOCK 
  WHERE RECID(fg-bin) EQ tt-report.rec-id,
 FIRST itemfg NO-LOCK 
  WHERE itemfg.company EQ fg-bin.company
    AND itemfg.i-no    EQ fg-bin.i-no
 BREAK BY tt-report.key-03
       BY tt-report.key-02
       BY tt-report.key-04
       BY tt-report.key-05:

  RUN get-first-date (OUTPUT lv-date).

  ASSIGN lv-job-no = TRIM(fg-bin.job-no).

  IF lv-job-no NE "" THEN
     ASSIGN lv-job-no = lv-job-no + "-" + STRING(fg-bin.job-no2,"99").

  ASSIGN li-palls = (IF fg-bin.case-count   EQ 0 THEN 1 ELSE fg-bin.case-count) 
                  * (IF fg-bin.cases-unit   EQ 0 THEN 1 ELSE fg-bin.cases-unit)   
                  * (IF fg-bin.units-pallet EQ 0 THEN 1 ELSE fg-bin.units-pallet)
         li-palls = fg-bin.qty / li-palls.

  {sys/inc/roundup.i li-palls}

  IF FIRST-OF(tt-report.key-03) THEN DO:

      ASSIGN
         v-cnt = 1
         v-item-no   = fg-bin.i-no   
         v-i-name    = itemfg.i-name.
      IF fg-bin.cust-no NE "" THEN
         v-itemfg = fg-bin.cust-no.
      ELSE
         v-itemfg = itemfg.cust-no.
  END.
  ELSE DO:
     ASSIGN
        v-item-no = ""
        v-itemfg  = "".
     IF v-cnt = 2 THEN
        v-i-name = itemfg.part-dscr1.
     ELSE 
        v-i-name = "".
  END.

  ASSIGN v-cnt = v-cnt + 1.

 /* IF SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no THEN */
     ASSIGN v-tag = fg-bin.tag.
 /* ELSE
     ASSIGN v-tag = fg-bin.tag. */

  IF v-prnt-onh = TRUE THEN
     ASSIGN v-qty       = STRING(fg-bin.qty)
            v-li-palls  = STRING(li-palls).
  ELSE
     ASSIGN v-qty       = ""
            v-li-palls  = "".
  v-cust-no = IF fg-bin.cust-no <> "" THEN fg-bin.cust-no ELSE itemfg.cust-no.
  ASSIGN cDisplay = ""
          cTmpField = ""
          cVarValue = ""
          cExcelDisplay = ""
          cExcelVarValue = ""
          v-costM =       0
          v-sellValue =  0
          cSellUom = "" .

   RUN get-first-counted-date(OUTPUT v-counted-date).
   FIND LAST oe-ordl WHERE
                 oe-ordl.company EQ fg-bin.company AND
                 oe-ordl.job-no EQ fg-bin.job-no AND
                 oe-ordl.job-no2 EQ fg-bin.job-no2 AND
                 oe-ordl.i-no EQ fg-bin.i-no AND
                 (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                 NO-LOCK NO-ERROR.
   v-sellValue = IF AVAIL oe-ordl THEN oe-ordl.price * (1 - (oe-ordl.disc / 100)) 
                 ELSE itemfg.sell-price.
   cSellUom = IF AVAIL oe-ordl THEN oe-ordl.pr-uom ELSE itemfg.sell-uom .

   IF fg-bin.pur-uom NE "" THEN
   DO:
       if fg-bin.pur-uom eq "M" THEN v-costM = fg-bin.std-tot-cost.
       ELSE run sys/ref/convcuom.p(fg-bin.pur-uom, "M", 0, 0, 0, 0,fg-bin.std-tot-cost, output v-costM).
   END.
   ELSE if itemfg.prod-uom eq "M" THEN v-costM = fg-bin.std-tot-cost.
   ELSE run sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,fg-bin.std-tot-cost, output v-costM).
   IF v-sellValue = ? THEN v-sellValue = 0.
   IF v-costM = ? THEN v-costM = 0.

   BUFFER bfg-bin:FIND-BY-ROWID(ROWID(fg-bin), NO-LOCK) .
   BUFFER bitemfg:FIND-BY-ROWID(ROWID(itemfg), NO-LOCK) .

   DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       IF INDEX(cTmpField,".") > 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 hField = IF cTmpField BEGINS "i-name" OR cTmpField BEGINS "part-no" THEN BUFFER bitemfg:BUFFER-FIELD(cTmpField)
                          ELSE BUFFER bfg-bin:BUFFER-FIELD(cTmpField).
                 IF hField <> ? THEN DO:                 
                     cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                     /*IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = cTmpField + IF cTmpField <> "" THEN "-" + string(fg-bin.job-no2,"99") ELSE "".                  
                      */
                     /*IF ENTRY(i,cSelectedList) = "Tag" THEN cTmpField = SUBSTRING(cTmpField,16,8).*/

                     cDisplay = cDisplay + 
                               IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) = "C" THEN
                                 (cTmpField + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)))
                               ELSE IF LENGTH(cTmpField) <  int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) THEN
                                 (FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(cTmpField)) + cTmpField) + " "
                               ELSE cTmpField.
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
                /*"lv-job-no,lv-date,li-palls,v-writein,v-counted-date,v-costM,v-sellValue"  */
                 WHEN "lv-job-no" THEN cVarValue = string(lv-job-no).
                 WHEN "lv-date" THEN cVarValue = IF lv-date <> ? THEN string(lv-date,"99/99/9999") ELSE "".
                 WHEN "li-palls" THEN cVarValue = STRING(li-palls,"->>,>>9").
                 WHEN "v-writein" THEN cVarValue = STRING(v-writein).
                 WHEN "v-tag" THEN cVarValue = STRING(v-tag,"x(20)").
                 WHEN "v-cust-no" THEN cVarValue = STRING(v-cust-no).
                 WHEN "v-counted-date" THEN cVarValue = IF v-counted-date <> ? THEN STRING(v-counted-date) ELSE "".
                 WHEN "v-costM" THEN cVarValue = STRING(v-CostM,">>>,>>9.99<<").
                 WHEN "v-sellValue" THEN cVarValue = STRING(v-sellValue,">>>,>>9.99").
                 WHEN "v-sellUom" THEN cVarValue = STRING(cSellUom).
            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
       END.
   END.

   PUT UNFORMATTED cDisplay SKIP.
   IF tb_dblspc EQ YES THEN PUT SKIP(1).            /*Task# 01161410*/
   IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
   END.

/*=====
  IF tb_prt-pallet THEN DO:
     DISPLAY 
        fg-bin.i-no    WHEN FIRST-OF(tt-report.key-02)
        itemfg.i-name  WHEN FIRST-OF(tt-report.key-02)
        itemfg.cust-no WHEN FIRST-OF(tt-report.key-03)
         v-itemfg      WHEN fg-bin.cust-no NE "" @ itemfg.cust-no
        lv-job-no
        fg-bin.loc
        fg-bin.loc-bin
        fg-bin.tag
        SUBSTR(fg-bin.tag,16,8) WHEN SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no @ fg-bin.tag
        lv-date
        fg-bin.qty     WHEN v-prnt-onh
        li-palls       WHEN v-prnt-onh
        v-writein
     WITH FRAME itemx.            
     DOWN WITH FRAME itemx.

     DISPLAY 
        itemfg.part-dscr1 WHEN FIRST-OF(tt-report.key-02) @ itemfg.i-name
     WITH FRAME itemx.           
     DOWN WITH FRAME itemx.

     IF tb_excel THEN 
        PUT STREAM excel UNFORMATTED
            '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(fg-bin.i-no, '"', "") ELSE "") '",'
            '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.i-name, '"', "") ELSE "")  '",'
            '"' (IF fg-bin.cust-no NE "" THEN v-itemfg ELSE IF FIRST-OF(tt-report.key-03) THEN itemfg.cust-no ELSE "")  '",'
            '"' fg-bin.loc       '",'
            '"' fg-bin.loc-bin   '",'
            '"' (IF SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no THEN SUBSTR(fg-bin.tag,16,8) ELSE fg-bin.tag) '",'
            '"' lv-job-no '",'
            '"' lv-date   '",'
            '"' (IF v-prnt-onh THEN STRING(fg-bin.qty) ELSE "") '",'
            '"' (IF v-prnt-onh THEN STRING(li-palls) ELSE "")  '",'
            '"' v-writein '",'
            SKIP
            '"' "" '",'
            '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.part-dscr1, '"', "") ELSE "") '",'
            SKIP.
  END.
  ELSE DO:
      DISPLAY 
         fg-bin.i-no    WHEN FIRST-OF(tt-report.key-02)
         itemfg.i-name  WHEN FIRST-OF(tt-report.key-02)
         itemfg.cust-no WHEN FIRST-OF(tt-report.key-03)
         fg-bin.cust-no WHEN fg-bin.cust-no NE "" @ itemfg.cust-no
         lv-job-no
         fg-bin.loc
         fg-bin.loc-bin
         fg-bin.tag
         SUBSTR(fg-bin.tag,16,8) WHEN SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no @ fg-bin.tag
         lv-date
         fg-bin.qty     WHEN v-prnt-onh
         /* li-palls       WHEN v-prnt-onh*/
         v-writein
      WITH FRAME itemx2.            
      DOWN WITH FRAME itemx2.

      DISPLAY itemfg.part-dscr1 WHEN FIRST-OF(tt-report.key-02) @ itemfg.i-name
         WITH FRAME itemx2.           
      DOWN WITH FRAME itemx2.

      IF tb_excel THEN 
        PUT STREAM excel UNFORMATTED
            '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(fg-bin.i-no, '"', "") ELSE "") '",'
            '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.i-name, '"', "") ELSE "")  '",'
            '"' (IF fg-bin.cust-no NE "" THEN fg-bin.cust-no ELSE IF FIRST-OF(tt-report.key-03) THEN itemfg.cust-no ELSE "")  '",'
            '"' fg-bin.loc       '",'
            '"' fg-bin.loc-bin   '",'
            '"' (IF SUBSTR(fg-bin.tag,1,15) EQ fg-bin.i-no THEN SUBSTR(fg-bin.tag,16,8) ELSE fg-bin.tag) '",'
            '"' lv-job-no '",'
            '"' lv-date   '",'
            '"' (IF v-prnt-onh THEN STRING(fg-bin.qty) ELSE "") '",'
            '"' v-writein '",'
            SKIP
            '"' "" '",'
            '"' (IF FIRST-OF(tt-report.key-02) THEN REPLACE(itemfg.part-dscr1, '"', "") ELSE "") '",'
            SKIP.
   END.
   ===*/
   IF LAST-OF(tt-report.key-02) THEN PUT SKIP(1).
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

