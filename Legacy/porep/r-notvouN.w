&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: porep\r-notvou.w

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
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
def var v-tot-qty   as   dec            no-undo.
def var v-tot-amt   as   dec            no-undo.
def var v-grand-tot-qty   as   dec      no-undo.
def var v-grand-tot-amt   as   dec      NO-UNDO.
def var v-date      as   date           no-undo.
DEF VAR v-vend-no   AS   CHAR           NO-UNDO.
def var v-procat    like item.procat    no-undo.
def var v-qty-r     as   dec            no-undo.
def var v-amt-r     as   dec            no-undo.
def var v-qty-i     as   dec            no-undo.
def var v-amt-i     as   dec            no-undo.
def var v-cost      like po-ordl.cost   no-undo.

DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR str-line AS cha FORM "x(300)" NO-UNDO.

DEF STREAM excel.
DEF TEMP-TABLE temp-po-rec NO-UNDO
    FIELD po-no AS INT
    FIELD vend-no AS CHAR FORMAT "X(6)"
    FIELD gl-acct LIKE po-ordl.actnum
    FIELD date-rec AS DATE
    FIELD item-no LIKE po-ordl.i-no
    FIELD descr   AS CHAR FORMAT "X(15)"
    FIELD prod-cat AS CHAR
    FIELD qty-to-inv AS DEC format "->>,>>>,>>9.9"
    FIELD whse AS CHAR
    FIELD cost-each AS DEC format ">>>,>>9.99<<<<"
    FIELD amt-to-inv AS DEC format "->>,>>>,>>9.99"

    INDEX temp-vend-no vend-no ASC po-no ASC
    INDEX temp-gl-acct gl-acct ASC po-no ASC.

DEF TEMP-TABLE tt-neg-po-line NO-UNDO
    FIELD po-no AS INT
    FIELD i-no AS CHAR
    FIELD item-type AS LOG
    FIELD qty LIKE fg-rdtlh.qty
    FIELD rcp-date AS DATE
    FIELD amt AS DEC DECIMALS 2
    INDEX po-no po-no i-no.

form temp-po-rec.vend-no    COLUMN-LABEL "Vendor"
                            FORMAT "X(6)"
     temp-po-rec.gl-acct    column-label "G/L Account"
                            FORMAT "X(25)"
     temp-po-rec.po-no      column-label "P.O.!Number"
                            FORMAT ">>>>>9"
     temp-po-rec.date-rec   column-label "Date!Received" FORMAT "99/99/99"
     temp-po-rec.item-no    column-label "Item!Number" FORMAT "X(15)"
     temp-po-rec.descr      column-label "Description"       
                            format "x(15)" 
     temp-po-rec.prod-cat   column-label "Prod!Cat" FORMAT "X(5)"
     temp-po-rec.qty-to-inv column-label "Quantity!To Invoice"
                            format "->>,>>>,>>9.9"
     temp-po-rec.whse       column-label "Whse" FORMAT "X(5)"
     temp-po-rec.cost-each  column-label "Cost Each"
                            format ">>>,>>9.99<<<<"
     temp-po-rec.amt-to-inv column-label "Amt To!Invoice"
                            format "->>,>>>,>>9.99"
    with frame detail down no-box STREAM-IO width 137.


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "Vendor,G/L Account,PO#,Date Rec,Item Number,Description,Cat," +
                           "Inv Qty,Whse,Cost Each,Invoice Amt" 

       cFieldListToSelect = "vend,act,po,date,item,desc,cat," +
                            "inv-qty,whse,cost,inv-amt"
       cFieldLength = "6,25,6,8,15,25,6," + "13,5,10,14"
       cFieldType = "c,c,i,c,c,c,c," + "i,c,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Vendor,G/L Account,PO#,Date Rec,Item Number,Description,Cat," +
                           "Inv Qty,Whse,Cost Each,Invoice Amt" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date end_date ~
begin_po-no end_po-no begin_po-i-no end_po-i-no begin_rdate end_rdate ~
begin_vend end_vend scr-neg-rec tb_in-po rd_sort sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_po-no end_po-no ~
begin_po-i-no end_po-i-no begin_rdate end_rdate begin_vend end_vend ~
scr-neg-rec tb_in-po rd_sort sl_avail sl_selected rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel ~
fi_file 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning PO Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_po-i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_rdate AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending PO Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_po-i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 999999 
     LABEL "Ending PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rdate AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-notvou.csv" 
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
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Vendor", "Vendor",
"GL Code", "GL Code"
     SIZE 26 BY .71 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 8.43.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.29.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE scr-neg-rec AS LOGICAL INITIAL no 
     LABEL "Show Negative Receipts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_in-po AS LOGICAL INITIAL no 
     LABEL "Include Closed POs?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_date AT ROW 2.24 COL 27 COLON-ALIGNED
     end_date AT ROW 2.24 COL 68 COLON-ALIGNED
     begin_po-no AT ROW 3.19 COL 27 COLON-ALIGNED HELP
          "Enter Beginning PO Number"
     end_po-no AT ROW 3.19 COL 68 COLON-ALIGNED HELP
          "Enter Ending PO Number"
     begin_po-i-no AT ROW 4.14 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_po-i-no AT ROW 4.14 COL 68 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_rdate AT ROW 5.1 COL 27 COLON-ALIGNED
     end_rdate AT ROW 5.1 COL 68 COLON-ALIGNED
     begin_vend AT ROW 6.05 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend AT ROW 6.05 COL 68 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     scr-neg-rec AT ROW 7.33 COL 29 WIDGET-ID 2
     tb_in-po AT ROW 8.19 COL 29 WIDGET-ID 58
     rd_sort AT ROW 9.14 COL 29 NO-LABEL
     sl_avail AT ROW 11.05 COL 4.2 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 11.05 COL 40.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 11.05 COL 59.6 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 12.05 COL 40.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 13.05 COL 40.2 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 14.1 COL 40.2 WIDGET-ID 40
     btn_down AT ROW 15.1 COL 40.2 WIDGET-ID 42
     rd-dest AT ROW 17.38 COL 6 NO-LABEL
     lv-ornt AT ROW 17.62 COL 31 NO-LABEL
     lines-per-page AT ROW 17.62 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 19.76 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 20.71 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 21.91 COL 31
     tb_excel AT ROW 23.05 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 23.05 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 23.86 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 25.48 COL 19
     btn-cancel AT ROW 25.48 COL 57
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 10.33 COL 5 WIDGET-ID 38
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 10.33 COL 59.6 WIDGET-ID 44
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 16.43 COL 3
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 9.1 COL 21
     RECT-6 AT ROW 16.81 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 26.29.


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
         TITLE              = "PO Receipts Not Vouchered"
         HEIGHT             = 26.52
         WIDTH              = 95.8
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
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_po-i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rdate:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_po-i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rdate:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* PO Receipts Not Vouchered */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* PO Receipts Not Vouchered */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning PO Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-i-no C-Win
ON LEAVE OF begin_po-i-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-no C-Win
ON LEAVE OF begin_po-no IN FRAME FRAME-A /* Beginning PO# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rdate C-Win
ON LEAVE OF begin_rdate IN FRAME FRAME-A /* Beginning Receipt Date */
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
  IF scr-neg-rec = NO THEN
     RUN run-report.
  ELSE
     run run-report-2. 
   STATUS DEFAULT "Processing Complete".
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_po-no
                            &END_cust=END_po-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending PO Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-i-no C-Win
ON LEAVE OF end_po-i-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-no C-Win
ON LEAVE OF end_po-no IN FRAME FRAME-A /* Ending PO# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rdate C-Win
ON LEAVE OF end_rdate IN FRAME FRAME-A /* Ending Receipt Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
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

  begin_rdate = date(1,1,year(today)).
  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_date.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-data-proc C-Win 
PROCEDURE display-data-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-last-of AS LOG NO-UNDO.

   /*display temp-po-rec.vend-no
           temp-po-rec.gl-acct
           temp-po-rec.po-no
           temp-po-rec.date-rec
           temp-po-rec.item-no 
           temp-po-rec.descr
           temp-po-rec.prod-cat
           temp-po-rec.qty-to-inv
           temp-po-rec.whse
           temp-po-rec.cost-each            
           temp-po-rec.amt-to-inv
       with frame detail.

   down with frame detail.*/

   ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "vend"    THEN cVarValue = string(temp-po-rec.vend-no,"x(6)") .
                         WHEN "act"   THEN cVarValue = string(temp-po-rec.gl-acct,"x(25)").
                         WHEN "po"   THEN cVarValue = STRING(temp-po-rec.po-no,">>>>>9").
                         WHEN "date"  THEN cVarValue = IF temp-po-rec.date-rec NE ? THEN STRING(temp-po-rec.date-rec,"99/99/99") ELSE "" .
                         WHEN "item"   THEN cVarValue = STRING(temp-po-rec.item-no,"x(15)") .
                         WHEN "desc"  THEN cVarValue = STRING(temp-po-rec.descr,"x(25)") .
                         WHEN "cat"   THEN cVarValue = STRING(temp-po-rec.prod-cat,"x(6)") .
                         WHEN "inv-qty"  THEN cVarValue = STRING(temp-po-rec.qty-to-inv,"->>,>>>,>>9.9") .
                         WHEN "whse"  THEN cVarValue = STRING(temp-po-rec.whse,"x(5)") .
                         WHEN "cost"   THEN cVarValue = STRING(temp-po-rec.cost-each,">>>>>>9.99<<<<") .
                         WHEN "inv-amt"  THEN cVarValue = STRING(temp-po-rec.amt-to-inv,"->>,>>>,>>9.99") .

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
      v-tot-qty = v-tot-qty + temp-po-rec.qty-to-inv
      v-tot-amt = v-tot-amt + temp-po-rec.amt-to-inv.


   if ip-last-of and v-tot-amt ne 0 then do:
     /* underline temp-po-rec.qty-to-inv temp-po-rec.amt-to-inv with frame detail.
      display v-tot-qty @ temp-po-rec.qty-to-inv v-tot-amt @ temp-po-rec.amt-to-inv with frame detail.
      down 2 with frame detail.*/
       PUT str-line SKIP.
       ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "vend"    THEN cVarValue = "" .
                         WHEN "act"   THEN cVarValue = "".
                         WHEN "po"   THEN cVarValue = "".
                         WHEN "date"  THEN cVarValue = "" .
                         WHEN "item"   THEN cVarValue = "" .
                         WHEN "desc"  THEN cVarValue = "" .
                         WHEN "cat"   THEN cVarValue = "" .
                         WHEN "inv-qty"  THEN cVarValue = STRING(v-tot-qty,"->>,>>>,>>9.9") .
                         WHEN "whse"  THEN cVarValue = "" .
                         WHEN "cost"   THEN cVarValue = "" .
                         WHEN "inv-amt"  THEN cVarValue = STRING(v-tot-amt,"->>,>>>,>>9.99") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED "           Totals:" + SUBSTRING(cDisplay,19,300) SKIP(1).
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  ' Totals ,'
                       substring(cExcelDisplay,4,300) SKIP(1).
             END.

      assign
       v-grand-tot-qty = v-grand-tot-qty + v-tot-qty
       v-grand-tot-amt = v-grand-tot-amt + v-tot-amt
       v-tot-qty = 0
       v-tot-amt = 0.
    end.
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
  DISPLAY begin_date end_date begin_po-no end_po-no begin_po-i-no end_po-i-no 
          begin_rdate end_rdate begin_vend end_vend scr-neg-rec tb_in-po rd_sort 
          sl_avail sl_selected rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_date end_date begin_po-no end_po-no begin_po-i-no 
         end_po-i-no begin_rdate end_rdate begin_vend end_vend scr-neg-rec 
         tb_in-po rd_sort sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
         btn_Up btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*{sys/form/r-topw.f}*/

def var fdat    as   date format "99/99/9999" no-undo init 01/01/0001.
def var tdat    as   date format "99/99/9999" no-undo init 12/31/9999.
def var fitm    like item.i-no     no-undo.
def var titm    like fitm          no-undo init "zzzzzzzzzzzzzzzz".
def var fpo     like po-ordl.po-no no-undo init 1.
def var tpo     like fpo           no-undo init 999999.
def var frdat   as   date format "99/99/9999" no-undo init 01/01/0001.
def var trdat   like frdat no-undo init 12/31/9999.
DEF VAR fvend   AS CHAR NO-UNDO.
DEF VAR tvend   AS CHAR NO-UNDO.

DEF VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.

{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.


ASSIGN v-grand-tot-qty = 0
       v-grand-tot-amt = 0
       v-tot-qty = 0
       v-tot-amt = 0.

EMPTY TEMP-TABLE temp-po-rec.

{sa/sa-sls01.i}

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 fdat    = begin_date
 tdat    = end_date
 fpo     = begin_po-no
 tpo     = end_po-no
 fitm    = begin_po-i-no
 titm    = end_po-i-no
 frdat   = begin_rdate
 trdat   = end_rdate
 fvend   = begin_vend
 tvend   = END_vend. 

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

        IF LOOKUP(ttRptSelected.TextList, "Inv Qty,Invoice Amt") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  /*excelheader = "Vendor,G/L Account,P.O. Number,Date Received,Item Number,"
              + "Description,Prod Cat,Quantity To Invoice,Whse,Cost Each,"
              + "Amt To Invoice".*/
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

display "" with frame r-top.

  for each po-ordl
      where po-ordl.company   eq cocode
        AND (po-ordl.opened    EQ YES OR ( tb_in-po AND NOT po-ordl.opened) )
        and po-ordl.i-no      ge fitm
        and po-ordl.i-no      le titm
        and po-ordl.po-no     ge fpo
        and po-ordl.po-no     le tpo
        and po-ordl.t-rec-qty gt 0
      USE-INDEX opened no-lock,

      first po-ord
      where po-ord.company eq cocode
        and po-ord.po-no   eq po-ordl.po-no
        and po-ord.po-date ge fdat
        and po-ord.po-date le tdat
        AND po-ord.vend-no GE fvend
        AND po-ord.vend-no LE tvend
      no-lock:
      {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}
    assign
     v-date  = 01/01/0001
     v-qty-r = 0
     v-amt-r = 0
     v-qty-i = 0
     v-amt-i = 0
     v-vend-no = po-ord.vend-no.

    if po-ordl.item-type then do:
      find first item
          where item.company eq cocode
            and item.i-no    eq po-ordl.i-no
          no-lock no-error.
      v-procat = if avail item then item.procat else "".

      for each rm-rcpth FIELDS(trans-date r-no)
          where rm-rcpth.company    eq cocode
            and rm-rcpth.i-no       eq po-ordl.i-no
            and rm-rcpth.po-no      eq string(po-ordl.po-no)
            and rm-rcpth.job-no     eq po-ordl.job-no
            and rm-rcpth.job-no2    eq po-ordl.job-no2
            and rm-rcpth.trans-date ge frdat
            and rm-rcpth.trans-date le trdat
            and rm-rcpth.rita-code  eq "R"
          use-index item-po no-lock,

          first rm-rdtlh
          where rm-rdtlh.r-no   eq rm-rcpth.r-no
            and rm-rdtlh.s-num  eq po-ordl.s-num
          no-lock

          by rm-rcpth.trans-date desc:

        v-date = rm-rcpth.trans-date.

        run sys/inc/po-recqa2.p (recid(po-ordl), frdat, trdat, output v-qty-r, output v-amt-r).
        leave.
      end.
    end.

    else do:
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq po-ordl.i-no
          no-lock no-error.
      v-procat = if avail itemfg then itemfg.procat else "".

      for each fg-rcpth FIELDS(trans-date)
          where fg-rcpth.company    eq cocode
            and fg-rcpth.i-no       eq po-ordl.i-no
            and fg-rcpth.po-no      eq string(po-ordl.po-no)
            and fg-rcpth.rita-code  eq "R"
            and fg-rcpth.trans-date ge frdat
            and fg-rcpth.trans-date le trdat
          use-index item-po no-lock:

        v-date = fg-rcpth.trans-date.

        run sys/inc/po-recqa2.p (recid(po-ordl), frdat, trdat, output v-qty-r, output v-amt-r).

        leave.
      end.
    end.

    if v-amt-r NE 0 then do:
      run sys/inc/po-invqa.p (recid(po-ordl), output v-qty-i, output v-amt-i).

      IF v-qty-r - v-qty-i GT 0 THEN
      DO:
         IF (v-amt-r GT v-amt-i AND v-amt-r GT 0) OR
            (v-amt-r LT v-amt-i AND v-amt-r LT 0) THEN DO:

           v-cost = (((IF v-amt-r LT 0 THEN -1 ELSE 1) * v-amt-r) +
                     ((IF v-amt-i LT 0 THEN -1 ELSE 1) * v-amt-i)) /
                    (((IF v-qty-r LT 0 THEN -1 ELSE 1) * v-qty-r) +
                     ((IF v-qty-i LT 0 THEN -1 ELSE 1) * v-qty-i)).

       IF po-ordl.pr-qty-uom NE "EA" THEN
                 RUN sys/ref/convcuom.p( po-ordl.pr-qty-uom, "EA", 0, 0, 0, 0,
                               v-cost, OUTPUT v-cost).

           CREATE temp-po-rec.
           ASSIGN
              temp-po-rec.vend-no = v-vend-no
              temp-po-rec.po-no = po-ordl.po-no
              temp-po-rec.gl-acct = po-ordl.actnum
              temp-po-rec.date-rec = v-date
              temp-po-rec.item-no = po-ordl.i-no
              temp-po-rec.descr = po-ordl.i-name  
              temp-po-rec.prod-cat = v-procat
              temp-po-rec.qty-to-inv = v-qty-r - v-qty-i
              temp-po-rec.whse = po-ord.loc
              temp-po-rec.cost-each = v-cost
              temp-po-rec.amt-to-inv = v-amt-r - v-amt-i.
           RELEASE temp-po-rec.
         end.
      END.
    end.
  end.  /* For each po-ordl */

IF rd_sort = "Vendor" THEN
   FOR EACH temp-po-rec
       USE-INDEX temp-vend-no
       BREAK BY temp-po-rec.vend-no
             BY temp-po-rec.po-no:
       {custom/statusMsg.i " 'Processing PO#  '  + string(temp-po-rec.po-no) "}
       RUN display-data-proc(INPUT LAST-OF(temp-po-rec.vend-no)).
   END.

ELSE
   FOR EACH temp-po-rec
       USE-INDEX temp-gl-acct
       BREAK BY temp-po-rec.gl-acct
             BY temp-po-rec.po-no:
             {custom/statusMsg.i " 'Processing PO#  '  + string(temp-po-rec.po-no) "}
       RUN display-data-proc(INPUT LAST-OF(temp-po-rec.gl-acct)).
   END.

/*underline temp-po-rec.qty-to-inv temp-po-rec.amt-to-inv with frame detail.
display "Grand Totals" @ temp-po-rec.gl-acct
        v-grand-tot-qty @ temp-po-rec.qty-to-inv
        v-grand-tot-amt @ temp-po-rec.amt-to-inv with frame detail.
down 2 with frame detail.*/
   PUT str-line SKIP.
   ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "vend"    THEN cVarValue = "" .
                         WHEN "act"   THEN cVarValue = "".
                         WHEN "po"   THEN cVarValue = "".
                         WHEN "date"  THEN cVarValue = "" .
                         WHEN "item"   THEN cVarValue = "" .
                         WHEN "desc"  THEN cVarValue = "" .
                         WHEN "cat"   THEN cVarValue = "" .
                         WHEN "inv-qty"  THEN cVarValue = STRING(v-grand-tot-qty,"->>,>>>,>>9.9") .
                         WHEN "whse"  THEN cVarValue = "" .
                         WHEN "cost"   THEN cVarValue = "" .
                         WHEN "inv-amt"  THEN cVarValue = STRING(v-grand-tot-amt,"->>,>>>,>>9.99") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED "     Grand Totals:" + SUBSTRING(cDisplay,19,300) SKIP(1).
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  ' Grand Totals ,'
                       substring(cExcelDisplay,4,300) SKIP.
             END.


  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
/*END.*/

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-2 C-Win 
PROCEDURE run-report-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*{sys/form/r-topw.f}*/

def var fdat    as   date format "99/99/9999" no-undo init 01/01/0001.
def var tdat    as   date format "99/99/9999" no-undo init 12/31/9999.
def var fitm    like item.i-no     no-undo.
def var titm    like fitm          no-undo init "zzzzzzzzzzzzzzzz".
def var fpo     like po-ordl.po-no no-undo init 1.
def var tpo     like fpo           no-undo init 999999.
def var frdat   as   date format "99/99/9999" no-undo init 01/01/0001.
def var trdat   like frdat no-undo init 12/31/9999.
DEF VAR fvend   AS CHAR NO-UNDO.
DEF VAR tvend   AS CHAR NO-UNDO.
DEF VAR ll-neg-inv-found AS LOG NO-UNDO.

DEF VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.

{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

ASSIGN v-grand-tot-qty = 0
       v-grand-tot-amt = 0
       v-tot-qty = 0
       v-tot-amt = 0.

EMPTY TEMP-TABLE temp-po-rec.
EMPTY TEMP-TABLE tt-neg-po-line.

{sa/sa-sls01.i}

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 fdat    = begin_date
 tdat    = end_date
 fpo     = begin_po-no
 tpo     = end_po-no
 fitm    = begin_po-i-no
 titm    = end_po-i-no
 frdat   = begin_rdate
 trdat   = end_rdate
 fvend   = begin_vend
 tvend   = END_vend. 

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

        IF LOOKUP(ttRptSelected.TextList, "Inv Qty,Invoice Amt") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   /*excelheader = "Vendor,G/L Account,P.O. Number,Date Received,Item Number,"
               + "Description,Prod Cat,Quantity To Invoice,Whse,Cost Each,"
               + "Amt To Invoice".*/
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

display "" with frame r-top.

  for each po-ordl
      where po-ordl.company   eq cocode
        AND (po-ordl.opened    EQ YES OR ( tb_in-po AND NOT po-ordl.opened) )
        and po-ordl.i-no      ge fitm
        and po-ordl.i-no      le titm
        and po-ordl.po-no     ge fpo
        and po-ordl.po-no     le tpo
      USE-INDEX opened no-lock,

      first po-ord
      where po-ord.company eq cocode
        and po-ord.po-no   eq po-ordl.po-no
        and po-ord.po-date ge fdat
        and po-ord.po-date le tdat
        AND po-ord.vend-no GE fvend
        AND po-ord.vend-no LE tvend
      no-lock:
      {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}
      IF po-ordl.item-type AND
         NOT CAN-FIND(FIRST rm-rcpth where
              rm-rcpth.company    eq cocode
              and rm-rcpth.i-no       eq po-ordl.i-no
              and rm-rcpth.po-no      eq string(po-ordl.po-no)
              and rm-rcpth.job-no     eq po-ordl.job-no
              and rm-rcpth.job-no2    eq po-ordl.job-no2
              and rm-rcpth.trans-date ge frdat
              and rm-rcpth.trans-date le trdat
              and rm-rcpth.rita-code  eq "R") THEN
          NEXT.
      ELSE IF NOT po-ordl.item-type AND
          NOT CAN-FIND(FIRST fg-rcpth WHERE
              fg-rcpth.company    eq cocode
              and fg-rcpth.i-no       eq po-ordl.i-no
              and fg-rcpth.po-no      eq string(po-ordl.po-no)
              and fg-rcpth.rita-code  eq "R"
              and fg-rcpth.trans-date ge frdat
              and fg-rcpth.trans-date le trdat) THEN
          NEXT.

      assign
       v-date  = 01/01/0001
       v-qty-r = 0
       v-amt-r = 0
       v-qty-i = 0
       v-amt-i = 0
       v-vend-no = po-ord.vend-no.

      if po-ordl.item-type then do:
         find first item
             where item.company eq cocode
               and item.i-no    eq po-ordl.i-no
             no-lock no-error.
         v-procat = if avail item then item.procat else "".

         for each rm-rcpth FIELDS(trans-date r-no)
             where rm-rcpth.company    eq cocode
               and rm-rcpth.i-no       eq po-ordl.i-no
               and rm-rcpth.po-no      eq string(po-ordl.po-no)
               and rm-rcpth.job-no     eq po-ordl.job-no
               and rm-rcpth.job-no2    eq po-ordl.job-no2
               and rm-rcpth.trans-date ge frdat
               and rm-rcpth.trans-date le trdat
               and rm-rcpth.rita-code  eq "R"
             use-index item-po no-lock,

             first rm-rdtlh
             where rm-rdtlh.r-no   eq rm-rcpth.r-no
               and rm-rdtlh.s-num  eq po-ordl.s-num
             no-lock

             by rm-rcpth.trans-date desc:

           v-date = rm-rcpth.trans-date.

           run sys/inc/po-recqa2.p (recid(po-ordl), frdat, trdat, output v-qty-r, output v-amt-r).
           leave.
         end.

         for each rm-rcpth FIELDS(trans-date r-no)
             where rm-rcpth.company    eq cocode
               and rm-rcpth.i-no       eq po-ordl.i-no
               and rm-rcpth.po-no      eq string(po-ordl.po-no)
               and rm-rcpth.job-no     eq po-ordl.job-no
               and rm-rcpth.job-no2    eq po-ordl.job-no2
               and rm-rcpth.trans-date ge frdat
               and rm-rcpth.trans-date le trdat
               and rm-rcpth.rita-code  eq "R"
             use-index item-po no-lock,

             first rm-rdtlh FIELDS(qty trans-date cost frt-cost)
             where rm-rdtlh.r-no   eq rm-rcpth.r-no
               and rm-rdtlh.s-num  eq po-ordl.s-num
               AND rm-rdtlh.qty LT 0
             no-lock:

             IF AVAIL ITEM AND ITEM.inv-by-cust THEN
                NEXT.

             /*IF NOT CAN-FIND(FIRST tt-neg-po-line WHERE
                tt-neg-po-line.po-no EQ po-ordl.po-no AND
                tt-neg-po-line.i-no EQ po-ordl.i-no and
                tt-neg-po-line.item-type EQ po-ordl.item-type AND
                tt-neg-po-line.qty EQ rm-rdtlh.qty) THEN
                DO:*/
                   CREATE tt-neg-po-line.
                   ASSIGN tt-neg-po-line.po-no = po-ordl.po-no
                          tt-neg-po-line.i-no = po-ordl.i-no
                          tt-neg-po-line.item-type = po-ordl.item-type
                          tt-neg-po-line.qty = rm-rdtlh.qty
                          tt-neg-po-line.rcp-date = rm-rcpth.trans-date
                          tt-neg-po-line.amt = (rm-rdtlh.qty * rm-rdtlh.cost) + rm-rdtlh.frt-cost.
                   RELEASE tt-neg-po-line.
                /*END.*/
         END.
      end.

      else do:
        find first itemfg
            where itemfg.company eq cocode
              and itemfg.i-no    eq po-ordl.i-no
            no-lock no-error.
        v-procat = if avail itemfg then itemfg.procat else "".

        for each fg-rcpth FIELDS(trans-date)
            where fg-rcpth.company    eq cocode
              and fg-rcpth.i-no       eq po-ordl.i-no
              and fg-rcpth.po-no      eq string(po-ordl.po-no)
              and fg-rcpth.rita-code  eq "R"
              and fg-rcpth.trans-date ge frdat
              and fg-rcpth.trans-date le trdat
            use-index item-po no-lock:

          v-date = fg-rcpth.trans-date.

          run sys/inc/po-recqa2.p (recid(po-ordl), frdat, trdat, output v-qty-r, output v-amt-r).

          leave.
        end.

        for each fg-rcpth FIELDS(trans-date rita-code r-no)
            where fg-rcpth.company    eq cocode
              and fg-rcpth.i-no       eq po-ordl.i-no
              and fg-rcpth.po-no      eq string(po-ordl.po-no)
              and fg-rcpth.rita-code  eq "R"
              and fg-rcpth.trans-date ge frdat
              and fg-rcpth.trans-date le trdat
            use-index item-po NO-LOCK,
            FIRST fg-rdtlh FIELDS(qty trans-date cost) WHERE
                  fg-rdtlh.r-no EQ fg-rcpth.r-no AND
                  fg-rdtlh.rita-code EQ fg-rcpth.rita-code AND
                  fg-rdtlh.qty LT 0
                  NO-LOCK:

/*                IF NOT CAN-FIND(FIRST tt-neg-po-line WHERE             */
/*                   tt-neg-po-line.po-no EQ po-ordl.po-no AND           */
/*                   tt-neg-po-line.i-no EQ po-ordl.i-no and             */
/*                   tt-neg-po-line.item-type EQ po-ordl.item-type) THEN */
/*                   DO:                                                 */
                     CREATE tt-neg-po-line.
                     ASSIGN tt-neg-po-line.po-no = po-ordl.po-no
                            tt-neg-po-line.i-no = po-ordl.i-no
                            tt-neg-po-line.item-type = po-ordl.item-type
                            tt-neg-po-line.qty = fg-rdtlh.qty
                            tt-neg-po-line.rcp-date = fg-rdtlh.trans-date
                            tt-neg-po-line.amt = fg-rdtlh.qty / 1000 * fg-rdtlh.cost.
                     RELEASE tt-neg-po-line.
/*                   END. */
        end.
      end.

      if v-amt-r NE 0 then do:
         run sys/inc/po-invqa.p (recid(po-ordl), output v-qty-i, output v-amt-i).

         IF v-qty-r - v-qty-i GT 0 THEN
         DO:
            IF (v-amt-r GT v-amt-i AND v-amt-r GT 0) OR
               (v-amt-r LT v-amt-i AND v-amt-r LT 0) THEN DO:

               v-cost = (((IF v-amt-r LT 0 THEN -1 ELSE 1) * v-amt-r) +
                         ((IF v-amt-i LT 0 THEN -1 ELSE 1) * v-amt-i)) /
                        (((IF v-qty-r LT 0 THEN -1 ELSE 1) * v-qty-r) +
                         ((IF v-qty-i LT 0 THEN -1 ELSE 1) * v-qty-i)).

                     IF po-ordl.pr-qty-uom NE "EA" THEN
                         RUN sys/ref/convcuom.p( po-ordl.pr-qty-uom, "EA", 0, 0, 0, 0,
                                                 v-cost, OUTPUT v-cost).

               CREATE temp-po-rec.
               ASSIGN
                  temp-po-rec.vend-no = v-vend-no
                  temp-po-rec.po-no = po-ordl.po-no
                  temp-po-rec.gl-acct = po-ordl.actnum
                  temp-po-rec.date-rec = v-date
                  temp-po-rec.item-no = po-ordl.i-no
                  temp-po-rec.descr = po-ordl.i-name  
                  temp-po-rec.prod-cat = v-procat
                  temp-po-rec.qty-to-inv = v-qty-r - v-qty-i
                  temp-po-rec.whse = po-ord.loc
                  temp-po-rec.cost-each = v-cost
                  temp-po-rec.amt-to-inv = /*v-amt-r -*/ v-amt-i.
               RELEASE temp-po-rec.
            end.
            ELSE
            DO:
               FOR EACH tt-neg-po-line WHERE
                    tt-neg-po-line.po-no = po-ordl.po-no AND
                    tt-neg-po-line.i-no = po-ordl.i-no AND
                    tt-neg-po-line.item-type = po-ordl.item-type
/*                     NO-ERROR.               */
/*                                             */
/*                IF AVAIL tt-neg-po-line THEN */
/*                DO                           */
                   :
                  ll-neg-inv-found = NO.
                  for each reftable
                      {ap/ap-reftbW.i po-ordl.po-no}
                      no-lock,
                     each ap-inv WHERE
                          ap-inv.company eq cocode AND
                          ap-inv.i-no    eq int(reftable.code2) AND
                          ap-inv.vend-no eq po-ord.vend-no AND
                          (ap-inv.po-no  eq po-ordl.po-no or ap-inv.po-no eq 0) AND
                          ap-inv.posted  eq yes
                          use-index i-no no-lock,
                     each ap-invl WHERE
                          ap-invl.i-no       eq ap-inv.i-no AND
                          (ap-invl.po-no     eq po-ordl.po-no or ap-inv.po-no ne 0) AND
                          {ap/invlline.i -1} eq po-ordl.LINE AND
                          ap-invl.qty        EQ tt-neg-po-line.qty
                          use-index i-no no-lock:

                          ll-neg-inv-found = YES.
                          LEAVE.
                  END.

                  IF ll-neg-inv-found = NO THEN
                  DO:
                     CREATE temp-po-rec.
                     ASSIGN
                        temp-po-rec.vend-no = v-vend-no
                        temp-po-rec.po-no = po-ordl.po-no
                        temp-po-rec.gl-acct = po-ordl.actnum
                        temp-po-rec.date-rec = tt-neg-po-line.rcp-date
                        temp-po-rec.item-no = po-ordl.i-no
                        temp-po-rec.descr = po-ordl.i-name  
                        temp-po-rec.prod-cat = v-procat
                        temp-po-rec.qty-to-inv = tt-neg-po-line.qty
                        temp-po-rec.whse = po-ord.loc
                        temp-po-rec.cost-each = tt-neg-po-line.amt / tt-neg-po-line.qty
                        temp-po-rec.amt-to-inv =  tt-neg-po-line.amt.
                     RELEASE temp-po-rec.
                  END.

                  RELEASE tt-neg-po-line.
               END.
            END.
         END.
         ELSE
         DO:
            FIND FIRST tt-neg-po-line WHERE
                 tt-neg-po-line.po-no = po-ordl.po-no AND
                 tt-neg-po-line.i-no = po-ordl.i-no AND
                 tt-neg-po-line.item-type = po-ordl.item-type
                 NO-ERROR.

            IF AVAIL tt-neg-po-line THEN
            DO:
               ll-neg-inv-found = NO.
               for each reftable
                   {ap/ap-reftbW.i po-ordl.po-no}
                   no-lock,
                  each ap-inv WHERE
                       ap-inv.company eq cocode AND
                       ap-inv.i-no    eq int(reftable.code2) AND
                       ap-inv.vend-no eq po-ord.vend-no AND
                       (ap-inv.po-no  eq po-ordl.po-no or ap-inv.po-no eq 0) AND
                       ap-inv.posted  eq yes
                       use-index i-no no-lock,
                  each ap-invl WHERE
                       ap-invl.i-no       eq ap-inv.i-no AND
                       (ap-invl.po-no     eq po-ordl.po-no or ap-inv.po-no ne 0) AND
                       {ap/invlline.i -1} eq po-ordl.LINE AND
                       ap-invl.qty        EQ tt-neg-po-line.qty
                       use-index i-no no-lock:

                       ll-neg-inv-found = YES.
                       LEAVE.
               END.

               IF ll-neg-inv-found = NO THEN
               DO:
                  CREATE temp-po-rec.
                  ASSIGN
                     temp-po-rec.vend-no = v-vend-no
                     temp-po-rec.po-no = po-ordl.po-no
                     temp-po-rec.gl-acct = po-ordl.actnum
                     temp-po-rec.date-rec = tt-neg-po-line.rcp-date
                     temp-po-rec.item-no = po-ordl.i-no
                     temp-po-rec.descr = po-ordl.i-name  
                     temp-po-rec.prod-cat = v-procat
                     temp-po-rec.qty-to-inv = tt-neg-po-line.qty
                     temp-po-rec.whse = po-ord.loc
                     temp-po-rec.cost-each = tt-neg-po-line.amt / tt-neg-po-line.qty
                     temp-po-rec.amt-to-inv =  tt-neg-po-line.amt.
                  RELEASE temp-po-rec.
               END.

               RELEASE tt-neg-po-line.
            END.
         END.
      end.
      ELSE
      DO:
         FIND FIRST tt-neg-po-line WHERE
              tt-neg-po-line.po-no = po-ordl.po-no AND
              tt-neg-po-line.i-no = po-ordl.i-no AND
              tt-neg-po-line.item-type = po-ordl.item-type
              NO-ERROR.

         IF AVAIL tt-neg-po-line THEN
         DO:
            ll-neg-inv-found = NO.
            for each reftable
                {ap/ap-reftbW.i po-ordl.po-no}
                no-lock,
               each ap-inv WHERE
                    ap-inv.company eq cocode AND
                    ap-inv.i-no    eq int(reftable.code2) AND
                    ap-inv.vend-no eq po-ord.vend-no AND
                    (ap-inv.po-no  eq po-ordl.po-no or ap-inv.po-no eq 0) AND
                    ap-inv.posted  eq yes
                    use-index i-no no-lock,
               each ap-invl WHERE
                    ap-invl.i-no       eq ap-inv.i-no AND
                    (ap-invl.po-no     eq po-ordl.po-no or ap-inv.po-no ne 0) AND
                    {ap/invlline.i -1} eq po-ordl.LINE AND
                    ap-invl.qty        EQ tt-neg-po-line.qty
                    use-index i-no no-lock:

                    ll-neg-inv-found = YES.
                    LEAVE.
            END.

            IF ll-neg-inv-found = NO THEN
            DO:
               CREATE temp-po-rec.
               ASSIGN
                  temp-po-rec.vend-no = v-vend-no
                  temp-po-rec.po-no = po-ordl.po-no
                  temp-po-rec.gl-acct = po-ordl.actnum
                  temp-po-rec.date-rec = tt-neg-po-line.rcp-date
                  temp-po-rec.item-no = po-ordl.i-no
                  temp-po-rec.descr = po-ordl.i-name  
                  temp-po-rec.prod-cat = v-procat
                  temp-po-rec.qty-to-inv = tt-neg-po-line.qty
                  temp-po-rec.whse = po-ord.loc
                  temp-po-rec.cost-each = tt-neg-po-line.amt / tt-neg-po-line.qty
                  temp-po-rec.amt-to-inv =  tt-neg-po-line.amt.
               RELEASE temp-po-rec.
            END.

            RELEASE tt-neg-po-line.
         END.
      END.
  end.  /* For each po-ordl */

IF rd_sort = "Vendor" THEN
   FOR EACH temp-po-rec
       USE-INDEX temp-vend-no
       BREAK BY temp-po-rec.vend-no
             BY temp-po-rec.po-no:
             {custom/statusMsg.i " 'Processing PO#  '  + string(temp-po-rec.po-no) "}
       RUN display-data-proc(INPUT LAST-OF(temp-po-rec.vend-no)).
   END.

ELSE
   FOR EACH temp-po-rec
       USE-INDEX temp-gl-acct
       BREAK BY temp-po-rec.gl-acct
             BY temp-po-rec.po-no:
             {custom/statusMsg.i " 'Processing PO#  '  + string(temp-po-rec.po-no) "}
       RUN display-data-proc(INPUT LAST-OF(temp-po-rec.gl-acct)).
   END.

/*underline temp-po-rec.qty-to-inv temp-po-rec.amt-to-inv with frame detail.
display "Grand Totals" @ temp-po-rec.gl-acct
        v-grand-tot-qty @ temp-po-rec.qty-to-inv
        v-grand-tot-amt @ temp-po-rec.amt-to-inv with frame detail.
down 2 with frame detail.*/
PUT str-line SKIP.
   ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "vend"    THEN cVarValue = "" .
                         WHEN "act"   THEN cVarValue = "".
                         WHEN "po"   THEN cVarValue = "".
                         WHEN "date"  THEN cVarValue = "" .
                         WHEN "item"   THEN cVarValue = "" .
                         WHEN "desc"  THEN cVarValue = "" .
                         WHEN "cat"   THEN cVarValue = "" .
                         WHEN "inv-qty"  THEN cVarValue = STRING(v-grand-tot-qty,"->>,>>>,>>9.9") .
                         WHEN "whse"  THEN cVarValue = "" .
                         WHEN "cost"   THEN cVarValue = "" .
                         WHEN "inv-amt"  THEN cVarValue = STRING(v-grand-tot-amt,"->>,>>>,>>9.99") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED "     Grand Totals:" + SUBSTRING(cDisplay,19,300) SKIP(1).
            IF tb_excel THEN DO:
                PUT STREAM excel UNFORMATTED  ' Grand Totals ,'
                    substring(cExcelDisplay,4,300) SKIP.
                OUTPUT STREAM excel CLOSE.
                IF tb_runExcel THEN
                OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
            END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */
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

