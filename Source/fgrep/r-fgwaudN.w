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
DEFINE VARIABLE list-name   AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir    AS CHARACTER NO-UNDO.

DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

/*{sys/inc/custlistform.i ""IL14"" }*/

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL NO-UNDO.

{fg/rep/fg-waud1.i NEW SHARED}

DEFINE NEW SHARED VARIABLE cDisplay           AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cExcelDisplay      AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE hField             AS HANDLE    NO-UNDO.
DEFINE NEW SHARED VARIABLE cTmpField          AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cVarValue          AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cExcelVarValue     AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cSelectedList      AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cFieldName         AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE str-tit4           AS cha       FORM "x(200)" NO-UNDO.
DEFINE NEW SHARED VARIABLE str-tit5           AS cha       FORM "x(200)" NO-UNDO.
DEFINE NEW SHARED VARIABLE str-line           AS cha       FORM "x(300)" NO-UNDO.

DEFINE            VARIABLE ll-secure          AS LOG       NO-UNDO.
DEFINE            VARIABLE is-xprint-form     AS LOG       NO-UNDO.
DEFINE            VARIABLE ls-fax-file        AS CHARACTER NO-UNDO.
DEFINE            VARIABLE excel-header-var-1 AS CHARACTER NO-UNDO.
DEFINE            VARIABLE excel-header-var-2 AS CHARACTER NO-UNDO.
DEFINE            VARIABLE excel-header-var-3 AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-random 
    FIELD RandomNum AS INTEGER.

DEFINE NEW SHARED VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE NEW SHARED VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lSelected          AS LOG       INIT YES NO-UNDO.
DEFINE            VARIABLE cFileName          AS CHARACTER NO-UNDO.

ASSIGN 
    cTextListToSelect  = "ITEM # ,ITEM NAME,WHSE,BIN,UNITS,COUNT,BIN QTY ITEMS TOTAL,TAG #" 

    cFieldListToSelect = "item,name,whse,bin,unit,count,tot,tag" 

    cFieldLength       = "15,30,5,9,9,8,19,20" 
    cFieldType         = "c,c,c,c,i,i,i,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "ITEM # ,ITEM NAME,WHSE,BIN,UNITS,COUNT,BIN QTY ITEMS TOTAL,TAG #" 
    .

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
begin_cust-no end_cust-no begin_whse end_whse begin_loc-bin end_loc-bin ~
rd_i-code tb_sets tb_cust-whse tb_random sl_avail Btn_Def sl_selected ~
Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV tbAutoClose ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS as-of-date fi_days-old tb_cust-list ~
begin_cust-no end_cust-no begin_whse end_whse begin_loc-bin end_loc-bin ~
lbl_i-code rd_i-code tb_sets tb_cust-whse tb_random sl_avail sl_selected ~
rd-dest fi_file tb_OpenCSV tbAutoClose 

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
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btnCustList 
    LABEL "Preview" 
    SIZE 9.8 BY .81.

DEFINE BUTTON Btn_Add 
    LABEL "&Add >>" 
    SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Def 
    LABEL "&Default" 
    SIZE 16 BY 1.1.

DEFINE BUTTON btn_down 
    LABEL "Move Down" 
    SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Remove 
    LABEL "<< &Remove" 
    SIZE 16 BY 1.1.

DEFINE BUTTON btn_Up 
    LABEL "Move Up" 
    SIZE 16 BY 1.1.

DEFINE VARIABLE as-of-date     AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/01 
    LABEL "As of" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_cat      AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Category" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust-no  AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_i-no     AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc-bin  AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Bin" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_whse     AS CHARACTER FORMAT "X(5)" 
    LABEL "Beginning Warehouse" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cat        AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Category" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_i-no       AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc-bin    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Bin" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_whse       AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
    LABEL "Ending Warehouse" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_days-old    AS INTEGER   FORMAT "->,>>>,>>9":U INITIAL 0 
    LABEL "Only Show QOH that is Older Than" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\ItemAuditByWhse.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_i-code     AS CHARACTER FORMAT "X(256)":U INITIAL "Item Code?" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_msf        AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_po-type    AS CHARACTER FORMAT "X(256)":U INITIAL "If Yes - PO Type?" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_price2     AS CHARACTER FORMAT "X(256)":U INITIAL "From" 
    VIEW-AS FILL-IN 
    SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt        AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 16.2 BY 4.76 NO-UNDO.

DEFINE VARIABLE rd_i-code      AS CHARACTER INITIAL "All" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Stock", "Stock",
    "Custom", "Custom",
    "All", "All"
    SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE rd_msf         AS CHARACTER INITIAL "Qty" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Qty", "Qty",
    "MSF", "MSF"
    SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE rd_po-type     AS CHARACTER INITIAL "Line" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Line", "Line",
    "Header", "Header"
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE rd_price       AS CHARACTER INITIAL "Order" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "FG", "FG",
    "Order", "Order"
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "FG Item#" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Customer#", "Customer#",
    "FG Item#", "FG Item#",
    "Part#", "Part#",
    "Product Category", "Product Category",
    "Whs/Bin", "Whs/Bin"
    SIZE 75 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 5.33.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 10.48.

DEFINE VARIABLE sl_avail       AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected    AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tbAutoClose    AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_actrel      AS LOGICAL   INITIAL NO 
    LABEL "Print Actual Release Qty?" 
    VIEW-AS TOGGLE-BOX
    SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cost        AS LOGICAL   INITIAL NO 
    LABEL "Print Cost?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cost-2      AS LOGICAL   INITIAL NO 
    LABEL "If Yes - DL/MAT Only?" 
    VIEW-AS TOGGLE-BOX
    SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-list   AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_cust-pt     AS LOGICAL   INITIAL NO 
    LABEL "Print Customer Part#?" 
    VIEW-AS TOGGLE-BOX
    SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-whse   AS LOGICAL   INITIAL YES 
    LABEL "Include Customer Owned Warehouse?" 
    VIEW-AS TOGGLE-BOX
    SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-whse-2 AS LOGICAL   INITIAL NO 
    LABEL "Only Customer Owned Warehouse?" 
    VIEW-AS TOGGLE-BOX
    SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV     AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY .81
    BGCOLOR 15 FGCOLOR 15 NO-UNDO.

DEFINE VARIABLE tb_po-num      AS LOGICAL   INITIAL NO 
    LABEL "Print PO#?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_random      AS LOGICAL   INITIAL NO 
    LABEL "Random Item?" 
    VIEW-AS TOGGLE-BOX
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_rct-date    AS LOGICAL   INITIAL NO 
    LABEL "Print Receipt Date?" 
    VIEW-AS TOGGLE-BOX
    SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sell-pr     AS LOGICAL   INITIAL YES 
    LABEL "Print Sell Price?" 
    VIEW-AS TOGGLE-BOX
    SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sets        AS LOGICAL   INITIAL NO 
    LABEL "Exclude Set Parts?" 
    VIEW-AS TOGGLE-BOX
    SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_subt        AS LOGICAL   INITIAL NO 
    LABEL "Print Subtotals?" 
    VIEW-AS TOGGLE-BOX
    SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE tb_zero        AS LOGICAL   INITIAL YES 
    LABEL "Include Zero Balances?" 
    VIEW-AS TOGGLE-BOX
    SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm   AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    as-of-date AT ROW 1.95 COL 27 COLON-ALIGNED
    fi_days-old AT ROW 1.95 COL 80 COLON-ALIGNED
    tb_cust-list AT ROW 3.05 COL 29.4 WIDGET-ID 6
    btnCustList AT ROW 3.1 COL 71.8 WIDGET-ID 8
    begin_cust-no AT ROW 4.1 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust-no AT ROW 4.1 COL 70 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_whse AT ROW 5.05 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Warehouse"
    end_whse AT ROW 5.05 COL 70 COLON-ALIGNED HELP
    "Enter Ending Warehouse Number"
    begin_loc-bin AT ROW 6 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Warehouse"
    end_loc-bin AT ROW 6 COL 70 COLON-ALIGNED HELP
    "Enter Ending Warehouse Number"
    begin_i-no AT ROW 6.95 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Order Number"
    end_i-no AT ROW 6.95 COL 70 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    lbl_i-code AT ROW 7.62 COL 4 COLON-ALIGNED NO-LABELS
    rd_i-code AT ROW 7.62 COL 18 NO-LABELS
    tb_sets AT ROW 7.86 COL 50
    begin_cat AT ROW 7.91 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Category"
    end_cat AT ROW 7.91 COL 70 COLON-ALIGNED HELP
    "Enter Ending Order Number"
    lbl_sort AT ROW 9.33 COL 9 COLON-ALIGNED NO-LABELS
    rd_sort AT ROW 9.33 COL 18 NO-LABELS
    tb_rct-date AT ROW 9.33 COL 56
    tb_cust-whse AT ROW 9.76 COL 50
    lbl_msf AT ROW 10.29 COL 9 COLON-ALIGNED NO-LABELS
    rd_msf AT ROW 10.29 COL 18 NO-LABELS
    tb_random AT ROW 10.71 COL 50 WIDGET-ID 2
    tb_zero AT ROW 12.19 COL 5
    sl_avail AT ROW 12.67 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 12.67 COL 39 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    tb_cust-pt AT ROW 12.67 COL 50
    sl_selected AT ROW 12.67 COL 63.6 NO-LABELS WIDGET-ID 28
    tb_sell-pr AT ROW 13.48 COL 5
    lbl_price2 AT ROW 13.48 COL 22 COLON-ALIGNED NO-LABELS
    rd_price AT ROW 13.48 COL 30 NO-LABELS
    Btn_Add AT ROW 13.67 COL 39 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    tb_subt AT ROW 14.57 COL 5
    Btn_Remove AT ROW 14.67 COL 39 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    tb_po-num AT ROW 15.05 COL 56
    tb_cust-whse-2 AT ROW 15.52 COL 5
    tb_actrel AT ROW 15.52 COL 50
    btn_Up AT ROW 15.71 COL 39 WIDGET-ID 40
    tb_cost AT ROW 16.71 COL 5
    tb_cost-2 AT ROW 16.71 COL 20
    btn_down AT ROW 16.71 COL 39 WIDGET-ID 42
    lbl_po-type AT ROW 17.43 COL 53 COLON-ALIGNED NO-LABELS
    rd_po-type AT ROW 17.43 COL 73 NO-LABELS
    rd-dest AT ROW 18.57 COL 4.8 NO-LABELS
    lv-ornt AT ROW 18.57 COL 44.8 NO-LABELS
    lines-per-page AT ROW 18.57 COL 89.6 COLON-ALIGNED
    lv-font-no AT ROW 18.62 COL 35 COLON-ALIGNED
    lv-font-name AT ROW 19.81 COL 31.4 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 21.33 COL 29.4
    fi_file AT ROW 22.24 COL 27.2 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 22.33 COL 94 RIGHT-ALIGNED
    tbAutoClose AT ROW 23.71 COL 29.2 WIDGET-ID 62
    btn-ok AT ROW 24.81 COL 29
    btn-cancel AT ROW 24.81 COL 55.2
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 11.95 COL 63.4 WIDGET-ID 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 97.8 BY 29.91
    BGCOLOR 15 .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.05 COL 4.6
    BGCOLOR 15 
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 17.95 COL 4.4
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 11.95 COL 3 WIDGET-ID 38
    RECT-6 AT ROW 18.29 COL 3
    RECT-7 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 97.8 BY 29.91
    BGCOLOR 15 .


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
        TITLE              = "Whse Item Audit List by Whse/Item"
        HEIGHT             = 25.52
        WIDTH              = 98
        MAX-HEIGHT         = 33.29
        MAX-WIDTH          = 273.2
        VIRTUAL-HEIGHT     = 33.29
        VIRTUAL-WIDTH      = 273.2
        RESIZE             = YES
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = YES
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
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
/* SETTINGS FOR FILL-IN as-of-date IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    as-of-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_cat IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    begin_cat:HIDDEN IN FRAME FRAME-A       = TRUE
    begin_cat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_i-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    begin_i-no:HIDDEN IN FRAME FRAME-A       = TRUE
    begin_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_loc-bin:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_whse:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR FILL-IN end_cat IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    end_cat:HIDDEN IN FRAME FRAME-A       = TRUE
    end_cat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN end_i-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    end_i-no:HIDDEN IN FRAME FRAME-A       = TRUE
    end_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_loc-bin:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_whse:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN fi_days-old IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    fi_days-old:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_i-code IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_i-code:PRIVATE-DATA IN FRAME FRAME-A = "rd_i-code".

/* SETTINGS FOR FILL-IN lbl_msf IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lbl_msf:HIDDEN IN FRAME FRAME-A       = TRUE
    lbl_msf:PRIVATE-DATA IN FRAME FRAME-A = "rd_msf".

/* SETTINGS FOR FILL-IN lbl_po-type IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lbl_po-type:HIDDEN IN FRAME FRAME-A       = TRUE
    lbl_po-type:PRIVATE-DATA IN FRAME FRAME-A = "rd_po-type".

/* SETTINGS FOR FILL-IN lbl_price2 IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lbl_price2:HIDDEN IN FRAME FRAME-A       = TRUE
    lbl_price2:PRIVATE-DATA IN FRAME FRAME-A = "rd_price".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lbl_sort:HIDDEN IN FRAME FRAME-A       = TRUE
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-font-name:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-font-no:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-ornt:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    rd_i-code:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR RADIO-SET rd_msf IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    rd_msf:HIDDEN IN FRAME FRAME-A       = TRUE
    rd_msf:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR RADIO-SET rd_po-type IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    rd_po-type:HIDDEN IN FRAME FRAME-A       = TRUE
    rd_po-type:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR RADIO-SET rd_price IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    rd_price:HIDDEN IN FRAME FRAME-A       = TRUE
    rd_price:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR RADIO-SET rd_sort IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    rd_sort:HIDDEN IN FRAME FRAME-A       = TRUE
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_actrel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_actrel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_actrel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_cost IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_cost:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_cost:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_cost-2 IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_cost-2:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_cost-2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_cust-pt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_cust-pt:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_cust-pt:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_cust-whse:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_cust-whse-2 IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_cust-whse-2:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_cust-whse-2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_po-num IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_po-num:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_po-num:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_random:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_rct-date IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_rct-date:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_rct-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_sell-pr IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_sell-pr:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_sell-pr:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_sets:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_subt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_subt:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_subt:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_zero IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_zero:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_zero:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Whse Item Audit List by Whse/Item */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Whse Item Audit List by Whse/Item */
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
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cat C-Win
ON LEAVE OF begin_cat IN FRAME FRAME-A /* Beginning Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode, {&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_loc-bin C-Win
ON LEAVE OF begin_loc-bin IN FRAME FRAME-A /* Beginning Bin */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_whse C-Win
ON LEAVE OF begin_whse IN FRAME FRAME-A /* Beginning Warehouse */
    DO:
        ASSIGN {&self-name}.
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
            ASSIGN {&DISPLAYED-OBJECTS}
                rd_i-code      = "All"
                rd_price       = "Order"
                rd_sort        = "FG Item#"
                tb_sell-pr     = NO
                tb_rct-date    = NO
                tb_subt        = NO
                tb_actrel      = NO
                tb_zero        = NO
                tb_po-num      = NO
                tb_cust-whse-2 = NO
                tb_cost        = NO
                /*tb_summ-bin = NO*/.

        END.
  
        IF rd-dest = 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.
  
        RUN GetSelectionList.
        FIND FIRST  ttCustList NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttCustList AND tb_cust-list THEN 
        DO:
            EMPTY TEMP-TABLE ttCustList.
            RUN BuildCustList(INPUT cocode,
                INPUT tb_cust-list AND glCustListActive ,
                INPUT begin_cust-no,
                INPUT end_cust-no).
        END.
        RUN run-report. 

        SESSION:SET-WAIT-STATE("general").

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN 
                DO:
                    IF NOT tb_OpenCSV THEN 
                    DO:        
                        MESSAGE "CSV file have been created." SKIP(1)
                            "~"OK"~" to open CSV file?"
                            VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                            TITLE "" UPDATE lChoice AS LOGICAL.
                 
                        IF lChoice THEN
                        DO:
                            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                        END.
                    END.
                END. /* WHEN 3 THEN DO: */
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &begin_cust=begin_cust-no
                            &END_cust=END_cust-no
                            &fax-subject=c-win:title 
                            &fax-body=c-win:title 
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_cust-no
                             &END_cust=end_cust-no
                             &mail-subject=c-win:title 
                             &mail-body=c-win:title 
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_cust-no
                                  &END_cust=end_cust-no
                                  &mail-subject=c-win:title 
                                  &mail-body=c-win:title 
                                  &mail-file=list-name }

                    END.

                END. 
            WHEN 6 THEN RUN output-to-port.
        END CASE.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
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


&Scoped-define SELF-NAME end_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cat C-Win
ON LEAVE OF end_cat IN FRAME FRAME-A /* Ending Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode, {&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_loc-bin C-Win
ON LEAVE OF end_loc-bin IN FRAME FRAME-A /* Ending Bin */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_whse C-Win
ON LEAVE OF end_whse IN FRAME FRAME-A /* Ending Warehouse */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* Name */
DO:
   DEF VAR ls-filename AS CHARACTER NO-UNDO.
   DEF VAR ll-ok AS LOG NO-UNDO.

   SYSTEM-DIALOG GET-FILE ls-filename 
                 TITLE "Select File to Save "
                 FILTERS "Excel Files    (*.csv)" "*.csv",
                         "All Files    (*.*) " "*.*"
                 INITIAL-DIR "c:\tmp"
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.

    IF ll-ok THEN SELF:SCREEN-VALUE = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Name */
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
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE        = ENTRY(1,char-val)
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
        RUN pChangeDest.
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
        DEF VAR cSelectedList AS CHARACTER NO-UNDO.
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
            IF {&SELF-NAME}:IS-SELECTED(i) THEN 
            DO:
                ASSIGN 
                    ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
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


&Scoped-define SELF-NAME tb_actrel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_actrel C-Win
ON VALUE-CHANGED OF tb_actrel IN FRAME FRAME-A /* Print Actual Release Qty? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cost C-Win
ON VALUE-CHANGED OF tb_cost IN FRAME FRAME-A /* Print Cost? */
    DO:
        ASSIGN {&self-name}.
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
        ASSIGN {&self-name}.
        EMPTY TEMP-TABLE ttCustList.
        RUN SetCustRange(INPUT tb_cust-list).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-pt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-pt C-Win
ON VALUE-CHANGED OF tb_cust-pt IN FRAME FRAME-A /* Print Customer Part#? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_po-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_po-num C-Win
ON VALUE-CHANGED OF tb_po-num IN FRAME FRAME-A /* Print PO#? */
    DO:
        IF {&self-name}:SCREEN-VALUE EQ "yes" THEN tb_sell-pr:SCREEN-VALUE = "yes".
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_random
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_random C-Win
ON VALUE-CHANGED OF tb_random IN FRAME FRAME-A /* Random Item? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rct-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rct-date C-Win
ON VALUE-CHANGED OF tb_rct-date IN FRAME FRAME-A /* Print Receipt Date? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sell-pr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sell-pr C-Win
ON VALUE-CHANGED OF tb_sell-pr IN FRAME FRAME-A /* Print Sell Price? */
    DO:
        IF tb_po-num:SCREEN-VALUE EQ "yes" THEN {&self-name}:SCREEN-VALUE = "yes".
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sets
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sets C-Win
ON VALUE-CHANGED OF tb_sets IN FRAME FRAME-A /* Exclude Set Parts? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_subt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_subt C-Win
ON VALUE-CHANGED OF tb_subt IN FRAME FRAME-A /* Print Subtotals? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_zero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zero C-Win
ON VALUE-CHANGED OF tb_zero IN FRAME FRAME-A /* Include Zero Balances? */
    DO:
        ASSIGN {&self-name}.
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
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.
    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "IL14" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "IL14",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i "AND lv-field-hdl:SENSITIVE"}
        RUN DisplaySelectionList2.
        ASSIGN
            as-of-date              = TODAY
            as-of-date:SCREEN-VALUE = STRING(TODAY).

        APPLY "value-changed" TO tb_po-num.
        APPLY "entry" TO as-of-date.
    END.

    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'IL14',
        INPUT NO,
        OUTPUT glCustListActive).
    {sys/inc/chblankcust.i ""IL14""}

    IF ou-log THEN 
    DO:
        ASSIGN 
            tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
            btnCustList:SENSITIVE IN FRAME {&FRAME-NAME}     = YES
            tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
            tb_cust-list                                     = YES 
            .
        RUN SetCustRange(INPUT tb_cust-list).
    END.
    ELSE
        ASSIGN
            tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
            tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
            btnCustList:SENSITIVE IN FRAME {&FRAME-NAME}     = NO
            .

    IF ou-log AND ou-cust-int = 0 THEN 
    DO:
        ASSIGN 
            tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME}    = YES
            btnCustList:SENSITIVE IN FRAME {&FRAME-NAME}     = NO
            tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
            tb_cust-list                                     = NO
            .
        RUN SetCustRange(tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "YES").
    END.
    RUN pChangeDest.
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

    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.

    IF iplList THEN 
    DO:
        RUN sys/ref/CustList.p (INPUT ipcCompany,
            INPUT 'IL14',
            INPUT YES,
            OUTPUT lActive).
    END.
    ELSE 
    DO:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildTTBin C-Win 
PROCEDURE BuildTTBin :
    /*------------------------------------------------------------------------------
      Purpose:     from fg/rep/tt-fgbin.p
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-date AS   DATE NO-UNDO.
    DEFINE INPUT PARAMETER ip-fjob LIKE fg-bin.job-no NO-UNDO.
    DEFINE INPUT PARAMETER ip-tjob LIKE fg-bin.job-no NO-UNDO.
    DEFINE INPUT PARAMETER ip-floc LIKE fg-bin.loc NO-UNDO.
    DEFINE INPUT PARAMETER ip-tloc LIKE fg-bin.loc NO-UNDO.
    DEFINE INPUT PARAMETER ip-fbin LIKE fg-bin.loc-bin NO-UNDO.
    DEFINE INPUT PARAMETER ip-tbin LIKE fg-bin.loc-bin NO-UNDO.
    DEFINE INPUT PARAMETER ip-zbal AS   LOG NO-UNDO.
    DEFINE INPUT PARAMETER ip-ager AS   INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ip-curr AS   LOG NO-UNDO.
    DEFINE INPUT PARAMETER ip-cust AS   LOG NO-UNDO.

    DEFINE VARIABLE iRandom       AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.
    DEFINE VARIABLE iRecordNum    AS INTEGER NO-UNDO.
    DEFINE VARIABLE isItemofFirst AS LOG     NO-UNDO.
    DEFINE VARIABLE iRandomCount  AS INTEGER NO-UNDO.


    FOR EACH itemfg NO-LOCK WHERE itemfg.company = cocode
        AND (NOT v-sets OR /*itemfg.isaset OR*/
        (v-sets AND NOT CAN-FIND(FIRST fg-set
        WHERE fg-set.company EQ itemfg.company
        AND fg-set.part-no EQ itemfg.i-no))) :

        isItemofFirst = YES.
        FOR EACH fg-bin NO-LOCK
            WHERE fg-bin.company   EQ itemfg.company
            AND fg-bin.i-no      EQ itemfg.i-no
            AND STRING(FILL(" ", iJobLen - LENGTH(TRIM(fg-bin.job-no))) +
            TRIM(fg-bin.job-no) + STRING(fg-bin.job-no2,"999"))
            GE ip-fjob
            AND STRING(FILL(" ", iJobLen - LENGTH(TRIM(fg-bin.job-no))) +
            TRIM(fg-bin.job-no) + STRING(fg-bin.job-no2,"999"))
            LE ip-tjob
            AND fg-bin.loc       GE ip-floc
            AND fg-bin.loc       LE ip-tloc
            AND fg-bin.loc-bin   GE ip-fbin
            AND fg-bin.loc-bin   LE ip-tbin
            AND fg-bin.cust-no GE fcus
            AND fg-bin.cust-no LE tcus
            AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ fg-bin.cust-no
            AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
            AND ((fg-bin.cust-no EQ "" AND fg-bin.loc NE "CUST") OR ip-cust)
            AND (fg-bin.qty      NE 0 OR ip-zbal):

            /*
             IF tb_random /*AND isItemofFirst*/ THEN DO:
                    iCount = iCount + 1.
                    IF iCount <> iRandom THEN NEXT.
                     MESSAGE "random: " icount irandom irecordnum SKIP
                         ITEMfg.i-no
                         VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    iRandom = iRandom + RANDOM(1,20).
                    iRecordNum = iRecordNum + 1.
                    /*MESSAGE iCount iRandom iRecordNum SKIP
                        itemfg.i-no 
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
         
                    IF iRecordNum > 20 THEN LEAVE.
                    isItemofFirst = NO.
             END.
            */

            CREATE tt-fg-bin.
            BUFFER-COPY fg-bin TO tt-fg-bin.
            /*     
             FOR EACH fg-rcpth FIELDS(r-no rita-code po-no) WHERE
               fg-rcpth.company EQ itemfg.company AND
               fg-rcpth.i-no EQ itemfg.i-no AND
               fg-rcpth.job-no EQ fg-bin.job-no AND
               fg-rcpth.job-no2 EQ fg-bin.job-no2 AND
               fg-rcpth.po-no NE "" AND
               fg-rcpth.rita-code EQ "R"
               NO-LOCK,
               FIRST fg-rdtlh fields() WHERE
                     fg-rdtlh.r-no EQ fg-rcpth.r-no AND
                     fg-rdtlh.rita-code EQ fg-rcpth.rita-code AND
                     fg-rdtlh.loc EQ fg-bin.loc AND
                     fg-rdtlh.loc-bin EQ fg-bin.loc-bin AND
                     fg-rdtlh.tag EQ fg-bin.tag AND
                     fg-rdtlh.cust-no EQ fg-bin.cust-no AND
                     fg-rdtlh.bol-no EQ fg-bin.bol-no AND
                     fg-rdtlh.inv-no EQ fg-bin.inv-no
                     NO-LOCK
               BY fg-rcpth.trans-date DESC
               BY fg-rdtlh.trans-time DESC:
         
               tt-fg-bin.po-no = fg-rcpth.po-no.
             END.
            */
            /*tt-fg-bin.first-date = tt-fg-bin.aging-date.*/

            /*
            IF TRIM(tt-fg-bin.tag) EQ "" THEN
             FOR EACH fg-rcpth NO-LOCK
                 WHERE fg-rcpth.company      EQ itemfg.company
                   AND fg-rcpth.i-no         EQ itemfg.i-no
                   AND fg-rcpth.job-no       EQ tt-fg-bin.job-no
                   AND fg-rcpth.job-no2      EQ tt-fg-bin.job-no2
                 USE-INDEX tran,
        
                 EACH fg-rdtlh NO-LOCK
                 WHERE fg-rdtlh.r-no         EQ fg-rcpth.r-no
                   /*AND fg-rdtlh.loc          EQ tt-fg-bin.loc
                   AND fg-rdtlh.loc-bin      EQ tt-fg-bin.loc-bin*/
                   AND fg-rdtlh.tag          EQ tt-fg-bin.tag
                   AND fg-rdtlh.cust-no      EQ tt-fg-bin.cust-no
                   AND fg-rdtlh.rita-code    EQ "R"
                 USE-INDEX rm-rdtl
        
                 BREAK BY fg-rcpth.trans-date
                       BY fg-rdtlh.trans-time
                       BY fg-rcpth.r-no:
        
                 IF FIRST(fg-rcpth.trans-date) THEN
                    tt-fg-bin.first-date = fg-rcpth.trans-date.
        
                 LEAVE.
            END.
            ELSE
             FOR EACH fg-rdtlh
                 WHERE fg-rdtlh.company      EQ tt-fg-bin.company
                   AND fg-rdtlh.tag          EQ tt-fg-bin.tag
                   /*AND fg-rdtlh.loc          EQ tt-fg-bin.loc
                   AND fg-rdtlh.loc-bin      EQ tt-fg-bin.loc-bin*/
                   AND fg-rdtlh.cust-no      EQ tt-fg-bin.cust-no
                 USE-INDEX tag NO-LOCK,
        
                 FIRST fg-rcpth NO-LOCK
                 WHERE fg-rcpth.r-no         EQ fg-rdtlh.r-no
                   AND fg-rcpth.i-no         EQ tt-fg-bin.i-no
                   AND fg-rcpth.job-no       EQ tt-fg-bin.job-no
                   AND fg-rcpth.job-no2      EQ tt-fg-bin.job-no2
                   AND fg-rcpth.rita-code    EQ "R"
                 USE-INDEX r-no
        
                 BREAK BY fg-rcpth.trans-date
                       BY fg-rdtlh.trans-time
                       BY fg-rcpth.r-no:
        
                IF FIRST(fg-rcpth.trans-date) THEN
                   tt-fg-bin.first-date = fg-rcpth.trans-date.
            END.
           */
            IF tt-fg-bin.first-date EQ ? THEN
                tt-fg-bin.first-date = IF fg-bin.rec_key BEGINS "2" THEN
                    DATE(SUBSTR(fg-bin.rec_key,5,4) + SUBSTRING(fg-bin.rec_key,1,4))
                    ELSE DATE(SUBSTR(fg-bin.rec_key,1,8)).
        END.  /* each fg-bin */


    END.

    iCount = 0.
    FOR EACH tt-fg-bin,
        FIRST itemfg WHERE itemfg.company = cocode
        AND itemfg.i-no = tt-fg-bin.i-no NO-LOCK
        BREAK BY tt-fg-bin.i-no:
        IF FIRST-OF(tt-fg-bin.i-no) THEN icount = icount + 1.
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
                                   STRING(tt-itemfg.tag,"x(20)")
            tt-itemfg.cases = TRUNC((tt-fg-bin.qty - tt-fg-bin.partial-count) / tt-fg-bin.case-count,0)
            tt-itemfg.case-count = tt-fg-bin.case-count  
            tt-itemfg.RecordCount = iCount 
            .
    END.

    DEFINE VARIABLE lGotRandom      AS LOG     NO-UNDO.
    DEFINE VARIABLE iRandomRecCount AS INTEGER NO-UNDO.

    iRecordNum = iCount.

    IF tb_random THEN 
    DO:
        EMPTY TEMP-TABLE tt-random.
        ASSIGN 
            lGotRandom   = NO
            iRandomCount = 0.
        DO WHILE NOT lGotRandom:
            iRandom = RANDOM(1,iRecordNum).
            iRandomCount = iRandomCount + 1.
            IF CAN-FIND(FIRST tt-random WHERE tt-random.RandomNum = iRandom)
                AND iRandomCount <= iRecordNum THEN NEXT.


            CREATE tt-random.
            ASSIGN 
                tt-random.RandomNum = iRandom
                iRandomRecCount     = iRandomRecCount + 1.

            IF iRandomRecCount > 20 THEN LEAVE.

        END.
        FOR EACH tt-itemfg:
            IF CAN-FIND(FIRST tt-random WHERE tt-random.RandomNum = tt-itemfg.RecordCount)
                THEN 
            DO:

            END.
            ELSE 
            DO:
                DELETE tt-itemfg.
            END.
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
        INPUT 'IL14').


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
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.

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
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.

    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:

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
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
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
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
  
    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:
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
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
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
    DISPLAY as-of-date fi_days-old tb_cust-list begin_cust-no end_cust-no 
        begin_whse end_whse begin_loc-bin end_loc-bin lbl_i-code rd_i-code 
        tb_sets tb_cust-whse tb_random sl_avail sl_selected rd-dest fi_file 
        tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 tb_cust-list btnCustList begin_cust-no end_cust-no 
        begin_whse end_whse begin_loc-bin end_loc-bin rd_i-code tb_sets 
        tb_cust-whse tb_random sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
        btn_Up btn_down rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok 
        btn-cancel 
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

        CREATE ttRptSelected.
        ASSIGN 
            ttRptSelected.TextList        = ENTRY(i,cTmpList)
            ttRptSelected.FieldList       = ttRptList.FieldList
            ttRptSelected.FieldLength     = int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
            ttRptSelected.DisplayOrder    = i
            ttRptSelected.HeadingFromLeft = IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
            iColumnLength                 = iColumnLength + ttRptSelected.FieldLength + 1.
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
                    ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
                    ldummy                   = sl_selected:DELETE(i)
                    sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
                    .
            ELSE
                IF move = "Up" AND i NE 1 THEN
                    ASSIGN
                        ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i - 1)
                        ldummy                   = sl_selected:DELETE(i + 1)
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
    RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChangeDest C-Win 
PROCEDURE pChangeDest :
    /*------------------------------------------------------------------------------
         Purpose:    
         Parameters:  <none>
         Notes:      
        ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF rd-dest:SCREEN-VALUE EQ "3" THEN
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "Yes"
                fi_file:SENSITIVE       = YES
                tb_OpenCSV:SENSITIVE    = YES      
                .
        ELSE
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                fi_file:SENSITIVE       = NO
                tb_OpenCSV:SENSITIVE    = NO      
                .
        ASSIGN 
            fi_file:SCREEN-VALUE = "c:\tmp\ItemAuditByWhse.csv".    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------ fg/rep/fg-ibtag.p 9/91 cd */
    /* FINISHED GOODS - INVENTORY ON HAND BY BIN / TAG".                          */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-topw.f}*/

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.

    FORM HEADER    
        "ITEM #         "
        "ITEM NAME                "
        "WHSE "
        "BIN      "
        "    UNITS"
        "   COUNT"
        "BIN QTY ITEMS TOTAL" 
        SKIP
        "---------------"
        "-------------------------"
        "-----"
        "---------"
        "---------"
        "--------"
        "-------------------"
        WITH FRAME r-top1 STREAM-IO WIDTH 200
        NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.

    ASSIGN
        str-tit2       = c-win:TITLE
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
        v-type         = "A" /*SUBSTR(rd_i-code,1,1)*/
        v-sort-by-cust = SUBSTR(rd_sort,1,2)
        zbal           = tb_zero
        v-custown      = tb_cust-whse
        v-prt-c        = tb_cost
        v-dl-mat       = tb_cost-2
        v-prt-p        = tb_sell-pr
        v-prt-cpn      = tb_cust-pt
        v-prt-po       = tb_po-num
        v-prt-arqty    = tb_actrel
        v-po-type      = SUBSTR(rd_po-type,1,1)
        v-prt-msf      = rd_msf EQ "MSF"
        v-subt         = tb_subt
        v-fgprice      = rd_price EQ "FG"
        v-sets         = tb_sets
        v-rct-date     = tb_rct-date
        v-summ-bin     = NO /*tb_summ-bin*/

        v-tot-qty      = 0
        v-tot-cst      = 0
        v-tot-ext      = 0
        v-label1       = ""
        v-label2       = ""
        v-label3       = ""
        lSelected      = tb_cust-list.

    ASSIGN
        v-file     = cFileName
        v-runexcel = tb_OpenCSV.
    v-excel        = IF rd-dest EQ 3 THEN YES ELSE NO .

    IF lselected THEN 
    DO:
        FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
        IF AVAILABLE ttCustList THEN ASSIGN  fcus = ttCustList.cust-no .
        FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
        IF AVAILABLE ttCustList THEN ASSIGN  tcus = ttCustList.cust-no .
    END.

    IF v-prt-c THEN 
    DO: 
        IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).
        ASSIGN
            v-prt-c = ll-secure
            v-prt-p = (v-prt-c AND v-dl-mat) OR (v-prt-p AND NOT v-dl-mat).
    END.

    SESSION:SET-WAIT-STATE ("general").

    v-qoh-f     = "->>>,>>9".

    DEFINE VARIABLE iRandom       AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.
    DEFINE VARIABLE iRecordNum    AS INTEGER NO-UNDO.
    DEFINE VARIABLE isItemofFirst AS LOG     NO-UNDO.

    iRandom = RANDOM(1,100) .
    ASSIGN
        str-tit4 = ""
        str-tit5 = ""
        str-line = "" .
    DEFINE VARIABLE cslist AS CHARACTER NO-UNDO.
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
            THEN ASSIGN str-tit4    = str-tit4 + ttRptSelected.TextList + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + "," .        
        ELSE 
            ASSIGN str-tit4    = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + ","
                .        
        cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "BIN QTY ITEMS TOTAL") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN 
    DO:
        RUN show-param.
        PAGE.
    END.

    VIEW FRAME r-top.
    /*VIEW FRAME r-top1.*/

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).

        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.

    /* EXPORT STREAM excel DELIMITER ","     
            "ITEM #"
            "ITEM NAME"
            "WHSE"
            "BIN"
            "UNITS"
            "COUNT"
            "BIN QTY ITEMS TOTAL" 
            SKIP.*/

    END. /* IF rd-dest = 3 THEN DO: */ 


    STATUS DEFAULT "Processing...".

    EMPTY TEMP-TABLE tt-fg-bin.
    EMPTY TEMP-TABLE tt-itemfg.

    /* ===
    ItemMain:
    FOR EACH itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
         /* AND itemfg.cust-no GE fcus
          AND itemfg.cust-no LE tcus
          AND itemfg.i-no    GE fino
          AND itemfg.i-no    LE tino
          AND itemfg.procat  GE fcat
          AND itemfg.procat  LE tcat
          AND (itemfg.i-code EQ v-type or v-type eq "A")
          */
          AND (NOT v-sets    OR
               itemfg.isaset OR
               CAN-FIND(FIRST fg-set
                        WHERE fg-set.company EQ itemfg.company
                          AND fg-set.part-no EQ itemfg.i-no))
        /*USE-INDEX customer*/ :

      RUN fg/rep/tt-fgbin.p (BUFFER itemfg, vdat, "", "zzzzzzzzzz",
                             v-loc[1], v-loc[2], v-loc-bin[1], v-loc-bin[2],
                             zbal, fi_days-old, YES, v-custown).
      isItemofFirst = YES.
      FOR EACH tt-fg-bin
          WHERE tt-fg-bin.company EQ itemfg.company
            AND tt-fg-bin.i-no    EQ itemfg.i-no
            AND (v-custown OR tb_cust-whse-2 OR
                 (tt-fg-bin.cust-no EQ "" AND tt-fg-bin.loc NE "CUST"))
            AND (NOT tb_cust-whse-2 OR
                 (tt-fg-bin.cust-no NE "" OR tt-fg-bin.loc EQ "CUST"))
          USE-INDEX co-ino:

        IF tb_random AND isItemofFirst THEN DO:
           iCount = iCount + 1.
           IF iCount <> iRandom THEN NEXT.

           iRandom = iRandom + RANDOM(1,100).
           iRecordNum = iRecordNum + 1.
           /*MESSAGE iCount iRandom iRecordNum SKIP
               itemfg.i-no 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

           IF iRecordNum > 20 THEN LEAVE.
           isItemofFirst = NO.
        END.

        IF (tt-fg-bin.qty NE 0 OR zbal) 
            /*AND icount = iRandom AND iRecordNum <= 20*/ THEN DO:            
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
                                   STRING(tt-itemfg.tag,"x(20)")
           tt-itemfg.cases = TRUNC((tt-fg-bin.qty - tt-fg-bin.partial-count) / tt-fg-bin.case-count,0)
           tt-itemfg.case-count = tt-fg-bin.case-count
           /*iRandom = iRandom + RANDOM(1,100)
           iRecordNum = iRecordNum + 1*/.

        END.

        ELSE DELETE tt-fg-bin.
      END.
    END.
    === */

    RUN buildTTBin (vdat, "", "zzzzzzzzzz",
        v-loc[1], v-loc[2], v-loc-bin[1], v-loc-bin[2],
        zbal, fi_days-old, YES, v-custown).
    RUN fg/rep/fg-waudN.p.

    PUT SKIP(1).


    DO:


        /* put "GRAND TOTALS" TO 70.
   
         IF v-prt-msf THEN
           PUT v-tot-qty[3] FORMAT "->>>,>>9.999" TO 96.
         ELSE
           PUT v-tot-qty[3] TO 96.   
         PUT SKIP.*/

        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "item"    THEN 
                    cVarValue = "".
                WHEN "name"   THEN 
                    cVarValue = "".
                WHEN "whse"   THEN 
                    cVarValue = "".
                WHEN "bin"  THEN 
                    cVarValue = "" .
                WHEN "unit"   THEN 
                    cVarValue = "" .
                WHEN "count"  THEN 
                    cVarValue = "" .
                WHEN "tot"   THEN 
                    cVarValue = STRING(v-tot-qty[3] ,"->>,>>>,>>>,>>>,>>9") .
                WHEN "tag"   THEN 
                    cVarValue = "" .

            END CASE.

            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
        PUT str-line SKIP .
        PUT UNFORMATTED  
            "           Grand Totals " SUBSTRING(cDisplay,25,350) SKIP.
        IF rd-dest = 3 THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                ' Grand Totals ,' 
                SUBSTRING(cExcelDisplay,4,350) SKIP(1).
        END.


    END.

    STATUS DEFAULT "".

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    SESSION:SET-WAIT-STATE ("").

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
            end_cust-no:SENSITIVE   = NOT iplChecked
            begin_cust-no:VISIBLE   = NOT iplChecked
            end_cust-no:VISIBLE     = NOT iplChecked
            btnCustList:SENSITIVE   = iplChecked
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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE    NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha.

    lv-frame-hdl = FRAME {&frame-name}:handle.
    lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
    lv-field-hdl = lv-group-hdl:FIRST-CHILD .

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
            THEN 
        DO:
            IF lv-field-hdl:LABEL <> ? THEN 
                ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                    .
            ELSE 
            DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    .
                lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN 
                    DO:
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
        SKIP(1).

    DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
        IF ENTRY(i,parm-fld-list) NE "" OR
            entry(i,parm-lbl-list) NE "" THEN 
        DO:

            lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                trim(ENTRY(i,parm-lbl-list)) + ":".

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

