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
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */     

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS cha       NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

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

DEFINE VARIABLE is-xprint-form  AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file     AS cha       NO-UNDO.
DEFINE VARIABLE v-tot-qty       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-tot-amt       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-grand-tot-qty AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-grand-tot-amt AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-date          AS DATE      NO-UNDO.
DEFINE VARIABLE v-vend-no       AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-procat        LIKE item.procat NO-UNDO.
DEFINE VARIABLE v-qty-r         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-amt-r         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-qty-i         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-amt-i         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-cost          LIKE po-ordl.cost NO-UNDO.

DEFINE VARIABLE cDisplay        AS cha       NO-UNDO.
DEFINE VARIABLE cExcelDisplay   AS cha       NO-UNDO.
DEFINE VARIABLE hField          AS HANDLE    NO-UNDO.
DEFINE VARIABLE cTmpField       AS CHA       NO-UNDO.
DEFINE VARIABLE cVarValue       AS cha       NO-UNDO.
DEFINE VARIABLE cExcelVarValue  AS cha       NO-UNDO.
DEFINE VARIABLE cSelectedList   AS cha       NO-UNDO.
DEFINE VARIABLE cFieldName      AS cha       NO-UNDO.
DEFINE VARIABLE str-line        AS cha       FORM "x(300)" NO-UNDO.

DEFINE STREAM excel.
DEFINE TEMP-TABLE temp-po-rec NO-UNDO
    FIELD po-no      AS INTEGER
    FIELD po-line    AS INTEGER
    FIELD vend-no    AS CHARACTER FORMAT "X(8)"
    FIELD gl-acct    LIKE po-ordl.actnum
    FIELD date-rec   AS DATE
    FIELD item-no    LIKE po-ordl.i-no
    FIELD descr      AS CHARACTER FORMAT "X(15)"
    FIELD prod-cat   AS CHARACTER
    FIELD qty-to-inv AS DECIMAL   FORMAT "->>,>>>,>>9.9"
    FIELD qty-uom    AS CHARACTER
    FIELD whse       AS CHARACTER
    FIELD cost-each  AS DECIMAL   FORMAT ">>>,>>9.99<<<<"
    FIELD amt-to-inv AS DECIMAL   FORMAT "->>,>>>,>>9.99"

    INDEX temp-vend-no vend-no ASC po-no ASC
    INDEX temp-gl-acct gl-acct ASC po-no ASC.

DEFINE TEMP-TABLE tt-neg-po-line NO-UNDO
    FIELD po-no     AS INTEGER
    FIELD po-line   AS INTEGER
    FIELD i-no      AS CHARACTER
    FIELD item-type AS LOG
    FIELD qty       LIKE fg-rdtlh.qty
    FIELD rcp-date  AS DATE
    FIELD amt       AS DECIMAL   DECIMALS 2
    INDEX po-no po-no i-no.

{api/ttReceipt.i}

FORM temp-po-rec.vend-no    COLUMN-LABEL "Vendor"
    FORMAT "X(8)"
    temp-po-rec.gl-acct    COLUMN-LABEL "G/L Account"
    FORMAT "X(25)"
    temp-po-rec.po-no      COLUMN-LABEL "P.O.!Number"
    FORMAT ">>>>>9"
    temp-po-rec.date-rec   COLUMN-LABEL "Date!Received" FORMAT "99/99/99"
    temp-po-rec.item-no    COLUMN-LABEL "Item!Number" FORMAT "X(15)"
    temp-po-rec.descr      COLUMN-LABEL "Description"       
    FORMAT "x(15)" 
    temp-po-rec.prod-cat   COLUMN-LABEL "Prod!Cat" FORMAT "X(5)"
    temp-po-rec.qty-to-inv COLUMN-LABEL "Quantity!To Invoice"
    FORMAT "->>,>>>,>>9.9"
    temp-po-rec.whse       COLUMN-LABEL "Whse" FORMAT "X(5)"
    temp-po-rec.cost-each  COLUMN-LABEL "Cost Each"
    FORMAT ">>>,>>9.99<<<<"
    temp-po-rec.amt-to-inv COLUMN-LABEL "Amt To!Invoice"
    FORMAT "->>,>>>,>>9.99"
    WITH FRAME detail DOWN NO-BOX STREAM-IO WIDTH 137.


DEFINE VARIABLE ldummy             AS LOG     NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS cha     NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha     NO-UNDO.
DEFINE VARIABLE cFieldLength       AS cha     NO-UNDO.
DEFINE VARIABLE cFieldType         AS cha     NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS cha       NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO .

ASSIGN 
    cTextListToSelect  = "Vendor,G/L Account,PO#,PO Line,Date Rec,Item Number,Description,Cat," +
                           "Qty to Be Invoiced,Whse,Cost Each,Amt to Invoice," +
                           "Receipt Date,Job#,location,Bin,Rec Quantity,Qty Uom,Cost,Cost Uom,Extended Value," +
                           "Invoice #,Invoice Date,Inv Quantity,Inv Qty Uom,Price,Price Uom,Inv line Amount"

    cFieldListToSelect = "vend,act,po,po-line,date,item,desc,cat," +
                            "inv-qty,whse,cost,inv-amt," +
                            "rec-date,rec-job,rec-loc,rec-bin,rec-qty,rec-qty-uom,rec-cost,rec-cost-uom,rec-ext-value," +
                            "inv-no,inv-date,inv-qty2,inv-qty-uom,inv-price,inv-price-uom,inv-line-amount"
    cFieldLength       = "8,25,6,8,8,15,25,6," + "18,5,10,14," + "12,13,8,8,12,10,14,10,14," + "20,12,12,11,13,10,15"
    cFieldType         = "c,c,i,c,c,c,c,c," + "i,c,i,i," + "c,c,c,c,i,c,i,c,i," + "c,c,i,c,i,c,i"
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Vendor,G/L Account,PO#,Date Rec,Item Number,Description,Cat," +
                           "Qty to Be Invoiced,Whse,Cost Each,Amt to Invoice" .

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
sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest td-show-parm fi_file ~
tb_show-details tb_print-subtotals tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_po-no end_po-no ~
begin_po-i-no end_po-i-no begin_rdate end_rdate begin_vend end_vend ~
scr-neg-rec tb_in-po rd_sort sl_avail sl_selected rd-dest td-show-parm ~
tb_show-details tb_print-subtotals fi_file tb_OpenCSV tbAutoClose 

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
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

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

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning PO Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_po-i-no  AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-no    AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "Beginning PO#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_rdate    AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Receipt Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_vend     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending PO Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_po-i-no    AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-no      AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 999999 
    LABEL "Ending PO#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rdate      AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Receipt Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_vend       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-notvou.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE
    SIZE 43 BY 1
    FGCOLOR 0 .

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

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 1 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 16 BY 3.81 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Vendor", "Vendor",
    "GL Code", "GL Code"
    SIZE 26 BY .71 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 4.91.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 9.05.

DEFINE VARIABLE sl_avail           AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected        AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE scr-neg-rec        AS LOGICAL   INITIAL NO 
    LABEL "Show Negative Receipts?" 
    VIEW-AS TOGGLE-BOX
    SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tbAutoClose        AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel           AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_in-po           AS LOGICAL   INITIAL NO 
    LABEL "Include Closed POs?" 
    VIEW-AS TOGGLE-BOX
    SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV         AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY .81
    BGCOLOR 15 NO-UNDO.
     
DEFINE VARIABLE tb_show-details    AS LOGICAL   INITIAL NO 
    LABEL "Show Details" 
    VIEW-AS TOGGLE-BOX
    SIZE 18 BY .81 NO-UNDO.
     
DEFINE VARIABLE tb_print-subtotals AS LOGICAL   INITIAL NO 
    LABEL "Print Subtotals" 
    VIEW-AS TOGGLE-BOX
    SIZE 18 BY .81 NO-UNDO.     

DEFINE VARIABLE td-show-parm       AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_date AT ROW 2.24 COL 27 COLON-ALIGNED
    end_date AT ROW 2.24 COL 68 COLON-ALIGNED
    begin_po-no AT ROW 3.33 COL 27 COLON-ALIGNED HELP
    "Enter Beginning PO Number"
    end_po-no AT ROW 3.33 COL 68 COLON-ALIGNED HELP
    "Enter Ending PO Number"
    begin_po-i-no AT ROW 4.43 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Order Number"
    end_po-i-no AT ROW 4.43 COL 68 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    begin_rdate AT ROW 5.57 COL 27 COLON-ALIGNED
    end_rdate AT ROW 5.57 COL 68 COLON-ALIGNED
    begin_vend AT ROW 6.67 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_vend AT ROW 6.67 COL 68 COLON-ALIGNED HELP
    "Enter Ending Vendor Number"
    scr-neg-rec AT ROW 7.86 COL 29 WIDGET-ID 2
    tb_show-details AT ROW 7.86 COL 62.4 WIDGET-ID 60
    tb_in-po AT ROW 8.67 COL 29 WIDGET-ID 58
    tb_print-subtotals AT ROW 8.67 COL 62.4 WIDGET-ID 62
    rd_sort AT ROW 9.62 COL 29 NO-LABELS
    sl_avail AT ROW 11.71 COL 3.6 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 11.71 COL 41.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 11.71 COL 60.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 12.71 COL 41.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 13.71 COL 41.2 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 14.76 COL 41.2 WIDGET-ID 40
    btn_down AT ROW 15.76 COL 41.2 WIDGET-ID 42
    rd-dest AT ROW 18.05 COL 7 NO-LABELS
    lv-font-no AT ROW 18.1 COL 34 COLON-ALIGNED
    lv-ornt AT ROW 18.29 COL 32 NO-LABELS
    lines-per-page AT ROW 18.29 COL 85 COLON-ALIGNED
    lv-font-name AT ROW 18.33 COL 27 COLON-ALIGNED NO-LABELS
    tb_excel AT ROW 18.33 COL 52 RIGHT-ALIGNED
    td-show-parm AT ROW 19.52 COL 30
    fi_file AT ROW 20.71 COL 27 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 20.76 COL 86.6 RIGHT-ALIGNED
    tbAutoClose AT ROW 22.67 COL 31 WIDGET-ID 16
    btn-ok AT ROW 23.43 COL 31
    btn-cancel AT ROW 23.43 COL 52.4
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 11 COL 6 WIDGET-ID 38
    "Sort By:" VIEW-AS TEXT
    SIZE 8 BY .62 AT ROW 9.67 COL 21
    "Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 17.14 COL 4.6
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.33 COL 4.6
    BGCOLOR 15 
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 11 COL 60.6 WIDGET-ID 44
    RECT-6 AT ROW 17.48 COL 3.6
    RECT-7 AT ROW 1.71 COL 3.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.6 ROW 1.24
    SIZE 95.2 BY 26.29
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
        TITLE              = "PO Receipts Not Vouchered"
        HEIGHT             = 24.29
        WIDTH              = 95.8
        MAX-HEIGHT         = 33.29
        MAX-WIDTH          = 204.8
        VIRTUAL-HEIGHT     = 33.29
        VIRTUAL-WIDTH      = 204.8
        RESIZE             = YES
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = 15
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
ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_po-i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_po-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_rdate:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_po-i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_po-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_rdate:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lines-per-page:HIDDEN IN FRAME FRAME-A = TRUE.

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

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* PO Receipts Not Vouchered */
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
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-i-no C-Win
ON LEAVE OF begin_po-i-no IN FRAME FRAME-A /* Beginning Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-no C-Win
ON LEAVE OF begin_po-no IN FRAME FRAME-A /* Beginning PO# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rdate C-Win
ON LEAVE OF begin_rdate IN FRAME FRAME-A /* Beginning Receipt Date */
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
            ASSIGN {&displayed-objects}.
        END.
        IF rd-dest = 3 THEN
        DO:
            fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.
        RUN GetSelectionList.
        IF scr-neg-rec = NO THEN
            RUN run-report.
        ELSE
            RUN run-report-2. 
        STATUS DEFAULT "Processing Complete".
        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN 
                DO:  
                    IF NOT tb_OpenCSV THEN 
                    DO:        
                        MESSAGE "CSV file have been created." SKIP(1)
                            "~"OK"~"Want to open CSV file?"
                            VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                            TITLE "" UPDATE lChoice AS LOGICAL.
                     
                        IF lChoice THEN
                        DO:
                            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                        END.
                    END.
                    ELSE DO:
                          OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)). 
                    END.
                END. /* WHEN 3 THEN DO: */
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &begin_cust=begin_po-no
                            &END_cust=END_po-no
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
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = ''
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

                    END.

                END. 
            WHEN 6 THEN RUN output-to-port.
        END CASE. 
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
    DO:
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

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
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

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
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-i-no C-Win
ON LEAVE OF end_po-i-no IN FRAME FRAME-A /* Ending Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-no C-Win
ON LEAVE OF end_po-no IN FRAME FRAME-A /* Ending PO# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rdate C-Win
ON LEAVE OF end_rdate IN FRAME FRAME-A /* Ending Receipt Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Name */
    DO:
   //  assign {&self-name}.
        fi_file = ''.
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
        DEFINE VARIABLE char-val AS cha NO-UNDO.

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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
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

    begin_rdate = DATE(1,1,YEAR(TODAY)).
    RUN DisplaySelectionList.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    Btn_Def:load-image("Graphics/32x32/default.png").
    Btn_Add:load-image("Graphics/32x32/additem.png").
    Btn_Remove:load-image("Graphics/32x32/remove.png").
    btn_Up:load-image("Graphics/32x32/moveup.png").
    btn_down:load-image("Graphics/32x32/movedown.png").
    RUN enable_UI.

    {methods/nowait.i}
    {sys/inc/reportsConfigNK1.i "PR7" }
    ASSIGN
        td-show-parm:sensitive = lShowParameters
        td-show-parm:hidden    = NOT lShowParameters
        td-show-parm:visible   = lShowParameters
        .
    
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_date.
    END.
    RUN pChangeDest.
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

    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "".

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:             
            WHEN "vend"    THEN 
                cVarValue = STRING(temp-po-rec.vend-no,"x(8)") .
            WHEN "act"   THEN 
                cVarValue = STRING(temp-po-rec.gl-acct,"x(25)").
            WHEN "po"   THEN 
                cVarValue = STRING(temp-po-rec.po-no,">>>>>9").
            WHEN "po-line"   THEN 
                cVarValue = IF temp-po-rec.po-line NE 0 THEN STRING(temp-po-rec.po-line,">9") ELSE "".
            WHEN "date"  THEN 
                cVarValue = IF temp-po-rec.date-rec NE ? THEN STRING(temp-po-rec.date-rec,"99/99/99") ELSE "" .
            WHEN "item"   THEN 
                cVarValue = STRING(temp-po-rec.item-no,"x(15)") .
            WHEN "desc"  THEN 
                cVarValue = STRING(temp-po-rec.descr,"x(25)") .
            WHEN "cat"   THEN 
                cVarValue = STRING(temp-po-rec.prod-cat,"x(6)") .
            WHEN "inv-qty"  THEN 
                cVarValue = STRING(temp-po-rec.qty-to-inv,"->>,>>>,>>9.9") .
            WHEN "whse"  THEN 
                cVarValue = STRING(temp-po-rec.whse,"x(5)") .
            WHEN "cost"   THEN 
                cVarValue = STRING(temp-po-rec.cost-each,">>>>>>9.99<<<<") .
            WHEN "inv-amt"  THEN 
                cVarValue = STRING(temp-po-rec.amt-to-inv,"->>,>>>,>>9.99") .
            OTHERWISE 
            cVarValue = "".
        END CASE.

        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT UNFORMATTED cDisplay SKIP.
    IF rd-dest = 3 THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            cExcelDisplay SKIP.
    END.
            
    IF LOGICAL(tb_show-details) THEN
        RUN pPrintDetail( INPUT temp-po-rec.vend-no,
            INPUT temp-po-rec.po-no,
            INPUT temp-po-rec.po-line).

    ASSIGN
        v-tot-qty = v-tot-qty + temp-po-rec.qty-to-inv
        v-tot-amt = v-tot-amt + temp-po-rec.amt-to-inv.


    IF tb_print-subtotals AND ip-last-of AND v-tot-amt NE 0 THEN 
    DO:
        /* underline temp-po-rec.qty-to-inv temp-po-rec.amt-to-inv with frame detail.
         display v-tot-qty @ temp-po-rec.qty-to-inv v-tot-amt @ temp-po-rec.amt-to-inv with frame detail.
         down 2 with frame detail.*/
        PUT str-line SKIP.
        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "vend"    THEN 
                    cVarValue = "" .
                WHEN "act"   THEN 
                    cVarValue = "".
                WHEN "po"   THEN 
                    cVarValue = "".
                WHEN "date"  THEN 
                    cVarValue = "" .
                WHEN "item"   THEN 
                    cVarValue = "" .
                WHEN "desc"  THEN 
                    cVarValue = "" .
                WHEN "cat"   THEN 
                    cVarValue = "" .
                WHEN "inv-qty"  THEN 
                    cVarValue = STRING(v-tot-qty,"->>,>>>,>>9.9") .
                WHEN "whse"  THEN 
                    cVarValue = "" .
                WHEN "cost"   THEN 
                    cVarValue = "" .
                WHEN "inv-amt"  THEN 
                    cVarValue = STRING(v-tot-amt,"->>,>>>,>>9.99") .
                OTHERWISE 
                cVarValue = "".
            END CASE.

            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.

        PUT UNFORMATTED "           Totals:" + SUBSTRING(cDisplay,19,300) SKIP(1).
        IF rd-dest = 3 THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                ' Totals ,'
                SUBSTRING(cExcelDisplay,4,300) SKIP(1).
        END.

      
    END.
    ASSIGN
        v-grand-tot-qty = v-grand-tot-qty + v-tot-qty
        v-grand-tot-amt = v-grand-tot-amt + v-tot-amt
        v-tot-qty       = 0
        v-tot-amt       = 0.
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
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.

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

    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.

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
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.
  
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
    DISPLAY begin_date end_date begin_po-no end_po-no begin_po-i-no end_po-i-no 
        begin_rdate end_rdate begin_vend end_vend scr-neg-rec tb_in-po rd_sort 
        sl_avail sl_selected rd-dest td-show-parm fi_file tb_OpenCSV 
        tbAutoClose tb_show-details tb_print-subtotals
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_date end_date begin_po-no end_po-no begin_po-i-no 
        end_po-i-no begin_rdate end_rdate begin_vend end_vend scr-neg-rec 
        tb_in-po rd_sort sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
        btn_Up btn_down rd-dest td-show-parm fi_file tb_OpenCSV tbAutoClose 
        tb_show-details tb_print-subtotals btn-ok btn-cancel 
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
    DEFINE VARIABLE cTmpList AS cha NO-UNDO.

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
    RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunAPIOutboundTrigger C-Win
PROCEDURE pRunAPIOutboundTrigger PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdOutboundProcs AS HANDLE    NO-UNDO.
    
    IF NOT TEMP-TABLE ttReceipt:HAS-RECORDS THEN
        RETURN.
        
    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.
                            
    RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
        INPUT  ttReceipt.company,                                  /* Company Code (Mandatory) */
        INPUT  ttReceipt.loc,                                      /* Location Code (Mandatory) */
        INPUT  "SendReceipts",                                     /* API ID (Mandatory) */
        INPUT  "",                                                 /* Scope ID */
        INPUT  "",                                                 /* Scoped Type */
        INPUT  "POReceipt",                                        /* Trigger ID (Mandatory) */
        INPUT  "TTReceiptHandle",                                  /* Comma separated list of table names for which data being sent (Mandatory) */
        INPUT  STRING(TEMP-TABLE ttReceipt:HANDLE),                /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
        INPUT  "PO Receipts Not Vouchered",                        /* Primary ID for which API is called for (Mandatory) */   
        INPUT  "Triggered from RM Post",                           /* Event's description (Optional) */
        OUTPUT lSuccess,                                           /* Success/Failure flag */
        OUTPUT cMessage                                            /* Status message */
        ) NO-ERROR.
        
    EMPTY TEMP-TABLE ttReceipt.
        
    DELETE PROCEDURE hdOutboundProcs.  
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE fdat     AS DATE      FORMAT "99/99/9999" NO-UNDO INIT 01/01/0001.
    DEFINE VARIABLE tdat     AS DATE      FORMAT "99/99/9999" NO-UNDO INIT 12/31/9999.
    DEFINE VARIABLE fitm     LIKE item.i-no NO-UNDO.
    DEFINE VARIABLE titm     LIKE fitm NO-UNDO INIT "zzzzzzzzzzzzzzzz".
    DEFINE VARIABLE fpo      LIKE po-ordl.po-no NO-UNDO INIT 1.
    DEFINE VARIABLE tpo      LIKE fpo NO-UNDO INIT 999999.
    DEFINE VARIABLE frdat    AS DATE      FORMAT "99/99/9999" NO-UNDO INIT 01/01/0001.
    DEFINE VARIABLE trdat    LIKE frdat NO-UNDO INIT 12/31/9999.
    DEFINE VARIABLE fvend    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE tvend    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE str-tit4 AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5 AS cha       FORM "x(200)" NO-UNDO.

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    ASSIGN 
        v-grand-tot-qty = 0
        v-grand-tot-amt = 0
        v-tot-qty       = 0
        v-tot-amt       = 0.

    EMPTY TEMP-TABLE temp-po-rec.

    {sa/sa-sls01.i}

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        fdat     = begin_date
        tdat     = end_date
        fpo      = begin_po-no
        tpo      = end_po-no
        fitm     = begin_po-i-no
        titm     = end_po-i-no
        frdat    = begin_rdate
        trdat    = end_rdate
        fvend    = begin_vend
        tvend    = END_vend
        str-line = "". 

    DEFINE VARIABLE cslist AS cha NO-UNDO.
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
            AND (LOGICAL(tb_show-details)
            OR LOOKUP(ttRptSelected.TextList,"Receipt Date,Job#,location,Bin,Rec Quantity,Qty Uom,Cost,Cost Uom,Extended Value,Invoice #,Invoice Date,Inv Quantity,Inv Qty Uom,Price,Price Uom,Inv line Amount") EQ 0 ) THEN
            ASSIGN str-tit4    = str-tit4 + ttRptSelected.TextList + " "
                   str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                   excelheader = excelHeader + ttRptSelected.TextList + "," .        
        ELSE IF (LOGICAL(tb_show-details)
            OR LOOKUP(ttRptSelected.TextList,"Receipt Date,Job#,location,Bin,Rec Quantity,Qty Uom,Cost,Cost Uom,Extended Value,Invoice #,Invoice Date,Inv Quantity,Inv Qty Uom,Price,Price Uom,Inv line Amount") EQ 0 ) THEN
            ASSIGN str-tit4    = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + ","
                .        
        cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "Qty to Be Invoiced,Amt to Invoice") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.
   
    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        /*excelheader = "Vendor,G/L Account,P.O. Number,Date Received,Item Number,"
                    + "Description,Prod Cat,Quantity To Invoice,Whse,Cost Each,"
                    + "Amt To Invoice".*/
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    DISPLAY "" WITH FRAME r-top.

    FOR EACH po-ordl
        WHERE po-ordl.company   EQ cocode
        AND (po-ordl.opened    EQ YES OR ( tb_in-po AND NOT po-ordl.opened) )
        AND po-ordl.i-no      GE fitm
        AND po-ordl.i-no      LE titm
        AND po-ordl.po-no     GE fpo
        AND po-ordl.po-no     LE tpo
        AND po-ordl.t-rec-qty GT 0
        AND NOT po-ordl.excludeFromVoucher
        USE-INDEX opened NO-LOCK,

        FIRST po-ord
        WHERE po-ord.company EQ cocode
        AND po-ord.po-no   EQ po-ordl.po-no
        AND po-ord.po-date GE fdat
        AND po-ord.po-date LE tdat
        AND po-ord.vend-no GE fvend
        AND po-ord.vend-no LE tvend
        AND NOT po-ord.excludeFromVoucher
        NO-LOCK:
        {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}
        ASSIGN
            v-date    = 01/01/0001
            v-qty-r   = 0
            v-amt-r   = 0
            v-qty-i   = 0
            v-amt-i   = 0
            v-vend-no = po-ord.vend-no.

        IF po-ordl.item-type THEN 
        DO:
            FIND FIRST item
                WHERE item.company EQ cocode
                AND item.i-no    EQ po-ordl.i-no
                NO-LOCK NO-ERROR.
            v-procat = IF AVAILABLE item THEN item.procat ELSE "".

            FOR EACH rm-rcpth FIELDS(trans-date r-no)
                WHERE rm-rcpth.company    EQ cocode
                AND rm-rcpth.i-no       EQ po-ordl.i-no
                AND rm-rcpth.po-no      EQ string(po-ordl.po-no)
                AND rm-rcpth.job-no     EQ po-ordl.job-no
                AND rm-rcpth.job-no2    EQ po-ordl.job-no2
                AND rm-rcpth.trans-date GE frdat
                AND rm-rcpth.trans-date LE trdat
                AND rm-rcpth.rita-code  EQ "R"
                USE-INDEX item-po NO-LOCK,

                FIRST rm-rdtlh
                WHERE rm-rdtlh.r-no   EQ rm-rcpth.r-no
                AND rm-rdtlh.s-num  EQ po-ordl.s-num
                NO-LOCK

                BY rm-rcpth.trans-date DESCENDING:

                v-date = rm-rcpth.trans-date.

                RUN sys/inc/po-recqa2.p (RECID(po-ordl), frdat, trdat, OUTPUT v-qty-r, OUTPUT v-amt-r).
                LEAVE.
            END.
        END.

        ELSE 
        DO:
            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ po-ordl.i-no
                NO-LOCK NO-ERROR.
            v-procat = IF AVAILABLE itemfg THEN itemfg.procat ELSE "".

            FOR EACH fg-rcpth FIELDS(trans-date)
                WHERE fg-rcpth.company    EQ cocode
                AND fg-rcpth.i-no       EQ po-ordl.i-no
                AND fg-rcpth.po-no      EQ string(po-ordl.po-no)
                AND fg-rcpth.rita-code  EQ "R"
                AND fg-rcpth.trans-date GE frdat
                AND fg-rcpth.trans-date LE trdat
                USE-INDEX item-po NO-LOCK:

                v-date = fg-rcpth.trans-date.

                RUN sys/inc/po-recqa2.p (RECID(po-ordl), frdat, trdat, OUTPUT v-qty-r, OUTPUT v-amt-r).

                LEAVE.
            END.
        END.

        IF v-amt-r NE 0 THEN 
        DO:
            RUN sys/inc/po-invqa.p (RECID(po-ordl), OUTPUT v-qty-i, OUTPUT v-amt-i).

            IF v-qty-r - v-qty-i GT 0 THEN
            DO:
                IF (v-amt-r GT v-amt-i AND v-amt-r GT 0) OR
                    (v-amt-r LT v-amt-i AND v-amt-r LT 0) THEN 
                DO:

                    v-cost = (((IF v-amt-r LT 0 THEN -1 ELSE 1) * v-amt-r) +
                        ((IF v-amt-i LT 0 THEN -1 ELSE 1) * v-amt-i)) /
                        (((IF v-qty-r LT 0 THEN -1 ELSE 1) * v-qty-r) +
                        ((IF v-qty-i LT 0 THEN -1 ELSE 1) * v-qty-i)).


                    CREATE temp-po-rec.
                    ASSIGN
                        temp-po-rec.vend-no    = v-vend-no
                        temp-po-rec.po-no      = po-ordl.po-no
                        temp-po-rec.po-line    = po-ordl.LINE
                        temp-po-rec.gl-acct    = po-ordl.actnum
                        temp-po-rec.date-rec   = v-date
                        temp-po-rec.item-no    = po-ordl.i-no
                        temp-po-rec.descr      = po-ordl.i-name  
                        temp-po-rec.prod-cat   = v-procat
                        temp-po-rec.qty-to-inv = v-qty-r - v-qty-i
                        temp-po-rec.qty-uom    = po-ordl.pr-uom
                        temp-po-rec.whse       = po-ord.loc
                        temp-po-rec.cost-each  = v-cost
                        temp-po-rec.amt-to-inv = v-amt-r - v-amt-i.
 
                     CREATE ttReceipt.
                     ASSIGN
                         iCount                 = iCount + 1
                         ttReceipt.lineID       = iCount
                         ttReceipt.company      = po-ordl.company
                         ttReceipt.location     = po-ord.loc
                         ttReceipt.poID         = po-ordl.po-no
                         ttReceipt.poLine       = po-ordl.line
                         ttReceipt.itemID       = po-ordl.i-no
                         ttReceipt.itemName     = IF po-ordl.i-name EQ "" THEN po-ordl.i-no ELSE po-ordl.i-name
                         ttReceipt.quantityUOM  = po-ordl.pr-uom
                         ttReceipt.quantity     = temp-po-rec.qty-to-inv
                         .
                                      
                    RELEASE temp-po-rec.
                END.
            END.
        END.
    END.  /* For each po-ordl */

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
    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "".

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:             
            WHEN "vend"    THEN 
                cVarValue = "" .
            WHEN "act"   THEN 
                cVarValue = "".
            WHEN "po"   THEN 
                cVarValue = "".
            WHEN "date"  THEN 
                cVarValue = "" .
            WHEN "item"   THEN 
                cVarValue = "" .
            WHEN "desc"  THEN 
                cVarValue = "" .
            WHEN "cat"   THEN 
                cVarValue = "" .
            WHEN "inv-qty"  THEN 
                cVarValue = STRING(v-grand-tot-qty,"->>,>>>,>>9.9") .
            WHEN "whse"  THEN 
                cVarValue = "" .
            WHEN "cost"   THEN 
                cVarValue = "" .
            WHEN "inv-amt"  THEN 
                cVarValue = STRING(v-grand-tot-amt,"->>,>>>,>>9.99") .
            OTHERWISE 
            cVarValue = "".
        END CASE.

        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT UNFORMATTED "     Grand Totals:" + SUBSTRING(cDisplay,19,300) SKIP(1).
    IF rd-dest = 3 THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            ' Grand Totals ,'
            SUBSTRING(cExcelDisplay,4,300) SKIP.
    END.


    OUTPUT STREAM excel CLOSE.
    /*END.*/

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    
    RUN pRunAPIOutboundTrigger.
    
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

    DEFINE VARIABLE fdat             AS DATE      FORMAT "99/99/9999" NO-UNDO INIT 01/01/0001.
    DEFINE VARIABLE tdat             AS DATE      FORMAT "99/99/9999" NO-UNDO INIT 12/31/9999.
    DEFINE VARIABLE fitm             LIKE item.i-no NO-UNDO.
    DEFINE VARIABLE titm             LIKE fitm NO-UNDO INIT "zzzzzzzzzzzzzzzz".
    DEFINE VARIABLE fpo              LIKE po-ordl.po-no NO-UNDO INIT 1.
    DEFINE VARIABLE tpo              LIKE fpo NO-UNDO INIT 999999.
    DEFINE VARIABLE frdat            AS DATE      FORMAT "99/99/9999" NO-UNDO INIT 01/01/0001.
    DEFINE VARIABLE trdat            LIKE frdat NO-UNDO INIT 12/31/9999.
    DEFINE VARIABLE fvend            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE tvend            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ll-neg-inv-found AS LOG       NO-UNDO.

    DEFINE VARIABLE str-tit4         AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5         AS cha       FORM "x(200)" NO-UNDO.

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

    ASSIGN 
        v-grand-tot-qty = 0
        v-grand-tot-amt = 0
        v-tot-qty       = 0
        v-tot-amt       = 0.

    EMPTY TEMP-TABLE temp-po-rec.
    EMPTY TEMP-TABLE tt-neg-po-line.

    {sa/sa-sls01.i}

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        fdat     = begin_date
        tdat     = end_date
        fpo      = begin_po-no
        tpo      = end_po-no
        fitm     = begin_po-i-no
        titm     = end_po-i-no
        frdat    = begin_rdate
        trdat    = end_rdate
        fvend    = begin_vend
        tvend    = END_vend
        str-line = "". 

    DEFINE VARIABLE cslist AS cha NO-UNDO.
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
            AND (LOGICAL(tb_show-details)
            OR LOOKUP(ttRptSelected.TextList,"Receipt Date,Job#,location,Bin,Rec Quantity,Qty Uom,Cost,Cost Uom,Extended Value,Invoice #,Invoice Date,Inv Quantity,Inv Qty Uom,Price,Price Uom,Inv line Amount") EQ 0 ) THEN 
            ASSIGN str-tit4    = str-tit4 + ttRptSelected.TextList + " "
                   str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                   excelheader = excelHeader + ttRptSelected.TextList + "," .        
        ELSE IF (LOGICAL(tb_show-details)
            OR LOOKUP(ttRptSelected.TextList,"Receipt Date,Job#,location,Bin,Rec Quantity,Qty Uom,Cost,Cost Uom,Extended Value,Invoice #,Invoice Date,Inv Quantity,Inv Qty Uom,Price,Price Uom,Inv line Amount") EQ 0 ) THEN
            ASSIGN str-tit4    = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + ","
                .        
        cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "Qty to Be Invoiced,Amt to Invoice") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        /*excelheader = "Vendor,G/L Account,P.O. Number,Date Received,Item Number,"
                    + "Description,Prod Cat,Quantity To Invoice,Whse,Cost Each,"
                    + "Amt To Invoice".*/
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    DISPLAY "" WITH FRAME r-top.

    FOR EACH po-ordl
        WHERE po-ordl.company   EQ cocode
        AND (po-ordl.opened    EQ YES OR ( tb_in-po AND NOT po-ordl.opened) )
        AND po-ordl.i-no      GE fitm
        AND po-ordl.i-no      LE titm
        AND po-ordl.po-no     GE fpo
        AND po-ordl.po-no     LE tpo
        AND NOT po-ordl.excludeFromVoucher
        USE-INDEX opened NO-LOCK,

        FIRST po-ord
        WHERE po-ord.company EQ cocode
        AND po-ord.po-no   EQ po-ordl.po-no
        AND po-ord.po-date GE fdat
        AND po-ord.po-date LE tdat
        AND po-ord.vend-no GE fvend
        AND po-ord.vend-no LE tvend
        AND NOT po-ord.excludeFromVoucher
        NO-LOCK:
        {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}
        IF po-ordl.item-type AND
            NOT CAN-FIND(FIRST rm-rcpth WHERE
            rm-rcpth.company    EQ cocode
            AND rm-rcpth.i-no       EQ po-ordl.i-no
            AND rm-rcpth.po-no      EQ string(po-ordl.po-no)
            AND rm-rcpth.job-no     EQ po-ordl.job-no
            AND rm-rcpth.job-no2    EQ po-ordl.job-no2
            AND rm-rcpth.trans-date GE frdat
            AND rm-rcpth.trans-date LE trdat
            AND rm-rcpth.rita-code  EQ "R") THEN
            NEXT.
        ELSE IF NOT po-ordl.item-type AND
                NOT CAN-FIND(FIRST fg-rcpth WHERE
                fg-rcpth.company    EQ cocode
                AND fg-rcpth.i-no       EQ po-ordl.i-no
                AND fg-rcpth.po-no      EQ string(po-ordl.po-no)
                AND fg-rcpth.rita-code  EQ "R"
                AND fg-rcpth.trans-date GE frdat
                AND fg-rcpth.trans-date LE trdat) THEN
                NEXT.

        ASSIGN
            v-date    = 01/01/0001
            v-qty-r   = 0
            v-amt-r   = 0
            v-qty-i   = 0
            v-amt-i   = 0
            v-vend-no = po-ord.vend-no.

        IF po-ordl.item-type THEN 
        DO:
            FIND FIRST item
                WHERE item.company EQ cocode
                AND item.i-no    EQ po-ordl.i-no
                NO-LOCK NO-ERROR.
            v-procat = IF AVAILABLE item THEN item.procat ELSE "".

            FOR EACH rm-rcpth FIELDS(trans-date r-no)
                WHERE rm-rcpth.company    EQ cocode
                AND rm-rcpth.i-no       EQ po-ordl.i-no
                AND rm-rcpth.po-no      EQ string(po-ordl.po-no)
                AND rm-rcpth.job-no     EQ po-ordl.job-no
                AND rm-rcpth.job-no2    EQ po-ordl.job-no2
                AND rm-rcpth.trans-date GE frdat
                AND rm-rcpth.trans-date LE trdat
                AND rm-rcpth.rita-code  EQ "R"
                USE-INDEX item-po NO-LOCK,

                FIRST rm-rdtlh
                WHERE rm-rdtlh.r-no   EQ rm-rcpth.r-no
                AND rm-rdtlh.s-num  EQ po-ordl.s-num
                NO-LOCK

                BY rm-rcpth.trans-date DESCENDING:

                v-date = rm-rcpth.trans-date.

                RUN sys/inc/po-recqa2.p (RECID(po-ordl), frdat, trdat, OUTPUT v-qty-r, OUTPUT v-amt-r).
                LEAVE.
            END.

            FOR EACH rm-rcpth FIELDS(trans-date r-no)
                WHERE rm-rcpth.company    EQ cocode
                AND rm-rcpth.i-no       EQ po-ordl.i-no
                AND rm-rcpth.po-no      EQ string(po-ordl.po-no)
                AND rm-rcpth.job-no     EQ po-ordl.job-no
                AND rm-rcpth.job-no2    EQ po-ordl.job-no2
                AND rm-rcpth.trans-date GE frdat
                AND rm-rcpth.trans-date LE trdat
                AND rm-rcpth.rita-code  EQ "R"
                USE-INDEX item-po NO-LOCK,

                FIRST rm-rdtlh FIELDS(qty trans-date cost frt-cost)
                WHERE rm-rdtlh.r-no   EQ rm-rcpth.r-no
                AND rm-rdtlh.s-num  EQ po-ordl.s-num
                AND rm-rdtlh.qty LT 0
                NO-LOCK:

                IF AVAILABLE ITEM AND ITEM.inv-by-cust THEN
                    NEXT.

                /*IF NOT CAN-FIND(FIRST tt-neg-po-line WHERE
                   tt-neg-po-line.po-no EQ po-ordl.po-no AND
                   tt-neg-po-line.i-no EQ po-ordl.i-no and
                   tt-neg-po-line.item-type EQ po-ordl.item-type AND
                   tt-neg-po-line.qty EQ rm-rdtlh.qty) THEN
                   DO:*/
                CREATE tt-neg-po-line.
                ASSIGN 
                    tt-neg-po-line.po-no     = po-ordl.po-no
                    tt-neg-po-line.i-no      = po-ordl.i-no
                    tt-neg-po-line.item-type = po-ordl.item-type
                    tt-neg-po-line.qty       = rm-rdtlh.qty
                    tt-neg-po-line.rcp-date  = rm-rcpth.trans-date
                    tt-neg-po-line.amt       = (rm-rdtlh.qty * rm-rdtlh.cost) + rm-rdtlh.frt-cost.
                RELEASE tt-neg-po-line.
            /*END.*/
            END.
        END.

        ELSE 
        DO:
            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ po-ordl.i-no
                NO-LOCK NO-ERROR.
            v-procat = IF AVAILABLE itemfg THEN itemfg.procat ELSE "".

            FOR EACH fg-rcpth FIELDS(trans-date)
                WHERE fg-rcpth.company    EQ cocode
                AND fg-rcpth.i-no       EQ po-ordl.i-no
                AND fg-rcpth.po-no      EQ string(po-ordl.po-no)
                AND fg-rcpth.rita-code  EQ "R"
                AND fg-rcpth.trans-date GE frdat
                AND fg-rcpth.trans-date LE trdat
                USE-INDEX item-po NO-LOCK:

                v-date = fg-rcpth.trans-date.

                RUN sys/inc/po-recqa2.p (RECID(po-ordl), frdat, trdat, OUTPUT v-qty-r, OUTPUT v-amt-r).

                LEAVE.
            END.

            FOR EACH fg-rcpth FIELDS(trans-date rita-code r-no)
                WHERE fg-rcpth.company    EQ cocode
                AND fg-rcpth.i-no       EQ po-ordl.i-no
                AND fg-rcpth.po-no      EQ string(po-ordl.po-no)
                AND fg-rcpth.rita-code  EQ "R"
                AND fg-rcpth.trans-date GE frdat
                AND fg-rcpth.trans-date LE trdat
                USE-INDEX item-po NO-LOCK,
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
                ASSIGN 
                    tt-neg-po-line.po-no     = po-ordl.po-no
                    tt-neg-po-line.i-no      = po-ordl.i-no
                    tt-neg-po-line.item-type = po-ordl.item-type
                    tt-neg-po-line.qty       = fg-rdtlh.qty
                    tt-neg-po-line.rcp-date  = fg-rdtlh.trans-date
                    tt-neg-po-line.amt       = fg-rdtlh.qty / 1000 * fg-rdtlh.cost.
                RELEASE tt-neg-po-line.
            /*                   END. */
            END.
        END.

        IF v-amt-r NE 0 THEN 
        DO:
            RUN sys/inc/po-invqa.p (RECID(po-ordl), OUTPUT v-qty-i, OUTPUT v-amt-i).

            IF v-qty-r - v-qty-i GT 0 THEN
            DO:
                IF (v-amt-r GT v-amt-i AND v-amt-r GT 0) OR
                    (v-amt-r LT v-amt-i AND v-amt-r LT 0) THEN 
                DO:

                    v-cost = (((IF v-amt-r LT 0 THEN -1 ELSE 1) * v-amt-r) +
                        ((IF v-amt-i LT 0 THEN -1 ELSE 1) * v-amt-i)) /
                        (((IF v-qty-r LT 0 THEN -1 ELSE 1) * v-qty-r) +
                        ((IF v-qty-i LT 0 THEN -1 ELSE 1) * v-qty-i)).


                    CREATE temp-po-rec.
                    ASSIGN
                        temp-po-rec.vend-no    = v-vend-no
                        temp-po-rec.po-no      = po-ordl.po-no
                        temp-po-rec.po-line    = po-ordl.LINE
                        temp-po-rec.gl-acct    = po-ordl.actnum
                        temp-po-rec.date-rec   = v-date
                        temp-po-rec.item-no    = po-ordl.i-no
                        temp-po-rec.descr      = po-ordl.i-name  
                        temp-po-rec.prod-cat   = v-procat
                        temp-po-rec.qty-to-inv = v-qty-r - v-qty-i
                        temp-po-rec.qty-uom    = po-ordl.pr-uom
                        temp-po-rec.whse       = po-ord.loc
                        temp-po-rec.cost-each  = v-cost
                        temp-po-rec.amt-to-inv = v-amt-r - v-amt-i.
                    RELEASE temp-po-rec.
                END.
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
                        FOR EACH reftable
                            {ap/ap-reftbW.i po-ordl.po-no}
                            NO-LOCK,
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
                            temp-po-rec.vend-no    = v-vend-no
                            temp-po-rec.po-no      = po-ordl.po-no
                            temp-po-rec.po-line    = po-ordl.LINE
                            temp-po-rec.gl-acct    = po-ordl.actnum
                            temp-po-rec.date-rec   = tt-neg-po-line.rcp-date
                            temp-po-rec.item-no    = po-ordl.i-no
                            temp-po-rec.descr      = po-ordl.i-name  
                            temp-po-rec.prod-cat   = v-procat
                            temp-po-rec.qty-to-inv = tt-neg-po-line.qty
                            temp-po-rec.qty-uom    = po-ordl.pr-uom
                            temp-po-rec.whse       = po-ord.loc
                            temp-po-rec.cost-each  = tt-neg-po-line.amt / tt-neg-po-line.qty
                            temp-po-rec.amt-to-inv = tt-neg-po-line.amt.
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

            IF AVAILABLE tt-neg-po-line THEN
            DO:
                ll-neg-inv-found = NO.
                FOR EACH reftable
                    {ap/ap-reftbW.i po-ordl.po-no}
                    NO-LOCK,
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
                    temp-po-rec.vend-no    = v-vend-no
                    temp-po-rec.po-no      = po-ordl.po-no
                    temp-po-rec.po-line    = po-ordl.LINE
                    temp-po-rec.gl-acct    = po-ordl.actnum
                    temp-po-rec.date-rec   = tt-neg-po-line.rcp-date
                    temp-po-rec.item-no    = po-ordl.i-no
                    temp-po-rec.descr      = po-ordl.i-name  
                    temp-po-rec.prod-cat   = v-procat
                    temp-po-rec.qty-to-inv = tt-neg-po-line.qty
                    temp-po-rec.qty-uom    = po-ordl.pr-uom
                    temp-po-rec.whse       = po-ord.loc
                    temp-po-rec.cost-each  = tt-neg-po-line.amt / tt-neg-po-line.qty
                    temp-po-rec.amt-to-inv = tt-neg-po-line.amt.
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

IF AVAILABLE tt-neg-po-line THEN
DO:
    ll-neg-inv-found = NO.
    FOR EACH reftable
        {ap/ap-reftbW.i po-ordl.po-no}
        NO-LOCK,
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
        temp-po-rec.vend-no    = v-vend-no
        temp-po-rec.po-no      = po-ordl.po-no
        temp-po-rec.po-line    = po-ordl.LINE
        temp-po-rec.gl-acct    = po-ordl.actnum
        temp-po-rec.date-rec   = tt-neg-po-line.rcp-date
        temp-po-rec.item-no    = po-ordl.i-no
        temp-po-rec.descr      = po-ordl.i-name  
        temp-po-rec.prod-cat   = v-procat
        temp-po-rec.qty-to-inv = tt-neg-po-line.qty
        temp-po-rec.qty-uom    = po-ordl.pr-uom
        temp-po-rec.whse       = po-ord.loc
        temp-po-rec.cost-each  = tt-neg-po-line.amt / tt-neg-po-line.qty
        temp-po-rec.amt-to-inv = tt-neg-po-line.amt.
    RELEASE temp-po-rec.
END.

RELEASE tt-neg-po-line.
END.
END.
END.  /* For each po-ordl */

FOR EACH temp-po-rec:
    CREATE ttReceipt.
    ASSIGN
        iCount                 = iCount + 1
        ttReceipt.lineID       = iCount
        ttReceipt.company      = cocode
        ttReceipt.location     = temp-po-rec.whse
        ttReceipt.poID         = temp-po-rec.po-no
        ttReceipt.poLine       = temp-po-rec.po-line
        ttReceipt.itemID       = temp-po-rec.item-no
        ttReceipt.itemName     = IF temp-po-rec.descr EQ "" THEN temp-po-rec.item-no ELSE temp-po-rec.descr
        ttReceipt.quantityUOM  = temp-po-rec.qty-uom
        ttReceipt.quantity     = temp-po-rec.qty-to-inv
        .
END.

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
ASSIGN 
    cDisplay       = ""
    cTmpField      = ""
    cVarValue      = ""
    cExcelDisplay  = ""
    cExcelVarValue = "".

DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
    CASE cTmpField:             
        WHEN "vend"    THEN 
            cVarValue = "" .
        WHEN "act"   THEN 
            cVarValue = "".
        WHEN "po"   THEN 
            cVarValue = "".
        WHEN "date"  THEN 
            cVarValue = "" .
        WHEN "item"   THEN 
            cVarValue = "" .
        WHEN "desc"  THEN 
            cVarValue = "" .
        WHEN "cat"   THEN 
            cVarValue = "" .
        WHEN "inv-qty"  THEN 
            cVarValue = STRING(v-grand-tot-qty,"->>,>>>,>>9.9") .
        WHEN "whse"  THEN 
            cVarValue = "" .
        WHEN "cost"   THEN 
            cVarValue = "" .
        WHEN "inv-amt"  THEN 
            cVarValue = STRING(v-grand-tot-amt,"->>,>>>,>>9.99") .
        OTHERWISE 
        cVarValue = "".
    END CASE.

    cExcelVarValue = cVarValue.
    cDisplay = cDisplay + cVarValue +
        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
END.

PUT UNFORMATTED "     Grand Totals:" + SUBSTRING(cDisplay,19,300) SKIP(1).
IF rd-dest = 3 THEN 
DO:
    PUT STREAM excel UNFORMATTED  
        ' Grand Totals ,'
        SUBSTRING(cExcelDisplay,4,300) SKIP.
    OUTPUT STREAM excel CLOSE.
    
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

RUN pRunAPIOutboundTrigger.

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintDetail C-Win 
PROCEDURE pPrintDetail :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcVender AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoLine AS INTEGER NO-UNDO.
  
    DEFINE VARIABLE excelheader  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQuantityRec AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCostUom     AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-po-ord  FOR po-ord.
  
    FIND FIRST bf-po-ord NO-LOCK
        WHERE bf-po-ord.company EQ cocode
        AND bf-po-ord.po-no EQ ipiPoNo NO-ERROR.
       
    FIND FIRST bf-po-ordl NO-LOCK
        WHERE bf-po-ordl.company EQ cocode
        AND bf-po-ordl.po-no EQ ipiPoNo
        AND bf-po-ordl.LINE EQ ipiPoLine NO-ERROR.         
       
    IF bf-po-ordl.item-type  THEN
    DO:     
        FOR EACH rm-rcpth FIELDS(r-no rita-code i-no job-no job-no2 trans-date po-line pur-uom)
            WHERE rm-rcpth.company    EQ cocode
            AND rm-rcpth.i-no       EQ bf-po-ordl.i-no
            AND rm-rcpth.po-no      EQ string(bf-po-ordl.po-no) 
            AND rm-rcpth.po-line    EQ bf-po-ordl.LINE
            AND rm-rcpth.trans-date GE begin_rdate
            AND rm-rcpth.trans-date LE end_rdate
            AND rm-rcpth.rita-code  EQ "R"
            USE-INDEX item-po NO-LOCK,

            EACH rm-rdtlh FIELDS(loc loc-bin qty trans-date cost frt-cost)
            WHERE rm-rdtlh.r-no   EQ rm-rcpth.r-no               
            NO-LOCK:
             
            IF rm-rcpth.job-no NE "" THEN
                cJobNo =  rm-rcpth.job-no + "-" + STRING(rm-rcpth.job-no2,"99") .        
            ELSE 
                cJobNo = "".
               
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
      
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "rec-date"    THEN 
                        cVarValue = STRING(rm-rcpth.trans-date,"99/99/9999") .
                    WHEN "rec-job"   THEN 
                        cVarValue = STRING(cJobNo,"x(13)").
                    WHEN "rec-loc"   THEN 
                        cVarValue = STRING(rm-rdtlh.loc,"x(8)").
                    WHEN "rec-bin"   THEN 
                        cVarValue = STRING(rm-rdtlh.loc-bin,"x(8)").
                    WHEN "rec-qty"  THEN 
                        cVarValue = STRING(rm-rdtlh.qty,"->>>,>>>,>>9")  .
                    WHEN "rec-qty-uom"   THEN 
                        cVarValue = STRING(rm-rcpth.pur-uom,"x(10)") .
                    WHEN "rec-cost"  THEN 
                        cVarValue = STRING(rm-rdtlh.cost,"->>,>>>,>>9.99<<<<") .
                    WHEN "rec-cost-uom"   THEN 
                        cVarValue = STRING(rm-rcpth.pur-uom,"x(10)") .
                    WHEN "rec-ext-value"  THEN 
                        cVarValue = STRING(rm-rdtlh.qty * rm-rdtlh.cost,"->>,>>>,>>9.99<<<<") .
                    OTHERWISE 
                    cVarValue = "".
                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            IF cDisplay NE "" THEN PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest = 3 AND cExcelDisplay NE "" THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.                        
            END.
             
        END.
    END.
    ELSE 
    DO:
      
        FOR EACH fg-rcpth FIELDS(r-no rita-code i-no job-no job-no2 trans-date po-line pur-uom)
            WHERE fg-rcpth.company    EQ cocode
            AND fg-rcpth.i-no       EQ bf-po-ordl.i-no
            AND fg-rcpth.po-no      EQ string(bf-po-ordl.po-no)
            AND fg-rcpth.po-line    EQ bf-po-ordl.LINE
            AND fg-rcpth.rita-code  EQ "R"
            AND fg-rcpth.trans-date GE begin_rdate
            AND fg-rcpth.trans-date LE end_rdate
            USE-INDEX item-po NO-LOCK,
            FIRST fg-rdtlh FIELDS(loc loc-bin qty trans-date cost) WHERE
            fg-rdtlh.r-no EQ fg-rcpth.r-no AND
            fg-rdtlh.rita-code EQ fg-rcpth.rita-code                 
            NO-LOCK:
                  
            IF fg-rcpth.job-no NE "" THEN
                cJobNo =  fg-rcpth.job-no + "-" + STRING(fg-rcpth.job-no2,"999") .        
            ELSE 
                cJobNo = "".
                
            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ fg-rcpth.i-no
                USE-INDEX i-no NO-ERROR.
            cCostUom = IF AVAILABLE itemfg THEN itemfg.pur-uom ELSE "EA".      
                
            RUN Conv_QuantityFromUOMToUOM (
                INPUT  bf-po-ordl.company,
                INPUT  bf-po-ordl.i-no,
                INPUT  "FG",
                INPUT  fg-rdtlh.qty,
                INPUT  cCostUom, 
                INPUT  fg-rcpth.pur-uom,
                INPUT  0,
                INPUT  bf-po-ordl.s-len,
                INPUT  bf-po-ordl.s-wid,
                INPUT  bf-po-ordl.s-dep,
                INPUT  0,
                OUTPUT dQuantityRec,
                OUTPUT lError,
                OUTPUT cMessage
                ).                                                           
                    
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "rec-date"    THEN 
                        cVarValue = STRING(fg-rcpth.trans-date,"99/99/9999") .
                    WHEN "rec-job"   THEN 
                        cVarValue = STRING(cJobNo,"x(13)").
                    WHEN "rec-loc"   THEN 
                        cVarValue = STRING(fg-rdtlh.loc,"x(8)").
                    WHEN "rec-bin"   THEN 
                        cVarValue = STRING(fg-rdtlh.loc-bin,"x(8)").
                    WHEN "rec-qty"  THEN 
                        cVarValue = STRING(fg-rdtlh.qty,"->>>,>>>,>>9")  .
                    WHEN "rec-qty-uom"   THEN 
                        cVarValue = STRING(fg-rcpth.pur-uom,"x(10)") .
                    WHEN "rec-cost"  THEN 
                        cVarValue = STRING(fg-rdtlh.cost,"->>,>>>,>>9.99<<<<") .
                    WHEN "rec-cost-uom"   THEN 
                        cVarValue = STRING(fg-rcpth.pur-uom,"x(10)") .
                    WHEN "rec-ext-value"  THEN 
                        cVarValue = STRING(dQuantityRec * fg-rdtlh.cost,"->>,>>>,>>9.99<<<<") .
                    OTHERWISE 
                    cVarValue = "".
                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            IF cDisplay NE "" THEN PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest = 3 AND cExcelDisplay NE "" THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.                        
            END.                 
        END.        
    END.
  
    FOR EACH reftable
        {ap/ap-reftbW.i bf-po-ordl.po-no}
        NO-LOCK,
               each ap-inv WHERE
                    ap-inv.company eq cocode AND
                    ap-inv.i-no    eq int(reftable.code2) AND
                    ap-inv.vend-no eq bf-po-ord.vend-no AND
                    (ap-inv.po-no  eq bf-po-ordl.po-no or ap-inv.po-no eq 0) AND
                    ap-inv.posted  eq yes
                    use-index i-no no-lock,
               each ap-invl WHERE
                    ap-invl.i-no       eq ap-inv.i-no AND
                    (ap-invl.po-no     eq bf-po-ordl.po-no or ap-inv.po-no ne 0) AND
                    {ap/invlline.i -1} eq bf-po-ordl.LINE 
                    use-index i-no no-lock:

    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "".
                   
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:             
            WHEN "inv-no"    THEN 
                cVarValue = STRING(ap-inv.inv-no,"x(20)") .
            WHEN "inv-date"   THEN 
                cVarValue = STRING(ap-inv.inv-date,"99/99/9999").
            WHEN "inv-qty2"   THEN 
                cVarValue = STRING(ap-invl.qty,"->>>,>>9.99").
            WHEN "inv-qty-uom"   THEN 
                cVarValue = STRING(ap-invl.cons-uom,"x(10)").
            WHEN "inv-price"  THEN 
                cVarValue = STRING(ap-invl.unit-pr,"->,>>>,>>9.99<<<<")  .
            WHEN "inv-price-uom"   THEN 
                cVarValue = STRING(ap-invl.pr-qty-uom,"x(10)") .
            WHEN "inv-line-amount"  THEN 
                cVarValue = STRING(ap-invl.amt,"->>,>>>,>>9.99<<<<") .                                 
            OTHERWISE 
            cVarValue = "".
        END CASE.

        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT UNFORMATTED cDisplay SKIP.
    IF rd-dest = 3 THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            cExcelDisplay SKIP.                        
    END.  

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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS cha     NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS cha     NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha.

    ASSIGN
        lv-frame-hdl = FRAME {&frame-name}:HANDLE
        lv-group-hdl = lv-frame-hdl:FIRST-CHILD
        lv-field-hdl = lv-group-hdl:FIRST-CHILD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChangeDest C-Win
PROCEDURE pChangeDest :
    /*------------------------------------------------------------------------------
     Purpose:    
     Parameters:  <none>
     Notes:      
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN rd-dest.
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
            fi_file:SCREEN-VALUE = "c:\tmp\r-notvou.csv".
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
    RETURN STRING(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

