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
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No.        */     

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

{sys/ref/CustList.i NEW}

DEFINE VARIABLE v-program        AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form   AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-report-title  AS CHARACTER NO-UNDO.

DEFINE VARIABLE v-sort           AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-ordl           AS LOG       NO-UNDO.
DEFINE VARIABLE v-po             LIKE oe-relh.po-no EXTENT 2 INIT ["","zzzzzzzzzzzzzzz"].
DEFINE VARIABLE job-qty          AS INTEGER   NO-UNDO.
DEFINE VARIABLE open-job-qty     AS INTEGER   NO-UNDO.
DEFINE VARIABLE qtyOnHand        AS INTEGER   NO-UNDO.
DEFINE VARIABLE order-qty        AS INTEGER   NO-UNDO.
DEFINE VARIABLE wip-qty          AS INTEGER   NO-UNDO.
DEFINE VARIABLE shipped-qty      AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-ship-qty       AS INTEGER   NO-UNDO.
DEFINE VARIABLE produced-qty     AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-prod-qty       AS INTEGER   NO-UNDO.
DEFINE VARIABLE on-hand-qty      AS INTEGER   NO-UNDO.
DEFINE VARIABLE job-qty-rcvd     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lJob-open        AS LOG       NO-UNDO.
DEFINE VARIABLE qoh              AS INTEGER   NO-UNDO.
DEFINE VARIABLE qprod            AS INTEGER   NO-UNDO.
DEFINE VARIABLE li-qoh           AS INTEGER   NO-UNDO.
DEFINE VARIABLE glCustListActive AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD q-onh       LIKE itemfg.q-onh
    FIELD q-shp       LIKE itemfg.q-onh
    FIELD q-wip       LIKE itemfg.q-onh
    FIELD po-no       LIKE oe-ord.po-no
    FIELD inv         AS LOG
    FIELD inv-no      LIKE ar-invl.inv-no
    FIELD prod-qty    AS INTEGER 
    FIELD qty-to-prod AS INTEGER

    FIELD row-id      AS ROWID
    INDEX row-id row-id.

DEFINE TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin.

DEFINE VARIABLE lv-pdf-file AS CHARACTER NO-UNDO.

DEFINE STREAM excel.

DEFINE BUFFER b-oe-bolh FOR oe-bolh.
DEFINE BUFFER b-oe-boll FOR oe-boll.
DEFINE BUFFER b-oe-rell FOR oe-rell.
DEFINE BUFFER b-oe-rel  FOR oe-rel.
DEFINE BUFFER b-oe-ordl FOR oe-ordl.

{custom/xprint.i}

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cColumnInit        AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO .

ASSIGN 
    cTextListToSelect  = "Rep,Sales Rep Name,Cust #,Cust Name,Order PO,Item PO,Release PO," +
                           "Order#,Customer P/N,FG Item #,Item Name,Ord Date,Order Qty," +
                           "Inv/Rel Date,Inv#,Qty Produce,Qty To Produce," +
                           "Qty Shipped,Release Qty,Invoice Amt,Balance Due,Qty On-Hand"
    cFieldListToSelect = "sman,sname,cust,cname,ord-po,item-po,rel-po," +
                            "ord-no,cust-part,fgitem,i-name,ord-date,ord-qty," +
                            "invrel-dt,inv-no,qty-pro,qty-to-pro," +
                            "qty-shp,rel-qty,inv-amt,bal-due,qty-on-hand" 
    cFieldLength       = "3,25,8,30,15,15,15," +  "8,15,15,30,10,11," + "12,6,11,14," + "11,11,11,11,11" 
    cFieldType         = "c,c,c,c,c,c,c," + "c,c,c,c,c,i," + "c,c,i,i," + "i,i,i,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Rep,Sales Rep Name,Cust #,Cust Name,Order PO,Item PO,Release PO," +
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
btn_SelectColumns rd-dest td-show-parm fi_file tb_OpenCSV ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust-no end_cust-no ~
begin_ord-date end_ord-date begin_po-no end_po-no begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_i-no end_i-no begin_slmn ~
end_slmn lbl_sort rd_sort tb_break lbl_jstat rd_jstat lbl_ostat rd_ostat ~
fi_days-old tb_under as-of-date tb_job-qty tb_0-qoh tb_0-bal tb_sch rd-dest ~
td-show-parm fi_file tb_OpenCSV tbAutoClose 

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
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
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

DEFINE VARIABLE as-of-date     AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "As of" 
    VIEW-AS FILL-IN 
    SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE begin_cust-no  AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE begin_i-no     AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no   AS CHARACTER FORMAT "X(9)":U 
    LABEL "Beginning Job#" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2  AS CHARACTER FORMAT "-999":U INITIAL "000" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5.5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-date AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Order Date" 
    VIEW-AS FILL-IN 
    SIZE 21 BY .95 NO-UNDO.

DEFINE VARIABLE begin_po-no    AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Customer PO#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slmn     AS CHARACTER FORMAT "XXX" 
    LABEL "Beginning Sales Rep#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE end_cust-no    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE end_i-no       AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no     AS CHARACTER FORMAT "X(9)":U INITIAL "zzzzzz" 
    LABEL "Ending Job#" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2    AS CHARACTER FORMAT "-999":U INITIAL "999" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5.5 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-date   AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Order Date" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-no      AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Customer PO#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_slmn       AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
    LABEL "Ending Sales Rep#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE fi_days-old    AS INTEGER   FORMAT "->,>>>,>>9":U INITIAL 0 
    LABEL "Older Than" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\OrderBalance.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_jstat      AS CHARACTER FORMAT "X(256)":U INITIAL "Job Status?" 
    VIEW-AS FILL-IN 
    SIZE 13 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_ostat      AS CHARACTER FORMAT "X(256)":U INITIAL "Order Status?" 
    VIEW-AS FILL-IN 
    SIZE 15 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
    VIEW-AS FILL-IN 
    SIZE 10 BY .95 NO-UNDO.

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
    SIZE 20 BY 5.43 NO-UNDO.

DEFINE VARIABLE rd_jstat       AS CHARACTER INITIAL "All" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Open", "Open",
    "Closed", "Closed",
    "All", "All"
    SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE rd_ostat       AS CHARACTER INITIAL "All" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Open", "Open",
    "Closed", "Closed",
    "All", "All"
    SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "PO#" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "PO#", "PO#",
    "Item", "Item",
    "Cust Part#", "Cust Part#",
    "FG Item Name", "FG Item Name",
    "Order#", "Order#",
    "Due Date", "Due Date"
    SIZE 83 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 100 BY 5.95.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 100 BY 16.62.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 37 BY 4.76.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 4.52 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 4.52 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_0-bal     AS LOGICAL   INITIAL YES 
    LABEL "Include Zero Order Balance Items?" 
    VIEW-AS TOGGLE-BOX
    SIZE 39 BY 1.05 NO-UNDO.

DEFINE VARIABLE tb_0-qoh     AS LOGICAL   INITIAL YES 
    LABEL "Include Items with Zero QOH?" 
    VIEW-AS TOGGLE-BOX
    SIZE 33 BY 1.05 NO-UNDO.

DEFINE VARIABLE tb_break     AS LOGICAL   INITIAL NO 
    LABEL "Page Break by Sales Rep?" 
    VIEW-AS TOGGLE-BOX
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 30.8 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .95 NO-UNDO.

DEFINE VARIABLE tb_job-qty   AS LOGICAL   INITIAL NO 
    LABEL "Print Job Qty Details?" 
    VIEW-AS TOGGLE-BOX
    SIZE 25 BY .95 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_sch       AS LOGICAL   INITIAL NO 
    LABEL "Show Scheduled Releases?" 
    VIEW-AS TOGGLE-BOX
    SIZE 31 BY .95 NO-UNDO.

DEFINE VARIABLE tb_under     AS LOGICAL   INITIAL NO 
    LABEL "Drop Order Underrun%" 
    VIEW-AS TOGGLE-BOX
    SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tb_cust-list AT ROW 1.71 COL 31.6 WIDGET-ID 6
    btnCustList AT ROW 1.81 COL 65.2 WIDGET-ID 8
    begin_cust-no AT ROW 2.76 COL 29.2 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust-no AT ROW 2.76 COL 77.2 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_ord-date AT ROW 3.71 COL 29.2 COLON-ALIGNED
    end_ord-date AT ROW 3.71 COL 77.2 COLON-ALIGNED HELP
    "Enter Ending Due Date"
    begin_po-no AT ROW 4.67 COL 29.2 COLON-ALIGNED HELP
    "Enter Ending Customer PO Number"
    end_po-no AT ROW 4.67 COL 77.2 COLON-ALIGNED HELP
    "Enter Ending Customer PO Number"
    begin_job-no AT ROW 5.62 COL 29.2 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    begin_job-no2 AT ROW 5.62 COL 44.6 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    end_job-no AT ROW 5.62 COL 77.2 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    end_job-no2 AT ROW 5.62 COL 92.6 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    begin_i-no AT ROW 6.57 COL 29.2 COLON-ALIGNED HELP
    "Enter Beginning Item Number"
    end_i-no AT ROW 6.57 COL 77.2 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    begin_slmn AT ROW 7.52 COL 29.2 COLON-ALIGNED HELP
    "Enter Beginning Sales Rep Number"
    end_slmn AT ROW 7.52 COL 77.2 COLON-ALIGNED HELP
    "Enter Ending Sales Rep Number"
    lbl_sort AT ROW 8.76 COL 8 COLON-ALIGNED NO-LABELS
    rd_sort AT ROW 8.76 COL 20 NO-LABELS
    tb_break AT ROW 10.05 COL 63.2
    lbl_jstat AT ROW 10.14 COL 5 COLON-ALIGNED NO-LABELS
    rd_jstat AT ROW 10.14 COL 20 NO-LABELS
    lbl_ostat AT ROW 11.43 COL 3 COLON-ALIGNED NO-LABELS
    rd_ostat AT ROW 11.43 COL 20 NO-LABELS
    fi_days-old AT ROW 12.29 COL 78.2 COLON-ALIGNED
    tb_under AT ROW 12.91 COL 6
    as-of-date AT ROW 13.48 COL 73.2 COLON-ALIGNED
    tb_job-qty AT ROW 13.95 COL 6
    tb_0-qoh AT ROW 14.67 COL 65.2
    tb_0-bal AT ROW 15 COL 6
    tb_sch AT ROW 16.05 COL 6 WIDGET-ID 2
    btn_SelectColumns AT ROW 16.14 COL 61 WIDGET-ID 10
    sl_selected AT ROW 16.95 COL 4 NO-LABELS WIDGET-ID 28
    sl_avail AT ROW 16.95 COL 4 NO-LABELS WIDGET-ID 26
    lines-per-page AT ROW 19 COL 91 COLON-ALIGNED
    lv-ornt AT ROW 19.14 COL 38 NO-LABELS
    rd-dest AT ROW 19.19 COL 6 NO-LABELS
    lv-font-no AT ROW 19.81 COL 41 COLON-ALIGNED
    tb_excel AT ROW 19.81 COL 84 RIGHT-ALIGNED
    lv-font-name AT ROW 20.76 COL 35 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 22.48 COL 38.6
    fi_file AT ROW 23.43 COL 36.6 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 23.52 COL 102.4 RIGHT-ALIGNED
    btn_down AT ROW 24.81 COL 5 WIDGET-ID 42
    btn_Up AT ROW 24.81 COL 5 WIDGET-ID 40
    Btn_Remove AT ROW 24.81 COL 5 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    Btn_Add AT ROW 24.81 COL 5 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    tbAutoClose AT ROW 24.81 COL 38.6 WIDGET-ID 60
    btn-ok AT ROW 25.76 COL 38.6
    btn-cancel AT ROW 25.76 COL 58.6
    "Days" VIEW-AS TEXT
    SIZE 6 BY 1 AT ROW 12.29 COL 90
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 18.52 COL 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 106 BY 26.86
    BGCOLOR 15 .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
    "Only Show QOH that is..." VIEW-AS TEXT
    SIZE 26 BY .62 AT ROW 11.33 COL 65.2
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.24 COL 5
    RECT-6 AT ROW 18.86 COL 4
    RECT-7 AT ROW 1.52 COL 4
    RECT-8 AT ROW 11.1 COL 63.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 106 BY 26.86
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
        TITLE              = "Order Balance by PO# / Job"
        HEIGHT             = 26.86
        WIDTH              = 105.8
        MAX-HEIGHT         = 47.91
        MAX-WIDTH          = 256
        VIRTUAL-HEIGHT     = 47.91
        VIRTUAL-WIDTH      = 256
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
ASSIGN 
    as-of-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ord-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_po-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_slmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    Btn_Add:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR BUTTON btn_down IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    btn_down:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR BUTTON Btn_Remove IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    Btn_Remove:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR BUTTON btn_Up IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    btn_Up:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_ord-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_po-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_days-old:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_jstat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_jstat:PRIVATE-DATA IN FRAME FRAME-A = "rd_jstat".

/* SETTINGS FOR FILL-IN lbl_ostat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_ostat:PRIVATE-DATA IN FRAME FRAME-A = "rd_ostat".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

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

ASSIGN 
    rd_jstat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_ostat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR SELECTION-LIST sl_avail IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    sl_avail:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_selected IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    sl_selected:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    tb_0-bal:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_0-qoh:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_break:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_job-qty:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_sch:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_under:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Order Balance by PO# / Job */
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
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
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


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-date C-Win
ON LEAVE OF begin_ord-date IN FRAME FRAME-A /* Beginning Order Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-no C-Win
ON LEAVE OF begin_po-no IN FRAME FRAME-A /* Beginning Customer PO# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slmn C-Win
ON LEAVE OF begin_slmn IN FRAME FRAME-A /* Beginning Sales Rep# */
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
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.
        ASSIGN
            lv-pdf-file    = init-dir + "\OrderBal"
            is-xprint-form = NO.

        RUN GetSelectionList.
        FIND FIRST  ttCustList NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttCustList AND tb_cust-list THEN 
        DO:
            EMPTY TEMP-TABLE ttCustList.
            RUN BuildCustList(INPUT cocode,
                INPUT tb_cust-list AND glCustListActive,
                INPUT begin_cust-no,
                INPUT END_cust-no).
        END.
        RUN run-report.
        STATUS DEFAULT "Processing Complete". 

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
                    {custom/asifax.i &type= "Customer"
                            &begin_cust=begin_cust-no
                            &END_cust= begin_cust-no
                            &fax-subject=lv-report-title
                            &fax-body=lv-report-title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    is-xprint-form = YES.
                    /*IF is-xprint-form THEN DO:
                       {custom/asimail.i &TYPE = "Customer"
                                      &begin_cust= begin_cust-no
                                      &END_cust=begin_cust-no
                                      &mail-subject=lv-report-title
                                      &mail-body=lv-report-title
                                      &mail-file=list-name }
                                      */
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail2.i &TYPE="CUSTOMER"
                             &group-title=v-prgmname
                             &begin_cust=begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject=lv-report-title
                             &mail-body=lv-report-title
                             &mail-file=lv-pdf-file + ".pdf" }

                    END.
                    ELSE 
                    DO:
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
        END CASE.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
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
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.
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
        APPLY "DEFAULT-ACTION" TO sl_selected  .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_SelectColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_SelectColumns C-Win
ON CHOOSE OF btn_SelectColumns IN FRAME FRAME-A /* Select Columns */
    DO:
        DEFINE VARIABLE cTextSelected AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTextListed   AS CHARACTER NO-UNDO.

        RUN displaySelectionList2.

        ASSIGN 
            cTextSelected = sl_selected:LIST-ITEMS
            cTextListed   = sl_avail:LIST-ITEMS.

        IF NOT cColumnInit THEN RUN custom/d-rptsel.w (INPUT-OUTPUT cTextListed, INPUT-OUTPUT cTextSelected, INPUT-OUTPUT cTextListToDefault, INPUT-OUTPUT cTextListToSelect).

        ASSIGN 
            sl_selected:LIST-ITEMS = cTextSelected
            sl_avail:LIST-ITEMS    = cTextListed.

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


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-date C-Win
ON LEAVE OF end_ord-date IN FRAME FRAME-A /* Ending Order Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-no C-Win
ON LEAVE OF end_po-no IN FRAME FRAME-A /* Ending Customer PO# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slmn C-Win
ON LEAVE OF end_slmn IN FRAME FRAME-A /* Ending Sales Rep# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* Name */
    DO:
        DEFINE VARIABLE ls-filename AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG       NO-UNDO.

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

        ASSIGN rd-dest lv-ornt.
        IF rd-dest = 5 THEN 
        DO:
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
        ASSIGN {&self-name}.
        RUN pChangeDest.
        IF rd-dest = 5 THEN 
        DO:
            IF lv-ornt:SCREEN-VALUE BEGINS "p" THEN lines-per-page:SCREEN-VALUE = "60".
            ELSE lines-per-page:SCREEN-VALUE = "45".     
        END.
        ELSE 
        DO:
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
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_ostat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_ostat C-Win
ON VALUE-CHANGED OF rd_ostat IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME tb_0-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_0-bal C-Win
ON VALUE-CHANGED OF tb_0-bal IN FRAME FRAME-A /* Include Zero Order Balance Items? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_0-qoh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_0-qoh C-Win
ON VALUE-CHANGED OF tb_0-qoh IN FRAME FRAME-A /* Include Items with Zero QOH? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_break
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_break C-Win
ON VALUE-CHANGED OF tb_break IN FRAME FRAME-A /* Page Break by Sales Rep? */
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_job-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_job-qty C-Win
ON VALUE-CHANGED OF tb_job-qty IN FRAME FRAME-A /* Print Job Qty Details? */
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


&Scoped-define SELF-NAME tb_sch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sch C-Win
ON VALUE-CHANGED OF tb_sch IN FRAME FRAME-A /* Show Scheduled Releases? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_under
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_under C-Win
ON VALUE-CHANGED OF tb_under IN FRAME FRAME-A /* Drop Order Underrun% */
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

    ASSIGN
        begin_ord-date = TODAY
        as-of-date     = TODAY.

    FIND FIRST users WHERE
        users.user_id EQ USERID("NOSWEAT")
        NO-LOCK NO-ERROR.

    IF AVAILABLE users AND users.user_program[2] NE "" THEN
        init-dir = users.user_program[2].
    ELSE
        init-dir = "c:\tmp".

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "OZ9" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-tt C-Win 
PROCEDURE build-tt :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-date AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.

    DEFINE BUFFER b-ar-invl  FOR ar-invl.
    DEFINE BUFFER b-inv-head FOR inv-head.
    DEFINE BUFFER b-inv-line FOR inv-line.

    DEFINE VARIABLE v-po-no   LIKE oe-ord.po-no NO-UNDO.
    DEFINE VARIABLE v-job     LIKE oe-ord.job-no EXTENT 2 INIT ["","zzzzzzzzz"].
    DEFINE VARIABLE v-job2    LIKE oe-ord.job-no2 FORMAT "999" EXTENT 2 INIT [0,999].
    DEFINE VARIABLE temp-job1 AS CHARACTER NO-UNDO.
    temp-job1 = TRIM(END_job-no).
    IF temp-job1 = "" THEN
        temp-job1 = "zzzzzzzzz".
    ASSIGN
        v-job[1] = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', begin_job-no, begin_job-no2))
        v-job[2] = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', temp-job1, end_job-no2)).

    /* IF rd_prt-po EQ "Line" THEN
        v-po-no = oe-ordl.po-no.
     ELSE IF rd_prt-po EQ "Header" THEN
        v-po-no = oe-ord.po-no.
     ELSE
     DO: */
    IF AVAILABLE oe-rell THEN
        v-po-no = oe-rell.po-no.
    ELSE
    DO: 
        IF AVAILABLE ar-invl THEN 
        DO:
            FIND FIRST b-oe-bolh
                WHERE b-oe-bolh.company EQ cocode
                AND b-oe-bolh.bol-no  EQ ar-invl.bol-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE b-oe-bolh THEN 
            DO:
                FIND FIRST b-oe-boll
                    WHERE b-oe-boll.company EQ cocode
                    AND b-oe-boll.b-no    EQ b-oe-bolh.b-no
                    AND b-oe-boll.i-no    EQ ar-invl.i-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE b-oe-boll THEN 
                DO:
                    FIND FIRST b-oe-rell
                        WHERE b-oe-rell.company EQ cocode
                        AND b-oe-rell.r-no    EQ b-oe-boll.r-no
                        AND b-oe-rell.ord-no  EQ b-oe-boll.ord-no
                        AND b-oe-rell.i-no    EQ b-oe-boll.i-no
                        AND b-oe-rell.line    EQ b-oe-boll.line
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE b-oe-rell THEN 
                        v-po-no = b-oe-rell.po-no. 
                END.
            END.
        END.
        ELSE IF AVAILABLE inv-line THEN 
            DO:
                FIND FIRST b-oe-bolh
                    WHERE b-oe-bolh.company EQ cocode
                    AND b-oe-bolh.b-no    EQ inv-line.b-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE b-oe-bolh THEN 
                DO:
                    FIND FIRST b-oe-boll
                        WHERE b-oe-boll.company EQ cocode
                        AND b-oe-boll.b-no    EQ b-oe-bolh.b-no
                        AND b-oe-boll.i-no    EQ inv-line.i-no
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE b-oe-boll THEN 
                    DO:
                        FIND FIRST b-oe-rell
                            WHERE b-oe-rell.company EQ cocode
                            AND b-oe-rell.r-no    EQ b-oe-boll.r-no
                            AND b-oe-rell.i-no    EQ b-oe-boll.i-no
                            AND b-oe-rell.line    EQ b-oe-boll.line
                            USE-INDEX r-no NO-LOCK NO-ERROR.
                        IF AVAILABLE b-oe-rell THEN 
                            v-po-no = b-oe-rell.po-no.
                    END.
                END.
            END.
            ELSE IF AVAILABLE oe-rel THEN
                    v-po-no = oe-rel.po-no.

                ELSE IF AVAILABLE oe-ordl THEN
                    DO:
                        FIND FIRST b-oe-rel WHERE
                            b-oe-rel.company EQ cocode AND
                            b-oe-rel.ord-no EQ oe-ordl.ord-no
                            NO-LOCK NO-ERROR.

                        IF AVAILABLE b-oe-rel THEN
                            v-po-no = b-oe-rel.po-no.
                    END.
                    ELSE v-po-no = "".

        IF tb_sch THEN 
        DO:
            IF AVAILABLE oe-rel THEN
                v-po-no = oe-rel.po-no.
            ELSE v-po-no = "".
        END.

        IF NOT(v-po-no GE v-po[1] AND
            v-po-no LE v-po[2]) THEN NEXT.
    END.
    /* END.*/

    FIND FIRST job-hdr
        WHERE job-hdr.company EQ cocode
        AND job-hdr.ord-no  EQ oe-ordl.ord-no
        AND job-hdr.i-no    EQ oe-ordl.i-no
        AND (FILL(" ",9 - length(TRIM(job-hdr.job-no))) +
        trim(job-hdr.job-no) + string(job-hdr.job-no2,"999") GE v-job[1]
        AND fill(" ",9 - length(TRIM(job-hdr.job-no))) +
        trim(job-hdr.job-no) + string(job-hdr.job-no2,"999") LE v-job[2])
        AND job-hdr.job-no2 GE int(begin_job-no2)
        AND job-hdr.job-no2 LE int(end_job-no2)
        USE-INDEX opened NO-LOCK NO-ERROR.


    /*
    IF NOT AVAIL job-hdr THEN DO:
        FIND FIRST job-hdr
               where job-hdr.company eq cocode
                 and job-hdr.ord-no  EQ oe-ordl.ord-no
                 AND job-hdr.i-no    EQ oe-ordl.i-no NO-LOCK NO-ERROR.
        MESSAGE "here2" AVAIL(job-hdr) oe-ordl.ord-no oe-ordl.i-no VIEW-AS ALERT-BOX.
    
    END.
    */
    IF NOT AVAILABLE job-hdr THEN
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
        tt-report.key-04  = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', job-hdr.job-no, job-hdr.job-no2))
        tt-report.key-05  = STRING(oe-ord.ord-no,"99999999999")
        tt-report.key-06  = oe-ordl.i-no
        tt-report.key-07  = STRING(YEAR(ip-date),"9999") +
                       STRING(MONTH(ip-date),"99")  +
                       STRING(DAY(ip-date),"99")
        tt-report.po-no   = v-po-no
        tt-report.rec-id  = ip-recid
        tt-report.row-id  = ROWID(oe-ordl)
        v-ordl            = NO
        .

    FIND b-ar-invl WHERE RECID(b-ar-invl) EQ ip-recid NO-LOCK NO-ERROR.
    IF AVAILABLE b-ar-invl THEN
        ASSIGN
            tt-report.q-shp  = b-ar-invl.ship-qty
            tt-report.inv    = YES
            tt-report.inv-no = b-ar-invl.inv-no.

    FIND b-inv-line WHERE RECID(b-inv-line) EQ ip-recid NO-LOCK NO-ERROR.
    IF AVAILABLE b-inv-line THEN 
    DO:
        FIND FIRST b-inv-head WHERE b-inv-head.r-no EQ b-inv-line.r-no NO-LOCK.
        ASSIGN
            tt-report.q-shp  = b-inv-line.ship-qty
            tt-report.inv    = YES
            tt-report.inv-no = b-inv-head.inv-no.
    END.

    DEFINE VARIABLE li AS INTEGER NO-UNDO.

    IF AVAILABLE oe-ordl THEN
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

    ASSIGN  
        tt-report.prod-qty = li .

    IF NOT CAN-FIND(FIRST tt-fg-bin WHERE tt-fg-bin.i-no EQ tt-report.key-06) THEN RUN calc-qoh.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
            INPUT 'OZ9',
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-qoh C-Win 
PROCEDURE calc-qoh :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE vdat     AS DATE.
    DEFINE VARIABLE v-curr   AS LOG.
    DEFINE VARIABLE v-q-or-v AS LOG.

    DEFINE VARIABLE v-qohj   AS DECIMAL EXTENT 6.
    DEFINE VARIABLE v-qohi   LIKE v-qohj.
    DEFINE VARIABLE v-qty    AS INTEGER.
    DEFINE VARIABLE v-qty1   LIKE v-qty.
    DEFINE VARIABLE v-qtyc   LIKE v-qty.
    DEFINE VARIABLE v-red    LIKE v-qty.
    DEFINE VARIABLE v        AS INTEGER.
    DEFINE VARIABLE v-val    AS DECIMAL EXTENT 4.
    DEFINE VARIABLE v-cst    AS DECIMAL EXTENT 4.
    DEFINE VARIABLE v-u-val  AS DECIMAL.
    DEFINE VARIABLE v-u-cst  AS DECIMAL.
    DEFINE VARIABLE v-date   AS DATE.
    DEFINE VARIABLE lv-tag   LIKE fg-rdtlh.tag NO-UNDO.
    DEFINE VARIABLE ld-last  AS DATE    NO-UNDO.

    DEFINE BUFFER b-f-rc FOR fg-rcpth.
    DEFINE BUFFER b-f-rd FOR fg-rdtlh.


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

        IF fi_days-old NE 0 THEN 
        DO:
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

                IF LAST-OF(fg-bin.tag) THEN 
                DO:
                    v-qtyc = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                        v-qohj[4] + v-qohj[5] + v-qohj[6].

                    IF v-qohj[6] LT 0 THEN 
                    DO:
                        v-qty = v-qohj[6] * -1.

                        DO v = 5 TO 1 BY -1:
                            IF v-qohj[v] GT 0 THEN
                                ASSIGN
                                    v-red     = min(v-qty,v-qohj[v])
                                    v-qohj[v] = v-qohj[v] - v-red
                                    v-qty     = v-qty     - v-red.

                            IF v-qty LE 0 THEN LEAVE.
                        END.

                        IF v-qty GT 0 THEN v-qohi[6] = v-qohi[6] - v-qty.
                    END.

                    /*release oe-ordl.*/ 
                    IF fg-bin.job-no NE "" THEN
                        FIND LAST b-oe-ordl
                            WHERE b-oe-ordl.company EQ cocode
                            AND b-oe-ordl.job-no  EQ fg-bin.job-no
                            AND b-oe-ordl.job-no2 EQ fg-bin.job-no2
                            AND b-oe-ordl.i-no    EQ fg-rcpth.i-no
                            USE-INDEX job NO-LOCK NO-ERROR.

                    IF NOT v-curr THEN
                        ASSIGN
                            v-qohj[1] = 0
                            v-qohj[2] = 0
                            v-qohj[3] = 0.

                    ASSIGN
                        v-qty     = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                       v-qohj[4] + v-qohj[5]
                        v-qohi[1] = v-qohi[1] + v-qohj[1]
                        v-qohi[2] = v-qohi[2] + v-qohj[2]
                        v-qohi[3] = v-qohi[3] + v-qohj[3]
                        v-qohi[4] = v-qohi[4] + v-qohj[4]
                        v-qohi[5] = v-qohi[5] + v-qohj[5]
                        v-qohj    = 0.

                    IF AVAILABLE b-oe-ordl THEN
                        ASSIGN
                            v-u-cst = b-oe-ordl.t-cost / b-oe-ordl.qty
                            v-u-val = b-oe-ordl.t-price / b-oe-ordl.qty.

                    ELSE 
                    DO:
                        IF itemfg.prod-uom EQ "EA" THEN
                            v-u-cst = itemfg.total-std-cost.
                        ELSE
                            RUN sys/ref/convcuom.p(itemfg.prod-uom, "EA", 0, 0, 0, 0,
                                itemfg.total-std-cost, OUTPUT v-u-cst).

                        IF itemfg.sell-uom EQ "EA" THEN
                            v-u-val = itemfg.sell-price.
                        ELSE
                            RUN sys/ref/convcuom.p(itemfg.sell-uom, "EA", 0, 0, 0, 0,
                                itemfg.sell-price, OUTPUT v-u-val).
                    END.

                    IF v-u-cst EQ ? THEN v-u-cst = 0.
                    IF v-u-val EQ ? THEN v-u-val = 0.

                    ASSIGN
                        v-cst[1] = v-cst[1] + (v-qty * v-u-cst)
                        v-val[1] = v-val[1] + (v-qty * v-u-val).
                END.

                IF LAST-OF(fg-bin.i-no) THEN 
                DO:
                    IF v-qohi[6] LT 0 THEN 
                    DO:
                        v-qty = v-qohi[6] * -1.

                        DO v = 5 TO 1 BY -1:
                            IF v-qohi[v] GT 0 THEN
                                ASSIGN
                                    v-red     = min(v-qty,v-qohi[v])
                                    v-qohi[v] = v-qohi[v] - v-red
                                    v-qty     = v-qty     - v-red.

                            IF v-qty LE 0 THEN LEAVE.
                        END.

                        IF v-qty GT 0 THEN
                            ASSIGN
                                v-qohi   = 0
                                v-cst[1] = 0
                                v-val[1] = 0.
                    END.

                    IF v-cst[1] LT 0 THEN v-cst[1] = 0.
                    IF v-val[1] LT 0 THEN v-val[1] = 0.

                    IF NOT v-q-or-v THEN 
                    DO:
                        v-qty = v-qohi[1] + v-qohi[2] + v-qohi[3] + v-qohi[4] + v-qohi[5].

                        DO v = 1 TO 5:
                            v-qohi[v] = v-val[1] / v-qty * v-qohi[v].

                            IF v-qohi[v] EQ ? THEN v-qohi[v] = 0.
                        END.
                    END.

                    tt-fg-bin.qty = v-qohi[2] + v-qohi[3] + v-qohi[4] + v-qohi[5].
                END.
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
        INPUT 'OZ9').


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

    /*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
    /*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
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
    DISPLAY tb_cust-list begin_cust-no end_cust-no begin_ord-date end_ord-date 
        begin_po-no end_po-no begin_job-no begin_job-no2 end_job-no 
        end_job-no2 begin_i-no end_i-no begin_slmn end_slmn lbl_sort rd_sort 
        tb_break lbl_jstat rd_jstat lbl_ostat rd_ostat fi_days-old tb_under 
        as-of-date tb_job-qty tb_0-qoh tb_0-bal tb_sch rd-dest td-show-parm 
        fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 RECT-8 tb_cust-list btnCustList begin_cust-no 
        end_cust-no begin_ord-date end_ord-date begin_po-no end_po-no 
        begin_job-no begin_job-no2 end_job-no end_job-no2 begin_i-no end_i-no 
        begin_slmn end_slmn rd_sort tb_break rd_jstat rd_ostat fi_days-old 
        tb_under as-of-date tb_job-qty tb_0-qoh tb_0-bal tb_sch 
        btn_SelectColumns rd-dest td-show-parm fi_file tb_OpenCSV 
        tbAutoClose btn-ok btn-cancel 
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
    DEFINE INPUT PARAMETER ipVarName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opVarValue AS CHARACTER NO-UNDO.

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
    RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------ oe/rep/backlog.p 10/94 gb */
    /* Order Backlog Summary / Detail Report                                      */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-top3w.f "Hot Keys O-Z-9"}*/

    DEFINE BUFFER b-tt-report FOR tt-report.
    DEFINE BUFFER b-oe-rell   FOR oe-rell.

    DEFINE VARIABLE v-cust         LIKE oe-ord.cust-no EXTENT 2 INIT ["","zzzzzzzz"].
    DEFINE VARIABLE v-date         LIKE ar-inv.inv-date FORMAT "99/99/9999"
        EXTENT 2 INIT [TODAY, 12/31/9999].
    DEFINE VARIABLE v-job          LIKE oe-ord.job-no EXTENT 2 INIT ["","zzzzzzzzz"].
    DEFINE VARIABLE v-job2         LIKE oe-ord.job-no2 FORMAT "999" EXTENT 2 INIT [0,999].
    DEFINE VARIABLE v-item         LIKE oe-ordl.i-no EXTENT 2 INIT ["","zzzzzzzzzzzzzzz"].
    DEFINE VARIABLE v-inc          AS LOG       FORMAT "Yes/No" INIT YES.
    DEFINE VARIABLE v-stat         AS CHARACTER FORMAT "!" INIT "A".
    DEFINE VARIABLE v-ostat        AS CHARACTER FORMAT "!" INIT "A".
    DEFINE VARIABLE v-bal          AS LOG       FORMAT "BalDue/InvAmt" INIT YES.
    DEFINE VARIABLE v-jobq         AS LOG       FORMAT "Yes/No" INIT NO.

    DEFINE VARIABLE v-dat          AS DATE      FORMAT "99/99/99".
    DEFINE VARIABLE v-bal-qty      AS INTEGER   FORMAT ">>>,>>>,>>9".
    DEFINE VARIABLE v-inv-amt      AS INTEGER   FORMAT ">>>>,>>9.99".
    DEFINE VARIABLE v-cust-no      LIKE cust.cust-no.
    DEFINE VARIABLE v-name         LIKE cust.name.
    DEFINE VARIABLE v-sman         LIKE sman.sman.
    DEFINE VARIABLE v-sname        LIKE sman.sname.
    DEFINE VARIABLE v-field1       AS CHARACTER FORMAT "x(52)".
    DEFINE VARIABLE v-label        AS CHARACTER FORMAT "x(11)" INIT "Invoice Amt".
    DEFINE VARIABLE v-field2       LIKE v-label.
    DEFINE VARIABLE v-ord-no       AS CHARACTER.
    DEFINE VARIABLE v-q-onh        LIKE itemfg.q-onh NO-UNDO.
    DEFINE VARIABLE v-q-shp        LIKE v-q-onh NO-UNDO.
    DEFINE VARIABLE v-q-rel        LIKE v-q-onh NO-UNDO.
    DEFINE VARIABLE v-q-wip        LIKE v-q-onh NO-UNDO.
    DEFINE VARIABLE v-q-avl        LIKE v-q-onh NO-UNDO.
    DEFINE VARIABLE begin_due-date AS DATE      NO-UNDO.
    DEFINE VARIABLE end_due-date   AS DATE      NO-UNDO.
    DEFINE VARIABLE rd_due-date    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-stat        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-due-date    LIKE oe-ordl.req-date NO-UNDO.
    DEFINE VARIABLE act-rel-qty    AS INTEGER   NO-UNDO.

    DEFINE VARIABLE v-time         AS INTEGER.
    v-time = TIME.
    DEFINE VARIABLE cSelectedList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDisplay       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-tit4       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line       AS cha       FORM "x(300)" NO-UNDO.
    DEFINE VARIABLE lSelected      AS LOGICAL   INIT YES NO-UNDO.

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.

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
        "-------------"
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
        str-tit2  = c-win:TITLE 
        str-tit3  = "By Customer" 
        {sys/inc/ctrtext.i str-tit2 112}
        {sys/inc/ctrtext.i str-tit3 132}

        v-cust[1] = begin_cust-no
        v-cust[2] = end_cust-no
        v-date[1] = begin_ord-date
        v-date[2] = end_ord-date
        v-po[1]   = begin_po-no
        v-po[2]   = end_po-no
        v-job[1]  = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', begin_job-no, begin_job-no2))
        v-job[2]  = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', end_job-no, end_job-no2))
        v-item[1] = begin_i-no
        v-item[2] = end_i-no
        v-sort    = substr(rd_sort,1,1)
        v-inc     = tb_0-bal
        v-stat    = substr(rd_jstat,1,1)
        v-ostat   = substr(rd_ostat,1,1)
        v-jobq    = tb_job-qty
        /*v-bal      = rd_prt-baldue eq "Balance Due"*/ 
        lSelected = tb_cust-list .

    IF v-bal THEN v-label = "Balance Due".
    IF v-item[2] = "" THEN
        v-item[2] = "zzzzzzzzzzzzz".

    IF lselected THEN 
    DO:
        FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
        IF AVAILABLE ttCustList THEN ASSIGN v-cust[1] = ttCustList.cust-no .
        FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
        IF AVAILABLE ttCustList THEN ASSIGN v-cust[2] = ttCustList.cust-no .
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(0)}


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

        IF LOOKUP(ttRptSelected.TextList, "") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.


    IF rd-dest = 5 THEN 
    DO:
        IF lv-ornt = "L" THEN PUT "<OLANDSCAPE><PREVIEW>".                   /*<p7><CPI20>*/ 
        ELSE PUT "<PREVIEW>".
        PUT "<PDF-EXCLUDE=MS Mincho></PROGRESS><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><P7>" FORM "x(150)" SKIP.
    END. 
    OUTPUT CLOSE.
        {sys/inc/outprint.i "value(lines-per-page) append" }                                     /**/

    VIEW FRAME r-top.

    IF td-show-parm THEN RUN show-param.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        /* IF tb_break THEN
            excelheader = "Sales Rep ID,Sales Rep Name,".
         /* wfk - took out job# after order # */
         excelheader = excelheader + "Cust #,Cust Name,PO Number,Order#,Customer P/N,"
                      + "FG Item #,Item Name,Ord Date,Order Qty,Inv/Rel Date,Inv#,Qty Produce,Qty To Produce,"
                      + "Qty Shipped,Release Qty," + v-label + ",Qty On-Hand". */

        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    /*IF tb_break THEN VIEW FRAME r-top1.*/

    VIEW FRAME r-top.

    EMPTY TEMP-TABLE tt-report.
    EMPTY TEMP-TABLE tt-fg-bin.

    {oerep/r-ordbjN.i}

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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
                tb_excel                = YES
                .
        ELSE
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                fi_file:SENSITIVE       = NO
                tb_OpenCSV:SENSITIVE    = NO
                tb_excel                = NO
                .
        ASSIGN 
            fi_file:SCREEN-VALUE = "c:\tmp\OrderBalance.csv".    
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
    DEFINE VARIABLE iTotalJobOnHandQty AS INTEGER NO-UNDO.
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

    DEFINE BUFFER b-oe-ordl FOR oe-ordl.


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
    RETURN STRING(hipField:BUFFER-VALUE).

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

    IF AVAILABLE job-hdr /* AND AVAILABLE job AND job.opened = TRUE */ THEN 
    DO:
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

    IF AVAILABLE job-hdr /*AND AVAILABLE job AND job.opened = TRUE */ THEN 
    DO:

        FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.i-no EQ job-hdr.i-no
            AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
        IF AVAILABLE oe-ordl THEN
        DO:
            IF oe-ordl.job-no NE '' THEN
                FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
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
    DEFINE VARIABLE rtnValue    AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-inv-qty  LIKE oe-ordl.inv-qty NO-UNDO.
    DEFINE VARIABLE li-ship-qty LIKE oe-ordl.ship-qty NO-UNDO.

    IF AVAILABLE job-hdr /* AND AVAILABLE job AND job.opened = TRUE */ THEN 
    DO:
        FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.i-no EQ job-hdr.i-no
            AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
        IF AVAILABLE oe-ordl THEN 
        DO:
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

    IF AVAILABLE job-hdr /* AND AVAILABLE job AND job.opened = TRUE */ THEN 
    DO:
        FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.i-no EQ job-hdr.i-no
            AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
        IF AVAILABLE oe-ordl THEN 
        DO:
            FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
            rtnValue = oe-ordl.qty - (qtyOnHand + oe-ordl.ship-qty).
            IF rtnValue LT 0 OR
                rtnValue LT oe-ordl.qty *
                (IF AVAILABLE oe-ord THEN oe-ordl.under-pct 
            ELSE 100) / 100 THEN
                rtnValue = 0.
        END. /* avail oe-ordl */
    END. /* avail job-hdr */
    RETURN rtnValue.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

