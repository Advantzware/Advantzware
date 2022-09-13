&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-hotsOp.w

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

{sys/inc/var.i NEW SHARED}

ASSIGN
    cocode = gcompany
    locode = gloc.

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL   NO-UNDO.

DEFINE VARIABLE v-program        AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-fax-file      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-report-title  AS CHARACTER NO-UNDO.

DEFINE VARIABLE v-sort           AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-ordl           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-q-onh          LIKE itemfg.q-onh NO-UNDO.
DEFINE VARIABLE lv-stat          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-due-date      LIKE oe-ordl.req-date NO-UNDO.
DEFINE VARIABLE v-set-count      AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE tt-report LIKE report
    FIELD q-onh        LIKE itemfg.q-onh
    FIELD q-shp        LIKE itemfg.q-onh
    FIELD q-rel        LIKE itemfg.q-onh
    FIELD q-wip        LIKE itemfg.q-onh
    FIELD q-avl        LIKE itemfg.q-onh
    FIELD po-no        LIKE oe-ord.po-no
    FIELD inv          AS LOGICAL
    FIELD inv-no       LIKE ar-invl.inv-no
    FIELD cad-no       LIKE itemfg.cad-no
    FIELD row-id       AS ROWID
    FIELD due-date     LIKE oe-ordl.req-date
    FIELD unit-count   LIKE eb.cas-cnt
    FIELD units-pallet LIKE eb.cas-pal
    FIELD routing      AS CHARACTER
    FIELD ship-to      AS CHARACTER
    FIELD rm-no        AS CHARACTER
    FIELD NumOfUnit    AS INTEGER
    FIELD msf          AS DECIMAL
    FIELD msf2         AS DECIMAL
    INDEX row-id row-id.

DEFINE TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin.

DEFINE TEMP-TABLE tt-fg-set NO-UNDO
    FIELD ord-no       AS INTEGER
    FIELD line         AS INTEGER
    FIELD part-no      AS CHARACTER FORMAT "X(15)"
    FIELD QtyPerSet    AS DECIMAL
    FIELD part-qty-dec AS DECIMAL
    FIELD routing      AS CHARACTER
    FIELD rm-no        AS CHARACTER
    INDEX ord-no ord-no LINE.

DEFINE VARIABLE lv-pdf-file AS CHARACTER NO-UNDO.
{custom/xprint.i}

DEFINE STREAM st-excel.

DEFINE VARIABLE v-prompt-excel AS LOGICAL NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK WHERE
    sys-ctrl.company EQ cocode AND
    sys-ctrl.name    EQ "OR16"
    NO-ERROR.

IF NOT AVAILABLE sys-ctrl THEN
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.name    = "OR16"
        sys-ctrl.descrip = "Prompt for Excel Filename?".
END.

v-prompt-excel = sys-ctrl.log-fld.


DEFINE VARIABLE ldummy             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO .
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

ASSIGN 
    cTextListToSelect  = "Due Date,Cust#,Cust Part#,Routing,Board,Job#,Ship To,Order Qty," +
                           "Rel Qty,#/Unit,Rel MSF,Ord MSF"
    cFieldListToSelect = "due-dt,cust,cust-prt,rout,brd,job,shipto,ord-qty," +
                            "rel-qty,unt,rel-msf,ord-msf"
    cFieldLength       = "10,8,15,20,20,13,7,9," + "9,8,11,11" 
    cFieldType         = "c,c,c,c,c,c,c,i," + "i,i,i,i"
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Due Date,Cust#,Cust Part#,Routing,Board,Job#,Ship To,Order Qty," +
                           "Rel Qty,#/Unit,Rel MSF,Ord MSF" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust-no end_cust-no begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_due-date end_due-date ~
begin_userid end_userid begin_slsmn end_slsmn sl_avail Btn_Def sl_selected ~
Btn_Add Btn_Remove btn_Up btn_down rd-dest tb_OpenCSV v-excel-file btn-ok ~
btn-cancel tb_cust-list btnCustList td-print-line RECT-39 RECT-40 ~
tbAutoClose 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_due-date end_due-date ~
begin_userid end_userid begin_slsmn end_slsmn sl_avail sl_selected rd-dest ~
tb_OpenCSV v-excel-file tb_cust-list td-print-line tbAutoClose 

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

DEFINE VARIABLE begin_cad-no   AS CHARACTER FORMAT "X(15)" 
    LABEL "Beginning CAD#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE begin_cust-no  AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE begin_due-date AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Due Date" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

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

DEFINE VARIABLE begin_slsmn    AS CHARACTER FORMAT "XXX" 
    LABEL "Beginning Sales Rep#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE begin_userid   AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning User ID" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_cad-no     AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending CAD#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE end_cust-no    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE end_due-date   AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Due Date" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

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

DEFINE VARIABLE end_slsmn      AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
    LABEL "Ending Sales Rep#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE end_userid     AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending User ID" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_due-date   AS CHARACTER FORMAT "X(256)":U INITIAL "Due Date?" 
    VIEW-AS FILL-IN 
    SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Secondary Sort?" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_sort-1     AS CHARACTER FORMAT "X(256)":U INITIAL "Primary Sort?" 
    VIEW-AS FILL-IN 
    SIZE 14 BY .95 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 55 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 56 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "13" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE v-excel-file   AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\OpenJobsHotList.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE
    SIZE 49 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt        AS CHARACTER INITIAL "L" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 28 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 14 BY 4.52 NO-UNDO.

DEFINE VARIABLE rd_due-date    AS CHARACTER INITIAL "Line" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Line", "Line",
    "Release", "Release"
    SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "PO#" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "PO#", "PO#",
    "Item", "Item",
    "Cust Part#", "Cust Part#",
    "FG Item Name", "FG Item Name",
    "Order#", "Order#",
    "Due Date", "Due Date",
    "CAD#", "CAD#"
    SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort-1      AS CHARACTER INITIAL "due date" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Customer#", "Customer#",
    "Due Date", "Due Date",
    "Sales Rep", "Salesman"
    SIZE 47 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-39
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 5.3.

DEFINE RECTANGLE RECT-40
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 9.71.

DEFINE VARIABLE sl_avail      AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected   AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tbAutoClose   AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_batch      AS LOGICAL   INITIAL NO 
    LABEL "Run In Batch Mode?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81
    BGCOLOR 14 NO-UNDO.

DEFINE VARIABLE tb_cust-list  AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel      AS LOGICAL   INITIAL YES 
    LABEL "Output to Excel File?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV    AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.

DEFINE VARIABLE td-print-line AS LOGICAL   INITIAL NO 
    LABEL "Print Gridlines?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm  AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_cust-no AT ROW 2.81 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust-no AT ROW 2.81 COL 72 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_ord-date AT ROW 9.52 COL 57 COLON-ALIGNED
    end_ord-date AT ROW 8.86 COL 62 COLON-ALIGNED HELP
    "Enter Ending Due Date"
    begin_po-no AT ROW 9 COL 52 COLON-ALIGNED HELP
    "Enter Ending Customer PO Number"
    end_po-no AT ROW 9.1 COL 60 COLON-ALIGNED HELP
    "Enter Ending Customer PO Number"
    begin_job-no AT ROW 3.76 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    begin_job-no2 AT ROW 3.76 COL 43.4 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    end_job-no AT ROW 3.76 COL 72 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    end_job-no2 AT ROW 3.76 COL 87.4 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    begin_i-no AT ROW 8.86 COL 33 COLON-ALIGNED HELP
    "Enter Beginning Item Number"
    end_i-no AT ROW 8.86 COL 59 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    begin_cad-no AT ROW 10 COL 30 COLON-ALIGNED HELP
    "Enter Beginning CAD Number"
    end_cad-no AT ROW 9.24 COL 55 COLON-ALIGNED HELP
    "Enter Ending CAD Number"
    begin_due-date AT ROW 4.71 COL 28 COLON-ALIGNED
    end_due-date AT ROW 4.71 COL 72 COLON-ALIGNED HELP
    "Enter Ending Due Date"
    begin_userid AT ROW 5.67 COL 28 COLON-ALIGNED HELP
    "Enter the Beginning User ID"
    end_userid AT ROW 5.67 COL 72 COLON-ALIGNED HELP
    "Enter the Ending User ID"
    begin_slsmn AT ROW 6.62 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Sales Rep Number"
    end_slsmn AT ROW 6.62 COL 72 COLON-ALIGNED HELP
    "Enter Ending Sales Rep Number"
    lbl_sort-1 AT ROW 9 COL 25 COLON-ALIGNED NO-LABELS
    rd_sort-1 AT ROW 9.52 COL 24 NO-LABELS
    lbl_sort AT ROW 9.24 COL 23 NO-LABELS
    rd_sort AT ROW 9.76 COL 19 NO-LABELS
    sl_avail AT ROW 12.29 COL 4 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 12.29 COL 42.6 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 12.29 COL 64.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 13.29 COL 42.6 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 14.29 COL 42.6 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 15.33 COL 42.6 WIDGET-ID 40
    btn_down AT ROW 16.33 COL 42.6 WIDGET-ID 42
    rd-dest AT ROW 18.62 COL 6 NO-LABELS
    lv-ornt AT ROW 18.14 COL 37 NO-LABELS
    lv-font-no AT ROW 18.86 COL 41 COLON-ALIGNED
    lines-per-page AT ROW 18.14 COL 85 COLON-ALIGNED
    lv-font-name AT ROW 19.81 COL 36 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 21 COL 31
    tb_batch AT ROW 21.05 COL 61
    tb_excel AT ROW 19.1 COL 58
    tb_OpenCSV AT ROW 22.05 COL 95.4 RIGHT-ALIGNED
    v-excel-file AT ROW 21.95 COL 29 COLON-ALIGNED
    btn-ok AT ROW 24.24 COL 31
    btn-cancel AT ROW 24.24 COL 55
    lbl_due-date AT ROW 9.76 COL 23 COLON-ALIGNED NO-LABELS WIDGET-ID 2
    rd_due-date AT ROW 9.14 COL 36 NO-LABELS WIDGET-ID 4
    tb_cust-list AT ROW 1.86 COL 30.2 WIDGET-ID 20
    btnCustList AT ROW 1.86 COL 62.6 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 100.6 BY 25.48
    BGCOLOR 15 .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
    td-print-line AT ROW 8.71 COL 34 WIDGET-ID 58
    tbAutoClose AT ROW 23.38 COL 31 WIDGET-ID 60
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 11.57 COL 4.2 WIDGET-ID 38
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 11.57 COL 64.4 WIDGET-ID 44
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 17.67 COL 5
    RECT-39 AT ROW 18 COL 4.2
    RECT-40 AT ROW 1.52 COL 4 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 100.6 BY 25.48
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
        TITLE              = "Open Jobs Hot List"
        HEIGHT             = 25.43
        WIDTH              = 100.4
        MAX-HEIGHT         = 32.71
        MAX-WIDTH          = 272.8
        VIRTUAL-HEIGHT     = 32.71
        VIRTUAL-WIDTH      = 272.8
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN begin_cad-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    begin_cad-no:HIDDEN IN FRAME FRAME-A       = TRUE
    begin_cad-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_due-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_i-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    begin_i-no:HIDDEN IN FRAME FRAME-A       = TRUE
    begin_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_ord-date IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    begin_ord-date:HIDDEN IN FRAME FRAME-A       = TRUE
    begin_ord-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_po-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    begin_po-no:HIDDEN IN FRAME FRAME-A       = TRUE
    begin_po-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_userid:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR FILL-IN end_cad-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    end_cad-no:HIDDEN IN FRAME FRAME-A       = TRUE
    end_cad-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_due-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN end_i-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    end_i-no:HIDDEN IN FRAME FRAME-A       = TRUE
    end_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN end_ord-date IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    end_ord-date:HIDDEN IN FRAME FRAME-A       = TRUE
    end_ord-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN end_po-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    end_po-no:HIDDEN IN FRAME FRAME-A       = TRUE
    end_po-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_userid:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_due-date IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lbl_due-date:HIDDEN IN FRAME FRAME-A       = TRUE
    lbl_due-date:PRIVATE-DATA IN FRAME FRAME-A = "rd_due-date".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
    lbl_sort:HIDDEN IN FRAME FRAME-A       = TRUE
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

/* SETTINGS FOR FILL-IN lbl_sort-1 IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lbl_sort-1:HIDDEN IN FRAME FRAME-A       = TRUE
    lbl_sort-1:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort-1".

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

/* SETTINGS FOR RADIO-SET rd_due-date IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    rd_due-date:HIDDEN IN FRAME FRAME-A       = TRUE
    rd_due-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR RADIO-SET rd_sort IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    rd_sort:HIDDEN IN FRAME FRAME-A       = TRUE
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR RADIO-SET rd_sort-1 IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    rd_sort-1:HIDDEN IN FRAME FRAME-A       = TRUE
    rd_sort-1:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_batch IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_batch:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Open Jobs Hot List */
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
ON WINDOW-CLOSE OF C-Win /* Open Jobs Hot List */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cad-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cad-no C-Win
ON LEAVE OF begin_cad-no IN FRAME FRAME-A /* Beginning CAD# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
    DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
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


&Scoped-define SELF-NAME begin_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_due-date C-Win
ON LEAVE OF begin_due-date IN FRAME FRAME-A /* Beginning Due Date */
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


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Sales Rep# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
    DO:
        DELETE PROCEDURE hdOutputProcs.
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
                v-excel-file = SUBSTRING(v-excel-file,1,INDEX(v-excel-file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT v-excel-file,OUTPUT cFileName) .
            v-excel-file:SCREEN-VALUE =  cFileName.
        END.
        ASSIGN
            lv-pdf-file    = init-dir + "\OpnOrder"
            is-xprint-form = NO.

        FIND FIRST  ttCustList NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttCustList AND tb_cust-list THEN 
        DO:
            EMPTY TEMP-TABLE ttCustList.
            RUN BuildCustList(INPUT cocode,
                INPUT tb_cust-list AND glCustListActive ,
                INPUT begin_cust-no,
                INPUT end_cust-no).
        END.

        IF g_batch THEN tb_batch = YES.
        IF v-prompt-excel AND tb_excel THEN 
        DO:
            DEFINE VARIABLE v-excel-file2 AS cha NO-UNDO.
            RUN oerep/d-ordexl.w (OUTPUT v-excel-file2).
            v-excel-file = v-excel-file + v-excel-file2 + ".csv".
            IF tb_batch THEN DISPLAY v-excel-file WITH FRAME {&FRAME-NAME}.
        END.

        IF tb_batch THEN 
        DO:
            RUN run-batch.
            RETURN NO-APPLY.
        END.

        RUN GetSelectionList.
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
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    is-xprint-form = YES.
                    /*IF is-xprint-form THEN DO:
                       {custom/asimail.i &TYPE = "Customer"
                                      &begin_cust= begin_cust-no
                                      &END_cust=begin_cust-no
                                      &mail-subject=c-win:title
                                      &mail-body=c-win:title
                                      &mail-file=list-name }
                                      */
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail2.i &TYPE="CUSTOMER"
                             &group-title=v-prgmname
                             &begin_cust=begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject="Open Order Report"
                             &mail-body="Open Order Report"
                             &mail-file=lv-pdf-file + ".pdf" }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr2.i &TYPE="Customer"
                                  &group-title=v-prgmname
                                  &begin_cust=begin_cust-no
                                  &END_cust=begin_cust-no
                                  &mail-subject=c-win:TITLE
                                  &mail-body=c-win:TITLE
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


&Scoped-define SELF-NAME end_cad-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cad-no C-Win
ON LEAVE OF end_cad-no IN FRAME FRAME-A /* Ending CAD# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
    DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val) .

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


&Scoped-define SELF-NAME end_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_due-date C-Win
ON LEAVE OF end_due-date IN FRAME FRAME-A /* Ending Due Date */
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


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Sales Rep# */
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
            ELSE lines-per-page:SCREEN-VALUE = "65".     
        END.
        ELSE 
        DO:
            IF lv-ornt:SCREEN-VALUE BEGINS "p" THEN lines-per-page:SCREEN-VALUE = "99".
            ELSE lines-per-page:SCREEN-VALUE = "65".     
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_due-date C-Win
ON VALUE-CHANGED OF rd_due-date IN FRAME FRAME-A
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


&Scoped-define SELF-NAME rd_sort-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort-1 C-Win
ON VALUE-CHANGED OF rd_sort-1 IN FRAME FRAME-A
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
                    ldummy = sl_Avail:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
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
        ASSIGN {&self-name}.
        EMPTY TEMP-TABLE ttCustList.
        RUN SetCustRange(INPUT tb_cust-list).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Output to Excel File? */
    DO:
        ASSIGN {&self-name}

            .
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


&Scoped-define SELF-NAME td-print-line
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-print-line C-Win
ON VALUE-CHANGED OF td-print-line IN FRAME FRAME-A /* Print Gridlines? */
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


&Scoped-define SELF-NAME v-excel-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-excel-file C-Win
ON HELP OF v-excel-file IN FRAME FRAME-A /* Name */
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


&Scoped-define SELF-NAME v-excel-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-excel-file C-Win
ON LEAVE OF v-excel-file IN FRAME FRAME-A /* Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE lv-title AS CHARACTER NO-UNDO.


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

    begin_ord-date = TODAY.
    IF g_batch THEN tb_batch = YES.

    FIND FIRST users NO-LOCK WHERE
        users.user_id EQ USERID("NOSWEAT")
        NO-ERROR.

    IF AVAILABLE users AND users.user_program[2] NE "" THEN
        init-dir = users.user_program[2].
    ELSE
        init-dir = "c:\tmp".

    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "OZ8" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "OZ8",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    DO WITH FRAME {&FRAME-NAME}:
        lv-title = c-win:TITLE.
        {custom/usrprint.i}
RUN DisplaySelectionList2.
c-win:TITLE = lv-title.
APPLY "entry" TO begin_cust-no.

END.
RUN pChangeDest.
RUN sys/ref/CustList.p (INPUT cocode,
    INPUT 'OZ8',
    INPUT NO,
    OUTPUT glCustListActive).

{sys/inc/chblankcust.i ""OZ8""}

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

    DEFINE BUFFER b-ar-invl        FOR ar-invl.
    DEFINE BUFFER b-inv-head       FOR inv-head.
    DEFINE BUFFER b-inv-line       FOR inv-line.
    DEFINE BUFFER b-oe-rell        FOR oe-rell.

    DEFINE BUFFER b1-in-house-cust FOR cust.
    DEFINE BUFFER b1-shipto        FOR shipto.

    DEFINE VARIABLE v-po-no LIKE oe-ord.po-no NO-UNDO.

    v-po-no = oe-ordl.po-no.

    CREATE tt-report.

    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ oe-ordl.company
        AND itemfg.i-no    EQ oe-ordl.i-no
        NO-ERROR.
    IF AVAILABLE itemfg THEN tt-report.cad-no = itemfg.cad-no.

    IF tt-report.cad-no EQ "" THEN 
    DO:
        RELEASE eb.
        IF TRIM(oe-ordl.est-no) NE "" THEN
            FIND FIRST eb NO-LOCK
                WHERE eb.company  EQ oe-ordl.company
                AND eb.est-no   EQ oe-ordl.est-no
                AND eb.stock-no EQ oe-ordl.i-no
                AND eb.cad-no   NE ""
                USE-INDEX est-no  NO-ERROR.
        IF NOT AVAILABLE eb THEN
            FIND FIRST eb NO-LOCK
                WHERE eb.company  EQ oe-ordl.company
                AND eb.stock-no EQ oe-ordl.i-no
                AND eb.cad-no   NE ""
                USE-INDEX stock  NO-ERROR.
        IF AVAILABLE eb THEN tt-report.cad-no = eb.cad-no.
    END.

    RELEASE eb.

    IF TRIM(oe-ordl.est-no) NE "" THEN
    DO:
        FIND FIRST eb WHERE
            eb.company  EQ oe-ordl.company AND
            eb.est-no   EQ oe-ordl.est-no AND
            eb.stock-no EQ oe-ordl.i-no AND
            eb.form-no  EQ oe-ordl.form-no AND
            eb.blank-no EQ oe-ordl.blank-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE eb THEN
        DO:
            ASSIGN
                tt-report.unit-count   = eb.cas-cnt
                tt-report.units-pallet = eb.cas-pal.
            RELEASE eb.
        END.
    END.

    ASSIGN
        tt-report.term-id  = ""
        tt-report.key-01   = IF rd_sort-1 EQ "Due Date" THEN
                          STRING(YEAR(lv-due-date),"9999") +
                          STRING(MONTH(lv-due-date),"99")  +
                          STRING(DAY(lv-due-date),"99")
                        ELSE
                        IF rd_sort-1 EQ "Salesman" THEN
                          oe-ordl.s-man[1]         ELSE ""
        tt-report.key-02   = oe-ord.cust-no
        tt-report.key-03   = IF v-sort EQ "PO" THEN v-po-no
                        ELSE 
                        IF v-sort EQ "It" THEN
                          (STRING(oe-ordl.i-no,"x(15)") + v-po-no)
                        ELSE
                        IF v-sort EQ "Cu" THEN
                          (STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE
                        IF v-sort EQ "FG" THEN
                          (STRING(oe-ordl.i-name,"x(30)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE
                        IF v-sort EQ "Or" THEN
                          (STRING(oe-ord.ord-no,"99999999999") + oe-ordl.part-no)
                        ELSE
                        IF v-sort EQ "CA" THEN
                          (STRING(tt-report.cad-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE
                          (STRING(YEAR(lv-due-date),"9999") +
                           STRING(MONTH(lv-due-date),"99")  +
                           STRING(DAY(lv-due-date),"99")    +
                           STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))              
        tt-report.key-04   = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', oe-ordl.job-no, oe-ordl.job-no2))
        tt-report.key-05   = STRING(oe-ord.ord-no,"99999999999")
        tt-report.key-06   = oe-ordl.i-no
        tt-report.key-07   = STRING(YEAR(ip-date),"9999") +
                        STRING(MONTH(ip-date),"99")  +
                        STRING(DAY(ip-date),"99")
        tt-report.po-no    = v-po-no
        tt-report.rec-id   = ip-recid
        tt-report.row-id   = ROWID(oe-ordl)
        tt-report.due-date = lv-due-date
        v-ordl             = NO.

    FIND FIRST oe-rel NO-LOCK
        WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
        NO-ERROR.

    /* for managed warehouse shipto */
    RELEASE b1-in-house-cust.
    RELEASE b1-shipto.
   

    IF AVAILABLE oe-ordl AND oe-ordl.managed = TRUE THEN 
    DO:
        FIND FIRST b1-in-house-cust NO-LOCK
            WHERE b1-in-house-cust.company EQ oe-ordl.company
            AND b1-in-house-cust.active  EQ "X" NO-ERROR.

        IF AVAILABLE b1-in-house-cust THEN
            FIND FIRST b1-shipto NO-LOCK WHERE
                b1-shipto.company EQ oe-ordl.company AND
                b1-shipto.cust-no EQ b1-in-house-cust.cust-no AND
                b1-shipto.ship-id EQ "WHSE" NO-ERROR.

    END.

    tt-report.ship-to = (IF AVAILABLE b1-shipto THEN b1-shipto.ship-id ELSE IF AVAILABLE oe-rel THEN oe-rel.ship-id ELSE "").

    FIND FIRST job-hdr NO-LOCK WHERE job-hdr.company = oe-ordl.company
        AND job-hdr.job-no = oe-ordl.job-no
        AND job-hdr.job-no2 = oe-ordl.job-no2
        AND job-hdr.i-no = oe-ordl.i-no
        NO-ERROR.
    IF AVAILABLE job-hdr THEN
        FOR EACH job-mch NO-LOCK WHERE 
            job-mch.company EQ oe-ordl.company
            AND job-mch.job     EQ job-hdr.job
            AND job-mch.job-no  EQ oe-ord.job-no
            AND job-mch.job-no2 EQ oe-ord.job-no2
            AND job-mch.frm     EQ job-hdr.frm 
            BREAK BY job-mch.LINE :

            tt-report.routing = tt-report.routing + job-mch.m-code + ",".
        END.

    IF tt-report.routing = "" AND oe-ordl.est-no <> "" THEN 
        FOR EACH est-op NO-LOCK WHERE 
            est-op.company = oe-ordl.company
            AND est-op.est-no = oe-ordl.est-no
            AND est-op.line LT 500 :
            /*((ASI.est-op.qty eq est-qty.eqty and est.est-type ne 8) or
               (ASI.est-op.qty eq lv-eqty and est.est-type ge 7)) NO-LOCK*/
            tt-report.routing = tt-report.routing + est-op.m-code + ",".
        END.

    tt-report.routing = RIGHT-TRIM(tt-report.routing,",").

    IF AVAILABLE job-hdr THEN FIND ef WHERE ef.company = job-hdr.company
            AND ef.est-no = job-hdr.est-no
            AND ef.form-no = job-hdr.frm NO-LOCK NO-ERROR.
    IF AVAILABLE ef THEN tt-report.rm-no = ef.board.

    FIND FIRST eb NO-LOCK WHERE
        eb.company  EQ oe-ordl.company AND
        eb.est-no   EQ oe-ordl.est-no AND
        eb.stock-no EQ oe-ordl.i-no AND
        eb.form-no  EQ oe-ordl.form-no AND
        eb.blank-no EQ oe-ordl.blank-no
        NO-ERROR.

    IF AVAILABLE eb THEN
        ASSIGN tt-report.NumOfUnit = eb.tr-cnt.

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

    FIND b-oe-rell WHERE RECID(b-oe-rell) EQ ip-recid NO-LOCK NO-ERROR.
    IF NOT tt-report.inv AND AVAILABLE b-oe-rell THEN
        tt-report.q-rel = b-oe-rell.qty.

    IF AVAILABLE job-hdr THEN
    DO:
        FIND FIRST job WHERE 
            job.company EQ job-hdr.company AND
            job.job     EQ job-hdr.job AND
            job.job-no  EQ job-hdr.job-no AND
            job.job-no2 EQ job-hdr.job-no2
            NO-LOCK NO-ERROR.

        IF AVAILABLE job THEN
        DO:
            IF ((AVAILABLE itemfg AND itemfg.isaset) OR oe-ordl.is-a-component) AND
                CAN-FIND(FIRST reftable WHERE 
                reftable.reftable EQ "jc/jc-calc.p"
                AND reftable.company  EQ job.company
                AND reftable.loc      EQ ""
                AND reftable.code     EQ STRING(job.job,"999999999")) THEN
                FOR EACH reftable NO-LOCK WHERE 
                    reftable.reftable EQ "jc/jc-calc.p"
                    AND reftable.company  EQ job-hdr.company
                    AND reftable.loc      EQ ""
                    AND reftable.code     EQ STRING(job-hdr.job,"999999999")
                    AND ((reftable.code2  EQ oe-ordl.i-no AND oe-ordl.is-a-component) OR
                    (job-hdr.i-no    EQ oe-ordl.i-no AND NOT oe-ordl.is-a-component)):

                    CREATE tt-fg-set.
                    ASSIGN
                        tt-fg-set.ord-no       = oe-ordl.ord-no
                        tt-fg-set.line         = oe-ordl.line
                        tt-fg-set.QtyPerSet    = reftable.val[12]
                        tt-fg-set.part-qty-dec = reftable.val[13].

                    FOR EACH job-mch FIELDS(m-code blank-no) WHERE 
                        job-mch.company EQ job.company AND
                        job-mch.job     EQ job.job AND
                        job-mch.job-no  EQ job.job-no AND
                        job-mch.job-no2 EQ job.job-no2 AND
                        job-mch.frm     EQ INTEGER(tt-fg-set.QtyPerSet) NO-LOCK
                        BREAK BY job-mch.line:

                        IF job-mch.blank-no EQ 0 OR
                            job-mch.blank-no EQ tt-fg-set.part-qty-dec THEN
                            tt-fg-set.routing = tt-fg-set.routing + job-mch.m-code + ",".
                    END.

                    IF tt-fg-set.routing = "" AND oe-ordl.est-no NE "" THEN
                        FOR EACH est-op FIELDS(m-code b-num ) WHERE 
                            est-op.company = oe-ordl.company AND
                            est-op.est-no = oe-ordl.est-no AND
                            est-op.line LT 500 AND
                            est-op.s-num EQ INTEGER(tt-fg-set.QtyPerSet)
                            NO-LOCK:

                            IF est-op.b-num EQ 0 OR
                                est-op.b-num EQ tt-fg-set.part-qty-dec THEN
                                tt-fg-set.routing = tt-fg-set.routing + est-op.m-code + ",".
                        END.

                    tt-fg-set.routing = RIGHT-TRIM(tt-fg-set.routing,",").

                    FIND FIRST ef WHERE
                        ef.company = job-hdr.company AND
                        ef.est-no = job-hdr.est-no AND
                        ef.form-no = INTEGER(tt-fg-set.QtyPerSet)
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE ef THEN
                        tt-fg-set.rm-no = ef.board.

                    FIND FIRST eb WHERE
                        eb.company EQ job-hdr.company AND
                        eb.est-no  EQ job-hdr.est-no AND
                        eb.form-no EQ tt-fg-set.QtyPerSet AND
                        eb.blank-no EQ tt-fg-set.part-qty-dec
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE eb THEN
                        tt-fg-set.part-no = eb.part-no.
                END.
        END.
    END.

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
            INPUT 'OZ8',
            INPUT YES,
            OUTPUT lActive).
    END.
    ELSE 
    DO:
        FOR EACH bf-cust NO-LOCK
            WHERE bf-cust.company EQ ipcCompany
            AND bf-cust.cust-no GE ipcBeginCust
            AND bf-cust.cust-no LE ipcEndCust :

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
    DEFINE VARIABLE v-curr   AS LOGICAL.
    DEFINE VARIABLE v-q-or-v AS LOGICAL.

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

    DEFINE BUFFER b-f-rc FOR fg-rcpth.
    DEFINE BUFFER b-f-rd FOR fg-rdtlh.


    ASSIGN
        vdat     = TODAY
        v-curr   = YES
        v-q-or-v = YES.

    FOR EACH itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ oe-ordl.i-no
        NO-LOCK,
        EACH fg-bin
        WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
        NO-LOCK:

        CREATE tt-fg-bin.
        BUFFER-COPY fg-bin EXCEPT rec_key TO tt-fg-bin.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustList C-Win 
PROCEDURE CustList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    RUN sys/ref/CustListManager.w(INPUT cocode,
        INPUT 'OZ8').

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
    DISPLAY begin_cust-no end_cust-no begin_job-no begin_job-no2 end_job-no 
        end_job-no2 begin_due-date end_due-date begin_userid end_userid 
        begin_slsmn end_slsmn sl_avail sl_selected rd-dest tb_OpenCSV 
        v-excel-file tb_cust-list td-print-line tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE begin_cust-no end_cust-no begin_job-no begin_job-no2 end_job-no 
        end_job-no2 begin_due-date end_due-date begin_userid end_userid 
        begin_slsmn end_slsmn sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
        btn_Up btn_down rd-dest tb_OpenCSV v-excel-file btn-ok btn-cancel 
        tb_cust-list btnCustList td-print-line RECT-39 RECT-40 tbAutoClose 
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
        FIND FIRST ttRptList NO-LOCK WHERE ttRptList.TextList = ENTRY(i,cTmpList)  NO-ERROR.     

        CREATE ttRptSelected.
        ASSIGN 
            ttRptSelected.TextList        = ENTRY(i,cTmpList)
            ttRptSelected.FieldList       = ttRptList.FieldList
            ttRptSelected.FieldLength     = INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
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
    RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
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
{BATCH/runbatch.i "oerep\s-hotsop.r"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------ oe/rep/backlog.p 10/94 gb */
    /* Order Backlog Summary / Detail Report                                      */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-top3w.f}*/

    DEFINE BUFFER b-tt-report FOR tt-report.
    DEFINE BUFFER b-oe-rell   FOR oe-rell.

    DEFINE VARIABLE v-cust       LIKE oe-ord.cust-no EXTENT 2 INITIAL ["","zzzzzzzz"].
    DEFINE VARIABLE v-date       LIKE ar-inv.inv-date FORMAT "99/99/9999"
        EXTENT 2 INITIAL [TODAY, 12/31/9999].

    DEFINE VARIABLE v-job        LIKE oe-ord.job-no EXTENT 2 INITIAL ["","zzzzzzzzz"].
    DEFINE VARIABLE v-job2       LIKE oe-ord.job-no2 FORMAT "999" EXTENT 2 INITIAL [0,999].
    DEFINE VARIABLE v-inc        AS LOGICAL   FORMAT "Yes/No" INITIAL YES.
    DEFINE VARIABLE v-ostat      AS CHARACTER FORMAT "!" INITIAL "A".
    DEFINE VARIABLE v-jobq       AS LOGICAL   FORMAT "Yes/No" INITIAL NO.

    DEFINE VARIABLE v-bal-qty    AS INTEGER   FORMAT ">>>,>>>,>>9".
    DEFINE VARIABLE v-ord-no     AS CHARACTER.
    DEFINE VARIABLE v-q-onh      LIKE itemfg.q-onh NO-UNDO.
    DEFINE VARIABLE v-q-shp      LIKE v-q-onh NO-UNDO.
    DEFINE VARIABLE v-q-rel      LIKE v-q-onh NO-UNDO.
    DEFINE VARIABLE lv-slsmn     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-due-dt    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-job-no    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-routing   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lv-tot-msf   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-date-msf  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-tot-msf2  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-date-msf2 AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE v-time       AS INTEGER   NO-UNDO.
    v-time = TIME.
    DEFINE VARIABLE v-comma        AS cha       FORM "x" INITIAL "," NO-UNDO.
    DEFINE VARIABLE lv-job-qty     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-rec-qty     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-job#         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE s-b-line       AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDisplay       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-tit4       AS CHARACTER FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5       AS CHARACTER FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line       AS CHARACTER FORM "x(300)" NO-UNDO.

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-print-line AS LOGICAL   NO-UNDO .
    DEFINE VARIABLE lSelected    AS LOGICAL   INITIAL YES NO-UNDO.

    FORMAT HEADER
        SKIP(1)
        "Sales Rep:"
        lv-slsmn

        WITH FRAME r-top2 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP STREAM-IO WIDTH 200.

    FORMAT s-b-line
        tt-fg-set.part-no FORMAT "X(15)" 
        tt-fg-set.routing FORMAT "X(20)"
        tt-fg-set.rm-no   FORMAT "X(15)"
        WITH FRAME tt-fg-set-frame NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP STREAM-IO WIDTH 200.

    FORMAT HEADER
        SKIP(1)
        "Cust#   "
        "Cust Part#     "
        "Routing             "
        "Board               "
        "Job#      "
        "Ship To "
        " Order Qty"
        "  Release Qty"
        "    #/Unit"
        "       MSF"
        SKIP
        "--------"
        "---------------"
        "--------------------"
        "--------------------"
        "----------"
        "-------"
        "-----------"
        "-------------"
        "----------"
        "----------"

        WITH FRAME r-top3 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP STREAM-IO WIDTH 200.


    SESSION:SET-WAIT-STATE ("general").

    ASSIGN 
        str-tit2     = c-win:TITLE
        str-tit3     = (IF rd_sort-1 EQ "Customer#" THEN ""
                                         ELSE "By " + TRIM(rd_sort-1) + " ")
        {sys/inc/ctrtext.i str-tit2 112}
        {sys/inc/ctrtext.i str-tit3 132}

        v-cust[1]    = begin_cust-no
        v-cust[2]    = end_cust-no
        v-date[1]    = begin_ord-date
        v-date[2]    = end_ord-date
        v-job[1]     = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', begin_job-no, begin_job-no2))
        v-job[2]     = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', end_job-no, end_job-no2))

        v-sort       = SUBSTRING(rd_sort,1,2)
        v-print-line = td-print-line 
        lSelected    = tb_cust-list.


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

        IF LOOKUP(ttRptSelected.TextList, "Rel MSF,Ord MSF") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.
    IF lselected THEN 
    DO:
        FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
        IF AVAILABLE ttCustList THEN ASSIGN v-cust[1] = ttCustList.cust-no .
        FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
        IF AVAILABLE ttCustList THEN ASSIGN v-cust[2] = ttCustList.cust-no .
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(0)}

    IF rd-dest = 5 THEN 
    DO:
        IF lv-ornt = "L" THEN PUT "<OLANDSCAPE><PREVIEW>".                   /*<p7><CPI20>*/ 
        ELSE PUT "<PREVIEW>".
        PUT "<PDF-EXCLUDE=MS Mincho></PROGRESS><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><P7><ADJUST=LPI>" FORM "x(165)" SKIP.
    END. 
    OUTPUT CLOSE.


    IF tb_excel THEN 
    DO:
        OUTPUT STREAM st-excel TO VALUE(cFileName).
        PUT STREAM st-excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

        {sys/inc/outprint.i "value(lines-per-page) append" }                                     /**/

    IF td-show-parm THEN RUN show-param.

    VIEW FRAME r-top.

    FOR EACH tt-report:
        DELETE tt-report.
    END.
    EMPTY TEMP-TABLE tt-fg-bin.
    EMPTY TEMP-TABLE tt-fg-set.

    {oerep/r-hotsoN.i}

    IF tb_excel THEN
    DO:
        OUTPUT STREAM st-excel CLOSE.
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

    lv-frame-hdl = FRAME {&FRAME-NAME}:HANDLE.
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

            lv-label = FILL(" ",34 - LENGTH(TRIM(ENTRY(i,parm-lbl-list)))) +
                TRIM(ENTRY(i,parm-lbl-list)) + ":".

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
                v-excel-file:SENSITIVE  = YES
                tb_OpenCSV:SENSITIVE    = YES
                tb_excel                = YES
                .
        ELSE
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                v-excel-file:SENSITIVE  = NO
                tb_OpenCSV:SENSITIVE    = NO
                tb_excel                = NO
                .
        ASSIGN 
            v-excel-file:SCREEN-VALUE = "c:\tmp\OpenJobsHotList.csv".    
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

