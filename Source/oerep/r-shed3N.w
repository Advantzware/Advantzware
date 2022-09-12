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

DEFINE VARIABLE v-fcust    LIKE cust.cust-no EXTENT 2 INIT ["","zzzzzzzz"] NO-UNDO.
DEFINE VARIABLE v-ford-no  AS INTEGER   FORMAT ">>>>>>>9" EXTENT 2 INIT [0,99999999] NO-UNDO.
DEFINE VARIABLE v-fdate    AS DATE      EXTENT 2 FORMAT "99/99/9999" INIT [TODAY, 12/31/9999] NO-UNDO.
DEFINE VARIABLE v-fitem    AS CHARACTER FORMAT "x(15)" EXTENT 2 INIT ["","zzzzzzzzzzzzzzz"] NO-UNDO.
DEFINE VARIABLE v-fsman    AS CHARACTER FORMAT "xxx" EXTENT 2 INIT ["","zzz"] NO-UNDO.
DEFINE VARIABLE v-fcarr    AS CHARACTER FORMAT "x(5)" EXTENT 2 INIT ["","zzzzz"] NO-UNDO.
DEFINE VARIABLE v-floc     AS CHARACTER FORMAT 'X(5)' EXTENT 2 INIT ['','zzzzz'] NO-UNDO.
DEFINE VARIABLE v-ponum    AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE v-sort     AS CHARACTER FORMAT "!" INIT "C" NO-UNDO.
DEFINE VARIABLE v-print    AS CHARACTER FORMAT "!" INIT "I" NO-UNDO.
DEFINE VARIABLE v-types    AS CHARACTER FORMAT "x(7)" INIT "PALSBIC" NO-UNDO.
DEFINE VARIABLE v-comps    AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE v-by-job   AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE chosen     AS INTEGER   INIT 3 NO-UNDO.
DEFINE VARIABLE v-qty      LIKE oe-rel.qty NO-UNDO.
DEFINE VARIABLE v-date     LIKE oe-rel.rel-date NO-UNDO.
DEFINE VARIABLE v-po-no    LIKE oe-rel.po-no NO-UNDO.
DEFINE VARIABLE v-rel-no   LIKE oe-rel.rel-no NO-UNDO.
DEFINE VARIABLE v-ship-id  LIKE oe-rel.ship-id NO-UNDO.
DEFINE VARIABLE v-carrier  LIKE oe-rel.carrier NO-UNDO.  
DEFINE VARIABLE v-type     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-tot-qty  AS INTEGER   FORMAT "->>>,>>>,>>9" EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-tot-val  AS DECIMAL   FORMAT "->>>,>>>,>>9" EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-tot-msf  AS DECIMAL   FORMAT "->>>,>>9.999" EXTENT 2 NO-UNDO.
DEFINE VARIABLE ld-qty-ord AS DECIMAL   FORMAT ">>>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE ld-qty-rec AS DECIMAL   FORMAT ">>>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE ll-po      AS LOG       NO-UNDO.
DEFINE VARIABLE lv-text    AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-qty-opt  AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE w-ord
    FIELD ord-no         LIKE oe-ord.ord-no
    FIELD est-no         LIKE oe-ord.est-no
    FIELD onh-qty        LIKE itemfg.q-onh
    FIELD cust-no        LIKE oe-ord.cust-no
    FIELD cust-name      LIKE oe-ord.cust-name
    FIELD part-no        LIKE oe-ordl.part-no
    FIELD i-no           LIKE oe-ordl.i-no
    FIELD i-name         LIKE oe-ordl.i-name
    FIELD qty            LIKE oe-ordl.qty
    FIELD cost           LIKE oe-ordl.cost
    FIELD price          LIKE oe-ordl.price
    FIELD t-price        LIKE oe-ordl.t-price FORMAT "->>,>>>,>>9"
    FIELD rel-qty        LIKE oe-rel.qty
    FIELD rel-date       AS CHARACTER FORMAT "x(9)"
    FIELD job            AS CHARACTER FORMAT "x(13)"
    FIELD job-no         LIKE oe-ordl.job-no
    FIELD job-no2        LIKE oe-ordl.job-no2
    FIELD rel-no         LIKE oe-rel.rel-no
    FIELD ship-id        LIKE oe-rel.ship-id
    FIELD po-num         LIKE oe-ordl.po-no
    FIELD ord-qty        LIKE oe-ordl.qty
    FIELD shp-qty        LIKE oe-ordl.ship-qty
    FIELD msf            AS DECIMAL   FORMAT "->>9.999"
    FIELD component      AS INTEGER
    FIELD prom-code      LIKE oe-ordl.prom-code FORMAT 'X(5)'
    FIELD last-date      LIKE oe-ord.last-date FORMAT "99/99/99"
    FIELD carrier        LIKE oe-relh.carrier
    FIELD is-a-component LIKE oe-ordl.is-a-component
    FIELD palls          AS INTEGER   FORMAT "->>,>>>,>>9"
    FIELD xls-rel-date   LIKE oe-rel.rel-date FORMAT "99/99/99"
    FIELD xls-status     AS CHARACTER
    FIELD iPro-qty       AS INTEGER
    FIELD cDueDate       AS CHARACTER FORMAT "x(10)" 
    FIELD lot-no         AS CHARACTER 
    FIELD cTrailer       AS CHARACTER 
    FIELD dtDockDate     AS DATE      FORMAT "99/99/9999"
    FIELD cDockTime      AS CHARACTER FORMAT "99:99"
    FIELD cShipNotes     AS CHARACTER
    .

DEFINE BUFFER b-w-ord FOR w-ord.

{fg/fullset.i new}

{custom/formtext.i NEW}

DEFINE            VARIABLE tb_prt-qoh     AS LOG       NO-UNDO.
DEFINE            VARIABLE tb_prt-last    AS LOG       NO-UNDO.
DEFINE            VARIABLE rd_print2      AS CHARACTER NO-UNDO.
DEFINE            VARIABLE rd_print3      AS CHARACTER NO-UNDO.

DEFINE            VARIABLE v-program      AS CHARACTER NO-UNDO.
DEFINE            VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE {1} SHARED VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE ll-secure      AS LOG       NO-UNDO.

DEFINE TEMP-TABLE tt-report LIKE report 
    FIELD qty      LIKE oe-rell.qty
    FIELD onh      LIKE fg-bin.qty
    FIELD ord-no   AS INTEGER 
    FIELD i-no     AS CHARACTER 
    FIELD rel-date AS DATE
    FIELD pro-qty  AS INTEGER  .
DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE glCustListActive   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO .
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.


ASSIGN 
    cTextListToSelect  = "Customer#,Rel Date,Rel Num,Ship To,Carrier,Order #,Cust Part#,Descrption,Fg Item#," +
                           "Po Number,Qty On Hand,Release Qty,Sales Value,Skids,Status,Projected Qty,Due Date,Customer Lot #," +
                           "Trailer#,Dock Appointment,Time,Ship Notes"
    cFieldListToSelect = "cust,rel-date,rel-num,ship,carr,ord,cust-part,desc,fg-item," +
                            "po-num,Qty-hand,rel-qty,sales,skid,stat,proj-qty,due-date,lot-no," +
                            "cTrailer,dtDockDate,cDockTime,cShipNotes"
    cFieldLength       = "9,8,7,8,7,8,15,25,15," + "15,11,11,15,7,6,13,8,15," + "20,16,5,256"
    cFieldType         = "c,c,i,c,c,i,c,c,c," + "c,i,i,i,i,c,i,c,c," + "c,c,c,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Customer#,Rel Date,Rel Num,Ship To,Carrier,Order #,Cust Part#,Descrption,Fg Item#," +
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
btn_down rd-dest tb_OpenCSV fi_file btn-ok btn-cancel tb_pro-qty ~
tb_cust-list btnCustList tbAutoClose RECT-6 RECT-7 RECT-11 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_ord-no ~
end_ord-no begin_i-no end_i-no begin_loc end_loc begin_slsmn end_slsmn ~
begin_date end_date begin_carr end_carr tb_actual tb_backordered rd_sort ~
tb_show-only sl_avail sl_selected rd-dest tb_OpenCSV fi_file tb_pro-qty ~
tb_cust-list tbAutoClose 

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

DEFINE VARIABLE begin_carr     AS CHARACTER FORMAT "X(5)":U 
    LABEL "Beginning Carrier#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust-no  AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_i-no     AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc      AS CHARACTER FORMAT "X(5)":U 
    LABEL "Beginning Warehouse" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-no   AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "Beginning Order#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn    AS CHARACTER FORMAT "XXX" 
    LABEL "Beginning Sales Rep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_carr       AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
    LABEL "Ending Carrier#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no       AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc        AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
    LABEL "Ending Warehouse" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no     AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 99999999 
    LABEL "Ending Order#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn      AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
    LABEL "Ending Sales Rep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\ActualReleases.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 52 BY 1.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 55.2 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt        AS CHARACTER INITIAL "L" 
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
    SIZE 14 BY 4.14 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Customer#" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Customer#", "Customer#",
    "Release Date", "Release Date",
    "Item#", "Item#",
    "Item Name", "Item Name",
    "Territory", "Territory",
    "Carrier", "Carrier",
    "Credit Rating", "Credit Rating"
    SIZE 90 BY 1.05 NO-UNDO.

DEFINE RECTANGLE RECT-11
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 93 BY 6.43.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 102 BY 4.95.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 104 BY 18.29.

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

DEFINE VARIABLE tb_actual      AS LOGICAL   INITIAL YES 
    LABEL "Actual" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.2 BY 1.33 NO-UNDO.

DEFINE VARIABLE tb_backordered AS LOGICAL   INITIAL YES 
    LABEL "Backorder" 
    VIEW-AS TOGGLE-BOX
    SIZE 18.2 BY 1.29 NO-UNDO.

DEFINE VARIABLE tb_cust-list   AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 30.8 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel       AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV     AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_pro-qty     AS LOGICAL   INITIAL YES 
    LABEL "Projected Qty?" 
    VIEW-AS TOGGLE-BOX
    SIZE 18.2 BY 1.19 NO-UNDO.

DEFINE VARIABLE tb_show-only   AS LOGICAL   INITIAL NO 
    LABEL "Show only releases with Qty > On Hand?" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm   AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_cust-no AT ROW 2.91 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust-no AT ROW 2.95 COL 75.4 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_ord-no AT ROW 3.86 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Order Number"
    end_ord-no AT ROW 3.91 COL 75.4 COLON-ALIGNED HELP
    "Enter Ending Order Number"
    begin_i-no AT ROW 4.81 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Order Number"
    end_i-no AT ROW 4.86 COL 75.4 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    begin_loc AT ROW 5.76 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Warehouse"
    end_loc AT ROW 5.81 COL 75.4 COLON-ALIGNED HELP
    "Enter Ending Warehouse"
    begin_slsmn AT ROW 6.71 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Sales Rep Number"
    end_slsmn AT ROW 6.76 COL 75.4 COLON-ALIGNED HELP
    "Enter Ending Sales Rep Number"
    begin_date AT ROW 7.67 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 7.71 COL 75.4 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_carr AT ROW 8.62 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Carrier Number"
    end_carr AT ROW 8.67 COL 75.4 COLON-ALIGNED HELP
    "Enter Ending Carrier Number"
    tb_actual AT ROW 10.48 COL 31.4
    tb_backordered AT ROW 10.52 COL 43.6
    rd_sort AT ROW 11.95 COL 16.4 NO-LABELS
    tb_show-only AT ROW 9.67 COL 31.2
    sl_avail AT ROW 14 COL 11.4 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 13.86 COL 47.4 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 14 COL 66.8 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 14.86 COL 47.4 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 15.86 COL 47.4 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 16.91 COL 47.4 WIDGET-ID 40
    btn_down AT ROW 17.91 COL 47.4 WIDGET-ID 42
    rd-dest AT ROW 20.76 COL 6 NO-LABELS
    lv-ornt AT ROW 20.43 COL 41 NO-LABELS
    lv-font-no AT ROW 21 COL 43 COLON-ALIGNED
    lines-per-page AT ROW 20.43 COL 96 COLON-ALIGNED
    lv-font-name AT ROW 21.95 COL 42 NO-LABELS
    td-show-parm AT ROW 22.91 COL 31
    tb_excel AT ROW 21.24 COL 86 RIGHT-ALIGNED
    tb_OpenCSV AT ROW 23.95 COL 97.8 RIGHT-ALIGNED
    fi_file AT ROW 23.86 COL 29 COLON-ALIGNED HELP
    "Enter File Name"
    btn-ok AT ROW 26.24 COL 31
    btn-cancel AT ROW 26.24 COL 61
    tb_pro-qty AT ROW 9.71 COL 77 WIDGET-ID 58
    tb_cust-list AT ROW 1.95 COL 29 WIDGET-ID 6
    btnCustList AT ROW 2.05 COL 61 WIDGET-ID 60
    tbAutoClose AT ROW 25.29 COL 31 WIDGET-ID 62
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 13.29 COL 12 WIDGET-ID 38
    "Release Types:" VIEW-AS TEXT
    SIZE 15 BY 1 AT ROW 10.52 COL 13.4
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 20 COL 5
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    "Sort Options:" VIEW-AS TEXT
    SIZE 12.6 BY 1 AT ROW 11.91 COL 3.4 WIDGET-ID 12
    "Print Options" VIEW-AS TEXT
    SIZE 15 BY 1 AT ROW 9.91 COL 15.8 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 108.6 BY 28.24
    BGCOLOR 15 .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 13.29 COL 66.8 WIDGET-ID 44
    RECT-6 AT ROW 20.33 COL 4
    RECT-7 AT ROW 1.52 COL 3
    RECT-11 AT ROW 13.14 COL 9 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 108.6 BY 28.24
    BGCOLOR 15 .


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
        HEIGHT             = 28.24
        WIDTH              = 108.6
        MAX-HEIGHT         = 29.24
        MAX-WIDTH          = 157.8
        VIRTUAL-HEIGHT     = 29.24
        VIRTUAL-WIDTH      = 157.8
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
ASSIGN 
    begin_carr:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_loc:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_carr:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_loc:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_ord-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lines-per-page:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
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
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_actual:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_backordered:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_pro-qty:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_show-only:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Actual Releases */
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
ON WINDOW-CLOSE OF C-Win /* Actual Releases */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_carr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_carr C-Win
ON LEAVE OF begin_carr IN FRAME FRAME-A /* Beginning Carrier# */
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


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
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


&Scoped-define SELF-NAME begin_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_loc C-Win
ON LEAVE OF begin_loc IN FRAME FRAME-A /* Beginning Warehouse */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Beginning Order# */
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
            ASSIGN {&DISPLAYED-OBJECTS}.
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
                INPUT tb_cust-list AND glCustListActive,
                INPUT begin_cust-no,
                INPUT END_cust-no).
        END.
        RUN run-report.

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
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = "Customer"
                             &begin_cust= begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Customer"
                                  &begin_cust= begin_cust-no
                                  &END_cust=begin_cust-no
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
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
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
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


&Scoped-define SELF-NAME end_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_loc C-Win
ON LEAVE OF end_loc IN FRAME FRAME-A /* Ending Warehouse */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* Ending Order# */
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


&Scoped-define SELF-NAME tb_backordered
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_backordered C-Win
ON VALUE-CHANGED OF tb_backordered IN FRAME FRAME-A /* Backorder */
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


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_pro-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_pro-qty C-Win
ON VALUE-CHANGED OF tb_pro-qty IN FRAME FRAME-A /* Projected Qty? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_show-only
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-only C-Win
ON VALUE-CHANGED OF tb_show-only IN FRAME FRAME-A /* Show only releases with Qty > On Hand? */
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

    begin_date = TODAY.
    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "OZ4" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "OZ4",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
RUN DisplaySelectionList2.
APPLY "entry" TO begin_cust-no.
END.
RUN pChangeDest.
RUN sys/ref/CustList.p (INPUT cocode,
    INPUT 'OZ4',
    INPUT NO,
    OUTPUT glCustListActive).

{sys/inc/chblankcust.i ""OZ4""}

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
            INPUT 'OZ4',
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
    DISPLAY begin_cust-no end_cust-no begin_ord-no end_ord-no begin_i-no end_i-no 
        begin_loc end_loc begin_slsmn end_slsmn begin_date end_date begin_carr 
        end_carr tb_actual tb_backordered rd_sort tb_show-only sl_avail 
        sl_selected rd-dest tb_OpenCSV fi_file tb_pro-qty tb_cust-list 
        tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE begin_cust-no end_cust-no begin_ord-no end_ord-no begin_i-no end_i-no 
        begin_loc end_loc begin_slsmn end_slsmn begin_date end_date begin_carr 
        end_carr tb_actual tb_backordered rd_sort tb_show-only sl_avail 
        Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest 
        tb_OpenCSV fi_file btn-ok btn-cancel tb_pro-qty tb_cust-list 
        btnCustList tbAutoClose RECT-6 RECT-7 RECT-11 
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
    RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
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
            fi_file:SCREEN-VALUE = "c:\tmp\ActualReleases.csv".    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    DEFINE BUFFER b-oe-ordl FOR oe-ordl.

    DEFINE VARIABLE lv-qty           LIKE oe-rell.qty NO-UNDO.
    DEFINE VARIABLE lv-subt          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-cr-rating     LIKE cust.cr-rating NO-UNDO.
    DEFINE VARIABLE ll-show-top-only AS LOG       NO-UNDO.
    DEFINE VARIABLE ld-palls         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE tb_notes         AS LOG       NO-UNDO.
    DEFINE VARIABLE begin_spec       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE end_spec         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE tb_stats         AS LOG       INIT NO NO-UNDO.
    DEFINE VARIABLE tb_subt          AS LOG       INIT YES NO-UNDO.
    DEFINE VARIABLE v-tot-pal        LIKE v-tot-qty NO-UNDO.

    DEFINE VARIABLE cDisplay         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField           AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedList    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-tit4         AS cha       FORM "x(276)" NO-UNDO.
    DEFINE VARIABLE str-tit5         AS cha       FORM "x(276)" NO-UNDO.
    DEFINE VARIABLE str-line         AS cha       FORM "x(300)" NO-UNDO.
    DEFINE VARIABLE iProjQty         AS INTEGER   NO-UNDO .
    DEFINE VARIABLE iRelqty          AS INTEGER   NO-UNDO .
    /*{sys/form/r-top5DL3.f} */
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSelected   AS LOGICAL   INIT YES NO-UNDO.

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

        WITH FRAME r-top ROW 1 COLUMN 1 STREAM-IO WIDTH 276
        NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.

    FORM HEADER
        "Credit Rating:"
        lv-cr-rating
        SKIP(1)

        WITH FRAME r-top2 PAGE-TOP NO-ATTR-SPACE NO-BOX WIDTH 276 STREAM-IO.



    ASSIGN 
        v-tot-qty = 0
        v-tot-msf = 0
        v-tot-val = 0.

    ASSIGN
        str-tit2     = c-win:TITLE
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
        lSelected    = tb_cust-list
        v-sort       = IF rd_sort EQ "Customer#"     THEN "C"  ELSE
                  IF rd_sort EQ "Release Date"  THEN "R"  ELSE
                  IF rd_sort EQ "Item#"         THEN "I"  ELSE
                  IF rd_sort EQ "Item Name"     THEN "N"  ELSE
                  IF rd_sort EQ "Territory"     THEN "T"  ELSE
                  IF rd_sort EQ "Credit Rating" THEN "CR" ELSE "A"
        v-types      = STRING(tb_actual,"A/")      + STRING(tb_backordered,"B/")

        str-tit3     = (IF v-sort EQ "C"  THEN "By Customer By Date"      ELSE
               IF v-sort EQ "R"  THEN "By Release Date"          ELSE
               IF v-sort EQ "I"  THEN "By Item By Date"          ELSE
               IF v-sort EQ "N"  THEN "By Item Name By Date"     ELSE
               IF v-sort EQ "A"  THEN "By Carrier By Date"       ELSE
               IF v-sort EQ "CR" THEN "By Credit Rating By Date" ELSE
                                      "By Territory By Date")    + "  "   +
               STRING(v-fdate[1],"99/99/9999")                   + " to " +
               STRING(v-fdate[2],"99/99/9999")

        {sys/inc/ctrtext.i str-tit3 172}.

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

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF lselected THEN 
    DO:
        FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
        IF AVAILABLE ttCustList THEN ASSIGN v-fcust[1] = ttCustList.cust-no .
        FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
        IF AVAILABLE ttCustList THEN ASSIGN v-fcust[2] = ttCustList.cust-no .
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
        AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ oe-ord.cust-no
        AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
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

            IF LAST-OF(oe-rell.po-no) THEN 
            DO:
                CREATE tt-report.
                ASSIGN
                    tt-report.term-id  = ""
                    tt-report.key-01   = IF v-sort EQ "R" THEN
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
                    tt-report.key-02   = IF v-sort EQ "I" OR v-sort EQ "D" THEN oe-rell.i-no
                             ELSE
                             IF v-sort EQ "T" THEN cust.terr
                             ELSE
                             IF v-sort EQ "A" THEN oe-relh.carrier
                             ELSE
                             IF v-sort EQ "CR" THEN cust.cr-rating
                             /*ELSE
                              IF v-sort EQ "R" THEN  string(oe-relh.release#,"9999999999")*/
                             ELSE oe-relh.cust-no
                    tt-report.key-03   = IF v-sort NE "R" THEN
                               (STRING(YEAR(oe-relh.rel-date),"9999") +
                                STRING(MONTH(oe-relh.rel-date),"99")  +
                                STRING(DAY(oe-relh.rel-date),"99"))
                             ELSE STRING(oe-relh.release#,"9999999999")
                    tt-report.key-04   = STRING(IF v-sort EQ "A" THEN oe-relh.cust-no
                                                     ELSE " ","x(10)") +
                             STRING(oe-ord.ord-no,"9999999999")
                    tt-report.key-05   = STRING(INDEX(v-types,v-type),"99")
                    tt-report.key-06   = IF oe-rell.b-ord-no EQ 0 THEN "A" ELSE "B"
                    tt-report.qty      = lv-qty
                    tt-report.rec-id   = RECID(oe-rell)
                    tt-report.ord-no   = oe-ord.ord-no
                    tt-report.i-no     = oe-rell.i-no 
                    tt-report.rel-date = oe-relh.rel-date   .

                FOR EACH fg-bin
                    WHERE fg-bin.company EQ oe-ordl.company
                    AND fg-bin.i-no    EQ oe-ordl.i-no
                    AND fg-bin.job-no  EQ oe-ordl.job-no
                    AND fg-bin.job-no2 EQ oe-ordl.job-no2
                    AND fg-bin.loc     GE v-floc[1]
                    AND fg-bin.loc     LE v-floc[2]
                    USE-INDEX job NO-LOCK:
                    tt-report.onh = tt-report.onh + fg-bin.qty.
                END.
            END.
        END.
    END.

    STATUS DEFAULT "Printing...".

  
    RELEASE tt-report.
    iRelqty = 0  .
    FOR EACH tt-report WHERE tt-report.term-id EQ "",
        FIRST oe-rell WHERE RECID(oe-rell) EQ tt-report.rec-id NO-LOCK,
        FIRST oe-relh
        WHERE oe-relh.company EQ oe-rell.company
        AND oe-relh.r-no    EQ oe-rell.r-no
        USE-INDEX r-no NO-LOCK,
        FIRST oe-ordl
        WHERE oe-ordl.company EQ oe-rell.company
        AND oe-ordl.ord-no  EQ oe-rell.ord-no
        AND oe-ordl.i-no    EQ oe-rell.i-no
        AND oe-ordl.line    EQ oe-rell.line
        NO-LOCK,
        FIRST oe-ord OF oe-ordl NO-LOCK,
        FIRST cust
        WHERE cust.company EQ oe-ord.company
        AND cust.cust-no EQ oe-ord.cust-no
        NO-LOCK
        BREAK 
        BY tt-report.i-no
        BY oe-ord.ord-no 
        BY tt-report.rel-date
        :
       
        IF FIRST-OF(oe-ord.ord-no) THEN
            iRelqty = 0.
  
        tt-report.pro-qty  = tt-report.onh - iRelqty  .

        IF FIRST-OF(oe-ord.ord-no) THEN
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

    IF NOT CAN-FIND(FIRST tt-report WHERE tt-report.term-id EQ "") THEN 
    DO:
        CREATE tt-report.
        ASSIGN
            tt-report.term-id = ""
            ll-show-top-only  = YES.
    END.
  
    FOR EACH tt-report WHERE tt-report.term-id EQ "",
        FIRST oe-rell WHERE RECID(oe-rell) EQ tt-report.rec-id NO-LOCK,
        FIRST oe-relh
        WHERE oe-relh.company EQ oe-rell.company
        AND oe-relh.r-no    EQ oe-rell.r-no
        USE-INDEX r-no NO-LOCK,
        FIRST oe-ordl
        WHERE oe-ordl.company EQ oe-rell.company
        AND oe-ordl.ord-no  EQ oe-rell.ord-no
        AND oe-ordl.i-no    EQ oe-rell.i-no
        AND oe-ordl.line    EQ oe-rell.line
        NO-LOCK,
        FIRST oe-ord OF oe-ordl NO-LOCK,
        FIRST cust
        WHERE cust.company EQ oe-ord.company
        AND cust.cust-no EQ oe-ord.cust-no
        NO-LOCK
        BREAK BY tt-report.key-01
        BY tt-report.key-02 
        BY tt-report.key-03 
        BY tt-report.key-04 DESCENDING :

        IF v-sort EQ "CR" AND FIRST-OF(tt-report.key-02) THEN 
        DO:
            lv-cr-rating = tt-report.key-02.
            IF FIRST(tt-report.key-02) THEN VIEW FRAME r-top2.
            PAGE.
        END.

        ELSE
            IF FIRST(tt-report.key-01) THEN PAGE.

        v-qty = IF tt-report.qty NE 0 THEN tt-report.qty ELSE oe-rell.qty.

        CREATE w-ord.

        FIND FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ oe-ordl.i-no
            NO-LOCK NO-ERROR.

        ASSIGN
            w-ord.ord-no         = oe-ord.ord-no
            w-ord.cust-no        = oe-ord.cust-no
            w-ord.cust-name      = oe-ord.cust-name
            w-ord.part-no        = oe-ordl.part-no
            w-ord.i-no           = oe-ordl.i-no
            w-ord.i-name         = oe-ordl.i-name
            w-ord.qty            = oe-ordl.qty
            w-ord.cost           = oe-ordl.cost
            w-ord.price          = oe-ordl.t-price / oe-ordl.qty
            w-ord.rel-qty        = v-qty
            w-ord.onh-qty        = tt-report.onh
            w-ord.t-price        = w-ord.price * w-ord.rel-qty
            w-ord.rel-date       = STRING(oe-relh.rel-date) + tt-report.key-06
            w-ord.rel-no         = oe-relh.release#
            w-ord.ship-id        = oe-relh.ship-id
            w-ord.job-no         = oe-ordl.job-no
            w-ord.job-no2        = oe-ordl.job-no2
            w-ord.job            = IF w-ord.job-no EQ "" THEN "" ELSE
                                   TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', w-ord.job-no, w-ord.job-no2)))
            w-ord.po-num         = oe-rell.po-no
            w-ord.ord-qty        = oe-ordl.qty
            w-ord.shp-qty        = oe-ordl.ship-qty
            w-ord.msf            = w-ord.rel-qty * ( IF AVAILABLE itemfg THEN itemfg.t-sqft ELSE 0) / 1000
            w-ord.prom-code      = oe-ordl.prom-code
            w-ord.last-date      = oe-ord.last-date
            w-ord.carrier        = oe-relh.carrier
            w-ord.is-a-component = oe-ordl.is-a-component
            ld-palls             = w-ord.rel-qty /
                       ((IF oe-ordl.cas-cnt    EQ 0 THEN 1 ELSE oe-ordl.cas-cnt) *
                        (IF oe-ordl.cases-unit EQ 0 THEN 1 ELSE oe-ordl.cases-unit))
            w-ord.cDueDate       = IF oe-ordl.req-date NE ? THEN STRING(oe-ordl.req-date) ELSE ""
            w-ord.lot-no         = oe-rell.lot-no
            w-ord.cTrailer       = oe-relh.trailer
            w-ord.dtDockDate     = DATE(oe-relh.releaseDockTime)
            w-ord.cDockTime      = TRIM(REPLACE(SUBSTRING(STRING(oe-relh.releaseDockTime),11,6), ":",""))
            w-ord.cShipNotes     = TRIM(oe-relh.ship-i[1]) + " " + TRIM(oe-relh.ship-i[2]) + " " + TRIM(oe-relh.ship-i[3]) + " " + TRIM(oe-relh.ship-i[4])
            .

        {sys/inc/roundup.i ld-palls}

        IF ld-palls LT 0 THEN ld-palls = ld-palls * -1.

        w-ord.palls = w-ord.palls + ld-palls.

        w-ord.iPro-qty = tt-report.pro-qty.

        IF NOT FIRST-OF(tt-report.key-02) AND v-sort EQ "C" THEN w-ord.cust-name = "".

        IF v-comps AND AVAILABLE itemfg AND itemfg.isaset THEN 
        DO:
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

            IF AVAILABLE itemfg OR ll-show-top-only THEN 
            DO:
                /*{oe/rep/schdrel.i}*/
                DEFINE VARIABLE lv-issue AS CHARACTER FORMAT "x(5)" NO-UNDO.
                lv-issue = IF w-ord.rel-qty GT /*w-ord.onh-qty*/ w-ord.iPro-qty THEN "ISSUE" ELSE "OK".

                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "cust"    THEN 
                            cVarValue = STRING(w-ord.cust-no,"x(8)") .
                        WHEN "rel-date"   THEN 
                            cVarValue = IF w-ord.rel-date NE ? THEN STRING(w-ord.rel-date,"x(8)") ELSE "".
                        WHEN "rel-num"   THEN 
                            cVarValue = STRING(w-ord.rel-no,">>>>>9").
                        WHEN "ship"  THEN 
                            cVarValue = STRING(w-ord.ship-id,"x(8)") .
                        WHEN "carr"   THEN 
                            cVarValue = STRING(w-ord.carrier ,"x(8)") .
                        WHEN "ord"  THEN 
                            cVarValue = TRIM(STRING(w-ord.ord-no,">>>>>>>>")) .
                        WHEN "cust-part"   THEN 
                            cVarValue = STRING(w-ord.part-no,"x(15)") .
                        WHEN "desc"  THEN 
                            cVarValue = STRING(w-ord.i-name,"x(25)") .

                        WHEN "fg-item"    THEN 
                            cVarValue = STRING(w-ord.i-no,"x(15)") .
                        WHEN "po-num"   THEN 
                            cVarValue = STRING(w-ord.po-num,"x(15)").
                        WHEN "Qty-hand"   THEN 
                            cVarValue = STRING(w-ord.onh-qty,"->>,>>>,>>9").
                        WHEN "rel-qty"  THEN 
                            cVarValue = STRING(w-ord.rel-qty,"->>,>>>,>>9") .
                        WHEN "sales"   THEN 
                            cVarValue = IF ll-secure THEN STRING(w-ord.t-price,"$->>,>>>,>>9.99") ELSE "" .
                        WHEN "skid"  THEN 
                            cVarValue = STRING(w-ord.palls,">>>,>>9") .
                        WHEN "stat"   THEN 
                            cVarValue = STRING(lv-issue,"x(6)") .
                        WHEN "proj-qty"   THEN 
                            cVarValue = IF tb_pro-qty THEN STRING(w-ord.iPro-qty,"->>>>,>>>,>>9") ELSE "" .
                        WHEN "due-date"   THEN 
                            cVarValue = STRING(w-ord.cDueDate,"x(8)") .
                        WHEN "lot-no"     THEN 
                            cVarValue = STRING(w-ord.lot-no,"x(15)").
                        WHEN "cTrailer"     THEN 
                            cVarValue = STRING(w-ord.cTrailer,"x(20)").
                        WHEN "dtDockDate"     THEN 
                            cVarValue = IF w-ord.dtDockDate NE ? THEN STRING(w-ord.dtDockDate,"99/99/9999") ELSE "".
                        WHEN "cDockTime"     THEN 
                            cVarValue = IF w-ord.cDockTime NE ? THEN STRING(w-ord.cDockTime,"99:99") ELSE "".
                        WHEN "cShipNotes"     THEN 
                            cVarValue = STRING(w-ord.cShipNotes,"x(256)").
                    END CASE.
                    
                    IF cTmpField = "rel-date"   THEN 
                            cExcelVarValue = IF w-ord.rel-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",DATE(w-ord.rel-date)) ELSE "".
                    ELSE IF cTmpField = "due-date"   THEN 
                            cExcelVarValue = DYNAMIC-FUNCTION("sfFormat_Date",DATE(w-ord.cDueDate)) .
                    ELSE IF cTmpField = "dtDockDate"     THEN 
                            cExcelVarValue = IF w-ord.dtDockDate NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",w-ord.dtDockDate) ELSE "".        

                    ELSE cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cExcelVarValue)) + ",".            
                END.

                PUT UNFORMATTED cDisplay SKIP.
                IF tb_excel THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  
                        cExcelDisplay SKIP.
                END.


            END.
        END.

        IF NOT ll-show-top-only THEN 
        DO:
            FIND FIRST w-ord.

            ASSIGN
                v-tot-qty[1] = v-tot-qty[1] + 1
                v-tot-msf[1] = v-tot-msf[1] + w-ord.msf.
            IF w-ord.t-price NE ? THEN
                v-tot-val[1] = v-tot-val[1] + w-ord.t-price .
            IF w-ord.palls NE ? THEN
                v-tot-pal[1] = v-tot-pal[1] + w-ord.palls.

            IF LAST-OF(tt-report.key-02) THEN 
            DO:
                /*IF v-sort EQ "C" THEN
                  PUT "Customer Totals:" TO 140.
                ELSE
                  PUT "      Subtotals:" TO 140.*/

                /* PUT v-tot-val[1] TO 156 FORMAT "$->>,>>>,>>9.99"
                     v-tot-pal[1] TO 164 FORMAT ">>>,>>9"
                     SKIP(2).*/

                PUT str-line SKIP .
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "cust"    THEN 
                            cVarValue = "" .
                        WHEN "rel-date"   THEN 
                            cVarValue = "".
                        WHEN "rel-num"   THEN 
                            cVarValue = "".
                        WHEN "ship"  THEN 
                            cVarValue = "".
                        WHEN "carr"   THEN 
                            cVarValue = "" .
                        WHEN "ord"  THEN 
                            cVarValue = "" .
                        WHEN "cust-part"   THEN 
                            cVarValue = "" .
                        WHEN "desc"  THEN 
                            cVarValue = "" .

                        WHEN "fg-item"    THEN 
                            cVarValue = "" .
                        WHEN "po-num"   THEN 
                            cVarValue =  "".
                        WHEN "Qty-hand"   THEN 
                            cVarValue = "".
                        WHEN "rel-qty"  THEN 
                            cVarValue = "" .
                        WHEN "sales"   THEN 
                            cVarValue = IF ll-secure THEN STRING(v-tot-val[1],"$->>,>>>,>>9.99") ELSE "" .
                        WHEN "skid"  THEN 
                            cVarValue = STRING(v-tot-pal[1],">>>,>>9") .
                        WHEN "stat"   THEN 
                            cVarValue = "" .
                        WHEN "proj-qty"   THEN 
                            cVarValue = "" .
                        WHEN "due-date"   THEN 
                            cVarValue = "" .
                        WHEN "lot-no"     THEN 
                            cVarValue = "".
                        WHEN "cTrailer"   THEN 
                            cVarValue = "".
                        WHEN "dtDockDate" THEN 
                            cVarValue = "".
                        WHEN "cDockTime"  THEN 
                            cVarValue = "".
                        WHEN "cShipNotes" THEN 
                            cVarValue = "".
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
                IF TRIM(cDisplay) NE "" THEN 
                DO:
                    IF v-sort EQ "C" THEN
                        PUT UNFORMATTED "          Customer Totals:"  SUBSTRING(cDisplay,27,350) SKIP(1).  
                    ELSE
                        PUT UNFORMATTED "          Sub Totals:" SUBSTRING(cDisplay,22,350) SKIP(1).
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

            IF LAST(tt-report.key-01) THEN 
            DO:
                /*PUT SKIP(1)
                    "Report Totals:" TO 140
                    v-tot-val[2]     TO 156 FORMAT "$->>,>>>,>>9.99"
                    v-tot-pal[2]     TO 164 FORMAT ">>>,>>9".*/
                PUT str-line SKIP .
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "cust"    THEN 
                            cVarValue = "" .
                        WHEN "rel-date"   THEN 
                            cVarValue = "".
                        WHEN "rel-num"   THEN 
                            cVarValue = "".
                        WHEN "ship"  THEN 
                            cVarValue = "".
                        WHEN "carr"   THEN 
                            cVarValue = "" .
                        WHEN "ord"  THEN 
                            cVarValue = "" .
                        WHEN "cust-part"   THEN 
                            cVarValue = "" .
                        WHEN "desc"  THEN 
                            cVarValue = "" .

                        WHEN "fg-item"    THEN 
                            cVarValue = "" .
                        WHEN "po-num"   THEN 
                            cVarValue =  "".
                        WHEN "Qty-hand"   THEN 
                            cVarValue = "".
                        WHEN "rel-qty"  THEN 
                            cVarValue = "" .
                        WHEN "sales"   THEN 
                            cVarValue = IF ll-secure THEN STRING(v-tot-val[2],"$->>,>>>,>>9.99") ELSE "" .
                        WHEN "skid"  THEN 
                            cVarValue = STRING(v-tot-pal[2],">>>,>>9") .
                        WHEN "stat"   THEN 
                            cVarValue = "" .
                        WHEN "proj-qty"   THEN 
                            cVarValue = "" .
                        WHEN "due-date"   THEN 
                            cVarValue = "" .
                        WHEN "lot-no"     THEN 
                            cVarValue = "".
                        WHEN "cTrailer"   THEN 
                            cVarValue = "".
                        WHEN "dtDockDate" THEN 
                            cVarValue = "".
                        WHEN "cDockTime"  THEN 
                            cVarValue = "".
                        WHEN "cShipNotes" THEN 
                            cVarValue = "".
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.

                IF TRIM(cDisplay) NE "" THEN 
                    PUT UNFORMATTED "          Report Totals:  "  SUBSTRING(cDisplay,27,350) SKIP(1).  


            END.
        END.

        FOR EACH w-ord:
            DELETE w-ord.
        END.
    END. /* each tt-report */

    STATUS DEFAULT "".

    SESSION:SET-WAIT-STATE ("").

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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

