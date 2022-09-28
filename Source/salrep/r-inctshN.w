&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-inctsh.w

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

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL   NO-UNDO.
DEFINE VARIABLE fcust            AS ch        INIT "" NO-UNDO.
DEFINE VARIABLE tcust            LIKE fcust INIT "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE fship            AS ch        INIT "" NO-UNDO.
DEFINE VARIABLE tship            LIKE fcust INIT "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE fshpz            AS ch        FORMAT "x(10)" INIT "" NO-UNDO.
DEFINE VARIABLE tshpz            LIKE fcust INIT "zzzzzzzzzz" NO-UNDO.
DEFINE VARIABLE fitem            LIKE itemfg.i-no INIT " " NO-UNDO.
DEFINE VARIABLE titem            LIKE fitem INIT "zzzzzzzzzzzzzzzzzzz" NO-UNDO.
DEFINE VARIABLE fsman            AS CHARACTER FORMAT "x(3)" INIT "" NO-UNDO.
DEFINE VARIABLE tsman            LIKE fsman INIT "zzz" NO-UNDO.
DEFINE VARIABLE fdate            AS DATE      FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE tdate            LIKE fdate NO-UNDO.
DEFINE VARIABLE v-sort1          AS CHARACTER FORMAT "!" INIT "I" NO-UNDO.
DEFINE VARIABLE v-disc-p         AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE v-freight        AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE v-inc-fc         AS LOG       INIT NO NO-UNDO.

DEFINE VARIABLE v-date           LIKE ar-inv.inv-date COLUMN-LABEL "Invoice!Date" NO-UNDO.
DEFINE VARIABLE v-ord            LIKE ar-invl.ord-no COLUMN-LABEL "Order!Number" NO-UNDO.
DEFINE VARIABLE v-pric           LIKE ar-invl.unit-pr COLUMN-LABEL "Unit Price" NO-UNDO.
DEFINE VARIABLE v-uom            LIKE ar-invl.pr-uom COLUMN-LABEL "UOM" NO-UNDO.

DEFINE VARIABLE v-sman-no        AS CHARACTER FORMAT "x(3)" NO-UNDO.

DEFINE VARIABLE v-exc            AS LOG       NO-UNDO.
DEFINE VARIABLE v-name           LIKE cust.name FORMAT "x(21)" NO-UNDO.

DEFINE VARIABLE v-pct            AS DECIMAL   FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE v-fac            AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-ship           LIKE ar-inv.ship-id NO-UNDO.
DEFINE VARIABLE v-shpz           LIKE ar-inv.sold-zip NO-UNDO.
DEFINE VARIABLE v-disc           LIKE ar-invl.disc NO-UNDO.

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.

DEFINE BUFFER xreport FOR tt-report.

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD i-no   LIKE ar-invl.i-no COLUMN-LABEL "FG Item"
    FIELD inv-no LIKE ar-invl.inv-no COLUMN-LABEL "Invoice!Number"
    FIELD rec-id AS RECID.

DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.

DEFINE STREAM st-excel.
DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.


ASSIGN 
    cTextListToSelect  = "Customer,Ship To,City,State,Rep,Inv#,Month,Year," +
                           "Inv Date,Fg Item#,Pro Code,Order #,Qty Shipped,Unit Price,UOM,Invoice Amt,BOL Whse," +
                           "Ship To Name,Ship Address 1,Ship Address 2,Ship Address 3,Ship To City,Ship To ST,Ship To Zip,BOL Date"
    cFieldListToSelect = "cust,shipto,city,stat,rep,inv,month,year," +
                            "inv-date,fg-item,pro-code,ord,qty-ship,unit-price,uom,inv-amt,bol-whs," +
                            "ship-name,ship-add1,ship-add2,ship-add3,ship-city,ship-st,ship-zip,bol-date"
    cFieldLength       = "8,8,15,5,20,7,5,4," + "8,15,8,8,12,15,4,17,8," + "30,30,30,30,15,10,12,10"
    cFieldType         = "c,c,c,c,c,i,i,i," + "c,c,c,i,i,i,c,i,c," + "c,c,c,c,c,c,c,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Customer,Ship To,City,State,Rep,Inv#,Month,Year," +
                           "Inv Date,Fg Item#,Pro Code,Order #,Qty Shipped,Unit Price,UOM,Invoice Amt,BOL Whse" .

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
begin_cust-no end_cust-no begin_cust-type end_cust-type begin_ship-to ~
end_ship-to begin_slsmn end_slsmn begin_i-no end_i-no begin_inv-date ~
end_inv-date rd_sort tb_disprice tb_fin-chg sl_avail Btn_Def sl_selected ~
Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV tbAutoClose ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust-no end_cust-no ~
begin_cust-type end_cust-type begin_ship-to end_ship-to begin_slsmn ~
end_slsmn begin_i-no end_i-no begin_inv-date end_inv-date lbl_sort rd_sort ~
tb_disprice tb_fin-chg sl_avail sl_selected rd-dest fi_file tb_OpenCSV ~
tbAutoClose 

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

DEFINE VARIABLE begin_cust-no   AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_cust-type AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Cust Type" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_i-no      AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_inv-date  AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Invoice Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_ship-to   AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Ship-to#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn     AS CHARACTER FORMAT "XXX" 
    LABEL "Beginning Salesrep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no     AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust-type   AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Cust Type" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_i-no        AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_inv-date    AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Invoice Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ship-to     AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Ship-to #" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn       AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
    LABEL "Ending Salesrep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file         AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\SalesAnalysisByShipto.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_sort        AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page  AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name    AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no      AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt         AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 31 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest         AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 15 BY 4.52 NO-UNDO.

DEFINE VARIABLE rd_sort         AS CHARACTER INITIAL "Item#" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Customer#", "Customer#",
    "Ship-to#", "Ship-to#",
    "Item#", "Item#",
    "Order#", "Order#"
    SIZE 50 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 4.76.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 10.48.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_disprice  AS LOGICAL   INITIAL NO 
    LABEL "Show Discounted Prices?" 
    VIEW-AS TOGGLE-BOX
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_fin-chg   AS LOGICAL   INITIAL NO 
    LABEL "Include Finance Charges?" 
    VIEW-AS TOGGLE-BOX
    SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tb_cust-list AT ROW 2.05 COL 31 WIDGET-ID 6
    btnCustList AT ROW 2.1 COL 73.8 WIDGET-ID 8
    begin_cust-no AT ROW 3.24 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust-no AT ROW 3.24 COL 72 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_cust-type AT ROW 4.19 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Customer Type"
    end_cust-type AT ROW 4.19 COL 72 COLON-ALIGNED HELP
    "Enter Ending Customer Type"
    begin_ship-to AT ROW 5.14 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Ship-to Number"
    end_ship-to AT ROW 5.14 COL 72 COLON-ALIGNED HELP
    "Enter Ending Order Number"
    begin_slsmn AT ROW 6.1 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Sales Rep Number"
    end_slsmn AT ROW 6.1 COL 72 COLON-ALIGNED HELP
    "Enter Ending Sales Rep Number"
    begin_i-no AT ROW 7.05 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Order Number"
    end_i-no AT ROW 7.05 COL 72 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    begin_inv-date AT ROW 8 COL 29 COLON-ALIGNED
    end_inv-date AT ROW 8 COL 72 COLON-ALIGNED HELP
    "Enter Ending Due Date"
    lbl_sort AT ROW 9.62 COL 17.6 COLON-ALIGNED NO-LABELS
    rd_sort AT ROW 9.62 COL 28.6 NO-LABELS
    tb_disprice AT ROW 10.91 COL 17.8
    tb_fin-chg AT ROW 10.91 COL 49.8
    sl_avail AT ROW 12.91 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 12.91 COL 41 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 12.91 COL 61.8 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 13.91 COL 41 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 14.91 COL 41 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 15.95 COL 41 WIDGET-ID 40
    btn_down AT ROW 16.95 COL 41 WIDGET-ID 42
    rd-dest AT ROW 18.62 COL 5 NO-LABELS
    lv-font-no AT ROW 18.86 COL 33 COLON-ALIGNED
    lv-ornt AT ROW 18.86 COL 43 NO-LABELS
    lines-per-page AT ROW 18.86 COL 88 COLON-ALIGNED
    lv-font-name AT ROW 20.05 COL 30 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 21.05 COL 28.2
    fi_file AT ROW 22 COL 26.2 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 22 COL 92.8 RIGHT-ALIGNED
    tbAutoClose AT ROW 23.38 COL 28.2 WIDGET-ID 60
    btn-ok AT ROW 24.33 COL 28
    btn-cancel AT ROW 24.33 COL 52
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 12.19 COL 3 WIDGET-ID 38
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 18.19 COL 4
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 4.2
    BGCOLOR 15 
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 12.19 COL 61.4 WIDGET-ID 44
    RECT-6 AT ROW 18.62 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96.4 BY 28.1
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
        TITLE              = "Sales Analysis - By Inv/Cat/Shipto"
        HEIGHT             = 24.95
        WIDTH              = 96
        MAX-HEIGHT         = 33.29
        MAX-WIDTH          = 204.8
        VIRTUAL-HEIGHT     = 33.29
        VIRTUAL-WIDTH      = 204.8
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
    begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_cust-type:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_inv-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ship-to:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cust-type:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_inv-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_ship-to:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_disprice:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_fin-chg:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Sales Analysis - By Inv/Cat/Shipto */
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
ON WINDOW-CLOSE OF C-Win /* Sales Analysis - By Inv/Cat/Shipto */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
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


&Scoped-define SELF-NAME begin_cust-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-type C-Win
ON LEAVE OF begin_cust-type IN FRAME FRAME-A /* Beginning Cust Type */
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


&Scoped-define SELF-NAME begin_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv-date C-Win
ON LEAVE OF begin_inv-date IN FRAME FRAME-A /* Beginning Invoice Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME begin_ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship-to C-Win
ON HELP OF begin_ship-to IN FRAME FRAME-A /* Beginning Ship-to# */
    DO:
        DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

        RUN system/openLookup.p (
            INPUT  cocode,
            INPUT  "", /* Lookup ID */
            INPUT  122,  /* Subject ID */
            INPUT  "", /* User ID */
            INPUT  0,  /* Param Value ID */
            OUTPUT cFieldsValue,
            OUTPUT cFoundValue,
            OUTPUT recFoundRecID
            ).

        IF cFoundValue NE "" THEN 
        DO:
            begin_ship-to:SCREEN-VALUE = cFoundValue.
        END.
    END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship-to C-Win
ON LEAVE OF begin_ship-to IN FRAME FRAME-A /* Beginning Ship-to# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Salesrep# */
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
                    ELSE DO:
                        OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                    END.
                END. /* WHEN 3 THEN DO: */
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &type="Salesman"
                            &begin_cust=begin_slsmn
                            &END_cust= begin_slsmn
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:TITLE 
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:

                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = "Salesman"
                             &begin_cust= begin_slsmn
                             &END_cust=begin_slsmn
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Salesman"
                                  &begin_cust= begin_slsmn
                                  &END_cust=begin_slsmn
                                  &mail-subject= c-win:TITLE   
                                  &mail-body=  c-win:TITLE  
                                  &mail-file=list-name }
                    END.
                END.
            WHEN 6 THEN RUN OUTPUT-to-port.

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


&Scoped-define SELF-NAME end_cust-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-type C-Win
ON LEAVE OF end_cust-type IN FRAME FRAME-A /* Ending Cust Type */
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


&Scoped-define SELF-NAME end_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv-date C-Win
ON LEAVE OF end_inv-date IN FRAME FRAME-A /* Ending Invoice Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME end_ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ship-to C-Win
ON HELP OF end_ship-to IN FRAME FRAME-A /* Ending Ship-to# */
    DO:
        DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

        RUN system/openLookup.p (
            INPUT  cocode,
            INPUT  "", /* Lookup ID */
            INPUT  122,  /* Subject ID */
            INPUT  "", /* User ID */
            INPUT  0,  /* Param Value ID */
            OUTPUT cFieldsValue,
            OUTPUT cFoundValue,
            OUTPUT recFoundRecID
            ).

        IF cFoundValue NE "" THEN 
        DO:
            end_ship-to:SCREEN-VALUE = cFoundValue.
        END.
    END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ship-to C-Win
ON LEAVE OF end_ship-to IN FRAME FRAME-A /* Ending Ship-to # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Salesrep# */
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

        IF ll-ok THEN SELF:screen-value = ls-filename.
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


&Scoped-define SELF-NAME tb_disprice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_disprice C-Win
ON VALUE-CHANGED OF tb_disprice IN FRAME FRAME-A /* Show Discounted Prices? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fin-chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fin-chg C-Win
ON VALUE-CHANGED OF tb_fin-chg IN FRAME FRAME-A /* Include Finance Charges? */
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

    ASSIGN
        begin_inv-date = DATE(1,1,YEAR(TODAY))
        end_inv-date   = TODAY.
    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "HR13" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "HR13",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_cust-no.

    /* skb - 1/3/07 - defauting the values */
    /*     ASSIGN                                            */
    /*         rd-dest = 3:CHECKED     = TRUE                   */
    /*         tb_OpenCSV:CHECKED  = TRUE                   */
    /*         fi_file:SCREEN-VALUE = "c:\tmp\r-inctsh.csv". */
    END.

    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'HR13',
        INPUT NO,
        OUTPUT glCustListActive).
    {sys/inc/chblankcust.i ""HR13""}

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
            INPUT 'HR13',
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report C-Win 
PROCEDURE create-report :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid  AS   RECID            NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-03 LIKE tt-report.key-03 NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-04 LIKE tt-report.key-04 NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-10 LIKE tt-report.key-10 NO-UNDO.


    CREATE xreport.

    ASSIGN
        v-exc           = NO
        xreport.term-id = ""
        xreport.rec-id  = ip-recid
        xreport.key-01  = TRIM(IF v-sort1 EQ "Z" THEN cust.zip ELSE "") +
                   tt-report.key-09
        xreport.key-02  = IF v-sort1 EQ "H" THEN (v-shpz + v-ship) ELSE
                   IF v-sort1 EQ "S" THEN v-ship ELSE
                   IF v-sort1 EQ "O" THEN
                     STRING(ar-invl.ord-no,"999999") ELSE ""
        xreport.key-03  = ip-key-03
        xreport.key-04  = ip-key-04
        xreport.key-05  = v-ship
        xreport.key-06  = v-sman-no
        xreport.key-07  = xreport.key-03
        xreport.key-09  = tt-report.key-09
        xreport.key-10  = ip-key-10.

    IF xreport.key-02 EQ "" AND v-sort1 NE "I" THEN
        IF v-sort1 EQ "C" THEN
            xreport.key-02 = xreport.key-05.
        ELSE
            ASSIGN
                xreport.key-02 = xreport.key-03
                xreport.key-03 = xreport.key-05.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report1 C-Win 
PROCEDURE create-report1 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid  AS   RECID         NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-03 LIKE report.key-03 NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-04 LIKE report.key-04 NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-10 LIKE report.key-10 NO-UNDO.


    DO i = 1 TO 3:
        v-sman-no = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
        ELSE ar-invl.sman[i].

        IF v-sman-no   LT fsman                         OR
            v-sman-no   GT tsman                         OR
            (i NE 1 AND
            (v-sman-no EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.

        RUN create-report (ip-recid, ip-key-03, ip-key-04, ip-key-10).
        LEAVE.
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
        INPUT 'HR13').


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
    DISPLAY tb_cust-list begin_cust-no end_cust-no begin_cust-type end_cust-type 
        begin_ship-to end_ship-to begin_slsmn end_slsmn begin_i-no end_i-no 
        begin_inv-date end_inv-date lbl_sort rd_sort tb_disprice tb_fin-chg 
        sl_avail sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 tb_cust-list btnCustList begin_cust-no end_cust-no 
        begin_cust-type end_cust-type begin_ship-to end_ship-to begin_slsmn 
        end_slsmn begin_i-no end_i-no begin_inv-date end_inv-date rd_sort 
        tb_disprice tb_fin-chg sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* -------------------------------------------------------------------------- */
    /*                                                                            */
    /* -------------------------------------------------------------------------- */

    DEFINE VARIABLE v-qty          AS INTEGER   EXTENT 2 COLUMN-LABEL "Qty Shipped".
    DEFINE VARIABLE v-amt          AS DECIMAL   EXTENT 2 COLUMN-LABEL "Invoice Amt".
    DEFINE VARIABLE lv-r-no        LIKE oe-retl.r-no NO-UNDO.
    DEFINE VARIABLE lv-type        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-year         AS INTEGER   EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-month        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-cust         LIKE shipto.cust-no NO-UNDO.
    /* DEF VAR v-ship    LIKE shipto.ship-id NO-UNDO. */
    DEFINE VARIABLE v-city         LIKE shipto.ship-city NO-UNDO.
    DEFINE VARIABLE v-state        LIKE shipto.ship-state NO-UNDO.
    DEFINE VARIABLE v-sname        LIKE sman.sname NO-UNDO.
    DEFINE VARIABLE v-fgcat        LIKE fgcat.procat NO-UNDO.
    DEFINE VARIABLE v-delimiter    AS cha       FORM "x" INIT "," NO-UNDO.
    DEFINE VARIABLE li-seq         AS INTEGER   NO-UNDO.

    /*{sys/form/r-topw.f}*/
    DEFINE VARIABLE cDisplay       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-tit4       AS cha       FORM "x(400)" NO-UNDO.
    DEFINE VARIABLE str-tit5       AS cha       FORM "x(400)" NO-UNDO.
    DEFINE VARIABLE str-line       AS cha       FORM "x(400)" NO-UNDO.
    DEFINE VARIABLE v-bolwhs       AS CHARACTER NO-UNDO .
    DEFINE VARIABLE dtBolDate      AS DATE      NO-UNDO .
    DEFINE VARIABLE iLineCount     AS INTEGER   NO-UNDO .

    {sys/form/r-topl.f}  
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSelected   AS LOG       INIT YES NO-UNDO.


    FORM cust.cust-no       COLUMN-LABEL "!Customer!    #"
        v-year[1]          COLUMN-LABEL "! !Year"
        FORMAT "9999"
        v-name             COLUMN-LABEL "!Customer Buying!    Location"
        FORMAT "x(25)"
        tt-report.key-05   COLUMN-LABEL "! !Ship-to"
        FORMAT "x(8)"
        v-city             COLUMN-LABEL "!Cust Buy!Loc City"
        v-state            COLUMN-LABEL "Cust Buy!  Loc! State"
        v-sname            COLUMN-LABEL "! !Rep"
        w-data.inv-no      COLUMN-LABEL "!  Inv!Number"
        v-month            COLUMN-LABEL "! !Month"
        FORMAT "99"
        v-year[2]          COLUMN-LABEL "! !Year"
        FORMAT "9999"
        v-date             COLUMN-LABEL "! !Inv Date"
        FORMAT "99/99/9999"
        w-data.i-no        COLUMN-LABEL "! !FG Item#"
        v-fgcat            COLUMN-LABEL " House!Product! Code"
        v-ord              COLUMN-LABEL "!  SO!Number"
        FORMAT ">>>>>>"
        v-qty[1]           COLUMN-LABEL "!  Qty!Shipped"
        FORMAT "->>>,>>>,>>>"
        v-pric             COLUMN-LABEL "! Unit!Price"
        FORMAT "->>>,>>>,>>9.99<<"
        v-uom              COLUMN-LABEL "! !UOM"
        v-amt[1]           COLUMN-LABEL "!Invoice!  Amt"
        FORMAT "->,>>>,>>>,>>9.99"

        WITH NO-BOX FRAME itemx DOWN STREAM-IO WIDTH 210.


    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        str-tit2  = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        fcust     = begin_cust-no
        tcust     = end_cust-no
        fship     = begin_ship-to
        tship     = end_ship-to
        fsman     = begin_slsmn
        tsman     = END_slsmn
        fitem     = begin_i-no
        titem     = end_i-no
        fdate     = begin_inv-date
        tdate     = end_inv-date
        v-sort1   = SUBSTR(rd_sort,1,1) 
        v-disc-p  = tb_disprice              
        v-freight = NO
        v-inc-fc  = tb_fin-chg
        lSelected = tb_cust-list.

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

        IF LOOKUP(ttRptSelected.TextList, "Qty Shipped,Invoice Amt") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.
    IF lselected THEN 
    DO:
        FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
        IF AVAILABLE ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
        FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
        IF AVAILABLE ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
    END.
    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF rd-dest = 3 AND cFileName NE '' THEN 
    DO:
        OUTPUT STREAM st-excel TO VALUE(cFileName).
        PUT STREAM st-excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    /*",,,,,Cust Buy,,,,,,,House,,,,,"
    SKIP
    "Customer,,Customer Buying,,Cust Buy,Loc,,Inv,,,,,Product,SO,Qty,Unit,,Invoice"
    SKIP
    "#,Year,Location,Ship To,Loc City,State,Rep,Number,Month,Year,Inv Date,FG Item#,Code,Number,Shipped,Price,UOM,Amt"
    SKIP.*/
    END.

    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.
    PUT  str-tit4 FORMAT "x(400)"
        SKIP
        str-tit5 FORMAT "x(400)"
        SKIP .

    FOR EACH tt-report:
        DELETE tt-report.
    END.

    FOR EACH xreport:
        DELETE xreport.
    END.

    FOR EACH cust
        WHERE cust.company EQ cocode
        AND cust.cust-no GE fcust
        AND cust.cust-no LE tcust
        AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ cust.cust-no
        AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
        AND cust.type    GE begin_cust-type
        AND cust.type    LE end_cust-type
        USE-INDEX cust NO-LOCK:
        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
        {sa/sa-sls03.i "fdate" "tdate"}    
    END.

    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
        AND tt-report.key-01  EQ ""
        AND tt-report.key-02  EQ ""
        AND tt-report.key-03  EQ ""
        AND tt-report.key-04  EQ ""
        AND tt-report.key-05  EQ ""
        AND tt-report.key-06  EQ ""
        AND tt-report.key-07  EQ ""
        AND tt-report.key-08  EQ "",

        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ tt-report.key-09
        NO-LOCK

        TRANSACTION:
        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
        IF tt-report.key-10 EQ "ar-inv" THEN 
        DO:
            FIND ar-inv WHERE RECID(ar-inv) EQ tt-report.rec-id NO-LOCK.

            RUN ship-data.

            IF v-ship GE fship AND
                v-ship LE tship AND
                v-shpz GE fshpz AND
                v-shpz LE tshpz THEN 
            DO:

                FOR EACH ar-invl
                    WHERE ar-invl.x-no    EQ ar-inv.x-no
                    AND ar-invl.i-no    GE fitem
                    AND ar-invl.i-no    LE titem
                    AND (ar-invl.billable OR NOT ar-invl.misc)
                    USE-INDEX x-no NO-LOCK:

                    RUN create-report1 (RECID(ar-invl),
                        IF ar-invl.misc THEN ar-invl.i-name ELSE
                        IF ar-invl.i-no NE "" THEN ar-invl.i-no ELSE
                        "AR SALE",
                        STRING(ar-inv.inv-no,"99999999"), "").
                END.

                IF v-freight AND ar-inv.f-bill THEN 
                DO:
                    FIND FIRST ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no
                        USE-INDEX x-no NO-LOCK NO-ERROR.

                    IF AVAILABLE ar-invl THEN 
                    DO:
                        v-sman-no = "".

                        DO i = 1 TO 3:
                            IF ar-invl.sman[i] NE "" THEN 
                            DO:
                                v-sman-no = ar-invl.sman[i].
                                LEAVE.
                            END.
                        END.

                        IF v-sman-no EQ "" THEN v-sman-no = cust.sman.

                        IF "freight" GE fitem                      AND
                            "freight" LE titem                      AND
                            v-sman-no GE fsman                      AND
                            v-sman-no LE tsman                      THEN

                            RUN create-report (RECID(ar-invl), "FREIGHT",
                                STRING(ar-inv.inv-no,"99999999"), "FREIGHT").
                    END.
                END.
            END.

            DELETE tt-report.
        END.

        ELSE
            IF tt-report.key-10 EQ "ar-cashl" THEN 
            DO:
                FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK.
                FIND ar-cash  WHERE ar-cash.c-no    EQ ar-cashl.c-no NO-LOCK.

                ASSIGN
                    v-exc            = YES
                    tt-report.key-01 = TRIM(IF v-sort1 EQ "Z" THEN cust.zip ELSE "") +
                          tt-report.key-09
                    tt-report.key-02 = IF v-sort1 NE "I" THEN
                            (IF v-sort1 EQ "H" THEN cust.zip ELSE "") +
                             tt-report.key-09
                          ELSE ""
                    tt-report.key-03 = "MEMO"
                    tt-report.key-04 = STRING(ar-cashl.inv-no,"99999999")
                    tt-report.key-05 = tt-report.key-09
                    tt-report.key-06 = cust.sman
                    tt-report.key-07 = tt-report.key-03.

                RELEASE ar-inv.

                RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

                ASSIGN
                    lv-r-no = 0
                    lv-type = "".

                IF AVAILABLE reftable THEN
                    ASSIGN
                        lv-r-no = reftable.val[1]
                        lv-type = reftable.dscr.
                ELSE
                    IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
                        ASSIGN
                            lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
                            lv-type = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).

                IF lv-r-no NE 0 THEN 
                DO:
                    FIND FIRST oe-reth
                        WHERE oe-reth.company EQ cocode
                        AND oe-reth.r-no    EQ lv-r-no
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE oe-reth THEN
                        FIND FIRST ar-inv
                            WHERE ar-inv.company EQ cocode
                            AND ar-inv.cust-no EQ oe-reth.cust-no
                            AND ar-inv.inv-no  EQ oe-reth.inv-no
                            NO-LOCK NO-ERROR.
                END.       

                IF AVAILABLE ar-inv THEN 
                DO:
                    RUN ship-data.

                    IF v-ship GE fship AND
                        v-ship LE tship AND
                        v-shpz GE fshpz AND
                        v-shpz LE tshpz THEN
                        IF lv-type EQ "items" THEN 
                        DO:
                            RELEASE ar-invl.
                            FIND FIRST oe-retl
                                WHERE oe-retl.company EQ cocode
                                AND oe-retl.r-no    EQ oe-reth.r-no
                                AND oe-retl.line    EQ ar-cashl.line
                                AND oe-retl.i-no    GE fitem
                                AND oe-retl.i-no    LE titem
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE oe-retl THEN
                                FIND FIRST ar-invl
                                    WHERE ar-invl.company EQ cocode
                                    AND ar-invl.cust-no EQ ar-cash.cust-no
                                    AND ar-invl.inv-no  EQ ar-cashl.inv-no
                                    AND ar-invl.i-no    EQ oe-retl.i-no
                                    AND (ar-invl.billable OR NOT ar-invl.misc)
                                    NO-LOCK NO-ERROR.
                            IF AVAILABLE ar-invl THEN 
                            DO:
                                RUN create-report1 (RECID(ar-cashl), oe-retl.i-no,
                                    tt-report.key-04, "").

                                DELETE tt-report.
                            END.
                        END.

                        ELSE
                            IF lv-type   EQ "freight"                  AND
                                "freight" GE fitem                      AND
                                "freight" LE titem                      AND
                                cust.sman GE fsman                      AND
                                cust.sman LE tsman                      AND
                                v-freight                               THEN
                                ASSIGN
                                    v-exc            = NO
                                    tt-report.key-02 = IF v-sort1 NE "I" THEN v-ship ELSE ""
                                    tt-report.key-03 = "FREIGHT"
                                    tt-report.key-05 = v-ship.

                            ELSE
                                IF lv-type   EQ "tax"                  AND
                                    "tax"     GE fitem                  AND
                                    "tax"     LE titem                  AND
                                    cust.sman GE fsman                  AND
                                    cust.sman LE tsman                  THEN
                                    ASSIGN
                                        v-exc            = NO
                                        tt-report.key-02 = IF v-sort1 NE "I" THEN v-ship ELSE ""
                                        tt-report.key-03 = "TAX"
                                        tt-report.key-05 = v-ship.

                                ELSE
                                    IF ""        GE fitem AND
                                        ""        LE titem AND
                                        cust.sman GE fsman AND
                                        cust.sman LE tsman THEN v-exc = NO.
                END.

                ELSE
                    IF ""               GE fitem AND
                        ""               LE titem AND
                        cust.sman        GE fsman AND
                        cust.sman        LE tsman AND
                        ar-cashl.cust-no GE fship AND
                        ar-cashl.cust-no LE tship AND
                        cust.zip         GE fshpz AND
                        cust.zip         LE tshpz THEN v-exc = NO.

                IF AVAILABLE tt-report THEN 
                DO:
                    tt-report.key-07 = tt-report.key-03.

                    IF v-exc THEN DELETE tt-report.

                    ELSE
                        IF tt-report.key-02 EQ "" AND v-sort1 NE "I" THEN
                            IF v-sort1 EQ "C" THEN
                                tt-report.key-02 = tt-report.key-05.
                            ELSE
                                ASSIGN
                                    tt-report.key-02 = tt-report.key-03
                                    tt-report.key-03 = tt-report.key-05.
                END.     
            END.
    END.

    FOR EACH xreport NO-LOCK: /* Strange problem of tt-report*/
    END.

    FOR EACH tt-report WHERE tt-report.term-id EQ "",

        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ tt-report.key-09
        NO-LOCK

        BREAK BY tt-report.key-01
        BY tt-report.key-02
        BY tt-report.key-03
        BY tt-report.key-04
        BY tt-report.key-05

        WITH FRAME itemx DOWN

        TRANSACTION:
        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
        CREATE w-data.
        ASSIGN
            w-data.i-no   = tt-report.key-07
            w-data.inv-no = int(tt-report.key-04)
            w-data.rec-id = tt-report.rec-id.

        FIND FIRST ar-invl
            WHERE RECID(ar-invl) EQ w-data.rec-id
            NO-LOCK NO-ERROR.

        v-bolwhs = "" .
        dtBolDate = ? .

        FOR EACH oe-boll WHERE oe-boll.company = ar-invl.company
            AND oe-boll.b-no = ar-invl.b-no NO-LOCK:

            v-bolwhs = oe-boll.loc .
            dtBolDate = oe-boll.bol-date .
            IF v-bolwhs NE "" THEN LEAVE .

        END.

        IF AVAILABLE ar-invl THEN 
        DO:
            FIND ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK.
            ASSIGN
                v-date   = ar-inv.inv-date
                v-ord    = ar-invl.ord-no
                v-pric   = ar-invl.unit-pr
                v-uom    = ar-invl.pr-uom
                v-qty[1] = ar-invl.ship-qty
                v-amt[1] = ar-invl.amt
                v-disc   = ar-invl.disc
                v-pct    = 1.

            IF tt-report.key-10 EQ "FREIGHT" THEN
                ASSIGN
                    v-pric   = ar-inv.freight
                    v-uom    = ""
                    v-qty[1] = 0
                    v-amt[1] = ar-inv.freight
                    v-disc   = 0.

            ELSE 
            DO:
                DO i = 1 TO 3:
                    IF ar-invl.sman[i] EQ tt-report.key-06 THEN
                        ASSIGN
                            v-pct = ar-invl.s-pct[i] / 100
                            i     = 3.
                END.

                IF v-pct EQ 0 THEN
                DO i = 1 TO 3:
                    IF i EQ 1 THEN j = 0.
                    IF ar-invl.sman[i] NE "" THEN j = j + 1.
                    IF i EQ 3 THEN v-pct = 1 / j.
                END.

                IF v-pct LE 0 OR v-pct EQ ? THEN v-pct = 1.
            END.

            v-amt[1] = v-amt[1] * v-pct.
        END.

        ELSE 
        DO:
            FIND FIRST ar-cashl
                WHERE RECID(ar-cashl) EQ w-data.rec-id
                NO-LOCK NO-ERROR.

            IF AVAILABLE ar-cashl THEN 
            DO:
                FIND FIRST ar-cash WHERE ar-cash.c-no EQ ar-cashl.c-no NO-LOCK.

                ASSIGN
                    v-date   = ar-cash.check-date
                    v-ord    = 0
                    v-pric   = ar-cashl.amt-paid - ar-cashl.amt-disc
                    v-uom    = ""
                    v-qty[1] = 0
                    v-amt[1] = ar-cashl.amt-paid - ar-cashl.amt-disc
                    v-disc   = 0.

                RELEASE ar-invl.

                RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

                IF AVAILABLE oe-retl THEN 
                DO:
                    ASSIGN
                        v-ord    = oe-retl.ord-no
                        v-pric   = oe-retl.unit-pr
                        v-uom    = oe-retl.uom
                        v-qty[1] = - oe-retl.tot-qty-return.

                    FIND FIRST ar-invl
                        WHERE ar-invl.company EQ cocode
                        AND ar-invl.cust-no EQ ar-cash.cust-no
                        AND ar-invl.inv-no  EQ ar-cashl.inv-no
                        AND ar-invl.i-no    EQ oe-retl.i-no
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE ar-invl THEN 
                    DO:
                        /* Added for decimal problem */
                        ASSIGN 
                            v-pric = ar-invl.unit-pr.

                        DO i = 1 TO 3:
                            IF ar-invl.sman[i] EQ tt-report.key-06 THEN
                                ASSIGN
                                    v-pct = ar-invl.s-pct[i] / 100
                                    i     = 3.
                        END.

                        IF v-pct EQ 0 THEN
                        DO i = 1 TO 3:
                            IF i EQ 1 THEN j = 0.
                            IF ar-invl.sman[i] NE "" THEN j = j + 1.
                            IF i EQ 3 THEN v-pct = 1 / j.
                        END.

                        IF v-pct LE 0 OR v-pct EQ ? THEN v-pct = 1.
                        ASSIGN
                            v-amt[1] = v-amt[1] * v-pct
                            v-disc   = ar-invl.disc.
                    END.
                END.
            END.
        END.

        IF v-disc-p AND v-disc NE 0 THEN v-pric = v-pric * (100 - v-disc) / 100.

        FIND FIRST shipto
            WHERE shipto.company EQ cust.company
            AND shipto.cust-no EQ cust.cust-no
            AND shipto.ship-id EQ tt-report.key-05
            NO-LOCK NO-ERROR.
        
        ASSIGN
            v-name  = cust.name
            v-city  = cust.city
            v-state = cust.state.

        FIND FIRST itemfg
            WHERE itemfg.company EQ cust.company
            AND itemfg.i-no    EQ w-data.i-no
            NO-LOCK NO-ERROR.

        FIND FIRST sman
            WHERE sman.company EQ cust.company
            AND sman.sman    EQ tt-report.key-06
            NO-LOCK NO-ERROR.

        ASSIGN
            v-fgcat = IF AVAILABLE itemfg THEN itemfg.procat ELSE ""
            v-sname = IF AVAILABLE sman THEN sman.sname ELSE tt-report.key-06
            v-year  = YEAR(v-date)
            v-month = MONTH(v-date).

        IF iLineCount GE (lines-per-page - 10)  THEN 
        DO:
            PAGE.
            PUT str-tit4 FORMAT "x(400)"
                SKIP
                str-tit5 FORMAT "x(400)"
                SKIP .
            iLineCount = 0 .
        END.
        iLineCount = iLineCount + 1.
      
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
                    cVarValue = STRING(cust.cust-no,"x(8)") .
                /*WHEN "name"   THEN cVarValue = string(v-name,"x(30)").*/
                WHEN "shipto"   THEN 
                    cVarValue = STRING(tt-report.key-05,"x(8)").
                WHEN "city"  THEN 
                    cVarValue = STRING(v-city,"x(15)") .
                WHEN "stat"   THEN 
                    cVarValue = STRING(v-state,"x(5)") .
                WHEN "rep"  THEN 
                    cVarValue = STRING(v-sname,"x(20)") .
                WHEN "inv"   THEN 
                    cVarValue = STRING(w-data.inv-no,">>>>>>>>") .
                WHEN "month"  THEN 
                    cVarValue = STRING(v-month,">>>>>") .
                WHEN "year"    THEN 
                    cVarValue = STRING(v-year[2],"9999") .
                WHEN "inv-date"   THEN 
                    cVarValue = STRING(v-date,"99/99/99").
                WHEN "fg-item"   THEN 
                    cVarValue = STRING(w-data.i-no,"x(15)").
                WHEN "pro-code"  THEN 
                    cVarValue = STRING(v-fgcat,"x(8)") .
                WHEN "ord"   THEN 
                    cVarValue = STRING(v-ord,">>>>>>>>") .
                WHEN "qty-ship"  THEN 
                    cVarValue = STRING(v-qty[1],"->>>,>>>,>>9") .
                WHEN "unit-price"   THEN 
                    cVarValue = STRING(v-pric,"->>>,>>>,>>9.99") .
                WHEN "uom"  THEN 
                    cVarValue = STRING(v-uom,"x(4)") .
                WHEN "inv-amt"  THEN 
                    cVarValue = STRING(v-amt[1],"->,>>>,>>>,>>9.99") .
                WHEN "bol-whs"  THEN 
                    cVarValue = STRING(v-bolwhs) .
                WHEN "ship-name"  THEN 
                    cVarValue = IF AVAILABLE shipto THEN STRING(shipto.ship-name) ELSE "" .
                WHEN "ship-add1"  THEN 
                    cVarValue = IF AVAILABLE shipto THEN STRING(shipto.ship-addr[1]) ELSE "" .
                WHEN "ship-add2"  THEN 
                    cVarValue = IF AVAILABLE shipto THEN STRING(shipto.ship-addr[2]) ELSE "" .
                WHEN "ship-add3"  THEN 
                    cVarValue = IF AVAILABLE shipto THEN STRING(shipto.spare-char-3) ELSE "" .
                WHEN "ship-city"  THEN 
                    cVarValue = IF AVAILABLE shipto THEN STRING(shipto.ship-city) ELSE "" .
                WHEN "ship-st"  THEN 
                    cVarValue = IF AVAILABLE shipto THEN STRING(shipto.ship-state) ELSE "" .
                WHEN "ship-zip"  THEN 
                    cVarValue = IF AVAILABLE shipto THEN STRING(shipto.ship-zip) ELSE "" .
                WHEN "bol-date"  THEN 
                    cVarValue = IF dtBolDate NE ? THEN STRING(dtBolDate) ELSE "" .                           

            END CASE.

            IF  cTmpField = "inv-date" THEN
                 cExcelVarValue = IF v-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",v-date) ELSE "".
            ELSE IF  cTmpField = "bol-date" THEN
                 cExcelVarValue = IF dtBolDate NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",dtBolDate) ELSE "".
            ELSE cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cExcelVarValue)) + ",".            
        END.

        PUT UNFORMATTED cDisplay SKIP.
        IF rd-dest = 3 THEN 
        DO:
            PUT STREAM st-excel UNFORMATTED  
                cExcelDisplay SKIP.
        END.

        ASSIGN
            v-qty[2] = v-qty[2] + v-qty[1]
            v-amt[2] = v-amt[2] + v-amt[1].

        DELETE w-data.
    END.

    /* display final totals */
    PUT SKIP(1).
    iLineCount = iLineCount + 1.
    IF iLineCount GE (lines-per-page - 10)  THEN 
    DO:
        PAGE.
        PUT str-tit4 FORMAT "x(400)"
            SKIP
            str-tit5 FORMAT "x(400)"
            SKIP .
        iLineCount = 0 .
    END.     
  
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
            /*WHEN "name"   THEN cVarValue = "".*/
            WHEN "shipto"   THEN 
                cVarValue = "".
            WHEN "city"  THEN 
                cVarValue = "" .
            WHEN "stat"   THEN 
                cVarValue = "" .
            WHEN "rep"  THEN 
                cVarValue = "" .
            WHEN "inv"   THEN 
                cVarValue = "" .
            WHEN "month"  THEN 
                cVarValue = "" .
            WHEN "year"    THEN 
                cVarValue = "" .
            WHEN "inv-date"   THEN 
                cVarValue = "".
            WHEN "fg-item"   THEN 
                cVarValue = "".
            WHEN "pro-code"  THEN 
                cVarValue = "" .
            WHEN "ord"   THEN 
                cVarValue = "" .
            WHEN "qty-ship"  THEN 
                cVarValue = STRING(v-qty[2],"->>>,>>>,>>9") .
            WHEN "unit-price"   THEN 
                cVarValue = /*STRING(v-pric,"->>>,>>>,>>9.99")*/ "" .
            WHEN "uom"  THEN 
                cVarValue = "" .
            WHEN "inv-amt"  THEN 
                cVarValue = STRING(v-amt[2],"->,>>>,>>>,>>9.99") .
            WHEN "bol-whs"  THEN 
                cVarValue = "" .
            WHEN "ship-name"  THEN 
                cVarValue = "" .
            WHEN "ship-add1"  THEN 
                cVarValue = "" .
            WHEN "ship-add2"  THEN 
                cVarValue = "" .
            WHEN "ship-add3"  THEN 
                cVarValue = "" .
            WHEN "ship-city"  THEN 
                cVarValue = "" .
            WHEN "ship-st"    THEN 
                cVarValue = "" .
            WHEN "ship-zip"   THEN 
                cVarValue = "" .
            WHEN "bol-date"   THEN 
                cVarValue = "" . 

        END CASE.

        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT UNFORMATTED 
        "          Grand Totals:   " SUBSTRING(cDisplay,27,350) SKIP.
    IF rd-dest = 3 THEN 
    DO:
        PUT STREAM st-excel UNFORMATTED 
            'Grand Totals ,' SUBSTRING(cExcelDisplay,4,350) SKIP.
    END.

    IF rd-dest = 3 AND cFileName NE '' THEN
        OUTPUT STREAM st-excel CLOSE.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ship-data C-Win 
PROCEDURE ship-data :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RELEASE shipto.
    IF ar-inv.ship-id NE "" THEN
        FIND FIRST shipto
            WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ ar-inv.cust-no
            AND shipto.ship-id EQ ar-inv.ship-id
            NO-LOCK NO-ERROR.
    IF AVAILABLE shipto THEN
        ASSIGN
            v-ship = ar-inv.ship-id
            v-shpz = shipto.ship-zip.

    ELSE
        IF ar-inv.sold-id NE "" THEN
            ASSIGN
                v-ship = ar-inv.sold-id
                v-shpz = ar-inv.sold-zip.

        ELSE
            ASSIGN
                v-ship = ar-inv.cust-no
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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE    NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-label      AS CHARACTER NO-UNDO.

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
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + ",".
            ELSE 
            DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE uncomma C-Win 
PROCEDURE uncomma :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ip-char AS CHARACTER NO-UNDO.

    ip-char = REPLACE(ip-char,',',' ').
/*
DEF VAR li AS INT NO-UNDO.

DO li = 1 TO LENGTH(ip-char):
  IF SUBSTR(ip-char,li,1) EQ "," THEN SUBSTR(ip-char,li,1) = " ".
END.
*/
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
            fi_file:SCREEN-VALUE = "c:\tmp\SalesAnalysisByShipto.csv".    
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

