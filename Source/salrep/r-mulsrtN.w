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

DEFINE VARIABLE fcust       AS ch        INIT "" NO-UNDO.
DEFINE VARIABLE tcust       LIKE fcust INIT "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE fship       AS ch        INIT "" NO-UNDO.
DEFINE VARIABLE tship       LIKE fcust INIT "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE fshpz       LIKE ar-inv.zip INIT "" NO-UNDO.
DEFINE VARIABLE tshpz       LIKE fshpz INIT "zzzzzzzzzz" NO-UNDO.
DEFINE VARIABLE fsman       AS CHARACTER FORMAT "x(3)" INIT "" NO-UNDO.
DEFINE VARIABLE tsman       LIKE fsman INIT "zzz" NO-UNDO.
DEFINE VARIABLE fdate       AS DATE      FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE tdate       LIKE fdate NO-UNDO.
DEFINE VARIABLE v-det       AS LOG       FORMAT "Detail/Summary" INIT YES NO-UNDO.
DEFINE VARIABLE v-sort      AS CHARACTER FORMAT "x(3)" INIT "" NO-UNDO.
DEFINE VARIABLE v-inc-fc    AS LOG       INIT NO NO-UNDO.

DEFINE VARIABLE v-sort-list AS CHARACTER INIT "CIDSZOM" NO-UNDO.
DEFINE VARIABLE v-sort-desc AS CHARACTER INIT
    "Customer,Invoice#,Invoice Date,Ship To,Ship To Zip,Order#,SalesRep" NO-UNDO.

DEFINE VARIABLE v-date      LIKE ar-inv.inv-date NO-UNDO.
DEFINE VARIABLE v-ord       LIKE ar-invl.ord-no NO-UNDO.
DEFINE VARIABLE v-pric      LIKE ar-invl.unit-pr NO-UNDO.
DEFINE VARIABLE v-uom       LIKE ar-invl.pr-uom NO-UNDO.

DEFINE VARIABLE v-sman-no   AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE v-qty       AS INTEGER   EXTENT 6 NO-UNDO.
DEFINE VARIABLE v-amt       AS DECIMAL   EXTENT 6 NO-UNDO.
DEFINE VARIABLE v-exc       AS LOG       NO-UNDO.
DEFINE VARIABLE v-name      LIKE cust.name FORMAT "x(21)" NO-UNDO.
DEFINE VARIABLE v-hdr       AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-tot-wght  AS DECIMAL   EXTENT 6 NO-UNDO.

DEFINE VARIABLE v-pct       AS DECIMAL   FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE v-fac       AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-ship      LIKE ar-inv.ship-id NO-UNDO.
DEFINE VARIABLE v-shpz      LIKE ar-inv.sold-zip NO-UNDO.
DEFINE VARIABLE v-total     AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD i-no   LIKE ar-invl.i-no
    FIELD inv-no LIKE ar-invl.inv-no
    FIELD rec-id AS RECID.

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.

DEFINE BUFFER xtt-report FOR tt-report.

DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.

DEFINE STREAM excel.
DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE glCustListActive   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.
                     
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

                     

ASSIGN 
    cTextListToSelect  = "Customer,Name,SRep,Ship To#,Ship Zip,Inv#,Inv Date," 
                           + "Order#,Item No,Item Name,Item Description,Cust Part#,"
                           + "Price,UOM,Qty,Inv Amt,Inv Weight"
    cFieldListToSelect = "cust,name,rep,shipto,ship-zip,inv#,inv-date," +
                            "ord,item-no,item-name,desc,cust-part," +
                            "price,uom,qty,inv-amt,inv-wt"
    cFieldLength       = "8,30,4,8,8,6,8," + "8,15,30,30,15," + "13,4,11,14,10"
    cFieldType         = "c,c,c,c,c,i,c," + "i,c,c,c,c," + "i,c,i,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Customer,Name,SRep,Ship To#,Ship Zip,Inv#,Inv Date," 
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
rd-dest tb_OpenCSV tbAutoClose fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust-no end_cust-no ~
begin_ship-to end_ship-to begin_ship-to-zip end_ship-to-zip begin_slsmn ~
end_slsmn begin_inv-date end_inv-date tb_cust-total tb_shipto-total fi_sort ~
tb_fin-chg sl_avail sl_selected rd-dest tb_OpenCSV fi_file tbAutoClose

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
    ( INPUT picust-no AS CHARACTER,
    INPUT pi-inv-no AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel /*AUTO-END-KEY */
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
    SIZE 16 BY 1.10.

DEFINE BUTTON Btn_Def 
    LABEL "&Default" 
    SIZE 16 BY 1.10.

DEFINE BUTTON btn_down 
    LABEL "Move Down" 
    SIZE 16 BY 1.10.

DEFINE BUTTON Btn_Remove 
    LABEL "<< &Remove" 
    SIZE 16 BY 1.10.

DEFINE BUTTON btn_Up 
    LABEL "Move Up" 
    SIZE 16 BY 1.10.

DEFINE VARIABLE begin_cust-no     AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_inv-date    AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Invoice Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_ship-to     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Ship-To#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ship-to-zip AS CHARACTER FORMAT "X(10)":U 
    LABEL "Beginning Ship-To Zip" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn       AS CHARACTER FORMAT "XXX":U 
    LABEL "Beginning Salesrep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no       AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_inv-date      AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Invoice Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ship-to       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Ship-To#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ship-to-zip   AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
    LABEL "Ending Ship-To Zip" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn         AS CHARACTER FORMAT "XXX":U INITIAL "zzz" 
    LABEL "Ending Salesrep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file           AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\SalesMultipleSorts.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1
    .

DEFINE VARIABLE fi_sort           AS CHARACTER FORMAT "XXX" 
    LABEL "Sort By?" 
    VIEW-AS FILL-IN 
    SIZE 8 BY 1.

DEFINE VARIABLE lines-per-page    AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name      AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no        AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt           AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest           AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 16 BY 4.71 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.48.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 10.71.

DEFINE VARIABLE sl_avail        AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.
     
DEFINE VARIABLE tbAutoClose     AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.     

DEFINE VARIABLE tb_cust-list    AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE tb_cust-total   AS LOGICAL   INITIAL YES 
    LABEL "Show Customer Totals?" 
    VIEW-AS TOGGLE-BOX
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel        AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_fin-chg      AS LOGICAL   INITIAL NO 
    LABEL "Include Finance Charges?" 
    VIEW-AS TOGGLE-BOX
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV      AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE tb_shipto-total AS LOGICAL   INITIAL YES 
    LABEL "Show Shipto Totals?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24.8 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm    AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tb_cust-list AT ROW 2 COL 31 WIDGET-ID 6
    btnCustList AT ROW 2 COL 73.8 WIDGET-ID 8
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
    tb_cust-total AT ROW 8.1 COL 57 RIGHT-ALIGNED WIDGET-ID 2
    tb_shipto-total AT ROW 8.1 COL 86 RIGHT-ALIGNED WIDGET-ID 4
    fi_sort AT ROW 9.29 COL 29 COLON-ALIGNED HELP
    "Choose up to three categories to sort by."
    tb_fin-chg AT ROW 11.19 COL 33
    sl_avail AT ROW 13.19 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 13.29 COL 40.4 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 13.19 COL 60.4 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 14.29 COL 40.4 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 15.29 COL 40.4 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 16.24 COL 40.4 WIDGET-ID 40
    btn_down AT ROW 17.24 COL 40.4 WIDGET-ID 42
    rd-dest AT ROW 19.19 COL 5 NO-LABELS
    lv-ornt AT ROW 19.1 COL 43 NO-LABELS
    lines-per-page AT ROW 19.1 COL 87 COLON-ALIGNED
    lv-font-no AT ROW 19.1 COL 43 COLON-ALIGNED
    lv-font-name AT ROW 20.05 COL 29 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 21.91 COL 28
    tb_excel AT ROW 21.24 COL 90 RIGHT-ALIGNED
    tb_OpenCSV AT ROW 22.91 COL 92.4 RIGHT-ALIGNED
    fi_file AT ROW 22.81 COL 26 COLON-ALIGNED HELP
    "Enter File Name"
    tbAutoClose AT ROW 24.33 COL 28 WIDGET-ID 64     
    btn-ok AT ROW 25.29 COL 28
    btn-cancel AT ROW 25.29 COL 54
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 12.48 COL 60.4 WIDGET-ID 44
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
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 18.43 COL 4.6
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 4.5
    BGCOLOR 15 
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
    SIZE 29 BY .62 AT ROW 12.48 COL 3 WIDGET-ID 38
    RECT-6 AT ROW 18.86 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 29.71
    BGCOLOR 15.


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
        HEIGHT             = 25.86
        WIDTH              = 95.2
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
    begin_inv-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ship-to:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ship-to-zip:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_inv-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_ship-to:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_ship-to-zip:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lv-font-name:HIDDEN IN FRAME FRAME-A = TRUE.   
   
/* SETTINGS FOR TOGGLE-BOX tb_cust-total IN FRAME FRAME-A
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
    tb_cust-total:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-ornt:HIDDEN IN FRAME FRAME-A = TRUE.
       
/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lines-per-page:HIDDEN IN FRAME FRAME-A = TRUE.
       
/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-font-no:HIDDEN IN FRAME FRAME-A = TRUE.       
       
/* SETTINGS FOR TOGGLE-BOX tb_shipto-total IN FRAME FRAME-A
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
    tb_shipto-total:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sales With Multiple Sorts */
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
ON WINDOW-CLOSE OF C-Win /* Sales With Multiple Sorts */
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
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
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
ON LEAVE OF begin_ship-to IN FRAME FRAME-A /* Beginning Ship-To# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ship-to-zip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship-to-zip C-Win
ON LEAVE OF begin_ship-to-zip IN FRAME FRAME-A /* Beginning Ship-To Zip */
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
        SESSION:SET-WAIT-STATE ("general").

        ASSIGN {&DISPLAYED-OBJECTS}.
  
        IF rd-dest = 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.

        IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "southpak"
            THEN is-xprint-form = YES.     
        ELSE is-xprint-form = NO.

        FIND FIRST  ttCustList NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttCustList AND tb_cust-list THEN 
        DO:
            EMPTY TEMP-TABLE ttCustList.
            RUN BuildCustList(INPUT cocode,
                INPUT tb_cust-list AND glCustListActive ,
                INPUT begin_cust-no,
                INPUT END_cust-no).
        END.       

        RUN GetSelectionList.
        RUN run-report. 
        STATUS DEFAULT "Processing Complete".
        SESSION:SET-WAIT-STATE ("").

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
                    {custom/asifax.i &type="Salesman"
                            &begin_cust=begin_slsmn
                            &END_cust= begin_slsmn
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = "Salesman"
                             &begin_cust= begin_slsmn
                             &END_cust=begin_slsmn
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Salesman"
                                  &begin_cust= begin_slsmn
                                  &END_cust=begin_slsmn
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
                    END.
                END.
            WHEN 6 THEN RUN OUTPUT-to-port.

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


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
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
ON LEAVE OF end_ship-to IN FRAME FRAME-A /* Ending Ship-To# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ship-to-zip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ship-to-zip C-Win
ON LEAVE OF end_ship-to-zip IN FRAME FRAME-A /* Ending Ship-To Zip */
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

        IF ll-ok THEN SELF:SCREEN-VALUE = ls-filename.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_sort C-Win
ON LEAVE OF fi_sort IN FRAME FRAME-A /* Sort By? */
    DO:
        ASSIGN {&self-name}.

        ASSIGN
            tb_cust-total:SCREEN-VALUE   = IF INDEX(fi_sort:SCREEN-VALUE,"C") NE 0 THEN "YES" ELSE "NO"
            tb_shipto-total:SCREEN-VALUE = IF INDEX(fi_sort:SCREEN-VALUE,"S") NE 0 THEN "YES" ELSE "NO".
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
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Auto Run Excel? */
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
        begin_inv-date = DATE(01,01,YEAR(TODAY))
        END_inv-date   = TODAY
        fi_file        = "c:\tmp\multisal.csv".
    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "HR4" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
  
    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "HR10",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    ASSIGN
        tb_cust-total:SCREEN-VALUE   = IF INDEX(fi_sort:SCREEN-VALUE,"C") NE 0 THEN "YES" ELSE "NO"
        tb_shipto-total:SCREEN-VALUE = IF INDEX(fi_sort:SCREEN-VALUE,"S") NE 0 THEN "YES" ELSE "NO".

    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'HR10',
        INPUT NO,
        OUTPUT glCustListActive).
    {sys/inc/chblankcust.i ""HR10""}

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
            INPUT 'HR10',
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
    DISPLAY tb_cust-list begin_cust-no end_cust-no begin_ship-to end_ship-to 
        begin_ship-to-zip end_ship-to-zip begin_slsmn end_slsmn begin_inv-date 
        end_inv-date tb_cust-total tb_shipto-total fi_sort tb_fin-chg sl_avail 
        sl_selected rd-dest tbAutoClose tb_OpenCSV fi_file 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 tb_cust-list btnCustList begin_cust-no end_cust-no 
        begin_ship-to end_ship-to begin_ship-to-zip end_ship-to-zip 
        begin_slsmn end_slsmn begin_inv-date end_inv-date fi_sort tb_fin-chg 
        sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down 
        rd-dest tbAutoClose tb_OpenCSV fi_file btn-ok btn-cancel 
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
    DEFINE INPUT  PARAMETER ip-sort AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER op-sort AS CHARACTER NO-UNDO.
  
  
    op-sort = IF ip-sort EQ "C" THEN ar-inv.cust-no                       ELSE
        IF ip-sort EQ "I" THEN STRING(ar-inv.inv-no,"9999999999")   ELSE
        IF ip-sort EQ "D" THEN STRING(YEAR(ar-inv.inv-date),"9999") +
        string(MONTH(ar-inv.inv-date),"99")  +
        string(DAY(ar-inv.inv-date),"99")    ELSE
        IF ip-sort EQ "S" THEN v-ship                               ELSE
        IF ip-sort EQ "Z" THEN v-shpz                               ELSE
        IF ip-sort EQ "O" THEN STRING(ar-invl.ord-no,"9999999999")  ELSE
        IF ip-sort EQ "R" THEN v-sman-no                            ELSE "".
                                   
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
    DEFINE INPUT  PARAMETER ip-sort AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER op-sort AS CHARACTER NO-UNDO.
  
  
    op-sort = IF ip-sort EQ "C" THEN ar-cash.cust-no                      ELSE
        IF ip-sort EQ "I" THEN STRING(ar-cashl.inv-no,"9999999999") ELSE
        IF ip-sort EQ "D" THEN STRING(YEAR(ar-cash.check-date),"9999") +
        string(MONTH(ar-cash.check-date),"99")  +
        string(DAY(ar-cash.check-date),"99") ELSE
        IF ip-sort EQ "S" THEN v-ship                               ELSE
        IF ip-sort EQ "Z" THEN v-shpz                               ELSE
        IF ip-sort EQ "O" THEN "0000000000"                         ELSE
        IF ip-sort EQ "R" THEN cust.sman                            ELSE "".
                
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE report-from-inv C-Win 
PROCEDURE report-from-inv :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO i = 1 TO 3:
        v-sman-no = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman  
        ELSE ar-invl.sman[i].  
        /*   IF ar-invl.sman[i] <> "" THEN
               ASSIGN v-sman-no = ar-invl.sman[i]. */

        IF v-sman-no  LT fsman                          OR
            v-sman-no  GT tsman                          OR
            (i NE 1 AND
            (v-sman-no EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.

        CREATE xtt-report.

        ASSIGN
            xtt-report.rec-id = RECID(ar-invl)
            xtt-report.key-04 = STRING(ar-invl.inv-no,"9999999999")
            xtt-report.key-05 = v-sman-no
            xtt-report.key-06 = STRING(ar-invl.ord-no,"9999999999")
            xtt-report.key-07 = IF ar-invl.misc THEN ar-invl.i-name ELSE
                         IF ar-invl.i-no NE "" THEN ar-invl.i-no ELSE
                         "AR SALE"
            xtt-report.key-08 = v-ship
            xtt-report.key-09 = tt-report.key-09
            xtt-report.key-10 = v-shpz.
           
        RUN get-sort-fields1 (substr(v-sort,1,1), OUTPUT xtt-report.key-01).
        RUN get-sort-fields1 (substr(v-sort,2,1), OUTPUT xtt-report.key-02).
        RUN get-sort-fields1 (substr(v-sort,3,1), OUTPUT xtt-report.key-03).
        LEAVE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* -------------------------------------------------------------------------- */
    /*                                                                            */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-top3lw.f}*/

    DEFINE VARIABLE lv-r-no        LIKE oe-retl.r-no NO-UNDO.
    DEFINE VARIABLE lv-type        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSman          LIKE ar-invl.sman[1] NO-UNDO.

    DEFINE VARIABLE cDisplay       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-tit4       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line       AS cha       FORM "x(300)" NO-UNDO.

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSelected   AS LOG       INIT YES NO-UNDO.


    FORM SPACE(8)
        w-data.i-no
        itemfg.i-name        FORMAT "x(20)"
        ar-invl.part-dscr1   FORMAT "x(20)"
        ar-invl.part-no
        v-pric               FORMAT "->,>>>,>>9.99<<"
        v-uom
        v-qty[1]             FORMAT "->>,>>>,>>>"          LABEL "Qty"
        v-amt[1]             FORMAT "->>,>>>,>>9.99"       LABEL "Inv Amt"

        WITH NO-BOX FRAME detail DOWN STREAM-IO WIDTH 132.
    
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
    
    FORM cust.cust-no
        cust.name
        tt-report.key-05     FORMAT "x(3)"
        SPACE(2)
        tt-report.key-08     FORMAT "x(8)"
        tt-report.key-10     FORMAT "x(10)"
        w-data.inv-no
        v-date               FORMAT "99/99/99"
        v-ord                FORMAT ">>>>>>"
        SPACE(14)
        v-qty[2]             FORMAT "->>,>>>,>>>"
        v-amt[2]             FORMAT "->>,>>>,>>9.99"
        SKIP(1)
        WITH NO-BOX NO-LABELS FRAME summary DOWN STREAM-IO WIDTH 132.

  
    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        str-tit2  = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        fcust     = begin_cust-no
        tcust     = end_cust-no
        fship     = begin_ship-to
        tship     = END_ship-to
        fshpz     = begin_ship-to-zip
        tshpz     = END_ship-to-zip
        fsman     = begin_slsmn
        tsman     = end_slsmn
        fdate     = begin_inv-date
        tdate     = END_inv-date
        /*v-det       = tb_detailed*/
        v-inc-fc  = tb_fin-chg
        v-sort    = fi_SORT
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

        IF LOOKUP(ttRptSelected.TextList, "Qty,Inv Amt,Inv Weight") <> 0    THEN
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

    {sys/inc/outprint.i value(lines-per-page)}

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        /*IF v-det THEN
           excelheader = "Customer,Name,SRep,Ship To#,Ship Zip,Inv#,Inv Date,"
                       + "Order#,Item No,Item Name,Item Description,Cust Part#,"
                       + "Price,UOM,Qty,Inv Amt".
        ELSE
           excelheader = "Customer,Name,SRep,Ship To#,Ship Zip,Inv#,Inv Date,"
                       + "Order#,Qty,Inv Amt".*/
      
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.
   
    DISPLAY "" WITH FRAME r-top.
  
    ASSIGN
        v-qty      = 0
        v-amt      = 0
        v-tot-wght = 0   .

    FOR EACH tt-report:
        DELETE tt-report.
    END.

    FOR EACH ar-inv
        WHERE ar-inv.company  EQ cocode
        AND ar-inv.posted   EQ YES
        AND ar-inv.cust-no  GE fcust
        AND ar-inv.cust-no  LE tcust
        AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ ar-inv.cust-no
        AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
        AND ar-inv.inv-date GE fdate
        AND ar-inv.inv-date LE tdate
        AND (ar-inv.type    NE "FC" OR v-inc-fc)
        NO-LOCK:
        {custom/statusMsg.i " 'Processing Customer#  '  + ar-inv.cust-no "}                          

        CREATE tt-report.

  
        ASSIGN
            tt-report.key-09 = ar-inv.cust-no
            tt-report.key-10 = "ar-inv"
            tt-report.rec-id = RECID(ar-inv).
     
    END.

    FOR EACH cust
        WHERE cust.company EQ cocode
        AND cust.cust-no GE fcust
        AND cust.cust-no LE tcust
        AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ cust.cust-no
        AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
        NO-LOCK,

        EACH ar-cash
        WHERE ar-cash.company    EQ cocode
        AND ar-cash.cust-no    EQ cust.cust-no
        AND ar-cash.check-date GE fdate
        AND ar-cash.check-date LE tdate
        AND ar-cash.posted     EQ YES
        NO-LOCK,

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
        CREATE tt-report.
        ASSIGN
            tt-report.key-09 = cust.cust-no
            tt-report.key-10 = "ar-cashl"
            tt-report.rec-id = RECID(ar-cashl).
    
    END.
        
    FOR EACH tt-report
        WHERE tt-report.key-01 EQ ""
        AND tt-report.key-02 EQ ""
        AND tt-report.key-03 EQ ""
        AND tt-report.key-04 EQ ""
        AND tt-report.key-05 EQ ""
        AND tt-report.key-06 EQ ""
        AND tt-report.key-07 EQ ""
        AND tt-report.key-08 EQ "",

        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ tt-report.key-09
        NO-LOCK

        TRANSACTION:
        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
        IF tt-report.key-10 EQ "ar-inv" THEN 
        DO:
        
            FIND ar-inv WHERE RECID(ar-inv) EQ tt-report.rec-id NO-LOCK.
      
            RUN ship-info.

            IF v-ship GE fship AND
                v-ship LE tship AND
                v-shpz GE fshpz AND
                v-shpz LE tshpz THEN
                FOR EACH ar-invl
                    WHERE ar-invl.x-no EQ ar-inv.x-no
                    AND (ar-invl.billable OR NOT ar-invl.misc)
                    NO-LOCK:
                    RUN report-from-inv. 
                END.

            DELETE tt-report.
        END.

        ELSE
            IF tt-report.key-10 EQ "ar-cashl" THEN 
            DO:
                FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK.
                FIND ar-cash  WHERE ar-cash.c-no    EQ ar-cashl.c-no NO-LOCK.

                ASSIGN 
                    cSman = getInvSman(cust.cust-no ,ar-cashl.inv-no).

                ASSIGN
                    v-exc            = YES
                    tt-report.key-04 = STRING(ar-cashl.inv-no,"9999999999")
                    tt-report.key-05 = cSman /*cust.sman  string(ar-inv.sman-no) */
                    tt-report.key-06 = "0000000000"
                    tt-report.key-07 = "MEMO".

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
             
                    RUN ship-info.

                    IF v-ship GE fship AND
                        v-ship LE tship AND
                        v-shpz GE fshpz AND
                        v-shpz LE tshpz THEN

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

                        IF lv-type   EQ "freight"                  AND
                            cust.sman GE fsman                      AND
                            cust.sman LE tsman                      THEN
                            ASSIGN
                                v-exc            = NO
                                tt-report.key-07 = "FREIGHT"
                                tt-report.key-08 = v-ship
                                tt-report.key-10 = v-shpz.

                        ELSE
                            IF lv-type   EQ "tax"                  AND
                                cust.sman GE fsman                  AND
                                cust.sman LE tsman                  THEN
                                ASSIGN
                                    v-exc            = NO
                                    tt-report.key-07 = "TAX"
                                    tt-report.key-08 = v-ship
                                    tt-report.key-10 = v-shpz.

                            ELSE
                                IF cust.sman GE fsman AND
                                    cust.sman LE tsman THEN
                                    ASSIGN
                                        v-exc            = NO
                                        tt-report.key-08 = v-ship
                                        tt-report.key-10 = v-shpz.
                END.

                ELSE
                    IF cust.sman    GE fsman AND
                        cust.sman    LE tsman AND
                        cust.cust-no GE fship AND
                        cust.cust-no LE tship AND
                        cust.zip     GE fshpz AND
                        cust.zip     LE tshpz THEN
                        ASSIGN
                            v-exc            = NO
                            tt-report.key-08 = cust.cust-no
                            tt-report.key-10 = cust.zip.
         
                IF AVAILABLE tt-report THEN 
                DO:
                    IF v-exc THEN DELETE tt-report.
      
                    ELSE 
                    DO:
                        RUN get-sort-fields2 (substr(v-sort,1,1), OUTPUT tt-report.key-01).
                        RUN get-sort-fields2 (substr(v-sort,2,1), OUTPUT tt-report.key-02).
                        RUN get-sort-fields2 (substr(v-sort,3,1), OUTPUT tt-report.key-03).
                    END.
                END.
            END.
    END.

    FOR EACH xtt-report:
      
    END.

    FOR EACH tt-report,

        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ tt-report.key-09
        NO-LOCK

        BREAK BY tt-report.key-01
        BY tt-report.key-02
        BY tt-report.key-03
        BY tt-report.key-04
        BY tt-report.key-05
        BY tt-report.key-06

        TRANSACTION:

        CREATE w-data.
        ASSIGN
            w-data.i-no   = tt-report.key-07
            w-data.inv-no = int(tt-report.key-04)
            w-data.rec-id = tt-report.rec-id.
     
        FIND FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ w-data.i-no
            NO-LOCK NO-ERROR.
     
        FIND FIRST ar-invl
            WHERE RECID(ar-invl) EQ w-data.rec-id
            NO-LOCK NO-ERROR.

        IF AVAILABLE ar-invl THEN 
        DO:
            FIND ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK.
            ASSIGN
                v-date        = ar-inv.inv-date
                v-ord         = ar-invl.ord-no
                v-pric        = ar-invl.unit-pr
                v-uom         = ar-invl.pr-uom
                v-qty[1]      = ar-invl.ship-qty
                v-amt[1]      = ar-invl.amt
                v-tot-wght[1] = ar-invl.t-weight 
                v-pct         = 1.

            DO i = 1 TO 3:
                IF ar-invl.sman[i] EQ tt-report.key-05 THEN
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
                    v-amt[1] = ar-cashl.amt-paid - ar-cashl.amt-disc.

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
                            IF ar-invl.sman[i] EQ tt-report.key-05 THEN
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

                        v-amt[1] = v-amt[1] * v-pct.
                    END.
                END.
            END.
        END.

        /*  IF w-data.inv-no NE 0 THEN
              FIND FIRST inv-head WHERE inv-head.company EQ cocode
              AND inv-head.inv-no EQ w-data.inv-no NO-LOCK NO-ERROR .
      
          IF AVAIL ar-invl THEN
              ASSIGN v-tot-wght[1] = ar-invl.t-weight .                                      */

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
                WHEN "name"   THEN 
                    cVarValue = STRING(cust.NAME,"x(30)").
                WHEN "rep"   THEN 
                    cVarValue = STRING(tt-report.key-05,"x(3)").
                WHEN "shipto"  THEN 
                    cVarValue = STRING(tt-report.key-08,"x(8)") .
                WHEN "ship-zip"   THEN 
                    cVarValue = STRING(tt-report.key-10,"x(8)") .
                WHEN "inv#"  THEN 
                    cVarValue = STRING(w-data.inv-no,">>>>>>") .
                WHEN "inv-date"   THEN 
                    cVarValue = STRING(v-date,"99/99/99") .
                WHEN "ord"  THEN 
                    cVarValue = STRING(v-ord,">>>>>>>>") .
                WHEN "item-no"    THEN 
                    cVarValue = IF AVAILABLE itemfg THEN STRING(itemfg.i-no,"x(15)")  ELSE STRING(w-data.i-no,"x(15)").
                WHEN "item-name"   THEN 
                    cVarValue = IF AVAILABLE itemfg THEN STRING(itemfg.i-name,"x(30)")  ELSE IF AVAILABLE ar-invl THEN STRING(ar-invl.i-name,"x(30)") ELSE "".
                WHEN "desc"   THEN 
                    cVarValue = IF AVAILABLE itemfg THEN STRING(itemfg.part-dscr1,"x(30)")  ELSE IF AVAILABLE ar-invl AND ar-invl.i-dscr EQ "" THEN STRING(ar-invl.part-dscr1,"x(30)") ELSE IF AVAILABLE ar-invl THEN STRING(ar-invl.i-dscr,"x(30)")  ELSE "" .
                WHEN "cust-part"  THEN 
                    cVarValue = IF AVAILABLE ar-invl THEN STRING(ar-invl.part-no,"x(15)") ELSE "".
                WHEN "price"   THEN 
                    cVarValue = STRING(v-pric,"->,>>>,>>9.99<<") .
                WHEN "uom"  THEN 
                    cVarValue = STRING(v-uom,"x(4)") .
                WHEN "qty"   THEN 
                    cVarValue = STRING(v-qty[1],"->>,>>>,>>>") .
                WHEN "inv-amt"  THEN 
                    cVarValue = STRING(v-amt[1],"->>,>>>,>>9.99") .
                WHEN "inv-wt"  THEN 
                    cVarValue = IF AVAILABLE ar-invl THEN STRING(v-tot-wght[1],">>>,>>9.99") ELSE "" .
                         
            END CASE.
                      
            IF  cTmpField = "inv-date" THEN
                 cExcelVarValue = IF v-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",v-date) ELSE "".
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

      
    
        ASSIGN
            v-qty[2]      = v-qty[2] + v-qty[1]
            v-amt[2]      = v-amt[2] + v-amt[1]
            v-tot-wght[2] = v-tot-wght[2] + v-tot-wght[1] .
     
        IF LAST-OF(tt-report.key-06) THEN 
        DO WITH FRAME summary:
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

            ASSIGN
                v-qty[3]      = v-qty[3] + v-qty[2]
                v-amt[3]      = v-amt[3] + v-amt[2]
                v-tot-wght[3] = v-tot-wght[3] + v-tot-wght[2] 

                v-qty[2]      = 0
                v-amt[2]      = 0
                v-tot-wght[2] = 0   .
        END.
    
        IF LAST-OF(tt-report.key-03) THEN 
        DO:
            IF INDEX("CSM",substr(v-sort,3,1)) NE 0 THEN 
            DO WITH FRAME summary:
                /*underline cust.name v-qty[2] v-amt[2].*/
        
                v-total = ENTRY(INDEX(v-sort-list,substr(v-sort,3,1)),v-sort-desc).

                /* display fill(" ",23 - length(trim(v-total))) + trim(v-total) +
                         " TOTALS"   @ cust.name
                         v-qty[3]    @ v-qty[2]
                         v-amt[3]    @ v-amt[2].
         
                 down.*/
                PUT str-line  SKIP.
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
                        WHEN "name"   THEN 
                            cVarValue = "".
                        WHEN "rep"   THEN 
                            cVarValue = "".
                        WHEN "shipto"  THEN 
                            cVarValue = "" .
                        WHEN "ship-zip"   THEN 
                            cVarValue = "" .
                        WHEN "inv#"  THEN 
                            cVarValue = "" .
                        WHEN "inv-date"   THEN 
                            cVarValue = "" .
                        WHEN "ord"  THEN 
                            cVarValue = "" .
                        WHEN "item-no"    THEN 
                            cVarValue = "".
                        WHEN "item-name"   THEN 
                            cVarValue = "".
                        WHEN "desc"   THEN 
                            cVarValue = "".
                        WHEN "cust-part"  THEN 
                            cVarValue = "".
                        WHEN "price"   THEN 
                            cVarValue = "".
                        WHEN "uom"  THEN 
                            cVarValue = "".
                        WHEN "qty"   THEN 
                            cVarValue = STRING(v-qty[3],"->>,>>>,>>>") .
                        WHEN "inv-amt"  THEN 
                            cVarValue = STRING(v-amt[3],"->>,>>>,>>9.99") .
                        WHEN "inv-wt"  THEN 
                            cVarValue = STRING(v-tot-wght[3],">>>,>>9.99") .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
          
                PUT UNFORMATTED FILL(" ",23 - length(TRIM(v-total))) + trim(v-total) +  " Total"  SUBSTRING(cDisplay,30,350) SKIP.
                IF tb_excel THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  FILL(" ",23 - length(TRIM(v-total))) + trim(v-total) + ' Totals ,'
                        SUBSTRING(cExcelDisplay,4,350) SKIP.
                END.


                PUT SKIP(1).
            END.
      
            ASSIGN
                v-qty[4]      = v-qty[4] + v-qty[3]
                v-amt[4]      = v-amt[4] + v-amt[3]
                v-tot-wght[4] = v-tot-wght[4] + v-tot-wght[3] 

                v-qty[3]      = 0
                v-amt[3]      = 0
                v-tot-wght[3] = 0.
        END.

        IF LAST-OF(tt-report.key-02) THEN 
        DO:
            IF INDEX("CSM",substr(v-sort,2,1)) NE 0 THEN 
            DO WITH FRAME summary:
                /*underline cust.name v-qty[2] v-amt[2].*/
        
                v-total = ENTRY(INDEX(v-sort-list,substr(v-sort,2,1)),v-sort-desc).

                /*display fill(" ",23 - length(trim(v-total))) + trim(v-total) +
                        " TOTALS"   @ cust.name
                        v-qty[4]    @ v-qty[2]
                        v-amt[4]    @ v-amt[2].*/
                PUT str-line  SKIP.
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
                        WHEN "name"   THEN 
                            cVarValue = "".
                        WHEN "rep"   THEN 
                            cVarValue = "".
                        WHEN "shipto"  THEN 
                            cVarValue = "" .
                        WHEN "ship-zip"   THEN 
                            cVarValue = "" .
                        WHEN "inv#"  THEN 
                            cVarValue = "" .
                        WHEN "inv-date"   THEN 
                            cVarValue = "" .
                        WHEN "ord"  THEN 
                            cVarValue = "" .
                        WHEN "item-no"    THEN 
                            cVarValue = "".
                        WHEN "item-name"   THEN 
                            cVarValue = "".
                        WHEN "desc"   THEN 
                            cVarValue = "".
                        WHEN "cust-part"  THEN 
                            cVarValue = "".
                        WHEN "price"   THEN 
                            cVarValue = "".
                        WHEN "uom"  THEN 
                            cVarValue = "".
                        WHEN "qty"   THEN 
                            cVarValue = STRING(v-qty[4],"->>,>>>,>>>") .
                        WHEN "inv-amt"  THEN 
                            cVarValue = STRING(v-amt[4],"->>,>>>,>>9.99") .
                        WHEN "inv-wt"  THEN 
                            cVarValue = STRING(v-tot-wght[4],">>>,>>9.99") .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
          
                PUT UNFORMATTED FILL(" ",23 - length(TRIM(v-total))) + trim(v-total) + " Total"  SUBSTRING(cDisplay,30,350) SKIP.
                IF tb_excel THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  FILL(" ",23 - length(TRIM(v-total))) + trim(v-total) + ' Totals ,'
                        SUBSTRING(cExcelDisplay,4,350) SKIP.
                END.

                DOWN.
                PUT SKIP(1).
            END.

            ASSIGN
                v-qty[5]      = v-qty[5] + v-qty[4]
                v-amt[5]      = v-amt[5] + v-amt[4]
                v-tot-wght[5] = v-tot-wght[5] + v-tot-wght[4] 

                v-qty[4]      = 0
                v-amt[4]      = 0
                v-tot-wght[4] = 0.
        END.
    
        IF LAST-OF(tt-report.key-01) THEN 
        DO:
            IF INDEX("CSM",substr(v-sort,1,1)) NE 0 THEN 
            DO WITH FRAME summary:
                /*underline cust.name v-qty[2] v-amt[2].*/
        
                v-total = ENTRY(INDEX(v-sort-list,substr(v-sort,1,1)),v-sort-desc).

                /*display fill(" ",23 - length(trim(v-total))) + trim(v-total) +
                        " TOTALS"   @ cust.name
                        v-qty[5]    @ v-qty[2]
                        v-amt[5]    @ v-amt[2].
        
                down.*/
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
                        WHEN "cust"    THEN 
                            cVarValue = "" .
                        WHEN "name"   THEN 
                            cVarValue = "".
                        WHEN "rep"   THEN 
                            cVarValue = "".
                        WHEN "shipto"  THEN 
                            cVarValue = "" .
                        WHEN "ship-zip"   THEN 
                            cVarValue = "" .
                        WHEN "inv#"  THEN 
                            cVarValue = "" .
                        WHEN "inv-date"   THEN 
                            cVarValue = "" .
                        WHEN "ord"  THEN 
                            cVarValue = "" .
                        WHEN "item-no"    THEN 
                            cVarValue = "".
                        WHEN "item-name"   THEN 
                            cVarValue = "".
                        WHEN "desc"   THEN 
                            cVarValue = "".
                        WHEN "cust-part"  THEN 
                            cVarValue = "".
                        WHEN "price"   THEN 
                            cVarValue = "".
                        WHEN "uom"  THEN 
                            cVarValue = "".
                        WHEN "qty"   THEN 
                            cVarValue = STRING(v-qty[5],"->>,>>>,>>>") .
                        WHEN "inv-amt"  THEN 
                            cVarValue = STRING(v-amt[5],"->>,>>>,>>9.99") .
                        WHEN "inv-wt"  THEN 
                            cVarValue = STRING(v-tot-wght[5],">>>,>>9.99") .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
          
                PUT UNFORMATTED FILL(" ",23 - length(TRIM(v-total))) + trim(v-total) + " Total"  SUBSTRING(cDisplay,30,350) SKIP.
                IF tb_excel THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  FILL(" ",23 - length(TRIM(v-total))) + trim(v-total) + ' Totals ,'
                        SUBSTRING(cExcelDisplay,4,350) SKIP.
                END.
                PUT SKIP(1).
            END.

            ASSIGN
                v-qty[6]      = v-qty[6] + v-qty[5]
                v-amt[6]      = v-amt[6] + v-amt[5]
                v-tot-wght[6] = v-tot-wght[6] + v-tot-wght[5] 

                v-qty[5]      = 0
                v-amt[5]      = 0
                v-tot-wght[5] = 0.
        END.

        IF LAST(tt-report.key-01) THEN 
        DO WITH FRAME summary:
            PUT SKIP(1).
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
                    WHEN "name"   THEN 
                        cVarValue = "".
                    WHEN "rep"   THEN 
                        cVarValue = "".
                    WHEN "shipto"  THEN 
                        cVarValue = "" .
                    WHEN "ship-zip"   THEN 
                        cVarValue = "" .
                    WHEN "inv#"  THEN 
                        cVarValue = "" .
                    WHEN "inv-date"   THEN 
                        cVarValue = "" .
                    WHEN "ord"  THEN 
                        cVarValue = "" .
                    WHEN "item-no"    THEN 
                        cVarValue = "".
                    WHEN "item-name"   THEN 
                        cVarValue = "".
                    WHEN "desc"   THEN 
                        cVarValue = "".
                    WHEN "cust-part"  THEN 
                        cVarValue = "".
                    WHEN "price"   THEN 
                        cVarValue = "".
                    WHEN "uom"  THEN 
                        cVarValue = "".
                    WHEN "qty"   THEN 
                        cVarValue = STRING(v-qty[6],"->>,>>>,>>>") .
                    WHEN "inv-amt"  THEN 
                        cVarValue = STRING(v-amt[6],"->>,>>>,>>9.99") .
                    WHEN "inv-wt"  THEN 
                        cVarValue = STRING(v-tot-wght[6],">>>,>>9.99") .
                         
                END CASE.
                      
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            PUT str-line SKIP.
            PUT UNFORMATTED 
                "                Grands Total"  SUBSTRING(cDisplay,29,350) SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    '             Grands Totals ,'
                    SUBSTRING(cExcelDisplay,4,350) SKIP.
            END.

        END.

        DELETE w-data.
    END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ship-info C-Win 
PROCEDURE ship-info :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    IF AVAILABLE ar-inv THEN 
        IF ar-inv.ship-id NE "" THEN
            ASSIGN
                v-ship = ar-inv.ship-id
                v-shpz = ar-inv.sold-zip.
     
        ELSE
            IF ar-inv.sold-id NE "" THEN
                ASSIGN
                    v-ship = ar-inv.sold-id
                    v-shpz = ar-inv.sold-zip.

            ELSE
                ASSIGN
                    v-ship = ar-inv.cust-no
                    v-shpz = ar-inv.zip.

    ELSE
        IF AVAILABLE cust THEN
            ASSIGN
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
                        parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getInvSman C-Win 
FUNCTION getInvSman RETURNS CHARACTER
    ( INPUT picust-no AS CHARACTER,
    INPUT pi-inv-no AS INTEGER) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER buf-ar-inv  FOR ar-inv.
    DEFINE BUFFER buf-ar-invl FOR ar-invl.
    DEFINE VARIABLE vi AS INTEGER NO-UNDO INIT 0.

    FIND FIRST buf-ar-inv
        WHERE buf-ar-inv.company EQ cocode
        AND buf-ar-inv.cust-no EQ picust-no
        AND buf-ar-inv.inv-no  EQ pi-inv-no
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE buf-ar-inv THEN
        RETURN cust.sman .

    FOR EACH buf-ar-invl NO-LOCK WHERE 
        buf-ar-invl.x-no EQ buf-ar-inv.x-no
        AND (buf-ar-invl.billable OR NOT buf-ar-invl.misc):


        DO vi = 1 TO 3:
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
            fi_file:SCREEN-VALUE = "c:\tmp\SalesMultipleSorts.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

