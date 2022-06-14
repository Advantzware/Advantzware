&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: porep\r-schrpt.w

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

/* ***************************  DefINITions  ************************** */

/* Parameters DefINITions ---                                           */

/* Local Variable DefINITions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE INIT-dir  AS CHARACTER NO-UNDO.

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


DEFINE NEW SHARED VARIABLE v-s-vend  LIKE vend.vend-no INIT "".
DEFINE NEW SHARED VARIABLE v-e-vend  LIKE vend.vend-no INIT "zzzzzzzz".
DEFINE NEW SHARED VARIABLE v-s-date  LIKE po-ord.po-date INIT TODAY FORMAT "99/99/9999".
DEFINE NEW SHARED VARIABLE v-e-date  LIKE v-s-date INIT 12/31/9999.
DEFINE NEW SHARED VARIABLE v-po-stat LIKE po-ord.stat INIT "O".

DEFINE TEMP-TABLE tt-sched NO-UNDO
    FIELD job-no    LIKE po-ordl.job-no
    FIELD job-no2   LIKE po-ordl.job-no2
    FIELD i-no      LIKE po-ordl.i-no
    FIELD i-name    LIKE po-ordl.i-name
    FIELD vend-no   LIKE po-ord.vend-no
    FIELD po-no     LIKE po-ordl.po-no
    FIELD ord-no    LIKE oe-ordl.ord-no
    FIELD po-date   LIKE po-ord.po-date
    FIELD cons-uom  LIKE po-ordl.cons-uom
    FIELD cons-qty  LIKE po-ordl.cons-qty
    FIELD t-rec-qty LIKE po-ordl.t-rec-qty
    FIELD due-date  LIKE po-ordl.due-date
    FIELD amt-msf   LIKE ap-invl.amt-msf
    FIELD vend-name LIKE vend.name
    FIELD carrier   LIKE po-ord.carrier
    FIELD loc-bin   LIKE fg-rctd.loc-bin
    FIELD rct-date  LIKE fg-rctd.rct-date
    FIELD rec_key   LIKE po-ordl.rec_key
    INDEX job  job-no  job-no2
    INDEX i-no i-no
    INDEX vend vend-no.

DEFINE TEMP-TABLE tt-fgs NO-UNDO
    FIELD sched-rowid AS ROWID
    FIELD i-no        LIKE po-ordl.i-no
    FIELD i-name      LIKE po-ordl.i-name
    FIELD cust-no     LIKE po-ord.cust-no
    FIELD cust-name   LIKE po-ordl.i-name
    FIELD ord-no      LIKE po-ordl.ord-no
    FIELD ord-qty     LIKE po-ordl.ord-qty
    FIELD due-date    LIKE po-ordl.due-date
    FIELD job-no      LIKE job.job-no
    INDEX i1 sched-rowid.

DEFINE VARIABLE v-print-fmt         AS CHARACTER.
DEFINE VARIABLE is-xprint-form      AS LOGICAL.
DEFINE VARIABLE ls-fax-file         AS CHARACTER NO-UNDO.
DEFINE VARIABLE stat-list           AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-len               LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE v-wid               LIKE po-ordl.s-wid NO-UNDO.
DEFINE VARIABLE v-bwt               LIKE item.basis-w NO-UNDO.
DEFINE VARIABLE v-dep               LIKE po-ordl.s-wid NO-UNDO.
DEFINE VARIABLE v-qty               AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-cost              AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-s-num             LIKE po-ordl.s-num INIT 1 NO-UNDO.
DEFINE VARIABLE v-start-rcv-date    AS DATE      NO-UNDO.
DEFINE VARIABLE v-end-rcv-date      AS DATE      NO-UNDO.
DEFINE VARIABLE v-show-posted       AS LOG       NO-UNDO.
DEFINE VARIABLE v-rcv-date-selected AS LOG       NO-UNDO.
DEFINE VARIABLE v-tot               AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-ord-qty           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-sort              AS CHARACTER FORMAT "x" INIT "J" NO-UNDO.
DEFINE VARIABLE lv-job-no           AS CHARACTER NO-UNDO.

DEFINE VARIABLE lv-uom              AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-mattype-list      AS CHARACTER FORMAT "x(36)" NO-UNDO.
DEFINE VARIABLE v-mat-dscr          AS CHARACTER FORMAT "x(20)" EXTENT 21 NO-UNDO.
DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG     NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS cha     NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha     NO-UNDO.
DEFINE VARIABLE cFieldLength       AS cha     NO-UNDO.
DEFINE VARIABLE cFieldType         AS cha     NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS cha       NO-UNDO.
DEFINE VARIABLE cColumnInit        AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO .

ASSIGN 
    cTextListToSelect  = "VENDOR #,VENDOR NAME,ITEM NO,FG ITEM,BIN,ITEM NAME,ON ORDER FOR,P/O#,ORDER#,"
                         + "P/O DATE,QTY ORDER,QTY RECEIVED,REQ DATE,CARRIER,"
                         + "JOB NO,SIZE,UOM,CUSTOMER" 
    cFieldListToSelect = "vend,vend-name,i-no,fg-itm,bin,i-name,cust-nam,po,ord," +
                            "po-dt,qty-ord,qty-rcv,rfq-dt,carr," +
                            "job-no,size,uom,cust"
    cFieldLength       = "8,30,15,15,8,30,30,8,8," + "8,15,15,8,7," + "13,20,3,8"
    cFieldType         = "c,c,c,c,c,c,c,c,c," + "c,i,i,c,c," + "c,c,c,c"  
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "VENDOR #,VENDOR NAME,ITEM NO,FG ITEM,BIN,ITEM NAME,ON ORDER FOR,P/O#,ORDER#,"
                         + "P/O DATE,QTY ORDER,QTY RECEIVED,REQ DATE,CARRIER,"
                         + "JOB NO,SIZE,UOM,CUSTOMER"
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_vend-no end_vend-no ~
begin_due-date end_due-date begin_procat end_procat begin_cat end_cat ~
tg_receipts begin_receipt-date end_receipt-date rd_show select-mat rd_print ~
tb_late tb_printNotes btn_SelectColumns rd-dest td-show-parm fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_vend-no end_vend-no begin_due-date ~
end_due-date begin_procat end_procat begin_cat end_cat tg_receipts ~
begin_receipt-date end_receipt-date rd_show select-mat rd_print tb_late ~
tb_printNotes rd-dest td-show-parm fi_file tb_OpenCSV tbAutoClose lbl_show ~
lbl_print 

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

DEFINE BUTTON btn_SelectColumns 
    LABEL "Select Columns" 
    SIZE 40 BY 1.48.

DEFINE VARIABLE begin_cat          AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning FG Category" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_due-date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning PO Due Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat       AS CHARACTER FORMAT "X(6)":U 
    LABEL "Beginning RM Category" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_receipt-date AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Receipt Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend-no      AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_cat            AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending FG Category" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_due-date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending PO Due Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat         AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
    LABEL "Ending RM Category" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_receipt-date   AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Receipt Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend-no        AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file            AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-rschrp.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 43 BY 1
    FGCOLOR 0 .

DEFINE VARIABLE lbl_print          AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
    VIEW-AS TEXT 
    SIZE 6 BY .62 NO-UNDO.

DEFINE VARIABLE lbl_show           AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
    VIEW-AS TEXT 
    SIZE 5.6 BY .62 NO-UNDO.

DEFINE VARIABLE lines-per-page     AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name       AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no         AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE mat-types          AS CHARACTER FORMAT "X(256)":U 
    LABEL "Material Types" 
    VIEW-AS FILL-IN 
    SIZE 1 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt            AS CHARACTER INITIAL "L" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest            AS INTEGER   INITIAL 1 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 16 BY 3.81 NO-UNDO.

DEFINE VARIABLE rd_print           AS CHARACTER INITIAL "Job" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Job", "Job",
    "Item", "Item",
    "Vendor", "Vendor"
    SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE rd_show            AS CHARACTER INITIAL "Open" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Open", "Open",
    "Closed (Not Received)", "Closed (Not Received)",
    "All PO's", "All PO's"
    SIZE 46 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 95 BY 5.71.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 95 BY 13.33.

DEFINE VARIABLE select-mat    AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 28 BY 4.52 NO-UNDO.

DEFINE VARIABLE sl_avail      AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 27 BY .95 NO-UNDO.

DEFINE VARIABLE sl_selected   AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31.2 BY 1.05 NO-UNDO.

DEFINE VARIABLE tbAutoClose   AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel      AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_late       AS LOGICAL   INITIAL NO 
    LABEL "Print Late Line Items Only?" 
    VIEW-AS TOGGLE-BOX
    SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE tb_printNotes AS LOGICAL   INITIAL NO 
    LABEL "Print Notes" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV    AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY .81
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE td-show-parm  AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg_receipts   AS LOGICAL   INITIAL NO 
    LABEL "Show posted receipts?" 
    VIEW-AS TOGGLE-BOX
    SIZE 29 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_vend-no AT ROW 2.67 COL 29.2 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_vend-no AT ROW 2.67 COL 70.2 COLON-ALIGNED HELP
    "Enter Ending Vendor number"
    begin_due-date AT ROW 3.81 COL 29.2 COLON-ALIGNED HELP
    "Enter Beginning Due Date"
    end_due-date AT ROW 3.81 COL 70.2 COLON-ALIGNED HELP
    "Enter ending Due Date"
    begin_procat AT ROW 4.95 COL 29.2 COLON-ALIGNED
    end_procat AT ROW 4.95 COL 70.2 COLON-ALIGNED HELP
    "Enter Ending Category"
    begin_cat AT ROW 6.1 COL 29.2 COLON-ALIGNED HELP
    "Enter Beginning Category"
    end_cat AT ROW 6.1 COL 70.2 COLON-ALIGNED HELP
    "Enter Ending Order Number"
    tg_receipts AT ROW 7.14 COL 31.2 WIDGET-ID 2
    begin_receipt-date AT ROW 8.1 COL 29.2 COLON-ALIGNED HELP
    "Enter Beginning Due Date" WIDGET-ID 4
    end_receipt-date AT ROW 8.1 COL 70.2 COLON-ALIGNED HELP
    "Enter ending Due Date" WIDGET-ID 6
    rd_show AT ROW 9.95 COL 12 NO-LABELS
    select-mat AT ROW 10.1 COL 68 NO-LABELS
    rd_print AT ROW 11 COL 12.2 NO-LABELS
    mat-types AT ROW 11.71 COL 66.4 COLON-ALIGNED
    tb_late AT ROW 11.95 COL 12.8
    tb_printNotes AT ROW 12.91 COL 12.8
    btn_SelectColumns AT ROW 15.29 COL 32 WIDGET-ID 10
    sl_selected AT ROW 17.86 COL 34.4 NO-LABELS WIDGET-ID 28
    sl_avail AT ROW 17.86 COL 39.4 NO-LABELS WIDGET-ID 26
    rd-dest AT ROW 18 COL 7.6 NO-LABELS
    lv-font-name AT ROW 18.1 COL 31.4 COLON-ALIGNED NO-LABELS
    lv-ornt AT ROW 18.1 COL 34.4 NO-LABELS
    lv-font-no AT ROW 18.1 COL 42.4 COLON-ALIGNED
    tb_excel AT ROW 18.1 COL 69.4 RIGHT-ALIGNED
    lines-per-page AT ROW 18.1 COL 56.4 COLON-ALIGNED
    td-show-parm AT ROW 19.91 COL 38.6
    fi_file AT ROW 20.81 COL 28.6 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 20.91 COL 87.6 RIGHT-ALIGNED
    tbAutoClose AT ROW 23.43 COL 33.2 WIDGET-ID 16
    btn-ok AT ROW 24.33 COL 32.6
    btn-cancel AT ROW 24.33 COL 52.4
    lbl_show AT ROW 10.14 COL 5.2 NO-LABELS
    lbl_print AT ROW 11.14 COL 5.4 NO-LABELS
    "Select/Deselect RM Types" VIEW-AS TEXT
    SIZE 24.8 BY .76 AT ROW 9.33 COL 69.2
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.38 COL 4.8
    BGCOLOR 15 
    "Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 16.76 COL 5.6
    RECT-6 AT ROW 17.14 COL 4.4
    RECT-7 AT ROW 1.71 COL 3.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 104.6 BY 26.81
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
        TITLE              = "Scheduled Receipts with Orders"
        HEIGHT             = 25.05
        WIDTH              = 100.8
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
    begin_cat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_due-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_procat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_receipt-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_vend-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_due-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_procat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_receipt-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_vend-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_print IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
    lbl_print:PRIVATE-DATA IN FRAME FRAME-A = "rd_print".

/* SETTINGS FOR FILL-IN lbl_show IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
    lbl_show:PRIVATE-DATA IN FRAME FRAME-A = "rd_show".

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

/* SETTINGS FOR FILL-IN mat-types IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    mat-types:HIDDEN IN FRAME FRAME-A       = TRUE
    mat-types:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_print:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_show:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    select-mat:AUTO-RESIZE IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_avail IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    sl_avail:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_selected IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    sl_selected:HIDDEN IN FRAME FRAME-A = TRUE.

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
ON END-ERROR OF C-Win /* Scheduled Receipts with Orders */
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
ON WINDOW-CLOSE OF C-Win /* Scheduled Receipts with Orders */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cat C-Win
ON LEAVE OF begin_cat IN FRAME FRAME-A /* Beginning FG Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_due-date C-Win
ON LEAVE OF begin_due-date IN FRAME FRAME-A /* Beginning PO Due Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_procat C-Win
ON LEAVE OF begin_procat IN FRAME FRAME-A /* Beginning RM Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_receipt-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_receipt-date C-Win
ON LEAVE OF begin_receipt-date IN FRAME FRAME-A /* Beginning Receipt Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no C-Win
ON LEAVE OF begin_vend-no IN FRAME FRAME-A /* Beginning Vendor# */
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
        DEFINE VARIABLE v-valid AS LOG NO-UNDO.

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
        RUN run-report(OUTPUT v-valid). 
        STATUS DEFAULT "Processing Complete".

        IF v-valid THEN
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
                    END. /* WHEN 3 THEN DO: */
                WHEN 4 THEN 
                    DO:
                        /*run output-to-fax.*/
                        {custom/asifax.i &begin_cust=begin_vend-no
                            &END_cust=END_vend-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                    END.
                WHEN 5 THEN 
                    DO:
                        IF is-xprint-form THEN 
                        DO:
                            RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                            {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust= begin_vend-no
                             &END_cust=end_vend-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                        END.
                        ELSE 
                        DO:
                            {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust= begin_vend-no
                                  &END_cust=end_vend-no
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


&Scoped-define SELF-NAME btn_SelectColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_SelectColumns C-Win
ON CHOOSE OF btn_SelectColumns IN FRAME FRAME-A /* Select Columns */
    DO:
        DEFINE VARIABLE cTextSelected AS cha NO-UNDO.
        DEFINE VARIABLE cTextListed   AS cha NO-UNDO.

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


&Scoped-define SELF-NAME end_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cat C-Win
ON LEAVE OF end_cat IN FRAME FRAME-A /* Ending FG Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_due-date C-Win
ON LEAVE OF end_due-date IN FRAME FRAME-A /* Ending PO Due Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_procat C-Win
ON LEAVE OF end_procat IN FRAME FRAME-A /* Ending RM Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_receipt-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_receipt-date C-Win
ON LEAVE OF end_receipt-date IN FRAME FRAME-A /* Ending Receipt Date */
    DO:
  //assign {&self-name}.
        fi_file = ''.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-no C-Win
ON LEAVE OF end_vend-no IN FRAME FRAME-A /* Ending Vendor# */
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME rd_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_print C-Win
ON VALUE-CHANGED OF rd_print IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_show C-Win
ON VALUE-CHANGED OF rd_show IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_late
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_late C-Win
ON VALUE-CHANGED OF tb_late IN FRAME FRAME-A /* Print Late Line Items Only? */
    DO:
        ASSIGN {&SELF-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_printNotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_printNotes C-Win
ON VALUE-CHANGED OF tb_printNotes IN FRAME FRAME-A /* Print Notes */
    DO:
        ASSIGN {&SELF-NAME}.
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
DEFINE VARIABLE v-mat-list AS CHARACTER NO-UNDO.         
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
        begin_due-date = TODAY
        end_due-date   = DATE(12,31,9999).

    RUN DisplaySelectionList.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    btn_SelectColumns:load-image("Graphics/32x32/selectColumns.png").
    RUN enable_UI.

    FOR EACH mat:
        v-mat-list = v-mat-list + string(mat.mat,"x(5)") + " " + mat.dscr + ",".
    END.
    IF substr(v-mat-list,LENGTH(TRIM(v-mat-list)),1) EQ "," THEN
        substr(v-mat-list,LENGTH(TRIM(v-mat-list)),1) = "".

    select-mat:list-items = v-mat-list.

    DO i = 1 TO select-mat:num-items:
        IF TRIM(substr(select-mat:entry(i),1,5)) EQ "B" THEN 
        DO:
            select-mat:screen-value = ENTRY(i,v-mat-list).
            LEAVE.
        END.
    END.

    {methods/nowait.i}
    {sys/inc/reportsConfigNK1.i "PR14" }
    ASSIGN
        td-show-parm:sensitive = lShowParameters
        td-show-parm:hidden    = NOT lShowParameters
        td-show-parm:visible   = lShowParameters
        .
   
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_vend-no.
    END.

    cColumnInit   = NO .
    RUN pChangeDest.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-fg-orders C-Win 
PROCEDURE add-fg-orders :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF po-ordl.ITEM-type THEN 
    DO:
        FOR EACH job-hdr WHERE job-hdr.company = po-ordl.company
            AND job-hdr.job-no = po-ordl.job-no
            AND job-hdr.job-no2 = po-ordl.job-no2
            NO-LOCK,
            EACH job WHERE job.company = job-hdr.company
            AND job.job-no  = job-hdr.job-no
            AND job.job-no2 = job-hdr.job-no2
            NO-LOCK,
            EACH job-mat WHERE job-mat.company = job.company
            AND job-mat.rm-i-no = po-ordl.i-no
            AND job-mat.job-no  = job.job-no
            AND job-mat.job-no2 = job.job-no2
            NO-LOCK,
            EACH oe-ordl 
            WHERE oe-ordl.company = job-mat.company
            AND oe-ordl.i-no      EQ job-hdr.i-no
            AND oe-ordl.stat NE "C" 
            NO-LOCK,
            FIRST oe-ord WHERE oe-ord.company = oe-ordl.company
            AND oe-ord.ord-no  = oe-ordl.ord-no
            NO-LOCK:

            FIND cust WHERE cust.company = po-ordl.company
                AND cust.cust-no = job-hdr.cust-no
                NO-LOCK NO-ERROR.
            FIND itemfg WHERE itemfg.company = po-ordl.company
                AND itemfg.i-no = job-hdr.i-no
                NO-LOCK NO-ERROR.

            FIND FIRST tt-fgs WHERE tt-fgs.sched-rowid = ROWID(tt-sched)
                AND tt-fgs.i-no        = job-hdr.i-no
                AND tt-fgs.ord-no      = oe-ordl.ord-no
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tt-fgs THEN 
            DO:
                CREATE tt-fgs.
                ASSIGN 
                    tt-fgs.sched-rowid = ROWID(tt-sched)
                    tt-fgs.i-no        = job-hdr.i-no
                    tt-fgs.cust-no     = job-hdr.cust-no
                    tt-fgs.ord-no      = oe-ordl.ord-no
                    tt-fgs.ord-qty     = oe-ordl.qty
                    tt-fgs.due-date    = oe-ord.due-date
                    tt-fgs.job-no      = STRING(po-ordl.s-wid) + " x " + STRING(po-ordl.s-len).
                IF AVAIL(itemfg) THEN
                    tt-fgs.i-name      = itemfg.i-name.
                IF AVAILABLE cust THEN
                    tt-fgs.cust-name   = cust.NAME.
            END.

        END.
    END.
    ELSE 
    DO:
        FOR EACH oe-ordl 
            WHERE oe-ordl.company = po-ordl.company
            AND oe-ordl.i-no      EQ po-ordl.i-no
            AND oe-ordl.stat NE "C" 
            NO-LOCK,
            FIRST oe-ord WHERE oe-ord.company = oe-ordl.company
            AND oe-ord.ord-no  = oe-ordl.ord-no
            NO-LOCK:

            FIND cust WHERE cust.company = po-ordl.company
                AND cust.cust-no = oe-ord.cust-no
                NO-LOCK NO-ERROR.
            FIND itemfg WHERE itemfg.company = po-ordl.company
                AND itemfg.i-no = po-ordl.i-no
                NO-LOCK NO-ERROR.

            FIND FIRST tt-fgs WHERE tt-fgs.sched-rowid = ROWID(tt-sched)
                AND tt-fgs.i-no        = po-ordl.i-no
                AND tt-fgs.ord-no      = oe-ordl.ord-no
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tt-fgs THEN 
            DO:
                CREATE tt-fgs.
                ASSIGN 
                    tt-fgs.sched-rowid = ROWID(tt-sched)
                    tt-fgs.i-no        = po-ordl.i-no
                    tt-fgs.cust-no     = oe-ord.cust-no
                    tt-fgs.ord-no      = oe-ordl.ord-no
                    tt-fgs.ord-qty     = oe-ordl.qty
                    tt-fgs.due-date    = oe-ord.due-date
                    tt-fgs.job-no      = STRING(po-ordl.s-wid) + " x " + STRING(po-ordl.s-len).
                IF AVAIL(itemfg) THEN
                    tt-fgs.i-name      = itemfg.i-name.
                IF AVAILABLE cust THEN
                    tt-fgs.cust-name   = cust.NAME.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tt C-Win 
PROCEDURE create-tt :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    FOR EACH po-ord
        WHERE po-ord.company EQ cocode
        AND po-ord.vend-no GE v-s-vend
        AND po-ord.vend-no LE v-e-vend
        /*and (lookup(po-ord.stat,stat-list) gt 0 or v-po-stat eq "A")*/
        AND ((po-ord.opened AND v-po-stat EQ "O") OR
        (NOT po-ord.opened AND v-po-stat EQ "C") OR
        v-po-stat EQ "A")
        NO-LOCK,

        EACH po-ordl
        WHERE po-ordl.company  EQ po-ord.company
        AND po-ordl.po-no    EQ po-ord.po-no
        AND (LOOKUP(po-ord.stat,stat-list) GT 0 OR v-po-stat EQ "A")
        AND po-ordl.due-date GE v-s-date
        AND po-ordl.due-date LE v-e-date
        AND ((po-ordl.item-type AND
        CAN-FIND(FIRST item
        WHERE item.company EQ po-ordl.company
        AND item.i-no    EQ po-ordl.i-no
        AND CAN-DO(v-mattype-list,item.mat-type)
        AND item.procat  GE begin_procat
        AND item.procat  LE end_procat
        AND item.inv-by-cust EQ NO)) OR
        (NOT po-ordl.item-type AND
        CAN-FIND(FIRST itemfg
        WHERE itemfg.company EQ po-ordl.company
        AND itemfg.i-no    EQ po-ordl.i-no
        AND itemfg.procat  GE begin_cat
        AND itemfg.procat  LE end_cat)))
        AND (NOT tb_late OR po-ordl.due-date LT TODAY)
        NO-LOCK:

        {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}

        ASSIGN
            v-bwt = 0
            v-len = po-ordl.s-len
            v-wid = po-ordl.s-wid
            v-dep = 0.

        IF po-ordl.item-type THEN 
        DO:
            lv-uom = po-ordl.pr-qty-uom.

            FIND FIRST item NO-LOCK
                WHERE item.company EQ po-ordl.company
                AND item.i-no    EQ po-ordl.i-no
                NO-ERROR.

            IF AVAILABLE item THEN 
            DO:
                v-dep = item.s-dep.
                {po/pol-dims.i}
            END.
        END.

        ELSE 
        DO:
            lv-uom = "EA".

            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ po-ordl.company
                AND itemfg.i-no    EQ po-ordl.i-no
                NO-ERROR.

            IF AVAILABLE itemfg THEN 
            DO:
                IF v-len EQ 0 THEN v-len = itemfg.t-len.
                IF v-wid EQ 0 THEN v-wid = itemfg.t-wid.
                v-dep = itemfg.t-dep.
            END.
        END.

        ASSIGN
            v-qty  = po-ordl.t-rec-qty
            v-cost = po-ordl.cons-cost. 

        IF po-ordl.cons-uom NE lv-uom THEN 
        DO:

            RUN sys/ref/convquom.p(po-ordl.cons-uom, lv-uom,
                v-bwt, v-len, v-wid, v-dep,
                v-qty, OUTPUT v-qty).

            RUN sys/ref/convcuom.p(po-ordl.cons-uom, lv-uom,
                v-bwt, v-len, v-wid, v-dep,
                v-cost, OUTPUT v-cost).
        END.


        IF lv-uom EQ "EA" THEN 
        DO:
            {sys/inc/roundup.i v-qty}
        END.

        v-ord-qty = po-ordl.ord-qty.
        IF NOT po-ordl.item-type AND po-ordl.cons-uom NE lv-uom THEN 
            RUN sys/ref/convquom.p(po-ordl.cons-uom, lv-uom,
                v-bwt, v-len, v-wid, v-dep,
                po-ordl.ord-qty, OUTPUT v-ord-qty).

        /*IF po-ordl.ord-qty - v-qty GT 0 THEN DO:*/
        IF (v-ord-qty - v-qty GT 0 OR v-show-posted) THEN 
        DO:

            IF v-cost EQ ? THEN v-cost = 0.

            CREATE tt-sched.
            ASSIGN
                tt-sched.job-no    = po-ordl.job-no
                tt-sched.job-no2   = po-ordl.job-no2
                tt-sched.i-no      = po-ordl.i-no
                tt-sched.i-name    = po-ordl.i-name
                tt-sched.vend-no   = po-ord.vend-no
                tt-sched.po-no     = po-ordl.po-no
                tt-sched.po-date   = po-ord.po-date
                tt-sched.cons-uom  = lv-uom
                tt-sched.cons-qty  = po-ordl.ord-qty
                tt-sched.t-rec-qty = (IF NOT po-ordl.item-type AND po-ordl.cons-uom NE lv-uom THEN po-ordl.t-rec-qty ELSE v-qty)
                tt-sched.due-date  = po-ordl.due-date
                tt-sched.carrier   = po-ord.carrier
                tt-sched.rec_key   = po-ordl.rec_key.
            IF v-ord-qty - v-qty GT 0 THEN
                v-tot = v-tot + ((po-ordl.ord-qty - v-qty) * v-cost).
            ELSE
                v-tot = v-tot + (v-qty * v-cost).

            /* Create tt record for fg's related to a rm */                       
            RUN add-fg-orders.

            FIND FIRST vend NO-LOCK
                WHERE vend.company EQ po-ord.company
                AND vend.vend-no EQ po-ord.vend-no
                NO-ERROR.

            ASSIGN
                tt-sched.vend-name = IF AVAILABLE vend THEN vend.name ELSE "" .

            IF v-sort EQ "V" THEN 
            DO:
                ASSIGN
                    v-s-num          = po-ordl.s-num
                    tt-sched.amt-msf = tt-sched.cons-qty - tt-sched.t-rec-qty.

                IF tt-sched.cons-uom NE "MSF" THEN
                    RUN sys/ref/convquom.p(tt-sched.cons-uom, "MSF",
                        v-bwt, v-len, v-wid, v-dep,
                        tt-sched.amt-msf, OUTPUT tt-sched.amt-msf).
            END.
            FIND itemfg WHERE itemfg.company = po-ordl.company
                AND itemfg.i-no = po-ordl.i-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE itemfg THEN
                tt-sched.loc-bin = itemfg.def-loc-bin.
            FIND ITEM WHERE ITEM.company = po-ordl.company
                AND ITEM.i-no = po-ordl.i-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE ITEM THEN
                tt-sched.loc-bin = ITEM.loc-bin.

            FOR EACH fg-rctd WHERE fg-rctd.company = po-ord.company
                AND fg-rctd.i-no = tt-sched.i-no
                AND integer(fg-rctd.po-no) = po-ord.po-no
                NO-LOCK.
                tt-sched.rct-date = fg-rctd.rct-date.
                IF fg-rctd.loc-bin > "" THEN
                    tt-sched.loc-bin = fg-rctd.loc-bin.
            END.
            FOR EACH rm-rctd WHERE rm-rctd.company = po-ord.company
                AND rm-rctd.i-no = tt-sched.i-no
                AND integer(rm-rctd.po-no) = po-ord.po-no
                NO-LOCK.
                tt-sched.rct-date = rm-rctd.rct-date.
                IF rm-rctd.loc-bin > "" THEN
                    tt-sched.loc-bin = rm-rctd.loc-bin.
            END.
        END.
    END.

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
    DISPLAY begin_vend-no end_vend-no begin_due-date end_due-date begin_procat 
        end_procat begin_cat end_cat tg_receipts begin_receipt-date 
        end_receipt-date rd_show select-mat rd_print tb_late tb_printNotes 
        rd-dest td-show-parm fi_file tb_OpenCSV tbAutoClose lbl_show 
        lbl_print 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_vend-no end_vend-no begin_due-date end_due-date 
        begin_procat end_procat begin_cat end_cat tg_receipts 
        begin_receipt-date end_receipt-date rd_show select-mat rd_print 
        tb_late tb_printNotes btn_SelectColumns rd-dest td-show-parm fi_file 
        tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file C-Win 
PROCEDURE output-to-file :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    {custom/out2file.i}.

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
    RUN cusotm/d-print.w (list-name).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printNotes C-Win 
PROCEDURE printNotes :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipRecKey AS CHARACTER NO-UNDO.

    IF CAN-FIND(FIRST notes WHERE notes.rec_key EQ ipRecKey) THEN
        FOR EACH notes NO-LOCK WHERE notes.rec_key EQ ipRecKey:
            PUT UNFORMATTED 
                'Note: ' AT 1 notes.note_title notes.note_text AT 1.
        END. /* each notes */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ---------------------------------------------- po/rep/sch-rcts.p 8/96 fwk  */
    /* Scheduled Receipts Report                                                  */
    /* -------------------------------------------------------------------------- */
    DEFINE OUTPUT PARAMETER op-valid AS LOG INIT TRUE NO-UNDO.

    /*{sys/form/r-topw.f}*/


    DEFINE VARIABLE code-text      AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDisplay       AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS cha       NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHA       NO-UNDO.
    DEFINE VARIABLE cVarValue      AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS cha       NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS cha       NO-UNDO.
    DEFINE VARIABLE cFieldName     AS cha       NO-UNDO.
    DEFINE VARIABLE str-tit4       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line       AS cha       FORM "x(300)" NO-UNDO.

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
//DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

//RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

    stat-list = "".
    v-len = 0.
    v-wid = 0.
    v-bwt = 0.
    v-dep = 0.
    v-qty = 0.
    v-cost = 0.
    v-s-num =  1.
    v-start-rcv-date .
    v-end-rcv-date.
    v-show-posted.
    v-rcv-date-selected.
    v-tot = 0.
    v-ord-qty = 0.
    v-sort = "J" .

    FORM HEADER
        "JOB NO/SIZE"
        "ITEM NO/FG ITEM" 
        "BIN     "
        "ITEM NAME/ON ORDER FOR"
        "VEND#/CUST"
        "P/O#   "
        " ORDER#"
        "P/O DATE"
        "UOM"
        "       QTY ORDER"
        "   QTY RECEIVED"
        "REQ DATE"
        "CARRIER"
        FILL("-",167) FORMAT "x(167)"
        WITH STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP
        FRAME sch-head-job.

    FORM HEADER
        "ITEM NO/FG ITEM" AT 1
        "BIN     "
        "ITEM NAME/ON ORDER FOR "
        "VEND#/CUST"    
        "P/O#   "
        " ORDER#"
        "P/O DATE"
        "UOM"
        "       QTY ORDER"
        "   QTY RECEIVED"
        "REQ DATE"
        "CARRIER"
        "JOB NO/SIZE  "    
        FILL("-",167) FORMAT "x(167)"
        WITH STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP
        FRAME sch-head-item.


    FORM HEADER
        "VEND NO   "
        "VENDOR NAME              "
        "ITEM NO/FG ITEM"
        "BIN     "
        "ITEM NAME/ON ORDER FOR  "
        "JOB NO/SIZE  "
        "P/O#   "
        " ORDER#"
        "P/O DATE"
        "       QTY ORDER"
        "    QTY RECEIVED"
        "REQ DATE "
        /* "   MSF" */
        "CARRIER"
        FILL("-",178) FORMAT "x(178)"
        WITH STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP
        FRAME sch-head-vend.

    FORM lv-job-no FORMAT "x(13)"
        tt-sched.i-no 
        tt-sched.loc-bin
        tt-sched.i-name FORMAT "x(22)"
        tt-sched.vend-no FORMAT "x(10)"
        tt-sched.po-no SPACE(3)
        tt-sched.ord-no SPACE(1)
        tt-sched.po-date FORMAT "99/99/99"
        tt-sched.cons-uom 
        tt-sched.cons-qty FORMAT "->>>,>>>,>>9.99"
        tt-sched.t-rec-qty FORMAT "->>>,>>>,>>9.99"
        tt-sched.due-date FORMAT "99/99/99"
        tt-sched.carrier 
        WITH DOWN STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE FRAME sch-rcts-job.

    FORM tt-sched.i-no
        tt-sched.loc-bin
        tt-sched.i-name FORMAT "x(23)"
        tt-sched.vend-no FORMAT "x(10)"
        tt-sched.po-no SPACE(3)
        tt-sched.ord-no
        tt-sched.po-date FORMAT "99/99/99"
        tt-sched.cons-uom
        tt-sched.cons-qty FORMAT "->>>,>>>,>>9.99"
        tt-sched.t-rec-qty FORMAT "->>>,>>>,>>9.99"
        tt-sched.due-date FORMAT "99/99/99" 
        tt-sched.carrier SPACE(2)
        lv-job-no FORMAT "x(13)"
        WITH DOWN STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE FRAME sch-rcts-item.

    FORM tt-sched.vend-no FORMAT "x(10)"
        tt-sched.vend-name FORMAT "x(25)"
        tt-sched.i-no
        tt-sched.loc-bin
        tt-sched.i-name FORMAT "x(24)"
        lv-job-no FORMAT "x(13)"
        tt-sched.po-no SPACE(3)
        tt-sched.ord-no
        tt-sched.po-date
        tt-sched.cons-qty FORMAT "->>>,>>>,>>9.99"
        tt-sched.t-rec-qty  FORMAT "->>>,>>>,>>9.99"
        tt-sched.due-date FORMAT "99/99/99" 
        /*tt-sched.amt-msf FORMAT "->,>>9.999" */ SPACE(2)
        tt-sched.carrier SKIP
        WITH DOWN STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE FRAME sch-rcts-vend.

    {ce/msfcalc.i}

    ASSIGN
        str-tit2  = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-s-vend  = begin_vend-no
        v-e-vend  = end_vend-no
        v-s-date  = begin_due-date
        v-e-date  = end_due-date
        v-po-stat = SUBSTR(rd_show,1,1)
        v-sort    = SUBSTR(rd_print,1,1).
    v-show-posted = tg_receipts.
    v-start-rcv-date = begin_receipt-date.
    v-end-rcv-date   = end_receipt-date.

    IF v-start-rcv-date = 01/01/0001 AND v-end-rcv-date = 12/31/9999 THEN
        v-rcv-date-selected = NO.
    ELSE
        v-rcv-date-selected = YES.

    DO WITH FRAME {&frame-name}:          
        DO i = 1 TO select-mat:num-items:
            IF select-mat:is-selected(i) THEN
                v-mattype-list = v-mattype-list + trim(substr(select-mat:entry(i),1,5)) + ",".
        END.

        IF LENGTH(TRIM(v-mattype-list)) EQ 0 THEN
        DO:
            MESSAGE "No Material Type Selected."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            op-valid = NO.
            LEAVE.
        END.

        IF substr(v-mattype-list,LENGTH(TRIM(v-mattype-list)),1) EQ "," THEN
            substr(v-mattype-list,LENGTH(TRIM(v-mattype-list)),1) = "".

        mat-types = v-mattype-list.

        DO i = 1 TO LENGTH(mat-types):
            IF substr(mat-types,i,1) EQ "," THEN substr(mat-types,i,1) = " ".
        END.

        DISPLAY mat-types.
    END.


    DEFINE VARIABLE cslist AS cha NO-UNDO.
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

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        /* IF v-sort EQ "J" THEN
            excelheader = "JOB NO/SIZE,ITEM NO/FG ITEM,BIN,ITEM NAME/ON ORDER FOR,VEND NO,P/O#,ORDER#,"
                        + "P/O DATE,UOM,QTY ORDER,QTY RECEIVED,REQ DATE,CARRIER".
         ELSE IF v-sort EQ "I" THEN
            excelheader = "ITEM NO/FG ITEM,BIN,ITEM NAME/ON ORDER FOR,VEND NO,P/O#,ORDER#,"
                        + "P/O DATE,UOM,QTY ORDER,QTY RECEIVED,REQ DATE,CARRIER,"
                        + "JOB NO/SIZE".
         ELSE
            excelheader = "VENDOR NO,VENDOR NAME,ITEM NO/FG ITEM,BIN,ITEM NAME/ON ORDER FOR,P/O#,ORDER#,"
                        + "P/O DATE,QTY ORDER,QTY RECEIVED,REQ DATE,CARRIER,"
                        + "JOB NO/SIZE".*/

        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    DISPLAY "" WITH FRAME r-top.

    /* IF v-sort EQ "J" THEN
       DISPLAY WITH FRAME sch-head-job.
     ELSE
     IF v-sort eq "I" THEN
       DISPLAY WITH FRAME sch-head-item.
     else
       DISPLAY WITH FRAME sch-head-vend.*/

    EMPTY TEMP-TABLE tt-sched.
    EMPTY TEMP-TABLE tt-fgs.
  
    stat-list = IF v-po-stat EQ "A" THEN ""             ELSE
        IF v-po-stat EQ "O" THEN "O,U,P,A,N,H"  ELSE "C,X,F".
    RUN create-tt.


    IF v-sort EQ "J" THEN
        FOR EACH tt-sched WHERE (IF v-rcv-date-selected = NO OR tt-sched.rct-date = ? THEN TRUE 
        ELSE 
            (tt-sched.rct-date GE v-start-rcv-date
            AND tt-sched.rct-date LE v-end-rcv-date))
            USE-INDEX job BREAK BY tt-sched.job-no BY tt-sched.job-no2:

            {custom/statusMsg.i " 'Processing PO#  '  + string(tt-sched.po-no) "}

            lv-job-no = IF tt-sched.job-no EQ "" THEN ""
            ELSE TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', tt-sched.job-no, tt-sched.job-no2))).

            /* DISPLAY lv-job-no
                     tt-sched.i-no
                     tt-sched.loc-bin
                     tt-sched.i-name
                     tt-sched.vend-no
                     tt-sched.po-no
                     tt-sched.po-date
                     tt-sched.cons-uom
                     tt-sched.cons-qty
                     tt-sched.t-rec-qty
                     tt-sched.due-date
                     tt-sched.carrier
                 WITH FRAME sch-rcts-job.
        
             IF tb_excel THEN
                PUT STREAM excel UNFORMATTED
                  '"' lv-job-no                                    '",'
                  '"' tt-sched.i-no                                '",'
                  '"' tt-sched.loc-bin                             '",'
                  '"' tt-sched.i-name                              '",'
                  '"' tt-sched.vend-no                             '",'
                  '"' STRING(tt-sched.po-no)                       '",'
                  '"' ""                                           '",'
                  '"' (IF tt-sched.po-date NE ? THEN
                          STRING(tt-sched.po-date) ELSE "")        '",'
                  '"' tt-sched.cons-uom                            '",'
                  '"' STRING(tt-sched.cons-qty,"->>>,>>>,>>9.99")  '",'
                  '"' STRING(tt-sched.t-rec-qty,"->>>,>>>,>>9.99") '",'
                  '"' (IF tt-sched.due-date NE ? THEN
                          STRING(tt-sched.due-date) ELSE "")       '",'
                  '"' tt-sched.carrier                             '",'
                  SKIP. */

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "vend"        THEN 
                        cVarValue = STRING(tt-sched.vend-no,"x(8)")  .
                    WHEN "vend-name"   THEN 
                        cVarValue = STRING(tt-sched.vend-name,"x(30)")  .
                    WHEN "i-no"        THEN 
                        cVarValue = STRING(tt-sched.i-no,"x(15)") .
                    WHEN "fg-itm"      THEN 
                        cVarValue = "" .
                    WHEN "bin"         THEN 
                        cVarValue = STRING(tt-sched.loc-bin)  .
                    WHEN "i-name"      THEN 
                        cVarValue = STRING(tt-sched.i-name).
                    WHEN "cust-nam"    THEN 
                        cVarValue = "".
                    WHEN "po"          THEN 
                        cVarValue = STRING(tt-sched.po-no) .
                    WHEN "ord"         THEN 
                        cVarValue = "" .
                    WHEN "po-dt"       THEN 
                        cVarValue = IF tt-sched.po-date NE ? THEN STRING(tt-sched.po-date) ELSE "" .
                    WHEN "qty-ord"     THEN 
                        cVarValue = STRING(tt-sched.cons-qty,"->>>,>>>,>>9.99") .
                    WHEN "qty-rcv"     THEN 
                        cVarValue = STRING(tt-sched.t-rec-qty,"->>>,>>>,>>9.99") .
                    WHEN "rfq-dt"      THEN 
                        cVarValue = IF tt-sched.due-date NE ? THEN STRING(tt-sched.due-date) ELSE ""  .
                    WHEN "carr"        THEN 
                        cVarValue = STRING(tt-sched.carrier).
                    WHEN "job-no"      THEN 
                        cVarValue = STRING(lv-job-no).
                    WHEN "size"        THEN 
                        cVarValue = "" .
                    WHEN "uom"         THEN 
                        cVarValue = STRING(tt-sched.cons-uom) .

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


            IF tb_printNotes THEN RUN printNotes (tt-sched.rec_key).


            FOR EACH tt-fgs WHERE tt-fgs.sched-rowid = ROWID(tt-sched)
                /* WITH FRAME {1}*/
                BREAK BY tt-fgs.i-no
                BY tt-fgs.due-date:


                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "vend"        THEN 
                            cVarValue = "" .
                        WHEN "vend-name"   THEN 
                            cVarValue = ""  .
                        WHEN "i-no"        THEN 
                            cVarValue = "" .
                        WHEN "fg-itm"      THEN 
                            cVarValue = STRING(tt-fgs.i-no,"x(15)") .
                        WHEN "bin"         THEN 
                            cVarValue = ""  .
                        WHEN "i-name"      THEN 
                            cVarValue = "" .
                        WHEN "cust-nam"    THEN 
                            cVarValue = STRING(tt-fgs.cust-name).
                        WHEN "po"          THEN 
                            cVarValue = "" .
                        WHEN "ord"         THEN 
                            cVarValue = STRING(tt-fgs.ord-no) .
                        WHEN "po-dt"       THEN 
                            cVarValue = "" .
                        WHEN "qty-ord"     THEN 
                            cVarValue = STRING(tt-fgs.ord-qty,"->>>,>>>,>>9.99") .
                        WHEN "qty-rcv"     THEN 
                            cVarValue = "" .
                        WHEN "rfq-dt"      THEN 
                            cVarValue = IF tt-fgs.due-date NE ? THEN STRING(tt-fgs.due-date) ELSE ""  .
                        WHEN "carr"        THEN 
                            cVarValue = "" .
                        WHEN "job-no"      THEN 
                            cVarValue = "" .
                        WHEN "size"        THEN 
                            cVarValue = STRING(tt-fgs.job-no) .
                        WHEN "uom"         THEN 
                            cVarValue = ""  .
                        WHEN "cust"        THEN 
                            cVarValue = STRING(tt-fgs.cust-no)  .

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

        /*  DOWN WITH FRAME sch-rcts-job.
          {porep\r-rschrp.i sch-rcts-job} */

        END.

    ELSE
        IF v-sort EQ "I" THEN
            FOR EACH tt-sched WHERE (IF v-rcv-date-selected = NO OR tt-sched.rct-date = ? THEN TRUE 
            ELSE 
                (tt-sched.rct-date GE v-start-rcv-date
                AND tt-sched.rct-date LE v-end-rcv-date))
                USE-INDEX i-no BREAK BY tt-sched.i-no:
                lv-job-no = IF tt-sched.job-no EQ "" THEN ""
                ELSE TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', tt-sched.job-no, tt-sched.job-no2))).

                /*  DISPLAY lv-job-no
                          tt-sched.i-no
                          tt-sched.loc-bin
                          tt-sched.i-name
                          tt-sched.vend-no
                          tt-sched.po-no
                          tt-sched.po-date
                          tt-sched.cons-uom
                          tt-sched.cons-qty
                          tt-sched.t-rec-qty
                          tt-sched.due-date
                          tt-sched.carrier
                      WITH FRAME sch-rcts-item.
             
                  IF tb_excel THEN
                     PUT STREAM excel UNFORMATTED
                       '"' tt-sched.i-no                                '",'
                       '"' tt-sched.loc-bin                             '",'
                       '"' tt-sched.i-name                              '",'
                       '"' tt-sched.vend-no                             '",'
                       '"' STRING(tt-sched.po-no)                       '",'
                       '"' ""                                           '",'
                       '"' (IF tt-sched.po-date NE ? THEN
                               STRING(tt-sched.po-date) ELSE "")        '",'
                       '"' tt-sched.cons-uom                            '",'
                       '"' STRING(tt-sched.cons-qty,"->>>,>>>,>>9.99")  '",'
                       '"' STRING(tt-sched.t-rec-qty,"->>>,>>>,>>9.99") '",'
                       '"' (IF tt-sched.due-date NE ? THEN
                               STRING(tt-sched.due-date) ELSE "")       '",'
                       '"' tt-sched.carrier                             '",'
                       '"' lv-job-no                                    '",'
                       SKIP. */

                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "vend"        THEN 
                            cVarValue = STRING(tt-sched.vend-no,"x(8)") .
                        WHEN "vend-name"   THEN 
                            cVarValue = STRING(tt-sched.vend-name,"x(30)")  .
                        WHEN "i-no"        THEN 
                            cVarValue = STRING(tt-sched.i-no,"x(15)") .
                        WHEN "fg-itm"      THEN 
                            cVarValue = "" .
                        WHEN "bin"         THEN 
                            cVarValue = STRING(tt-sched.loc-bin)  .
                        WHEN "i-name"      THEN 
                            cVarValue = STRING(tt-sched.i-name).
                        WHEN "cust-nam"    THEN 
                            cVarValue = "".
                        WHEN "po"          THEN 
                            cVarValue = STRING(tt-sched.po-no) .
                        WHEN "ord"         THEN 
                            cVarValue = "" .
                        WHEN "po-dt"       THEN 
                            cVarValue = IF tt-sched.po-date NE ? THEN STRING(tt-sched.po-date) ELSE "" .
                        WHEN "qty-ord"     THEN 
                            cVarValue = STRING(tt-sched.cons-qty,"->>>,>>>,>>9.99") .
                        WHEN "qty-rcv"     THEN 
                            cVarValue = STRING(tt-sched.t-rec-qty,"->>>,>>>,>>9.99") .
                        WHEN "rfq-dt"      THEN 
                            cVarValue = IF tt-sched.due-date NE ? THEN STRING(tt-sched.due-date) ELSE ""  .
                        WHEN "carr"        THEN 
                            cVarValue = STRING(tt-sched.carrier).
                        WHEN "job-no"      THEN 
                            cVarValue = STRING(lv-job-no).
                        WHEN "size"        THEN 
                            cVarValue = "" .
                        WHEN "uom"         THEN 
                            cVarValue = STRING(tt-sched.cons-uom) .

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


                IF tb_printNotes THEN RUN printNotes (tt-sched.rec_key).


                FOR EACH tt-fgs WHERE tt-fgs.sched-rowid = ROWID(tt-sched)
                    /* WITH FRAME {1}*/
                    BREAK BY tt-fgs.i-no
                    BY tt-fgs.due-date:


                    ASSIGN 
                        cDisplay       = ""
                        cTmpField      = ""
                        cVarValue      = ""
                        cExcelDisplay  = ""
                        cExcelVarValue = "".

                    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                        CASE cTmpField:             
                            WHEN "vend"        THEN 
                                cVarValue = "" .
                            WHEN "vend-name"   THEN 
                                cVarValue = ""  .
                            WHEN "i-no"        THEN 
                                cVarValue = "" .
                            WHEN "fg-itm"      THEN 
                                cVarValue = STRING(tt-fgs.i-no,"x(15)") .
                            WHEN "bin"         THEN 
                                cVarValue = ""  .
                            WHEN "i-name"      THEN 
                                cVarValue = "" .
                            WHEN "cust-nam"    THEN 
                                cVarValue = STRING(tt-fgs.cust-name).
                            WHEN "po"          THEN 
                                cVarValue = "" .
                            WHEN "ord"         THEN 
                                cVarValue = STRING(tt-fgs.ord-no) .
                            WHEN "po-dt"       THEN 
                                cVarValue = "" .
                            WHEN "qty-ord"     THEN 
                                cVarValue = STRING(tt-fgs.ord-qty,"->>>,>>>,>>9.99") .
                            WHEN "qty-rcv"     THEN 
                                cVarValue = "" .
                            WHEN "rfq-dt"      THEN 
                                cVarValue = IF tt-fgs.due-date NE ? THEN STRING(tt-fgs.due-date) ELSE ""  .
                            WHEN "carr"        THEN 
                                cVarValue = "" .
                            WHEN "job-no"      THEN 
                                cVarValue = "" .
                            WHEN "size"        THEN 
                                cVarValue = STRING(tt-fgs.job-no) .
                            WHEN "uom"         THEN 
                                cVarValue = ""  .
                            WHEN "cust"        THEN 
                                cVarValue = STRING(tt-fgs.cust-no)  .

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

            /* DOWN WITH FRAME sch-rcts-item.
             {porep\r-rschrp.i sch-rcts-item} */
            END.

        ELSE 
            FOR EACH tt-sched WHERE (IF v-rcv-date-selected = NO OR tt-sched.rct-date = ? THEN TRUE 
            ELSE 
                (tt-sched.rct-date GE v-start-rcv-date
                AND tt-sched.rct-date LE v-end-rcv-date))
                USE-INDEX vend BREAK BY tt-sched.vend-no:
                lv-job-no = IF tt-sched.job-no EQ "" THEN ""
                ELSE TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', tt-sched.job-no, tt-sched.job-no2))).

                IF FIRST-OF(tt-sched.vend-no) THEN 
                DO:
                /* PUT SKIP(1).
                 DISPLAY tt-sched.vend-no tt-sched.vend-name WITH FRAME sch-rcts-vend. */
                END.


                /*  DISPLAY lv-job-no
                          tt-sched.i-no
                          tt-sched.loc-bin
                          tt-sched.i-name
                          tt-sched.po-no
                          tt-sched.po-date
                          tt-sched.cons-qty
                          tt-sched.t-rec-qty
                          tt-sched.due-date
                          /*tt-sched.amt-msf */
                          tt-sched.carrier
                      WITH FRAME sch-rcts-vend.
             
                  IF tb_excel THEN
                     PUT STREAM excel UNFORMATTED
                       '"' (IF FIRST-OF(tt-sched.vend-no) THEN
                               tt-sched.vend-no ELSE "")                '",'
                       '"' (IF FIRST-OF(tt-sched.vend-no) THEN
                               tt-sched.vend-name ELSE "")              '",'
                       '"' tt-sched.i-no                                '",'
                       '"' tt-sched.loc-bin                             '",'
                       '"' tt-sched.i-name                              '",'
                       '"' STRING(tt-sched.po-no)                       '",'
                       '"' ""                                           '",'
                       '"' (IF tt-sched.po-date NE ? THEN
                               STRING(tt-sched.po-date) ELSE "")        '",'
                       '"' STRING(tt-sched.cons-qty,"->>>,>>>,>>9.99")  '",'
                       '"' STRING(tt-sched.t-rec-qty,"->>>,>>>,>>9.99") '",'
                       '"' (IF tt-sched.due-date NE ? THEN              
                               STRING(tt-sched.due-date) ELSE "")       '",'
                      /* '"' STRING(tt-sched.amt-msf,"->,>>9.999")        '",' */
                       '"' tt-sched.carrier                             '",'
                       '"' lv-job-no                                    '",'
                       SKIP. */

                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "vend"        THEN 
                            cVarValue = IF FIRST-OF(tt-sched.vend-no) THEN STRING(tt-sched.vend-no,"x(8)") ELSE "" .
                        WHEN "vend-name"   THEN 
                            cVarValue = IF FIRST-OF(tt-sched.vend-no) THEN STRING(tt-sched.vend-name,"x(30)") ELSE "" .
                        WHEN "i-no"        THEN 
                            cVarValue = STRING(tt-sched.i-no,"x(15)") .
                        WHEN "fg-itm"      THEN 
                            cVarValue = "" .
                        WHEN "bin"         THEN 
                            cVarValue = STRING(tt-sched.loc-bin)  .
                        WHEN "i-name"      THEN 
                            cVarValue = STRING(tt-sched.i-name).
                        WHEN "cust-nam"    THEN 
                            cVarValue = "".
                        WHEN "po"          THEN 
                            cVarValue = STRING(tt-sched.po-no) .
                        WHEN "ord"         THEN 
                            cVarValue = "" .
                        WHEN "po-dt"       THEN 
                            cVarValue = IF tt-sched.po-date NE ? THEN STRING(tt-sched.po-date) ELSE "" .
                        WHEN "qty-ord"     THEN 
                            cVarValue = STRING(tt-sched.cons-qty,"->>>,>>>,>>9.99") .
                        WHEN "qty-rcv"     THEN 
                            cVarValue = STRING(tt-sched.t-rec-qty,"->>>,>>>,>>9.99") .
                        WHEN "rfq-dt"      THEN 
                            cVarValue = IF tt-sched.due-date NE ? THEN STRING(tt-sched.due-date) ELSE ""  .
                        WHEN "carr"        THEN 
                            cVarValue = STRING(tt-sched.carrier).
                        WHEN "job-no"      THEN 
                            cVarValue = STRING(lv-job-no).
                        WHEN "size"        THEN 
                            cVarValue = "" .
                        WHEN "uom"         THEN 
                            cVarValue = STRING(tt-sched.cons-uom) .

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


                IF tb_printNotes THEN RUN printNotes (tt-sched.rec_key).


                FOR EACH tt-fgs WHERE tt-fgs.sched-rowid = ROWID(tt-sched)
                    /* WITH FRAME {1}*/
                    BREAK BY tt-fgs.i-no
                    BY tt-fgs.due-date:


                    ASSIGN 
                        cDisplay       = ""
                        cTmpField      = ""
                        cVarValue      = ""
                        cExcelDisplay  = ""
                        cExcelVarValue = "".

                    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                        CASE cTmpField:             
                            WHEN "vend"        THEN 
                                cVarValue = "" .
                            WHEN "vend-name"   THEN 
                                cVarValue = ""  .
                            WHEN "i-no"        THEN 
                                cVarValue = "" .
                            WHEN "fg-itm"      THEN 
                                cVarValue = STRING(tt-fgs.i-no,"x(15)") .
                            WHEN "bin"         THEN 
                                cVarValue = ""  .
                            WHEN "i-name"      THEN 
                                cVarValue = "" .
                            WHEN "cust-nam"    THEN 
                                cVarValue = STRING(tt-fgs.cust-name).
                            WHEN "po"          THEN 
                                cVarValue = "" .
                            WHEN "ord"         THEN 
                                cVarValue = STRING(tt-fgs.ord-no) .
                            WHEN "po-dt"       THEN 
                                cVarValue = "" .
                            WHEN "qty-ord"     THEN 
                                cVarValue = STRING(tt-fgs.ord-qty,"->>>,>>>,>>9.99") .
                            WHEN "qty-rcv"     THEN 
                                cVarValue = "" .
                            WHEN "rfq-dt"      THEN 
                                cVarValue = IF tt-fgs.due-date NE ? THEN STRING(tt-fgs.due-date) ELSE ""  .
                            WHEN "carr"        THEN 
                                cVarValue = "" .
                            WHEN "job-no"      THEN 
                                cVarValue = "" .
                            WHEN "size"        THEN 
                                cVarValue = STRING(tt-fgs.job-no) .
                            WHEN "uom"         THEN 
                                cVarValue = ""  .
                            WHEN "cust"        THEN 
                                cVarValue = STRING(tt-fgs.cust-no)  .

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

            /* DOWN WITH FRAME sch-rcts-vend. 
             {porep\r-rschrp.i sch-rcts-vend} */
            END. 

    IF CAN-FIND(FIRST tt-sched) THEN
        PUT SKIP(1) "Total Value:" AT 100 v-tot FORMAT ">>,>>>,>>9.99" SKIP(1).

    IF rd-dest = 3 THEN 
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
    DEFINE VARIABLE lv-label      AS cha     NO-UNDO.

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
            fi_file:SCREEN-VALUE = "c:\tmp\r-rschrp.csv".   
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

