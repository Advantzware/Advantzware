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

{sys/inc/VAR.i NEW SHARED}

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
    FIELD po-date   LIKE po-ord.po-date
    FIELD cons-uom  LIKE po-ordl.cons-uom
    FIELD cons-qty  LIKE po-ordl.cons-qty
    FIELD t-rec-qty LIKE po-ordl.t-rec-qty
    FIELD due-date  LIKE po-ordl.due-date
    FIELD amt-msf   LIKE ap-invl.amt-msf
    FIELD vend-name LIKE vend.NAME
    FIELD carrier   LIKE po-ord.carrier  
    FIELD m-code    LIKE job-mch.m-code   
    FIELD rec_key   LIKE po-ordl.rec_key
    FIELD buyer     LIKE po-ord.buyer
    FIELD USER-ID   LIKE po-ord.USER-ID
    FIELD custNo    LIKE po-ordl.cust-no
    FIELD ordNo     LIKE po-ordl.ord-no
    FIELD costLine  LIKE po-ordl.cost
    FIELD costUom   LIKE po-ordl.pr-uom
    FIELD totCost   LIKE po-ordl.t-cost      
    INDEX job  job-no  job-no2
    INDEX i-no i-no
    INDEX vend vend-no.

DEFINE VARIABLE v-print-fmt    AS CHARACTER.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE STREAM excel.
DEFINE VARIABLE ldummy             AS LOGICAL   NO-UNDO.
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
    cTextListToSelect  = "Job No,Item No,Item Name,Vend No,Vend Name,P/O#,P/O Date,UOM," +  /*8*/
                               "Qty Order,Qty Received,Req Date,MSF,Carrier,First Resource,Buyer Id,User Id," + /*7*/
                               "Customer#,Order#,Cost Line Item,Cost Uom,Total Cost Line Item"
    cFieldListToSelect = "lv-job-no,tt-sched.i-no,tt-sched.i-name,tt-sched.vend-no,tt-sched.vend-name,tt-sched.po-no,po-date,tt-sched.cons-uom," +
                                "tt-sched.cons-qty,tt-sched.t-rec-qty,tt-sched.due-date,tt-sched.amt-msf,tt-sched.carrier,tt-sched.m-code,tt-sched.buyer,tt-sched.user-id," +
                                "tt-sched.custno,tt-sched.ordno,tt-sched.costLine,tt-sched.costUom,tt-sched.totCost"
    cFieldLength       = "13,15,30,8,30,8,10,4," + "15,15,8,13,7,14,10,8,10,8," + "14,8,20"
    cFieldType         = "c,c,c,c,c,c,c,c," + "i,i,c,i,c,c,c,c,c,i," + "i,c,i"
    .
ASSIGN 
    cTextListToDefault = "Job No,Item No,Item Name,Vend No,P/O#,P/O Date,UOM," +  /*8*/
                                     "Qty Order,Qty Received,Req Date,MSF,Carrier"  . /*5*/


{sys/inc/ttRptSel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE NO

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_vend-no end_vend-no ~
begin_due-date end_due-date begin_procat end_procat begin_cat end_cat ~
begin_buyer end_buyer begin_user-id end_user-id select-mat rd_show ~
tb_type-1 tb_type-2 tb_type-3 tb_late tb_printNotes sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest td-show-parm fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_vend-no end_vend-no begin_due-date ~
end_due-date begin_procat end_procat begin_cat end_cat begin_buyer ~
end_buyer begin_user-id end_user-id select-mat rd_show tb_type-1 tb_type-2 ~
tb_type-3 tb_late tb_printNotes sl_avail sl_selected rd-dest td-show-parm ~
fi_file tb_OpenCSV tbAutoClose lbl_show 

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

DEFINE VARIABLE begin_buyer    AS CHARACTER FORMAT "X(10)":U 
    LABEL "Beginning Buyer ID" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cat      AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning FG Category" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_due-date AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Due Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat   AS CHARACTER FORMAT "X(6)":U 
    LABEL "Beginning RM Category" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_user-id  AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning User ID" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend-no  AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_buyer      AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Buyer ID" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_cat        AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending FG Category" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_due-date   AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Due Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat     AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
    LABEL "Ending RM Category" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_user-id    AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending User ID" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend-no    AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-schrpt.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE
    SIZE 43 BY 1.

DEFINE VARIABLE lbl_show       AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
    VIEW-AS TEXT 
    SIZE 5.6 BY .62 NO-UNDO.

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

DEFINE VARIABLE mat-types      AS CHARACTER FORMAT "X(256)":U 
    LABEL "Material Types" 
    VIEW-AS TEXT 
    SIZE 1 BY .62 NO-UNDO.

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

DEFINE VARIABLE rd_show        AS CHARACTER INITIAL "Open" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Open", "Open",
    "Closed (Not Received)", "Closed (Not Received)",
    "All PO's", "All PO's"
    SIZE 45.2 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 93 BY 5.24.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 93 BY 13.1.

DEFINE VARIABLE select-mat    AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 28 BY 4.52 NO-UNDO.

DEFINE VARIABLE sl_avail      AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 35 BY 5.81 NO-UNDO.

DEFINE VARIABLE sl_selected   AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 35.2 BY 5.81 NO-UNDO.

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
    SIZE 14 BY .81
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE tb_type-1     AS LOGICAL   INITIAL NO 
    LABEL "Regular" 
    VIEW-AS TOGGLE-BOX
    SIZE 10.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_type-2     AS LOGICAL   INITIAL NO 
    LABEL "Drop Ships" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_type-3     AS LOGICAL   INITIAL NO 
    LABEL "Sheeting" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm  AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_vend-no AT ROW 2.67 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_vend-no AT ROW 2.67 COL 69 COLON-ALIGNED HELP
    "Enter Ending Vendor number"
    begin_due-date AT ROW 3.76 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Due Date"
    end_due-date AT ROW 3.76 COL 69 COLON-ALIGNED HELP
    "Enter ending Due Date"
    begin_procat AT ROW 4.86 COL 28 COLON-ALIGNED
    end_procat AT ROW 4.86 COL 69 COLON-ALIGNED HELP
    "Enter Ending Category"
    begin_cat AT ROW 5.95 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Category"
    end_cat AT ROW 5.95 COL 69 COLON-ALIGNED HELP
    "Enter Ending Order Number"
    begin_buyer AT ROW 7.05 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Category" WIDGET-ID 160
    end_buyer AT ROW 7.05 COL 69 COLON-ALIGNED HELP
    "Enter Ending Order Number" WIDGET-ID 162
    begin_user-id AT ROW 8.14 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Category" WIDGET-ID 164
    end_user-id AT ROW 8.14 COL 69 COLON-ALIGNED HELP
    "Enter Ending Order Number" WIDGET-ID 166
    select-mat AT ROW 10.05 COL 66.2 NO-LABELS
    rd_show AT ROW 10.19 COL 14.8 NO-LABELS
    tb_type-1 AT ROW 11.14 COL 15.2 WIDGET-ID 152
    tb_type-2 AT ROW 11.14 COL 27.2 WIDGET-ID 154
    tb_type-3 AT ROW 11.14 COL 43.2 WIDGET-ID 156
    tb_late AT ROW 12.05 COL 15.2
    tb_printNotes AT ROW 13 COL 15.2
    sl_avail AT ROW 15.67 COL 2.4 NO-LABELS WIDGET-ID 146
    Btn_Def AT ROW 15.67 COL 41 HELP
    "Default Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 15.67 COL 60.2 NO-LABELS WIDGET-ID 148
    Btn_Add AT ROW 16.86 COL 41 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 138
    Btn_Remove AT ROW 18.05 COL 41 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 142
    btn_Up AT ROW 19.24 COL 41 WIDGET-ID 144
    btn_down AT ROW 20.43 COL 41 WIDGET-ID 140
    tb_excel AT ROW 22.57 COL 55 RIGHT-ALIGNED
    lv-ornt AT ROW 22.57 COL 36 NO-LABELS
    lv-font-no AT ROW 22.57 COL 37 COLON-ALIGNED
    lines-per-page AT ROW 22.57 COL 79 COLON-ALIGNED
    lv-font-name AT ROW 22.67 COL 30 COLON-ALIGNED NO-LABELS
    rd-dest AT ROW 23.19 COL 5.8 NO-LABELS
    td-show-parm AT ROW 25.1 COL 41.2
    fi_file AT ROW 26 COL 27.2 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 26.05 COL 85.2 RIGHT-ALIGNED
    tbAutoClose AT ROW 28.29 COL 26.8 WIDGET-ID 16
    btn-ok AT ROW 29.1 COL 27
    btn-cancel AT ROW 29.1 COL 48.2
    lbl_show AT ROW 10.38 COL 6.2 COLON-ALIGNED NO-LABELS
    mat-types AT ROW 12 COL 62.2 COLON-ALIGNED
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 14.95 COL 60.6 WIDGET-ID 44
    "Available Columns" VIEW-AS TEXT
    SIZE 22 BY .62 AT ROW 15 COL 9.8 WIDGET-ID 38
    "Po Type?" VIEW-AS TEXT
    SIZE 10 BY .62 AT ROW 11.29 COL 5 WIDGET-ID 158
    "Select/Deselect RM Types" VIEW-AS TEXT
    SIZE 26.8 BY .71 AT ROW 9.29 COL 67.6
    "Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 22.05 COL 3.2
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.38 COL 3.6
    BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96.2 BY 30.33
    BGCOLOR 15 .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
    RECT-6 AT ROW 22.43 COL 2.4
    RECT-7 AT ROW 1.71 COL 2.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96.2 BY 30.33
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
        TITLE              = "Scheduled Receipts"
        HEIGHT             = 30.33
        WIDTH              = 96.2
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
    begin_buyer:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_cat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_due-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_procat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_user-id:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_vend-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_buyer:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_due-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_procat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_user-id:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_vend-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_show IN FRAME FRAME-A
   NO-ENABLE                                                            */
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
    rd_show:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    select-mat:AUTO-RESIZE IN FRAME FRAME-A = TRUE.

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
ON END-ERROR OF C-Win /* Scheduled Receipts */
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
ON WINDOW-CLOSE OF C-Win /* Scheduled Receipts */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_buyer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_buyer C-Win
ON LEAVE OF begin_buyer IN FRAME FRAME-A /* Beginning Buyer ID */
    DO:
        ASSIGN {&self-name}.
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
ON LEAVE OF begin_due-date IN FRAME FRAME-A /* Beginning Due Date */
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


&Scoped-define SELF-NAME begin_user-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_user-id C-Win
ON LEAVE OF begin_user-id IN FRAME FRAME-A /* Beginning User ID */
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
        DELETE PROCEDURE hdOutputProcs.
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
            ASSIGN 
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
                        ELSE DO:
                            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                        END.
                    END. /* WHEN 3 THEN DO: */
                WHEN 4 THEN 
                    DO:
           /*run output-to-fax.*/
                        {custom/asifax.i &begin_cust=begin_vend-no
                            &END_cust=END_vend-no
                            &fax-subject=c-win:TITLE
                            &fax-body=c-win:TITLE
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
                             &mail-subject=c-win:TITLE
                             &mail-body=c-win:TITLE
                             &mail-file=list-name }
                        END.
                        ELSE 
                        DO:
                            {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust= begin_vend-no
                                  &END_cust=end_vend-no
                                  &mail-subject=c-win:TITLE
                                  &mail-body=c-win:TITLE
                                  &mail-file=list-name }

                        END.

                    END. 
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
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def C-Win
ON CHOOSE OF Btn_Def IN FRAME FRAME-A /* Default */
    DO:
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04141406 */ 
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


&Scoped-define SELF-NAME end_buyer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_buyer C-Win
ON LEAVE OF end_buyer IN FRAME FRAME-A /* Ending Buyer ID */
    DO:
        ASSIGN {&self-name}.
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
ON LEAVE OF end_due-date IN FRAME FRAME-A /* Ending Due Date */
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


&Scoped-define SELF-NAME end_user-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_user-id C-Win
ON LEAVE OF end_user-id IN FRAME FRAME-A /* Ending User ID */
    DO:
        ASSIGN {&self-name}.
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
    // assign {&self-name}.
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
        RUN pChangeDest .
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

        IF (NOT CAN-DO(sl_selected:LIST-ITEMS,{&SELF-NAME}:SCREEN-VALUE) OR
            sl_selected:NUM-ITEMS = 0)
            THEN ASSIGN ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
                ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
                /* sl_selected:SCREEN-VALUE = sl_selected:ENTRY(sl_selected:NUM-ITEMS) */
                .

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


&Scoped-define SELF-NAME tb_type-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_type-1 C-Win
ON VALUE-CHANGED OF tb_type-1 IN FRAME FRAME-A /* Regular */
    DO:
        ASSIGN {&SELF-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_type-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_type-2 C-Win
ON VALUE-CHANGED OF tb_type-2 IN FRAME FRAME-A /* Drop Ships */
    DO:
        ASSIGN {&SELF-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_type-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_type-3 C-Win
ON VALUE-CHANGED OF tb_type-3 IN FRAME FRAME-A /* Sheeting */
    DO:
        ASSIGN {&SELF-NAME}.
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
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.

    FOR EACH mat:
        v-mat-list = v-mat-list + STRING(mat.mat,"x(5)") + " " + mat.dscr + ",".
    END.
    IF SUBSTR(v-mat-list,LENGTH(TRIM(v-mat-list)),1) EQ "," THEN
        SUBSTR(v-mat-list,LENGTH(TRIM(v-mat-list)),1) = "".

    select-mat:LIST-ITEMS = v-mat-list.

    DO i = 1 TO select-mat:NUM-ITEMS:
        IF TRIM(SUBSTR(select-mat:ENTRY(i),1,5)) EQ "B" THEN 
        DO:
            select-mat:SCREEN-VALUE = ENTRY(i,v-mat-list).
            LEAVE.
        END.
    END.

    {methods/nowait.i}
    {sys/inc/reportsConfigNK1.i "PR2" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
RUN DisplaySelectionList2.
APPLY "entry" TO begin_vend-no.  
END.

RUN pChangeDest .
  
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
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
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
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.
  
    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:
        RETURN.
    END.

    EMPTY TEMP-TABLE ttRptList.

    DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

        cListContents = cListContents +
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect) .
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
        end_procat begin_cat end_cat begin_buyer end_buyer begin_user-id 
        end_user-id select-mat rd_show tb_type-1 tb_type-2 tb_type-3 tb_late 
        tb_printNotes sl_avail sl_selected rd-dest td-show-parm fi_file 
        tb_OpenCSV tbAutoClose lbl_show 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_vend-no end_vend-no begin_due-date end_due-date 
        begin_procat end_procat begin_cat end_cat begin_buyer end_buyer 
        begin_user-id end_user-id select-mat rd_show tb_type-1 tb_type-2 
        tb_type-3 tb_late tb_printNotes sl_avail Btn_Def sl_selected Btn_Add 
        Btn_Remove btn_Up btn_down rd-dest td-show-parm fi_file tb_OpenCSV 
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
    iColumnLength = 0.

    DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
        FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.

        IF NOT AVAILABLE ttRptList THEN
            MESSAGE "no " i ENTRY(i,ctmplist) SKIP
                ctmplist
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        CREATE ttRptSelected.
        ASSIGN 
            ttRptSelected.TextList        = ENTRY(i,cTmpList)
            ttRptSelected.FieldList       = ttRptList.FieldList
            ttRptSelected.FieldLength     = INT(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
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
    DEFINE INPUT PARAMETER MOVE AS CHARACTER NO-UNDO.

    DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME}
        WITH FRAME {&FRAME-NAME}:
        IF sl_selected:IS-SELECTED(i) THEN
        DO:
            IF MOVE = "Down" AND i NE sl_selected:NUM-ITEMS THEN
                ASSIGN
                    ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
                    ldummy                   = sl_selected:DELETE(i)
                    sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
                    .
            ELSE
                IF MOVE = "Up" AND i NE 1 THEN
                    ASSIGN
                        ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i - 1)
                        ldummy                   = sl_selected:DELETE(i + 1)
                        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i - 1)
                        .
            LEAVE.
        END.
    END.
END PROCEDURE.

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
    RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
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
    DEFINE VARIABLE str-tit4 AS cha NO-UNDO.
    DEFINE VARIABLE str-tit5 AS cha NO-UNDO.
    DEFINE VARIABLE str-line AS cha FORM "x(300)" NO-UNDO.

    {sys/FORM/r-top5DL.f}

    DEFINE VARIABLE v-sort         AS CHARACTER FORMAT "x" INIT "J" NO-UNDO.
    DEFINE VARIABLE code-text      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE stat-list      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-len          LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid          LIKE po-ordl.s-wid NO-UNDO.
    DEFINE VARIABLE v-bwt          LIKE ITEM.basis-w NO-UNDO.
    DEFINE VARIABLE v-dep          LIKE po-ordl.s-wid NO-UNDO.
    DEFINE VARIABLE v-qty          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-cost         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-s-num        LIKE po-ordl.s-num INIT 1 NO-UNDO.
    DEFINE VARIABLE v-tot          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-job-no      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-uom         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-ord-qty      AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE v-mattype-list AS CHARACTER FORMAT "x(36)" NO-UNDO.
    DEFINE VARIABLE v-mat-dscr     AS CHARACTER FORMAT "x(20)" EXTENT 21 NO-UNDO.
    DEFINE VARIABLE excelheader    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDisplay       AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS cha       NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHA       NO-UNDO.
    DEFINE VARIABLE cVarValue      AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS cha       NO-UNDO.
    DEFINE VARIABLE cFieldName     AS cha       NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS cha       NO-UNDO.
    DEFINE BUFFER b-tt-sched FOR tt-sched .

    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    {ce/msfcalc.i}

    ASSIGN
        str-tit2  = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-s-vend  = begin_vend-no
        v-e-vend  = end_vend-no
        v-s-date  = begin_due-date
        v-e-date  = end_due-date
        v-po-stat = SUBSTR(rd_show,1,1) .

    DO WITH FRAME {&FRAME-NAME}:          
        DO i = 1 TO select-mat:NUM-ITEMS:
            IF select-mat:IS-SELECTED(i) THEN
                v-mattype-list = v-mattype-list + TRIM(SUBSTR(select-mat:ENTRY(i),1,5)) + ",".
        END.

        IF LENGTH(TRIM(v-mattype-list)) EQ 0 THEN
        DO:
            MESSAGE "No Material Type Selected."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            op-valid = NO.
            LEAVE.
        END.

        IF SUBSTR(v-mattype-list,LENGTH(TRIM(v-mattype-list)),1) EQ "," THEN
            SUBSTR(v-mattype-list,LENGTH(TRIM(v-mattype-list)),1) = "".

        mat-types = v-mattype-list.

        DO i = 1 TO LENGTH(mat-types):
            IF SUBSTR(mat-types,i,1) EQ "," THEN SUBSTR(mat-types,i,1) = " ".
        END.

        DISPLAY mat-types.
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

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
    END.

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    DISPLAY "" WITH FRAME r-top.

    EMPTY TEMP-TABLE tt-sched.

    stat-list = IF v-po-stat EQ "A" THEN ""             ELSE
        IF v-po-stat EQ "O" THEN "O,U,P,A,N,H"  ELSE "C,X,F".

    FOR EACH po-ord
        WHERE po-ord.company EQ cocode
        AND po-ord.vend-no GE v-s-vend
        AND po-ord.vend-no LE v-e-vend
        AND po-ord.buyer GE begin_buyer
        AND po-ord.buyer LE end_buyer
        AND po-ord.USER-ID GE begin_user-id
        AND po-ord.USER-ID LE end_user-id
        AND ((po-ord.TYPE = "R" AND tb_type-1) OR  (po-ord.TYPE = "D" AND tb_type-2) OR (po-ord.TYPE = "S" AND tb_type-3))
        /*and (lookup(po-ord.stat,stat-list) gt 0 or v-po-stat eq "A")*/
        AND ((po-ord.opened AND v-po-stat EQ "O") OR
        (NOT po-ord.opened AND v-po-stat EQ "C") OR
        v-po-stat EQ "A")
        NO-LOCK,

        EACH po-ordl
        WHERE po-ordl.company  EQ po-ord.company
        AND po-ordl.po-no    EQ po-ord.po-no
        AND (CAN-DO(stat-list,po-ordl.stat) OR v-po-stat EQ "A")
        AND po-ordl.due-date GE v-s-date
        AND po-ordl.due-date LE v-e-date
        AND ((po-ordl.item-type AND
        CAN-FIND(FIRST ITEM
        WHERE ITEM.company EQ po-ordl.company
        AND ITEM.i-no    EQ po-ordl.i-no
        AND CAN-DO(v-mattype-list,ITEM.mat-type)
        AND ITEM.procat  GE begin_procat
        AND ITEM.procat  LE end_procat
        AND ITEM.inv-by-cust EQ NO)) OR
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

            FIND FIRST ITEM NO-LOCK
                WHERE ITEM.company EQ po-ordl.company
                AND ITEM.i-no    EQ po-ordl.i-no
                NO-ERROR.

            IF AVAILABLE ITEM THEN 
            DO:
                v-dep = ITEM.s-dep.
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
        FIND FIRST job-hdr NO-LOCK
            WHERE job-hdr.company EQ cocode
            AND job-hdr.job-no  EQ po-ordl.job-no
            AND job-hdr.job-no2 EQ po-ordl.job-no2
            NO-ERROR.
        IF AVAILABLE job-hdr THEN 
            FIND FIRST  job-mch NO-LOCK 
                WHERE job-mch.company EQ cocode
                AND job-mch.job EQ job-hdr.job                
                AND job-mch.frm EQ po-ordl.s-num USE-INDEX seq-idx NO-ERROR.        

        /*IF po-ordl.ord-qty - v-qty GT 0 THEN DO:*/
        IF v-ord-qty - v-qty GT 0 THEN 
        DO:

            IF v-cost EQ ? THEN v-cost = 0.

            CREATE tt-sched.
            ASSIGN
                v-tot              = v-tot + ((po-ordl.ord-qty - v-qty) * v-cost)
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
                tt-sched.rec_key   = po-ordl.rec_key
                tt-sched.m-code    = IF AVAILABLE job-mch THEN job-mch.m-code ELSE ""
                tt-sched.buyer     = po-ord.buyer
                tt-sched.USER-ID   = po-ord.USER-ID
                tt-sched.custno    = po-ordl.cust-no
                tt-sched.ordno     = po-ordl.ord-no
                tt-sched.costLine  = po-ordl.cost
                tt-sched.costUom   = po-ordl.pr-uom
                tt-sched.totCost   = po-ordl.t-cost
                .

            /*IF v-sort EQ "V" THEN DO:*/
            FIND FIRST vend NO-LOCK
                WHERE vend.company EQ po-ord.company
                AND vend.vend-no EQ po-ord.vend-no
                NO-ERROR.

            ASSIGN
                tt-sched.vend-name = IF AVAILABLE vend THEN vend.NAME ELSE ""
                v-s-num            = po-ordl.s-num
                tt-sched.amt-msf   = tt-sched.cons-qty - tt-sched.t-rec-qty.

            IF tt-sched.cons-uom NE "MSF" THEN
                RUN sys/ref/convquom.p(tt-sched.cons-uom, "MSF",
                    v-bwt, v-len, v-wid, v-dep,
                    tt-sched.amt-msf, OUTPUT tt-sched.amt-msf).
        /* END.*/
        END.
    END.

    FOR EACH tt-sched USE-INDEX job BREAK BY tt-sched.job-no BY tt-sched.job-no2:

    {custom/statusMsg.i " 'Processing PO#  '  + string(tt-sched.po-no) "}

        lv-job-no = IF tt-sched.job-no EQ "" THEN ""
        ELSE TRIM(tt-sched.job-no) + "-" + STRING(tt-sched.job-no2,"999").
        /*MESSAGE "test" STRING(tt-sched.cons-qty)     STRING(tt-sched.amt-msf) VIEW-AS ALERT-BOX ERROR.*/

        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".
        BUFFER b-tt-sched:FIND-BY-ROWID(ROWID(tt-sched), NO-LOCK) .        
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            IF INDEX(cTmpField,".") > 0 THEN 
            DO:
                cFieldName = cTmpField.
                cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                hField = BUFFER b-tt-sched:BUFFER-FIELD(cTmpField).
                cTmpField = SUBSTRING(GetFieldValue(hField),1,INT(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                IF cFieldName = "tt-sched.cons-qty"
                    THEN cTmpField = STRING(DECIMAL(cTmpField),"->>>,>>>,>>9.99").
                IF cFieldName = "tt-sched.t-rec-qty"
                    THEN cTmpField = STRING(DECIMAL(cTmpField),"->>>,>>>,>>9.99"). 
                IF cFieldName = "tt-sched.amt-msf"
                    THEN cTmpField = STRING(DECIMAL(cTmpField),"->,>>>,>>9.99").
                IF cFieldName = "tt-sched.costline"
                    THEN cTmpField = STRING(DECIMAL(cTmpField),"->>,>>>,>>9.99<<<<").
                IF cFieldName = "tt-sched.totCost"
                    THEN cTmpField = STRING(DECIMAL(cTmpField),"->>,>>>,>>9.99<<").        
                cDisplay = cDisplay + cTmpField + 
                    FILL(" ",INT(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                    .
                cExcelDisplay = cExcelDisplay + QUOTER(GetFieldValue(hField)) + ",".       
            END.
            ELSE 
            DO:            
                CASE cTmpField: 
                    WHEN "lv-job-no" THEN 
                        cVarValue = lv-job-no.
                    WHEN "po-date" THEN 
                        cVarValue = STRING(tt-sched.po-date).                                                  
                END CASE.
                IF  cTmpField = "po-date" THEN
                     cExcelVarValue = IF tt-sched.po-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",tt-sched.po-date) ELSE "".
                ELSE cExcelVarValue = cVarValue.  
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",INT(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                cExcelDisplay = cExcelDisplay + QUOTER(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cExcelVarValue)) + ",". 
            END.
        END.
        PUT UNFORMATTED cDisplay SKIP.
        IF rd-dest = 3 THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                cExcelDisplay SKIP.
        END.


        IF tb_printNotes THEN RUN printNotes (tt-sched.rec_key).

    END.

    IF CAN-FIND(FIRST tt-sched) THEN
        PUT SKIP(1) "Total Value:" AT 100 v-tot FORMAT "->>,>>>,>>9.99" SKIP(1).

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE    NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-label      AS CHARACTER NO-UNDO.

    ASSIGN
        lv-frame-hdl = FRAME {&FRAME-NAME}:HANDLE
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
            ENTRY(i,parm-lbl-list) NE "" THEN 
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
            fi_file:SCREEN-VALUE = "c:\tmp\r-schrpt.csv".
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

