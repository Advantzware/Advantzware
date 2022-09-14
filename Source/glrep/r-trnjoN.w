&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: glrep\r-trnjou.w

  Description: GL Transaction Report

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
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
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

DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.

DEFINE STREAM excel.
DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cColumnInit        AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.

DEFINE VARIABLE lRecFound          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lAPInvoiceLength   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cNK1Value          AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

RUN sys/ref/nk1look.p (INPUT cocode, "APInvoiceLength", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cNK1Value, OUTPUT lRecFound).
IF lRecFound THEN
    lAPInvoiceLength = LOGICAL(cNK1Value) NO-ERROR.

ASSIGN 
    cTextListToSelect  = "Account,Account Description,Journal,Vendor,Name,Date,Inv#,Check#,Order#,Quantity," +
                            "Amt MSF,Discount,Amount,Currency Code,Created By,Created Date,Posted,Run#" 
    cFieldListToSelect = "account,acc-desc,jou,vend,name,date,inv,chk,ord,qty," +
                            "msf,dis,amt,curr-code,createdby,createddate,posted,run"
    cFieldType         = "c,c,c,c,c,c,i,i,i,i," + "i,i,i,c,c,c,c,i" 
    .
IF lAPInvoiceLength THEN
    ASSIGN cFieldLength = "25,30,8,8,35,8,20,7,8,13," + "10,10,14,13,10,12,6,7".
ELSE
    ASSIGN cFieldLength = "25,30,8,8,35,8,12,7,7,13," + "10,10,14,13,10,12,6,7".
      
           
{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Journal,Vendor,Name,Date,Inv#,Check#,Order#,Quantity," +
                           "Amt MSF,Discount,Amount" .

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
begin_accnt end_accnt tb_cashr tb_cashrvd tb_general tb_apckr tb_mcshrec ~
tb_arinv tb_apmem tb_cdisb tb_acpay tb_crmem tb_ap-purch tb_apvoidck ~
tb_adjust tb_oeinv tb_fgpost tb_jcost tb_autodist tb_rmpost tb_void_checks ~
btn_SelectColumns rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_accnt end_accnt ~
tb_cashr tb_cashrvd tb_general tb_apckr tb_mcshrec tb_arinv tb_apmem ~
tb_cdisb tb_acpay tb_crmem tb_ap-purch tb_apvoidck tb_adjust tb_oeinv ~
tb_fgpost tb_jcost tb_autodist tb_rmpost tb_void_checks rd-dest fi_file ~
tb_OpenCSV tbAutoClose 

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

DEFINE BUTTON btn_SelectColumns 
    LABEL "Select Columns" 
    SIZE 35 BY 1.19.

DEFINE BUTTON btn_Up 
    LABEL "Move Up" 
    SIZE 16 BY 1.

DEFINE VARIABLE begin_accnt    AS CHARACTER FORMAT "X(25)":U 
    LABEL "Beginning Acct#" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_accnt      AS CHARACTER FORMAT "X(25)":U INITIAL "zzzzzzzzzzzzzzzzzzzzzzzzz" 
    LABEL "Ending Acct#" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\GLTransaction.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

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
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 17 BY 4.76 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.24.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 13.1.

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

DEFINE VARIABLE tb_acpay       AS LOGICAL   INITIAL NO 
    LABEL "Accounts Payable" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_adjust      AS LOGICAL   INITIAL NO 
    LABEL "FG Adjustments" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_ap-purch    AS LOGICAL   INITIAL NO 
    LABEL "Accounts Payable Purchases" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_apckr       AS LOGICAL   INITIAL NO 
    LABEL "Accounts Payable Check Register" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_apmem       AS LOGICAL   INITIAL NO 
    LABEL "Accounts Payable Memo" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_apvoidck    AS LOGICAL   INITIAL NO 
    LABEL "Accounts Payable Void Check" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_arinv       AS LOGICAL   INITIAL NO 
    LABEL "Accounts Receivable Invoice" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_autodist    AS LOGICAL   INITIAL NO 
    LABEL "Automatic Distributions" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cashr       AS LOGICAL   INITIAL NO 
    LABEL "Cash Receipts" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cashrvd     AS LOGICAL   INITIAL NO 
    LABEL "Voided Cash Receipts" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cdisb       AS LOGICAL   INITIAL NO 
    LABEL "Cash Disbursements" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_crmem       AS LOGICAL   INITIAL NO 
    LABEL "Accounts Receivable Memo" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel       AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_fgpost      AS LOGICAL   INITIAL NO 
    LABEL "Finished Goods Posting" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_general     AS LOGICAL   INITIAL NO 
    LABEL "General Journal Entries" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_jcost       AS LOGICAL   INITIAL NO 
    LABEL "Job Cost Posting Register" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_mcshrec     AS LOGICAL   INITIAL NO 
    LABEL "Misc Cash Receipts" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_oeinv       AS LOGICAL   INITIAL NO 
    LABEL "Order Entry Invoice" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_rmpost      AS LOGICAL   INITIAL NO 
    LABEL "Raw Materials Posting" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV     AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_void_checks AS LOGICAL   INITIAL NO 
    LABEL "Voided Checks" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm   AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_date AT ROW 2.33 COL 19 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 2.33 COL 64 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_accnt AT ROW 3.29 COL 19 COLON-ALIGNED HELP
    "Enter Beginning Account Number"
    end_accnt AT ROW 3.29 COL 64 COLON-ALIGNED HELP
    "Enter Ending Account Number"
    tb_cashr AT ROW 4.71 COL 6
    tb_cashrvd AT ROW 4.71 COL 50 WIDGET-ID 2
    sl_avail AT ROW 5.57 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 5.57 COL 39 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 5.57 COL 58.4 NO-LABELS WIDGET-ID 28
    tb_general AT ROW 5.67 COL 6
    tb_apckr AT ROW 5.67 COL 50
    Btn_Add AT ROW 6.57 COL 39 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    tb_mcshrec AT ROW 6.62 COL 6
    tb_arinv AT ROW 6.62 COL 50
    tb_apmem AT ROW 7.57 COL 6
    Btn_Remove AT ROW 7.57 COL 39 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    tb_cdisb AT ROW 7.57 COL 50
    tb_acpay AT ROW 8.52 COL 6
    tb_crmem AT ROW 8.52 COL 50
    btn_Up AT ROW 8.62 COL 39 WIDGET-ID 40
    tb_ap-purch AT ROW 9.48 COL 6
    tb_apvoidck AT ROW 9.48 COL 50
    btn_down AT ROW 9.62 COL 39 WIDGET-ID 42
    tb_adjust AT ROW 10.43 COL 6
    tb_oeinv AT ROW 10.43 COL 50
    tb_fgpost AT ROW 11.38 COL 6
    tb_jcost AT ROW 11.38 COL 50
    tb_autodist AT ROW 12.33 COL 6
    tb_rmpost AT ROW 12.33 COL 50
    tb_void_checks AT ROW 13.29 COL 6 WIDGET-ID 4
    btn_SelectColumns AT ROW 13.29 COL 50 WIDGET-ID 10
    rd-dest AT ROW 15.48 COL 4.4 NO-LABELS
    lv-font-no AT ROW 15.48 COL 42 COLON-ALIGNED
    lv-ornt AT ROW 15.48 COL 51 NO-LABELS
    lines-per-page AT ROW 15.48 COL 87 COLON-ALIGNED
    lv-font-name AT ROW 16.43 COL 29 COLON-ALIGNED NO-LABELS
    tb_excel AT ROW 17.62 COL 92 RIGHT-ALIGNED
    td-show-parm AT ROW 18.24 COL 28.4
    fi_file AT ROW 19.1 COL 26.4 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 19.19 COL 92.6 RIGHT-ALIGNED
    tbAutoClose AT ROW 20.48 COL 28.4 WIDGET-ID 64
    btn-ok AT ROW 21.43 COL 28.2
    btn-cancel AT ROW 21.43 COL 55
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 4
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 14.81 COL 4
    RECT-6 AT ROW 15.24 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 25.81
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
        TITLE              = "GL Transaction Report"
        HEIGHT             = 21.91
        WIDTH              = 94.8
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
    begin_accnt:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR BUTTON Btn_Add IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    Btn_Add:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR BUTTON Btn_Def IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    Btn_Def:HIDDEN IN FRAME FRAME-A = TRUE.

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
    end_accnt:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

/* SETTINGS FOR SELECTION-LIST sl_avail IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    sl_avail:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_selected IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    sl_selected:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    tb_acpay:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_adjust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_ap-purch:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_apckr:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_apmem:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_apvoidck:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_arinv:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_autodist:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_cashr:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_cashrvd:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_cdisb:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_crmem:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_fgpost:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_general:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_jcost:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_mcshrec:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_oeinv:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_rmpost:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_void_checks:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* GL Transaction Report */
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
ON WINDOW-CLOSE OF C-Win /* GL Transaction Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_accnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_accnt C-Win
ON LEAVE OF begin_accnt IN FRAME FRAME-A /* Beginning Acct# */
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
                    {custom/asifax.i &type= " "
                            &begin_cust= "begin_accnt"
                            &END_cust= "begin_accnt" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = " "
                             &begin_cust= "begin_accnt"
                             &END_cust= "begin_accnt"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = " "
                                  &begin_cust="begin_accnt"
                                  &END_cust="begin_accnt"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

                    END.
                END. 
            WHEN 6 THEN RUN OUTPUT-to-port.
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


&Scoped-define SELF-NAME end_accnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_accnt C-Win
ON LEAVE OF end_accnt IN FRAME FRAME-A /* Ending Acct# */
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


&Scoped-define SELF-NAME tb_acpay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_acpay C-Win
ON VALUE-CHANGED OF tb_acpay IN FRAME FRAME-A /* Accounts Payable */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_adjust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_adjust C-Win
ON VALUE-CHANGED OF tb_adjust IN FRAME FRAME-A /* FG Adjustments */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ap-purch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ap-purch C-Win
ON VALUE-CHANGED OF tb_ap-purch IN FRAME FRAME-A /* Accounts Payable Purchases */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_apckr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_apckr C-Win
ON VALUE-CHANGED OF tb_apckr IN FRAME FRAME-A /* Accounts Payable Check Register */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_apmem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_apmem C-Win
ON VALUE-CHANGED OF tb_apmem IN FRAME FRAME-A /* Accounts Payable Memo */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_apvoidck
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_apvoidck C-Win
ON VALUE-CHANGED OF tb_apvoidck IN FRAME FRAME-A /* Accounts Payable Void Check */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_arinv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_arinv C-Win
ON VALUE-CHANGED OF tb_arinv IN FRAME FRAME-A /* Accounts Receivable Invoice */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_autodist
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_autodist C-Win
ON VALUE-CHANGED OF tb_autodist IN FRAME FRAME-A /* Automatic Distributions */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cashr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cashr C-Win
ON VALUE-CHANGED OF tb_cashr IN FRAME FRAME-A /* Cash Receipts */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cdisb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cdisb C-Win
ON VALUE-CHANGED OF tb_cdisb IN FRAME FRAME-A /* Cash Disbursements */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_crmem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_crmem C-Win
ON VALUE-CHANGED OF tb_crmem IN FRAME FRAME-A /* Accounts Receivable Memo */
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME tb_fgpost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fgpost C-Win
ON VALUE-CHANGED OF tb_fgpost IN FRAME FRAME-A /* Finished Goods Posting */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_general
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_general C-Win
ON VALUE-CHANGED OF tb_general IN FRAME FRAME-A /* General Journal Entries */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_jcost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_jcost C-Win
ON VALUE-CHANGED OF tb_jcost IN FRAME FRAME-A /* Job Cost Posting Register */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_mcshrec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_mcshrec C-Win
ON VALUE-CHANGED OF tb_mcshrec IN FRAME FRAME-A /* Misc Cash Receipts */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_oeinv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_oeinv C-Win
ON VALUE-CHANGED OF tb_oeinv IN FRAME FRAME-A /* Order Entry Invoice */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rmpost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rmpost C-Win
ON VALUE-CHANGED OF tb_rmpost IN FRAME FRAME-A /* Raw Materials Posting */
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


&Scoped-define SELF-NAME tb_void_checks
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_void_checks C-Win
ON VALUE-CHANGED OF tb_void_checks IN FRAME FRAME-A /* Voided Checks */
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
        begin_date = DATE(MONTH(TODAY),1,YEAR(TODAY))
        end_date   = TODAY.
    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "GR3" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_date.
    END.
    cColumnInit = NO.
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
    DISPLAY begin_date end_date begin_accnt end_accnt tb_cashr tb_cashrvd 
        tb_general tb_apckr tb_mcshrec tb_arinv tb_apmem tb_cdisb tb_acpay 
        tb_crmem tb_ap-purch tb_apvoidck tb_adjust tb_oeinv tb_fgpost tb_jcost 
        tb_autodist tb_rmpost tb_void_checks rd-dest fi_file tb_OpenCSV 
        tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_date end_date begin_accnt end_accnt tb_cashr 
        tb_cashrvd tb_general tb_apckr tb_mcshrec tb_arinv tb_apmem tb_cdisb 
        tb_acpay tb_crmem tb_ap-purch tb_apvoidck tb_adjust tb_oeinv tb_fgpost 
        tb_jcost tb_autodist tb_rmpost tb_void_checks btn_SelectColumns 
        rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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
    /*    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /***************************************************************************\
    *****************************************************************************
    **  Program: ap/rep/pjgl.p
    **       By: Chris Heins
    ** Descript: CASH DISBURSEMENT / VOUCHER REGISTER BY GL ACCT
    **
    *****************************************************************************
    \***************************************************************************/

    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE lo_trandate    AS DATE      FORMAT "99/99/9999" NO-UNDO LABEL "From Date".
    DEFINE VARIABLE hi_trandate    AS DATE      FORMAT "99/99/9999" NO-UNDO LABEL "Thru Date".
    DEFINE VARIABLE DEBUG          AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE VARIABLE ws_disc        LIKE ap-payl.amt-disc COLUMN-LABEL "Discount" NO-UNDO.
    DEFINE VARIABLE ws_check-no    LIKE ap-chk.check-no NO-UNDO FORMAT ">>>>>>>"
        COLUMN-LABEL "Check#".
    DEFINE VARIABLE ws_order-no    LIKE oe-ord.ord-no NO-UNDO
        FORMAT ">>>>>>>".
    DEFINE VARIABLE ws_jrnl        LIKE glhist.jrnl COLUMN-LABEL "Journal" NO-UNDO.
    DEFINE VARIABLE GL_JRNL_LIST   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lo_actnum      LIKE account.actnum LABEL "From GL Acct#" NO-UNDO.
    DEFINE VARIABLE hi_actnum      LIKE account.actnum LABEL "Thru GL Acct#" NO-UNDO.

    DEFINE VARIABLE t-amt          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE t-disc         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE t-qty          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE t-msf          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE hdg_printed    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE g-amt          AS DECIMAL   NO-UNDO.
    /*DEF VAR excelheader AS CHAR NO-UNDO.*/
    DEFINE VARIABLE viCount        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-line         AS CHARACTER FORMAT "X(60)" NO-UNDO.
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


    FORM
        ws_jrnl
        ap-inv.vend-no    COLUMN-LABEL "Vendor"
        vend.NAME FORMAT "X(35)" /* gdm - 01210902 adjstd frm 54*/
        ap-inv.inv-date FORMAT "99/99/99" COLUMN-LABEL "Date" 
        ap-inv.inv-no FORMAT "X(6)" COLUMN-LABEL "Inv#"
        ws_check-no
        ws_order-no
        ap-invl.qty FORMAT "->>,>>>,>>9.9<"
        ap-invl.amt-msf
        ws_disc
        ap-invl.amt FORMAT "->>,>>>,>>9.99"
        WITH FRAME f-det WIDTH 158 DOWN STREAM-IO.


    ASSIGN
        str-tit2     = c-win:TITLE 
        {sys/inc/ctrtext.i str-tit2 112}

        lo_actnum    = begin_accnt
        hi_actnum    = end_accnt
        lo_trandate  = begin_date
        hi_trandate  = end_date
        gl_jrnl_list = (IF tb_cashr    THEN "CASHR,CRDIS," ELSE "") +
                (IF tb_general  THEN "GENERAL,"     ELSE "") +
                (IF tb_mcshrec  THEN "MCSHREC,"     ELSE "") +
                (IF tb_apmem    THEN "APMEM,"       ELSE "") +
                (IF tb_acpay    THEN "ACPAY,"       ELSE "") +
                (IF tb_ap-purch THEN "AP-PURCH,"    ELSE "") +
                (IF tb_apckr    THEN "APCKR,"       ELSE "") +
                (IF tb_arinv    THEN "ARINV,"       ELSE "") +
                (IF tb_cdisb    THEN "CDISB,"       ELSE "") +
                (IF tb_crmem    THEN "CRMEM,DBMEM," ELSE "") +
                (IF tb_apvoidck THEN "APVOIDCK,"    ELSE "") +
                (IF tb_oeinv    THEN "OEINV,"       ELSE "") +
                (IF tb_adjust   THEN "ADJUST,"      ELSE "") +
                (IF tb_jcost    THEN "JCOST,"       ELSE "") +
                (IF tb_fgpost   THEN "FGPOST,"      ELSE "") +
                (IF tb_rmpost   THEN "RMPOST,"      ELSE "") +
                (IF tb_autodist THEN "AUTODIST,"    ELSE "") +
                (IF tb_void_checks THEN "APVOIDCK,"    ELSE "") +
                (IF tb_cashrvd  THEN "CASHRVD,"     ELSE "").

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


        IF LOOKUP(ttRptSelected.TextList, "Quantity,Amt MSF,Discount,Amount") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.



    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.

    SESSION:SET-WAIT-STATE ("general").
    FOR EACH account NO-LOCK
        WHERE account.company = cocode
        AND account.actnum >= lo_actnum
        AND account.actnum <= hi_actnum:

        {custom/statusMsg.i " 'Processing Account#  '  + account.actnum "}
    
        ASSIGN
            hdg_printed = FALSE
            t-amt       = 0
            t-disc      = 0
            t-msf       = 0
            t-qty       = 0
            ws_disc     = 0
            ws_jrnl     = ''
            ws_check-no = 0
            ws_order-no = 0.
    

        FOR EACH glhist NO-LOCK
            WHERE glhist.company = cocode
            AND glhist.actnum = account.actnum
            AND glhist.tr-date >= lo_trandate
            AND glhist.tr-date <= hi_trandate
            AND CAN-DO(GL_JRNL_LIST, glhist.jrnl)        
            BY glhist.tr-date:
            IF NOT hdg_printed THEN
            DO:

                hdg_printed = TRUE.
            END.

    

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "jou"    THEN 
                        cVarValue = STRING(glhist.jrnl,"x(8)") .
                    WHEN "vend"   THEN 
                        cVarValue = "".
                    WHEN "name"   THEN 
                        cVarValue = STRING(glhist.tr-dscr,"x(35)").
                    WHEN "date"  THEN 
                        cVarValue = STRING(glhist.tr-date,"99/99/99") .
                    WHEN "inv"   THEN 
                        cVarValue = "" .
                    WHEN "chk"  THEN 
                        cVarValue = "" .
                    WHEN "ord"   THEN 
                        cVarValue = "" .
                    WHEN "qty"  THEN 
                        cVarValue = "" .

                    WHEN "msf"  THEN 
                        cVarValue = "" .
                    WHEN "dis"   THEN 
                        cVarValue = "" .
                    WHEN "amt"  THEN 
                        cVarValue = STRING(glhist.tr-amt,"->>,>>>,>>9.99") .
                    WHEN "account"  THEN 
                        cVarValue = STRING(account.actnum,"x(25)") .
                    WHEN "acc-desc"  THEN 
                        cVarValue = STRING(account.dscr,"x(30)") .
                    WHEN "curr-code"  THEN 
                        cVarValue = STRING(glhist.curr-code[1]) .
                    WHEN "createdby"  THEN 
                        cVarValue = STRING(glhist.createdBy,"x(10)") .
                    WHEN "createddate"  THEN 
                        cVarValue = IF glhist.createdDate NE ? THEN STRING(glhist.createdDate,"99/99/9999") ELSE "" .
                    WHEN "posted"  THEN 
                        cVarValue = STRING(glhist.posted) .
                    WHEN "run"  THEN 
                        cVarValue = STRING(glhist.tr-num,"9999999") .

                END CASE.
                
                IF cTmpField = "date" THEN cExcelVarValue = DYNAMIC-FUNCTION("sfFormat_Date",glhist.tr-date) .
                ELSE IF cTmpField = "createddate" THEN cExcelVarValue = IF glhist.createdDate NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",glhist.createdDate) ELSE "" .

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
                t-disc = t-disc + ws_disc
                t-amt  = t-amt + glhist.tr-amt.
        END.    

        /*
              ws_jrnl = "AP-DIS".
        */
        /* Commented out for duplicatation of AP-PURCH and ACPAY and non posted CDISB
        
              ws_jrnl = "CDISB".
            FOR EACH ap-disl NO-LOCK
                WHERE ap-disl.company = cocode
                AND ap-disl.actnum = account.actnum,
                EACH ap-dis NO-LOCK
                WHERE ap-dis.d-no = ap-disl.d-no
                AND ap-dis.check-date >= lo_trandate
                AND ap-dis.check-date <= hi_trandate
                BY ap-dis.check-date BY ap-dis.check-no:
              IF NOT hdg_printed THEN
              DO:
                PUT SKIP account.actnum ' - '
                  account.dscr
                  SKIP.
                hdg_printed = TRUE.
              END.
        
              FIND vend OF ap-dis NO-LOCK NO-ERROR.
              ws_disc = 0.
              ws_check-no = ap-dis.check-no.
              DISPLAY
                ws_jrnl
                ap-dis.vend-no    @ ap-inv.vend-no
                vend.name WHEN AVAILABLE vend
                ap-dis.check-date @ ap-inv.inv-date
                ws_check-no
                ws_order-no
                ap-disl.qty WHEN ap-disl.qty <> 0 @ ap-invl.qty
                ap-disl.amt @ ap-invl.amt
                WITH FRAME f-det.
              down 1 with frame f-det.
              ASSIGN t-disc = t-disc + ws_disc
                t-amt = t-amt + ap-disl.amt
                t-qty = t-qty + ap-disl.qty.
            END.    /* ap-disl */
        
        */


        IF CAN-DO(GL_JRNL_LIST, "AP-PURCH") THEN
        DO:
            ws_jrnl = "AP-PURCH".
            ws_check-no = 0.
            FOR EACH ap-invl NO-LOCK
                WHERE ap-invl.company = cocode
                AND ap-invl.actnum = account.actnum,
                EACH ap-inv NO-LOCK
                WHERE ap-inv.i-no = ap-invl.i-no
                AND ap-inv.inv-date >= lo_trandate
                AND ap-inv.inv-date <= hi_trandate
                BY ap-inv.inv-date BY ap-inv.inv-no:
                IF NOT hdg_printed THEN
                DO:
                    hdg_printed = TRUE.
                END.

                FIND vend OF ap-inv NO-LOCK NO-ERROR.
                ws_disc = ap-invl.amt * (ap-inv.disc-% / 100).
                FIND FIRST po-ordl 
                    WHERE po-ordl.company EQ ap-invl.company
                    AND po-ordl.po-no EQ ap-invl.po-no 
                    AND po-ordl.LINE = ap-invl.LINE - (ap-invl.po-no * 1000)
                    NO-LOCK NO-ERROR.
                IF AVAILABLE po-ordl THEN ws_order-no = po-ordl.ord-no.

                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "jou"    THEN 
                            cVarValue = STRING(ws_jrnl,"x(8)") .
                        WHEN "vend"   THEN 
                            cVarValue = STRING(ap-inv.vend-no,"x(8)").
                        WHEN "name"   THEN 
                            cVarValue = IF AVAILABLE vend THEN STRING(vend.name,"x(35)") ELSE "".
                        WHEN "date"  THEN 
                            cVarValue = STRING(ap-inv.inv-date,"99/99/99") .
                        WHEN "inv"   THEN 
                            cVarValue = IF lAPInvoiceLength THEN STRING(ap-inv.inv-no,"x(20)") ELSE STRING(ap-inv.inv-no,"x(12)") .
                        WHEN "chk"  THEN 
                            cVarValue = STRING(ws_check-no,">>>>>>>") .
                        WHEN "ord"   THEN 
                            cVarValue = STRING(ws_order-no,">>>>>>>>") .
                        WHEN "qty"  THEN 
                            cVarValue = IF ap-invl.qty <> 0 THEN STRING(ap-invl.qty,"->,>>>,>>9.99") ELSE "" .

                        WHEN "msf"  THEN 
                            cVarValue = IF ap-invl.amt-msf <> 0 THEN STRING(ap-invl.amt-msf,"->>,>>9.99") ELSE "" .
                        WHEN "dis"   THEN 
                            cVarValue = IF ws_disc <> 0 THEN STRING(ws_disc,"->>,>>9.99") ELSE "" .
                        WHEN "amt"  THEN 
                            cVarValue = STRING(ap-invl.amt,"->>,>>>,>>9.99") .
                        WHEN "account"  THEN 
                            cVarValue = STRING(account.actnum,"x(25)") .
                        WHEN "acc-desc"  THEN 
                            cVarValue = STRING(account.dscr,"x(30)") .
                        WHEN "curr-code"  THEN 
                            cVarValue = STRING(ap-inv.curr-code[1]) .
                        WHEN "createdby"  THEN 
                            cVarValue = "" .
                        WHEN "createddate"  THEN 
                            cVarValue = "" .
                        WHEN "posted"  THEN 
                            cVarValue = "" .
                        WHEN "run"  THEN 
                            cVarValue = "" .

                    END CASE.
                
                    IF cTmpField = "date" THEN cExcelVarValue = DYNAMIC-FUNCTION("sfFormat_Date",ap-inv.inv-date) .

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
                    t-disc = t-disc + ws_disc
                    t-amt  = t-amt + ap-invl.amt
                    t-qty  = t-qty + ap-invl.qty
                    t-msf  = t-msf + ap-invl.amt-msf.
            END.    /* ap-invl */
        END.

        IF NOT hdg_printed THEN
            NEXT.   /* inactive account */
   
        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "jou"    THEN 
                    cVarValue ="" .
                WHEN "vend"   THEN 
                    cVarValue ="".
                WHEN "name"   THEN 
                    cVarValue = "".
                WHEN "date"  THEN 
                    cVarValue = "" .
                WHEN "inv"   THEN 
                    cVarValue = "" .
                WHEN "chk"  THEN 
                    cVarValue = "" .
                WHEN "ord"   THEN 
                    cVarValue = "" .
                WHEN "qty"  THEN 
                    cVarValue =  STRING(t-qty,"->,>>>,>>9.99") .
                WHEN "msf"  THEN 
                    cVarValue =  STRING(t-msf,"->>,>>9.99") .
                WHEN "dis"   THEN 
                    cVarValue = STRING(t-disc,"->>,>>9.99")  .
                WHEN "amt"  THEN 
                    cVarValue = STRING(t-amt,"->>,>>>,>>9.99") .
                WHEN "account"  THEN 
                    cVarValue = "" .
                WHEN "acc-desc"  THEN 
                    cVarValue = "" .
                WHEN "curr-code" THEN 
                    cVarValue = "".
                WHEN "createdby"  THEN 
                    cVarValue = "" .
                WHEN "createddate"  THEN 
                    cVarValue = "" .
                WHEN "posted"  THEN 
                    cVarValue = "" .
                WHEN "run"  THEN 
                    cVarValue = "" .

            END CASE.

            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
        PUT str-line SKIP.
        PUT UNFORMATTED cDisplay SKIP(1).
            

        g-amt = g-amt + t-amt.
    END.  /* for each account */

    /*IF g-amt NE 0 THEN*/
    /*do with frame f-det:
      UNDERLINE ap-invl.amt.
      DOWN 3.
      DISPLAY g-amt @ ap-invl.amt.
     end.*/
    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "".

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:             
            WHEN "jou"    THEN 
                cVarValue ="" .
            WHEN "vend"   THEN 
                cVarValue ="".
            WHEN "name"   THEN 
                cVarValue = "".
            WHEN "date"  THEN 
                cVarValue = "" .
            WHEN "inv"   THEN 
                cVarValue = "" .
            WHEN "chk"  THEN 
                cVarValue = "" .
            WHEN "ord"   THEN 
                cVarValue = "" .
            WHEN "qty"  THEN 
                cVarValue =  "" .
            WHEN "msf"  THEN 
                cVarValue =  "" .
            WHEN "dis"   THEN 
                cVarValue = "" .
            WHEN "amt"  THEN 
                cVarValue = STRING(g-amt,"->>,>>>,>>9.99") .
            WHEN "account"  THEN 
                cVarValue = "" .
            WHEN "acc-desc"  THEN 
                cVarValue = "" .
            WHEN "curr-code" THEN 
                cVarValue = "".
            WHEN "createdby"  THEN 
                cVarValue = "" .
            WHEN "createddate"  THEN 
                cVarValue = "" .
            WHEN "posted"  THEN 
                cVarValue = "" .
            WHEN "run"  THEN 
                cVarValue = "" .

        END CASE.

        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.
    PUT SKIP(1) str-line SKIP.
    PUT UNFORMATTED cDisplay SKIP.
            

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE("").

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
            fi_file:SCREEN-VALUE = "c:\tmp\GLTransaction.csv".   
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

