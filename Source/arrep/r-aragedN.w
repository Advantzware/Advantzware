&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: arrep\r-araged.w

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

/*{sys/inc/custlistform.i ""AR5"" }*/

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL   NO-UNDO.

DEFINE VARIABLE lv-default-comp  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-count          AS INTEGER   NO-UNDO INIT 0.

FOR EACH usercomp WHERE usercomp.USER_id = USERID("nosweat") AND  usercomp.loc = "" NO-LOCK :
    v-count = v-count + 1 .
END.
FIND FIRST usercomp WHERE usercomp.USER_id = USERID("nosweat") AND
    usercomp.company_default NO-LOCK NO-ERROR.
ASSIGN     
    lv-default-comp = IF AVAILABLE usercomp THEN usercomp.company ELSE "001".


{ar/ar-agng2.i new}
{custom/formtext.i NEW}
DEFINE TEMP-TABLE w-sort 
    FIELD w-int AS INTEGER.
DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE tt-cust NO-UNDO 
    FIELD curr-code LIKE cust.curr-code
    FIELD sorter    LIKE cust.cust-no
    FIELD row-id    AS ROWID
    INDEX tt-cust curr-code sorter.

DEFINE TEMP-TABLE tt-inv NO-UNDO  
    FIELD sorter LIKE ar-inv.inv-no
    FIELD inv-no LIKE ar-inv.inv-no
    FIELD row-id AS ROWID
    INDEX tt-inv sorter inv-no.

DEFINE NEW SHARED VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE NEW SHARED VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cColumnInit        AS LOG       INIT YES NO-UNDO.
DEFINE NEW SHARED VARIABLE cSelectedList      AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE str-line           AS cha       FORM "x(300)" NO-UNDO.
DEFINE NEW SHARED VARIABLE str-tit4           AS cha       FORM "x(300)" NO-UNDO.
DEFINE NEW SHARED VARIABLE str-tit5           AS cha       FORM "x(300)" NO-UNDO.
DEFINE NEW SHARED VARIABLE str-tit6           AS cha       FORM "x(300)" NO-UNDO.
DEFINE NEW SHARED VARIABLE str-tit7           AS cha       FORM "x(300)" NO-UNDO.
DEFINE NEW SHARED VARIABLE cstrtit            AS CHARACTER NO-UNDO .
DEFINE NEW SHARED VARIABLE cstrtit2           AS CHARACTER NO-UNDO .
DEFINE NEW SHARED VARIABLE iline              AS INTEGER   NO-UNDO .
DEFINE            VARIABLE cFileName          AS CHARACTER NO-UNDO.

ASSIGN 
    cTextListToSelect  = "CUSTOMER,CUST NAME,CONTACT,SALES REP,TERMS,ADDRESS1,ADDRESS2,CITY,STATE,ZIP,CREDIT LIM,PHONE,FAX,CHECK/MEMO," +
                           "DAYS OLD,TYPE,INV#,INV DATE,AMOUNT,CURRENT,ADTP,TD,"
                         + "PERIOD DAY1,PERIOD DAY2,PERIOD DAY3,CUSTOMER PO#,JOB#,BOL#,INVOICE NOTE,COLLECTION NOTE,PERIOD DAY4,"
                         + "CURRENCY,TOTAL DUE,AR CLASS"

    cFieldListToSelect = "cust,cust-name,cont,sman,term,add1,add2,city,stat,zip,cre-lim,phone,fax,chk-memo," +
                            "day-old,type,inv,inv-date,amount,current,adtp,td," +
                            "per-1,per-2,per-3,cust-po,job,bol,inv-note,coll-note,per-4," +
                            "currency,tot-due,arclass"    

    cFieldLength       = "8,30,25,25,15,25,25,10,5,10,14,13,12,12," + "8,4,8,8,15,15,4,4," + "15,15,15,15,9,8,30,30,14," + "10,13,8" 
    cFieldType         = "c,c,c,c,c,c,c,c,c,c,c,c,c,c," + "i,c,i,c,i,i,i,i," + "i,i,i,c,c,c,c,c,i," + "c,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "CUSTOMER,CUST NAME,CONTACT,SALES REP,TERMS,ADDRESS1,CITY,STATE,ZIP,CHECK/MEMO," +
                           "DAYS OLD,TYPE,INV#,INV DATE,AMOUNT,CURRENT,ADTP,TD,"
                         + "PERIOD DAY1,PERIOD DAY2,PERIOD DAY3,CUSTOMER PO#,JOB#"
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_comp end_comp ~
btnCustList tb_cust-list begin_cust-no end_cust-no begin_slsmn end_slsmn ~
begin_curr end_curr begin_term end_term begin_arclass end_arclass ~
trend_days as-of-date period-days-1 period-days-2 period-days-3 ~
period-days-4 rs_detail rd_sort rd_sort2 tb_paid tb_include-factored ~
tb_fuel tb_separate-fc begin_inv-date end_inv-date tgInactiveCust ~
btn_SelectColumns rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_comp end_comp tb_cust-list ~
begin_cust-no end_cust-no begin_slsmn end_slsmn begin_curr end_curr ~
begin_term end_term begin_arclass end_arclass trend_days as-of-date ~
period-days-1 period-days-2 period-days-3 period-days-4 rs_detail lbl_sort ~
rd_sort lbl_sort2 rd_sort2 tb_paid tb_include-factored tb_fuel ~
tb_separate-fc begin_inv-date end_inv-date tgInactiveCust rd-dest fi_file ~
tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
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

DEFINE BUTTON btn_SelectColumns 
    LABEL "Select Columns" 
    SIZE 47 BY 1.52.

DEFINE VARIABLE as-of-date     AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/01 
    LABEL "As of" 
    VIEW-AS FILL-IN 
    SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE begin_arclass  AS INTEGER   FORMAT ">>":U INITIAL 0 
    LABEL "Beginning AR Class" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_comp     AS CHARACTER FORMAT "X(3)":U 
    LABEL "Beginning Company#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_curr     AS CHARACTER FORMAT "X(3)":U 
    LABEL "Beginning Currency" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust-no  AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_inv-date AS DATE      FORMAT "99/99/9999" INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1.

DEFINE VARIABLE begin_slsmn    AS CHARACTER FORMAT "XXX" 
    LABEL "Beginning SalesRep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_term     AS CHARACTER FORMAT "X(5)":U 
    LABEL "Beginning Terms" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_arclass    AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Ending AR Class" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_comp       AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" 
    LABEL "Ending Company#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_curr       AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" 
    LABEL "Ending Currency" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_inv-date   AS DATE      FORMAT "99/99/9999" INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1.

DEFINE VARIABLE end_slsmn      AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
    LABEL "Ending SalesRep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_term       AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
    LABEL "Ending Terms" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\ARAgedReceivables.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Sorted By?" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort2      AS CHARACTER FORMAT "X(256)":U INITIAL "Aged By?" 
    VIEW-AS FILL-IN 
    SIZE 10.8 BY 1 NO-UNDO.

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

DEFINE VARIABLE period-days-1  AS INTEGER   FORMAT ">,>>>":U INITIAL 9999 
    LABEL "Period Days 1" 
    VIEW-AS FILL-IN 
    SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE period-days-2  AS INTEGER   FORMAT ">,>>>":U INITIAL 9999 
    LABEL "2" 
    VIEW-AS FILL-IN 
    SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE period-days-3  AS INTEGER   FORMAT ">,>>>":U INITIAL 9999 
    LABEL "3" 
    VIEW-AS FILL-IN 
    SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE period-days-4  AS INTEGER   FORMAT ">,>>>":U INITIAL 9999 
    LABEL "4" 
    VIEW-AS FILL-IN 
    SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE trend_days     AS INTEGER   FORMAT ">>9":U INITIAL 0 
    LABEL "Days for Recent Trend" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

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
    SIZE 17 BY 4.33 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Name" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Customer#", "#Number",
    "Name", "Name",
    "SalesRep#", "SalesRep#",
    "Invoice#", "Invoice#",
    "AR Class", "ArClass"
    SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort2       AS CHARACTER INITIAL "InvDate" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Due Date", "DueDate",
    "Invoice Date", "InvDate"
    SIZE 32 BY .86 NO-UNDO.

DEFINE VARIABLE rs_detail      AS INTEGER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Detail", 1,
    "Summary", 2,
    "Totals Only", 3
    SIZE 42 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 4.81.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 18.33.

DEFINE VARIABLE sl_avail            AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected         AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tbAutoClose         AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cust-list        AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 31.8 BY .62 NO-UNDO.

DEFINE VARIABLE tb_excel            AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .95 NO-UNDO.

DEFINE VARIABLE tb_fuel             AS LOGICAL   INITIAL YES 
    LABEL "Include Fuel Surcharges?" 
    VIEW-AS TOGGLE-BOX
    SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tb_include-factored AS LOGICAL   INITIAL NO 
    LABEL "Include Factored FG Items?" 
    VIEW-AS TOGGLE-BOX
    SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE tb_paid             AS LOGICAL   INITIAL NO 
    LABEL "Include Paid Invoices?" 
    VIEW-AS TOGGLE-BOX
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV          AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY .95 NO-UNDO.

DEFINE VARIABLE tb_separate-fc      AS LOGICAL   INITIAL NO 
    LABEL "Separate Finance Charges?" 
    VIEW-AS TOGGLE-BOX
    SIZE 30.4 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm        AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tgInactiveCust      AS LOGICAL   INITIAL NO 
    LABEL "Inactive Customers?" 
    VIEW-AS TOGGLE-BOX
    SIZE 26 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_comp AT ROW 1.86 COL 27 COLON-ALIGNED
    end_comp AT ROW 1.86 COL 70 COLON-ALIGNED
    btnCustList AT ROW 3 COL 61.6 WIDGET-ID 22
    tb_cust-list AT ROW 3.1 COL 29.2 WIDGET-ID 24
    begin_cust-no AT ROW 3.81 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust-no AT ROW 3.81 COL 70 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    sl_selected AT ROW 4.57 COL 7 NO-LABELS WIDGET-ID 28
    begin_slsmn AT ROW 4.76 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Sales Rep Number"
    end_slsmn AT ROW 4.76 COL 70 COLON-ALIGNED HELP
    "Enter Ending Sales Rep Number"
    begin_curr AT ROW 5.71 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Currency Code"
    end_curr AT ROW 5.71 COL 70 COLON-ALIGNED HELP
    "Enter Ending Currency Code"
    begin_term AT ROW 6.67 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Terms Code" WIDGET-ID 30
    end_term AT ROW 6.67 COL 70 COLON-ALIGNED HELP
    "Enter Ending Terms Code" WIDGET-ID 32
    begin_arclass AT ROW 7.62 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Terms Code" WIDGET-ID 36
    end_arclass AT ROW 7.62 COL 70 COLON-ALIGNED HELP
    "Enter Ending Terms Code" WIDGET-ID 38
    trend_days AT ROW 8.57 COL 70 COLON-ALIGNED WIDGET-ID 14
    as-of-date AT ROW 8.76 COL 27 COLON-ALIGNED
    period-days-1 AT ROW 9.95 COL 27 COLON-ALIGNED
    period-days-2 AT ROW 9.95 COL 39 COLON-ALIGNED
    period-days-3 AT ROW 9.95 COL 51 COLON-ALIGNED
    period-days-4 AT ROW 9.95 COL 63 COLON-ALIGNED WIDGET-ID 34
    rs_detail AT ROW 10.91 COL 29 NO-LABELS WIDGET-ID 6
    lbl_sort AT ROW 11.91 COL 14 COLON-ALIGNED NO-LABELS
    rd_sort AT ROW 11.91 COL 29 NO-LABELS
    sl_avail AT ROW 12.43 COL 6 NO-LABELS WIDGET-ID 26
    lbl_sort2 AT ROW 12.95 COL 15.2 COLON-ALIGNED NO-LABELS
    rd_sort2 AT ROW 12.95 COL 29 NO-LABELS
    tb_paid AT ROW 13.81 COL 29
    tb_include-factored AT ROW 13.81 COL 59.6
    tb_fuel AT ROW 14.76 COL 29
    tb_separate-fc AT ROW 14.76 COL 59.6
    begin_inv-date AT ROW 15.76 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Invoice Date"
    end_inv-date AT ROW 15.76 COL 70 COLON-ALIGNED HELP
    "Enter Ending Invoice Date"
    tgInactiveCust AT ROW 17 COL 29 WIDGET-ID 20
    btn_SelectColumns AT ROW 18.1 COL 29 WIDGET-ID 10
    rd-dest AT ROW 20.71 COL 4.6 NO-LABELS
    lv-font-no AT ROW 20.76 COL 38 COLON-ALIGNED
    lv-ornt AT ROW 20.76 COL 48 NO-LABELS
    lines-per-page AT ROW 20.76 COL 87 COLON-ALIGNED
    lv-font-name AT ROW 21.95 COL 29 COLON-ALIGNED NO-LABELS
    tb_excel AT ROW 23 COL 92 RIGHT-ALIGNED
    td-show-parm AT ROW 23.14 COL 28.8
    fi_file AT ROW 24.05 COL 26.8 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 24.05 COL 92.2 RIGHT-ALIGNED
    tbAutoClose AT ROW 25.29 COL 28.8 WIDGET-ID 64
    btn-ok AT ROW 26.24 COL 28.6
    btn-cancel AT ROW 26.24 COL 55
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 4
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 20.05 COL 4
    RECT-6 AT ROW 20.48 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 98.2 BY 26.81
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
        TITLE              = "AR Aged Receivables"
        HEIGHT             = 26.81
        WIDTH              = 98.2
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
    as-of-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_arclass:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_comp:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_curr:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_inv-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_term:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_arclass:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_comp:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_curr:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_inv-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_term:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "pram".

/* SETTINGS FOR FILL-IN lbl_sort2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort2:PRIVATE-DATA IN FRAME FRAME-A = "pram".

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
    period-days-1:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    period-days-2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    period-days-3:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    period-days-4:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm,Sort 1".

ASSIGN 
    rd_sort2:PRIVATE-DATA IN FRAME FRAME-A = "parm,Sort 2".

ASSIGN 
    rs_detail:PRIVATE-DATA IN FRAME FRAME-A = "parm,Report Type".

/* SETTINGS FOR SELECTION-LIST sl_avail IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    sl_avail:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_selected IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    sl_selected:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_fuel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_include-factored:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_paid:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_separate-fc:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    tgInactiveCust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    trend_days:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* AR Aged Receivables */
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
ON WINDOW-CLOSE OF C-Win /* AR Aged Receivables */
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


&Scoped-define SELF-NAME begin_arclass
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_arclass C-Win
ON LEAVE OF begin_arclass IN FRAME FRAME-A /* Beginning AR Class */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_comp C-Win
ON LEAVE OF begin_comp IN FRAME FRAME-A /* Beginning Company# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_curr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_curr C-Win
ON LEAVE OF begin_curr IN FRAME FRAME-A /* Beginning Currency */
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


&Scoped-define SELF-NAME begin_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv-date C-Win
ON LEAVE OF begin_inv-date IN FRAME FRAME-A /* Beginning Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning SalesRep# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_term
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_term C-Win
ON HELP OF begin_term IN FRAME FRAME-A /* Beginning Terms */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN windows/l-terms.w 
            (gcompany,{&SELF-NAME}:SCREEN-VALUE IN FRAME {&frame-name}, OUTPUT char-val).
        IF char-val <> "" THEN 
            ASSIGN {&SELF-NAME}:SCREEN-VALUE IN FRAME {&frame-name} = ENTRY(1,char-val) .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_term C-Win
ON LEAVE OF begin_term IN FRAME FRAME-A /* Beginning Terms */
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

        /*{&WINDOW-NAME}:WINDOW-STATE = WINDOW-minIMIZE.*/

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
  
        FIND FIRST  ttCustList NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttCustList AND tb_cust-list THEN 
        DO:
            EMPTY TEMP-TABLE ttCustList.
            RUN BuildCustList(INPUT cocode,
                INPUT tb_cust-list AND glCustListActive,
                INPUT begin_cust-no,
                INPUT END_cust-no).
        END.
        RUN GetSelectionList.
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
                    {custom/asifax.i &type= 'begin_cust=begin_cust-slsmn'
                            &begin_cust= "begin_slsmn"
                            &end_cust= "begin_slsmn" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail2.i &TYPE = "Customer"
                             &group-title='r-araged.' /* v-prgmname */
                             &begin_cust= "begin_cust-no"
                             &end_cust= "begin_cust-no" 
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr2.i &TYPE = "Customer"
                                  &group-title='r-araged.' /* v-prgmname */
                                  &begin_cust= "begin_cust-no"
                                  &end_cust= "begin_cust-no" 
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

                    END.
                END. 
            WHEN 6 THEN RUN OUTPUT-to-port.
        END CASE.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
        SESSION:SET-WAIT-STATE ("").
        CURRENT-WINDOW:WINDOW-STATE  = WINDOW-NORMAL.

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


&Scoped-define SELF-NAME end_arclass
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_arclass C-Win
ON LEAVE OF end_arclass IN FRAME FRAME-A /* Ending AR Class */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_curr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_curr C-Win
ON LEAVE OF end_curr IN FRAME FRAME-A /* Ending Currency */
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


&Scoped-define SELF-NAME end_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv-date C-Win
ON LEAVE OF end_inv-date IN FRAME FRAME-A /* Ending Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending SalesRep# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_term
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_term C-Win
ON HELP OF end_term IN FRAME FRAME-A /* Ending Terms */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN windows/l-terms.w 
            (gcompany,{&SELF-NAME}:SCREEN-VALUE IN FRAME {&frame-name}, OUTPUT char-val).
        IF char-val <> "" THEN 
            ASSIGN {&SELF-NAME}:SCREEN-VALUE IN FRAME {&frame-name} = ENTRY(1,char-val) .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_term C-Win
ON LEAVE OF end_term IN FRAME FRAME-A /* Ending Terms */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* Name */
    DO:
        DEFINE VARIABLE ls-filename AS cha NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG NO-UNDO.

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

        RUN WINDOWS/l-fonts.w ({&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
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


&Scoped-define SELF-NAME period-days-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period-days-1 C-Win
ON LEAVE OF period-days-1 IN FRAME FRAME-A /* Period Days 1 */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME period-days-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period-days-2 C-Win
ON LEAVE OF period-days-2 IN FRAME FRAME-A /* 2 */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME period-days-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period-days-3 C-Win
ON LEAVE OF period-days-3 IN FRAME FRAME-A /* 3 */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME period-days-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period-days-4 C-Win
ON LEAVE OF period-days-4 IN FRAME FRAME-A /* 4 */
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME rd_sort2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort2 C-Win
ON VALUE-CHANGED OF rd_sort2 IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_include-factored
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_include-factored C-Win
ON VALUE-CHANGED OF tb_include-factored IN FRAME FRAME-A /* Include Factored FG Items? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_paid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_paid C-Win
ON VALUE-CHANGED OF tb_paid IN FRAME FRAME-A /* Include Paid Invoices? */
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


&Scoped-define SELF-NAME trend_days
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL trend_days C-Win
ON LEAVE OF trend_days IN FRAME FRAME-A /* Days for Recent Trend */
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
        as-of-date    = TODAY
        period-days-1 = 30
        period-days-2 = 60
        period-days-3 = 90
        end_inv-date  = TODAY
        fi_file       = "c:\tmp\ar-aging.csv" 
        begin_comp    = lv-default-comp
        end_comp      = lv-default-comp
        .
    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").    
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "AR5" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "AR5",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        as-of-date:SCREEN-VALUE = STRING(TODAY).
        lbl_sort:SCREEN-VALUE =  "Sorted By?:" .
        lbl_sort2:SCREEN-VALUE =  "Aged By?:" .
        IF v-count LE 1 THEN
            ASSIGN
                begin_comp:SENSITIVE    = NO
                end_comp:SENSITIVE      = NO 
                begin_comp:SCREEN-VALUE = lv-default-comp
                begin_comp              = lv-default-comp
                end_comp:SCREEN-VALUE   = lv-default-comp
                end_comp                = lv-default-comp.
        APPLY "entry" TO begin_cust-no.
    END.
  
    {sys/inc/chblankcust.i ""AR5""}
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
    ASSIGN 
        cColumnInit = NO .
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
            INPUT 'AR5',
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
        INPUT 'AR5').


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
    DISPLAY begin_comp end_comp tb_cust-list begin_cust-no end_cust-no begin_slsmn 
        end_slsmn begin_curr end_curr begin_term end_term begin_arclass 
        end_arclass trend_days as-of-date period-days-1 period-days-2 
        period-days-3 period-days-4 rs_detail lbl_sort rd_sort lbl_sort2 
        rd_sort2 tb_paid tb_include-factored tb_fuel tb_separate-fc 
        begin_inv-date end_inv-date tgInactiveCust rd-dest fi_file tb_OpenCSV 
        tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_comp end_comp btnCustList tb_cust-list 
        begin_cust-no end_cust-no begin_slsmn end_slsmn begin_curr end_curr 
        begin_term end_term begin_arclass end_arclass trend_days as-of-date 
        period-days-1 period-days-2 period-days-3 period-days-4 rs_detail 
        rd_sort rd_sort2 tb_paid tb_include-factored tb_fuel tb_separate-fc 
        begin_inv-date end_inv-date tgInactiveCust btn_SelectColumns rd-dest 
        fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Output-to-File C-Win 
PROCEDURE Output-to-File :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*  DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-cust-add C-Win 
PROCEDURE print-cust-add :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DISPLAY cust.addr[1]                                                SKIP
        cust.addr[2]                                                SKIP
        TRIM(cust.city) + ", " +
        trim(cust.state) + "  " + trim(cust.zip) FORMAT "x(50)"

        WITH NO-LABELS NO-BOX FRAME cust-detail WIDTH 132.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* --------------------------------------------------- ar/ar-aging.p  9/94 RM */
    /* A/R Aged Receivables Report Program - A/R Module                           */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-top3w.f}*/

    DEFINE VARIABLE li             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-hdr          AS CHARACTER INIT "Customer,Name,Contact,SalesRep,Terms,Address1,Address2,City,State,Zip,Credit Limit,Phone,Fax,Check/Memo,DaysOld,Type,Invoice#,InvoiceDate,InvoiceAmt,Current,ADTP,TD," NO-UNDO.  /*Task# 11151304*/
    DEFINE VARIABLE v-hdr2         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-rpt-type     AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDisplay       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName     AS CHARACTER NO-UNDO.

    {sys/form/r-top5L3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.


    IF rs_detail = 1 THEN 
        v-rpt-type = "DETAIL".
    ELSE IF rs_detail = 2  THEN
            v-rpt-type = "SUMMARY".
        ELSE IF rs_detail = 3 THEN
                v-rpt-type = "TOTALS ONLY".

    /*IF tb_address = YES THEN
       v-hdr2 = "Customer,Name,Address1,Address2,City,State,Zip,Phone,Fax,Amount,Current,ADTP,TD,".      /*Task# 11151304*/
    ELSE
       v-hdr2 = "Customer,Name,Phone,Amount,Current,ADTP,TD,".*/

    ASSIGN
        str-tit2           = c-win:TITLE + " - " + v-rpt-type
        {sys/inc/ctrtext.i str-tit2 112}

        v-s-cust           = begin_cust-no
        v-e-cust           = end_cust-no
        v-s-sman           = begin_slsmn
        v-e-sman           = end_slsmn
        v-s-curr           = begin_curr
        v-e-curr           = end_curr
        v-date             = as-of-date
        v-trend-days       = trend_days
        v-days[1]          = period-days-1
        v-days[2]          = period-days-2
        v-days[3]          = period-days-3
        v-days[4]          = period-days-4
        det-rpt            = rs_detail
        v-sort             = rd_sort
        v-sort2            = rd_sort2
        v-inc              = tb_paid
        v-s-dat            = begin_inv-date
        v-e-dat            = end_inv-date
        /*v-days-old         = tb_days-old
        v-prt-add          = tb_address   */
        v-exp-name         = cFileName
        v-include-factored = tb_include-factored
        v-export           = tb_excel
        v-include-fuel     = tb_fuel
        v-sep-fc           = tb_separate-fc
        b-comp             = begin_comp
        e-comp             = end_comp
        /*v-print-job        = tb_job#*/
        v-inactive-custs   = tgInactiveCust
        /*v-print-cust-po    = tb_cust-po*/
        v-s-terms          = begin_term
        v-e-terms          = end_term
        sPrtInvNote        = NO
        sPrtCollectionNote = NO
        str-line           = "" 
        lSelected          = tb_cust-list

        str-tit3           = "Company From: " + STRING(begin_comp) + " To: " + STRING(end_comp) +  "    As of Date: " + STRING(v-date) 
        {sys/inc/ctrtext.i str-tit3 132}.
    str-tit4 = "Sorted By: " + STRING(rd_sort) + "     "   +  "Aged By : " + STRING(rd_sort2) .
    {sys/inc/ctrtext.i str-tit4 132}.
    cstrtit = str-tit2 .
    cstrtit2 = str-tit3 .
    iline    = lines-per-page .
    str-tit6 = "" .
    str-tit7 = "" . 
    v-s-class = begin_arclass.
    v-e-class = end_arclass.
    SESSION:SET-WAIT-STATE ("general").

    DO WITH FRAME {&frame-name}:
        FOR EACH w-sort:
            DELETE w-sort.
        END.
        DO li = 1 TO 4:
            CREATE w-sort.
            w-int = v-days[li].
        END.
        li = 0.
        FOR EACH w-sort BY w-int:
            li = li + 1.
            v-days[li] = w-int.
            IF i GT 3 THEN LEAVE.
        END.
        ASSIGN
            period-days-1:screen-value = STRING(v-days[1])
            period-days-1
            period-days-2:screen-value = STRING(v-days[2])
            period-days-2
            period-days-3:screen-value = STRING(v-days[3])
            period-days-3
            period-days-4:screen-value = STRING(v-days[4])
            period-days-4.
    END.
    IF lselected THEN 
    DO:
        FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
        IF AVAILABLE ttCustList THEN ASSIGN v-s-cust = ttCustList.cust-no .
        FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
        IF AVAILABLE ttCustList THEN ASSIGN v-e-cust = ttCustList.cust-no .
    END.

    DEFINE VARIABLE cslist AS CHARACTER NO-UNDO.
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF LOOKUP(ttRptSelected.TextList, "INVOICE NOTE,COLLECTION NOTE") <> 0    THEN 
        DO:
            IF ttRptSelected.TextList = "INVOICE NOTE"  THEN
                sPrtInvNote = YES.
            IF ttRptSelected.TextList = "COLLECTION NOTE"  THEN
                sPrtCollectionNote = YES.
            NEXT.
        END.

        IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
            THEN ASSIGN str-tit6    = str-tit6 + ttRptSelected.TextList + " "
                str-tit7    = str-tit7 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + "," .        
        ELSE 
        DO: 
            IF ttRptSelected.TextList =  "PERIOD DAY1" THEN
                ASSIGN str-tit6    = str-tit6 + 
           (IF ttRptSelected.HeadingFromLeft THEN
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
               ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + "  " + string( "(" + string(v-days[1]) + "-" + STRING(v-days[2] - 1) + ")","x(9)")) + " "
                    str-tit7    = str-tit7 + FILL("-",ttRptSelected.FieldLength) + " "
                    excelheader = excelHeader + string(v-days[1]) + "," .       

            ELSE IF ttRptSelected.TextList =  "PERIOD DAY2" THEN
                    ASSIGN str-tit6    = str-tit6 + 
           (IF ttRptSelected.HeadingFromLeft THEN
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
               ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + "  " + string( "(" + string(v-days[2]) + "-" + STRING(v-days[3] - 1) + ")","x(9)" )) + " "
                        str-tit7    = str-tit7 + FILL("-",ttRptSelected.FieldLength) + " "
                        excelheader = excelHeader + string(v-days[2]) + "," .
                ELSE IF ttRptSelected.TextList =  "PERIOD DAY3" THEN
                        ASSIGN str-tit6    = str-tit6 + 
            (IF ttRptSelected.HeadingFromLeft THEN
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
               ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + "  " + string( "(" + string(v-days[3]) + "-" + STRING(v-days[4] - 1) + ")","x(9)" )) + " "
                            str-tit7    = str-tit7 + FILL("-",ttRptSelected.FieldLength) + " "
                            excelheader = excelHeader + string(v-days[3]) + "," .
                    ELSE IF ttRptSelected.TextList =  "PERIOD DAY4" THEN
                            ASSIGN str-tit6    = str-tit6 + 
            (IF ttRptSelected.HeadingFromLeft THEN
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
               ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + "  " + string( "Over " + STRING(v-days[4]),"x(9)" )) + " "
                                str-tit7    = str-tit7 + FILL("-",ttRptSelected.FieldLength) + " "
                                excelheader = excelHeader + string(v-days[4]) + "," .
                        ELSE
                            ASSIGN str-tit6    = str-tit6 + 
           (IF ttRptSelected.HeadingFromLeft THEN
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
               ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                                str-tit7    = str-tit7 + FILL("-",ttRptSelected.FieldLength) + " "
                                excelheader = excelHeader + ttRptSelected.TextList + "," . 


        END.
        cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "AMOUNT,CURRENT,PERIOD DAY1,PERIOD DAY2,PERIOD DAY3,PERIOD DAY4,TOTAL DUE") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    /*VIEW FRAME r-top.*/

    ASSIGN 
        grand-t     = 0
        grand-t-pri = 0
        grand-t-fc  = 0.

    IF td-show-parm THEN RUN show-param.

    IF tb_excel THEN 
    DO:
        OUTPUT stream s-temp to value(v-exp-name).

        IF det-rpt = 1 OR det-rpt = 2 THEN 
        DO:
            v-hdr = v-hdr + trim(STRING(v-days[1],">,>>>")) + "," +
                trim(STRING(v-days[2],">,>>>")) + "," +
                trim(STRING(v-days[3],">,>>>")) + "," +
                trim(STRING(v-days[4],">,>>>")) + "+". 
      
            PUT STREAM s-temp UNFORMATTED excelheader SKIP.
    
        END.
    END.


    IF v-sort2 BEGINS "Due" THEN 
    DO:
        IF v-sort EQ "Name" THEN
            RUN ar/ar-agng1N.p.
        ELSE
            IF v-sort EQ "#Number" THEN
                RUN ar/ar-agng2N.p.
            ELSE IF v-sort EQ "SalesRep#" THEN
                    RUN ar/ar-agng7N.p.
                ELSE IF v-sort EQ "ArClass" THEN
                        RUN ar/ar-agng9N.p.  
                    ELSE
                        RUN ar/ar-agng5N.p. 
    END.

    ELSE 
    DO:
        IF v-sort2 BEGINS "InvD" THEN
            IF v-sort EQ "Name" THEN
                RUN ar/ar-agng3N.p.

            ELSE
                IF v-sort EQ "#Number" THEN
                    RUN ar/ar-agng4N.p.

                ELSE IF v-sort EQ "SalesRep#" THEN
                        RUN ar/ar-agng8N.p.
                    ELSE IF v-sort EQ "ArClass" THEN
                            RUN ar/ar-agng10N.p.   
                        ELSE
                            RUN ar/ar-agng6N.p. 
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM s-temp CLOSE.
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
    RUN sys/ref/getParms.p (INPUT lv-frame-hdl, 
        OUTPUT parm-fld-list, 
        OUTPUT parm-lbl-list).
    /*   lv-group-hdl = lv-frame-hdl:first-child.                                             */
    /*   lv-field-hdl = lv-group-hdl:first-child .                                            */
    /*                                                                                        */
    /*   do while true:                                                                       */
    /*      if not valid-handle(lv-field-hdl) then leave.                                     */
    /*      if lookup(lv-field-hdl:private-data,"parm") > 0                                   */
    /*         then do:                                                                       */
    /*            if lv-field-hdl:label <> ? then                                             */
    /*               assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","   */
    /*                      parm-lbl-list = parm-lbl-list + lv-field-hdl:label + ","          */
    /*                      .                                                                 */
    /*            else do:  /* radio set */                                                   */
    /*            MESSAGE lv-field-hdl:NAME lv-field-hdl:LABEL                                */
    /*              VIEW-AS ALERT-BOX INFO BUTTONS OK.                                        */
    /*               assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","   */
    /*                      .                                                                 */
    /*               lv-field2-hdl = lv-group-hdl:first-child.                                */
    /*               repeat:                                                                  */
    /*                   if not valid-handle(lv-field2-hdl) then leave.                       */
    /*                   if lv-field2-hdl:private-data = lv-field-hdl:name then do:           */
    /*                      parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",". */
    /*                   end.                                                                 */
    /*                   lv-field2-hdl = lv-field2-hdl:next-sibling.                          */
    /*               end.                                                                     */
    /*            end.                                                                        */
    /*         end.                                                                           */
    /*      lv-field-hdl = lv-field-hdl:next-sibling.                                         */
    /*   end.                                                                                 */

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

    PAGE.

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
            fi_file:SCREEN-VALUE = "c:\tmp\ARAgedReceivables.csv".    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

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

