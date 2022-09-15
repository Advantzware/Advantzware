&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-commcr.w

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

/*{sys/inc/custlistform.i ""AR15"" }*/

{sys/ref/CustList.i NEW}

/*DEF VAR v-per-rpt   AS   LOG FORMAT "PTD/YTD" INIT YES.
DEF VAR v-period    AS   INT INIT 1.*/
DEFINE VARIABLE v-sman   AS CHARACTER FORMAT "x(3)" EXTENT 2 INIT ["", "zzz"].
DEFINE VARIABLE v-date   AS DATE      EXTENT 4 INIT [01/01/01, TODAY].
DEFINE VARIABLE v-cust   AS CHARACTER EXTENT 2 INIT ["", "zzzzzzzz"].
DEFINE VARIABLE v-sumdet AS LOG       FORMAT "Summary/Detail" INIT YES.
DEFINE VARIABLE v-cost1  AS CHARACTER.

/*DEF VAR v-year      AS   INTEGER.*/

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD row-id    AS ROWID
    FIELD qty       LIKE ar-invl.qty FORMAT "->>>>>>>9"
    FIELD amt       LIKE ar-invl.amt FORMAT "->>>>>>>9.99"
    FIELD amtp      LIKE ar-invl.amt FORMAT "->>>>>>>9.99"
    FIELD amtd      LIKE ar-invl.amt FORMAT "->>>>>>>9.99"
    FIELD delta     LIKE ar-invl.amt FORMAT "->>>>>>>9.99"
    FIELD cost      LIKE ar-invl.t-cost FORMAT "->>>>>>>9.99"
    FIELD comm      LIKE sman.scomm         
    FIELD cash-date LIKE ar-inv.inv-date.

DEFINE VARIABLE v-print-fmt      AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form   AS LOGICAL.
DEFINE VARIABLE ls-fax-file      AS CHARACTER NO-UNDO.

DEFINE VARIABLE glCustListActive AS LOGICAL   NO-UNDO.

DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

ASSIGN 
    cTextListToSelect  = "Sman,Customer,Inv#,Inv Date,FG Item Code,Order#,Quantity," +
                            "Inv Amt,Date Inv Paid,Amt Paid,Delta,GrossProfit%,Comm Amt,Comm Pct,Basis,Total Cost"  
    cFieldListToSelect = "sman,cust-no,inv-no,inv-date,i-no,ord-no,qty," +
                            "amt,cash-date,amtp,delta,v-gp,v-camt,v-comm,basis,ttl-cst"

    cFieldLength       = "4,8,7,10,15,8,9," + "12,13,12,12,12,12,8,5,15"
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Sman,Customer,Inv#,Inv Date,FG Item Code,Order#,Quantity," +
                            "Inv Amt,Date Inv Paid,Amt Paid,Delta,GrossProfit%,Comm Amt,Comm Pct".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rcptbe_date rcptend_date ~
begin_date end_date begin_slsmn end_slsmn tb_cust-list btnCustList ~
begin_cust-no end_cust-no rd_inv tb_prep tb_detailed tb_calc sl_avail ~
sl_selected Btn_Def Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS rcptbe_date rcptend_date begin_date ~
end_date begin_slsmn end_slsmn tb_cust-list begin_cust-no end_cust-no ~
lbl_inv rd_inv tb_prep tb_detailed tb_calc sl_avail sl_selected rd-dest ~
fi_file tb_OpenCSV tbAutoClose 

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

DEFINE VARIABLE begin_cust-no  AS CHARACTER FORMAT "X(8)" 
    LABEL "From Customer#" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "From Invoice Date" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn    AS CHARACTER FORMAT "XXX" 
    LABEL "From Sales Rep#" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1.

DEFINE VARIABLE end_cust-no    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "To Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "To Invoice Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn      AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
    LABEL "To Sales Rep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\CommissionCash.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_inv        AS CHARACTER FORMAT "X(256)":U INITIAL "Show Invoices?" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

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

DEFINE VARIABLE rcptbe_date    AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "From Receipt Date" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE rcptend_date   AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "To Receipt Date" 
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
    SIZE 16.4 BY 4.71 NO-UNDO.

DEFINE VARIABLE rd_inv         AS CHARACTER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Paid", "Paid",
    "Unpaid", "Unpaid",
    "All", "All"
    SIZE 32 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.29.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 10.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 34 BY 5.67 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 34 BY 5.67 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_calc      AS LOGICAL   INITIAL NO 
    LABEL "Calc Commission When Delta is Zero?" 
    VIEW-AS TOGGLE-BOX
    SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 30.8 BY .95 NO-UNDO.

DEFINE VARIABLE tb_detailed  AS LOGICAL   INITIAL NO 
    LABEL "Detailed?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .95 NO-UNDO.

DEFINE VARIABLE tb_prep      AS LOGICAL   INITIAL NO 
    LABEL "Show Prep Charges?" 
    VIEW-AS TOGGLE-BOX
    SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16.2 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    rcptbe_date AT ROW 2.33 COL 25.6 COLON-ALIGNED HELP
    "Enter Beginning Date" WIDGET-ID 58
    rcptend_date AT ROW 2.33 COL 68.6 COLON-ALIGNED HELP
    "Enter Ending Date" WIDGET-ID 60
    begin_date AT ROW 3.38 COL 25.6 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 3.38 COL 68.6 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_slsmn AT ROW 4.43 COL 25.6 COLON-ALIGNED HELP
    "Enter Beginning Sales Rep Number"
    end_slsmn AT ROW 4.43 COL 68.6 COLON-ALIGNED HELP
    "Enter Ending Sales Rep Number"
    tb_cust-list AT ROW 5.62 COL 27.6 WIDGET-ID 6
    btnCustList AT ROW 5.71 COL 70.2 WIDGET-ID 8
    begin_cust-no AT ROW 6.71 COL 25.6 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust-no AT ROW 6.71 COL 68.6 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    lbl_inv AT ROW 8.05 COL 23 NO-LABELS
    rd_inv AT ROW 8.05 COL 40 NO-LABELS
    tb_prep AT ROW 9.19 COL 40
    tb_detailed AT ROW 9.24 COL 23
    tb_calc AT ROW 10.38 COL 23
    sl_avail AT ROW 12.48 COL 3 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 12.48 COL 59.6 NO-LABELS WIDGET-ID 28
    Btn_Def AT ROW 12.57 COL 40.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    Btn_Add AT ROW 13.67 COL 40.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 14.76 COL 40.2 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 15.86 COL 40.2 WIDGET-ID 40
    btn_down AT ROW 16.95 COL 40.2 WIDGET-ID 42
    lv-font-no AT ROW 18.86 COL 39 COLON-ALIGNED
    lv-ornt AT ROW 18.86 COL 48 NO-LABELS
    lines-per-page AT ROW 18.86 COL 87 COLON-ALIGNED
    rd-dest AT ROW 18.91 COL 4.6 NO-LABELS
    lv-font-name AT ROW 19.81 COL 29 COLON-ALIGNED NO-LABELS
    tb_excel AT ROW 21 COL 92 RIGHT-ALIGNED
    td-show-parm AT ROW 21.57 COL 28
    fi_file AT ROW 22.52 COL 26 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 22.62 COL 92.6 RIGHT-ALIGNED
    tbAutoClose AT ROW 23.86 COL 28 WIDGET-ID 64
    btn-ok AT ROW 24.81 COL 28
    btn-cancel AT ROW 24.81 COL 55
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 11.71 COL 60.6 WIDGET-ID 44
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 18.19 COL 4
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 4
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 11.71 COL 3 WIDGET-ID 38
    RECT-6 AT ROW 18.57 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 94.4 BY 25.29
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
        TITLE              = "Commission - Cash Receipt Report"
        HEIGHT             = 25.29
        WIDTH              = 94.4
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
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_inv IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
    lbl_inv:PRIVATE-DATA IN FRAME FRAME-A = "rd_inv".

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
    rcptbe_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rcptend_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_inv:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_calc:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_detailed:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_prep:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Commission - Cash Receipt Report */
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
ON WINDOW-CLOSE OF C-Win /* Commission - Cash Receipt Report */
    DO:
        DELETE PROCEDURE hdOutputProcs.
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
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
                    ELSE DO:
                        OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                    END.
                END. /* WHEN 3 THEN DO: */
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &type= "Customer "
                            &begin_cust= "begin_cust-no"
                            &END_cust= "begin_cust-no" 
                            &fax-subject=c-win:TITLE
                            &fax-body=c-win:TITLE
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = "Customer "
                             &begin_cust= "begin_cust-no"
                             &END_cust= "begin_cust-no"
                             &mail-subject=c-win:TITLE
                             &mail-body=c-win:TITLE
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Customer "
                                  &begin_cust="begin_cust-no"
                                  &END_cust="begin_cust-no"
                                  &mail-subject=c-win:TITLE
                                  &mail-body=c-win:TITLE
                                  &mail-file=list-name }

                    END.
                END.
            WHEN 6 THEN RUN OUTPUT-TO-PORT.
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


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
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
    RUN DisplaySelectionList. 
    ASSIGN
        begin_date = TODAY
        end_date   = TODAY.

    /*FIND FIRST period NO-LOCK
        WHERE period.company EQ gcompany
          AND period.pst     LE TODAY
          AND period.pend    GE TODAY
          AND period.pstat
        NO-ERROR.
  
    IF AVAIL period THEN
      ASSIGN
       begin_period = period.pnum
       v-year       = period.yr
       begin_date   = period.pst.
  
    ELSE
      ASSIGN
       begin_period = MONTH(TODAY)
       v-year       = YEAR(TODAY).*/


    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "AR15" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "AR15",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_date.

    /* FIND FIRST period
         WHERE period.company EQ gcompany
           AND period.pst     LE DATE(begin_date:SCREEN-VALUE)
           AND period.pend    GE DATE(end_date:SCREEN-VALUE)
           AND period.pstat
         NO-LOCK NO-ERROR.
     v-year = IF AVAIL period THEN period.yr ELSE YEAR(TODAY).*/
    END.

    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'AR15',
        INPUT NO,
        OUTPUT glCustListActive).

    {sys/inc/chblankcust.i ""AR15""}

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
            INPUT 'AR15',
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
        INPUT 'AR15').


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
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
  
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
    DISPLAY rcptbe_date rcptend_date begin_date end_date begin_slsmn end_slsmn 
        tb_cust-list begin_cust-no end_cust-no lbl_inv rd_inv tb_prep 
        tb_detailed tb_calc sl_avail sl_selected rd-dest fi_file tb_OpenCSV 
        tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 rcptbe_date rcptend_date begin_date end_date begin_slsmn 
        end_slsmn tb_cust-list btnCustList begin_cust-no end_cust-no rd_inv 
        tb_prep tb_detailed tb_calc sl_avail sl_selected Btn_Def Btn_Add 
        Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV tbAutoClose 
        btn-ok btn-cancel 
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
            ttRptSelected.TextList     = ENTRY(i,cTmpList)
            ttRptSelected.FieldList    = ttRptList.FieldList
            ttRptSelected.FieldLength  = int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
            ttRptSelected.DisplayOrder = i
            iColumnLength              = iColumnLength + ttRptSelected.FieldLength + 1.
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
    RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------ oe/rep/sa-comm.p 3/96 JLF */
    /* Commission Summary / Detail Report                                         */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-top3w.f}*/
    DEFINE VARIABLE str-tit4 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-tit5 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-line AS cha       FORM "x(300)" NO-UNDO.

    {sys/form/r-top5DL.f}

    DEFINE BUFFER b-ar-invl  FOR ar-invl.
    DEFINE BUFFER b-ar-cashl FOR ar-cashl.

    DEFINE VARIABLE v-frst         AS LOG       EXTENT 2.
    DEFINE VARIABLE p-sman         AS CHARACTER FORMAT "x(3)".
    DEFINE VARIABLE v-camt         LIKE ar-invl.amt.
    DEFINE VARIABLE v-prof         LIKE ar-invl.amt.
    DEFINE VARIABLE v-comm         AS DECIMAL   FORMAT ">>9.99".
    DEFINE VARIABLE v-gp           AS DECIMAL   FORMAT ">>9.99".
    DEFINE VARIABLE v-slsm         LIKE ar-invl.sman EXTENT 1.
    DEFINE VARIABLE v-slsc         LIKE ar-invl.s-comm EXTENT 1.
    DEFINE VARIABLE v-slsp         LIKE ar-invl.s-pct EXTENT 1.
    DEFINE VARIABLE v-inv-no       LIKE ar-invl.inv-no FORMAT ">>>>>>>9".
    DEFINE VARIABLE v-qty          AS DECIMAL.
    DEFINE VARIABLE v-amt          LIKE ar-invl.amt.
    DEFINE VARIABLE v-amtp         LIKE ar-invl.amt.
    DEFINE VARIABLE v-amtd         LIKE ar-invl.amt.
    DEFINE VARIABLE v-cost         LIKE ar-invl.t-cost.
    DEFINE VARIABLE v-cust-part    LIKE ar-invl.part-no NO-UNDO.
    DEFINE VARIABLE v-ord-no       LIKE ar-invl.ord-no.
    DEFINE VARIABLE v-job-no       LIKE job.job-no.
    DEFINE VARIABLE v-job-no2      LIKE job.job-no2.
    DEFINE VARIABLE v-i-no         LIKE ar-invl.i-no.
    DEFINE VARIABLE v-basis        LIKE sman.commbasis NO-UNDO.
    DEFINE VARIABLE ld-inv-pct     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ll-comp        AS LOG       NO-UNDO.
    DEFINE VARIABLE ld-date        AS DATE      NO-UNDO.
    DEFINE VARIABLE ld-slsp        AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE v-tot-samt     AS DECIMAL   FORMAT "->>>>>>>9.99" EXTENT 3.
    DEFINE VARIABLE v-tot-pamt     AS DECIMAL   FORMAT "->>>>>>>9.99" EXTENT 3.
    DEFINE VARIABLE v-tot-damt     AS DECIMAL   FORMAT "->>>>>>>9.99" EXTENT 3.
    DEFINE VARIABLE v-tot-camt     AS DECIMAL   FORMAT "->>>>>>>9.99" EXTENT 3.
    DEFINE VARIABLE v-tot-cost     AS DECIMAL   FORMAT "->>>>>>>9.99" EXTENT 3.

    DEFINE VARIABLE excelheader    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDisplay       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS CHARACTER NO-UNDO.
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE BUFFER bar-inv FOR ar-inv.
    DEFINE VARIABLE lSelected AS LOG INIT YES NO-UNDO.


    ASSIGN
        str-tit2  = TRIM(c-win:TITLE) + " (A-R-15)"
        {sys/inc/ctrtext.i str-tit2 112}

        /*v-per-rpt   = rd_ptd EQ "PTD"
        v-period    = begin_period*/
        v-date[1] = begin_date
        v-date[2] = end_date
        v-date[3] = rcptbe_date
        v-date[4] = rcptend_date
        v-sman[1] = begin_slsmn
        v-sman[2] = end_slsmn
        v-cust[1] = begin_cust-no
        v-cust[2] = end_cust-no 
        lSelected = tb_cust-list
        .

    SESSION:SET-WAIT-STATE ("general").

    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        ASSIGN 
            str-tit4    = str-tit4 + 
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength + 1 - LENGTH(ttRptSelected.TextList))
            str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + ",".        

        IF LOOKUP(ttRptSelected.TextList, "Inv Amt,Amt Paid,Delta,GrossProfit%,Comm Amt,Comm Pct") <> 0    THEN
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

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    /*IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).*/

    EMPTY TEMP-TABLE tt-report.

    {oerep/r-commrN.i}

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
    END.  

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-period-dates C-Win 
PROCEDURE show-period-dates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*DO WITH FRAME {&FRAME-NAME}:
  IF rd_ptd:SCREEN-VALUE eq "PTD" then do:
    FIND FIRST period
        WHERE period.company EQ cocode
          AND period.yr      EQ v-year
          AND period.pnum    EQ INT(begin_period:SCREEN-VALUE)
        NO-LOCK NO-ERROR.

    IF AVAIL period THEN
      ASSIGN
       v-year                  = period.yr
       begin_date:SCREEN-VALUE = STRING(period.pst)
       end_date:SCREEN-VALUE   = STRING(IF period.pend LT TODAY THEN period.pend ELSE TODAY).

    ELSE DO: 
      MESSAGE begin_period "is not a valid period. "
          VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
  END.
END.*/

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
            fi_file:SCREEN-VALUE = "c:\tmp\CommissionCash.csv".    
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

