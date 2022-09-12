&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-slmpef.w

  Description: Sales Rep Performance Report

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
DEFINE VARIABLE glCustListActive AS LOGICAL NO-UNDO.

DEFINE VARIABLE called           AS LOG     NO-UNDO.

called = YES.

DEFINE VARIABLE fdate      AS DATE      EXTENT 3 NO-UNDO.
/* [day] [period] [year] */
DEFINE VARIABLE edate      AS DATE      EXTENT 3 NO-UNDO.
DEFINE VARIABLE tdate      AS DATE      FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE fsman      AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE tsman      AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE v-inc-fc   AS LOG       INIT NO NO-UNDO.

DEFINE VARIABLE v-sman-no  AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE v-amt      LIKE ar-inv.gross NO-UNDO.
DEFINE VARIABLE v-sqft     LIKE itemfg.t-sqft FORMAT "->>,>>9.999" NO-UNDO.
DEFINE VARIABLE v-pmsf     AS DECIMAL   FORMAT "->>>>>9.99" EXTENT 3 NO-UNDO.

DEFINE VARIABLE v-tot-sqft LIKE v-sqft EXTENT 6.
DEFINE VARIABLE v-tot-amt  LIKE v-amt EXTENT 6.

DEFINE VARIABLE v-period   AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-year     AS INTEGER   FORMAT "9999" NO-UNDO.
DEFINE VARIABLE v-qty      LIKE ar-invl.ship-qty FORMAT "->>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-exclude  AS LOG       NO-UNDO.
DEFINE VARIABLE v-pct      AS DECIMAL   FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE v-misc     AS LOG       NO-UNDO.
DEFINE VARIABLE v-leave    AS LOG       NO-UNDO.
DEFINE VARIABLE v-fac      AS INTEGER.
DEFINE VARIABLE head       AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD row-id AS ROWID.

DEFINE BUFFER b-tt-report FOR tt-report.
DEFINE BUFFER b-ar-invl   FOR ar-invl.

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD w-type    AS CHARACTER
    FIELD w-sman-no LIKE v-sman-no
    FIELD w-sqft    LIKE v-sqft EXTENT 3 COLUMN-LABEL "Sq Ft/M"
    FIELD w-amt     LIKE v-amt EXTENT 3 COLUMN-LABEL "Amount"
    FIELD w-pmsf    LIKE v-pmsf COLUMN-LABEL "$/MSF"
    FIELD w-cust-no LIKE ar-inv.cust-no COLUMN-LABEL "Customer"  .

DEFINE TEMP-TABLE w-data1 NO-UNDO LIKE w-data.
DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.

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
DEFINE VARIABLE str-line       AS cha       FORM "x(350)" NO-UNDO.

DEFINE STREAM excel.

head = IF called THEN "SALES ANALYSIS - SALESREP PERFORMANCE tt-report"
ELSE "SALES ANALYSIS - SALESREP PRODUCTION tt-report".


FORM w-data1.w-sman-no   COLUMN-LABEL "No"
    sman.sname         COLUMN-LABEL "Name" FORMAT "x(17)"
    w-data1.w-sqft[1]
    w-data1.w-amt[1]  FORMAT "->>,>>>,>>9.99" 
    w-data1.w-pmsf[1]
    w-data1.w-sqft[2]
    w-data1.w-amt[2]  FORMAT "->>,>>>,>>9.99" 
    w-data1.w-pmsf[2]
    w-data1.w-sqft[3]      
    w-data1.w-amt[3]  FORMAT "->>,>>>,>>9.99" 
    w-data1.w-pmsf[3]
    HEADER SKIP(1)
    "    SalesRep"
    "---------------Daily----------------" AT 23
    "-----------Period to Date-----------"
    "------------Year to Date-------------"

    WITH NO-BOX FRAME f-prod DOWN STREAM-IO WIDTH 144.

FORM w-data1.w-sman-no COLUMN-LABEL "No"
    sman.sname COLUMN-LABEL "Name" FORMAT "x(20)"
    w-data1.w-sqft[1]
    w-data1.w-amt[1] FORMAT "->>,>>>,>>9.99" 
    SPACE(2)
    w-data1.w-sqft[2]
    w-data1.w-amt[2] FORMAT "->>,>>>,>>9.99" 
    SPACE(2)
    w-data1.w-sqft[3]
    w-data1.w-amt[3] FORMAT "->>,>>>,>>9.99" 
    HEADER SKIP(1)
    "    SalesRep"
    "----------Daily-----------" AT 26 SPACE(2)
    "------Period to Date------" SPACE(2)
    "-------Year to Date-------"

    WITH NO-BOX FRAME f-perf DOWN STREAM-IO WIDTH 144.


DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.

ASSIGN 
    cTextListToSelect  = "No,Sales Rep Name,Dly Sq Ft/M,Dly Amount,"
                         + "PTD Sq Ft/M,PTD Amount,YTD Sq Ft/M,YTD Amount,Cust#"
    cFieldListToSelect = "rep,name,dly-sf,dly-amt," +
                            "ptd-sf,ptd-amt,ytd-sf,ytd-amt,cust"
    cFieldLength       = "3,25,11,13," + "11,13,11,14,8" 
    cFieldType         = "c,c,i,i," + "i,i,i,i,c"
    .



{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "No,Sales Rep Name,Dly Sq Ft/M,Dly Amount,"
                         + "PTD Sq Ft/M,PTD Amount,YTD Sq Ft/M,YTD Amount"     .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 inv-date tb_cust-list ~
btnCustList begin_cust-no end_cust-no begin_slsmn end_slsmn tb_fin-chg ~
sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up btn_down rd-dest ~
fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS inv-date tb_cust-list begin_cust-no ~
end_cust-no begin_slsmn end_slsmn tb_fin-chg sl_avail sl_selected rd-dest ~
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

DEFINE VARIABLE begin_cust-no  AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_slsmn    AS CHARACTER FORMAT "XXX" 
    LABEL "Beginning Salesrep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_slsmn      AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
    LABEL "Ending Salesrep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\SalesRepPerformance.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 50 BY 1.

DEFINE VARIABLE inv-date       AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Invoice Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

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
    SIZE 15 BY 4.91 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.95.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 6.86.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.95 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.95 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_fin-chg   AS LOGICAL   INITIAL NO 
    LABEL "Include Finance Charges?" 
    VIEW-AS TOGGLE-BOX
    SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    inv-date AT ROW 1.91 COL 32 COLON-ALIGNED HELP
    "Enter Invoice Date"
    tb_cust-list AT ROW 3.14 COL 31.8 WIDGET-ID 6
    btnCustList AT ROW 3.19 COL 63.8 WIDGET-ID 8
    begin_cust-no AT ROW 4.1 COL 29.2 COLON-ALIGNED HELP
    "Enter Beginning Customer Number" WIDGET-ID 58
    end_cust-no AT ROW 4.1 COL 70.2 COLON-ALIGNED HELP
    "Enter Ending Customer Number" WIDGET-ID 60
    begin_slsmn AT ROW 5.05 COL 29.2 COLON-ALIGNED HELP
    "Enter Beginning Sales Rep Number"
    end_slsmn AT ROW 5.05 COL 70.2 COLON-ALIGNED HELP
    "Enter Ending Sales Rep Number"
    tb_fin-chg AT ROW 7.19 COL 35
    sl_avail AT ROW 9.43 COL 4 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 9.43 COL 60.6 NO-LABELS WIDGET-ID 28
    Btn_Def AT ROW 9.52 COL 40.6 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    Btn_Add AT ROW 10.67 COL 40.6 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 11.86 COL 40.6 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 13.05 COL 40.6 WIDGET-ID 40
    btn_down AT ROW 14.24 COL 40.6 WIDGET-ID 42
    lv-ornt AT ROW 16.24 COL 30 NO-LABELS
    lines-per-page AT ROW 16.24 COL 83 COLON-ALIGNED
    rd-dest AT ROW 16.81 COL 6 NO-LABELS
    lv-font-no AT ROW 16.95 COL 33 COLON-ALIGNED
    tb_excel AT ROW 16.95 COL 72 RIGHT-ALIGNED
    lv-font-name AT ROW 17.91 COL 27 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 19.57 COL 28
    fi_file AT ROW 20.52 COL 26 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 20.62 COL 92.6 RIGHT-ALIGNED
    tbAutoClose AT ROW 22.05 COL 28 WIDGET-ID 78
    btn-ok AT ROW 23 COL 28
    btn-cancel AT ROW 23 COL 48
    "Available Columns" VIEW-AS TEXT
    SIZE 20 BY .62 AT ROW 8.67 COL 4.6 WIDGET-ID 38
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 15.67 COL 5
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 8.71 COL 60.4 WIDGET-ID 44
    RECT-6 AT ROW 16 COL 4
    RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 24.05
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
        TITLE              = "Sales Analysis - Sales Rep Performance Report"
        HEIGHT             = 24.05
        WIDTH              = 95.8
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
    begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    inv-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
    tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sales Analysis - Sales Rep Performance Report */
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
ON WINDOW-CLOSE OF C-Win /* Sales Analysis - Sales Rep Performance Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
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
        IF rd-dest EQ 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.
        RUN GetSelectionList.
        FIND FIRST  ttCustList NO-LOCK NO-ERROR.
        IF NOT tb_cust-list OR  NOT AVAILABLE ttCustList THEN 
        DO:
            EMPTY TEMP-TABLE ttCustList.
            RUN BuildCustList(INPUT cocode,
                INPUT tb_cust-list AND glCustListActive ,
                INPUT begin_cust-no,
                INPUT end_cust-no).
        END.
        RUN run-report. 

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
                            &fax-subject="cwin:title"
                            &fax-body="cwin:title"
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = "Salesman"
                             &begin_cust= begin_slsmn
                             &END_cust=begin_slsmn
                             &mail-subject="cwin:title"
                             &mail-body="cwin:title"
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Salesman"
                                  &begin_cust= begin_slsmn
                                  &END_cust=begin_slsmn
                                  &mail-subject="cwin:title"
                                  &mail-body="cwin:title"
                                  &mail-file=list-name }
                    END.
                END.
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
   DEF VAR ls-filename AS CHARACTER NO-UNDO.
   DEF VAR ll-ok AS LOG NO-UNDO.

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


&Scoped-define SELF-NAME inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-date C-Win
ON LEAVE OF inv-date IN FRAME FRAME-A /* Invoice Date */
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
        inv-date = TODAY.

    RUN DisplaySelectionList.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    Btn_Def:load-image("Graphics/32x32/default.png").
    Btn_Add:load-image("Graphics/32x32/additem.png").
    Btn_Remove:load-image("Graphics/32x32/remove.png").
    btn_Up:load-image("Graphics/32x32/moveup.png").
    btn_down:load-image("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {methods/nowait.i}
    {sys/inc/reportsConfigNK1.i "HT" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    RUN sys/inc/CustListForm.p ( "HT",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO inv-date IN FRAME {&FRAME-NAME}.
    END.
    RUN pChangeDest.
    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'HT',
        INPUT NO,
        OUTPUT glCustListActive).
    {sys/inc/chblankcust.i ""HT""}

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
            INPUT 'HT',
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
        INPUT 'HT').


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
    DISPLAY inv-date tb_cust-list begin_cust-no end_cust-no begin_slsmn end_slsmn 
        tb_fin-chg sl_avail sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 inv-date tb_cust-list btnCustList begin_cust-no 
        end_cust-no begin_slsmn end_slsmn tb_fin-chg sl_avail sl_selected 
        Btn_Def Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV 
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
            fi_file:SCREEN-VALUE = "c:\tmp\SalesRepPerformance.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /***************************************************************************\
    *****************************************************************************
    **  Program: /u2/fold/all/test/asi/sa/sa
    **       by: Christopher A. Heins
    ** Descript: Invoicing summary by rep
    **
    *****************************************************************************
    \***************************************************************************/

    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE lv-r-no     LIKE oe-retl.r-no NO-UNDO.
    DEFINE VARIABLE lv-type     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ld-inv-pct  AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.

    {sys/form/r-top5DL3.f}
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.


    ASSIGN 
        str-tit4 = "" .
    str-tit5 = "" .
    str-line = "" .


    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        tdate    = inv-date
        fsman    = begin_slsmn
        tsman    = end_slsmn
        v-inc-fc = tb_fin-chg.


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

        IF LOOKUP(ttRptSelected.TextList, "Dly Sq Ft/M,Dly Amount,PTD Sq Ft/M,PTD Amount,YTD Sq Ft/M,YTD Amount") <> 0    THEN
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
        /* excelheader = "No,Sales Rep Name,Daily Sq Ft/M,Daily Amount,"
                     + "PTD Sq Ft/M,PTD Amount,YTD Sq Ft/M,YTD Amount". */
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.

    SESSION:SET-WAIT-STATE ("general").

    IF tdate EQ ? THEN tdate = TODAY.

    FIND FIRST period
        WHERE period.company EQ cocode
        AND period.pst     LE tdate
        AND period.pend    GE tdate
        NO-LOCK.

    ASSIGN
        v-period = period.pnum
        v-year   = period.yr
        fdate[2] = period.pst
        edate[2] = tdate
        fdate[1] = tdate
        edate[1] = tdate.

    FIND FIRST period
        WHERE period.company EQ cocode
        AND period.yr      EQ v-year
        NO-LOCK.

    ASSIGN
        fdate[3] = period.pst
        edate[3] = tdate.

    EMPTY TEMP-TABLE tt-report.
    ASSIGN
        v-tot-amt  = 0
        v-tot-sqft = 0.

    FOR EACH ttCustList 
        WHERE ttCustList.log-fld
        NO-LOCK,
        EACH cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ ttCustList.cust-no 
        NO-LOCK:
        FOR EACH ar-inv
            WHERE ar-inv.company  EQ cocode
            AND ar-inv.posted   EQ YES
            AND ar-inv.cust-no  EQ cust.cust-no
            AND ar-inv.inv-date GE fdate[3]
            AND ar-inv.inv-date LE edate[3]
            AND (ar-inv.type    NE "FC" OR v-inc-fc)
            NO-LOCK,

            EACH ar-invl
            WHERE ar-invl.x-no EQ ar-inv.x-no
            AND (ar-invl.billable OR NOT ar-invl.misc)
            NO-LOCK
            TRANSACTION:
          

            {sa/sa-sman4.i "ar-invl"}
        END.

        FOR EACH ar-cash
            WHERE ar-cash.company    EQ cocode
            AND ar-cash.cust-no    EQ cust.cust-no
            AND ar-cash.check-date GE fdate[3]
            AND ar-cash.check-date LE edate[3]
            AND ar-cash.posted     EQ YES
            NO-LOCK,

            EACH ar-cashl
            WHERE ar-cashl.c-no    EQ ar-cash.c-no
            AND ar-cashl.posted  EQ YES
            AND ar-cashl.memo    EQ YES
            AND (/*ar-cashl.inv-no NE 0 OR*/
            CAN-FIND(FIRST account
            WHERE account.company EQ ar-cashl.company
            AND account.actnum  EQ ar-cashl.actnum
            AND account.type    EQ "R"))
            NO-LOCK

            TRANSACTION:

            RELEASE ar-invl.

            RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

            IF AVAILABLE oe-retl THEN
                FIND FIRST ar-invl
                    WHERE ar-invl.company EQ cocode
                    AND ar-invl.cust-no EQ ar-cash.cust-no
                    AND ar-invl.inv-no  EQ ar-cashl.inv-no
                    AND ar-invl.i-no    EQ oe-retl.i-no
                    AND (ar-invl.billable OR NOT ar-invl.misc)
                    NO-LOCK NO-ERROR.

            IF ar-cashl.inv-no NE 0                                                       AND
                (AVAILABLE ar-invl                             OR
                (NOT AVAILABLE reftable AND
                NOT ar-cashl.dscr MATCHES "*oe return*") OR
                SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,5) EQ "items") THEN
                FOR EACH b-ar-invl
                    WHERE b-ar-invl.company EQ ar-cashl.company
                    AND b-ar-invl.cust-no EQ cust.cust-no
                    AND b-ar-invl.inv-no  EQ ar-cashl.inv-no
                    AND (b-ar-invl.billable OR NOT b-ar-invl.misc)
                    AND (NOT AVAILABLE ar-invl OR ROWID(b-ar-invl) EQ ROWID(ar-invl))
                    NO-LOCK:
                    {sa/sa-sman4.i "ar-cashl" "b-"}
                END.

            ELSE
                IF cust.sman GE fsman AND
                    cust.sman LE tsman THEN 
                DO:
                    CREATE tt-report.
                    ASSIGN
                        tt-report.term-id = ""
                        tt-report.key-01  = IF AVAILABLE reftable                      OR
                               ar-cashl.dscr MATCHES "*oe return*" THEN "2" ELSE "4"
                        tt-report.key-02  = cust.sman
                        tt-report.key-09  = cust.cust-no
                        tt-report.key-10  = "ar-cashl"
                        tt-report.rec-id  = RECID(ar-cashl).
        
                END.
        END.
    END.

    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""

        BREAK BY tt-report.key-02

        TRANSACTION:

        FIND FIRST w-data
            WHERE w-data.w-type    EQ tt-report.key-01
            AND w-data.w-sman-no EQ tt-report.key-02
            NO-LOCK NO-ERROR.

        IF NOT AVAILABLE w-data THEN 
        DO:
            CREATE w-data.
            ASSIGN
                w-data.w-type    = tt-report.key-01
                w-data.w-sman-no = tt-report.key-02
                w-data.w-cust-no = tt-report.key-09.
        END.

        FIND ar-invl WHERE RECID(ar-invl) EQ tt-report.rec-id NO-LOCK NO-ERROR.

        IF AVAILABLE ar-invl THEN 
        DO:
            FIND ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK.

            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ ar-invl.i-no
                NO-LOCK NO-ERROR.

            ASSIGN
                v-pct  = 1
                v-amt  = ar-invl.amt
                v-sqft = IF ar-invl.amt-msf NE 0 THEN ar-invl.amt-msf
                  ELSE
                  IF AVAILABLE itemfg THEN
                    (itemfg.t-sqft * ar-invl.ship-qty / 1000) ELSE 0.

            IF v-sqft EQ ? THEN v-sqft = 0.

            DO i = 1 TO 3:
                IF ar-invl.sman[i] EQ tt-report.key-02 THEN
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

            DO i = 1 TO 3:
                IF ar-inv.inv-date GE fdate[i] AND
                    ar-inv.inv-date LE edate[i] THEN
                    ASSIGN
                        w-data.w-sqft[i] = w-data.w-sqft[i] + (v-sqft * v-pct)
                        w-data.w-amt[i]  = w-data.w-amt[i]  + (v-amt  * v-pct).
            END.
        END.

        ELSE 
        DO:
            FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK NO-ERROR.

            IF AVAILABLE ar-cashl THEN 
            DO:
                FIND ar-cash WHERE ar-cash.c-no EQ ar-cashl.c-no NO-LOCK.

                ASSIGN
                    v-amt  = ar-cashl.amt-paid - ar-cashl.amt-disc
                    v-sqft = 0
                    v-pct  = 1.

                RELEASE ar-invl.
                RELEASE oe-retl.

                FIND ar-invl WHERE ROWID(ar-invl) EQ tt-report.row-id NO-LOCK NO-ERROR.

                IF NOT AVAILABLE ar-invl THEN
                    RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

                IF AVAILABLE oe-retl THEN 
                DO:
                    FIND FIRST itemfg
                        WHERE itemfg.company EQ cocode
                        AND itemfg.i-no    EQ oe-retl.i-no
                        NO-LOCK NO-ERROR.

                    v-sqft = IF AVAILABLE itemfg THEN
                        (oe-retl.tot-qty-return * itemfg.t-sqft / 1000)
                        ELSE 0.
                END.

                ELSE
                    IF AVAILABLE ar-invl THEN 
                    DO:
                        ld-inv-pct = 0.
                        FOR EACH b-ar-invl WHERE b-ar-invl.x-no EQ ar-invl.x-no NO-LOCK:
                            ld-inv-pct = ld-inv-pct + b-ar-invl.amt.
                            ACCUMULATE 1 (TOTAL).
                        END.
                        ld-inv-pct = IF ld-inv-pct EQ 0 THEN
                            (1 / IF (ACCUM TOTAL 1) EQ 0 THEN 1
                                                         ELSE (ACCUM TOTAL 1))
                         ELSE (ar-invl.amt / ld-inv-pct).

                        IF ld-inv-pct EQ ? THEN ld-inv-pct = 0.

                        v-amt = v-amt * ld-inv-pct.

                        IF v-sqft EQ ? THEN v-sqft = 0.

                        DO i = 1 TO 3:
                            IF ar-invl.sman[i] EQ tt-report.key-02 THEN
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

                DO i = 1 TO 3:
                    IF ar-cash.check-date GE fdate[i] AND
                        ar-cash.check-date LE edate[i] THEN
                        ASSIGN
                            w-data.w-sqft[i] = w-data.w-sqft[i] - (v-sqft * v-pct)
                            w-data.w-amt[i]  = w-data.w-amt[i]  + (v-amt  * v-pct).
                END.
            END.
        END.

        IF LAST-OF(tt-report.key-02) THEN 
        DO:
            CREATE w-data1.
            ASSIGN
                w-data1.w-sman-no = tt-report.key-02
                w-data1.w-cust-no = tt-report.key-09.

            FOR EACH w-data WHERE w-data.w-sman-no EQ w-data1.w-sman-no:
                DO i = 1 TO 3:
                    ASSIGN
                        w-data1.w-sqft[i] = w-data1.w-sqft[i] + w-data.w-sqft[i]
                        w-data1.w-amt[i]  = w-data1.w-amt[i]  + w-data.w-amt[i]

                        v-tot-amt[i]      = v-tot-amt[i]  + w-data.w-amt[i]
                        v-tot-sqft[i]     = v-tot-sqft[i] + w-data.w-sqft[i].
                END.
            END.

            DO i = 1 TO 3:
                w-data1.w-pmsf[i] = IF w-data1.w-sqft[i] EQ 0 THEN 0
                ELSE (w-data1.w-amt[i] / w-data1.w-sqft[i]).
            END.

            PUT SKIP(1).
            FIND FIRST sman
                WHERE sman.company EQ cocode
                AND sman.sman    EQ w-data1.w-sman-no
                NO-LOCK NO-ERROR.
            FIND FIRST cust
                WHERE cust.company EQ cocode
                AND cust.sman    EQ w-data1.w-cust-no
                NO-LOCK NO-ERROR.
            IF NOT called THEN 
            DO:
            /*display w-data1.w-sman-no
                    sman.sname when avail sman
                    w-data1.w-sqft[1 for 3]
                    w-data1.w-amt[1 for 3] 
                    w-data1.w-pmsf[1 for 3]
                with frame f-prod down.
            down with frame f-prod. */
            END.
            ELSE 
            DO:
            /* display w-data1.w-sman-no
                     sman.sname when avail sman
                     w-data1.w-sqft[1 for 3]
                     w-data1.w-amt[1 for 3] 
                 with frame f-perf down.
             down with frame f-perf. */
            END.

            /* IF tb_excel THEN
                PUT STREAM excel UNFORMATTED
                    '"' w-data1.w-sman-no    '",'
                    '"' IF AVAIL sman THEN sman.sname ELSE "" '",'
                    '"' STRING(w-data1.w-sqft[1],"->>,>>9.999")    '",'
                    '"' STRING(w-data1.w-amt[1],"->,>>>,>>9.99")    '",'
                    '"' STRING(w-data1.w-sqft[2],"->>,>>9.999")    '",'
                    '"' STRING(w-data1.w-amt[2],"->,>>>,>>9.99")    '",'
                    '"' STRING(w-data1.w-sqft[3],"->>,>>9.999")    '",'
                    '"' STRING(w-data1.w-amt[3],"->>,>>>,>>9.99")    '",'
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
                    WHEN "rep"      THEN 
                        cVarValue = w-data1.w-sman-no .
                    WHEN "name"     THEN 
                        cVarValue = IF AVAILABLE sman THEN STRING(sman.sname,"X(25)") ELSE "". 
                    WHEN "dly-sf"   THEN 
                        cVarValue = STRING(w-data1.w-sqft[1],"->>,>>9.999")  .  
                    WHEN "dly-amt"  THEN 
                        cVarValue = STRING(w-data1.w-amt[1],"->,>>>,>>9.99") . 
                    WHEN "ptd-sf"   THEN 
                        cVarValue = STRING(w-data1.w-sqft[2],"->>,>>9.999") .  
                    WHEN "ptd-amt"  THEN 
                        cVarValue = STRING(w-data1.w-amt[2],"->,>>>,>>9.99") . 
                    WHEN "ytd-sf"   THEN 
                        cVarValue = STRING(w-data1.w-sqft[3],"->>,>>9.999") .  
                    WHEN "ytd-amt"  THEN 
                        cVarValue = STRING(w-data1.w-amt[3],"->>,>>>,>>9.99") .
                    WHEN "cust"     THEN 
                        cVarValue = STRING(w-data1.w-cust-no,"x(8)") .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

            DELETE w-data1.
        END.

        DELETE tt-report.
    END.

    RUN run-report-2.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-2 C-Win 
PROCEDURE run-report-2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    FOR EACH w-data BREAK BY w-data.w-type:
        IF w-data.w-type NE "1" THEN
        DO i = 1 TO 3:
            ASSIGN
                v-tot-amt[i]  = v-tot-amt[i]  + w-data.w-amt[i]
                v-tot-sqft[i] = v-tot-sqft[i] + w-data.w-sqft[i].
        END.

        IF LAST-OF(w-data.w-type) THEN 
        DO:
            IF w-data.w-type EQ "1" THEN
            /* if not called then do with frame f-prod:
               underline w-data1.w-sqft[1 for 3]
                         w-data1.w-amt[1 for 3]
                         w-data1.w-pmsf[1 for 3].
               down 1.
             end.
   
             else
             do with frame f-perf:
               underline w-data1.w-sqft[1 for 3]
                         w-data1.w-amt[1 for 3].
               down 1.
             end. */

            DO i = 1 TO 3:
                v-pmsf[i] = v-tot-amt[i] / v-tot-sqft[i].
                IF v-pmsf[i] EQ ? THEN v-pmsf[i] = 0.
            END.

            /* if not called then do: /*with frame f-prod: */
               if w-data.w-type eq "1" then
                 display "  TOTAL SALES" @ sman.sname.
               else
               if w-data.w-type eq "2" then
                 display "  MISC" @ sman.sname.
               else
               if w-data.w-type eq "3" then
                 display "  PREP" @ sman.sname.
               else
                 display "  MEMO" @ sman.sname.
     
               display v-tot-amt[1]  @ w-data1.w-amt[1]
                       v-tot-sqft[1] @ w-data1.w-sqft[1]
                       v-pmsf[1]     @ w-data1.w-pmsf[1]
                       v-tot-amt[2]  @ w-data1.w-amt[2]
                       v-tot-sqft[2] @ w-data1.w-sqft[2]
                       v-pmsf[2]     @ w-data1.w-pmsf[2]
                       v-tot-amt[3]  @ w-data1.w-amt[3]
                       v-tot-sqft[3] @ w-data1.w-sqft[3]
                       v-pmsf[3]     @ w-data1.w-pmsf[3].
               down 1. */

            /* IF tb_excel THEN
                PUT STREAM excel UNFORMATTED
                    SKIP(1)
                    '"' ""     '",'
                    '"' if w-data.w-type eq "1" THEN "  TOTAL SALES"
                        ELSE IF w-data.w-type EQ "2" THEN "  MISC"
                        ELSE IF w-data.w-type EQ "3" THEN "  PREP"
                        ELSE "  MEMO"                            '",'
                    '"' STRING(v-tot-sqft[1],"->>,>>9.999")    '",'
                    '"' STRING(v-tot-amt[1],"->,>>>,>>9.99")    '",'
                    '"' STRING(v-tot-sqft[2],"->>,>>9.999")    '",'
                    '"' STRING(v-tot-amt[2],"->,>>>,>>9.99")    '",'
                    '"' STRING(v-tot-sqft[3],"->>,>>9.999")    '",'
                    '"' STRING(v-tot-amt[3],"->,>>>,>>9.99")    '",'
                    SKIP.
   
             if w-data.w-type eq "1" then do:
               underline w-data1.w-sqft[1 for 3]
                         w-data1.w-amt[1 for 3]
                         w-data1.w-pmsf[1 for 3].
               down 1.
             end. */
            PUT SKIP str-line SKIP .
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:
                    WHEN "rep"      THEN 
                        cVarValue = "" .
                    WHEN "name"     THEN 
                        cVarValue = "". 
                    WHEN "dly-sf"   THEN 
                        cVarValue = STRING(v-tot-sqft[1],"->>,>>9.999")  .  
                    WHEN "dly-amt"  THEN 
                        cVarValue = STRING(v-tot-amt[1],"->,>>>,>>9.99") . 
                    WHEN "ptd-sf"   THEN 
                        cVarValue = STRING(v-tot-sqft[2],"->>,>>9.999") .   
                    WHEN "ptd-amt"  THEN 
                        cVarValue = STRING(v-tot-amt[2],"->,>>>,>>9.99") . 
                    WHEN "ytd-sf"   THEN 
                        cVarValue = STRING(v-tot-sqft[3],"->>,>>9.999")  .  
                    WHEN "ytd-amt"  THEN 
                        cVarValue = STRING(v-tot-amt[3],"->>,>>>,>>9.99")  .
                    WHEN "cust"    THEN 
                        cVarValue = "" .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            IF w-data.w-type EQ "1" THEN 
            DO:
                PUT UNFORMATTED 
                    "  TOTAL SALES " SUBSTRING(cDisplay,15,350) SKIP.
                IF tb_excel THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  
                        "  TOTAL SALES " + substring(cExcelDisplay,3,300) SKIP.
                END.
            END.
            ELSE IF w-data.w-type EQ "2" THEN 
                DO:
                    PUT UNFORMATTED 
                        "  MISC " SUBSTRING(cDisplay,8,350) SKIP.
                    IF tb_excel THEN 
                    DO:
                        PUT STREAM excel UNFORMATTED  
                            "  MISC " + substring(cExcelDisplay,3,300) SKIP.
                    END.
                END.
                ELSE IF w-data.w-type EQ "3" THEN 
                    DO:
                        PUT UNFORMATTED 
                            "  PREP " SUBSTRING(cDisplay,8,350) SKIP.
                        IF tb_excel THEN 
                        DO:
                            PUT STREAM excel UNFORMATTED  
                                "  PREP " + substring(cExcelDisplay,3,300) SKIP.
                        END.
                    END.
                    ELSE 
                    DO:
                        PUT UNFORMATTED 
                            "  MEMO " SUBSTRING(cDisplay,8,350) SKIP.
                        IF tb_excel THEN 
                        DO:
                            PUT STREAM excel UNFORMATTED  
                                "  MEMO " + substring(cExcelDisplay,3,300) SKIP.
                        END.
                    END.

            /* end.
     
             else
             do: /*with frame f-perf:*/
               if w-data.w-type eq "1" then
                 display "  TOTAL SALES" @ sman.sname.
               else
               if w-data.w-type eq "2" then
                 display "  MISC" @ sman.sname.
               else
               if w-data.w-type eq "3" then
                 display "  PREP" @ sman.sname.
               else
                 display "  MEMO" @ sman.sname.
     
               display v-tot-amt[1]  @ w-data1.w-amt[1]
                       v-tot-sqft[1] @ w-data1.w-sqft[1]
                       v-tot-amt[2]  @ w-data1.w-amt[2]
                       v-tot-sqft[2] @ w-data1.w-sqft[2]
                       v-tot-amt[3]  @ w-data1.w-amt[3]
                       v-tot-sqft[3] @ w-data1.w-sqft[3].
               down 1.
     
               IF tb_excel THEN
                  PUT STREAM excel UNFORMATTED
                      SKIP(1)
                      '"' ""     '",'
                      '"' if w-data.w-type eq "1" THEN "  TOTAL SALES"
                          ELSE IF w-data.w-type EQ "2" THEN "  MISC"
                          ELSE IF w-data.w-type EQ "3" THEN "  PREP"
                          ELSE "  MEMO"                            '",'
                      '"' STRING(v-tot-sqft[1],"->>,>>9.999")    '",'
                      '"' STRING(v-tot-amt[1],"->,>>>,>>9.99")    '",'
                      '"' STRING(v-tot-sqft[2],"->>,>>9.999")    '",'
                      '"' STRING(v-tot-amt[2],"->,>>>,>>9.99")    '",'
                      '"' STRING(v-tot-sqft[3],"->>,>>9.999")    '",'
                      '"' STRING(v-tot-amt[3],"->,>>>,>>9.99")    '",'
                      SKIP.
     
               if w-data.w-type eq "1" then do:
                 underline w-data1.w-sqft[1 for 3]
                           w-data1.w-amt[1 for 3].
                 down 1.
               end.
             end.  */

            DO i = 4 TO 6:
                ASSIGN
                    v-tot-amt[i]      = v-tot-amt[i]  + (v-tot-amt[i - 3] *
                                         IF w-data.w-type EQ "1" THEN 1 ELSE -1)
                    v-tot-sqft[i]     = v-tot-sqft[i] + (v-tot-sqft[i - 3] *
                                         IF w-data.w-type EQ "1" THEN 1 ELSE -1)

                    v-tot-amt[i - 3]  = 0
                    v-tot-sqft[i - 3] = 0.
            END.
        END.

        IF LAST(w-data.w-type) THEN 
        DO:
            DO i = 4 TO 6:
                v-pmsf[i - 3] = v-tot-amt[i] / v-tot-sqft[i].
                IF v-pmsf[i - 3] EQ ? THEN v-pmsf[i - 3] = 0.
            END.

            /* if not called then do with frame f-prod:
               display "  SALES"       @ sman.sname
                       v-tot-amt[4]    @ w-data1.w-amt[1]
                       v-tot-sqft[4]   @ w-data1.w-sqft[1]
                       v-pmsf[1]       @ w-data1.w-pmsf[1]
                       v-tot-amt[5]    @ w-data1.w-amt[2]
                       v-tot-sqft[5]   @ w-data1.w-sqft[2]
                       v-pmsf[2]       @ w-data1.w-pmsf[2]
                       v-tot-amt[6]    @ w-data1.w-amt[3]
                       v-tot-sqft[6]   @ w-data1.w-sqft[3]
                       v-pmsf[3]       @ w-data1.w-pmsf[3].
               down 1.
     
               IF tb_excel THEN
                  PUT STREAM excel UNFORMATTED
                      SKIP(1)
                      '"' ""     '",'
                      '"' if w-data.w-type eq "1" THEN "  TOTAL SALES"
                          ELSE IF w-data.w-type EQ "2" THEN "  MISC"
                          ELSE IF w-data.w-type EQ "3" THEN "  PREP"
                          ELSE "  MEMO"                            '",'
                      '"' STRING(v-tot-sqft[4],"->>,>>9.999")    '",'
                      '"' STRING(v-tot-amt[4],"->,>>>,>>9.99")    '",'
                      '"' STRING(v-tot-sqft[5],"->>,>>9.999")    '",'
                      '"' STRING(v-tot-amt[5],"->,>>>,>>9.99")    '",'
                      '"' STRING(v-tot-sqft[6],"->>,>>9.999")    '",'
                      '"' STRING(v-tot-amt[6],"->,>>>,>>9.99")    '",'
                      SKIP.
             end. 
     
             else
             do with frame f-perf:
               display "  SALES"       @ sman.sname
                       v-tot-amt[4]    @ w-data1.w-amt[1]
                       v-tot-sqft[4]   @ w-data1.w-sqft[1]
                       v-tot-amt[5]    @ w-data1.w-amt[2]
                       v-tot-sqft[5]   @ w-data1.w-sqft[2]
                       v-tot-amt[6]    @ w-data1.w-amt[3]
                       v-tot-sqft[6]   @ w-data1.w-sqft[3].
               down 1.
     
               IF tb_excel THEN
                  PUT STREAM excel UNFORMATTED
                      SKIP(1)
                      '"' ""     '",'
                      '"' "  SALES"                           '",'
                      '"' STRING(v-tot-sqft[4],"->>,>>9.999")    '",'
                      '"' STRING(v-tot-amt[4],"->,>>>,>>9.99")    '",'
                      '"' STRING(v-tot-sqft[5],"->>,>>9.999")    '",'
                      '"' STRING(v-tot-amt[5],"->,>>>,>>9.99")    '",'
                      '"' STRING(v-tot-sqft[6],"->>,>>9.999")    '",'
                      '"' STRING(v-tot-amt[6],"->,>>>,>>9.99")    '",'
                      SKIP.
             end. */
            PUT SKIP str-line SKIP .
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:
                    WHEN "rep"      THEN 
                        cVarValue = "" .
                    WHEN "name"     THEN 
                        cVarValue = "". 
                    WHEN "dly-sf"   THEN 
                        cVarValue = STRING(v-tot-sqft[4],"->>,>>9.999")  .  
                    WHEN "dly-amt"  THEN 
                        cVarValue = STRING(v-tot-amt[4],"->,>>>,>>9.99") . 
                    WHEN "ptd-sf"   THEN 
                        cVarValue = STRING(v-tot-sqft[5],"->>,>>9.999") .   
                    WHEN "ptd-amt"  THEN 
                        cVarValue = STRING(v-tot-amt[5],"->,>>>,>>9.99") . 
                    WHEN "ytd-sf"   THEN 
                        cVarValue = STRING(v-tot-sqft[6],"->>,>>9.999")  .  
                    WHEN "ytd-amt"  THEN 
                        cVarValue = STRING(v-tot-amt[6],"->>,>>>,>>9.99")  .
                    WHEN "cust"    THEN 
                        cVarValue = "".

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED 
                "  SALES " SUBSTRING(cDisplay,9,350) SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    "  SALES " + substring(cExcelDisplay,3,300) SKIP.
            END.

        END.

        DELETE w-data.
    END.
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
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," .
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

