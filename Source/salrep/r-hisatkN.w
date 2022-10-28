&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-hisatk.w

  Description: High Sales Tracking

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

/*{sys/inc/custlistform.i ""HR1"" }*/

{sys/ref/CustList.i NEW}

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD dec1 AS DECIMAL 
    FIELD dec2 AS DECIMAL
    INDEX key-01 key-01 key-02
    INDEX key-02 key-02.

DEFINE TEMP-TABLE tt-report2 NO-UNDO LIKE report
    FIELD dec1 AS DECIMAL 
    FIELD dec2 AS DECIMAL
    INDEX high-sales key-01 dec1 DESCENDING dec2 DESCENDING.

DEFINE VARIABLE is-xprint-form AS LOG       NO-UNDO.
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
DEFINE VARIABLE lEndYear           AS LOGICAL   NO-UNDO .
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.

FIND FIRST company NO-LOCK
    WHERE company.company EQ cocode NO-ERROR .

IF company.yend-off EQ 12 THEN 
DO:
    
    ASSIGN 
        cTextListToSelect  = "Rep,SalesRep Name,Customer,Name,January,February,March,April,May,June," +
                                "July,August,September,October,November,December,YTD Amt"
        cFieldListToSelect = "rep,rname,cust,name,p1,p2,p3,p4,p5,p6," +
                                "p7,p8,p9,p10,p11,p12,ytd-amt"
        cFieldLength       = "3,25,8,30,17,17,17,17,17,17," + "17,17,17,17,17,17,17"
        cFieldType         = "c,c,c,c,i,i,i,i,i,i," + "i,i,i,i,i,i,i" 
        .

    ASSIGN 
        cTextListToDefault = "Customer,Name,January,February,March,April,May,June," +
                                               "July,August,September,October,November,December,YTD Amt".
END.
ELSE 
DO:
    ASSIGN 
        lEndYear = YES .
    ASSIGN 
        cTextListToSelect  = "Rep,SalesRep Name,Customer,Name,Period 01,Period 02,Period 03,Period 04,Period 05,Period 06," +
                                "Period 07,Period 08,Period 09,Period 10,Period 11,Period 12,YTD Amt"
        cFieldListToSelect = "rep,rname,cust,name,p1,p2,p3,p4,p5,p6," +
                                "p7,p8,p9,p10,p11,p12,ytd-amt"
        cFieldLength       = "3,25,8,30,17,17,17,17,17,17," + "17,17,17,17,17,17,17"
        cFieldType         = "c,c,c,c,i,i,i,i,i,i," + "i,i,i,i,i,i,i" 
        .

    ASSIGN 
        cTextListToDefault = "Customer,Name,Period 01,Period 02,Period 03,Period 04,Period 05,Period 06," +
                                               "Period 07,Period 08,Period 09,Period 10,Period 11,Period 12,YTD Amt".

END.

{sys/inc/ttRptSel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date end_date srt-period ~
rd_print tb_cust-list begin_cust end_cust begin_slsmn end_slsmn tb_prt-cust ~
sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest ~
fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date srt-period lbl_sort ~
rd_print tb_cust-list begin_cust end_cust begin_slsmn end_slsmn tb_prt-cust ~
sl_avail sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 

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

DEFINE VARIABLE begin_cust     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Cust#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Start Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_slsmn    AS CHARACTER FORMAT "XXX" 
    LABEL "Beginning Salesrep#" 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1.

DEFINE VARIABLE end_cust       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Cust#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "End Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_slsmn      AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
    LABEL "Ending Salesrep#" 
    VIEW-AS FILL-IN 
    SIZE 10 BY 1.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\HighSalesTracking.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
    VIEW-AS FILL-IN 
    SIZE 10 BY 1 NO-UNDO.

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

DEFINE VARIABLE srt-period     AS INTEGER   FORMAT "->9" INITIAL 0 
    LABEL "Sort By Period" 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1.

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
    SIZE 16 BY 4.71 NO-UNDO.

DEFINE VARIABLE rd_print       AS CHARACTER INITIAL "Customer" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Customer", "Customer",
    "Salesrep", "Salesrep"
    SIZE 34 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 5.14.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 9.05.

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
    SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE tb_prt-cust  AS LOGICAL   INITIAL YES 
    LABEL "Print Customers w/Zero Balance?" 
    VIEW-AS TOGGLE-BOX
    SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.8 BY .81
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_date AT ROW 2.19 COL 28.8 COLON-ALIGNED HELP
    "Enter As Of Date"
    end_date AT ROW 2.19 COL 68 COLON-ALIGNED HELP
    "Enter As Of Date"
    srt-period AT ROW 3.33 COL 28.8 COLON-ALIGNED HELP
    "Enter Periods to Sort By"
    lbl_sort AT ROW 4.91 COL 17.8 COLON-ALIGNED NO-LABELS
    rd_print AT ROW 4.91 COL 30.8 NO-LABELS
    tb_cust-list AT ROW 6.1 COL 30.8 WIDGET-ID 6
    begin_cust AT ROW 7.05 COL 28.8 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust AT ROW 7.05 COL 68 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    btnCustList AT ROW 8.05 COL 64.2 WIDGET-ID 8
    begin_slsmn AT ROW 8.19 COL 28.8 COLON-ALIGNED HELP
    "Enter Beginning Sales Rep Number" WIDGET-ID 6
    end_slsmn AT ROW 8.19 COL 68 COLON-ALIGNED HELP
    "Enter Ending Sales Rep Number" WIDGET-ID 8
    tb_prt-cust AT ROW 9.38 COL 31
    sl_avail AT ROW 11.43 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 11.43 COL 41 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 11.43 COL 61.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 12.43 COL 41 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 13.43 COL 41 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 14.48 COL 41 WIDGET-ID 40
    btn_down AT ROW 15.48 COL 41 WIDGET-ID 42
    lv-font-no AT ROW 17.19 COL 39 COLON-ALIGNED
    lines-per-page AT ROW 17.19 COL 88 COLON-ALIGNED
    rd-dest AT ROW 17.38 COL 5 NO-LABELS
    lv-ornt AT ROW 17.43 COL 47 NO-LABELS
    lv-font-name AT ROW 18.14 COL 30 COLON-ALIGNED NO-LABELS
    tb_excel AT ROW 19.33 COL 93 RIGHT-ALIGNED
    td-show-parm AT ROW 20.1 COL 28.6
    fi_file AT ROW 20.95 COL 26.6 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 21 COL 92.8 RIGHT-ALIGNED
    tbAutoClose AT ROW 22.19 COL 28.8 WIDGET-ID 64
    btn-ok AT ROW 23.14 COL 28.6
    btn-cancel AT ROW 23.14 COL 52
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 10.71 COL 61.2 WIDGET-ID 44
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 10.71 COL 3 WIDGET-ID 38
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 5
    BGCOLOR 15 
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 16.67 COL 4
    "(Enter 99 For YTD)" VIEW-AS TEXT
    SIZE 22 BY 1 AT ROW 3.57 COL 42.8
    FGCOLOR 9 
    RECT-6 AT ROW 17.05 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1.05
    SIZE 96 BY 28.38
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
        TITLE              = "High Sales Tracking"
        HEIGHT             = 23.81
        WIDTH              = 95.6
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
    begin_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR BUTTON btnCustList IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    btnCustList:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "rd_print".

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
    rd_print:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    srt-period:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_prt-cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* High Sales Tracking */
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
ON WINDOW-CLOSE OF C-Win /* High Sales Tracking */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON HELP OF begin_cust IN FRAME FRAME-A /* Beginning Cust# */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Cust# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Start Date */
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
                INPUT tb_cust-list AND glCustListActive ,
                INPUT begin_cust,
                INPUT END_cust).
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
                    {custom/asifax.i &begin_cust=begin_slsmn
                            &END_cust=END_slsmn
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE = "salesman"
                             &begin_cust= begin_slsmn
                             &END_cust=end_slsmn
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Salesman"
                                  &begin_cust= begin_slsmn
                                  &END_cust=end_slsmn
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


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON HELP OF end_cust IN FRAME FRAME-A /* Ending Cust# */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Cust# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* End Date */
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


&Scoped-define SELF-NAME srt-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL srt-period C-Win
ON LEAVE OF srt-period IN FRAME FRAME-A /* Sort By Period */
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


&Scoped-define SELF-NAME tb_prt-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-cust C-Win
ON VALUE-CHANGED OF tb_prt-cust IN FRAME FRAME-A /* Print Customers w/Zero Balance? */
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

    FIND FIRST period
        WHERE period.company EQ cocode
        AND period.pst     LE TODAY
        AND period.pend    GE TODAY
        NO-LOCK.  

    ASSIGN
        /*as-of-date = TODAY*/
        /*prt-period = 3*/
        srt-period = period.pnum
        /* cust       = 99999*/.

    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "HR1" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "HR1",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.

        APPLY "entry" TO begin_date IN FRAME {&FRAME-NAME}.
    END.

    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'HR1',
        INPUT NO,
        OUTPUT glCustListActive).
    {sys/inc/chblankcust.i ""HR1""}

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
            INPUT 'HR1',
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
        INPUT 'HR1').


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
    DEFINE VARIABLE cCheckValue   AS CHARACTER NO-UNDO .

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
  
    IF NOT lEndYear THEN 
    DO:
        ldummy = sl_selected:DELETE("Period 01") .
        ldummy = sl_selected:DELETE("Period 02") .
        ldummy = sl_selected:DELETE("Period 03") .
        ldummy = sl_selected:DELETE("Period 04") .
        ldummy = sl_selected:DELETE("Period 05") .
        ldummy = sl_selected:DELETE("Period 06") . 
        ldummy = sl_selected:DELETE("Period 07") .
        ldummy = sl_selected:DELETE("Period 08") .
        ldummy = sl_selected:DELETE("Period 09") .
        ldummy = sl_selected:DELETE("Period 11") .
        ldummy = sl_selected:DELETE("Period 10") .
        ldummy = sl_selected:DELETE("Period 12") .
    END.
    ELSE 
    DO:              
        ldummy = sl_selected:DELETE("January") .
        ldummy = sl_selected:DELETE("February") .
        ldummy = sl_selected:DELETE("March") .
        ldummy = sl_selected:DELETE("April") .
        ldummy = sl_selected:DELETE("May") .
        ldummy = sl_selected:DELETE("June") . 
        ldummy = sl_selected:DELETE("July") .
        ldummy = sl_selected:DELETE("August") .
        ldummy = sl_selected:DELETE("September") .
        ldummy = sl_selected:DELETE("October") .
        ldummy = sl_selected:DELETE("November") .
        ldummy = sl_selected:DELETE("December") .
    END.

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
    DISPLAY begin_date end_date srt-period lbl_sort rd_print tb_cust-list 
        begin_cust end_cust begin_slsmn end_slsmn tb_prt-cust sl_avail 
        sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_date end_date srt-period rd_print tb_cust-list 
        begin_cust end_cust begin_slsmn end_slsmn tb_prt-cust sl_avail Btn_Def 
        sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file 
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
    /* -------------------------------------------------------------------------- */
    /*                                                                            */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE fdate          AS DATE      EXTENT 20 NO-UNDO.
    DEFINE VARIABLE tdate          AS DATE      EXTENT 20 NO-UNDO.
    DEFINE VARIABLE fsman          LIKE cust.sman INIT "" NO-UNDO.
    DEFINE VARIABLE tsman          LIKE fsman INIT "zzz" NO-UNDO.
    DEFINE VARIABLE fcus           LIKE cust.cust-no INIT "" NO-UNDO.
    DEFINE VARIABLE tcus           LIKE cust.cust-no INIT "" NO-UNDO.
    DEFINE VARIABLE v-date         AS DATE      FORMAT "99/99/9999" NO-UNDO.
    DEFINE VARIABLE v-per-1        AS INTEGER   FORMAT ">9" NO-UNDO.
    DEFINE VARIABLE v-per-2        AS INTEGER   FORMAT ">9" NO-UNDO.
    DEFINE VARIABLE v-custs        AS INTEGER   FORMAT ">>,>>9" NO-UNDO.
    DEFINE VARIABLE v-sort         AS LOG       INIT YES FORMAT "Customer/Salesrep" NO-UNDO.
    DEFINE VARIABLE v-inc          AS LOG       INIT YES NO-UNDO.

    DEFINE VARIABLE v-amt1         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-slsm         LIKE ar-invl.sman EXTENT 1 NO-UNDO.
    DEFINE VARIABLE v-slsp         LIKE ar-invl.s-pct EXTENT 1 NO-UNDO.

    DEFINE VARIABLE v-amt          AS DECIMAL   EXTENT 21 NO-UNDO.
    DEFINE VARIABLE v-tot-amt      AS DECIMAL   EXTENT 21 NO-UNDO.
    DEFINE VARIABLE v-label        AS CHARACTER EXTENT 4 FORMAT "x(17)" NO-UNDO.

    DEFINE VARIABLE v-yr           LIKE period.yr NO-UNDO.
    DEFINE VARIABLE v              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v1             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-ytd          AS LOG       NO-UNDO.
    DEFINE VARIABLE v-prt          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-j            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-sman        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-sname       AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dSdate         AS DATE      FORMAT "99/99/9999" NO-UNDO.
    DEFINE VARIABLE dEdate         AS DATE      FORMAT "99/99/9999" NO-UNDO.

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
    DEFINE VARIABLE excelheader  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSelected    AS LOG       INIT YES NO-UNDO.
    DEFINE VARIABLE d-gr-tot-amt AS DECIMAL   EXTENT 21 NO-UNDO.
    DEFINE VARIABLE lp-zero      AS LOGICAL   INITIAL YES NO-UNDO.


    FORM HEADER
        "Salesrep:"
        lv-sman        FORMAT "x(40)"

        WITH PAGE-TOP FRAME r-top2 STREAM-IO WIDTH 180.

    FORM cust.cust-no
        cust.name
        v-amt[17]                                      FORMAT "->,>>>,>>>,>>9.99"
        v-amt[18]                                      FORMAT "->,>>>,>>>,>>9.99"
        v-amt[19]                                      FORMAT "->,>>>,>>>,>>9.99"
        v-amt[20]                                      FORMAT "->,>>>,>>>,>>9.99"
        v-amt[21]      COLUMN-LABEL "YTD"              FORMAT "->,>>>,>>>,>>9.99"

        HEADER SKIP(1)
        "Customer Name                          "
        v-label[1]
        v-label[2]
        v-label[3]
        v-label[4]
        "          YTD Amt"                                          SKIP

        WITH NO-BOX NO-LABELS FRAME custx DOWN STREAM-IO WIDTH 180.


    SESSION:SET-WAIT-STATE ("general").

    EMPTY TEMP-TABLE tt-report.
    EMPTY TEMP-TABLE tt-report2.

    /*v-date = today.*/

    FIND FIRST company WHERE company.company EQ cocode NO-LOCK.

    ASSIGN
        v-per-1   = 12
        v-per-2   = period.pnum
        str-tit2  = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}
        /*v-date     = as-of-date*/
        dSdate    = begin_date
        dEdate    = end_date
        v-per-2   = srt-period
        v-sort    = rd_print EQ "Customer"
        fsman     = begin_slsmn
        tsman     = end_slsmn
        v-inc     = tb_prt-cust
        lSelected = tb_cust-list
        fcus      = begin_cust
        tcus      = END_cust .

    FIND FIRST period
        WHERE period.company EQ cocode
        AND period.pst     LE dEdate
        AND period.pend    GE dSdate
        NO-LOCK.
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

        IF LOOKUP(ttRptSelected.TextList, "January,February,March,April,May,June,July,August,September,October,November,December,YTD Amt") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    FIND LAST period
        WHERE period.company EQ cocode
        AND period.pst     LE dEdate
        AND period.pend    GE dSdate
        NO-LOCK.

    v-yr = period.yr.

    IF v-per-2 EQ 99 THEN 
    DO:
        FIND LAST period
            WHERE period.company EQ cocode
            AND period.yr      EQ v-yr
            AND period.pnum    LE v-per-1
            NO-LOCK.

        ASSIGN
            v-ytd   = YES
            v-per-2 = period.pnum.
    END.

    ASSIGN
        v-date = period.pend
        v1     = v-per-2 - v-per-1 + 1.

    IF v1 LT 1 THEN v1 = 1.

    /*IF tb_excel THEN DO:
      OUTPUT STREAM excel TO VALUE(cFileName).
      excelheader = "Customer,Name".
    END. */

    FOR EACH period
        WHERE period.company EQ cocode
        AND period.yr      EQ v-yr
        AND period.pnum    LE v-per-2
        NO-LOCK:

        ASSIGN
            fdate[period.pnum] = period.pst
            tdate[period.pnum] = period.pend.

        IF period.pnum GE v1 THEN 
        DO:
            ASSIGN
                v          = v + 1
                v-label[v] = v-label[v] +
                  (IF v-label[v] EQ "" THEN "Period " ELSE "/") +
                  trim(STRING(period.pnum,">9")).

            /* IF tb_excel THEN
                excelheader = excelheader + ",Period " + STRING(period.pnum,">9").*/

            IF v GE 4 THEN v = 0.
        END.
    END.

    DO i = 1 TO 4:
        v-label[i] = FILL(" ",17 - length(TRIM(v-label[i]))) + trim(v-label[i]).
    END.

    /*IF tb_excel THEN
    DO:
       excelheader = excelheader + ",YTD Amt".
       PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
    END. */

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    IF lselected THEN 
    DO:
        FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
        IF AVAILABLE ttCustList THEN ASSIGN fcus = ttCustList.cust-no .
        FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
        IF AVAILABLE ttCustList THEN ASSIGN tcus = ttCustList.cust-no .
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END. 
    /*display "" with frame r-top.*/
    FOR EACH ar-inv
        WHERE ar-inv.company  EQ cocode
        AND ar-inv.cust-no  GE fcus
        AND ar-inv.cust-no  LE tcus
        AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ ar-inv.cust-no
        AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
        AND ar-inv.inv-date GE dSdate
        AND ar-inv.inv-date LE dEdate
        AND ar-inv.posted   EQ YES
        USE-INDEX inv-date NO-LOCK,
        FIRST cust 
        WHERE cust.company EQ ar-inv.company
        AND cust.cust-no EQ ar-inv.cust-no
        NO-LOCK:

        FOR EACH ar-invl
            WHERE ar-invl.x-no    EQ ar-inv.x-no
            AND (ar-invl.billable OR NOT ar-invl.misc)
            NO-LOCK:
            {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
            DO i = 1 TO 3:
                ASSIGN
                    v-amt     = 0
                    v-slsm[1] = IF ar-invl.sman[i] EQ "" AND i EQ 1 AND NOT ar-invl.misc THEN
                     cust.sman ELSE ar-invl.sman[i].

                IF v-slsm[1]   LT fsman                         OR
                    v-slsm[1]   GT tsman                         OR
                    (i NE 1 AND
                    (v-slsm[1] EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.

                ASSIGN
                    v-slsp[1] = IF ar-invl.sman[i] EQ ""              OR
                      (ar-invl.s-pct[i] EQ 0 AND i EQ 1) THEN 100
                   ELSE ar-invl.s-pct[i]
                    v-amt1    = ar-invl.amt * v-slsp[1] / 100.
      
                IF ar-inv.inv-date GE fdate[v-per-2] AND
                    ar-inv.inv-date LE tdate[v-per-2] THEN
                    v-amt[1] = v-amt[1] + v-amt1.

                v-amt[2] = v-amt[2] + v-amt1.

                CREATE tt-report.
                ASSIGN
                    tt-report.key-01 = IF v-sort THEN cust.cust-no ELSE v-slsm[1]
                    tt-report.key-02 = cust.cust-no
                    tt-report.key-03 = v-slsm[1]
                    tt-report.dec1   = v-amt[1]
                    tt-report.dec2   = v-amt[2]
                    tt-report.key-10 = "ar-invl"
                    tt-report.rec-id = RECID(ar-invl).
                RELEASE tt-report.
            END.
        END.
    END.



    FOR EACH cust WHERE cust.company EQ cocode
        AND cust.cust-no  GE fcus
        AND cust.cust-no  LE tcus
        AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ cust.cust-no
        AND ttCustList.log-fld NO-LOCK) ELSE TRUE) NO-LOCK,

        EACH ar-cash
        WHERE ar-cash.company    EQ cocode
        AND ar-cash.cust-no    EQ cust.cust-no
        AND ar-cash.check-date GE dSdate
        AND ar-cash.check-date LE dEdate
        AND ar-cash.posted     EQ YES
        USE-INDEX ar-cash NO-LOCK,

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
        RELEASE ar-invl.

        RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

        IF AVAILABLE oe-retl THEN
            FIND FIRST ar-invl
                WHERE ar-invl.company EQ cocode
                AND ar-invl.cust-no EQ cust.cust-no
                AND ar-invl.inv-no  EQ ar-cashl.inv-no
                AND ar-invl.i-no    EQ oe-retl.i-no
                AND (ar-invl.billable OR NOT ar-invl.misc)
                NO-LOCK NO-ERROR.
        DO i = 1 TO 3:
            ASSIGN
                v-amt     = 0
                v-slsm[1] = IF (NOT AVAILABLE ar-invl)                OR
                     (ar-invl.sman[i] EQ "" AND i EQ 1) then
                    cust.sman else ar-invl.sman[i].

            IF v-slsm[1]   LT fsman                         OR
                v-slsm[1]   GT tsman                         OR
                (i NE 1 AND
                (v-slsm[1] EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.

            ASSIGN
                v-slsp[1] = IF (NOT AVAILABLE ar-invl)                OR
                     ar-invl.sman[i] EQ ""              OR
                     (ar-invl.s-pct[i] EQ 0 AND i EQ 1) then 100
                  else ar-invl.s-pct[i]
      v-amt1    = (ar-cashl.amt-paid - ar-cashl.amt-disc) *
                  v-slsp[1] / 100.
      
            IF ar-cash.check-date GE fdate[v-per-2] AND
                ar-cash.check-date LE tdate[v-per-2] THEN
                v-amt[1] = v-amt[1] + v-amt1.

            v-amt[2] = v-amt[2] + v-amt1.

            CREATE tt-report.
            ASSIGN
                tt-report.key-01 = IF v-sort THEN cust.cust-no ELSE  v-slsm[1]
                tt-report.key-02 = cust.cust-no
                tt-report.key-03 = v-slsm[1]
                tt-report.dec1   = v-amt[1]
                tt-report.dec2   = v-amt[2]
                tt-report.key-10 = "ar-cashl"
                tt-report.rec-id = RECID(ar-cashl).
            RELEASE tt-report.

            IF NOT AVAILABLE ar-invl THEN LEAVE.
        END.
    END.

    IF v-inc THEN 
        FOR EACH cust WHERE cust.company EQ cocode
            AND cust.cust-no  GE fcus
            AND cust.cust-no  LE tcus
            AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ cust.cust-no
            AND ttCustList.log-fld NO-LOCK) ELSE TRUE) AND
            NOT CAN-FIND(FIRST tt-report WHERE
            tt-report.key-02 EQ cust.cust-no)
            AND cust.sman GE fsman
            AND cust.sman LE tsman
            NO-LOCK:

            CREATE tt-report.
            ASSIGN
                tt-report.key-01 = IF v-sort THEN cust.cust-no ELSE cust.sman
                tt-report.key-02 = cust.cust-no
                tt-report.key-03 = cust.sman .
            RELEASE tt-report.
        END.


    v-amt = 0.

    FOR EACH tt-report NO-LOCK

        BREAK BY tt-report.key-02
        BY tt-report.key-03 :

        ASSIGN
            v-amt[1] = v-amt[1] + tt-report.dec1
            v-amt[2] = v-amt[2] + tt-report.dec2.

        IF LAST-OF(tt-report.key-03) THEN 
        DO:
            IF v-amt[1] NE 0 OR v-inc OR v-ytd THEN 
            DO:
                CREATE tt-report2.
                ASSIGN
                    tt-report2.key-01 = tt-report.key-01
                    tt-report2.key-02 = tt-report.key-02
                    tt-report2.key-03 = tt-report.key-03 
                    tt-report2.dec1   = IF v-ytd THEN 0 ELSE v-amt[1]
                    tt-report2.dec2   = v-amt[2].
            END.

            v-amt = 0.
        END.
    END.

    ASSIGN
        lv-sman  = ""
        lv-sname = "" .

    FOR EACH tt-report2,

        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ tt-report2.key-02
        NO-LOCK

        BREAK BY tt-report2.key-01
        BY tt-report2.dec1 DESCENDING
        BY tt-report2.dec2 DESCENDING

        WITH FRAME custx:
        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
        IF FIRST-OF(tt-report2.key-01) THEN 
        DO:
            v-prt = 0.
            IF FIRST(tt-report2.key-01) THEN VIEW FRAME r-top.
        END.
  
        FIND FIRST sman
            WHERE sman.company EQ cocode
            AND sman.sman    EQ tt-report2.key-03
            NO-LOCK NO-ERROR.
        lv-sman = TRIM(tt-report2.key-03) .
        lv-sname = TRIM(IF AVAILABLE sman THEN sman.sname ELSE "Not on file").
  
        ASSIGN
            v-amt = 0
            v-prt = v-prt + 1.

        FOR EACH tt-report
            WHERE tt-report.key-01 EQ tt-report2.key-01
            AND tt-report.key-02 EQ tt-report2.key-02:

            IF tt-report.key-10 EQ "ar-invl" THEN 
            DO:
                FIND ar-invl WHERE RECID(ar-invl) EQ tt-report.rec-id NO-LOCK.
                FIND ar-inv  WHERE ar-inv.x-no    EQ ar-invl.x-no  NO-LOCK.

                DO v = v1 TO v-per-2: 
                    IF ar-inv.inv-date GE fdate[v] AND
                        ar-inv.inv-date LE tdate[v] THEN
                        v-amt[v] = v-amt[v] + dec(tt-report.dec2).
                END.

                v-amt[21] = v-amt[21] + dec(tt-report.dec2).
            END.

            ELSE
                IF tt-report.key-10 EQ "ar-cashl" THEN 
                DO:
                    FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK.
                    FIND ar-cash  WHERE ar-cash.c-no    EQ ar-cashl.c-no NO-LOCK.

                    DO v = v1 TO v-per-2:
                        IF ar-cash.check-date GE fdate[v] AND
                            ar-cash.check-date LE tdate[v] THEN
                            v-amt[v] = v-amt[v] + dec(tt-report.dec2).
                    END.

                    v-amt[21] = v-amt[21] + dec(tt-report.dec2).
                END.
        END.
        ASSIGN 
            lp-zero = YES.
        DO i = 1 TO 21: 
            IF v-amt[i] NE 0 THEN
                ASSIGN lp-zero = NO.
        END. 

        IF v-inc OR (NOT v-inc AND (lp-zero EQ NO)) THEN 
        DO:
            /* if v-prt le v-custs then do:*/
            v = 0.

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:  
                    WHEN "rep"    THEN 
                        cVarValue = STRING(lv-sman,"x(3)") .
                    WHEN "rname"   THEN 
                        cVarValue = STRING(lv-sname,"x(25)").
                    WHEN "cust"    THEN 
                        cVarValue = STRING(cust.cust-no,"x(8)") .
                    WHEN "name"   THEN 
                        cVarValue = STRING(cust.name,"x(30)").
                    WHEN "p1"   THEN 
                        cVarValue = STRING(v-amt[1],"->,>>>,>>>,>>9.99").
                    WHEN "p2"  THEN 
                        cVarValue = STRING(v-amt[2],"->,>>>,>>>,>>9.99").
                    WHEN "p3"   THEN 
                        cVarValue = STRING(v-amt[3],"->,>>>,>>>,>>9.99").
                    WHEN "p4"  THEN 
                        cVarValue = STRING(v-amt[4],"->,>>>,>>>,>>9.99").
                    WHEN "p5"   THEN 
                        cVarValue = STRING(v-amt[5],"->,>>>,>>>,>>9.99").
                    WHEN "p6"  THEN 
                        cVarValue = STRING(v-amt[6],"->,>>>,>>>,>>9.99").
                    WHEN "p7"  THEN 
                        cVarValue = STRING(v-amt[7],"->,>>>,>>>,>>9.99").
                    WHEN "p8"   THEN 
                        cVarValue = STRING(v-amt[8],"->,>>>,>>>,>>9.99").
                    WHEN "p9"  THEN 
                        cVarValue = STRING(v-amt[9],"->,>>>,>>>,>>9.99").
                    WHEN "p10"   THEN 
                        cVarValue = STRING(v-amt[10],"->,>>>,>>>,>>9.99").
                    WHEN "p11"  THEN 
                        cVarValue = STRING(v-amt[11],"->,>>>,>>>,>>9.99").
                    WHEN "p12"   THEN 
                        cVarValue = STRING(v-amt[12],"->,>>>,>>>,>>9.99").
                    WHEN "ytd-amt"  THEN 
                        cVarValue = STRING(v-amt[21],"->,>>>,>>>,>>9.99").

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

            PUT SKIP.
            IF tb_excel THEN
                PUT STREAM excel UNFORMATTED SKIP(1).
            /*end.*/

            DO i = 1 TO 21:
                v-tot-amt[i] = v-tot-amt[i] + v-amt[i].
            END.

        END.

        IF LAST-OF(tt-report2.key-01) /*or v-prt eq v-custs*/ THEN 
        DO:
     
            v = 0.

            DO i = v1 TO v-per-2:
                v = v + 1.

                IF v MODULO 4 EQ 0 OR
                    i EQ v-per-2   THEN 
                DO:

                    v-j = v / 4.

                    {sys/inc/roundup.i v-j}

                    ASSIGN
                        j = v-j
                        j = v1 + (4 * (j - 1)).
   
                END.
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
                    WHEN "rep"    THEN 
                        cVarValue = "" .
                    WHEN "rname"   THEN 
                        cVarValue = "".
                    WHEN "cust"    THEN 
                        cVarValue = "" .
                    WHEN "name"   THEN 
                        cVarValue = "".
                    WHEN "p1"   THEN 
                        cVarValue = STRING(v-tot-amt[1],"->,>>>,>>>,>>9.99").
                    WHEN "p2"  THEN 
                        cVarValue = STRING(v-tot-amt[2],"->,>>>,>>>,>>9.99").
                    WHEN "p3"   THEN 
                        cVarValue = STRING(v-tot-amt[3],"->,>>>,>>>,>>9.99").
                    WHEN "p4"  THEN 
                        cVarValue = STRING(v-tot-amt[4],"->,>>>,>>>,>>9.99").
                    WHEN "p5"   THEN 
                        cVarValue = STRING(v-tot-amt[5],"->,>>>,>>>,>>9.99").
                    WHEN "p6"  THEN 
                        cVarValue = STRING(v-tot-amt[6],"->,>>>,>>>,>>9.99").
                    WHEN "p7"  THEN 
                        cVarValue = STRING(v-tot-amt[7],"->,>>>,>>>,>>9.99").
                    WHEN "p8"   THEN 
                        cVarValue = STRING(v-tot-amt[8],"->,>>>,>>>,>>9.99").
                    WHEN "p9"  THEN 
                        cVarValue = STRING(v-tot-amt[9],"->,>>>,>>>,>>9.99").
                    WHEN "p10"   THEN 
                        cVarValue = STRING(v-tot-amt[10],"->,>>>,>>>,>>9.99").
                    WHEN "p11"  THEN 
                        cVarValue = STRING(v-tot-amt[11],"->,>>>,>>>,>>9.99").
                    WHEN "p12"   THEN 
                        cVarValue = STRING(v-tot-amt[12],"->,>>>,>>>,>>9.99").
                    WHEN "ytd-amt"  THEN 
                        cVarValue = STRING(v-tot-amt[21],"->,>>>,>>>,>>9.99").

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT str-line SKIP.
            PUT UNFORMATTED 
                "         TOTALS" SUBSTRING(cDisplay,16,350) SKIP(1).
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    '               TOTALS ,'
                    SUBSTRING(cExcelDisplay,4,350) SKIP(1).
            END.
   
            DO i = 1 TO 21:
                d-gr-tot-amt[i] = d-gr-tot-amt[i] + v-tot-amt[i].
            END.

            IF LAST-OF(tt-report2.key-01) THEN v-tot-amt = 0.
     
        END.

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
            WHEN "rep"    THEN 
                cVarValue = "" .
            WHEN "rname"   THEN 
                cVarValue = "".
            WHEN "cust"    THEN 
                cVarValue = "" .
            WHEN "name"   THEN 
                cVarValue = "".
            WHEN "p1"   THEN 
                cVarValue = STRING(d-gr-tot-amt[1],"->,>>>,>>>,>>9.99").
            WHEN "p2"  THEN 
                cVarValue = STRING(d-gr-tot-amt[2],"->,>>>,>>>,>>9.99").
            WHEN "p3"   THEN 
                cVarValue = STRING(d-gr-tot-amt[3],"->,>>>,>>>,>>9.99").
            WHEN "p4"  THEN 
                cVarValue = STRING(d-gr-tot-amt[4],"->,>>>,>>>,>>9.99").
            WHEN "p5"   THEN 
                cVarValue = STRING(d-gr-tot-amt[5],"->,>>>,>>>,>>9.99").
            WHEN "p6"  THEN 
                cVarValue = STRING(d-gr-tot-amt[6],"->,>>>,>>>,>>9.99").
            WHEN "p7"  THEN 
                cVarValue = STRING(d-gr-tot-amt[7],"->,>>>,>>>,>>9.99").
            WHEN "p8"   THEN 
                cVarValue = STRING(d-gr-tot-amt[8],"->,>>>,>>>,>>9.99").
            WHEN "p9"  THEN 
                cVarValue = STRING(d-gr-tot-amt[9],"->,>>>,>>>,>>9.99").
            WHEN "p10"   THEN 
                cVarValue = STRING(d-gr-tot-amt[10],"->,>>>,>>>,>>9.99").
            WHEN "p11"  THEN 
                cVarValue = STRING(d-gr-tot-amt[11],"->,>>>,>>>,>>9.99").
            WHEN "p12"   THEN 
                cVarValue = STRING(d-gr-tot-amt[12],"->,>>>,>>>,>>9.99").
            WHEN "ytd-amt"  THEN 
                cVarValue = STRING(d-gr-tot-amt[21],"->,>>>,>>>,>>9.99").

        END CASE.

        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT str-line SKIP.
    PUT UNFORMATTED 
        "   GRAND TOTALS" SUBSTRING(cDisplay,16,350) SKIP.
    IF tb_excel THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            '        GRAND TOTALS ,'
            SUBSTRING(cExcelDisplay,4,350) SKIP(1).
    END. 

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
            begin_cust:SENSITIVE  = NOT iplChecked
            end_cust:SENSITIVE    = NOT iplChecked
            begin_cust:VISIBLE    = NOT iplChecked
            end_cust:VISIBLE      = NOT iplChecked
            btnCustList:SENSITIVE = iplChecked
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
            fi_file:SCREEN-VALUE = "c:\tmp\HighSalesTracking.csv".   
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

