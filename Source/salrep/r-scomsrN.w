&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-scomsr.w

  Description: S/A High Sales By Date Range

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
    locode = gloc  .

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL   NO-UNDO.

DEFINE VARIABLE is-xprint-form   AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file      AS CHARACTER NO-UNDO.
DEFINE VARIABLE fdate            AS DATE      FORMAT "99/99/9999" EXTENT 4 NO-UNDO.
DEFINE VARIABLE tdate            LIKE fdate NO-UNDO.
DEFINE VARIABLE fsman            LIKE cust.sman INIT "" NO-UNDO.
DEFINE VARIABLE tsman            LIKE fsman INIT "zzz" NO-UNDO.

DEFINE VARIABLE v-custs          AS INTEGER   FORMAT ">>,>>9" NO-UNDO.
DEFINE VARIABLE v-sort           AS LOG       INIT YES FORMAT "Customer/Salesrep" NO-UNDO.
DEFINE VARIABLE v-inc            AS LOG       INIT YES NO-UNDO.

DEFINE VARIABLE v-amt1           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-slsm           LIKE ar-invl.sman EXTENT 1 NO-UNDO.
DEFINE VARIABLE v-slsp           LIKE ar-invl.s-pct EXTENT 1 NO-UNDO.

DEFINE VARIABLE v-amt            AS DECIMAL   EXTENT 5 NO-UNDO.
DEFINE VARIABLE v-tot-amt        AS DECIMAL   EXTENT 5 NO-UNDO.
DEFINE VARIABLE v-get-mth        AS INTEGER   EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-get-year       AS INTEGER /*FORMAT 9999*/  NO-UNDO.
DEFINE VARIABLE v-label          AS CHARACTER EXTENT 4 FORMAT "x(17)" NO-UNDO.
DEFINE VARIABLE v-head           AS CHARACTER FORMAT "x(300)" EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-prt            AS INTEGER   NO-UNDO.

/*DEF VAR li AS INT NO-UNDO. */
DEFINE VARIABLE lv-start         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lo-date          AS DATE      INIT 12/31/9999 NO-UNDO.
DEFINE VARIABLE hi-date          AS DATE      INIT 01/01/0001 NO-UNDO.
DEFINE VARIABLE fcust            AS ch        INIT "" NO-UNDO.
DEFINE VARIABLE tcust            LIKE fcust INIT "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE lv-sman          AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader      AS CHARACTER NO-UNDO.
/*DEF VAR custcount AS CHAR NO-UNDO.*/
DEFINE VARIABLE salecount        AS CHARACTER NO-UNDO.

DEFINE VARIABLE cDisplay         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcelDisplay    AS CHARACTER NO-UNDO.
DEFINE VARIABLE hField           AS HANDLE    NO-UNDO.
DEFINE VARIABLE cTmpField        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVarValue        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcelVarValue   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSelectedList    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName       AS CHARACTER NO-UNDO.
DEFINE VARIABLE str-tit4         AS cha       FORM "x(200)" NO-UNDO.
DEFINE VARIABLE str-tit5         AS cha       FORM "x(200)" NO-UNDO.
DEFINE VARIABLE str-line         AS cha       FORM "x(350)" NO-UNDO.

DEFINE VARIABLE v-city           AS CHARACTER NO-UNDO .
DEFINE VARIABLE v-state          AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-report NO-UNDO 
    FIELD cust      AS CHARACTER
    FIELD cust-name AS CHARACTER
    FIELD sman      AS CHARACTER
    FIELD curr-year AS DECIMAL
    FIELD Sales1    AS DECIMAL
    FIELD Sales2    AS DECIMAL
    FIELD Sales3    AS DECIMAL
    FIELD Sales4    AS DECIMAL
    FIELD Sales5    AS DECIMAL
    FIELD Delta1    AS DECIMAL
    FIELD Delta2    AS DECIMAL
    FIELD Delta3    AS DECIMAL
    FIELD Delta4    AS DECIMAL
    FIELD Delta5    AS DECIMAL
    FIELD per1      AS DECIMAL
    FIELD per2      AS DECIMAL
    FIELD per3      AS DECIMAL
    FIELD per4      AS DECIMAL .

DEFINE TEMP-TABLE tt-budget NO-UNDO 
    FIELD cust   AS CHARACTER
    FIELD sman   AS CHARACTER
    FIELD vyear  AS DATE
    FIELD budget AS DECIMAL .


DEFINE TEMP-TABLE tt-report2 NO-UNDO LIKE tt-report.

DEFINE STREAM excel.
DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.


ASSIGN 
    cTextListToSelect  = "Name,City,State,Rep,Budget Year,Sales1,Delta1,%-OF Sales Differ1," +
                           "Sales2,Delta2,%-OF Sales Differ2,Sales3,Delta3,%-OF Sales Differ3," +
                           "Sales4,Delta4,%-OF Sales Differ4" 
    cFieldListToSelect = "name,city,stat,rep,bud-yr,sals1,delt1,diff1," +
                            "sals2,delt2,diff2,sals3,delt3,diff3," +
                            "sals4,delt4,diff4"
    cFieldLength       = "30,20,5,3,18,18,18,18," + "18,18,18,18,18,18," + "18,18,18"
    cFieldType         = "c,c,c,c,i,i,i,i," + "i,i,i,i,i,i," + "i,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Name,City,State,Rep,Budget Year,Sales1,Delta1,%-OF Sales Differ1," +
                           "Sales2,Delta2,%-OF Sales Differ2,Sales3,Delta3,%-OF Sales Differ3," +
                           "Sales4,Delta4,%-OF Sales Differ4"  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date1 end_date1 ~
begin_date2 end_date2 begin_date3 end_date3 begin_date4 end_date4 ~
fi_budgetyear tb_col rd_print tb_cust-list btnCustList begin_cust-no ~
end_cust-no begin_slsmn end_slsmn tb_zer-col sl_avail Btn_Def sl_selected ~
Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV tbAutoClose ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date1 end_date1 begin_date2 ~
end_date2 begin_date3 end_date3 begin_date4 end_date4 fi_budgetyear tb_col ~
lbl_sort rd_print tb_cust-list begin_cust-no end_cust-no begin_slsmn ~
end_slsmn tb_zer-col sl_avail sl_selected rd-dest fi_file tb_OpenCSV ~
tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 begin_date1 end_date1 begin_date2 end_date2 ~
begin_date3 end_date3 begin_date4 end_date4 

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
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_date1    AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "1" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_date2    AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "2" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_date3    AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "3" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_date4    AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "4" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_slsmn    AS CHARACTER FORMAT "XXX" 
    LABEL "Beginning Salesrep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date1      AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date2      AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date3      AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date4      AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn      AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
    LABEL "Ending Salesrep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_budgetyear  AS INTEGER   FORMAT ">>>>>" INITIAL 99999 
    VIEW-AS FILL-IN 
    SIZE 13 BY 1.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\SalesComparison.csv" 
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
    SIZE 17 BY 4.43 NO-UNDO.

DEFINE VARIABLE rd_print       AS CHARACTER INITIAL "Customer" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Customer", "Customer",
    "Salesrep", "Salesrep"
    SIZE 31 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 5.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 11.43.

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

DEFINE VARIABLE tb_col       AS LOGICAL   INITIAL NO 
    LABEL "Print Budget Year ?" 
    VIEW-AS TOGGLE-BOX
    SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.2 BY .81 NO-UNDO.

DEFINE VARIABLE tb_zer-col   AS LOGICAL   INITIAL YES 
    LABEL "Page Break by Sales Rep ?" 
    VIEW-AS TOGGLE-BOX
    SIZE 36 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_date1 AT ROW 2.43 COL 30 COLON-ALIGNED
    end_date1 AT ROW 2.43 COL 69 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_date2 AT ROW 3.38 COL 30 COLON-ALIGNED
    end_date2 AT ROW 3.38 COL 69 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_date3 AT ROW 4.33 COL 30 COLON-ALIGNED
    end_date3 AT ROW 4.33 COL 69 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_date4 AT ROW 5.29 COL 30 COLON-ALIGNED
    end_date4 AT ROW 5.29 COL 69 COLON-ALIGNED HELP
    "Enter Ending Date"
    fi_budgetyear AT ROW 6.57 COL 57.2 COLON-ALIGNED HELP
    "Customers To Print" NO-LABELS
    tb_col AT ROW 6.67 COL 32 WIDGET-ID 6
    lbl_sort AT ROW 7.57 COL 20 COLON-ALIGNED NO-LABELS
    rd_print AT ROW 7.57 COL 32 NO-LABELS
    tb_cust-list AT ROW 8.76 COL 32.4 WIDGET-ID 6
    btnCustList AT ROW 8.81 COL 70.6 WIDGET-ID 8
    begin_cust-no AT ROW 9.86 COL 30 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust-no AT ROW 9.86 COL 69 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_slsmn AT ROW 10.86 COL 30 COLON-ALIGNED HELP
    "Enter Beginning Sales Rep Number" WIDGET-ID 2
    end_slsmn AT ROW 10.86 COL 69 COLON-ALIGNED HELP
    "Enter Ending Sales Rep Number" WIDGET-ID 4
    tb_zer-col AT ROW 12.1 COL 32
    sl_avail AT ROW 13.91 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 13.91 COL 40.6 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 13.91 COL 61.8 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 14.91 COL 40.6 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 15.91 COL 40.6 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 16.95 COL 40.6 WIDGET-ID 40
    btn_down AT ROW 17.95 COL 40.6 WIDGET-ID 42
    lv-font-no AT ROW 19.81 COL 34 COLON-ALIGNED
    lv-ornt AT ROW 19.81 COL 44 NO-LABELS
    lines-per-page AT ROW 19.81 COL 88 COLON-ALIGNED
    rd-dest AT ROW 19.91 COL 5 NO-LABELS
    lv-font-name AT ROW 21 COL 30 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 22.43 COL 29
    fi_file AT ROW 23.29 COL 27 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 23.33 COL 93 RIGHT-ALIGNED
    tbAutoClose AT ROW 24.62 COL 29 WIDGET-ID 60
    btn-ok AT ROW 25.57 COL 28.8
    btn-cancel AT ROW 25.57 COL 52
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 13.19 COL 61.2 WIDGET-ID 44
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 19.14 COL 4.2
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 4.6
    BGCOLOR 15 
    "Date Ranges - Column" VIEW-AS TEXT
    SIZE 23 BY 1.19 AT ROW 2.43 COL 5
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 13.19 COL 3 WIDGET-ID 38
    RECT-6 AT ROW 19.57 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 29.81
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
        TITLE              = "Sales Comparison/Sales Rep"
        HEIGHT             = 26.24
        WIDTH              = 96.6
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

/* SETTINGS FOR FILL-IN begin_date1 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    begin_date1:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_date2 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    begin_date2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_date3 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    begin_date3:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_date4 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    begin_date4:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN end_date1 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    end_date1:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN end_date2 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    end_date2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN end_date3 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    end_date3:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN end_date4 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    end_date4:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_budgetyear:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
    tb_col:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_zer-col:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Sales Comparison/Sales Rep */
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
ON WINDOW-CLOSE OF C-Win /* Sales Comparison/Sales Rep */
    DO:
        /* This event will close the window and terminate the procedure.  */
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

        RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
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


&Scoped-define SELF-NAME begin_date1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date1 C-Win
ON LEAVE OF begin_date1 IN FRAME FRAME-A /* 1 */
    DO:
        ASSIGN {&self-name}.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date2 C-Win
ON LEAVE OF begin_date2 IN FRAME FRAME-A /* 2 */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date3 C-Win
ON LEAVE OF begin_date3 IN FRAME FRAME-A /* 3 */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date4 C-Win
ON LEAVE OF begin_date4 IN FRAME FRAME-A /* 4 */
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
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.
  
        IF rd-dest = 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.

        SESSION:SET-WAIT-STATE("general").
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
        SESSION:SET-WAIT-STATE("").  

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

        RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
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


&Scoped-define SELF-NAME end_date1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date1 C-Win
ON LEAVE OF end_date1 IN FRAME FRAME-A /* Ending Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date2 C-Win
ON LEAVE OF end_date2 IN FRAME FRAME-A /* Ending Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date3 C-Win
ON LEAVE OF end_date3 IN FRAME FRAME-A /* Ending Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date4 C-Win
ON LEAVE OF end_date4 IN FRAME FRAME-A /* Ending Date */
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


&Scoped-define SELF-NAME fi_budgetyear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_budgetyear C-Win
ON LEAVE OF fi_budgetyear IN FRAME FRAME-A
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
        IF rd_print = "Customer" THEN 
        DO: 
            tb_zer-col:SCREEN-VALUE = "No" .
            tb_zer-col:SENSITIVE = NO.
        END.
        ELSE
            ASSIGN
                tb_zer-col:SCREEN-VALUE = "Yes"
                tb_zer-col:SENSITIVE    = YES .
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


&Scoped-define SELF-NAME tb_col
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_col C-Win
ON VALUE-CHANGED OF tb_col IN FRAME FRAME-A /* Print Budget Year ? */
    DO:
        ASSIGN {&self-name}.
        IF tb_col = YES THEN
            fi_budgetyear:SENSITIVE =YES .
        ELSE 
        DO:
            ASSIGN
                fi_budgetyear:SCREEN-VALUE = ""
                fi_budgetyear:SENSITIVE    = NO .
        END.
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


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_zer-col
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zer-col C-Win
ON VALUE-CHANGED OF tb_zer-col IN FRAME FRAME-A /* Page Break by Sales Rep ? */
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
    /*  IF access-close THEN DO:
         APPLY "close" TO THIS-PROCEDURE.
         RETURN .
      END.*/

    ASSIGN
        begin_date1 = TODAY - 27
        end_date1   = begin_date1 + 6
        begin_date2 = END_date1 + 1
        end_date2   = begin_date2 + 6 
        begin_date3 = END_date2 + 1 
        end_date3   = begin_date3 + 6 
        begin_date4 = END_date3 + 1
        end_date4   = TODAY.

    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "HR16" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "HR16",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        IF begin_date1:SCREEN-VALUE = "" THEN 
            ASSIGN
                begin_date1:SCREEN-VALUE = STRING(begin_date1)
                end_date1:SCREEN-VALUE   = STRING(end_date1)
                begin_date2:SCREEN-VALUE = STRING(begin_date2)
                end_date2:SCREEN-VALUE   = STRING(end_date2) 
                begin_date3:SCREEN-VALUE = STRING(begin_date3) 
                end_date3:SCREEN-VALUE   = STRING(end_date3) 
                begin_date4:SCREEN-VALUE = STRING(begin_date4)
                end_date4:SCREEN-VALUE   = STRING(end_date4).


        IF tb_col:SCREEN-VALUE = "YES" THEN
            fi_budgetyear:SENSITIVE =YES .
        ELSE 
        DO:
            ASSIGN
                fi_budgetyear:SCREEN-VALUE = ""
                fi_budgetyear:SENSITIVE    = NO .
        END.
        IF rd_print:SCREEN-VALUE = "Customer" THEN 
        DO: 
            tb_zer-col:SCREEN-VALUE = "No" .
            tb_zer-col:SENSITIVE = NO.
        END.
        ELSE
            ASSIGN
                tb_zer-col:SCREEN-VALUE = "Yes" 
                tb_zer-col:SENSITIVE    = YES .

        APPLY "entry" TO begin_date1.
    END.

    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'HR16',
        INPUT NO,
        OUTPUT glCustListActive).
    {sys/inc/chblankcust.i ""HR16""}

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
            INPUT 'HR16',
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
        INPUT 'HR16').


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
    DISPLAY begin_date1 end_date1 begin_date2 end_date2 begin_date3 end_date3 
        begin_date4 end_date4 fi_budgetyear tb_col lbl_sort rd_print 
        tb_cust-list begin_cust-no end_cust-no begin_slsmn end_slsmn 
        tb_zer-col sl_avail sl_selected rd-dest fi_file tb_OpenCSV 
        tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_date1 end_date1 begin_date2 end_date2 begin_date3 
        end_date3 begin_date4 end_date4 fi_budgetyear tb_col rd_print 
        tb_cust-list btnCustList begin_cust-no end_cust-no begin_slsmn 
        end_slsmn tb_zer-col sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nosort C-Win 
PROCEDURE nosort :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE sal-tot AS DECIMAL EXTENT 5 NO-UNDO.
    DEFINE VARIABLE sal-del AS DECIMAL EXTENT 5 NO-UNDO.
    DEFINE VARIABLE sal-per AS DECIMAL EXTENT 5 NO-UNDO.
    /* {sys/form/r-topw.f}

     FORM HEADER
          "Salesrep:"
          lv-sman        FORMAT "x(40)"
         WITH PAGE-TOP FRAME r-top2 STREAM-IO WIDTH 180. */

    {sys/form/r-top5DL3.f}

    FOR EACH tt-report2 NO-LOCK BREAK BY tt-report2.sman:

        FIND FIRST cust WHERE cust.company = cocode 
            AND cust.cust-no = tt-report2.cust NO-LOCK NO-ERROR.
        IF AVAILABLE cust THEN 
        DO:
            ASSIGN
                tt-report2.cust-name = cust.name .
            IF cust.city <> "" THEN
                v-city =  cust.city. 
            IF cust.state <> "" THEN
                v-state = cust.state.
        END. 

        /* format header
                skip(1)
                "                         " 
                "          " 
                "                  " 
                "                  "
                "                  "
                "%-OF Sales        " 
                "                  "  
                "                  " 
                "%-OF Sales        " 
                "                  " 
                "                  "
                "%-OF Sales        " 
                "                  " 
                "                  "  
                "%-OF Sales        " 
                SKIP
                "Name/City/State          " 
                "Sales Rep " 
                "Budget Year" string(v-get-year,"9999")
                "Sales  "  string(v-label[1]) 
                "Delta             " 
                "Difference        " 
                "Sales "   string(v-label[2]) "  " 
                "Delta             " 
                "Difference        " 
                "Sales "   string(v-label[3]) "  "
                "Delta             "  
                "Difference        " 
                "Sales "  string(v-label[4]) "  "
                "Delta             "  
                "Difference        " 
                SKIP 
                "-------------------------"
                "----------"
                "------------------"
                "------------------"
                "------------------"
                "------------------"
                "------------------"
                "------------------"
                "------------------"
                "------------------"
                "------------------"
                "------------------"
                "------------------"
                "------------------"
                "------------------"

             with frame r-top-2 stream-io width 380 no-labels
                  no-box no-underline page-top. */

        IF FIRST-OF(tt-report2.sman) THEN 
        DO:
            ASSIGN
                sal-tot = 0
                sal-del = 0 .
            FIND FIRST sman
                WHERE sman.company EQ cocode
                AND sman.sman    EQ tt-report2.sman
                NO-LOCK NO-ERROR.
        /*   IF AVAIL sman THEN
               lv-sman = string(sman.sman) + "  " + sman.sname .
           IF AVAIL sman AND rd-dest = 3 THEN
                PUT STREAM excel UNFORMATTED
                SKIP
                 '"' string(sman.sman) + "    " + sman.sname                                  '",'
                SKIP. */
        END. 
        IF FIRST(tt-report2.sman) THEN VIEW FRAME r-top. 
        IF FIRST-OF(tt-report2.sman) THEN 
        DO:
        /* IF v-inc THEN */
        /* VIEW FRAME r-top. 
         VIEW FRAME r-top-2. */
        /*DISPLAY "" WITH FRAME r-top.*/
        END.
        IF v-inc THEN 
        DO:
            IF FIRST-OF(tt-report2.sman) THEN PAGE.
        END.  

        /*  PUT tt-report2.cust-name          FORMAT "x(25)" SPACE(1)
                  tt-report2.sman      FORMAT "x(10)" SPACE(1)
                  tt-report2.curr-year FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  tt-report2.Sales1    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  tt-report2.Delta1    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  tt-report2.per1      FORMAT "->,>>>,>>>,>>9.99%" SPACE(1)
                  tt-report2.Sales2    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  tt-report2.Delta2    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  tt-report2.per2      FORMAT "->,>>>,>>>,>>9.99%" SPACE(1)
                  tt-report2.Sales3    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  tt-report2.Delta3    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  tt-report2.per3      FORMAT "->,>>>,>>>,>>9.99%" SPACE(1) 
                  tt-report2.Sales4    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  tt-report2.Delta4    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  tt-report2.per4      FORMAT "->,>>>,>>>,>>9.99%" SKIP  .



          IF rd-dest = 3 THEN
           PUT STREAM excel UNFORMATTED
               '"' tt-report2.cust-name                                  '",'
               '"' tt-report2.sman                                  '",'
               '"' "$ " + STRING(tt-report2.curr-year,"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(tt-report2.Sales1,"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(tt-report2.Delta1,"->,>>>,>>>,>>9.99")   '",'
               '"' IF tt-report2.per1 = 0 THEN "" ELSE STRING(tt-report2.per1,"->,>>>,>>>,>>9.99") + "%"   '",'
               '"' "$ " + STRING(tt-report2.Sales2,"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(tt-report2.Delta2,"->,>>>,>>>,>>9.99")   '",'
               '"' IF tt-report2.per2 = 0 THEN "" ELSE STRING(tt-report2.per2,"->,>>>,>>>,>>9.99") + "%"    '",'
               '"' "$ " + STRING(tt-report2.Sales3,"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(tt-report2.Delta3,"->,>>>,>>>,>>9.99")   '",'
               '"' IF tt-report2.per3 = 0 THEN "" ELSE STRING(tt-report2.per3,"->,>>>,>>>,>>9.99") + "%"   '",'
               '"' "$ " + STRING(tt-report2.Sales4,"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(tt-report2.Delta4,"->,>>>,>>>,>>9.99")   '",'
               '"' IF tt-report2.per4 = 0 THEN "" ELSE STRING(tt-report2.per4,"->,>>>,>>>,>>9.99") + "%"   '",'
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
                WHEN "name"     THEN 
                    cVarValue = STRING(tt-report2.cust-name) . 
                WHEN "city"     THEN 
                    cVarValue = v-city.                        
                WHEN "stat"     THEN 
                    cVarValue = v-state.                       
                WHEN "rep"      THEN 
                    cVarValue = tt-report2.sman  .
                WHEN "bud-yr"   THEN 
                    cVarValue = STRING(tt-report2.curr-year,"$->,>>>,>>>,>>9.99") .
                WHEN "sals1"    THEN 
                    cVarValue = STRING(tt-report2.Sales1,"$->,>>>,>>>,>>9.99")    .
                WHEN "delt1"    THEN 
                    cVarValue = STRING(tt-report2.Delta1,"$->,>>>,>>>,>>9.99")    .
                WHEN "diff1"    THEN 
                    cVarValue = IF tt-report2.per1 = 0 OR tt-report2.per1 = ? THEN "" ELSE STRING(tt-report2.per1,"->,>>>,>>>,>>9.99%") .
                WHEN "sals2"    THEN 
                    cVarValue = STRING(tt-report2.Sales2,"$->,>>>,>>>,>>9.99")    .                              
                WHEN "delt2"    THEN 
                    cVarValue = STRING(tt-report2.Delta2,"$->,>>>,>>>,>>9.99")    .                              
                WHEN "diff2"    THEN 
                    cVarValue = IF tt-report2.per2 = 0 OR tt-report2.per2 = ? THEN "" ELSE STRING(tt-report2.per2,"->,>>>,>>>,>>9.99%") .
                WHEN "sals3"    THEN 
                    cVarValue = STRING(tt-report2.Sales3,"$->,>>>,>>>,>>9.99")    .                              
                WHEN "delt3"    THEN 
                    cVarValue = STRING(tt-report2.Delta3,"$->,>>>,>>>,>>9.99")    .                              
                WHEN "diff3"    THEN 
                    cVarValue = IF tt-report2.per3 = 0 OR tt-report2.per3 = ? THEN "" ELSE STRING(tt-report2.per3,"->,>>>,>>>,>>9.99%") .
                WHEN "sals4"    THEN 
                    cVarValue = STRING(tt-report2.Sales4,"$->,>>>,>>>,>>9.99")    .                              
                WHEN "delt4"    THEN 
                    cVarValue = STRING(tt-report2.Delta4,"$->,>>>,>>>,>>9.99")    .                              
                WHEN "diff4"    THEN 
                    cVarValue = IF tt-report2.per4 = 0 OR tt-report2.per4 = ? THEN "" ELSE STRING(tt-report2.per4,"->,>>>,>>>,>>9.99%") .

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

        ASSIGN
            sal-tot[1] = sal-tot[1] + tt-report2.curr-year 
            sal-tot[2] = sal-tot[2] + tt-report2.Sales1 
            sal-tot[3] = sal-tot[3] + tt-report2.Sales2 
            sal-tot[4] = sal-tot[4] + tt-report2.Sales3 
            sal-tot[5] = sal-tot[5] + tt-report2.Sales4 
            sal-del[1] = sal-del[1] + tt-report2.Delta1
            sal-del[2] = sal-del[2] + tt-report2.Delta2
            sal-del[3] = sal-del[3] + tt-report2.Delta3
            sal-del[4] = sal-del[4] + tt-report2.Delta4 .

        IF LAST-OF(tt-report2.sman) THEN 
        DO:

            ASSIGN
                sal-per[1] = sal-del[1] / sal-tot[2]   * 100
                sal-per[2] = sal-del[2] / sal-tot[3]   * 100
                sal-per[3] = sal-del[3] / sal-tot[4]   * 100
                sal-per[4] = sal-del[4] / sal-tot[5]   * 100.

            /*  PUT SKIP 
                  "                         "
                  "            "
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SPACE(1)
                  "------------------" SKIP  .

              PUT 
                  SPACE(23) "TOTAL Sales: "  SPACE(1)
                  sal-tot[1] FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  sal-tot[2]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  sal-del[1]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  sal-per[1]      FORMAT "->,>>>,>>>,>>9.99%" SPACE(1)
                  sal-tot[3]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  sal-del[2]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                  sal-per[2]     FORMAT "->,>>>,>>>,>>9.99%" SPACE(1)
                  sal-tot[4]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  sal-del[3]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  sal-per[3]     FORMAT "->,>>>,>>>,>>9.99%" SPACE(1) 
                  sal-tot[5]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  sal-del[4]    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                  sal-per[4]      FORMAT "->,>>>,>>>,>>9.99%" SKIP(1)  .

            IF rd-dest = 3 THEN
            PUT STREAM excel UNFORMATTED
               '"' "TOTAL Sales "     + tt-report2.sman            '",'
               '"'                                                '",'
               '"' "$ " + STRING(sal-tot[1],"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(sal-tot[2],"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(sal-del[1],"->,>>>,>>>,>>9.99")   '",'
               '"' IF sal-per[1] = ? THEN "" ELSE STRING(sal-per[1],"->,>>>,>>>,>>9.99") + "%"   '",'
               '"' "$ " + STRING(sal-tot[3],"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(sal-del[2],"->,>>>,>>>,>>9.99")   '",'
               '"' IF sal-per[2] = ? THEN "" ELSE STRING(sal-per[2],"->,>>>,>>>,>>9.99") + "%"    '",'
               '"' "$ " + STRING(sal-tot[4],"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(sal-del[3],"->,>>>,>>>,>>9.99")   '",'
               '"' IF sal-per[3] = ? THEN "" ELSE STRING(sal-per[3],"->,>>>,>>>,>>9.99") + "%"   '",'
               '"' "$ " + STRING(sal-tot[5],"->,>>>,>>>,>>9.99")   '",'
               '"' "$ " + STRING(sal-del[4],"->,>>>,>>>,>>9.99")   '",'
               '"' IF sal-per[4] = ? THEN "" ELSE STRING(sal-per[4],"->,>>>,>>>,>>9.99") + "%"   '",'

              SKIP(1). */
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
                    WHEN "name"     THEN 
                        cVarValue = "" .
                    WHEN "city"     THEN 
                        cVarValue = "".
                    WHEN "stat"     THEN 
                        cVarValue = "".
                    WHEN "rep"      THEN 
                        cVarValue = ""  .
                    WHEN "bud-yr"   THEN 
                        cVarValue = STRING(sal-tot[1],"$->,>>>,>>>,>>9.99") .
                    WHEN "sals1"    THEN 
                        cVarValue = STRING(sal-tot[2],"$->,>>>,>>>,>>9.99")    .
                    WHEN "delt1"    THEN 
                        cVarValue = STRING(sal-del[1],"$->,>>>,>>>,>>9.99")    .
                    WHEN "diff1"    THEN 
                        cVarValue = IF sal-per[1] = 0 OR sal-per[1] = ? THEN "" ELSE STRING(sal-per[1],"->,>>>,>>>,>>9.99%") .
                    WHEN "sals2"    THEN 
                        cVarValue = STRING(sal-tot[3],"$->,>>>,>>>,>>9.99")    .                              
                    WHEN "delt2"    THEN 
                        cVarValue = STRING(sal-del[2],"$->,>>>,>>>,>>9.99")    .                              
                    WHEN "diff2"    THEN 
                        cVarValue = IF sal-per[2] = 0 OR sal-per[2] = ? THEN "" ELSE STRING(sal-per[2],"->,>>>,>>>,>>9.99%") .
                    WHEN "sals3"    THEN 
                        cVarValue = STRING(sal-tot[4],"$->,>>>,>>>,>>9.99")    .                              
                    WHEN "delt3"    THEN 
                        cVarValue = STRING(sal-del[3],"$->,>>>,>>>,>>9.99")    .                              
                    WHEN "diff3"    THEN 
                        cVarValue = IF sal-per[3] = 0 OR sal-per[3] = ? THEN "" ELSE STRING(sal-per[3],"->,>>>,>>>,>>9.99%") .
                    WHEN "sals4"    THEN 
                        cVarValue = STRING(sal-tot[5],"$->,>>>,>>>,>>9.99")    .                              
                    WHEN "delt4"    THEN 
                        cVarValue = STRING(sal-del[4],"$->,>>>,>>>,>>9.99")    .                              
                    WHEN "diff4"    THEN 
                        cVarValue = IF sal-per[4] = 0 OR sal-per[4] = ? THEN "" ELSE STRING(sal-per[4],"->,>>>,>>>,>>9.99%") .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED 
                "TOTAL Sales  " SUBSTRING(cDisplay,14,350) SKIP.
            IF rd-dest = 3 THEN 
            DO:
                PUT STREAM excel UNFORMATTED  "TOTAL Sales " + tt-report2.sman
                    SUBSTRING(cExcelDisplay,4,350) SKIP.
            END.

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
    /*     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
    
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
    DEFINE VARIABLE v-tot     AS DECIMAL FORMAT ">>>,>>>,>>9.99".
    DEFINE VARIABLE v-per-tot AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lSelected AS LOG     INIT YES NO-UNDO.


    {custom/statusMsg.i "'Processing...'"} 

    ASSIGN 
        str-tit4 = "" 
        str-tit5 = "" 
        str-line = "" .


    /*{sys/form/r-topw.f}*/
    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    FORM HEADER
        "Salesrep:"
        lv-sman        FORMAT "x(40)"
        WITH PAGE-TOP FRAME r-top2 STREAM-IO WIDTH 180.

    FORMAT tt-report2.cust     FORMAT "X(30)"
        tt-report2.sman     FORMAT "X(20)"
        tt-report2.curr-year   FORMAT "->,>>>,>>>,>>9.99"
        tt-report2.Sales1      FORMAT "->,>>>,>>>,>>9.99"
        tt-report2.Delta1      FORMAT "->,>>>,>>>,>>9.99"
        tt-report2.per1        FORMAT "->,>>>,>>>,>>9.99"
        tt-report2.Sales2      FORMAT "->,>>>,>>>,>>9.99"
        tt-report2.Delta2      FORMAT "->,>>>,>>>,>>9.99"
        tt-report2.per2        FORMAT "->,>>>,>>>,>>9.99"
        tt-report2.Sales3      FORMAT "->,>>>,>>>,>>9.99"
        tt-report2.Delta3      FORMAT "->,>>>,>>>,>>9.99"
        tt-report2.per3        FORMAT "->,>>>,>>>,>>9.99"
        tt-report2.Sales4      FORMAT "->,>>>,>>>,>>9.99"
        tt-report2.Delta4      FORMAT "->,>>>,>>>,>>9.99"
        tt-report2.per4        FORMAT "->,>>>,>>>,>>9.99"

        WITH FRAME custx DOWN NO-LABELS NO-BOX STREAM-IO WIDTH 380.

    ASSIGN
        str-tit2  = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        fcust     = begin_cust-no
        tcust     = end_cust-no
        fdate[1]  = begin_date1
        tdate[1]  = end_date1
        fdate[2]  = begin_date2
        tdate[2]  = end_date2
        fdate[3]  = begin_date3
        tdate[3]  = end_date3
        fdate[4]  = begin_date4
        tdate[4]  = end_date4
        fsman     = begin_slsmn
        tsman     = end_slsmn

        v-custs   = 0
        v-sort    = rd_print EQ "Customer"
        v-inc     = tb_zer-col
        lSelected = tb_cust-list. 

    IF lselected THEN 
    DO:
        FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
        IF AVAILABLE ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
        FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
        IF AVAILABLE ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
    END.

    DO WITH FRAME {&frame-name}:
        ASSIGN
            v-label[1]   = SUBSTRING(begin_date1:SCREEN-VALUE,7,10) 
            v-label[2]   = SUBSTRING(begin_date2:SCREEN-VALUE,7,10) 
            v-label[3]   = SUBSTRING(begin_date3:SCREEN-VALUE,7,10) 
            v-label[4]   = SUBSTRING(begin_date4:SCREEN-VALUE,7,10) 
            v-get-mth[1] = int(SUBSTRING(begin_date1:SCREEN-VALUE,1,2))
            v-get-mth[2] = int(SUBSTRING(begin_date2:SCREEN-VALUE,1,2))  
            v-get-mth[3] = int(SUBSTRING(begin_date3:SCREEN-VALUE,1,2))  
            v-get-mth[4] = int(SUBSTRING(begin_date4:SCREEN-VALUE,1,2))  
            v-get-mth[5] = int(SUBSTRING(end_date1:SCREEN-VALUE,1,2))
            v-get-mth[6] = int(SUBSTRING(end_date2:SCREEN-VALUE,1,2))  
            v-get-mth[7] = int(SUBSTRING(end_date3:SCREEN-VALUE,1,2))  
            v-get-mth[8] = int(SUBSTRING(end_date4:SCREEN-VALUE,1,2)) 
            .               

        ASSIGN 
            hi-date = TODAY .
        IF tb_col THEN
            lo-date = DATE("01/01/" + STRING(fi_budgetyear)).
        DO li = 1 TO 4:
            IF fdate[li] LT lo-date THEN lo-date = fdate[li].
            IF tdate[li] GT hi-date THEN hi-date = tdate[li].
        END.

    END.
    IF tb_col THEN 
    DO:
        ASSIGN
            v-get-year    = fi_budgetyear  
            v-get-mth[10] = int(12)  .
    END.
    ELSE 
    DO:
        ASSIGN
            v-get-year    = int(SUBSTRING(STRING(TODAY,"99/99/9999"),7,10))  
            v-get-mth[10] = int(SUBSTRING(STRING(TODAY),1,2))  .

    END.

    FORMAT HEADER
        SKIP(1)
        "                         " 
        "          " 
        "                  " 
        "                  "
        "                  "
        "%-OF Sales        " 
        "                  "  
        "                  " 
        "%-OF Sales        " 
        "                  " 
        "                  "
        "%-OF Sales        " 
        "                  " 
        "                  "  
        "%-OF Sales        " 
        SKIP
        "Name/City/State          " 
        "Sales Rep " 
        "Budget Year" STRING(v-get-year,"9999")
        "Sales  "  STRING(v-label[1]) 
        "Delta             " 
        "Difference        " 
        "Sales "   STRING(v-label[2]) "  " 
        "Delta             " 
        "Difference        " 
        "Sales "   STRING(v-label[3]) "  "
        "Delta             "  
        "Difference        " 
        "Sales "  STRING(v-label[4]) "  "
        "Delta             "  
        "Difference        " 
        SKIP 
        "-------------------------"
        "----------"
        "------------------"
        "------------------"
        "------------------"
        "------------------"
        "------------------"
        "------------------"
        "------------------"
        "------------------"
        "------------------"
        "------------------"
        "------------------"
        "------------------"
        "------------------"

        WITH FRAME r-top-2 STREAM-IO WIDTH 380 NO-LABELS
        NO-BOX NO-UNDERLINE PAGE-TOP.


    DEFINE VARIABLE cslist AS CHARACTER NO-UNDO.
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF LOOKUP(ttRptSelected.TextList, "Budget Year,Sales1,Delta1,%-OF Sales Differ1,Sales2,Delta2,%-OF Sales Differ2,Sales3,Delta3,%-OF Sales Differ3,Sales4,Delta4,%-OF Sales Differ4") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 

        IF LOOKUP(ttRptSelected.TextList, "Budget Year") <> 0    THEN 
        DO:
            ASSIGN 
                ttRptSelected.TextList = ttRptSelected.TextList + " " + string(v-get-year) .
        END.
        IF LOOKUP(ttRptSelected.TextList, "Sales1") <> 0    THEN 
        DO:
            ASSIGN 
                ttRptSelected.TextList = ttRptSelected.TextList + " " + string(v-label[1]) .
        END.
        IF LOOKUP(ttRptSelected.TextList, "Sales2") <> 0    THEN 
        DO:
            ASSIGN 
                ttRptSelected.TextList = ttRptSelected.TextList + " " + string(v-label[2]) .
        END.
        IF LOOKUP(ttRptSelected.TextList, "Sales3") <> 0    THEN 
        DO:
            ASSIGN 
                ttRptSelected.TextList = ttRptSelected.TextList + " " + string(v-label[3]) .
        END.
        IF LOOKUP(ttRptSelected.TextList, "Sales4") <> 0    THEN 
        DO:
            ASSIGN 
                ttRptSelected.TextList = ttRptSelected.TextList + " " + string(v-label[4]) .
        END. 

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

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        /*  excelheader = "Name/City/State,Salesrep," + " Budget Year " + string(v-get-year)  + "," + " Sales " + v-label[1] +  ","  + "Delta" + ",%-OF Sales Difference," + " Sales " + v-label[2] +  ","
                      + "Delta" + ",%-OF Sales Difference," + " Sales " + v-label[3] +  "," +  "Delta" + ",%-OF Sales Difference," + " Sales " +  v-label[4] + "," + "Delta" + ",%-OF Sales Difference"
                      . */
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END. 

    EMPTY TEMP-TABLE tt-report.
    EMPTY TEMP-TABLE tt-report2.
    EMPTY TEMP-TABLE tt-budget.
    ASSIGN
        salecount = ""
        custcount = "" 

        .


    SESSION:SET-WAIT-STATE ("general").

    FOR EACH ar-inv
        WHERE ar-inv.company  EQ cocode
        AND ar-inv.cust-no  GE fcust
        AND ar-inv.cust-no  LE tcust
        AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ ar-inv.cust-no
        AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
        AND ar-inv.inv-date GE lo-date
        AND ar-inv.inv-date LE hi-date
        AND ar-inv.posted   EQ YES
        USE-INDEX inv-date NO-LOCK,

        FIRST cust
        WHERE cust.company EQ ar-inv.company
        AND cust.cust-no EQ ar-inv.cust-no
        NO-LOCK:

        {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

        FOR EACH ar-invl
            WHERE ar-invl.x-no    EQ ar-inv.x-no
            AND (ar-invl.billable OR NOT ar-invl.misc)
            NO-LOCK:

            DO i = 1 TO 3:
                ASSIGN
                    v-amt     = 0
                    v-slsm[1] = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN
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

                v-amt[1] = v-amt[1] + v-amt1.
                v-tot = v-tot + v-amt1.
                CREATE tt-budget.
                ASSIGN

                    tt-budget.cust   = cust.cust-no
                    tt-budget.sman   = v-slsm[1]
                    tt-budget.vyear  = ar-inv.inv-date
                    tt-budget.budget = v-amt[1] .
                IF LOOKUP(tt-budget.sman,salecount) = 0 THEN
                    ASSIGN salecount = salecount + tt-budget.sman + ",".
                IF LOOKUP(tt-budget.cust, custcount) = 0 THEN
                    ASSIGN custcount = custcount + tt-budget.cust + "," .
            END.
        END.
    END.




    FOR EACH cust WHERE cust.company EQ cocode
        AND cust.cust-no GE fcust
        AND cust.cust-no LE tcust
        AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ cust.cust-no
        AND ttCustList.log-fld NO-LOCK) ELSE TRUE) NO-LOCK,

        EACH ar-cash
        WHERE ar-cash.company    EQ cocode
        AND ar-cash.cust-no    EQ cust.cust-no
        AND ar-cash.check-date GE lo-date
        AND ar-cash.check-date LE hi-date
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

        {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

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


            v-amt[1] = v-amt[1] + v-amt1.

            CREATE tt-budget.
            ASSIGN

                tt-budget.cust   = cust.cust-no
                tt-budget.sman   = v-slsm[1]
                tt-budget.vyear  = ar-cash.check-date
                tt-budget.budget = v-amt[1] .
            IF LOOKUP(tt-budget.sman,salecount) = 0 THEN
                ASSIGN salecount = salecount + tt-budget.sman + ",".
            IF LOOKUP(tt-budget.cust, custcount) = 0 THEN
                ASSIGN custcount = custcount + tt-budget.cust + "," .         
            IF NOT AVAILABLE ar-invl THEN LEAVE.
        END.
    END.

    FOR EACH tt-budget 
        WHERE tt-budget.vyear  GE date( "01/01/" + string(v-get-year))
        AND tt-budget.vyear LE date("12/31/" + string(v-get-year))  NO-LOCK:

        IF tt-budget.budget = 0  THEN NEXT.

        CREATE tt-report .
        ASSIGN
            tt-report.cust      = tt-budget.cust
            tt-report.sman      = tt-budget.sman
            tt-report.curr-year = 0 .
        FIND FIRST smanbcst NO-LOCK WHERE smanbcst.company = cocode
            AND smanbcst.sman = tt-budget.sman
            AND smanbcst.budget-yr = YEAR(tt-budget.vyear)
            AND smanbcst.budget-period = MONTH(tt-budget.vyear)
            NO-ERROR.
        IF AVAILABLE smanbcst THEN
            tt-report.curr-year = smanbcst.budget-amt.

        IF tt-budget.budget > 0 THEN 
        DO:
            IF LOOKUP(tt-budget.sman,salecount) = 0 THEN
                ASSIGN salecount = salecount + tt-budget.sman + ",".
            IF LOOKUP(tt-budget.cust, custcount) = 0 THEN
                ASSIGN custcount = custcount + tt-budget.cust + "," .
        END.
    END.   


    FOR EACH tt-budget 
        WHERE tt-budget.vyear  GE fdate[1]
        AND tt-budget.vyear LE tdate[1]
        AND LOOKUP(tt-budget.cust,custcount) <> 0 
        AND LOOKUP(tt-budget.sman,salecount) <> 0 NO-LOCK:
        IF tt-budget.budget = 0 THEN NEXT.
        CREATE tt-report .
        ASSIGN
            tt-report.cust   = tt-budget.cust
            tt-report.sman   = tt-budget.sman
            tt-report.Sales1 = tt-budget.budget .
    END. 

    FOR EACH tt-budget 
        WHERE tt-budget.vyear  GE fdate[2]
        AND tt-budget.vyear LE tdate[2] 
        AND LOOKUP(tt-budget.cust,custcount) <> 0 
        AND LOOKUP(tt-budget.sman,salecount) <> 0 NO-LOCK:
        IF tt-budget.budget = 0 THEN NEXT.

        CREATE tt-report .
        ASSIGN
            tt-report.cust   = tt-budget.cust
            tt-report.sman   = tt-budget.sman
            tt-report.Sales2 = tt-budget.budget .
    END.

    FOR EACH tt-budget 
        WHERE tt-budget.vyear  GE fdate[3]
        AND tt-budget.vyear LE tdate[3] 
        AND LOOKUP(tt-budget.cust,custcount) <> 0 
        AND LOOKUP(tt-budget.sman,salecount) <> 0 NO-LOCK:
        IF tt-budget.budget = 0 THEN NEXT.

        CREATE tt-report .
        ASSIGN
            tt-report.cust   = tt-budget.cust
            tt-report.sman   = tt-budget.sman
            tt-report.Sales3 = tt-budget.budget .
    END. 

    FOR EACH tt-budget 
        WHERE tt-budget.vyear  GE fdate[4]
        AND tt-budget.vyear LE tdate[4] 
        AND LOOKUP(tt-budget.cust,custcount) <> 0 
        AND LOOKUP(tt-budget.sman,salecount) <> 0 NO-LOCK:
        IF tt-budget.budget = 0 THEN NEXT.

        CREATE tt-report .
        ASSIGN
            tt-report.cust   = tt-budget.cust
            tt-report.sman   = tt-budget.sman
            tt-report.Sales4 = tt-budget.budget .
    END. 

    FOR EACH tt-report NO-LOCK BREAK BY tt-report.sman  BY tt-report.cust:

        {custom/statusMsg.i "'Processing Customer # ' + string(tt-report.cust)"} 

        IF FIRST-OF(tt-report.cust)  THEN 
        DO:
            CREATE tt-report2.
            ASSIGN
                tt-report2.cust = tt-report.cust     
                tt-report2.sman = tt-report.sman .
        END.
        ASSIGN
            tt-report2.curr-year = tt-report2.curr-year + tt-report.curr-year
            tt-report2.Sales1    = tt-report2.Sales1 + tt-report.Sales1  
            tt-report2.Sales2    = tt-report2.Sales2 + tt-report.Sales2 
            tt-report2.Sales3    = tt-report2.Sales3 + tt-report.Sales3 
            tt-report2.Sales4    = tt-report2.Sales4 + tt-report.Sales4 .
    END.

    FOR EACH tt-report2 NO-LOCK BREAK BY tt-report2.cust:
        v-per-tot = 0.
        FOR EACH smanbcst NO-LOCK WHERE smanbcst.company = cocode
            AND smanbcst.sman = tt-report2.sman
            AND smanbcst.cust-no = tt-report2.cust
            AND smanbcst.budget-yr = (IF tb_col = NO THEN YEAR(TODAY)
            ELSE fi_budgetyear)
            /*AND smanbcst.budget-period = MONTH(tt-budget.vyear) */
            .
            v-per-tot = v-per-tot + smanbcst.budget-amt.
        END.
        tt-report2.curr-year = v-per-tot.
        ASSIGN
            tt-report2.Delta1 = tt-report2.Sales1 - tt-report2.curr-year 
            tt-report2.per1   = tt-report2.Sales1 / tt-report2.curr-year  * 100 
            tt-report2.Delta2 = tt-report2.Sales2 - tt-report2.curr-year 
            tt-report2.per2   = tt-report2.Sales2 / tt-report2.curr-year  * 100 
            tt-report2.Delta3 = tt-report2.Sales3 - tt-report2.curr-year
            tt-report2.per3   = tt-report2.Sales3 / tt-report2.curr-year  * 100
            tt-report2.Delta4 = tt-report2.Sales4  - tt-report2.curr-year
            tt-report2.per4   = tt-report2.Sales4 / tt-report2.curr-year  * 100 .
        IF tt-report2.per1 = ? THEN ASSIGN tt-report2.per1 = 100.
        IF tt-report2.per2 = ? THEN ASSIGN tt-report2.per2 = 100.
        IF tt-report2.per3 = ? THEN ASSIGN tt-report2.per3 = 100.
        IF tt-report2.per4 = ? THEN ASSIGN tt-report2.per4 = 100.



    END.

    IF v-sort THEN 
    DO:
        /*  ASSIGN 
              v-city = "" 
              v-state = "" . */

        FOR EACH tt-report2 NO-LOCK BREAK BY tt-report2.cust :

            {custom/statusMsg.i "'Processing Customer # ' + string(tt-report2.cust)"} 

            FIND FIRST cust WHERE cust.company = cocode 
                AND cust.cust-no = tt-report2.cust NO-LOCK NO-ERROR.
            IF AVAILABLE cust THEN 
            DO:
                ASSIGN
                    tt-report2.cust-name = cust.name .
                IF cust.city <> "" THEN
                    v-city =  cust.city. 
                IF cust.state <> "" THEN
                    v-state = cust.state.
            END. 

            IF FIRST(tt-report2.cust) THEN VIEW FRAME r-top.
            /* IF FIRST(tt-report2.cust) THEN VIEW FRAME r-top-2. */

            /* PUT tt-report2.cust-name          FORMAT "x(25)" SPACE(1)
                     tt-report2.sman      FORMAT "x(10)" SPACE(1)
                     tt-report2.curr-year FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                     tt-report2.Sales1    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                     tt-report2.Delta1    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                     tt-report2.per1      FORMAT "->,>>>,>>>,>>9.99%" SPACE(1)
                     tt-report2.Sales2    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                     tt-report2.Delta2    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1)
                     tt-report2.per2      FORMAT "->,>>>,>>>,>>9.99%" SPACE(1)
                     tt-report2.Sales3    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                     tt-report2.Delta3    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                     tt-report2.per3      FORMAT "->,>>>,>>>,>>9.99%" SPACE(1) 
                     tt-report2.Sales4    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                     tt-report2.Delta4    FORMAT "$->,>>>,>>>,>>9.99" SPACE(1) 
                     tt-report2.per4      FORMAT "->,>>>,>>>,>>9.99%" SKIP 
                       . */

            /*PUT SKIP .*/

            /* IF rd-dest = 3 THEN
              PUT STREAM excel UNFORMATTED
                  '"' tt-report2.cust-name                                  '",'
                  '"' tt-report2.sman                                  '",'
                  '"' STRING(tt-report2.curr-year,"$->,>>>,>>>,>>9.99")   '",'
                  '"' STRING(tt-report2.Sales1,"$->,>>>,>>>,>>9.99")   '",'
                  '"' STRING(tt-report2.Delta1,"$->,>>>,>>>,>>9.99")   '",'
                  '"' IF tt-report2.per1 = 0 THEN "" ELSE STRING(tt-report2.per1,"->,>>>,>>>,>>9.99") + "%"    '",'
                  '"' "$ " + STRING(tt-report2.Sales2,"->,>>>,>>>,>>9.99")   '",'
                  '"' "$ " + STRING(tt-report2.Delta2,"->,>>>,>>>,>>9.99")   '",'
                  '"' IF tt-report2.per2 = 0 THEN "" ELSE STRING(tt-report2.per2,"->,>>>,>>>,>>9.99") + "%"  '",'
                  '"' "$ " + STRING(tt-report2.Sales3,"->,>>>,>>>,>>9.99")   '",'
                  '"' "$ " + STRING(tt-report2.Delta3,"->,>>>,>>>,>>9.99")   '",'
                  '"' IF tt-report2.per3 = 0 THEN "" ELSE STRING(tt-report2.per3,"->,>>>,>>>,>>9.99") + "%"  '",'
                  '"' "$ " + STRING(tt-report2.Sales4,"->,>>>,>>>,>>9.99")   '",'
                  '"' "$ " + STRING(tt-report2.Delta4,"->,>>>,>>>,>>9.99")   '",'
                  '"' IF tt-report2.per4 = 0 THEN "" ELSE STRING(tt-report2.per4,"->,>>>,>>>,>>9.99") + "%"  '",'
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
                    WHEN "name"     THEN 
                        cVarValue = STRING(tt-report2.cust-name) .
                    WHEN "city"     THEN 
                        cVarValue = v-city.
                    WHEN "stat"     THEN 
                        cVarValue = v-state.
                    WHEN "rep"      THEN 
                        cVarValue = tt-report2.sman  .
                    WHEN "bud-yr"   THEN 
                        cVarValue = STRING(tt-report2.curr-year,"$->,>>>,>>>,>>9.99") .
                    WHEN "sals1"    THEN 
                        cVarValue = STRING(tt-report2.Sales1,"$->,>>>,>>>,>>9.99")    .
                    WHEN "delt1"    THEN 
                        cVarValue = STRING(tt-report2.Delta1,"$->,>>>,>>>,>>9.99")    .
                    WHEN "diff1"    THEN 
                        cVarValue = IF tt-report2.per1 = 0 THEN "" ELSE STRING(tt-report2.per1,"->,>>>,>>>,>>9.99%") .
                    WHEN "sals2"    THEN 
                        cVarValue = STRING(tt-report2.Sales2,"$->,>>>,>>>,>>9.99")    .                              
                    WHEN "delt2"    THEN 
                        cVarValue = STRING(tt-report2.Delta2,"$->,>>>,>>>,>>9.99")    .                              
                    WHEN "diff2"    THEN 
                        cVarValue = IF tt-report2.per2 = 0 THEN "" ELSE STRING(tt-report2.per2,"->,>>>,>>>,>>9.99%") .
                    WHEN "sals3"    THEN 
                        cVarValue = STRING(tt-report2.Sales3,"$->,>>>,>>>,>>9.99")    .                              
                    WHEN "delt3"    THEN 
                        cVarValue = STRING(tt-report2.Delta3,"$->,>>>,>>>,>>9.99")    .                              
                    WHEN "diff3"    THEN 
                        cVarValue = IF tt-report2.per3 = 0 THEN "" ELSE STRING(tt-report2.per3,"->,>>>,>>>,>>9.99%") .
                    WHEN "sals4"    THEN 
                        cVarValue = STRING(tt-report2.Sales4,"$->,>>>,>>>,>>9.99")    .                              
                    WHEN "delt4"    THEN 
                        cVarValue = STRING(tt-report2.Delta4,"$->,>>>,>>>,>>9.99")    .                              
                    WHEN "diff4"    THEN 
                        cVarValue = IF tt-report2.per4 = 0 THEN "" ELSE STRING(tt-report2.per4,"->,>>>,>>>,>>9.99%") .

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
    END.
    ELSE 
    DO:
        RUN nosort.  
    END.  /*else do*/


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
            fi_file:SCREEN-VALUE = "c:\tmp\SalesComparison.csv".    
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

