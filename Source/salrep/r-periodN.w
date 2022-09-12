&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-period.w

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

DEFINE TEMP-TABLE tt-report LIKE report
    FIELD row-id   AS ROWID
    FIELD ytd-only AS LOG
    INDEX ytd-only ytd-only key-09.

DEFINE TEMP-TABLE w-inv NO-UNDO
    FIELD inv-no       LIKE ar-inv.inv-no
    FIELD inv-date     LIKE ar-inv.inv-date
    FIELD pst-date     LIKE ar-ledger.tr-date LABEL "Post Date"
    FIELD cust-no      LIKE ar-inv.cust-no
    FIELD name         LIKE cust.name
    FIELD amt          LIKE ar-inv.net FORMAT "->,>>>,>>9.99" LABEL "Net"
    FIELD msf          AS DECIMAL FORMAT "->,>>>,>>9.99" LABEL "MSF"
    FIELD firstInvDate LIKE ar-inv.inv-date .

DEFINE TEMP-TABLE w-sum NO-UNDO
    FIELD cust-no LIKE cust.cust-no
    FIELD name    AS CHARACTER FORMAT "x(30)" LABEL "Customer"
    FIELD amt     AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "PTD Amount"
    FIELD amt-y   AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" LABEL "YTD Amount"
    FIELD msf     AS DECIMAL   FORMAT "->>,>>,>>9.99" LABEL "MSF"
    FIELD wgt     AS DECIMAL.

DEFINE BUFFER xar-inv FOR ar-inv.

DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE STREAM excel.
DEFINE STREAM excel-2.
DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName2         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.


ASSIGN 
    cTextListToSelect  = "Inv #,Inv# Date,First Invoice Date,Post Date,Cust. #,Customer Name,Net,MSF,$/MSF"  /*+
                           "PTD AMT,WGT/MSF"*/
    cFieldListToSelect = "inv,inv-date,firstInvDate,post-date,cust,cust-name,net,msf,msf$" /*+ 
                            "ptd-amt,wgt-msf"*/
    cFieldLength       = "7,10,19,10,8,30,13,13,13" /*+ "13,13" */
    cFieldType         = "i,c,c,c,c,c,i,i,i" /*+ "13,13"  */
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Inv #,Inv# Date,First Invoice Date,Post Date,Cust. #,Customer Name,Net,MSF,$/MSF" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 tb_firstinvdate as-of-date ~
begin_slsmn end_slsmn begin_firstinvdate end_firstinvdate rd_select rd_sort ~
tb_detailed tb_ytd tb_fin-chg sl_avail Btn_Def sl_selected Btn_Add ~
Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV fi_file-2 tbAutoClose ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_firstinvdate as-of-date begin_slsmn ~
end_slsmn begin_firstinvdate end_firstinvdate lbl_select rd_select lbl_sort ~
rd_sort tb_detailed tb_ytd tb_fin-chg sl_avail sl_selected rd-dest fi_file ~
tb_OpenCSV fi_file-2 tbAutoClose 

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

DEFINE VARIABLE as-of-date         AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "As Of Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_firstinvdate AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning First Inv Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_slsmn        AS CHARACTER FORMAT "XXX" 
    LABEL "Beginning Salesrep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_firstinvdate   AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/9999 
    LABEL "Ending First Inv Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_slsmn          AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
    LABEL "Ending Salesrep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file            AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\SalesDetailReport.csv" 
    LABEL "Detail File Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 45 BY 1.

DEFINE VARIABLE fi_file-2          AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\SalesSummaryReport.csv" 
    LABEL "Summary File Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 45 BY 1.

DEFINE VARIABLE lbl_select         AS CHARACTER FORMAT "X(256)":U INITIAL "Select By ?" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort           AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page     AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name       AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no         AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt            AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest            AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 15 BY 4.62 NO-UNDO.

DEFINE VARIABLE rd_select          AS CHARACTER INITIAL "Invoice Date" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Invoice Date", "Invoice Date",
    "Posting Date", "Posting Date"
    SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort            AS CHARACTER INITIAL "Invoice#" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Invoice#", "Invoice#",
    "Customer#", "Cutomer#"
    SIZE 36 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 8.76.

DEFINE RECTANGLE RECT-9
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 7.62.

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

DEFINE VARIABLE tb_detailed     AS LOGICAL   INITIAL YES 
    LABEL "Detailed?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel        AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 FGCOLOR 15 NO-UNDO.

DEFINE VARIABLE tb_fin-chg      AS LOGICAL   INITIAL NO 
    LABEL "Include Finance Charges?" 
    VIEW-AS TOGGLE-BOX
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_firstinvdate AS LOGICAL   INITIAL NO 
    LABEL "First invoice date" 
    VIEW-AS TOGGLE-BOX
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV      AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.6 BY .81 NO-UNDO.

DEFINE VARIABLE tb_ytd          AS LOGICAL   INITIAL NO 
    LABEL "YTD Totals?" 
    VIEW-AS TOGGLE-BOX
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm    AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tb_firstinvdate AT ROW 1.95 COL 75.4 RIGHT-ALIGNED
    as-of-date AT ROW 2.05 COL 28 COLON-ALIGNED
    begin_slsmn AT ROW 3.05 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Sales Rep Number"
    end_slsmn AT ROW 3.05 COL 71 COLON-ALIGNED HELP
    "Enter Ending Sales Rep Number"
    begin_firstinvdate AT ROW 4.1 COL 28 COLON-ALIGNED
    end_firstinvdate AT ROW 4.1 COL 71 COLON-ALIGNED
    lbl_select AT ROW 5.14 COL 23 COLON-ALIGNED NO-LABELS
    rd_select AT ROW 5.14 COL 37 NO-LABELS
    lbl_sort AT ROW 6.1 COL 28 COLON-ALIGNED NO-LABELS
    rd_sort AT ROW 6.1 COL 37 NO-LABELS
    tb_detailed AT ROW 7.05 COL 51 RIGHT-ALIGNED
    tb_ytd AT ROW 8 COL 54 RIGHT-ALIGNED
    tb_fin-chg AT ROW 8.95 COL 37
    sl_avail AT ROW 11.29 COL 4 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 11.29 COL 40.6 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 11.29 COL 60.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 12.29 COL 40.6 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 13.29 COL 40.6 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 14.33 COL 40.6 WIDGET-ID 40
    btn_down AT ROW 15.33 COL 40.6 WIDGET-ID 42
    lv-ornt AT ROW 17.48 COL 31 NO-LABELS
    lines-per-page AT ROW 17.48 COL 84 COLON-ALIGNED
    rd-dest AT ROW 17.81 COL 6 NO-LABELS
    lv-font-no AT ROW 18.14 COL 34 COLON-ALIGNED
    lv-font-name AT ROW 19.1 COL 28 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 20.29 COL 31
    tb_excel AT ROW 20.29 COL 78 RIGHT-ALIGNED
    fi_file AT ROW 22.24 COL 29 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 22.95 COL 91.6 RIGHT-ALIGNED
    fi_file-2 AT ROW 23.43 COL 29 COLON-ALIGNED HELP
    "Enter File Name"
    tbAutoClose AT ROW 25.05 COL 31 WIDGET-ID 78
    btn-ok AT ROW 26.05 COL 31
    btn-cancel AT ROW 26.05 COL 51
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 16.86 COL 5
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 10.57 COL 60.2 WIDGET-ID 44
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 10.57 COL 4.4 WIDGET-ID 38
    RECT-7 AT ROW 1.52 COL 4
    RECT-9 AT ROW 17.19 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 27.29
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
        TITLE              = "Period To Date Sales Report"
        HEIGHT             = 27.29
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
    as-of-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_firstinvdate:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_firstinvdate:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file-2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_select IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_select:PRIVATE-DATA IN FRAME FRAME-A = "rd_select".

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
    rd_select:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_detailed IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_detailed:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_fin-chg:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_firstinvdate IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_firstinvdate:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_ytd IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_ytd:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Period To Date Sales Report */
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
ON WINDOW-CLOSE OF C-Win /* Period To Date Sales Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME as-of-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL as-of-date C-Win
ON LEAVE OF as-of-date IN FRAME FRAME-A /* As Of Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_firstinvdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_firstinvdate C-Win
ON LEAVE OF begin_firstinvdate IN FRAME FRAME-A /* Beginning First Inv Date */
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
  
        IF rd-dest EQ 3 THEN
        DO:
            fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            fi_file-2 = SUBSTRING(fi_file-2,1,INDEX(fi_file-2,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file-2,OUTPUT cFileName2) .
            fi_file:SCREEN-VALUE =  cFileName.
            fi_file-2:SCREEN-VALUE =  cFileName2.
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
                            IF tb_detailed THEN 
                            DO:
                                OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                            END.
                            ELSE 
                            DO:
                                OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName2)).
                            END.
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
        END CASE. 

        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
        SESSION:SET-WAIT-STATE ("").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.
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


&Scoped-define SELF-NAME end_firstinvdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_firstinvdate C-Win
ON LEAVE OF end_firstinvdate IN FRAME FRAME-A /* Ending First Inv Date */
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
ON LEAVE OF fi_file IN FRAME FRAME-A /* Detail File Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file-2 C-Win
ON HELP OF fi_file-2 IN FRAME FRAME-A /* Name */
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


&Scoped-define SELF-NAME fi_file-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file-2 C-Win
ON LEAVE OF fi_file-2 IN FRAME FRAME-A /* Summary File Name */
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


&Scoped-define SELF-NAME rd_select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_select C-Win
ON VALUE-CHANGED OF rd_select IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
    DO:
        ASSIGN {&self-name}.
        RUN pChangeDest .
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


&Scoped-define SELF-NAME tb_firstinvdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_firstinvdate C-Win
ON VALUE-CHANGED OF tb_firstinvdate IN FRAME FRAME-A /* First invoice date */
    DO:
        ASSIGN {&self-name}.
        RUN pFirstInvDateValueChanged.
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


&Scoped-define SELF-NAME tb_ytd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ytd C-Win
ON VALUE-CHANGED OF tb_ytd IN FRAME FRAME-A /* YTD Totals? */
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
        as-of-date = TODAY.
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
    {sys/inc/reportsConfigNK1.i "HC" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN pFirstInvDateValueChanged.
        RUN DisplaySelectionList2.
        APPLY "entry" TO as-of-date.
    END.
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
    DISPLAY tb_firstinvdate as-of-date begin_slsmn end_slsmn begin_firstinvdate 
        end_firstinvdate lbl_select rd_select lbl_sort rd_sort tb_detailed 
        tb_ytd tb_fin-chg sl_avail sl_selected rd-dest fi_file tb_OpenCSV 
        fi_file-2 tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-7 RECT-9 tb_firstinvdate as-of-date begin_slsmn end_slsmn 
        begin_firstinvdate end_firstinvdate rd_select rd_sort tb_detailed 
        tb_ytd tb_fin-chg sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
        btn_Up btn_down rd-dest fi_file tb_OpenCSV fi_file-2 tbAutoClose 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE out-to-port C-Win 
PROCEDURE out-to-port :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RUN custom/d-print.w (list-name).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChangeDest C-Win 
PROCEDURE pChangeDest :
    /*------------------------------------------------------------------------------
             Purpose:    
             Parameters:  <none>
             Notes:      
            ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF rd-dest:SCREEN-VALUE EQ "3" THEN 
        DO:
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "Yes"
                fi_file-2:SENSITIVE     = YES
                tb_OpenCSV:SENSITIVE    = YES      
                tb_excel                = YES
                .
            IF tb_detailed THEN
            DO:
                fi_file:SENSITIVE       = YES . 
            END.
            ELSE 
            DO:
                fi_file:SENSITIVE       = NO . 
            END.
        END.        
        ELSE
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                fi_file:SENSITIVE       = NO
                fi_file-2:SENSITIVE     = NO
                tb_OpenCSV:SENSITIVE    = NO
                tb_excel                = NO
                .
        
        fi_file:SCREEN-VALUE   = "c:\tmp\SalesDetailReport.csv".   
        fi_file-2:SCREEN-VALUE = "c:\tmp\SalesSummaryReport.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFirstInvDateValueChanged C-Win 
PROCEDURE pFirstInvDateValueChanged :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    
        IF LOGICAL(tb_firstinvdate:SCREEN-VALUE) EQ YES THEN 
        DO:
            begin_firstinvdate:HIDDEN = NO.
            end_firstinvdate:HIDDEN   = NO.
        END.
        ELSE 
        DO: 
            begin_firstinvdate:HIDDEN = YES.
            end_firstinvdate:HIDDEN   = YES.
        END.
    
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* --------------------------------------------------- sa/sa-mtdsl.p  7/94 RM */
    /* S/A Month To Date Sales Report Program - S/A Module                        */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE fdate               AS da.
    DEFINE VARIABLE tdate               AS da.
    DEFINE VARIABLE fsman               AS CHARACTER FORMAT "x(3)".
    DEFINE VARIABLE tsman               AS CHARACTER FORMAT "x(3)" INIT "zzz".
    DEFINE VARIABLE xskip               AS CHARACTER FORMAT "x".
    DEFINE VARIABLE linv                AS INTEGER.
    DEFINE VARIABLE save_id             AS RECID.
    DEFINE VARIABLE time_stamp          AS ch.
    DEFINE VARIABLE slct-by-inv         AS LOG       INIT YES FORMAT "I/P" NO-UNDO.
    DEFINE VARIABLE sort-by-inv         AS LOG       INIT YES FORMAT "I/C" NO-UNDO.
    DEFINE VARIABLE v-summ              AS LOG       INIT NO FORMAT "Y/N" NO-UNDO.
    DEFINE VARIABLE v-inc-fc            AS LOG       INIT NO FORMAT "Y/N" NO-UNDO.
    DEFINE VARIABLE v-tot               AS DECIMAL   INIT 0 FORMAT "->>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-msf               LIKE v-tot NO-UNDO.
    DEFINE VARIABLE num-recs            AS INTEGER   INIT 0.
    DEFINE VARIABLE v-tot-sum-tot       AS DECIMAL   FORMAT "->>,>>>,>>9.99".
    DEFINE VARIABLE v-tot-sum-msf       AS DECIMAL   FORMAT "->>,>>>,>>9.99".
    DEFINE VARIABLE v-tot-sum-wgt       AS DECIMAL   FORMAT "->>,>>9.99".
    DEFINE VARIABLE v-tot-sum-tot-y     AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99".
    DEFINE VARIABLE v-sman-no           AS CHARACTER FORMAT "x(3)".
    DEFINE VARIABLE v-amt               LIKE ar-inv.gross.
    DEFINE VARIABLE v-sqft              AS DECIMAL.
    DEFINE VARIABLE v-wght              AS DECIMAL.
    DEFINE VARIABLE v-pct               AS DECIMAL   FORMAT "99.99" NO-UNDO.
    DEFINE VARIABLE dPerSales           AS DECIMAL   FORMAT "99.99" NO-UNDO.
    DEFINE VARIABLE dPerAmt             AS DECIMAL   FORMAT "99.99" NO-UNDO.
    DEFINE VARIABLE ld-inv-pct          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-amt-msf          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-wgt-msf          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dtFirstInvDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE dtCheckFirstInvDate AS DATE      NO-UNDO.

    DEFINE BUFFER xar-inv   FOR ar-inv.
    DEFINE BUFFER b-ar-invl FOR ar-invl.
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

    ASSIGN
        str-tit2    = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        fsman       = begin_slsmn
        tsman       = END_slsmn

        slct-by-inv = rd_select EQ "Invoice Date"
        sort-by-inv = rd_sort BEGINS "Inv"     
        v-summ      = NOT tb_detailed   
        v-inc-fc    = tb_fin-chg.

    SESSION:SET-WAIT-STATE ("general").
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

        IF LOOKUP(ttRptSelected.TextList, "Net,MSF,$/MSF") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.


    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    EMPTY TEMP-TABLE tt-report .

    IF tb_excel THEN 
    DO:
        IF NOT v-summ THEN 
        DO:
        
            OUTPUT STREAM excel TO VALUE(cFileName).
            PUT STREAM excel UNFORMATTED 
                '"' REPLACE(excelheader,',','","') '"' SKIP.

            OUTPUT STREAM excel-2 TO VALUE(cFileName2).
            IF tb_ytd THEN
                excelheader = "Customer,Cust#,PTD AMT,MSF,$/MSF,WGT/MSF,YTD Amount".
            ELSE 
                excelheader = "Customer,Cust#,PTD AMT,MSF,$/MSF,WGT/MSF".
            PUT STREAM excel-2 UNFORMATTED 
                '"' REPLACE(excelheader,',','","') '"' SKIP.
        END.
        ELSE 
        DO:
            OUTPUT STREAM excel TO VALUE(cFileName).
            /*excelheader = "Inv Number,Inv Date,Post Date,Cust#,Name," +
                          "Net,MSF,$/MSF".*/
            PUT STREAM excel UNFORMATTED 
                '"' REPLACE(excelheader,',','","') '"' SKIP.

            OUTPUT STREAM excel-2 TO VALUE(cFileName2).
            IF tb_ytd THEN
                excelheader = "Customer,Cust#,PTD AMT,MSF,$/MSF,WGT/MSF,YTD Amount".
            ELSE 
                excelheader = "Customer,Cust#,PTD AMT,MSF,$/MSF,WGT/MSF".
            PUT STREAM excel-2 UNFORMATTED 
                '"' REPLACE(excelheader,',','","') '"' SKIP.
        END.
    END. 

    DISPLAY WITH FRAME r-top.

    {salrep/r-periodN.i}


    IF tb_excel THEN 
    DO:
        IF NOT v-summ THEN 
        DO:
       
            OUTPUT STREAM excel CLOSE.
            IF tb_OpenCSV THEN
                OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
   
            OUTPUT STREAM excel-2 CLOSE.
            IF tb_OpenCSV THEN
                OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName2)).
        END.
        ELSE 
        DO:
            OUTPUT STREAM excel CLOSE.
            IF tb_OpenCSV THEN
                OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
   
            OUTPUT STREAM excel-2 CLOSE.
            IF tb_OpenCSV THEN
                OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName2)).
        END.
   
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

