&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: gl\r-tribal.w

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
{gl\ttTrialBalance.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmp-dir   AS CHARACTER NO-UNDO.

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

DEFINE VARIABLE v-invalid  AS LOG NO-UNDO.
DEFINE VARIABLE v-download AS LOG INIT NO NO-UNDO.
DEFINE VARIABLE v-prior    AS LOG INIT NO NO-UNDO.

DEFINE BUFFER tmp-per FOR period.

DEFINE STREAM s-temp.
DEFINE VARIABLE v-postable     AS LOG       NO-UNDO.
DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtDateRange1   AS DATE      NO-UNDO.
DEFINE VARIABLE dtDateRange2   AS DATE      NO-UNDO.
DEFINE VARIABLE cFileName      AS CHARACTER NO-UNDO.

DEFINE STREAM excel.

DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAuditdir AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (cocode, "AUDITDIR", "C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN cAuditdir = cReturn.

IF LOOKUP(SUBSTR(cAuditdir,LENGTH(cAuditdir),1),"/,\") > 0 THEN
    cAuditdir = SUBSTR(cAuditdir,1,LENGTH(cAuditdir) - 1).


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-10 RECT-6 RECT-7 tb_show-all-account ~
tran-date begin_acct-no end_acct-no tb_sup-zero tb_show-detail ~
tb_include-summary-total tb_fill-field tb_Period-Detail tb_sub-acct ~
begin_sub-acct end_sub-acct rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_show-all-account lbl_show-all-account ~
tran-date tran-period begin_acct-no end_acct-no lbl_paid tb_sup-zero ~
lbl_show-detail tb_show-detail lbl_include-summery tb_include-summary-total ~
tb_fill-field lbl_fill-field lbl_Period-Details tb_Period-Detail ~
tb_sub-acct lbl_paid-2 v-sub-acct-lvl begin_sub-acct end_sub-acct rd-dest ~
fi_file tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE begin_acct-no        AS CHARACTER FORMAT "x(25)" INITIAL "0" 
    LABEL "Beginning Acct#" 
    VIEW-AS FILL-IN 
    SIZE 30 BY 1.

DEFINE VARIABLE begin_sub-acct       AS INTEGER   FORMAT ">>>>>>>>9" INITIAL 0 
    LABEL "Beginning Subacct" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1.

DEFINE VARIABLE end_acct-no          AS CHARACTER FORMAT "x(25)" INITIAL "zzzzzzzz" 
    LABEL "Ending Acct#" 
    VIEW-AS FILL-IN 
    SIZE 30 BY 1.

DEFINE VARIABLE end_sub-acct         AS INTEGER   FORMAT ">>>>>>>>9" INITIAL 999999999 
    LABEL "Ending Subacct" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1.

DEFINE VARIABLE fi_file              AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\TrialBalance.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_fill-field       AS CHARACTER FORMAT "X(256)":U INITIAL "Include the fill in fields:" 
    VIEW-AS FILL-IN 
    SIZE 23.6 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_include-summery  AS CHARACTER FORMAT "X(256)":U INITIAL "Include Summary Total:" 
    VIEW-AS FILL-IN 
    SIZE 23.4 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_paid             AS CHARACTER FORMAT "X(256)":U INITIAL "Suppress Zero Balances?" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_paid-2           AS CHARACTER FORMAT "X(256)":U INITIAL "Sort by Sub Account Level?" 
    VIEW-AS FILL-IN 
    SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_Period-Details   AS CHARACTER FORMAT "X(256)":U INITIAL "Period Details:" 
    VIEW-AS FILL-IN 
    SIZE 15.4 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_show-all-account AS CHARACTER FORMAT "X(256)":U INITIAL "Show All Account:" 
    VIEW-AS FILL-IN 
    SIZE 18.8 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_show-detail      AS CHARACTER FORMAT "X(256)":U INITIAL "Show Detail:" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page       AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name         AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no           AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date            AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Transaction Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period          AS INTEGER   FORMAT ">9":U INITIAL 0 
    LABEL "Period" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-sub-acct-lvl       AS INTEGER   FORMAT ">>":U INITIAL 0 
    LABEL "Sub Account Level" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt              AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest              AS INTEGER   INITIAL 1 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 15 BY 5 NO-UNDO.

DEFINE RECTANGLE RECT-10
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 4.14.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.48.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 8.29.

DEFINE VARIABLE tbAutoClose              AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel                 AS LOGICAL INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_fill-field            AS LOGICAL INITIAL NO 
    LABEL "" 
    VIEW-AS TOGGLE-BOX
    SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE tb_include-summary-total AS LOGICAL INITIAL NO 
    LABEL "Show Detail" 
    VIEW-AS TOGGLE-BOX
    SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE tb_Period-Detail         AS LOGICAL INITIAL NO 
    LABEL "" 
    VIEW-AS TOGGLE-BOX
    SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV               AS LOGICAL INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.2 BY .81 NO-UNDO.

DEFINE VARIABLE tb_show-all-account      AS LOGICAL INITIAL NO 
    LABEL "Show Detail" 
    VIEW-AS TOGGLE-BOX
    SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE tb_show-detail           AS LOGICAL INITIAL NO 
    LABEL "Show Detail" 
    VIEW-AS TOGGLE-BOX
    SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sub-acct              AS LOGICAL INITIAL NO 
    LABEL "Sort by Sub Account Level?" 
    VIEW-AS TOGGLE-BOX
    SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sup-zero              AS LOGICAL INITIAL YES 
    LABEL "Suppress Zero Balance" 
    VIEW-AS TOGGLE-BOX
    SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm             AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tb_show-all-account AT ROW 1.86 COL 76 WIDGET-ID 8
    lbl_show-all-account AT ROW 1.91 COL 55.2 COLON-ALIGNED NO-LABELS WIDGET-ID 6
    tran-date AT ROW 1.95 COL 33.2 COLON-ALIGNED
    tran-period AT ROW 3 COL 33.2 COLON-ALIGNED
    begin_acct-no AT ROW 4.33 COL 33.2 COLON-ALIGNED HELP
    "Enter Beginning Account Number"
    end_acct-no AT ROW 5.29 COL 33.2 COLON-ALIGNED HELP
    "Enter Ending Account Number"
    lbl_paid AT ROW 6.48 COL 12 COLON-ALIGNED NO-LABELS
    tb_sup-zero AT ROW 6.48 COL 41
    lbl_show-detail AT ROW 7.48 COL 25.6 COLON-ALIGNED NO-LABELS
    tb_show-detail AT ROW 7.48 COL 41
    lbl_include-summery AT ROW 7.48 COL 53.6 COLON-ALIGNED NO-LABELS WIDGET-ID 2
    tb_include-summary-total AT ROW 7.48 COL 79.2 WIDGET-ID 4
    tb_fill-field AT ROW 8.52 COL 41 WIDGET-ID 16
    lbl_fill-field AT ROW 8.57 COL 15.2 COLON-ALIGNED NO-LABELS WIDGET-ID 14
    lbl_Period-Details AT ROW 8.57 COL 61.4 COLON-ALIGNED NO-LABELS WIDGET-ID 10
    tb_Period-Detail AT ROW 8.57 COL 79.2 WIDGET-ID 12
    tb_sub-acct AT ROW 10.57 COL 41
    lbl_paid-2 AT ROW 10.67 COL 10 COLON-ALIGNED NO-LABELS
    v-sub-acct-lvl AT ROW 11.76 COL 39 COLON-ALIGNED
    begin_sub-acct AT ROW 12.95 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Sub Account#"
    end_sub-acct AT ROW 12.95 COL 66.4 COLON-ALIGNED HELP
    "Enter Ending Sub Account#"
    rd-dest AT ROW 14.95 COL 5 NO-LABELS
    lines-per-page AT ROW 15.1 COL 86.8 COLON-ALIGNED
    lv-font-no AT ROW 15.19 COL 32 COLON-ALIGNED
    lv-ornt AT ROW 15.19 COL 42.6 NO-LABELS
    lv-font-name AT ROW 16.33 COL 28.8 COLON-ALIGNED NO-LABELS NO-TAB-STOP 
    tb_excel AT ROW 17.48 COL 91.6 RIGHT-ALIGNED
    td-show-parm AT ROW 17.81 COL 28.4
    fi_file AT ROW 18.81 COL 26.2 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 18.91 COL 92.2 RIGHT-ALIGNED
    tbAutoClose AT ROW 20.43 COL 28.2 WIDGET-ID 64
    btn-ok AT ROW 21.48 COL 28
    btn-cancel AT ROW 21.48 COL 50.8
    " SORT OPTIONS" VIEW-AS TEXT
    SIZE 20 BY .62 AT ROW 9.81 COL 33.8
    FONT 6
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.05 COL 4
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 14.29 COL 4
    RECT-10 AT ROW 10.14 COL 3
    RECT-6 AT ROW 14.71 COL 3
    RECT-7 AT ROW 1.52 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 99 BY 23
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
        TITLE              = "Trial Balance"
        HEIGHT             = 22
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
    begin_acct-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_sub-acct:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_acct-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_sub-acct:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_fill-field IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_include-summery IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_paid IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_paid-2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_Period-Details IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_show-all-account IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_show-detail IN FRAME FRAME-A
   NO-ENABLE                                                            */
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

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_sub-acct:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_sup-zero:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    tran-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    tran-period:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN v-sub-acct-lvl IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Trial Balance */
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
ON WINDOW-CLOSE OF C-Win /* Trial Balance */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_acct-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_acct-no C-Win
ON LEAVE OF begin_acct-no IN FRAME FRAME-A /* Beginning Acct# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_sub-acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_sub-acct C-Win
ON LEAVE OF begin_sub-acct IN FRAME FRAME-A /* Beginning Subacct */
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
        RUN check-date.
        IF v-invalid THEN RETURN NO-APPLY.

        DO WITH FRAME frame-a:
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.
        IF rd-dest = 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.

        SESSION:SET-WAIT-STATE ("general").

        IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "southpak"
            THEN is-xprint-form = YES.     
        ELSE is-xprint-form = NO.
  
        IF tb_show-detail THEN
            RUN run-report-detail .
        ELSE 
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
                    {custom/asifax.i &type= " "
                            &begin_cust= "begin_acct-no"
                            &END_cust= "begin_acct-no" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = " "
                             &begin_cust= "begin_acct-no"
                             &END_cust= "begin_acct-no"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = " "
                                  &begin_cust="begin_acct-no"
                                  &END_cust="begin_acct-no"
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


&Scoped-define SELF-NAME end_acct-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_acct-no C-Win
ON LEAVE OF end_acct-no IN FRAME FRAME-A /* Ending Acct# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_sub-acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_sub-acct C-Win
ON LEAVE OF end_sub-acct IN FRAME FRAME-A /* Ending Subacct */
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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fill-field
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fill-field C-Win
ON VALUE-CHANGED OF tb_fill-field IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_include-summary-total
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_include-summary-total C-Win
ON VALUE-CHANGED OF tb_include-summary-total IN FRAME FRAME-A /* Show Detail */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_Period-Detail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_Period-Detail C-Win
ON VALUE-CHANGED OF tb_Period-Detail IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_show-all-account
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-all-account C-Win
ON VALUE-CHANGED OF tb_show-all-account IN FRAME FRAME-A /* Show Detail */
    DO:
        ASSIGN {&self-name}.
  
        RUN pSetParameter.   
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_show-detail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-detail C-Win
ON VALUE-CHANGED OF tb_show-detail IN FRAME FRAME-A /* Show Detail */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sub-acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sub-acct C-Win
ON VALUE-CHANGED OF tb_sub-acct IN FRAME FRAME-A /* Sort by Sub Account Level? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sup-zero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sup-zero C-Win
ON VALUE-CHANGED OF tb_sup-zero IN FRAME FRAME-A /* Suppress Zero Balance */
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


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Transaction Date */
    DO:
        ASSIGN {&self-name}.

        IF LASTKEY NE -1 THEN 
        DO:
            RUN check-date.
            IF v-invalid THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-sub-acct-lvl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-sub-acct-lvl C-Win
ON LEAVE OF v-sub-acct-lvl IN FRAME FRAME-A /* Sub Account Level */
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


    tran-date = TODAY.

    RUN init-proc.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "GT" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    {custom/usrprint.i}
  
    tran-date:SCREEN-VALUE = STRING(TODAY).

    RUN check-date.
  
    RUN pSetParameter.

    {methods/nowait.i}
    RUN pChangeDest.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date C-Win 
PROCEDURE check-date :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&frame-name}:
        v-invalid = NO.

        FIND FIRST period                   
            WHERE period.company EQ cocode
            AND period.pst     LE tran-date
            AND period.pend    GE tran-date
            NO-LOCK NO-ERROR.
        IF AVAILABLE period THEN 
        DO: 
            tran-period:SCREEN-VALUE = STRING(period.pnum).
            dtDateRange1 = period.pst.
            dtDateRange2 = period.pend.
        END.

        ELSE 
        DO:
            MESSAGE "No Defined Period Exists for" tran-date VIEW-AS ALERT-BOX ERROR.
            v-invalid = YES.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateAuditDir C-Win 
PROCEDURE pCreateAuditDir :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE temp_fid   AS CHARACTER FORMAT "X(50)" NO-UNDO.
    DEFINE VARIABLE dirname1   AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE dirname2   AS CHARACTER FORMAT "X(30)" NO-UNDO.
  
    ASSIGN
        dirname1   = cAuditdir
        dirname2   = cAuditdir + "\GT".

    IF SEARCH(temp_fid) EQ ? THEN 
    DO:
        OS-CREATE-DIR VALUE(dirname1).
        OS-CREATE-DIR VALUE(dirname2).
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
    DISPLAY tb_show-all-account lbl_show-all-account tran-date tran-period 
        begin_acct-no end_acct-no lbl_paid tb_sup-zero lbl_show-detail 
        tb_show-detail lbl_include-summery tb_include-summary-total 
        tb_fill-field lbl_fill-field lbl_Period-Details tb_Period-Detail 
        tb_sub-acct lbl_paid-2 v-sub-acct-lvl begin_sub-acct end_sub-acct 
        rd-dest fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-10 RECT-6 RECT-7 tb_show-all-account tran-date begin_acct-no 
        end_acct-no tb_sup-zero tb_show-detail tb_include-summary-total 
        tb_fill-field tb_Period-Detail tb_sub-acct begin_sub-acct end_sub-acct 
        rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc C-Win 
PROCEDURE init-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.company = cocode AND
        sys-ctrl.name    = "TRIALBAL"
        NO-LOCK NO-ERROR.
    IF NOT(AVAILABLE(sys-ctrl)) THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = cocode
            sys-ctrl.name    = "TRIALBAL"
            sys-ctrl.descrip = "Download Trial Balance to Excel?".
        MESSAGE sys-ctrl.descrip
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE sys-ctrl.log-fld.
    END.
    v-download = sys-ctrl.log-fld.


    FIND FIRST company WHERE company.company EQ cocode NO-LOCK.
    v-sub-acct-lvl = company.acc-level.

    FIND FIRST tmp-per WHERE tmp-per.company EQ cocode
        AND tmp-per.pstat
        AND tmp-per.yr      EQ (period.yr - 1)
        NO-LOCK NO-ERROR.
    IF AVAILABLE tmp-per OR NOT company.yend-per THEN
        ASSIGN v-prior = TRUE.

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
    DEFINE VARIABLE printok   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
    DEFINE VARIABLE result    AS LOGICAL   NO-UNDO.

    /*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
         IF NOT printok THEN
         RETURN NO-APPLY.
    */
    /*
      /* Use Progress Print. Always use Font#9 in Registry (set above) */
         RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                                INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                        /* use-dialog(1) and landscape(2) */
      */                                  
    RUN custom/prntproc.p (list-name,INT(lv-font-no), lv-ornt).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCompanyAttributes C-Win 
PROCEDURE pGetCompanyAttributes PRIVATE :
    /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsFYEnd AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcContra AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRet AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdtPeriodStart AS DATE NO-UNDO.
    
    FIND FIRST company NO-LOCK 
        WHERE company.company EQ ipcCompany
        NO-ERROR.
    IF AVAILABLE company THEN 
    DO:
        FIND LAST period NO-LOCK 
            WHERE period.company EQ ipcCompany
            AND period.pst     LE ipdtAsOf
            AND period.pend    GE ipdtAsOf
            NO-ERROR.
        IF AVAILABLE period THEN 
            ASSIGN
                opdtPeriodStart = period.pst
                .
            
        FIND FIRST gl-ctrl NO-LOCK
            WHERE gl-ctrl.company EQ company.company
            NO-ERROR.
        IF AVAILABLE gl-ctrl THEN 
            ASSIGN 
                opcContra = gl-ctrl.contra
                opcRet    = gl-ctrl.ret
                .
        FIND LAST period NO-LOCK 
            WHERE period.company EQ company.company 
            AND period.pnum EQ company.num-per  /* it's the last period of (a) year */ 
            AND period.pend EQ ipdtAsOf        /* it's the end date of the last period */
            NO-ERROR.
        ASSIGN 
            oplIsFYEnd = AVAILABLE period.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetParameter C-Win 
PROCEDURE pSetParameter :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&frame-name}:
    
        IF LOGICAL(tb_show-all-account:SCREEN-VALUE) EQ TRUE THEN
        DO:
            ASSIGN 
                begin_acct-no:SCREEN-VALUE = ""
                end_acct-no:SCREEN-VALUE   = "zzzzzzzzzzzzzzzzzzzzzzzzz".
            DISABLE begin_acct-no end_acct-no .
        END.
        ELSE
            ENABLE begin_acct-no end_acct-no.
   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    DEFINE VARIABLE save_id             AS RECID     NO-UNDO.
    DEFINE VARIABLE time_stamp          AS ch        NO-UNDO.
    DEFINE VARIABLE subac               AS INTEGER   FORMAT ">>>>>>>>9" NO-UNDO.
    DEFINE VARIABLE subac-lvl           AS INTEGER   FORMAT "9" NO-UNDO.
    DEFINE VARIABLE fsubac              AS INTEGER   FORMAT ">>>>>>>>9" INIT 0 NO-UNDO.
    DEFINE VARIABLE tsubac              AS INTEGER   FORMAT ">>>>>>>>9" INIT 999999999 NO-UNDO.
    DEFINE VARIABLE aclevel             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cyr                 AS DECIMAL   FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE tcyr                AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dadj                AS CHARACTER LABEL "DB Adjust" FORMAT "x(10)" INIT "__________" NO-UNDO.
    DEFINE VARIABLE cadj                AS CHARACTER LABEL "CR Adjust" FORMAT "x(10)" INIT "__________" NO-UNDO.
    DEFINE VARIABLE bsht                AS CHARACTER LABEL "Bal Sheet" FORMAT "x(10)" INIT "__________" NO-UNDO.
    DEFINE VARIABLE incs                AS CHARACTER LABEL "Income Stat" FORMAT "x(11)" INIT "___________" NO-UNDO.
    DEFINE VARIABLE v-rep-tot           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE vyear               LIKE period.yr NO-UNDO.
    DEFINE VARIABLE dtPeriodStart       LIKE glhist.tr-date NO-UNDO.
    DEFINE VARIABLE v-fisc-yr           LIKE period.yr NO-UNDO.

    DEFINE VARIABLE tacct               LIKE glhist.actnum LABEL "      To Account Number" NO-UNDO.
    DEFINE VARIABLE facct               LIKE glhist.actnum LABEL "    From Account Number" NO-UNDO.
    DEFINE VARIABLE ptd-value           AS DECIMAL   FORMAT "->>>,>>>,>>9.99" INIT 0 NO-UNDO.
    DEFINE VARIABLE tot-ptd             AS DECIMAL   FORMAT "->>>,>>>,>>9.99" INIT 0 NO-UNDO.
    DEFINE VARIABLE suppress-zero       AS LOGICAL   NO-UNDO INIT TRUE
        LABEL "Suppress Zero Balances?".
    DEFINE VARIABLE break-flag          AS LOG       INIT NO NO-UNDO.
    DEFINE VARIABLE start-lvl           AS INTEGER   INIT 0 NO-UNDO.
    DEFINE VARIABLE temp_fid            AS CHARACTER NO-UNDO.

    DEFINE VARIABLE str_buffa           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-first             AS LOG       INIT YES NO-UNDO.
    DEFINE VARIABLE v-hdr               AS CHARACTER INITIAL
        "Account#,Description,PTD,YTD,DB Adjust,CR Adjust,Bal Sheet,Income Stat" NO-UNDO.
    DEFINE VARIABLE v-comma             AS CHARACTER FORMAT "x" INITIAL "," NO-UNDO.

    DEFINE VARIABLE cPeriodLabel        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cYtdLabel           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAsofDateLabel      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dAssetAmountPTD     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dAssetAmountYTD     AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE dCapitalAmountPTD   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCapitalAmountYTD   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dExpenseAmountPTD   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dExpenseAmountYTD   AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE dLiabilityAmountPTD AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dLiabilityAmountYTD AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE dRevenueAmountPTD   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dRevenueAmountYTD   AS DECIMAL   NO-UNDO.  
    DEFINE VARIABLE dTitleAmountPTD     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTitleAmountYTD     AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE cAccountType        AS CHARACTER NO-UNDO.   
    DEFINE VARIABLE cFinancialReport    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-tit4            AS CHARACTER FORMAT "x(240)" NO-UNDO.
    DEFINE VARIABLE str-tit5            AS CHARACTER FORMAT "x(240)" NO-UNDO.
    DEFINE VARIABLE dOpenBalance        AS DECIMAL   FORMAT "->>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE dDebitActivity      AS DECIMAL   FORMAT "->>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE dCreditActivity     AS DECIMAL   FORMAT "->>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE dEndingBalance      AS DECIMAL   FORMAT "->>>>,>>>,>>9.99" NO-UNDO.

    DEFINE VARIABLE dTotPTD             AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotYTD             AS DECIMAL   NO-UNDO.
    

 
    {sys/inc/print1.i}
    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

 
    SESSION:SET-WAIT-STATE("general").
 
    cPeriodLabel = " Period " + string(MONTH(tran-date),"99") + " Total".
    cYtdLabel = "  " + string(YEAR(tran-date)) + " YTD Total".
    cAsofDateLabel = "As of Date: " + string(tran-date) .

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        IF tb_Period-Detail THEN
            EXPORT STREAM excel DELIMITER ","
                "Account Number"
                "Description"
                cPeriodLabel
                cYtdLabel          
                "Account Type"
                "Financial Report"
                "Opening Balance"
                "Debit Activity"
                "Credit Activity"
                "Ending Balance"
                cAsofDateLabel
                SKIP.
        ELSE
            EXPORT STREAM excel DELIMITER ","
                "Account Number"
                "Description"
                cPeriodLabel
                cYtdLabel          
                "Account Type"
                "Financial Report"
                cAsofDateLabel
                SKIP.
    END. 

    /* create a unique filename ... */
    temp_fid = 
        IF OPSYS EQ 'win32' THEN
        cAuditdir + "\GT\" + "TryB" + substring(STRING(TODAY,"999999"),1,6) + ".csv"
        ELSE
        cAuditdir + "\GT\" + "TryB" + substring(STRING(TODAY,"999999"),1,4) + ".csv".

    RUN pCreateAuditDir.
        
    time_stamp = STRING(TIME, "hh:mmam").

    {sys/form/r-topw3.f}

    ASSIGN 
        facct         = begin_acct-no
        tacct         = END_acct-no
        subac-lvl     = v-sub-acct-lvl
        fsubac        = begin_sub-acct
        tsubac        = END_sub-acct
        break-flag    = tb_sub-acct
        suppress-zero = tb_sup-zero
        dTotYTD       = 0
        dTotPTD       = 0.
       
    FIND FIRST gl-ctrl NO-LOCK
        WHERE gl-ctrl.company EQ company.company
        NO-ERROR.       
       
    EMPTY TEMP-TABLE ttTrialBalance.
    RUN gl\TrialBalance.p(company.company, tran-date, facct, tacct, INPUT-OUTPUT TABLE ttTrialBalance).

    blok:
    DO:
        ASSIGN
            str-tit   = company.name
            str-tit2  = "TRIAL  BALANCE AS OF " + STRING(tran-date,"99/99/99")
            str-tit3  = "Period " + string(tran-period,"99") + " Date Range:" + STRING(dtDateRange1) + "-" + STRING(dtDateRange2)
            v-rep-tot = 0
            {sys/inc/ctrtext.i str-tit  112}
            {sys/inc/ctrtext.i str-tit2 112}
            {sys/inc/ctrtext.i str-tit3 132}.
     
        IF tb_fill-field AND tb_Period-Detail THEN
            ASSIGN
                str-tit4 = "Account Number           Description           Opening Balance   Debit Activity  Credit Activity   Ending Balance                YTD  DB Adjust  CR Adjust  Bal Sheet  Income Stat "
                str-tit5 = "--------------------------------------------- ---------------- ---------------- ---------------- ---------------- ------------------- ---------- ---------- ---------- ----------- ".
        ELSE IF tb_fill-field THEN
                ASSIGN
                    str-tit4 = "Account Number           Description                     PTD                 YTD  DB Adjust  CR Adjust  Bal Sheet  Income Stat "
                    str-tit5 = "--------------------------------------------- --------------- ------------------- ---------- ---------- ---------- ----------- ".
            ELSE IF tb_Period-Detail THEN
                    ASSIGN
                        str-tit4 = "Account Number           Description            Opening Balance   Debit Activity  Credit Activity   Ending Balance                YTD "
                        str-tit5 = "---------------------------------------------  ---------------- ---------------- ---------------- ---------------- -------------------".
                ELSE 
                    ASSIGN
                        str-tit4 = "Account Number           Description                     PTD                 YTD  "
                        str-tit5 = "--------------------------------------------- --------------- ------------------- ".
     
        DISPLAY str-tit3 FORMAT "x(130)" SKIP(1)     
            SKIP str-tit4 SKIP str-tit5 SKIP WITH FRAME r-top STREAM-IO.    
        
        IF break-flag AND subac-lvl NE 1 THEN 
        DO:
            start-lvl = 0.
            DO i = 1 TO (subac-lvl - 1):
                start-lvl = start-lvl + company.acc-dig[i].
            END.
            start-lvl = start-lvl + subac-lvl - 1.
        END.
        IF start-lvl LE 1 THEN start-lvl = 1.

        IF v-download /*and v-first*/  THEN
        DO:         
            ASSIGN 
                str_buffa = ""
                v-first   = NO.        
            OUTPUT stream s-temp TO VALUE(temp_fid). 
            {gl/outstr.i v-hdr 1 70}.
            PUT STREAM s-temp UNFORMATTED str_buffa SKIP.
        END.
    
        FIND LAST period NO-LOCK 
            WHERE period.company EQ cocode
            AND period.pst     LE tran-date
            AND period.pend    GE tran-date
            NO-ERROR.
    
        FOR EACH ttTrialBalance, 
            FIRST account NO-LOCK 
            WHERE account.company EQ company.company
            AND account.actnum EQ ttTrialBalance.accountID        
            BY SUBSTRING(account.actnum,start-lvl) WITH WIDTH 132: 


            subac = IF subac-lvl EQ 1 THEN account.n1 ELSE
                IF subac-lvl EQ 2 THEN account.n2 ELSE
                IF subac-lvl EQ 3 THEN account.n3 ELSE
                IF subac-lvl EQ 4 THEN account.n4 ELSE account.n5.
            IF subac LT fsubac OR subac GT tsubac THEN NEXT.
            cAccountType = "" .
            cFinancialReport = "".
        
            IF account.TYPE EQ "A" THEN
                ASSIGN
                    dAssetAmountPTD  = dAssetAmountPTD + ttTrialBalance.amountPTD 
                    dAssetAmountYTD  = dAssetAmountYTD + ttTrialBalance.amountYTD
                    cAccountType     = "Asset"
                    cFinancialReport = "Balance Sheet".
            ELSE IF account.TYPE EQ "C" THEN
                    ASSIGN
                        dCapitalAmountPTD = dCapitalAmountPTD + ttTrialBalance.amountPTD 
                        dCapitalAmountYTD = dCapitalAmountYTD + ttTrialBalance.amountYTD
                        cAccountType      = "Capital"
                        cFinancialReport  = "Balance Sheet".
                ELSE IF account.TYPE EQ "E" AND account.actnum NE gl-ctrl.contra  THEN
                        ASSIGN
                            dExpenseAmountPTD = dExpenseAmountPTD + ttTrialBalance.amountPTD 
                            dExpenseAmountYTD = dExpenseAmountYTD + ttTrialBalance.amountYTD
                            cAccountType      = "Expense"
                            cFinancialReport  = "Income statement".
                    ELSE IF account.TYPE EQ "L" THEN
                            ASSIGN
                                dLiabilityAmountPTD = dLiabilityAmountPTD + ttTrialBalance.amountPTD 
                                dLiabilityAmountYTD = dLiabilityAmountYTD + ttTrialBalance.amountYTD
                                cAccountType        = "Liability"
                                cFinancialReport    = "Balance Sheet".
                        ELSE IF account.TYPE EQ "R" THEN
                                ASSIGN
                                    dRevenueAmountPTD = dRevenueAmountPTD + ttTrialBalance.amountPTD 
                                    dRevenueAmountYTD = dRevenueAmountYTD + ttTrialBalance.amountYTD
                                    cAccountType      = "Revenue"
                                    cFinancialReport  = "Income statement".
                            ELSE IF account.TYPE EQ "T" THEN
                                    ASSIGN
                                        dTitleAmountPTD = dTitleAmountPTD + ttTrialBalance.amountPTD 
                                        dTitleAmountYTD = dTitleAmountYTD + ttTrialBalance.amountYTD
                                        cAccountType    = "Title".
        
        
            dTotPTD = dTotPTD + ttTrialBalance.amountPTD.
            dTotYTD = dTotYTD + ttTrialBalance.amountYTD.
            ASSIGN          
                dDebitActivity  = 0
                dCreditActivity = 0
                dEndingBalance  = ttTrialBalance.amountYTDOpen .
       
            IF NOT suppress-zero OR ttTrialBalance.amountYTD NE 0 OR ttTrialBalance.amountPTD NE 0 THEN
            DO:
                RUN GL_GetAccountOpenBal(ROWID(account), period.pst , OUTPUT dOpenBalance).
                dEndingBalance  =  dOpenBalance. 
                FOR EACH glhist NO-LOCK 
                    WHERE glhist.company EQ account.company
                    AND glhist.actnum  EQ account.actnum
                    AND glhist.tr-date GE period.pst 
                    AND glhist.tr-date LE tran-date BY glhist.tr-date  :
            
                    IF glhist.tr-amt GT 0 THEN
                        dDebitActivity = dDebitActivity + glhist.tr-amt.
                    ELSE dCreditActivity = dCreditActivity + glhist.tr-amt.
                    dEndingBalance = dEndingBalance + glhist.tr-amt.                 
                END.    
           
                IF tb_fill-field AND tb_Period-Detail THEN
                    PUT SKIP(1)
                        account.actnum + "  " + account.dscr FORMAT "x(45)" SPACE(1)
                        dOpenBalance SPACE(1) dDebitActivity SPACE(1) dCreditActivity SPACE(1) 
                        dEndingBalance  FORMAT "->>>,>>>,>>9.99" SPACE(1)
                        ttTrialBalance.amountYTD FORMAT "->>>,>>>,>>>,>>9.99" SPACE(2)
                        dadj SPACE(1) cadj SPACE(1) bsht SPACE(1) incs SKIP .
           
                ELSE IF tb_fill-field THEN
                        PUT SKIP(1)
                            account.actnum + "  " + account.dscr FORMAT "x(45)" SPACE(1)
                            ttTrialBalance.amountPTD  FORMAT "->>>,>>>,>>9.99" SPACE(1)
                            ttTrialBalance.amountYTD FORMAT "->>>,>>>,>>>,>>9.99" SPACE(1)
                            dadj SPACE(1) cadj SPACE(1) bsht SPACE(1) incs SKIP .
             
                    ELSE IF tb_Period-Detail THEN
                            PUT SKIP(1)
                                account.actnum + "  " + account.dscr FORMAT "x(45)" SPACE(1)             
                                dOpenBalance SPACE(1) dDebitActivity SPACE(1) dCreditActivity SPACE(1) 
                                dEndingBalance  FORMAT "->>>,>>>,>>9.99" SPACE(1)
                                ttTrialBalance.amountYTD FORMAT "->>>,>>>,>>>,>>9.99" SPACE(1)
                                SKIP .
                        ELSE
                            PUT SKIP(1)
                                account.actnum + "  " + account.dscr FORMAT "x(45)" SPACE(1)
                                ttTrialBalance.amountPTD  FORMAT "->>>,>>>,>>9.99" SPACE(1)
                                ttTrialBalance.amountYTD FORMAT "->>>,>>>,>>>,>>9.99"
                                SKIP . 
             
                IF tb_excel THEN
                DO:
                    IF tb_Period-Detail THEN
                        EXPORT STREAM excel DELIMITER ","
                            account.actnum
                            account.dscr
                            ttTrialBalance.amountPTD    
                            ttTrialBalance.amountYTD
                            cAccountType 
                            cFinancialReport
                            dOpenBalance
                            dDebitActivity
                            dCreditActivity
                            dEndingBalance
                            SKIP.
                    ELSE
                        EXPORT STREAM excel DELIMITER ","
                            account.actnum
                            account.dscr
                            ttTrialBalance.amountPTD    
                            ttTrialBalance.amountYTD
                            cAccountType 
                            cFinancialReport
                            SKIP.
                END.
                IF v-download THEN
                DO:
                    ASSIGN 
                        str_buffa = "".
                    ASSIGN 
                        str_buffa = TRIM(account.actnum) + v-comma 
                           + trim(account.dscr)   + v-comma
                           + trim(STRING(ttTrialBalance.amountPTD,'->>>>>>>>9.99')) + v-comma
                           + trim(STRING(ttTrialBalance.amountYTD,'->>>>>>>>9.99'))       + v-comma 
                           + v-comma + v-comma + v-comma.
                    PUT STREAM s-temp UNFORMATTED str_buffa SKIP.
                END.
            END.  
            
        END. /* each account */

        PUT SKIP(1) "===============" TO 61 "===================" TO 81 SKIP
            "TRIAL BALANCE:" AT 10 dTotPTD FORMAT "->>>,>>>,>>9.99" TO 61
            dTotYTD FORMAT "->>>,>>>,>>>,>>9.99" TO 81.
        IF tb_fill-field THEN                          
            PUT
                " " dadj " " cadj " " bsht " " incs .
        PUT SKIP(1).
        
        IF tb_include-summary-total THEN
        DO: 
            PAGE.
            PUT SKIP 
                "===============" TO 61 "===================" TO 81 SKIP
                "Total Assets:" AT 10 dAssetAmountPTD FORMAT "->>>,>>>,>>9.99" TO 61
                dAssetAmountYTD FORMAT "->>>,>>>,>>>,>>9.99" TO 81 SKIP.       
                                      
            PUT SKIP 
                "===============" TO 61 "===================" TO 81 SKIP
                "Total Liabilities:" AT 10 dLiabilityAmountPTD FORMAT "->>>,>>>,>>9.99" TO 61
                dLiabilityAmountYTD FORMAT "->>>,>>>,>>>,>>9.99" TO 81 SKIP.  
                                       
            PUT SKIP 
                "===============" TO 61 "===================" TO 81 SKIP
                "Total Capital:" AT 10 dCapitalAmountPTD FORMAT "->>>,>>>,>>9.99" TO 61
                dCapitalAmountYTD FORMAT "->>>,>>>,>>>,>>9.99" TO 81 SKIP.        
                                
            PUT SKIP 
                "===============" TO 61 "===================" TO 81 SKIP
                "Balance Sheet Total:" AT 10 (dAssetAmountPTD + dLiabilityAmountPTD + dCapitalAmountPTD ) FORMAT "->>>,>>>,>>9.99" TO 61
                (dAssetAmountYTD + dLiabilityAmountYTD + dCapitalAmountYTD ) FORMAT "->>>,>>>,>>>,>>9.99" TO 81 SKIP(1).  
                                         
            PUT SKIP 
                "===============" TO 61 "===================" TO 81 SKIP
                "Total Revenue:" AT 10 dRevenueAmountPTD FORMAT "->>>,>>>,>>9.99" TO 61
                dRevenueAmountYTD FORMAT "->>>,>>>,>>>,>>9.99" TO 81 SKIP. 
                                       
            PUT SKIP 
                "===============" TO 61 "===================" TO 81 SKIP
                "Total Expenses:" AT 10 dExpenseAmountPTD FORMAT "->>>,>>>,>>9.99" TO 61
                dExpenseAmountYTD FORMAT "->>>,>>>,>>>,>>9.99" TO 81 SKIP.  
                                       
            PUT SKIP 
                "===============" TO 61 "===================" TO 81 SKIP
                "Net Income:" AT 10 (-1 * dRevenueAmountPTD - dExpenseAmountPTD) FORMAT "->>>,>>>,>>9.99" TO 61
                (-1 * dRevenueAmountYTD - dExpenseAmountYTD) FORMAT "->>>,>>>,>>>,>>9.99" TO 81 SKIP.                                                                  
        END.                                
                                   
        IF tb_excel THEN 
        DO: 
            IF tb_include-summary-total THEN
            DO: 
                EXPORT STREAM excel DELIMITER ","
                    "Total Assets:"
                    ""
                    dAssetAmountPTD
                    dAssetAmountYTD SKIP .
                EXPORT STREAM excel DELIMITER ","
                    "Total Liabilities:"
                    ""
                    dLiabilityAmountPTD
                    dLiabilityAmountYTD SKIP.
                EXPORT STREAM excel DELIMITER ","
                    "Total Capital:"
                    ""
                    dCapitalAmountPTD
                    dCapitalAmountYTD SKIP.
            
                EXPORT STREAM excel DELIMITER ","
                    "Balance Sheet Total:"
                    ""
                    (dAssetAmountPTD + dLiabilityAmountPTD + dCapitalAmountPTD  )
                    (dAssetAmountYTD + dLiabilityAmountYTD + dCapitalAmountYTD  ) SKIP(1)  .
                EXPORT STREAM excel DELIMITER ","
                    "Total Revenue:"
                    ""
                    dRevenueAmountPTD
                    dRevenueAmountYTD SKIP.
                EXPORT STREAM excel DELIMITER ","
                    "Total Expenses:"
                    ""
                    dExpenseAmountPTD
                    dExpenseAmountYTD                     
                    SKIP.
                EXPORT STREAM excel DELIMITER ","
                    "Net Income:"
                    ""
                    (-1 * dRevenueAmountPTD - dExpenseAmountPTD)
                    (-1 * dRevenueAmountYTD - dExpenseAmountYTD) SKIP.
            END.
        END.   
   
        IF tb_show-all-account EQ TRUE  THEN
        DO:   
            IF dTotYTD EQ 0 THEN MESSAGE "TRIAL BALANCE IN BALANCE" VIEW-AS ALERT-BOX.
            ELSE              MESSAGE "TRIAL BALANCE NOT IN BALANCE BY " dTotYTD VIEW-AS ALERT-BOX.
        END.
        /*if v-download then  */
        OUTPUT stream s-temp close. 
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
    END.
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-detail C-Win 
PROCEDURE run-report-detail :
    DEFINE VARIABLE save_id           AS RECID     NO-UNDO.
    DEFINE VARIABLE time_stamp        AS ch        NO-UNDO.
    DEFINE VARIABLE subac             AS INTEGER   FORMAT ">>>>>>>>9" NO-UNDO.
    DEFINE VARIABLE subac-lvl         AS INTEGER   FORMAT "9" NO-UNDO.
    DEFINE VARIABLE fsubac            AS INTEGER   FORMAT ">>>>>>>>9" INIT 0 NO-UNDO.
    DEFINE VARIABLE tsubac            AS INTEGER   FORMAT ">>>>>>>>9" INIT 999999999 NO-UNDO.
    DEFINE VARIABLE aclevel           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cyr               AS DECIMAL   FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE tcyr              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dadj              AS CHARACTER LABEL "DB Adjust" FORMAT "x(10)" INIT "__________" NO-UNDO.
    DEFINE VARIABLE cadj              AS CHARACTER LABEL "CR Adjust" FORMAT "x(10)" INIT "__________" NO-UNDO.
    DEFINE VARIABLE bsht              AS CHARACTER LABEL "Bal Sheet" FORMAT "x(10)" INIT "__________" NO-UNDO.
    DEFINE VARIABLE incs              AS CHARACTER LABEL "Income Stat" FORMAT "x(11)" INIT "___________" NO-UNDO.
    DEFINE VARIABLE v-rep-tot         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE vyear             LIKE period.yr NO-UNDO.
    DEFINE VARIABLE dtPeriodStart     LIKE glhist.tr-date NO-UNDO.
    DEFINE VARIABLE v-fisc-yr         LIKE period.yr NO-UNDO.

    DEFINE VARIABLE tacct             LIKE glhist.actnum LABEL "      To Account Number" NO-UNDO.
    DEFINE VARIABLE facct             LIKE glhist.actnum LABEL "    From Account Number" NO-UNDO.
    DEFINE VARIABLE ptd-value         AS DECIMAL   FORMAT "->>>,>>>,>>9.99" INIT 0 NO-UNDO.
    DEFINE VARIABLE tot-ptd           AS DECIMAL   FORMAT "->>>,>>>,>>9.99" INIT 0 NO-UNDO.
    DEFINE VARIABLE suppress-zero     AS LOGICAL   NO-UNDO INIT TRUE
        LABEL "Suppress Zero Balances?".
    DEFINE VARIABLE break-flag        AS LOG       INIT NO NO-UNDO.
    DEFINE VARIABLE start-lvl         AS INTEGER   INIT 0 NO-UNDO.
    DEFINE VARIABLE temp_fid          AS CHARACTER NO-UNDO.

    DEFINE VARIABLE str_buffa         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-first           AS LOG       INIT YES NO-UNDO.
    DEFINE VARIABLE v-hdr             AS CHARACTER INITIAL
        "Account#,Description,PTD,YTD,DB Adjust,CR Adjust,Bal Sheet,Income Stat" NO-UNDO.
    DEFINE VARIABLE v-comma           AS CHARACTER FORMAT "x" INITIAL "," NO-UNDO.

    DEFINE VARIABLE cPeriodLabel      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cYtdLabel         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAsofDateLabel    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dDebitAmt         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCreditAmt        AS DECIMAL   NO-UNDO.  
    DEFINE VARIABLE dPeriodTotal      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dYtdAmount        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotalOpenBalance AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotalYtdAmount   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotYTD           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lRecordExist      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE str-tit4          AS CHARACTER FORMAT "x(240)" NO-UNDO.
    DEFINE VARIABLE str-tit5          AS CHARACTER FORMAT "x(240)" NO-UNDO.
    DEFINE VARIABLE str-line          AS CHARACTER FORMAT "x(240)" NO-UNDO.
    DEFINE VARIABLE dTotalDebitAmt    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotalCreditAmt   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOpenBalance      AS DECIMAL   FORMAT "->>>>,>>>,>>9.99" NO-UNDO.
       


    {sys/inc/print1.i}
    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE("general").
 
    cPeriodLabel = " Period " + string(MONTH(tran-date),"99") + " Total".
    cYtdLabel = "  " + string(YEAR(tran-date)) + " YTD Total".
    cAsofDateLabel = "As of Date: " + string(tran-date) .
    str-line = FILL(" ",45) + "  " + "---------------" + " " + FILL(" ",51) + "--------------" + " " + "--------------" + " " + "--------------" + FILL(" ",57) + "-----------------" .                                                 

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        EXPORT STREAM excel DELIMITER ","
            "Account Number"
            "Description"
            "Opening Balance"
            "Date"
            "Ref#"
            "Description"
            "Debit Amt"
            "Credit Amt"
            cPeriodLabel
            "Document id"
            "Source Date"
            "Run Number"
            cYtdLabel                
            SKIP.
    END.      

    /* create a unique filename ... */
    temp_fid = 
        IF OPSYS EQ 'win32' THEN
        cAuditdir + "\GT\" + "TryB" + substring(STRING(TODAY,"999999"),1,6) + ".csv"
        ELSE
        cAuditdir + "\GT\" + "TryB" + substring(STRING(TODAY,"999999"),1,4) + ".csv".

    RUN pCreateAuditDir.
        
    time_stamp = STRING(TIME, "hh:mmam").

    {sys/form/r-topw3.f}

    ASSIGN 
        facct         = begin_acct-no
        tacct         = END_acct-no
        subac-lvl     = v-sub-acct-lvl
        fsubac        = begin_sub-acct
        tsubac        = END_sub-acct
        break-flag    = tb_sub-acct
        suppress-zero = tb_sup-zero
        dTotYTD       = 0
        .
       
    EMPTY TEMP-TABLE ttTrialBalance.
    RUN gl\TrialBalance.p(company.company, tran-date, facct, tacct, INPUT-OUTPUT TABLE ttTrialBalance).

    blok:
    DO:
        ASSIGN
            str-tit   = company.name
            str-tit2  = "TRIAL  BALANCE AS OF " + STRING(tran-date,"99/99/99")
            str-tit3  = "Period " + string(tran-period,"99") + " Date Range:" + STRING(dtDateRange1) + "-" + STRING(dtDateRange2)
            v-rep-tot = 0
            {sys/inc/ctrtext.i str-tit  112}
            {sys/inc/ctrtext.i str-tit2 112}
            {sys/inc/ctrtext.i str-tit3 132}.
         
        str-tit4 = "Account Number           Description          Opening Balance  Date       Ref      Description                    Debit Amt      Credit Amt     PTD Total      Document ID                      Source Date Run Number YTD".
        str-tit5 = "--------------------------------------------- ---------------- ---------- -------- ------------------------------ -------------- -------------- -------------- -------------------------------- ----------- ---------- -----------------".
        DISPLAY str-tit3 FORMAT "x(130)" SKIP(1)     
            SKIP str-tit4 SKIP str-tit5 SKIP WITH FRAME r-top STREAM-IO.

        IF break-flag AND subac-lvl NE 1 THEN 
        DO:
            start-lvl = 0.
            DO i = 1 TO (subac-lvl - 1):
                start-lvl = start-lvl + company.acc-dig[i].
            END.
            start-lvl = start-lvl + subac-lvl - 1.
        END.
        IF start-lvl LE 1 THEN start-lvl = 1.

        IF v-download /*and v-first*/  THEN
        DO:         
            ASSIGN 
                str_buffa = ""
                v-first   = NO.        
            OUTPUT stream s-temp TO VALUE(temp_fid). 
            {gl/outstr.i v-hdr 1 70}.
            PUT STREAM s-temp UNFORMATTED str_buffa SKIP.
        END.
    
        FIND LAST period NO-LOCK 
            WHERE period.company EQ cocode
            AND period.pst     LE tran-date
            AND period.pend    GE tran-date
            NO-ERROR.
    
        FOR EACH ttTrialBalance, 
            FIRST account NO-LOCK 
            WHERE account.company EQ company.company
            AND account.actnum EQ ttTrialBalance.accountID        
            BY SUBSTRING(account.actnum,start-lvl) WITH WIDTH 132: 


            subac = IF subac-lvl EQ 1 THEN account.n1 ELSE
                IF subac-lvl EQ 2 THEN account.n2 ELSE
                IF subac-lvl EQ 3 THEN account.n3 ELSE
                IF subac-lvl EQ 4 THEN account.n4 ELSE account.n5.
            IF subac LT fsubac OR subac GT tsubac THEN NEXT.
                        
            dTotYTD = dTotYTD + ttTrialBalance.amountYTD.
            dTotalDebitAmt = 0 .
            dTotalCreditAmt = 0.
        
            IF NOT suppress-zero OR ttTrialBalance.amountYTD NE 0 OR ttTrialBalance.amountPTD NE 0 THEN
            DO:
                dTotalYtdAmount = dTotalYtdAmount + ttTrialBalance.amountYTDOpen .           
                lRecordExist = NO.
           
                RUN GL_GetAccountOpenBal(ROWID(account), period.pst , OUTPUT dOpenBalance).
                IF dOpenBalance EQ ? THEN dOpenBalance = 0.
                dYtdAmount = dOpenBalance. 
                dTotalOpenBalance = dTotalOpenBalance + dOpenBalance.
                FOR EACH glhist NO-LOCK 
                    WHERE glhist.company EQ account.company
                    AND glhist.actnum  EQ account.actnum
                    AND glhist.tr-date GE period.pst 
                    AND glhist.tr-date LE tran-date BY glhist.tr-date  :
                    dDebitAmt = 0.
                    dCreditAmt = 0.            
                    IF glhist.tr-amt GT 0 THEN
                        ASSIGN
                            dDebitAmt      = glhist.tr-amt
                            dTotalDebitAmt = dTotalDebitAmt + glhist.tr-amt .
                    ELSE
                        ASSIGN
                            dCreditAmt      = glhist.tr-amt
                            dTotalCreditAmt = dTotalCreditAmt + glhist.tr-amt .
            
                    dPeriodTotal = dPeriodTotal + glhist.tr-amt. 
                    dYtdAmount = dYtdAmount +  glhist.tr-amt.
                    IF glhist.tr-amt NE ? THEN
                        dTotalOpenBalance = dTotalOpenBalance + glhist.tr-amt.
                    lRecordExist = YES.
            
                    PUT 
                        account.actnum + "  " + account.dscr FORMAT "x(45)" SPACE(2)                
                        SPACE(16) 
                        glhist.tr-date FORMAT "99/99/9999" SPACE(1)
                        glhist.jrnl FORMAT "x(8)" SPACE(1)
                        glhist.tr-dscr FORMAT "x(30)" SPACE(1)
                        dDebitAmt FORMAT "->>,>>>,>>9.99" SPACE(1)
                        dCreditAmt FORMAT "->>,>>>,>>9.99" SPACE(1)
                        dPeriodTotal FORMAT "->>,>>>,>>9.99"  SPACE(1)
                        glhist.documentID FORMAT "x(32)" SPACE(1)
                        glhist.sourceDate FORMAT "99/99/9999" SPACE(2)
                        glhist.tr-num FORMAT ">>>>>>>>>>" SPACE(1)
                        dYtdAmount FORMAT "->,>>>,>>>,>>9.99" SPACE(1)
                        SKIP.                
                          
                    IF tb_excel THEN 
                        EXPORT STREAM excel DELIMITER ","
                            account.actnum
                            account.dscr
                            dOpenBalance    
                            glhist.tr-date
                            glhist.jrnl
                            glhist.tr-dscr
                            dDebitAmt
                            dCreditAmt
                            dPeriodTotal
                            glhist.documentID
                            glhist.sourceDate
                            glhist.tr-num
                            dYtdAmount
                            dYtdAmount
                            SKIP.                     
                END.           
           
                IF lRecordExist THEN
                DO:
                    PUT SKIP str-line FORMAT "x(240)" SKIP.
                    PUT  "                              Account Total:" FORMAT "x(45)" SPACE(2)
                        dOpenBalance  FORMAT "->>>,>>>,>>9.99" SPACE(1)
                        SPACE(11) 
                        SPACE(9) 
                        SPACE(31) 
                        dTotalDebitAmt FORMAT "->>,>>>,>>9.99" SPACE(1)
                        dTotalCreditAmt FORMAT "->>,>>>,>>9.99" SPACE(1)
                        dPeriodTotal FORMAT "->>,>>>,>>9.99"  SPACE(1)
                        SPACE(33) 
                        SPACE(12)
                        SPACE(11) 
                        dYtdAmount FORMAT "->,>>>,>>>,>>9.99" SPACE(1)
                        SKIP(1).
                 
                    IF tb_excel THEN 
                        EXPORT STREAM excel DELIMITER ","
                            "Account Total"
                            ""
                            dOpenBalance
                            ""
                            ""
                            ""
                            dTotalDebitAmt
                            dTotalCreditAmt
                            dPeriodTotal
                            ""
                            ""
                            ""  
                            dYtdAmount
                            SKIP(1).    
                END.
           
                IF NOT lRecordExist THEN
                DO:  
                
                    PUT SKIP
                        account.actnum + "  " + account.dscr FORMAT "x(45)" SPACE(2)
                        dOpenBalance FORMAT "->>>,>>>,>>9.99" SPACE(1)
                        "" FORMAT "x(152)" 
                        dYtdAmount FORMAT "->,>>>,>>>,>>9.99" SKIP .
                    PUT str-line FORMAT "x(240)" SKIP.
                    PUT  "                              Account Total:" FORMAT "x(45)" SPACE(2)
                        ttTrialBalance.amountYTDOpen  FORMAT "->>>,>>>,>>9.99"
                        "" FORMAT "x(153)" 
                        dYtdAmount FORMAT "->,>>>,>>>,>>9.99" SKIP(1).
                
                          
                    IF tb_excel THEN 
                        EXPORT STREAM excel DELIMITER ","
                            account.actnum
                            account.dscr
                            dOpenBalance
                            ""
                            ""
                            ""
                            ""
                            ""
                            ""
                            ""
                            ""
                            ""
                            dYtdAmount
                            SKIP(1). 
                    
                    IF tb_excel THEN 
                        EXPORT STREAM excel DELIMITER ","
                            "Account Total"
                            ""
                            dOpenBalance
                            ""
                            ""
                            ""
                            ""
                            ""
                            ""
                            ""
                            ""
                            ""
                            dYtdAmount
                            SKIP(1).                
                END.

                IF v-download THEN
                DO:
                    ASSIGN 
                        str_buffa = "".
                    ASSIGN 
                        str_buffa = TRIM(account.actnum) + v-comma 
                           + trim(account.dscr)   + v-comma
                           + trim(STRING(ttTrialBalance.amountPTD,'->>>>>>>>9.99')) + v-comma
                           + trim(STRING(ttTrialBalance.amountYTD,'->>>>>>>>9.99'))       + v-comma 
                           + v-comma + v-comma + v-comma.
                    PUT STREAM s-temp UNFORMATTED str_buffa SKIP.
                END.
            END.  
            
        END. /* each account */

        PUT SKIP(1) "===================" TO 61  SKIP
            "Opening Balance Total:" AT 10 dTotalOpenBalance FORMAT "->>>,>>>,>>>,>>9.99" TO 61 SKIP
            "====================" TO 61 SKIP
            "YTD Balance Total:" AT 10  dTotalYtdAmount FORMAT "->>>,>>>,>>>,>>9.99" TO 61
            SKIP(1).       
                                   
        IF tb_excel THEN 
        DO:          
            EXPORT STREAM excel DELIMITER ","
                SKIP
                "Opening Balance Total:"
                ""
                dTotalOpenBalance SKIP .
            EXPORT STREAM excel DELIMITER ","
                SKIP
                "Ytd Balance Total:"
                ""
                dTotalYtdAmount SKIP .                     
                                                     
        END.             
        IF tb_show-all-account EQ TRUE THEN
        DO:   
            IF dTotYTD EQ 0 THEN MESSAGE "TRIAL BALANCE IN BALANCE" VIEW-AS ALERT-BOX.
            ELSE              MESSAGE "TRIAL BALANCE NOT IN BALANCE BY " dTotYTD VIEW-AS ALERT-BOX.
        END.     
        OUTPUT stream s-temp close. 
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
    END.
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    SESSION:SET-WAIT-STATE("").

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
            fi_file:SCREEN-VALUE = "c:\tmp\TrialBalance.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

