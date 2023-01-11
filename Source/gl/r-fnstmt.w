&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-fnstmt.w

  Description: GL Financial Statements

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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

{gl/gl-fs.i NEW}

FORMAT
    v-hdr[1] AT 1 FORMAT "x(200)" SKIP
    v-hdr[2] AT 1 FORMAT "x(200)" SKIP
    v-hdr[3] AT 1 FORMAT "x(200)" SKIP
    v-hdr[4] AT 1 FORMAT "x(200)" SKIP
    v-hdr[5] AT 1 FORMAT "x(200)" SKIP(2)
    r-top1   AT 1 FORMAT "x(200)"
    r-top2   AT 1 FORMAT "x(200)"
    WITH FRAME rpt-top PAGE-TOP NO-BOX NO-LABELS WIDTH 200 STREAM-IO.

DEFINE BUFFER bf-rpt FOR gl-rpt.
DEFINE VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName      AS CHARACTER NO-UNDO.
DEFINE NEW SHARED STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-10 RECT-6 RECT-7 v-dscr tran-date ~
select-rpt tb_pre tb_paid tb_supp tb_acct# tb_round tb_mul-comp ~
lv-company-list sub-acct-lvl begin_sub-acct end_sub-acct rd-dest fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS v-dscr tran-date select-rpt tran-period ~
tb_pre tb_paid tb_supp tb_acct# tb_round tb_mul-comp lv-company-list ~
sub-acct-lvl begin_sub-acct end_sub-acct rd-dest fi_file tb_OpenCSV ~
tbAutoClose 

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

DEFINE VARIABLE begin_sub-acct  AS INTEGER   FORMAT "->>>>>>>>9" INITIAL 0 
    LABEL "Beginning Subacct" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1.

DEFINE VARIABLE end_sub-acct    AS INTEGER   FORMAT "->>>>>>>>9" INITIAL 999999999 
    LABEL "Ending Subacct" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1.

DEFINE VARIABLE fi_file         AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\GLFinancial.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lines-per-page  AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-company-list AS CHARACTER FORMAT "X(100)" 
    LABEL "List" 
    VIEW-AS FILL-IN 
    SIZE 52 BY 1.

DEFINE VARIABLE lv-font-name    AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no      AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE rpt-codes       AS CHARACTER FORMAT "X(256)":U 
    LABEL "Reports" 
    VIEW-AS FILL-IN 
    SIZE 1 BY 1 NO-UNDO.

DEFINE VARIABLE sub-acct-lvl    AS INTEGER   FORMAT ">>":U INITIAL 0 
    LABEL "Sub Account Level" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date       AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Transaction Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period     AS INTEGER   FORMAT ">>":U INITIAL 0 
    LABEL "Period" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-dscr          AS CHARACTER FORMAT "x(50)" 
    LABEL "Report Heading" 
    VIEW-AS FILL-IN 
    SIZE 57 BY 1.

DEFINE VARIABLE lv-ornt         AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest         AS INTEGER   INITIAL 1 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 18 BY 4.76 NO-UNDO.

DEFINE RECTANGLE RECT-10
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 3.57.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 95 BY 5.24.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 95 BY 14.52.

DEFINE VARIABLE select-rpt   AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE 
    SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
    SIZE 49 BY 5.71
    FONT 6 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_acct#     AS LOGICAL   INITIAL NO 
    LABEL "Print GL Acct#?" 
    VIEW-AS TOGGLE-BOX
    SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .95 NO-UNDO.

DEFINE VARIABLE tb_mul-comp  AS LOGICAL   INITIAL NO 
    LABEL "Multiple Companies?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tb_paid      AS LOGICAL   INITIAL YES 
    LABEL "Skip Zero Lines?" 
    VIEW-AS TOGGLE-BOX
    SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_pre       AS LOGICAL   INITIAL YES 
    LABEL "Pre Close Period?" 
    VIEW-AS TOGGLE-BOX
    SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_round     AS LOGICAL   INITIAL NO 
    LABEL "Suppress Decimals?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_supp      AS LOGICAL   INITIAL YES 
    LABEL "Suppress Zero Fields?" 
    VIEW-AS TOGGLE-BOX
    SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    rpt-codes AT ROW 1.76 COL 88 COLON-ALIGNED
    v-dscr AT ROW 2.71 COL 20 COLON-ALIGNED HELP
    "Report Description"
    tran-date AT ROW 3.76 COL 20 COLON-ALIGNED
    select-rpt AT ROW 4.57 COL 46.6 HELP
    "Select Reports you wish to print." NO-LABELS
    tran-period AT ROW 4.81 COL 20 COLON-ALIGNED
    tb_pre AT ROW 6 COL 5
    tb_paid AT ROW 6.95 COL 5
    tb_supp AT ROW 7.91 COL 5
    tb_acct# AT ROW 8.86 COL 5
    tb_round AT ROW 9.81 COL 5
    tb_mul-comp AT ROW 10.76 COL 5
    lv-company-list AT ROW 10.76 COL 33 COLON-ALIGNED HELP
    "List"
    sub-acct-lvl AT ROW 12.76 COL 31 COLON-ALIGNED
    begin_sub-acct AT ROW 13.95 COL 31 COLON-ALIGNED
    end_sub-acct AT ROW 13.95 COL 66 COLON-ALIGNED
    rd-dest AT ROW 16.71 COL 5 NO-LABELS
    lv-font-no AT ROW 16.81 COL 35.4 COLON-ALIGNED
    lines-per-page AT ROW 16.81 COL 90.2 COLON-ALIGNED
    lv-ornt AT ROW 16.86 COL 46 NO-LABELS
    lv-font-name AT ROW 17.95 COL 32.2 COLON-ALIGNED NO-LABELS
    tb_excel AT ROW 19.1 COL 95 RIGHT-ALIGNED
    td-show-parm AT ROW 19.38 COL 31.4
    fi_file AT ROW 20.38 COL 29.4 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 20.43 COL 95.4 RIGHT-ALIGNED
    tbAutoClose AT ROW 21.81 COL 31.6 WIDGET-ID 64
    btn-ok AT ROW 22.76 COL 31.4
    btn-cancel AT ROW 22.76 COL 54.6
    " SORT OPTIONS" VIEW-AS TEXT
    SIZE 20 BY .62 AT ROW 11.76 COL 35.8
    FONT 6
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 16.05 COL 4
    "Select/Deselect Reports" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 3.86 COL 51
    FONT 6
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.05 COL 4
    RECT-10 AT ROW 12.14 COL 5
    RECT-6 AT ROW 16.48 COL 3
    RECT-7 AT ROW 1.52 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 98.8 BY 24.38
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
        TITLE              = "GL Financial Statements"
        HEIGHT             = 23.24
        WIDTH              = 98.8
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
    begin_sub-acct:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_sub-acct:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lines-per-page:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    lv-company-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

/* SETTINGS FOR FILL-IN rpt-codes IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    rpt-codes:HIDDEN IN FRAME FRAME-A       = TRUE
    rpt-codes:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    sub-acct-lvl:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_acct#:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_mul-comp:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_paid:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_pre:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_round:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_supp:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

ASSIGN 
    v-dscr:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* GL Financial Statements */
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
ON WINDOW-CLOSE OF C-Win /* GL Financial Statements */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON HELP OF FRAME FRAME-A
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lk-recid AS RECID     NO-UNDO.

        CASE FOCUS:NAME :
            WHEN "lv-rpt" THEN 
                DO:
                    RUN gl/l-glrpt (g_company,FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT lk-recid).
                    FIND gl-rpt WHERE RECID(gl-rpt) EQ lk-recid NO-LOCK NO-ERROR.
                    IF AVAILABLE  gl-rpt AND gl-rpt.rpt NE FOCUS:SCREEN-VALUE THEN 
                    DO:              
                        FOCUS:SCREEN-VALUE = gl-rpt.rpt.
                        RUN new-rpt.
                    END.
                END.
        END CASE.

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
        /*RUN valid-rpt NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.*/

        RUN check-date NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-sub-lvl NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN /*rd-dest
           tran-date
           tran-period
           tb_pre
           tb_paid
           tb_mul-comp
           lv-company-list
           sub-acct-lvl
           begin_sub-acct
           END_sub-acct*/
                {&displayed-objects}
                udate        = tran-date
                uperiod      = tran-period
                pre-close    = tb_pre
                skip_zero    = tb_paid
                supp_zero    = tb_supp
                consolidate  = tb_mul-comp
                company-list = lv-company-list
                subac-lvl    = sub-acct-lvl
                fsubac       = begin_sub-acct
                tsubac       = END_sub-acct
                ll-acct#     = tb_acct#.
        END.
        IF rd-dest = 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.

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
                    ELSE DO:
                        OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                    END.
                END. /* WHEN 3 THEN DO: */
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &begin_cust=tran-date
                            &END_cust=tran-date
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:title 
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
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


&Scoped-define SELF-NAME lv-ornt
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


&Scoped-define SELF-NAME rpt-codes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rpt-codes C-Win
ON LEAVE OF rpt-codes IN FRAME FRAME-A /* Reports */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME select-rpt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL select-rpt C-Win
ON VALUE-CHANGED OF select-rpt IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sub-acct-lvl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sub-acct-lvl C-Win
ON LEAVE OF sub-acct-lvl IN FRAME FRAME-A /* Sub Account Level */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-sub-lvl NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
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


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Transaction Date */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN check-date NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE lv-rpt-list AS CHARACTER NO-UNDO.

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

    TRAN-date = TODAY.

    RUN init-proc.
  
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "GW1" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
        
    RUN check-date. 

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        APPLY "entry" TO v-dscr.
    END.
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

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST period                   
            WHERE period.company EQ cocode
            AND period.pst     LE DATE(tran-date:SCREEN-VALUE)
            AND period.pend    GE DATE(tran-date:SCREEN-VALUE)
            NO-LOCK NO-ERROR.
        IF AVAILABLE period THEN tran-period:SCREEN-VALUE = STRING(period.pnum).

        ELSE 
        DO:
            MESSAGE "No Defined Period Exists for" DATE(tran-date:SCREEN-VALUE) VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO tran-date.
            RETURN ERROR.
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
    DISPLAY v-dscr tran-date select-rpt tran-period tb_pre tb_paid tb_supp 
        tb_acct# tb_round tb_mul-comp lv-company-list sub-acct-lvl 
        begin_sub-acct end_sub-acct rd-dest fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-10 RECT-6 RECT-7 v-dscr tran-date select-rpt tb_pre tb_paid 
        tb_supp tb_acct# tb_round tb_mul-comp lv-company-list sub-acct-lvl 
        begin_sub-acct end_sub-acct rd-dest fi_file tb_OpenCSV tbAutoClose 
        btn-ok btn-cancel 
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

    DO WITH FRAME {&FRAME-NAME}:
        FOR EACH gl-rpt
            WHERE gl-rpt.company EQ cocode
            AND gl-rpt.line    EQ 0
            NO-LOCK:

            lv-rpt-list = lv-rpt-list + STRING(gl-rpt.rpt,"x(6)") + " " + gl-rpt.dscr + ",".

            IF TRIM(v-dscr) EQ "" THEN 
            DO:
                FIND FIRST bf-rpt
                    WHERE bf-rpt.company    EQ cocode
                    AND bf-rpt.rpt        EQ gl-rpt.rpt
                    AND bf-rpt.line       EQ 7
                    AND TRIM(bf-rpt.dscr) NE ""
                    NO-LOCK NO-ERROR.
                IF AVAILABLE bf-rpt THEN v-dscr = bf-rpt.dscr.
            END.
        END.
        IF SUBSTR(lv-rpt-list,LENGTH(TRIM(lv-rpt-list)),1) EQ "," THEN
            SUBSTR(lv-rpt-list,LENGTH(TRIM(lv-rpt-list)),1) = "".

        select-rpt:LIST-ITEMS = lv-rpt-list.

        FOR EACH company:
            company-list = company-list + company.company + ",".
        END.
        IF company-list NE "" THEN
            SUBSTR(company-list,LENGTH(TRIM(company-list)),1) = "".

        FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
        IF AVAILABLE company THEN 
        DO:
            aclevel = company.acc-level.
            IF company-list EQ "" THEN company-list = company.company.
        END.  
        ELSE aclevel = 1.

        ASSIGN
            sub-acct-lvl    = aclevel
            lv-company-list = company-list.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-rpt C-Win 
PROCEDURE new-rpt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*DO WITH FRAME {&FRAME-NAME}:
  FIND gl-rpt
      WHERE gl-rpt.company EQ cocode
        AND gl-rpt.rpt     BEGINS lv-rpt:SCREEN-VALUE 
        AND gl-rpt.line    EQ 7 
      NO-LOCK NO-ERROR.
  IF AVAIL gl-rpt THEN DO: 
    ASSIGN
     lv-rpt:SCREEN-VALUE = TRIM(CAPS(gl-rpt.rpt))
     v-dscr:SCREEN-VALUE = gl-rpt.dscr.

    DO li = 1 TO LENGTH(lv-rpt:SCREEN-VALUE):
      APPLY "cursor-right" TO lv-rpt.
    END.
  END.
END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ouput-to-port C-Win 
PROCEDURE ouput-to-port :
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
    
         IF NOT OKpressed THEN  RETURN NO-APPLY.  */

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
    RUN custom/prntproc.p (list-name,INT(lv-font-no), lv-ornt). /* open file-name, title */ 
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
    RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no), lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    DEFINE VARIABLE lv-company-save AS CHARACTER NO-UNDO.
    DEFINE VARIABLE li              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-rcode        AS CHARACTER FORMAT "x(47)".
    DEFINE VARIABLE lv-pct-hdr      AS CHARACTER INIT "  % Sales" NO-UNDO.
    DEFINE VARIABLE excelheader     AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        DO li = 1 TO select-rpt:NUM-ITEMS:
            IF select-rpt:IS-SELECTED(li) THEN
                lv-rcode = lv-rcode + TRIM(SUBSTR(select-rpt:ENTRY(li),1,6)) + ",".
        END.

        IF LENGTH(lv-rcode) > 0 AND
            SUBSTR(lv-rcode,LENGTH(TRIM(lv-rcode)),1) EQ "," THEN
            SUBSTR(lv-rcode,LENGTH(TRIM(lv-rcode)),1) = "".

        rpt-codes = lv-rcode.

        DO li = 1 TO LENGTH(rpt-codes):
            IF SUBSTR(rpt-codes,li,1) EQ "," THEN SUBSTR(rpt-codes,li,1) = " ".
        END.

        lv-company-save = company-list.

        DO li = 1 TO LENGTH(lv-company-list):
            IF SUBSTR(lv-company-list,li,1) EQ "," THEN
                SUBSTR(lv-company-list,li,1) = " ".
        END.

        ASSIGN
            lv-company-list:SCREEN-VALUE = lv-company-list
            lv-company-list
            rpt-codes:SCREEN-VALUE       = rpt-codes
            rpt-codes.
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    IF tb_excel THEN
        OUTPUT STREAM excel TO VALUE(cFileName).

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            lv-company-list:SCREEN-VALUE = lv-company-save
            lv-company-list.
    END. 

    SESSION:SET-WAIT-STATE("general").

    FOR EACH gl-rpt
        WHERE gl-rpt.company EQ cocode
        AND LOOKUP(gl-rpt.rpt,lv-rcode) GT 0
        AND gl-rpt.line    EQ 0
        NO-LOCK
        BY gl-rpt.rpt:

        DO TRANSACTION:
            FIND FIRST bf-rpt
                WHERE bf-rpt.company EQ cocode
                AND bf-rpt.rpt     EQ gl-rpt.rpt
                AND bf-rpt.line    EQ 7
                NO-ERROR.               
            IF AVAILABLE bf-rpt THEN
            DO:
                bf-rpt.dscr = v-dscr.
                FIND CURRENT bf-rpt NO-LOCK.
            END.

        END.

        ASSIGN
            v-d-wid = 0
            r-top1  = ""
            r-top2  = ""
            r-top3  = ""
            r-top4  = ""
            v-hdr   = ""
            v-per   = FALSE.

        fil_id = RECID(gl-rpt).
        RUN gl/gl-rptg.p (INPUT fil_id, INPUT NO).

        IF NOT all-per AND tb_round THEN 
        DO:
            ASSIGN
                tot-format  = "->>>,>>>,>>9"
                pct-format  = "->>,>>>,>>9%"
                pct-formats = "->>>9%"
                sul-format  = " -----------"
                sul-formats = " -----"
                dul-format  = " ==========="
                dul-formats = " ====="
                lv-pct-hdr  = " % Sls".

            DO i = 1 TO v-no-col:
                v-ch[i] = SUBSTR(TRIM(v-ch[i]),1,11).
            END.
        END.

        IF consolidate AND index(company-list,",") GT 0 THEN
            v-hdr[5] = v-hdr[5] + (IF v-hdr[5] EQ "" THEN "" ELSE " - ") +
                "Companies: " + trim(company-list).
  
        IF v-hdr[2] EQ "" THEN
            ASSIGN  v-hdr[2] = "Printed: " + STRING(TODAY,"99/99/9999") + " @ " + STRING(TIME,"HH:MM").
    
        /* form headers */
        DO i = 1 TO 5:
            ASSIGN 
                v-hdr[i] = REPLACE(v-hdr[i],"<company>",company-list)
                v-hdr[i] = REPLACE(v-hdr[i],"<period>",tran-period:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                tot2[i]  = LENGTH(v-hdr[i])
                tot2[i]  = int(v-col-used - int(tot2[i])) / 2
                v-hdr[i] = FILL(" ", int (tot2[i])) + v-hdr[i].
        END.

        r-top1 = FILL(" ",v-d-wid).
        r-top2 = r-top1.
        DO i = 1 TO v-no-col:
            ASSIGN 
                r-top3 = FILL(" ",((IF all-per THEN 10 ELSE IF tb_round THEN 11 ELSE 14) - LENGTH(v-ch[i]))) +
                    v-ch[i] +
                    IF all-per THEN " " ELSE (IF v-per[i] THEN lv-pct-hdr ELSE "")
                r-top1 = r-top1 + " " + r-top3
                r-top4 = r-top4 + dul-format + (IF v-per[i] THEN dul-formats ELSE "").
        END.
        r-top2 = r-top2 + r-top4.

        DISPLAY v-hdr r-top1 r-top2 WITH FRAME rpt-top.

        v-rpt = gl-rpt.rpt.

        IF tb_excel THEN 
        DO:
            ASSIGN 
                excelheader = ",".

            DO i = 1 TO v-no-col:
                excelheader = excelheader + v-ch[i] + ","
                    + (IF all-per THEN " " ELSE (IF v-per[i] THEN lv-pct-hdr + "," ELSE "")).
            END.

            excelheader = RIGHT-TRIM(excelheader,",").

            PUT STREAM excel UNFORMATTED 
                '"' REPLACE(excelheader,',','","') '"' SKIP.
        END.

        RUN gl/gl-fs.p(INPUT tb_excel).

        HIDE FRAME rpt-top.
        PUT SKIP(1).
        PAGE.
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
            ELSE IF lv-field-hdl:TYPE = "Fill-in" THEN 
                    ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                        parm-lbl-list = parm-lbl-list + lv-field-hdl:HELP + "," 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-rpt C-Win 
PROCEDURE valid-rpt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*FIND FIRST gl-rpt WHERE gl-rpt.company = cocode                      
                    AND gl-rpt.rpt = lv-rpt:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                    AND gl-rpt.LINE = 1
    NO-LOCK NO-ERROR.
IF NOT AVAIL gl-rpt THEN DO:
   MESSAGE "Invalid Report Name, try help..." VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR. 
END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sub-lvl C-Win 
PROCEDURE valid-sub-lvl :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF INT(sub-acct-lvl:SCREEN-VALUE) GT aclevel THEN 
        DO:
            MESSAGE TRIM(sub-acct-lvl:LABEL) +
                " may not be greater than " +
                TRIM(STRING(aclevel,">>>>")) +
                "..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO sub-acct-lvl.
            RETURN ERROR. 
        END.
    END.

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
            fi_file:SCREEN-VALUE = "c:\tmp\GLFinancial.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
