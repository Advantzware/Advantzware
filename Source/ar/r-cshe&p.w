&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File: ar\r-cshe&p.w
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
DEFINE VARIABLE list-name    AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-comp-curr AS CHARACTER NO-UNDO.


{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.
IF AVAILABLE company THEN lv-comp-curr = company.curr-code.

DEFINE VARIABLE v-invalid                AS LOG       NO-UNDO.
DEFINE VARIABLE v-postable               AS LOG       NO-UNDO.

DEFINE VARIABLE v-from-date              AS DATE      FORMAT "99/99/9999" INIT TODAY.
DEFINE VARIABLE v-to-date                AS DATE      FORMAT "99/99/9999" INIT TODAY.
DEFINE VARIABLE xtrnum                   AS INTEGER   NO-UNDO.
DEFINE VARIABLE xar-acct                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE xdis-acct                AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-print-fmt              AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form           AS LOGICAL.
DEFINE VARIABLE ls-fax-file              AS CHARACTER NO-UNDO.
DEFINE VARIABLE ld-curr                  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lv-audit-dir             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustStatCheck           AS CHARACTER NO-UNDO .
DEFINE VARIABLE cRtnChar                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lARAutoReleaseCreditHold AS LOGICAL   NO-UNDO .
DEFINE TEMP-TABLE tt-post NO-UNDO 
    FIELD row-id   AS ROWID
    FIELD ex-rate  LIKE currency.ex-rate INIT 1
    FIELD curr-amt LIKE ar-cash.check-amt
    FIELD actnum   LIKE account.actnum.
                               
DEFINE TEMP-TABLE ttGLTrans NO-UNDO 
    FIELD company AS CHARACTER 
    FIELD actnum  AS CHARACTER 
    FIELD jrnl    AS CHARACTER 
    FIELD tr-dscr AS CHARACTER 
    FIELD tr-date AS DATE 
    FIELD period  AS INTEGER 
    FIELD tr-num  AS INTEGER
    FIELD tr-amt  AS DECIMAL
    FIELD doDscr  AS CHARACTER.  
    
DEFINE TEMP-TABLE ttARLedger NO-UNDO
    FIELD company  AS CHARACTER 
    FIELD cust-no  AS CHARACTER 
    FIELD amt      AS DECIMAL 
    FIELD ref-num  AS CHARACTER 
    FIELD ref-date AS DATE 
    FIELD tr-date  AS DATE 
    FIELD tr-num   AS INTEGER.  
    
DEFINE BUFFER bf-period FOR period.    
   
DO TRANSACTION:
    {sys/inc/oecredit.i}
    {sys/inc/postdate.i}
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.company EQ cocode AND
        sys-ctrl.name    EQ "AUDITDIR"
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "AUDITDIR"
            sys-ctrl.descrip  = "Audit Trails directory"
            sys-ctrl.char-fld = ".\AUDIT TRAILS".
    END.

    lv-audit-dir = sys-ctrl.char-fld.

    IF LOOKUP(SUBSTR(lv-audit-dir,LENGTH(lv-audit-dir),1),"/,\") > 0 THEN
        lv-audit-dir = SUBSTR(lv-audit-dir,1,LENGTH(lv-audit-dir) - 1).

    RELEASE sys-ctrl.

    RUN sys/ref/nk1look.p (INPUT cocode, "ARAutoReleaseCreditHold", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cRtnChar, OUTPUT lRecFound).
    IF lRecFound THEN
        lARAutoReleaseCreditHold = LOGICAL(cRtnChar) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_cust end_cust ~
begin_date end_date begin_check-no end_check-no rd_sort rd-dest ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_cust end_cust ~
begin_date end_date begin_check-no end_check-no lbl_sort rd_sort rd-dest ~
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

DEFINE VARIABLE begin_check-no AS int64     FORMAT ">>>>>>>>>>>9" INITIAL 0 
    LABEL "Beginning Check No" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1.

DEFINE VARIABLE begin_cust     AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Receipt Date" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE end_check-no   AS int64     FORMAT ">>>>>>>>>>>9" INITIAL 999999999 
    LABEL "Ending Check No" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1.

DEFINE VARIABLE end_cust       AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Receipt Date" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1 NO-UNDO.

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

DEFINE VARIABLE tran-date      AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Post Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period    AS INTEGER   FORMAT ">>":U INITIAL 0 
    LABEL "Period" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

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
    "To Email", 5
    SIZE 15 BY 5 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Customer" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Customer", "Customer",
    "Sequence", "Sequence"
    SIZE 31 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 5.33.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 7.62.
     
DEFINE VARIABLE tbAutoClose  AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.     

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tran-date AT ROW 2.25 COL 44 COLON-ALIGNED
    tran-period AT ROW 3.45 COL 44 COLON-ALIGNED
    begin_cust AT ROW 4.79 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust AT ROW 4.79 COL 70 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_date AT ROW 5.83 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Invoice Date"
    end_date AT ROW 5.83 COL 70 COLON-ALIGNED HELP
    "Enter Ending Invoice Date"
    begin_check-no AT ROW 6.83 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Check Number" WIDGET-ID 2
    end_check-no AT ROW 6.83 COL 70 COLON-ALIGNED HELP
    "Enter Ending Check Number" WIDGET-ID 4
    lbl_sort AT ROW 8 COL 29 COLON-ALIGNED NO-LABELS
    rd_sort AT ROW 8 COL 41 NO-LABELS
    rd-dest AT ROW 9.76 COL 5 NO-LABELS
    lv-ornt AT ROW 9.86 COL 44 NO-LABELS
    lines-per-page AT ROW 9.86 COL 86 COLON-ALIGNED
    lv-font-no AT ROW 9.86 COL 33 COLON-ALIGNED
    lv-font-name AT ROW 11 COL 29 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 14 COL 29
    btn-ok AT ROW 16 COL 28.8
    btn-cancel AT ROW 16 COL 50
    tbAutoClose AT ROW 15.1 COL 29 WIDGET-ID 64
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.2 COL 4 
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 9.3 COL 4
    RECT-6 AT ROW 9.67 COL 3
    RECT-7 AT ROW 1.62 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 18
    BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW C-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "Cash Receipts Edit/Post Register"
        HEIGHT             = 16.5
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
    begin_check-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_check-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tran-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    tran-period:PRIVATE-DATA IN FRAME FRAME-A = "parm".
                
                
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
                
/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A = TRUE.                

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Cash Receipts Edit/Post Register */
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
ON WINDOW-CLOSE OF C-Win /* Cash Receipts Edit/Post Register */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_check-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_check-no C-Win
ON LEAVE OF begin_check-no IN FRAME FRAME-A /* Beginning Check No */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Receipt Date */
    DO:
        ASSIGN {&self-name}.
        {ar/checkPeriod.i begin_date tran-date:SCREEN-VALUE 2}
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
        DEFINE VARIABLE lv-post AS LOG NO-UNDO.


        RUN check-date.
        IF v-invalid THEN RETURN NO-APPLY.

        DO WITH FRAME {&FRAME-NAME}:
            {ar/checkPeriod.i begin_date:SCREEN-VALUE tran-date:SCREEN-VALUE 2}
        
            {ar/checkPeriod.i end_date:SCREEN-VALUE tran-date:SCREEN-VALUE 2}
            
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.

        DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/

            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                WHERE gl-ctrl.company EQ cocode NO-ERROR.
            IF AVAILABLE gl-ctrl THEN 
            DO:
                ASSIGN 
                    xtrnum        = gl-ctrl.trnum + 1
                    gl-ctrl.trnum = xtrnum.
                RELEASE gl-ctrl.
            END.
        END.

        RUN run-report. 

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &type= 'Customer'
                            &begin_cust= "begin_cust"
                            &END_cust= "begin_cust" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE="Customer"
                             &begin_cust= "begin_cust"
                             &END_cust= "begin_cust" 
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE="Customer"
                                  &begin_cust= "begin_cust"
                                  &END_cust= "begin_cust" 
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

                    END.
                END. 
            WHEN 6 THEN RUN OUTPUT-to-port.
        END CASE.

        IF v-postable THEN 
        DO:
            lv-post = NO.

            MESSAGE "Post Cash Receipts?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE lv-post.

            IF lv-post THEN 
            DO:      
                RUN post-gl.
                RUN check-status .
                RUN copy-report-to-audit-dir.
                MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
            END.
            ELSE RUN undo-trnum.  
        END.

        ELSE 
        DO:
            MESSAGE "No Cash Receipts available for posting..." VIEW-AS ALERT-BOX ERROR.
            RUN undo-trnum.
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_check-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_check-no C-Win
ON LEAVE OF end_check-no IN FRAME FRAME-A /* Ending Check No */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Receipt Date */
    DO:
        ASSIGN {&self-name}.
       {ar/checkPeriod.i end_date tran-date:SCREEN-VALUE 2}
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
ON LEAVE OF tran-date IN FRAME FRAME-A /* Post Date */
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

    RUN init-proc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "AC2" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}
    DO WITH FRAME {&frame-name}:
        {custom/usrprint.i}

        IF postdate-log THEN 
        DO:
            ASSIGN
                tran-date:SCREEN-VALUE = STRING(TODAY)
                tran-date              = TODAY.
            RUN check-date.
        END.

        ELSE
            ASSIGN
                tran-date:SCREEN-VALUE   = ""
                tran-period:SCREEN-VALUE = "".
        APPLY "entry" TO tran-date.
    
    END.

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
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DO WITH FRAME {&frame-name}:
        v-invalid = NO.

        RUN GL_CheckModClosePeriod(INPUT cocode, INPUT DATE(tran-date), INPUT "AR", OUTPUT cMessage, OUTPUT lSuccess ) .  
        IF NOT lSuccess THEN 
        DO:
            MESSAGE cMessage VIEW-AS ALERT-BOX INFORMATION.
            v-invalid = YES.
        END.
    
        FIND FIRST period                   
            WHERE period.company EQ cocode
            AND period.pst     LE tran-date
            AND period.pend    GE tran-date
            NO-LOCK NO-ERROR.
        IF AVAILABLE period THEN tran-period:SCREEN-VALUE = STRING(period.pnum).
        
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-report-to-audit-dir C-Win 
PROCEDURE copy-report-to-audit-dir :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE targetfile AS CHARACTER FORMAT "X(50)" NO-UNDO.
    DEFINE VARIABLE dirname1   AS CHARACTER FORMAT "X(20)" NO-UNDO.
    DEFINE VARIABLE dirname2   AS CHARACTER FORMAT "X(20)" NO-UNDO.
    DEFINE VARIABLE dirname3   AS CHARACTER FORMAT "X(20)" NO-UNDO.

    ASSIGN 
        targetfile = lv-audit-dir + "\AR\AC2\Run#"
                    + STRING(xtrnum) + ".txt"
        dirname1   = lv-audit-dir
        dirname2   = lv-audit-dir + "\AR"
        dirname3   = lv-audit-dir + "\AR\AC2".

    OS-COPY VALUE(list-name) VALUE (targetfile).

    IF SEARCH(targetfile) EQ ? THEN 
    DO:
        OS-CREATE-DIR VALUE(dirname1).
        OS-CREATE-DIR VALUE(dirname2).
        OS-CREATE-DIR VALUE(dirname3).
        OS-COPY VALUE(list-name) VALUE (targetfile).
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
    DISPLAY tran-date tran-period begin_cust end_cust begin_date end_date 
        begin_check-no end_check-no lbl_sort rd_sort rd-dest tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 tran-date begin_cust end_cust begin_date end_date 
        begin_check-no end_check-no rd_sort rd-dest tbAutoClose 
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

    FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK.
    xar-acct   = ar-ctrl.receivables.
    xdis-acct  = ar-ctrl.discount.

    FIND FIRST account WHERE account.company EQ cocode
        AND account.actnum  EQ xar-acct NO-LOCK NO-ERROR.
    IF NOT AVAILABLE account OR account.actnum EQ "" THEN 
    DO:
        MESSAGE " Receivables Account is blank or is not on file for this Company." VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
    FIND FIRST account WHERE account.company EQ cocode
        AND account.actnum  EQ xdis-acct
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE account OR account.actnum EQ "" THEN 
    DO:

        MESSAGE " Discount Account is blank or is not on file for this Company." VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
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
    
         IF NOT OKpressed THEN  RETURN NO-APPLY.  */

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
         IF NOT RESULT THEN v-postable = NO.
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
    RUN scr-rpt-d.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateARLedger C-Win 
PROCEDURE pCreateARLedger PRIVATE :
    /*------------------------------------------------------------------------------
     Purpose: To create AR Ledger records from the temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH ttARLedger:
        CREATE ar-ledger.
        ASSIGN
            ar-ledger.company  = ttARLedger.company  
            ar-ledger.cust-no  = ttARLedger.cust-no  
            ar-ledger.amt      = ttARLedger.amt      
            ar-ledger.ref-num  = ttARLedger.ref-num                  
            ar-ledger.ref-date = ttARLedger.ref-date               
            ar-ledger.tr-date  = ttARLedger.tr-date               
            ar-ledger.tr-num   = ttARLedger.tr-num
            .
            
        DELETE ttARLedger.                       
    END.   
    RELEASE ar-ledger.                        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateGLTrans C-Win 
PROCEDURE pCreateGLTrans PRIVATE :
    /*------------------------------------------------------------------------------
     Purpose: To create gltrans records from temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH ttGLTrans:
        RUN GL_SpCreateGLHist(ttGLTrans.company,
            ttGLTrans.actnum,
            ttGLTrans.jrnl,
            ttGLTrans.tr-dscr,
            ttGLTrans.tr-date,
            ttGLTrans.tr-amt,
            ttGLTrans.tr-num,
            ttGLTrans.period,
            "A",
            ttGLTrans.tr-date,
            ttGLTrans.doDscr,
            "AR").
        
        DELETE ttGLTrans.        
    END.  
    RELEASE gltrans.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-gl C-Win 
PROCEDURE post-gl :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE t1        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE xar-cashl AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-rowid  AS ROWID     NO-UNDO.
    DEFINE VARIABLE li-iter   AS INTEGER   NO-UNDO.
  
    DEFINE BUFFER b-cashl   FOR ar-cashl.
    DEFINE BUFFER ar-c-memo FOR reftable.
    
    DO WHILE CAN-FIND(FIRST tt-post) AND li-iter LE 100000:
        li-iter = li-iter + 1.
    
        RELEASE tt-post.
    
        FOR EACH tt-post,
            FIRST ar-cash EXCLUSIVE-LOCK
            WHERE ROWID(ar-cash) EQ tt-post.row-id 
            BREAK BY tt-post.actnum
            TRANSACTION:

            FIND FIRST bank EXCLUSIVE-LOCK
                WHERE bank.company   EQ cocode
                AND bank.bank-code EQ ar-cash.bank-code
                NO-WAIT NO-ERROR.
            IF NOT AVAILABLE bank THEN 
                NEXT.
    
            FIND FIRST cust EXCLUSIVE-LOCK
                {sys/ref/custW.i}
                AND cust.cust-no EQ ar-cash.cust-no
                NO-ERROR NO-WAIT.
            IF NOT AVAILABLE cust THEN
                NEXT.
    
            ASSIGN
                xar-cashl = bank.actnum
                bank.bal  = bank.bal + tt-post.curr-amt
                t1        = 0 
                .
                
            xar-acct = STRING(DYNAMIC-FUNCTION("GL_GetAccountAR", cust.company, cust.cust-no)).
            
            FOR EACH ar-cashl 
                WHERE ar-cashl.c-no EQ ar-cash.c-no:
                ar-cashl.posted = YES.
    
                RELEASE b-cashl.
    
                IF ar-cashl.inv-no NE 0 AND ar-cashl.on-account EQ NO THEN 
                DO:
                    FIND FIRST ar-inv
                        WHERE ar-inv.company EQ cocode
                        AND ar-inv.inv-no  EQ ar-cashl.inv-no
                        NO-ERROR.
                    IF AVAILABLE ar-inv THEN 
                    DO:
                        ASSIGN
                            ar-inv.disc-taken = ar-inv.disc-taken + ar-cashl.amt-disc
                            ar-inv.paid       = ar-inv.paid + ar-cashl.amt-paid
                            ar-inv.due        = ar-inv.due - ar-cashl.amt-paid
                            ar-inv.pay-date   = ar-cash.check-date.
        
                        IF ar-inv.due LE 0 THEN 
                        DO:
                            IF cust.avg-pay LT 1 OR cust.avg-pay EQ ? THEN 
                                cust.avg-pay = 1.
                            cust.avg-pay = ((cust.num-inv * cust.avg-pay) +
                                (ar-cash.check-date - ar-inv.inv-date)) / (cust.num-inv + 1).
                            cust.num-inv = cust.num-inv + 1.
                        END.
        
                        FIND CURRENT ar-inv NO-LOCK NO-ERROR.
                    END.
    
                    FIND FIRST ar-c-memo NO-LOCK
                        WHERE ar-c-memo.reftable EQ "ar-cashl.ar-cashl"
                        AND ar-c-memo.company  EQ ar-cash.company
                        AND ar-c-memo.loc      EQ ""
                        AND ar-c-memo.code     EQ ar-cashl.rec_key
                        NO-ERROR.
        
                    IF AVAILABLE ar-c-memo THEN
                        FIND FIRST b-cashl
                            WHERE b-cashl.c-no EQ INT(ar-c-memo.val[1])
                            AND b-cashl.line EQ INT(ar-c-memo.val[2])
                            NO-ERROR.
                END.
    
                IF AVAILABLE b-cashl THEN 
                DO:
                    ASSIGN
                        b-cashl.inv-no     = ar-cashl.inv-no
                        b-cashl.inv-date   = ar-cashl.inv-date
                        b-cashl.amt-due    = ar-cashl.amt-due
                        b-cashl.on-account = NO
                        .
                    DELETE ar-cashl.
                END.
    
                ELSE 
                DO:
                    IF ar-cashl.inv-no EQ 0 AND ar-cashl.on-account EQ YES THEN 
                        cust.on-account = cust.on-account + ar-cashl.amt-paid.
           
                    CREATE ttGLTrans.
                    ASSIGN
                        t1                = t1 + ar-cashl.amt-paid
                
                        ttGLTrans.company = cocode
                        ttGLTrans.actnum  = ar-cashl.actnum
                        ttGLTrans.jrnl    = "CASHR"
                        ttGLTrans.tr-dscr = cust.cust-no + " " +
                                             STRING(ar-cash.check-no,"999999999999") +
                                             " Inv# " + STRING(ar-cashl.inv-no)
                        ttGLTrans.tr-date = tran-date
                        ttGLTrans.tr-amt  = ar-cashl.amt-paid - ar-cashl.amt-disc
                        ttGLTrans.period  = tran-period
                        ttGLTrans.tr-num  = xtrnum
                        ttGLTrans.doDscr  = "Check: " + STRING(ar-cash.check-no,"999999999999") +
                                             " Date: " + STRING(tran-date)
                        ar-cashl.amt-paid = ar-cashl.amt-paid - ar-cashl.amt-disc                        
                        .
    
                    IF ar-cashl.amt-disc NE 0 THEN 
                    DO:
                        CREATE ttGLTrans.
                        ASSIGN
                            ttGLTrans.company = cocode
                            ttGLTrans.actnum  = xdis-acct
                            ttGLTrans.jrnl    = "CRDIS"
                            ttGLTrans.tr-dscr = cust.cust-no + " " +
                                           STRING(ar-cash.check-no,"999999999999") +
                                           " Inv# " + STRING(ar-cashl.inv-no)
                            ttGLTrans.tr-date = tran-date
                            ttGLTrans.tr-amt  = ar-cashl.amt-disc
                            ttGLTrans.period  = tran-period
                            ttGLTrans.tr-num  = xtrnum
                            ttGLTrans.doDscr  = "Check: " + STRING(ar-cash.check-no,"999999999999") +
                                                " Date: " + STRING(tran-date).
                    
                        CREATE ttArLedger.
                        ASSIGN
                            ttARLedger.company  = cocode
                            ttARLedger.cust-no  = ar-cash.cust-no
                            ttARLedger.amt      = ar-cashl.amt-disc
                            ttARLedger.ref-num  = "DISC " +
                                                 STRING(ar-cash.check-no,"999999999999") +
                                                 "-" + STRING(ar-cashl.line,"9999999999")
                            ttARLedger.ref-date = ar-cash.check-date
                            ttARLedger.tr-date  = tran-date
                            ttARLedger.tr-num   = xtrnum
                            ttGLTrans.doDscr    = "Check: " + STRING(ar-cash.check-no,"999999999999") +
                                                  " Date: " + STRING(tran-date).
                    END.
                END.
            END.  /* each line */
    
            ASSIGN
                cust.acc-bal   = cust.acc-bal - t1
                cust.lpay      = t1
                cust.lpay-date = ar-cash.check-date.
    
            IF cust.acc-bal GE cust.hibal THEN
                ASSIGN
                    cust.hibal      = cust.acc-bal
                    cust.hibal-date = ar-cash.check-date
                    .
    
            IF t1 NE 0 THEN 
            DO:
                FIND ttGLTrans 
                    WHERE ttGlTrans.company EQ cocode
                    AND ttGLTrans.actnum  EQ xar-acct
                    AND ttGLTrans.jrnl    EQ "CASHR"
                    AND ttGLTrans.tr-dscr EQ "CASH RECEIPTS"
                    AND ttGLTrans.tr-date EQ tran-date
                    AND ttGLTrans.period  EQ tran-period
                    AND ttGLTrans.tr-num  EQ xtrnum 
                    NO-ERROR.
                IF NOT AVAILABLE ttGLTrans THEN 
                DO:
                    CREATE ttGLTrans.
                    ASSIGN
                        ttGlTrans.company = cocode
                        ttGlTrans.actnum  = xar-acct
                        ttGlTrans.jrnl    = "CASHR"
                        ttGlTrans.tr-dscr = "CASH RECEIPTS"
                        ttGlTrans.tr-date = tran-date
                        ttGlTrans.period  = tran-period
                        ttGlTrans.tr-num  = xtrnum
                        lv-rowid          = ROWID(ttGLTrans)
                        ttGLTrans.doDscr  = "Check: " + STRING(ar-cash.check-no,"999999999999") +
                                            " Date: " + STRING(tran-date)
                        .
                END.
                ttGLTrans.tr-amt = ttGLTrans.tr-amt - t1.
            END.
    
            CREATE ttARLedger.
            ASSIGN
                ttARLedger.company  = cocode
                ttARLedger.cust-no  = ar-cash.cust-no
                ttARLedger.amt      = ar-cash.check-amt
                ttARLedger.ref-num  = "CHK# " + STRING(ar-cash.check-no,"999999999999")
                ttARLedger.ref-date = ar-cash.check-date
                ttARLedger.tr-date  = tran-date
                ttARLedger.tr-num   = xtrnum
                ttGLTrans.doDscr    = "Check: " + STRING(ar-cash.check-no,"999999999999") +
                                      " Date: " + STRING(tran-date)
                ar-cash.posted      = YES.
            
            RELEASE cust.    
            RELEASE bank.
        
            ACCUM tt-post.curr-amt - ar-cash.check-amt (TOTAL BY tt-post.actnum).
        
            IF LAST-OF(tt-post.actnum) AND tt-post.actnum NE "" THEN 
            DO:
                CREATE ttGLTrans.
                ASSIGN
                    ttGLTrans.company = cocode
                    ttGLTrans.actnum  = tt-post.actnum
                    ttGLTrans.jrnl    = "CASHR"
                    ttGLTrans.tr-dscr = "CASH RECEIPTS CURRENCY GAIN/LOSS"
                    ttGLTrans.tr-date = tran-date
                    ttGLTrans.period  = tran-period
                    ttGLTrans.tr-num  = xtrnum
                    ttGLTrans.tr-amt  = (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-cash.check-amt)
                    ttGLTrans.doDscr  = "Check: " + STRING(ar-cash.check-no,"999999999999") +
                                        " Date: " + STRING(tran-date)
                    .
        
                CREATE ttGLTrans.
                ASSIGN
                    ttGLTrans.company = cocode
                    ttGLTrans.actnum  = tt-post.actnum
                    ttGLTrans.jrnl    = "CASHR"
                    ttGLTrans.tr-dscr = "CASH RECEIPTS CURRENCY GAIN/LOSS"
                    ttGLTrans.tr-date = tran-date
                    ttGLTrans.period  = tran-period
                    ttGLTrans.tr-num  = xtrnum
                    ttGLTrans.tr-amt  = - (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-cash.check-amt)
                    ttGLTrans.doDscr  = "Check: " + STRING(ar-cash.check-no,"999999999999") +
                                        " Date: " + STRING(tran-date).
            END.

            RUN pCreateARLedger.
            
            RUN pCreateGLTrans.

            DELETE tt-post.
        END.
    END.  /* DO WHILE */

    RELEASE ar-cash.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ---------------------------------------------------- ar/ar-creg.p 10/94 gb */
    /* AR Cash  - Edit Register & Post Transactions                   */
    /* -------------------------------------------------------------------------- */

    {sys/FORM/r-top3w.f}

    DEFINE VARIABLE dsc            AS DECIMAL FORMAT "->>,>>>,>>9.99".
    DEFINE VARIABLE net-cr         AS DECIMAL FORMAT "->>,>>>,>>9.99".
    DEFINE VARIABLE save_id        AS RECID.
    DEFINE VARIABLE time_stamp     AS ch.
    DEFINE VARIABLE qfirst         AS l.
    DEFINE VARIABLE post           AS LOG     FORMAT "Yes/No"
        LABEL "   Post to G/L & Customer files?   " INIT NO.
    DEFINE VARIABLE g1             AS DECIMAL FORMAT "->>,>>>,>>9.99".
    DEFINE VARIABLE t1             LIKE g1.
    DEFINE VARIABLE g2             LIKE g1.
    DEFINE VARIABLE t3             LIKE g1.
    DEFINE VARIABLE v1             LIKE g1.
    DEFINE VARIABLE v2             LIKE g1.
    DEFINE VARIABLE t2             LIKE g1.


    DEFINE VARIABLE v-amt-due-sub  AS DECIMAL FORMAT "->,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-amt-due-tot  AS DECIMAL FORMAT "->,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-amt-paid-sub AS DECIMAL FORMAT "->,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-amt-paid-tot AS DECIMAL FORMAT "->,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-disc-sub     AS DECIMAL FORMAT "->>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-disc-tot     AS DECIMAL FORMAT "->>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-on-act-amt   AS DECIMAL FORMAT "->>>>>>9.99" NO-UNDO.
    DEFINE VARIABLE v-on-act-sub   AS DECIMAL FORMAT "->>>>>>9.99" NO-UNDO.
    DEFINE VARIABLE v-on-act-tot   AS DECIMAL FORMAT "->>>>>>9.99" NO-UNDO.
    DEFINE VARIABLE archk          AS DECIMAL FORMAT ">>>>>>>>>>>>".
    DEFINE VARIABLE sort-by-cust   AS LOG     INIT YES FORMAT "Customer/Sequence" NO-UNDO.
    /*
    DEF VAR tmp-dir AS CHARACTER NO-UNDO. 
    DEF VAR str-tit AS cha FORM "x(50)" NO-UNDO.
    DEF VAR str-tit2 AS cha FORM "x(50)" NO-UNDO.
    DEF VAR str-tit3 AS cha FORM "x(50)" NO-UNDO. 
    DEF VAR coname AS CHARACTER NO-UNDO. 
    DEF VAR loname AS CHARACTER NO-UNDO.   */

    SESSION:SET-WAIT-STATE("general").

    ASSIGN 
        sort-by-cust = rd_sort = "customer"
        v-from-date  = begin_date
        v-to-date    = END_date
        .

    FORM HEADER
        "CUSTOMER      NAME                   CHECK       DATE     CASH       INVOICE         AMOUNT       AMOUNT     DISCOUNT     ON ACCOUNT"
        "   #                                 NUMBER               RECEIVED   NUMBER          DUE          APPLIED                   PAYMENTS"
        SKIP FILL("_",132) FORMAT "x(132)"
        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top PAGE-TOP WIDTH 200 STREAM-IO.

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    /* str-tit3 = "Period " + STRING(tran-period,"99") + " - " +
                  IF AVAIL period THEN
                    (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
       {sys/inc/ctrtext.i str-tit3 132}. */
    ASSIGN
        str-tit  = coname + " - " + loname
        str-tit2 = "CASH RECEIPTS  -  EDIT REGISTER " + string(xtrnum)
        str-tit3 = "Period " + string(tran-period,"99") + " - " + string(tran-date) 
        x        = (112 - length(str-tit)) / 2
        str-tit  = FILL(" ",x) + str-tit
        x        = (114 - length(str-tit2)) / 2
        str-tit2 = FILL(" ",x) + str-tit2
        x        = (132 - length(str-tit3)) / 2
        str-tit3 = FILL(" ",x) + str-tit3.

    ASSIGN 
        v-amt-due-tot  = 0
        v-amt-paid-tot = 0
        v-disc-tot     = 0
        v-on-act-tot   = 0.

    DISPLAY "" WITH FRAME r-top.
    DISPLAY "" WITH FRAME f-top.

    EMPTY TEMP-TABLE tt-post.

    IF sort-by-cust THEN 
    DO:
        {ar/ar-creg.i cust-no 1}
    END.
    ELSE 
    DO:
        {ar/ar-creg.i c-no 2}
    END.

    ASSIGN
        str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date) + " - " +
              "Summary by Account".
    x = (132 - length(str-tit3)) / 2.
    str-tit3 = FILL(" ",x) + str-tit3.

    PAGE.

    FORMAT HEADER
        "ACCOUNT                                  DATE   CUSTOMER   MEMO #    "
        "LINE DESCRIPTION                QTY   UNIT PRICE           AMOUNT" SKIP
        FILL("_",132) FORMAT "x(132)"
        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top2 PAGE-TOP WIDTH 200 STREAM-IO.

    DISPLAY "" WITH FRAME f-top2.

    g2 = 0.

    FOR EACH tt-post,
        FIRST ar-cash WHERE ROWID(ar-cash) EQ tt-post.row-id NO-LOCK,
        EACH ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no NO-LOCK
        BREAK BY ar-cashl.actnum BY ar-cashl.c-no:

        IF LOOKUP(ar-cash.cust-no,cCustStatCheck) EQ 0 THEN
            ASSIGN cCustStatCheck = cCustStatCheck + ar-cash.cust-no + "," .

        IF FIRST-OF(ar-cashl.actnum) THEN 
        DO:
            FIND FIRST account NO-LOCK
                WHERE account.company EQ cocode
                AND account.actnum  EQ ar-cashl.actnum
                NO-ERROR.
            IF AVAILABLE account THEN
                PUT ar-cashl.actnum + " - " + account.dscr FORMAT "x(39)".
            ELSE
                PUT ar-cashl.actnum.
        END.
        v-postable = YES.

        archk = ar-cash.check-no.

        PUT ar-cash.check-date AT 41        SPACE(1)
            ar-cash.cust-no                 SPACE(1)
            archk                           SPACE(1)
            ar-cashl.line   FORMAT ">>>>"   SPACE(1)
            ar-cashl.dscr   FORMAT "x(20)"  SPACE(1)
            ar-cashl.amt-paid - ar-cashl.amt-disc FORMAT "->,>>>,>>9.99" TO 132
            SKIP.

        ACCUM ar-cashl.amt-paid - ar-cashl.amt-disc (TOTAL BY ar-cashl.actnum).

        IF LAST-OF(ar-cashl.actnum) THEN 
        DO:
            PUT "** TOTAL "                           TO 116
                (ACCUM TOTAL BY ar-cashl.actnum ar-cashl.amt-paid - ar-cashl.amt-disc)
                FORMAT "->,>>>,>>9.99"   TO 132
                SKIP(1).

            g2 = g2 +
                (ACCUM TOTAL BY ar-cashl.actnum ar-cashl.amt-paid - ar-cashl.amt-disc).
        END.
    END.

    FOR EACH tt-post WHERE tt-post.actnum NE "",
        FIRST ar-cash WHERE ROWID(ar-cash) EQ tt-post.row-id NO-LOCK
        BREAK BY tt-post.actnum BY ar-cash.c-no:

        IF FIRST-OF(tt-post.actnum) THEN 
        DO:
            FIND FIRST account NO-LOCK
                WHERE account.company EQ cocode
                AND account.actnum  EQ tt-post.actnum
                NO-ERROR.
            IF AVAILABLE account THEN
                PUT tt-post.actnum + " - " + account.dscr FORMAT "x(39)".
            ELSE
                PUT tt-post.actnum.
        END.

        archk = ar-cash.check-no.

        PUT ar-cash.check-date AT 41        SPACE(1)
            ar-cash.cust-no                 SPACE(1)
            archk                           SPACE(1)
            0               FORMAT ">>>>"   SPACE(1)
            ""              FORMAT "x(20)"  SPACE(1)
            tt-post.curr-amt - ar-cash.check-amt FORMAT "->,>>>,>>9.99" TO 132
            SKIP.

        ACCUM tt-post.curr-amt - ar-cash.check-amt (TOTAL BY tt-post.actnum).

        IF LAST-OF(tt-post.actnum) THEN 
        DO:
            PUT "** TOTAL "                           TO 116
                (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-cash.check-amt)
                FORMAT "->,>>>,>>9.99"  TO 132
                SKIP(1).

            g2 = g2 +
                (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-cash.check-amt).
        END.
    END.

    IF v-postable THEN
        DISPLAY "***** TOTAL FOR ALL ACCOUNTS "   TO 116
            g2 FORMAT "->,>>>,>>9.99"         TO 132
            WITH NO-LABELS NO-BOX STREAM-IO WIDTH 200.

    SESSION:SET-WAIT-STATE("").

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undo-trnum C-Win 
PROCEDURE undo-trnum :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO TRANSACTION: 
        /* 47885 removed loop around find */
        FIND FIRST gl-ctrl EXCLUSIVE-LOCK
            WHERE gl-ctrl.company EQ cocode NO-ERROR.
        IF AVAILABLE gl-ctrl THEN 
        DO:
            IF xtrnum EQ gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
            FIND CURRENT gl-ctrl NO-LOCK.
            RELEASE gl-ctrl.
        END. /* IF AVAIL gl-ctrl */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-status C-Win 
PROCEDURE check-status :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
 
    DEFINE VARIABLE lRelHold AS LOGICAL NO-UNDO .
  
    FOR EACH cust NO-LOCK
        WHERE cust.company EQ cocode
        AND LOOKUP(cust.cust-no,cCustStatCheck) NE 0
        AND cust.cust-no NE "" :
            
        FIND FIRST  terms NO-LOCK
            WHERE terms.company = cust.company
            AND terms.t-code  = cust.terms NO-ERROR.
     
        IF cust.cr-hold THEN 
        DO:
            lRelHold   = NO
                .
            IF cust.ord-bal + cust.acc-bal LT cust.cr-lim 
                AND cust.ord-bal LT cust.ord-lim THEN
                ASSIGN lRelHold = YES . 
         

            IF lRelHold AND lARAutoReleaseCreditHold THEN  
            DO:  
                FIND CURRENT cust EXCLUSIVE-LOCK NO-ERROR.
                cust.cr-hold = NO.
                  
                FOR EACH oe-ord EXCLUSIVE-LOCK
                    WHERE oe-ord.company             EQ cocode
                    AND oe-ord.cust-no             EQ cust.cust-no
                    AND oe-ord.stat EQ "H" :
                    ASSIGN
                        oe-ord.stat = "A" .
                END.
            END. /* lRelHold*/

        END. /* cust.cr-hold */
    END. /* for cust */
 
END PROCEDURE.
 
 /* _UIB-CODE-BLOCK-END */
 &ANALYZE-RESUME
