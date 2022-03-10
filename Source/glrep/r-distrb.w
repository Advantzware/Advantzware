&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: glrep\r-distrb.w

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

DEFINE TEMP-TABLE tt-trans NO-UNDO
    FIELD tt-recid AS RECID
    FIELD TYPE     AS cha
    FIELD c-rate   LIKE acctcost.c-rate
    FIELD costacct LIKE acctcost.costacct
    FIELD jrnl     LIKE glhist.jrnl
    FIELD tr-amt   LIKE glhist.tr-amt
    INDEX tt-trans IS PRIMARY TYPE tt-recid.

DEFINE VARIABLE v-distribute   AS LOG           NO-UNDO.
DEFINE VARIABLE v-print-fmt    AS CHARACTER     NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER     NO-UNDO.

DEFINE VARIABLE v-invalid      AS LOG           NO-UNDO.
DEFINE VARIABLE v-trnum        LIKE gl-ctrl.trnum NO-UNDO.

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
begin_accnt end_accnt tran-date rd_detsum rd-dest tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_accnt end_accnt ~
tran-date tran-period rd_detsum rd-dest tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win          AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE begin_accnt    AS CHARACTER FORMAT "X(25)":U 
    LABEL "Beginning Acct#" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Posting Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_accnt      AS CHARACTER FORMAT "X(25)":U INITIAL "zzzzzzzzzzzzzzzzzzzzzzzzz" 
    LABEL "Ending Acct#" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Posting Date" 
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

DEFINE VARIABLE tran-date      AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Transaction Date" 
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
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5
    SIZE 17.2 BY 5.19 NO-UNDO.

DEFINE VARIABLE rd_detsum      AS CHARACTER INITIAL "S" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Detail", "D",
    "Summary", "S"
    SIZE 33 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 93 BY 5.67.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 93 BY 7.86.

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
    begin_date AT ROW 2.19 COL 30.2 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 2.19 COL 74 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_accnt AT ROW 4.1 COL 20.2 COLON-ALIGNED HELP
    "Enter Beginning Account Number"
    end_accnt AT ROW 4.1 COL 64 COLON-ALIGNED HELP
    "Enter Ending Account Number"
    tran-date AT ROW 5.76 COL 38 COLON-ALIGNED
    tran-period AT ROW 6.95 COL 38 COLON-ALIGNED
    rd_detsum AT ROW 7.91 COL 40 NO-LABELS
    rd-dest AT ROW 10.1 COL 4.8 NO-LABELS
    lv-font-no AT ROW 10.19 COL 32.4 COLON-ALIGNED
    lv-ornt AT ROW 10.19 COL 43.8 NO-LABELS
    lines-per-page AT ROW 10.19 COL 88.4 COLON-ALIGNED
    lv-font-name AT ROW 11.29 COL 30.2 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 14 COL 31
    tbAutoClose AT ROW 15.76 COL 31.2 WIDGET-ID 64
    btn-ok AT ROW 16.81 COL 31
    btn-cancel AT ROW 16.81 COL 51.4
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 9.43 COL 4
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.1 COL 5
    "Report :" VIEW-AS TEXT
    SIZE 9 BY .62 AT ROW 8.14 COL 30
    RECT-6 AT ROW 9.86 COL 3
    RECT-7 AT ROW 1.57 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 97.2 BY 18.24
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
        TITLE              = "Generate Auto Distribution"
        HEIGHT             = 17.33
        WIDTH              = 97.2
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

ASSIGN 
    end_accnt:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

ASSIGN 
    tran-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Generate Auto Distribution */
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
ON WINDOW-CLOSE OF C-Win /* Generate Auto Distribution */
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

        CASE FOCUS:NAME:
            WHEN "begin_accnt" OR 
            WHEN "end_accnt" THEN 
                DO:
                    RUN windows/l-acct.w (g_company,"",FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME}, OUTPUT char-val).
                    IF char-val <> "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
                    RETURN NO-APPLY.

                END.
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_accnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_accnt C-Win
ON LEAVE OF begin_accnt IN FRAME FRAME-A /* Beginning Acct# */
    DO:
        ASSIGN {&self-name}.
        RUN pcheckGLAccount NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Posting Date */
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
  
        RUN pcheckGLAccount NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            RETURN NO-APPLY.

        ASSIGN {&DISPLAYED-OBJECTS}.

        RUN run-report. 

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
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
        SESSION:SET-WAIT-STATE("").

        IF v-distribute  THEN 
        DO:
            MESSAGE "Are you sure you want to distribute to cost account?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.

            IF ll-ans THEN 
            DO:
                DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/

                    REPEAT:
                        FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                        IF AVAILABLE gl-ctrl THEN
                        DO:
                            ASSIGN 
                                v-trnum       = gl-ctrl.trnum + 1
                                gl-ctrl.trnum = v-trnum.
                            LEAVE.
                        END.
                    END.
                END.

                FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.

                IF rd_detsum = "S" THEN RUN cost-distribute.
                ELSE RUN cost-distribute-det.
            END.
        END.
        v-distribute = NO.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
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
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Posting Date */
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
        end_date   = TODAY
        tran-date  = TODAY.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "GU5" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    RUN check-date.

    {methods/nowait.i}
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
            AND period.pst     GE begin_date
            AND period.pend    LE end_date
            AND NOT period.pstat
            NO-LOCK NO-ERROR.
        IF AVAILABLE period THEN 
        DO:
            MESSAGE "Period from " pst " to " pend " is Closed. " VIEW-AS ALERT-BOX ERROR.
            v-invalid = YES.
            RETURN.
        END.

        FIND FIRST period                   
            WHERE period.company EQ cocode
            AND period.pst     LE tran-date
            AND period.pend    GE tran-date
            NO-LOCK NO-ERROR.
        IF AVAILABLE period THEN 
        DO:
            IF NOT period.pstat /* closed */ THEN 
            DO:
                MESSAGE "Period is Closed. " VIEW-AS ALERT-BOX ERROR.
                v-invalid = YES.
            END.
            ELSE tran-period:SCREEN-VALUE = STRING(period.pnum).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cost-distribute C-Win 
PROCEDURE cost-distribute :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("general").
    DEFINE BUFFER cost-trans FOR glhist.
    DEFINE BUFFER cost-hist  FOR glhist.
    DEFINE VARIABLE v-act-amt AS DECIMAL NO-UNDO.

    FOR EACH tt-trans WHERE tt-trans.TYPE = "GLTRANS",
        EACH glhist WHERE RECID(glhist) = tt-trans.tt-recid
        /*EACH acctcost NO-LOCK WHERE acctcost.company = gltrans.company
                                AND acctcost.actnum = gltrans.actnum */
        BREAK BY /*gltrans.actnum*/ tt-trans.costacct:

        IF FIRST-OF(tt-trans.costacct) THEN v-act-amt = 0.

        v-act-amt = v-act-amt + tt-trans.tr-amt .
        IF LAST-OF(tt-trans.costacct) THEN 
        DO:
            CREATE cost-trans.
            BUFFER-COPY glhist TO cost-trans.
            ASSIGN 
                cost-trans.tr-amt  = v-act-amt /*tt-trans.tr-amt*/
                cost-trans.actnum  = tt-trans.costacct
                cost-trans.tr-date = tran-date
                cost-trans.jrnl    = "AUTODIST"
                cost-trans.period  = tran-period
                cost-trans.tr-num  = v-trnum
                cost-trans.tr-dscr = "Auto Distribution"
                .
            /*gltrans.jrnl = "AUTODIST"*/ .
        END.

    END.


    FOR EACH tt-trans WHERE tt-trans.TYPE = "GLHIST",
        EACH glhist WHERE RECID(glhist) = tt-trans.tt-recid
        /*EACH acctcost NO-LOCK WHERE acctcost.company = glhist.company
                                AND acctcost.actnum = glhist.actnum */
        BREAK BY tt-trans.costacct:

        IF FIRST-OF(tt-trans.costacct) THEN v-act-amt = 0.

        v-act-amt = v-act-amt + tt-trans.tr-amt .

        IF LAST-OF( tt-trans.costacct) THEN 
        DO:
            CREATE cost-hist.
            BUFFER-COPY glhist TO cost-hist.
            ASSIGN 
                cost-hist.tr-amt  = v-act-amt /*tt-trans.tr-amt */
                cost-hist.actnum  = tt-trans.costacct
                cost-hist.tr-date = tran-date
                cost-hist.jrnl    = "AUTODIST"
                cost-hist.period  = tran-period
                cost-hist.tr-num  = v-trnum
                cost-hist.tr-dscr = "Auto Distribution"
                .
        END.
        /*
        IF LAST-OF(glhist.actnum) THEN DO:
           CREATE cost-hist.
           BUFFER-COPY glhist TO cost-hist.
           ASSIGN cost-hist.tr-amt = glhist.tr-amt * (-1) 
                  cost-hist.jrnl = "AUTODIST".
        END.
        */
        /*glhist.jrnl = "AUTODIST" */.
    END.
    SESSION:SET-WAIT-STATE("").
    MESSAGE "Auto Distribution is completed." VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cost-distribute-det C-Win 
PROCEDURE cost-distribute-det :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("general").
    DEFINE BUFFER cost-trans FOR glhist.
    DEFINE BUFFER cost-hist  FOR glhist.
    DEFINE VARIABLE v-act-amt AS DECIMAL NO-UNDO.

    FOR EACH tt-trans WHERE tt-trans.TYPE = "GLTRANS",
        EACH glhist WHERE RECID(glhist) = tt-trans.tt-recid
        /*EACH acctcost NO-LOCK WHERE acctcost.company = gltrans.company
                                AND acctcost.actnum = gltrans.actnum */
        BREAK BY glhist.actnum:

        CREATE cost-trans.
        BUFFER-COPY glhist TO cost-trans.
        ASSIGN 
            cost-trans.tr-amt  = tt-trans.tr-amt
            cost-trans.actnum  = tt-trans.costacct
            cost-trans.tr-date = tran-date
            cost-trans.jrnl    = "AUTODIST"
            cost-trans.period  = tran-period
            cost-trans.tr-num  = v-trnum
            cost-trans.tr-dscr = "Auto Distribution"
            .
        /*gltrans.jrnl = "AUTODIST"*/ .

    END.


    FOR EACH tt-trans WHERE tt-trans.TYPE = "GLHIST",
        EACH glhist WHERE RECID(glhist) = tt-trans.tt-recid
        /*EACH acctcost NO-LOCK WHERE acctcost.company = glhist.company
                                AND acctcost.actnum = glhist.actnum */
        BREAK BY glhist.actnum:

        CREATE cost-hist.
        BUFFER-COPY glhist TO cost-hist.
        ASSIGN 
            cost-hist.tr-amt  = tt-trans.tr-amt 
            cost-hist.actnum  = tt-trans.costacct
            cost-hist.tr-date = tran-date
            cost-hist.jrnl    = "AUTODIST"
            cost-hist.period  = tran-period
            cost-hist.tr-num  = v-trnum
            cost-hist.tr-dscr = "Auto Distribution"
            .
        /*
        IF LAST-OF(glhist.actnum) THEN DO:
           CREATE cost-hist.
           BUFFER-COPY glhist TO cost-hist.
           ASSIGN cost-hist.tr-amt = glhist.tr-amt * (-1) 
                  cost-hist.jrnl = "AUTODIST".
        END.
        */
        /*glhist.jrnl = "AUTODIST" */.
    END.
    SESSION:SET-WAIT-STATE("").
    MESSAGE "Auto Distribution is completed." VIEW-AS ALERT-BOX.

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
    DISPLAY begin_date end_date begin_accnt end_accnt tran-date tran-period 
        rd_detsum rd-dest tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_date end_date begin_accnt end_accnt tran-date 
        rd_detsum rd-dest tbAutoClose btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pcheckGLAccount C-Win 
PROCEDURE pcheckGLAccount :
    /*------------------------------------------------------------------------------
     Purpose: To check valid and active GL account.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lValid    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE hValidate AS HANDLE    NO-UNDO.
    
    RUN util/Validate.p PERSISTENT SET hValidate.

    DO WITH FRAME {&FRAME-NAME}:        
        IF begin_accnt:SCREEN-VALUE NE "" THEN 
        DO:
            RUN pIsValidGLAccount IN hValidate (
                INPUT  begin_accnt:SCREEN-VALUE, 
                INPUT  NO, 
                INPUT  cocode, 
                OUTPUT lValid, 
                OUTPUT cMessage
                ) NO-ERROR.        
            IF NOT lValid THEN 
            DO:                
                MESSAGE cMessage VIEW-AS ALERT-BOX ERROR. 
                RUN presetColor NO-ERROR.      
                IF INDEX(cMessage, "Inactive") GT 0 THEN 
                    ASSIGN 
                        begin_accnt:BGCOLOR = 16
                        begin_accnt:FGCOLOR = 15
                        .                     
                APPLY "ENTRY" TO begin_accnt.
                RETURN ERROR.   
            END.  
        END.                  
       
        IF begin_accnt:SCREEN-VALUE EQ "" OR lValid THEN 
            RUN presetColor NO-ERROR.         
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE presetColor C-Win 
PROCEDURE presetColor :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF begin_accnt:BGCOLOR EQ 16 THEN             
            ASSIGN 
                begin_accnt:BGCOLOR = ?
                begin_accnt:FGCOLOR = ?
                .                          
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /***************************************************************************\
    *****************************************************************************
    **  Program: ap/rep/pjgl.p
    **      
    ** Descript: CASH DISBURSEMENT / VOUCHER REGISTER BY GL ACCT
    **
    *****************************************************************************
    \***************************************************************************/

    {sys/form/r-topw.f}

    DEFINE VARIABLE lo_trandate  AS DATE      FORMAT "99/99/9999" NO-UNDO LABEL "From Date".
    DEFINE VARIABLE hi_trandate  AS DATE      FORMAT "99/99/9999" NO-UNDO LABEL "Thru Date".
    DEFINE VARIABLE DEBUG        AS LOG       NO-UNDO INITIAL TRUE.
    DEFINE VARIABLE ws_disc      LIKE ap-payl.amt-disc COLUMN-LABEL "Discount" NO-UNDO.
    DEFINE VARIABLE ws_check-no  LIKE ap-chk.check-no NO-UNDO FORMAT ">>>>>>>"
        COLUMN-LABEL "Check#".
    DEFINE VARIABLE ws_order-no  LIKE oe-ord.ord-no NO-UNDO
        FORMAT ">>>>>>>".
    DEFINE VARIABLE ws_jrnl      LIKE glhist.jrnl COLUMN-LABEL "Journal" NO-UNDO.
    DEFINE VARIABLE gl_jrnl_list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lo_actnum    LIKE account.actnum LABEL "From GL Acct#" NO-UNDO.
    DEFINE VARIABLE hi_actnum    LIKE account.actnum LABEL "Thru GL Acct#" NO-UNDO.
    DEFINE VARIABLE t-amt        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE t-disc       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE t-qty        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE t-msf        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE hdg_printed  AS LOG       NO-UNDO.
    DEFINE VARIABLE v-tot-amt    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-tramt      AS DECIMAL   NO-UNDO.

    DEFINE BUFFER b-tt-trans FOR tt-trans.

    FORM    ws_jrnl FORM "x(15)"
        ap-inv.vend-no    COLUMN-LABEL "Vendor"
        vend.name
        ap-inv.inv-date COLUMN-LABEL "Date"
        ap-inv.inv-no COLUMN-LABEL "Invoice#"
        ws_check-no
        ws_order-no
        ap-invl.qty
        /*ap-invl.amt-msf*/
        ws_disc
        ap-invl.amt
        WITH FRAME f-det WIDTH 145 DOWN STREAM-IO.


    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        str-tit2     = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        lo_actnum    = begin_accnt
        hi_actnum    = end_accnt
        lo_trandate  = begin_date
        hi_trandate  = end_date
        gl_jrnl_list = /*(if tb_cashr    then "!CASHR,"    else "") +
                (if tb_general  then "!GENERAL,,"  else "") +
                (if tb_mcshrec  then "!MCSHREC,"  else "") +
                (if tb_apmem    then "!APMEM,"    else "") +
                (if tb_acpay    then "!ACPAY,"    else "") +
                (if tb_ap-purch then "!AP-PURCH," else "") +
                (if tb_apckr    then "!APCHR,"    else "") +
                (if tb_arinv    then "!ARINV,"    else "") +
                (if tb_cdisb    then "!CDISB,"    else "") +
                (if tb_crmem    then "!CRMEM,"    else "") +
                (if tb_apvoidck then "!APVOIDCK," else "") +
                (if tb_oeinv    then "!OEINV,"    else "")*/
   "CASHR,GENERAL,MCSHREC,APMEM,ACPAY,AP-PURCH,APCHR,ARINV,CDISB,CRMEM,DBMEM,OEINV,APCKR,APVOIDCK,CRDIS".


    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.

    EMPTY TEMP-TABLE tt-trans.

    v-distribute = NO.

    FOR EACH account NO-LOCK
        WHERE account.company EQ cocode
        AND account.actnum  GE lo_actnum
        AND account.actnum  LE hi_actnum
        AND account.inactive EQ NO :

        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ cocode
            AND glhist.actnum  EQ account.actnum
            AND glhist.tr-date GE lo_trandate
            AND glhist.tr-date LE hi_trandate          
            AND CAN-DO(gl_jrnl_list,glhist.jrnl),

            EACH acctcost NO-LOCK
            WHERE acctcost.company EQ account.company
            AND acctcost.actnum  EQ account.actnum

            BREAK BY glhist.actnum
            /* BY acctcost.costacct*/:

            IF FIRST-OF(glhist.actnum) THEN v-tot-amt = 0.

            /* auto distribution to sub cost account */
            CREATE tt-trans.
            ASSIGN
                tt-trans.tt-recid = RECID(glhist)
                tt-trans.type     = IF glhist.posted  EQ NO THEN "GLTRANS" ELSE "GLHIST"
                tt-trans.c-rate   = acctcost.c-rate
                tt-trans.tr-amt   = ROUND(glhist.tr-amt * acctcost.c-rate / 100,2)
                tt-trans.costacct = acctcost.costacct
                tt-trans.jrnl     = glhist.jrnl.

            v-tot-amt = v-tot-amt + tt-trans.tr-amt.

            IF LAST-OF(glhist.actnum) THEN 
            DO:
                /*  IF v-tot-amt NE gltrans.tr-amt THEN
                    tt-trans.tr-amt = tt-trans.tr-amt + (gltrans.tr-amt - v-tot-amt).
                */
                CREATE tt-trans.
                ASSIGN
                    tt-trans.tt-recid = RECID(glhist)
                    tt-trans.type     = IF glhist.posted  EQ NO THEN "GLTRANS" ELSE "GLHIST"
                    tt-trans.c-rate   = 100
                    tt-trans.tr-amt   = v-tot-amt * -1 /*gltrans.tr-amt * -1*/
                    tt-trans.costacct = glhist.actnum
                    tt-trans.jrnl     = "Cost Distribution".           
            END.               
        END. /* glhist */    
    END. /* account*/

    /*======*/
    VIEW FRAME F-DET.
    DOWN 0 WITH FRAME F-DET.
    ASSIGN
        hdg_printed = FALSE
        t-amt       = 0
        t-disc      = 0
        t-msf       = 0
        t-qty       = 0
        ws_disc     = 0
        ws_jrnl     = ''
        ws_check-no = 0
        ws_order-no = 0
        .


    FOR EACH tt-trans,
        FIRST account WHERE account.company = cocode
        AND account.actnum = tt-trans.costacct NO-LOCK
        BREAK BY tt-trans.costacct:

        IF LINE-COUNTER >= (PAGE-SIZE - 2) THEN 
        DO:
            PAGE.
            VIEW FRAME f-det.
            DOWN 0 WITH FRAME f-det.
        END.

        IF FIRST-OF(tt-trans.costacct) THEN 
        DO:


            v-tot-amt = 0.  
        END.

        v-distribute = YES.

        IF NOT hdg_printed THEN
        DO:
            /*  PUT SKIP account.actnum ' - '
                account.dscr
                SKIP. */
            hdg_printed = TRUE.
        END.

        IF rd_detsum = "D" THEN 
        DO: /* detail*/
            DISPLAY account.actnum @ ws_jrnl 
                account.dscr @ vend.NAME        
                /*"Cost Distribute" @ vend.NAME   */            
                tt-trans.tr-amt @ ap-invl.amt 
                WITH FRAME f-det.
            DOWN WITH FRAME f-det.
        END.
        v-tramt = tt-trans.tr-amt.
        v-tot-amt = v-tot-amt + v-tramt.

        IF LAST-OF(tt-trans.costacct) THEN 
        DO:
            IF rd_detsum = "D" THEN 
            DO: /* detail*/
                UNDERLINE ws_disc ap-invl.amt ap-invl.qty WITH FRAME f-det.
                DOWN WITH FRAME f-det.
            END.
            DISPLAY account.actnum @ ws_jrnl
                "Account Total" 
                WHEN rd_detsum = "D" @ ws_jrnl
                account.dscr @ vend.NAME        
                /*"Cost Distribute" @ vend.NAME   */            
                v-tot-amt @ ap-invl.amt 
                WITH FRAME f-det.
            DOWN WITH FRAME f-det.

            IF NOT LAST(tt-trans.costacct) AND rd_detsum = "D" THEN 
            DO: /* detail*/
                UNDERLINE ws_disc ap-invl.amt ap-invl.qty WITH FRAME f-det.
                DOWN WITH FRAME f-det.
            END.
            ASSIGN 
                t-amt = t-amt + v-tot-amt . /*gltrans.tr-amt * (-1).*/
        END.

    END.

    UNDERLINE ws_disc ap-invl.amt ap-invl.qty WITH FRAME f-det.
    DOWN WITH FRAME f-det.
    DISPLAY
        /* "* ACCOUNT TOTAL *" @ vend.name */
        t-disc @ ws_disc
        t-amt  @ ap-invl.amt
        t-qty  @ ap-invl.qty WITH FRAME f-det.
    /*t-msf  @ ap-invl.amt-msf.*/
    /* down 1. */

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

