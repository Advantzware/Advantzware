&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap\r-vdchrg.w

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
DEFINE VARIABLE v-invalid    AS LOG       NO-UNDO.
DEFINE VARIABLE lv-audit-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

DO TRANSACTION:
    {sys/inc/aplockbx.i}    
END.

ASSIGN
    cocode = gcompany
    locode = gloc.
DEFINE BUFFER xap-payl FOR ap-payl.    

DEFINE NEW SHARED VARIABLE v-trnum    AS INTEGER   NO-UNDO.    
DEFINE            VARIABLE v-postable AS LOG       INIT NO NO-UNDO.
DEFINE            VARIABLE time_stamp AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE xpayl NO-UNDO 
    FIELD recnum AS RECID.

DEFINE TEMP-TABLE w-disb NO-UNDO 
    FIELD w-actnum   LIKE ap-payl.actnum
    FIELD w-amt-paid LIKE ap-payl.amt-paid
    FIELD w-amt-disc LIKE ap-payl.amt-disc
    FIELD cDesc AS CHARACTER
    .

{sys/form/r-top3w.f}

time_stamp = STRING(TIME, "HH:MMam").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date rd-dest tbAutoClose ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period rd-dest tbAutoClose 

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

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2
    SIZE 17.2 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 4.33.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.1.

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
    tran-date AT ROW 3.05 COL 39 COLON-ALIGNED
    tran-period AT ROW 4.24 COL 39 COLON-ALIGNED
    lv-font-no AT ROW 7.33 COL 33.6 COLON-ALIGNED
    lv-ornt AT ROW 7.33 COL 43.4 NO-LABELS
    lines-per-page AT ROW 7.33 COL 86.8 COLON-ALIGNED
    rd-dest AT ROW 7.48 COL 4.8 NO-LABELS
    lv-font-name AT ROW 8.38 COL 28.2 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 10.24 COL 29.8
    tbAutoClose AT ROW 11.67 COL 29.8 WIDGET-ID 64
    btn-ok AT ROW 12.71 COL 29.4
    btn-cancel AT ROW 12.71 COL 50.8
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.1 COL 4
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 6.71 COL 4
    RECT-6 AT ROW 7.14 COL 3
    RECT-7 AT ROW 1.62 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 105.2 BY 14.91
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
        TITLE              = "A/P Voided Check Register"
        HEIGHT             = 13.14
        WIDTH              = 95
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
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

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


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* A/P Voided Check Register */
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
ON WINDOW-CLOSE OF C-Win /* A/P Voided Check Register */
    DO:
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
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
    DO:
        DEFINE VARIABLE lv-post      AS LOG       NO-UNDO.
        DEFINE VARIABLE lv-bank-file AS CHARACTER NO-UNDO.

        RUN check-date.

        ASSIGN rd-dest tran-period tran-date .

        IF v-invalid THEN RETURN NO-APPLY.
        ASSIGN rd-dest tran-period tran-date .

        DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
            /* gdm - 11050906 */
            REPEAT:
                FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                    WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
                IF AVAILABLE gl-ctrl THEN 
                DO:
                    ASSIGN 
                        v-trnum       = gl-ctrl.trnum + 1
                        gl-ctrl.trnum = v-trnum.
                    RELEASE gl-ctrl.       
                    LEAVE.
                END. /* IF AVAIL gl-ctrl */
            END. /* REPEAT */
        /* gdm - 11050906 */

        END.

        RUN run-report.

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
        END CASE.

        IF v-postable THEN 
        DO:    
            lv-post = NO.
            MESSAGE "Do you want to post voided checks?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE lv-post.

            IF lv-post THEN 
            DO:  
                IF aplockbx-log THEN 
                DO:
                    RUN create-bank-file (OUTPUT lv-bank-file).
                    MESSAGE "Check Register/Lock Box file is created into " 
                        aplockbx-path + lv-bank-file
                        VIEW-AS ALERT-BOX INFORMATION.
                END.
                RUN post-gl.
                RUN copy-report-to-audit-dir.
                MESSAGE "Posting complete. " VIEW-AS ALERT-BOX.
            END.
            ELSE RUN undo-trnum.
        END.
        ELSE 
        DO:
            MESSAGE "No Void Checks available for posting..." VIEW-AS ALERT-BOX ERROR.
            RUN undo-trnum.
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
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
        ASSIGN 
            lines-per-page = 68.
        DISPLAY lines-per-page WITH FRAME {&FRAME-NAME}.
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
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.
    tran-date = TODAY.

    FIND FIRST sys-ctrl WHERE
        sys-ctrl.company EQ cocode AND
        sys-ctrl.name    EQ "AUDITDIR"
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE sys-ctrl THEN 
    DO TRANSACTION:
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

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "VC5" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    {methods/nowait.i}

    RUN check-date.
    APPLY "entry" TO tran-date.

  
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

        RUN GL_CheckModClosePeriod(INPUT cocode, INPUT DATE(tran-date), INPUT "AP", OUTPUT cMessage, OUTPUT lSuccess ) .  
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
        targetfile = lv-audit-dir + "\AP\VC5\Run#"
                    + STRING(v-trnum) + ".txt"
        dirname1   = lv-audit-dir
        dirname2   = lv-audit-dir + "\AP"
        dirname3   = lv-audit-dir + "\AP\VC5".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-bank-file C-Win 
PROCEDURE create-bank-file :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-data-file AS CHARACTER NO-UNDO.

    DEFINE VARIABLE targetfile          AS CHARACTER FORMAT "X(50)" NO-UNDO.
    DEFINE VARIABLE dirname1            AS CHARACTER FORMAT "X(20)" NO-UNDO.
    DEFINE VARIABLE v-account           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-ref               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-check-date        AS DATE      NO-UNDO.
    DEFINE VARIABLE v-check-date-string AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-total-amt         AS DECIMAL   NO-UNDO.

    ASSIGN 
        targetfile = aplockbx-path +
                     "CheckRegister" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") +
                      STRING(DAY(TODAY),"99") + STRING(TIME) + ".txt"
        dirname1   = aplockbx-path
        .

    IF SEARCH(dirname1) EQ ? THEN 
    DO:
        OS-CREATE-DIR VALUE(dirname1).
    END.


    OUTPUT TO VALUE(targetfile).
    PUT UNFORMATTED 
        "01021226C3648091" SKIP.
    v-total-amt = 0.

    FOR EACH ap-pay WHERE ap-pay.company = cocode AND
        ap-pay.cleared = ? AND
        ap-pay.reconciled = NO NO-LOCK:

        FIND FIRST vend WHERE vend.company = ap-pay.company
            AND vend.vend-no = ap-pay.vend-no NO-LOCK NO-ERROR.

        FIND FIRST bank WHERE bank.company = cocode AND
            bank.bank-code = ap-pay.bank-code NO-LOCK NO-ERROR.

        ASSIGN
            v-account           = IF AVAILABLE bank THEN bank.bk-act ELSE ""
            v-ref               = IF AVAILABLE vend THEN SUBSTRING(vend.name,1,12) ELSE ""
            v-check-date        = ap-pay.check-date
            v-check-date-string = STRING(MONTH(v-check-date),"99") +
                            STRING(DAY(v-check-date),"99") + 
                            SUBstring(STRING(YEAR(v-check-date),"9999"),3,2)
            v-total-amt         = v-total-amt + ap-pay.check-amt.
        PUT UNFORMATTED 
            "V"
            ap-pay.check-no FORM "9999999999"
            v-account FORM "99999999999999"
            ap-pay.check-amt * 100 FORM "9999999999"
            v-ref FORM  "x(12)"
            v-check-date-string FORM "x(6)"
            SPACE(25)
            "38"  /*for void*/
            SKIP.

    END.
    PUT UNFORMATTED 
        "T          "
        v-account FORM "99999999999999"
        v-total-amt * 100 FORM "9999999999"
        SKIP.

    OUTPUT CLOSE.
    op-data-file = TARGETfile.

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
    DISPLAY tran-date tran-period rd-dest tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 tran-date rd-dest tbAutoClose btn-ok btn-cancel 
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
    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

    IF init-dir = "" THEN init-dir = "c:\temp" .
    SYSTEM-DIALOG GET-FILE list-name
        TITLE      "Enter Listing Name to SAVE AS ..."
        FILTERS    "Listing Files (*.rpt)" "*.rpt",
        "All Files (*.*)" "*.*"
        INITIAL-DIR init-dir
        ASK-OVERWRITE
        SAVE-AS
        USE-FILENAME

        UPDATE OKpressed.

    IF NOT OKpressed THEN  RETURN NO-APPLY.


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

    DEFINE VARIABLE lv-orint  AS INTEGER   NO-UNDO.
    lv-orint = IF lv-ornt BEGINS "L" THEN 3 ELSE 1.

    RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
        INPUT lv-font-no, INPUT lv-orint, INPUT 0, INPUT 0, OUTPUT result).

    IF NOT RESULT THEN v-postable = NO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-gl C-Win 
PROCEDURE post-gl :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-bank-amt     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-tot-amt-paid LIKE ap-pay.check-amt NO-UNDO.
    DEFINE VARIABLE v-tot-amt-disc LIKE ap-payl.amt-disc NO-UNDO.

    /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
    ASSIGN 
        uperiod = tran-period
        udate   = tran-date.

    EMPTY TEMP-TABLE w-disb.

    postit:
    DO TRANSACTION ON ERROR UNDO, LEAVE:
        FIND FIRST ap-ctrl WHERE ap-ctrl.company = cocode NO-LOCK NO-ERROR.

        FOR EACH ap-pay WHERE ap-pay.company = cocode AND
            ap-pay.cleared = ? AND
            ap-pay.reconciled = NO
            ON ERROR UNDO postit, LEAVE postit
            BREAK BY ap-pay.bank-code:

            ASSIGN 
                ap-pay.cleared          = NO
                ap-pay.reconciled       = ?
                ap-pay.transactionDate  = udate
                v-tot-amt-paid          = v-tot-amt-paid + ap-pay.check-amt
                v-bank-amt              = v-bank-amt + ap-pay.check-amt.

            FIND FIRST bank WHERE bank.company = cocode AND
                bank.bank-code = ap-pay.bank-code NO-ERROR.
            IF AVAILABLE bank THEN
                bank.bal = bank.bal + ap-pay.check-amt.

            CREATE ap-ledger.
            ASSIGN
                ap-ledger.company  = ap-pay.company
                ap-ledger.vend-no  = ap-pay.vend-no
                ap-ledger.refnum   = "VOIDED CHECK" + string(ap-pay.check-no, "zzzzzzz9")
                ap-ledger.ref-date = TODAY
                ap-ledger.tr-date  = udate
                ap-ledger.trnum    = v-trnum
                ap-ledger.period   = tran-period
                ap-ledger.amt      = ap-ledger.amt - ap-pay.check-amt
                ap-ledger.actnum   = bank.actnum.
            RELEASE ap-ledger.

            EMPTY TEMP-TABLE xpayl.

            FOR EACH ap-payl NO-LOCK WHERE ap-payl.c-no = ap-pay.c-no:
                CREATE xpayl.                    
                xpayl.recnum = RECID(ap-payl).   

                FIND FIRST ap-inv WHERE ap-inv.company = cocode AND
                    ap-inv.vend-no = ap-pay.vend-no AND
                    ap-inv.inv-no = ap-payl.inv-no NO-ERROR.
                IF AVAILABLE ap-inv THEN
                DO:
                    FIND vend WHERE vend.company = cocode AND
                        vend.vend-no = ap-inv.vend-no
                        EXCLUSIVE-LOCK NO-ERROR.
                    ASSIGN
                        ap-inv.paid       = ap-inv.paid - ap-payl.amt-paid
                        ap-inv.disc-taken = ap-inv.disc-taken - ap-payl.amt-disc
                        ap-inv.due        = ap-inv.due + ap-payl.amt-paid + ap-payl.amt-disc.
                    IF AVAILABLE vend THEN
                        vend.acc-bal = vend.acc-bal + ap-payl.amt-paid +
                            ap-payl.amt-disc.
                END. /* if avail ap-inv .. */
                
                v-tot-amt-disc = v-tot-amt-disc + ap-payl.amt-disc.

                IF ap-payl.d-no NE 0 THEN 
                DO:
                    CREATE w-disb.
                    ASSIGN
                        w-actnum   = ap-payl.actnum
                        w-amt-paid = ap-payl.amt-paid
                        w-amt-disc = ap-payl.amt-disc
                        w-disb.cDesc = "Vendor:" + string(vend.vend-no,"x(8)") + " Inv:" + STRING(ap-inv.inv-no,"99999999") + " Check: " + STRING(ap-payl.check-no,"999999999999")
                        .
                END.
                
                RELEASE ap-inv.
                RELEASE vend.
            END. /* for each ap-payl record */

            FOR EACH xpayl,
                FIRST ap-payl WHERE RECID(ap-payl) EQ xpayl.recnum
                NO-LOCK:

                FIND LAST xap-payl WHERE xap-payl.c-no EQ ap-payl.c-no
                    USE-INDEX c-no NO-LOCK NO-ERROR.
                x = IF AVAILABLE xap-payl THEN xap-payl.line ELSE 0.

                CREATE xap-payl.

                ASSIGN
                    xap-payl.c-no      = ap-payl.c-no
                    xap-payl.line      = x + 1
                    xap-payl.actnum    = ap-payl.actnum
                    xap-payl.amt-disc  = -(ap-payl.amt-disc)
                    xap-payl.amt-due   = ap-payl.amt-due + ap-payl.amt-paid +
                                                   ap-payl.amt-disc
                    xap-payl.amt-paid  = -(ap-payl.amt-paid)
                    xap-payl.check-no  = ap-payl.check-no
                    xap-payl.due-date  = ap-payl.due-date
                    xap-payl.inv-no    = ap-payl.inv-no
                    xap-payl.man-check = ap-payl.man-check
                    xap-payl.memo      = ap-payl.memo
                    xap-payl.posted    = ap-payl.posted
                    xap-payl.vend-no   = ap-payl.vend-no.
                RELEASE xap-payl.
            END.  /* for each xpayl */
            IF LAST-OF(ap-pay.bank-code) THEN 
            DO:
                RUN GL_SpCreateGLHist(cocode,
                    bank.actnum,
                    "APVOIDCK",
                    "AP VOIDED CHECK REGISTER",
                    udate,
                    v-bank-amt,
                    v-trnum,
                    tran-period,
                    "A",
                    udate,
                    "Vendor:" + string(ap-pay.vend-no,"x(8)") + " Check:" + STRING(ap-pay.check-no,"999999999999"),
                    "AP").
          
                ASSIGN 
                    v-bank-amt = 0.
            END.

            RELEASE bank.
        END. /* for each ap-pay record */

        FIND CURRENT ap-pay NO-LOCK NO-ERROR.


        IF v-tot-amt-disc NE 0 THEN 
        DO:
            RUN GL_SpCreateGLHist(cocode,
                ap-ctrl.discount,
                "APVOIDCK",
                "AP VOIDED CHECK REGISTER",
                udate,
                v-tot-amt-disc,
                v-trnum,
                tran-period,
                "A",
                udate,
                (IF AVAILABLE ap-pay THEN "Vendor" + string(ap-pay.vend-no,"x(8)") ELSE ""),
                "AP").
        END.

        FOR EACH w-disb BREAK BY w-actnum:
            ASSIGN
                v-tot-amt-paid = v-tot-amt-paid - w-amt-paid
                v-tot-amt-disc = v-tot-amt-disc - w-amt-disc.

            ACCUMULATE w-amt-paid (sub-total by w-actnum).
            ACCUMULATE w-amt-disc (sub-total by w-actnum).

            IF LAST-OF(w-actnum) THEN 
            DO:
                RUN GL_SpCreateGLHist(cocode,
                    w-actnum,
                    "APVOIDCK",
                    "AP VOIDED CHECK REGISTER",
                    udate,
                    (((ACCUM SUB-TOTAL BY w-actnum w-amt-paid) +
                    (ACCUM SUB-TOTAL BY w-actnum w-amt-disc)) * -1),
                    v-trnum,
                    tran-period,
                    "A",
                    udate,
                    w-disb.cDesc,
                    "AP").
            END.
        END.
        RUN GL_SpCreateGLHist(cocode,
            ap-ctrl.payables,
            "APVOIDCK",
            "AP VOIDED CHECK REGISTER",
            udate,
            ((v-tot-amt-paid + v-tot-amt-disc) * -1),
            v-trnum,
            tran-period,
            "A",
            udate,
            (IF AVAIL ap-pay THEN "Vendor:" + string(ap-pay.vend-no,"x(8)") ELSE ""),
            "AP").

    END. /* postit */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ---------------------------------------------------- oe/invpost.p 10/94 gb */
    /* Invoicing  - Edit Register & Post Invoicing Transactions                   */
    /* -------------------------------------------------------------------------- */

    FORM HEADER
        "Check #" AT 1
        "Date" AT 17
        "Amount" AT 33
        "Vendor" AT 59 SKIP
        FILL("_",132) FORMAT "x(130)" SKIP
        WITH STREAM-IO FRAME f-top WIDTH 132 NO-BOX NO-LABELS NO-UNDERLINE.

    ASSIGN
        time_stamp = STRING(TIME, "hh:mmam")
        tmpstore   = FILL("_",125).             

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE("general").

    ASSIGN
        str-tit  = coname + " - " + loname                                    
        str-tit2 = "A/P VOIDED CHECK REGISTER " + STRING(v-trnum)
        str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date)
        x        = (112 - length(str-tit)) / 2
        str-tit  = FILL(" ",x) + str-tit
        x        = (114 - length(str-tit2)) / 2
        str-tit2 = FILL(" ",x) + str-tit2
        x        = (132 - length(str-tit3)) / 2
        str-tit3 = FILL(" ",x) + str-tit3 .

    DISPLAY "" WITH FRAME r-top.
    DISPLAY "" WITH FRAME f-top.

    v-postable = NO.

    FOR EACH ap-pay WHERE ap-pay.company = cocode AND
        ap-pay.cleared = ? AND
        ap-pay.reconciled = NO
        NO-LOCK
        BREAK BY ap-pay.check-no:
        FIND FIRST vend WHERE vend.company = cocode AND
            vend.vend-no = ap-pay.vend-no NO-LOCK NO-ERROR.
        DISPLAY ap-pay.check-no
            ap-pay.check-date
            ap-pay.check-amt
            ap-pay.vend-no
            vend.name 
            WHEN AVAILABLE vend
            WITH FRAME report-lines WIDTH 132 NO-BOX NO-LABELS STREAM-IO.
        DOWN WITH FRAME report-lines.
        ASSIGN 
            v-postable = YES.
    END. /* for each ap-pay record */

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
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + ",".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undo-trnum C-Win 
PROCEDURE undo-trnum :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO TRANSACTION:
        /* gdm - 11050906 */
        REPEAT:
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
            IF AVAILABLE gl-ctrl THEN 
            DO:
                IF v-trnum EQ gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
                RELEASE gl-ctrl.
                LEAVE.
            END. /* IF AVAIL gl-ctrl */
        END. /* REPEAT */
    /* gdm - 11050906 */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

