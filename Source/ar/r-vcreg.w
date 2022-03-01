&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-vcreg.w

  Description: Voided Cash Receipts Register

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
DEFINE VARIABLE xar-acct     AS CHARACTER NO-UNDO.
DEFINE VARIABLE xdis-acct    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-audit-dir AS CHARACTER NO-UNDO.

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
DEFINE BUFFER xar-cashl FOR ar-cashl.    

DEFINE NEW SHARED VARIABLE v-trnum    AS INTEGER NO-UNDO.    
DEFINE            VARIABLE v-postable AS LOG     INIT NO NO-UNDO.

DEFINE            VARIABLE v-post     AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE xcashl NO-UNDO 
    FIELD recnum AS RECID.

{sys/form/r-top3w.f}

FORM ar-cash.check-no AT 1 FORMAT "zzzzzzz9"
    ar-cash.check-date AT 17
    ar-cash.check-amt AT 33 FORMAT "-ZZ,ZZZ,ZZ9.99"
    ar-cash.cust-no AT 59 SPACE(1) cust.name
    WITH FRAME report-lines WIDTH 132 NO-BOX NO-LABELS STREAM-IO.

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
    SIZE 91 BY 5.48.

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
    lines-per-page AT ROW 7.57 COL 86.4 COLON-ALIGNED
    rd-dest AT ROW 7.71 COL 4.8 NO-LABELS
    lv-font-no AT ROW 7.71 COL 29.6 COLON-ALIGNED
    lv-ornt AT ROW 7.71 COL 40.6 NO-LABELS
    lv-font-name AT ROW 8.71 COL 28.4 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 10.48 COL 31.4
    tbAutoClose AT ROW 11.81 COL 31.4 WIDGET-ID 64
    btn-ok AT ROW 12.81 COL 31.2
    btn-cancel AT ROW 12.81 COL 53.8
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 6.95 COL 4
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.05 COL 4
    RECT-6 AT ROW 7.38 COL 3
    RECT-7 AT ROW 1.48 COL 3
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
        TITLE              = "A/R Voided Cash Receipt Register"
        HEIGHT             = 13.38
        WIDTH              = 95
        MAX-HEIGHT         = 20.29
        MAX-WIDTH          = 105.2
        VIRTUAL-HEIGHT     = 20.29
        VIRTUAL-WIDTH      = 105.2
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
ON END-ERROR OF C-Win /* A/R Voided Cash Receipt Register */
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
ON WINDOW-CLOSE OF C-Win /* A/R Voided Cash Receipt Register */
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
        DEFINE VARIABLE lv-post AS LOG NO-UNDO.

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
                    FIND CURRENT gl-ctrl NO-LOCK.
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

            MESSAGE "Do you want to post voided cash receipts?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE lv-post.

            IF lv-post THEN 
            DO:  
                RUN post-gl.
                RUN copy-report-to-audit-dir.
                MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
            END.
            ELSE RUN undo-trnum.
        END.

        ELSE 
        DO:
            MESSAGE "No Voided Cash Receipts available for posting..." VIEW-AS ALERT-BOX ERROR.
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
        DEFINE VARIABLE char-val AS cha NO-UNDO.

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

    RUN init-proc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN.

    tran-date = TODAY.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "AC8" }
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
        targetfile = lv-audit-dir + "\AR\AC8\Run#"
                    + STRING(v-trnum) + ".txt"
        dirname1   = lv-audit-dir
        dirname2   = lv-audit-dir + "\AR"
        dirname3   = lv-audit-dir + "\AR\AC8".

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
    DISPLAY tran-date tran-period rd-dest tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 tran-date rd-dest tbAutoClose btn-ok btn-cancel 
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
    ASSIGN
        xar-acct  = ar-ctrl.receivables
        xdis-acct = ar-ctrl.discount.

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

    /* Use Progress Print. Always use Font#9 in Registry (set above) */
    DEFINE VARIABLE lv-orint  AS INTEGER   NO-UNDO.
    lv-orint = IF lv-ornt BEGINS "L" THEN 3 ELSE 1.

    RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
        INPUT lv-font-no, INPUT lv-orint, INPUT 0, INPUT 0, OUTPUT result).
    /* use-dialog(1) and landscape(2) */



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
 /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
 DEF VAR t1 AS DEC NO-UNDO.
 DEF VAR lv-rowid AS ROWID NO-UNDO.

 postit:
 do transaction on error undo, leave:

   for each ar-cash EXCLUSIVE-LOCK where
       ar-cash.company = cocode and
       ar-cash.cleared = ? and
       ar-cash.posted = YES, 
       FIRST cust WHERE
             cust.company EQ cocode AND
             cust.cust-no EQ ar-cash.cust-no
        EXCLUSIVE-LOCK
        on error undo postit, leave postit:

       assign t1 = 0
              ar-cash.cleared = NO
              ar-cash.reconciled = ?.

       find first bank where
            bank.company = cocode and
            bank.bank-code = ar-cash.bank-code
            no-error.

       if AVAIL bank then
       DO:
          bank.bal = bank.bal - ar-cash.check-amt.
          RELEASE bank.
       END.

       EMPTY TEMP-TABLE xcashl.

       for each ar-cashl where ar-cashl.c-no = ar-cash.c-no
           EXCLUSIVE-LOCK:

           create xcashl.   

           xcashl.recnum = recid(ar-cashl).   

           IF ar-cashl.inv-no NE 0 AND ar-cashl.on-account EQ NO THEN DO:

              find first ar-inv where
                   ar-inv.company = cocode and
                   ar-inv.inv-no = ar-cashl.inv-no
                   EXCLUSIVE-LOCK no-error.

              if AVAIL ar-inv then
              do:
                 assign
                 ar-inv.paid = ar-inv.paid - ar-cashl.amt-paid - ar-cashl.amt-disc
                 ar-inv.disc-taken = ar-inv.disc-taken + ar-cashl.amt-disc
                 ar-inv.due = ar-inv.due + ar-cashl.amt-paid + ar-cashl.amt-disc.

                 RELEASE ar-inv.

              end. /* if avail ar-inv  */

           END. /*inv-no ne 0 AND ar-cashl.on-account EQ NO*/

           IF ar-cashl.inv-no EQ 0 AND ar-cashl.on-account EQ YES THEN
              cust.on-account = cust.on-account - ar-cashl.amt-paid.

          RUN GL_SpCreateGLHist(cocode,
                             ar-cashl.actnum,
                             "CASHRVD",
                             "VOID " + cust.cust-no + " " +
                                 STRING(ar-cash.check-no,"999999999999") +
                                 " Inv: " + STRING(ar-cashl.inv-no),
                             tran-date,
                             (-1 * ar-cashl.amt-paid),
                             v-trnum,
                             tran-period,
                             "A",
                             tran-date,
                             "Void Payment Check#:" + STRING(ar-cash.check-no,"999999999999") + " Date: " + STRING(tran-date),
                             "AR").

           ASSIGN
             t1 = t1 + ar-cashl.amt-paid + ar-cashl.amt-disc.

           IF ar-cashl.amt-disc NE 0 THEN DO:
               RUN GL_SpCreateGLHist(cocode,
                                  xdis-acct,
                                  "CRDISVD",
                                  "VOID " + cust.cust-no + " " +
                                          STRING(ar-cash.check-no,"999999999999") +
                                          " Inv: " + STRING(ar-cashl.inv-no),
                                  tran-date,
                                  (-1 * ar-cashl.amt-disc),
                                  v-trnum,
                                  tran-period,
                                  "A",
                                  tran-date,
                                  "Void Payment Check#:" + STRING(ar-cash.check-no,"999999999999") + " Date: " + STRING(tran-date),
                                  "AR").
             

                    CREATE ar-ledger.
                    ASSIGN
                        ar-ledger.company  = cocode
                        ar-ledger.cust-no  = ar-cash.cust-no
                        ar-ledger.amt      = -1 * ar-cashl.amt-disc
                        ar-ledger.ref-num  = "DISC VD" +
                                   STRING(ar-cash.check-no,"999999999999") +
                                   "-" + STRING(ar-cashl.line,"9999999999")
                        ar-ledger.ref-date = ar-cash.check-date
                        ar-ledger.tr-date  = tran-date
                        ar-ledger.tr-num   = v-trnum.

                    RELEASE ar-ledger.
                END.
            END. /* for each ar-cashl record */

            cust.acc-bal = cust.acc-bal + t1.

            IF cust.acc-bal GE cust.hibal THEN
                ASSIGN
                    cust.hibal      = cust.acc-bal
                    cust.hibal-date = tran-date.
             
            xar-acct = STRING(DYNAMIC-FUNCTION("GL_GetAccountAR", cust.company, cust.cust-no)).

            IF t1 NE 0 THEN 
            DO:
                FIND glhist WHERE ROWID(glhist) EQ lv-rowid NO-ERROR.
                IF NOT AVAILABLE glhist THEN 
                DO:
                    CREATE glhist.
                    ASSIGN
                        glhist.company   = cocode
                        glhist.actnum    = xar-acct
                        glhist.jrnl      = "CASHRVD"
                        glhist.tr-dscr   = "CASH RECEIPTS VOID"
                        glhist.tr-date   = tran-date
                        glhist.period    = tran-period
                        glhist.tr-num    = v-trnum   
                        glhist.glYear    = IF AVAILABLE period THEN period.yr ELSE YEAR(tran-date)
                        glhist.yr        = YEAR(tran-date)
                        glhist.module    = "AR"
                        glhist.posted    = NO
                        glhist.entryType = "A"
                        glhist.documentID = "Void Payment Check#:" + STRING(ar-cash.check-no,"999999999999") + " Date: " + STRING(tran-date)
                        glhist.sourceDate = tran-date
                        lv-rowid         = ROWID(glhist).
                END.
                ELSE ASSIGN 
                    glhist.company = cocode
                    glhist.actnum  = xar-acct
                    glhist.jrnl    = "CASHRVD"
                    glhist.tr-dscr = "CASH RECEIPTS VOID"
                    glhist.tr-date = tran-date
                    glhist.period  = tran-period
                    glhist.tr-num  = v-trnum   
                    glhist.glYear  = IF AVAIL period THEN period.yr ELSE YEAR(tran-date)
                    glhist.yr      = YEAR(tran-date)
                    glhist.module  = "AR"
                    glhist.posted  = NO
                    glhist.entryType = "A"
                    glhist.documentID = "Void Payment Check#:" + STRING(ar-cash.check-no,"999999999999") + " Date: " + STRING(tran-date)
                    glhist.sourceDate = tran-date
                    lv-rowid       = ROWID(glhist).
                
                glhist.tr-amt = glhist.tr-amt + t1.

                RELEASE glhist.
            END.

            create ar-ledger.
            assign
                ar-ledger.company = cocode
                ar-ledger.cust-no = ar-cash.cust-no
                ar-ledger.amt = -1 * ar-cash.check-amt
                ar-ledger.ref-num = "VOIDED CHK# " + STRING(ar-cash.check-no,"999999999999")
                ar-ledger.ref-date = ar-cash.check-date
                ar-ledger.tr-date = tran-date
                ar-ledger.tr-num = v-trnum.

            RELEASE ar-ledger.

            for each xcashl,
            first ar-cashl where recid(ar-cashl) eq xcashl.recnum
            no-lock:

                find last xar-cashl where xar-cashl.c-no eq ar-cashl.c-no
                use-index c-no no-lock no-error.
                x = if avail xar-cashl then xar-cashl.line else 0.

                create xar-cashl.

                BUFFER-COPY ar-cashl EXCEPT LINE amt-disc amt-due amt-paid rec_key
                   TO xar-cashl 
                assign
                    xar-cashl.line       = x + 1
                    xar-cashl.amt-due    = -(ar-cashl.amt-due)
                    xar-cashl.amt-disc   = -(ar-cashl.amt-disc)
                    xar-cashl.amt-paid   = -(ar-cashl.amt-paid).
                ASSIGN
                    xar-cashl.voided = YES.
                xar-cashl.voidDate = tran-date.
 
                RELEASE xar-cashl.
            END.  /* for each xcashl */
        END. /* for each ar-cash record */
    END. /* postit */

    FIND CURRENT cust NO-LOCK NO-ERROR.
    FIND CURRENT ar-cashl NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    FORM HEADER
        "Check #" AT 1
        "Date" AT 17
        "Amount" AT 33
        "Customer" AT 59 SKIP
        FILL("_",132) FORMAT "x(130)" SKIP
        WITH STREAM-IO FRAME f-top WIDTH 132 NO-BOX NO-LABELS NO-UNDERLINE.

    ASSIGN
        tmpstore = FILL("_",125).             

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE("general").

    ASSIGN
        str-tit    = coname + " - " + loname                                    
        str-tit2   = "A/R VOIDED CASH RECEIPT REGISTER " + STRING(v-trnum)
        str-tit3   = "Period " + string(tran-period,"99") + " " + string(tran-date)
        x          = (112 - length(str-tit)) / 2
        str-tit    = FILL(" ",x) + str-tit
        x          = (114 - length(str-tit2)) / 2
        str-tit2   = FILL(" ",x) + str-tit2
        x          = (132 - length(str-tit3)) / 2
        str-tit3   = FILL(" ",x) + str-tit3
        v-postable = NO.

    DISPLAY "" WITH FRAME r-top.
    DISPLAY "" WITH FRAME f-top.


    FOR EACH ar-cash FIELDS(company cleared posted check-no check-date check-amt
        cust-no) WHERE
        ar-cash.company = cocode AND
        ar-cash.cleared = ? AND
        ar-cash.posted = YES
        NO-LOCK
        BREAK BY ar-cash.check-no:

        FIND FIRST cust WHERE
            cust.company = cocode AND
            cust.cust-no = ar-cash.cust-no
            NO-LOCK NO-ERROR.

        DISPLAY ar-cash.check-no FORMAT "ZZZZZZZZZZZ9"
            ar-cash.check-date
            ar-cash.check-amt
            ar-cash.cust-no
            cust.name 
            WHEN AVAILABLE cust
            WITH FRAME report-lines WIDTH 132 NO-BOX NO-LABELS STREAM-IO.
        DOWN WITH FRAME report-lines.

        v-postable = YES.
    END. /* for each ar-cash record */

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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS cha     NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS cha     NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER NO-UNDO.
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
        /* gdm - 11050906 */
        REPEAT:
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
            IF AVAILABLE gl-ctrl THEN 
            DO:
                IF v-trnum EQ gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
                FIND CURRENT gl-ctrl NO-LOCK.
                RELEASE gl-ctrl.
                LEAVE.
            END. /* IF AVAIL gl-ctrl */
        END. /* REPEAT */
    /* gdm - 11050906 */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

