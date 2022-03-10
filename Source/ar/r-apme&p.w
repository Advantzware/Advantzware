&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ar\r-apme&p.w

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
    
DEFINE BUFFER b-line FOR ar-cashl.
/*
if v-return then return.
*/
DEFINE VARIABLE v-invalid    AS LOG       NO-UNDO.
DEFINE VARIABLE v-postable   AS LOG       NO-UNDO.

DEFINE VARIABLE g1           AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE g2           LIKE g1 NO-UNDO.
DEFINE VARIABLE t3           LIKE g1 NO-UNDO.
DEFINE VARIABLE v1           LIKE g1 NO-UNDO.
DEFINE VARIABLE v2           LIKE g1 NO-UNDO.
DEFINE VARIABLE t2           LIKE g1 NO-UNDO.
DEFINE VARIABLE invo-sub     LIKE ar-inv.net NO-UNDO.
DEFINE VARIABLE disc-sub     LIKE ar-inv.disc-taken NO-UNDO.
DEFINE VARIABLE xtrnum       AS INTEGER   NO-UNDO.
DEFINE VARIABLE xar-acct     AS CHARACTER NO-UNDO.
DEFINE VARIABLE xdis-acct    AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcust        LIKE cust.cust-no NO-UNDO.
DEFINE VARIABLE xchk         LIKE ar-cashl.check-no NO-UNDO.
DEFINE VARIABLE tmp-amt-paid LIKE ar-cashl.amt-paid NO-UNDO.
DEFINE VARIABLE alf-check    AS CHARACTER FORMAT "x(12)" NO-UNDO.
DEFINE VARIABLE v-non-zero   AS LOG       NO-UNDO.
DEFINE VARIABLE fcust        LIKE ar-cashl.cust-no INIT "" NO-UNDO.
DEFINE VARIABLE tcust        LIKE fcust INIT "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE tmp-dir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE time_stamp   AS ch        NO-UNDO.
DEFINE VARIABLE dsc          AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE net-cr       AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE ck-onac      AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.

DO TRANSACTION:
    {sys/inc/postdate.i}
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
rd-dest tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_cust end_cust ~
rd-dest tbAutoClose 

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

DEFINE VARIABLE begin_cust     AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust       AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

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
    SIZE 91 BY 5.95.

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
    tran-date AT ROW 2.52 COL 39 COLON-ALIGNED
    tran-period AT ROW 3.71 COL 39 COLON-ALIGNED
    begin_cust AT ROW 5.14 COL 28.2 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust AT ROW 5.14 COL 68.2 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    rd-dest AT ROW 8.14 COL 4.8 NO-LABELS
    lv-font-no AT ROW 8.14 COL 33 COLON-ALIGNED
    lv-ornt AT ROW 8.14 COL 43 NO-LABELS
    lines-per-page AT ROW 8.14 COL 87 COLON-ALIGNED
    lv-font-name AT ROW 9.33 COL 29 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 10.86 COL 31
    tbAutoClose AT ROW 12.43 COL 31 WIDGET-ID 64
    btn-ok AT ROW 13.48 COL 30.8
    btn-cancel AT ROW 13.48 COL 51.8
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.05 COL 4
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 7.43 COL 4
    RECT-6 AT ROW 7.86 COL 3
    RECT-7 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 15.29
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
        TITLE              = "On Account Application Register"
        HEIGHT             = 13.95
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
    begin_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* On Account Application Register */
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
ON WINDOW-CLOSE OF C-Win /* On Account Application Register */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
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
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.

        DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
            /* gdm - 11050906 */
            REPEAT:
                FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                    WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
                IF AVAILABLE gl-ctrl THEN 
                DO:
                    ASSIGN 
                        xtrnum        = gl-ctrl.trnum + 1
                        gl-ctrl.trnum = xtrnum.
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

            MESSAGE "Post Invoices?"
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
            MESSAGE "No Cash Receipts available for posting..." VIEW-AS ALERT-BOX ERROR.
            RUN undo-trnum.
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
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
    {sys/inc/reportsConfigNK1.i "AC6" }
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE alf-check C-Win 
PROCEDURE alf-check :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    ASSIGN
        alf-check  = FILL(" ",12 - length(TRIM(alf-check))) + trim(alf-check)
        v-non-zero = NO.
         
    DO i = 1 TO LENGTH(alf-check):
        IF substr(alf-check,i,1) NE "0" AND
            substr(alf-check,i,1) NE " " THEN v-non-zero = YES.
        ELSE
            IF NOT v-non-zero THEN substr(alf-check,i,1) = " ".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
            IF NOT period.pstat THEN 
            DO:
                MESSAGE "Period Already Closed. " VIEW-AS ALERT-BOX ERROR.
                v-invalid = YES.
            END.
            tran-period:SCREEN-VALUE = STRING(period.pnum).
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
        targetfile = lv-audit-dir + "\AR\AC6\Run#"
                    + STRING(xtrnum) + ".txt"
        dirname1   = lv-audit-dir
        dirname2   = lv-audit-dir + "\AR"
        dirname3   = lv-audit-dir + "\AR\AC6".

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
    DISPLAY tran-date tran-period begin_cust end_cust rd-dest tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 tran-date begin_cust end_cust rd-dest tbAutoClose btn-ok 
        btn-cancel 
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

    DO :
        FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK.
        ASSIGN
            xar-acct  = ar-ctrl.receivables
            xdis-acct = ar-ctrl.discount.
        RELEASE ar-ctrl.
        FIND FIRST account
            WHERE account.company EQ cocode
            AND account.actnum  EQ xar-acct
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE account OR account.actnum = "" THEN 
        DO:
            MESSAGE " Receivables Account is blank or is not on file for this Company."
                VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.
        FIND FIRST account
            WHERE account.company EQ cocode
            AND account.actnum  EQ xdis-acct
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE account OR account.actnum = "" THEN 
        DO:    
            MESSAGE " Discount Account is blank or is not on file for this Company." VIEW-AS ALERT-BOX ERROR.    
            RETURN ERROR.
        END.
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
        /*     CREATE-TEST-FILE*/
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
    RUN scr-rpt-d.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-gl C-Win 
PROCEDURE post-gl :
    DO TRANSACTION:
        FOR EACH ar-cashl
            WHERE ar-cashl.company    EQ cocode
            AND ar-cashl.cust-no    GE fcust
            AND ar-cashl.cust-no    LE tcust
            AND ar-cashl.on-account EQ YES
            AND ar-cashl.inv-no     NE 0,

            FIRST ar-cash
            WHERE ar-cash.c-no EQ ar-cashl.c-no
            NO-LOCK

            BREAK BY ar-cashl.cust-no
            BY ar-cashl.check-no
            BY ar-cashl.inv-no:

            {ar/ar-oreg.i ar-cashl 1}

            ar-cashl.posted = YES.
        END.
    END. /* postit: transaction */

    FIND CURRENT ar-cashl NO-LOCK NO-ERROR.
    FIND CURRENT ar-inv NO-LOCK NO-ERROR.
    FIND CURRENT cust NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ---------------------------------------------------- ar/ar-creg.p 10/94 gb */
    /* AR Cash  - Edit Register & Post Transactions                   */
    /* -------------------------------------------------------------------------- */

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    v-postable = NO.

    FORM HEADER
        "CUST.#   NAME                                    CHECK#      DATE         "
        "ON ACCOUNT       INVOICE #          APPLIED     DISCOUNT"
        SKIP FILL("_",132) FORMAT "x(132)"

        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top PAGE-TOP WIDTH 132 STREAM-IO.

    ASSIGN
        time_stamp = STRING(TIME, "HH:MMam")
        tmpstore   = FILL("_",132).

    {sys/form/r-top3w.f}

    ASSIGN 
        fcust = begin_cust
        tcust = END_cust.
    SESSION:SET-WAIT-STATE("general").

    ASSIGN
        str-tit  = coname + " - " + loname
        str-tit2 = "CASH ON ACCOUNT APPLICATION  -  EDIT REGISTER " +
                string(xtrnum)
        str-tit3 = "Period " + string(tran-period,"99") + " - " + string(tran-date)
        x        = (112 - length(str-tit)) / 2
        str-tit  = FILL(" ",x) + str-tit
        x        = (114 - length(str-tit2)) / 2
        str-tit2 = FILL(" ",x) + str-tit2
        x        = (132 - length(str-tit3)) / 2
        str-tit3 = FILL(" ",x) + str-tit3.


    DISPLAY "" WITH FRAME r-top.
    DISPLAY "" WITH FRAME f-top.

    FOR EACH ar-cashl
        WHERE ar-cashl.company    EQ cocode
        AND ar-cashl.cust-no    GE fcust
        AND ar-cashl.cust-no    LE tcust
        AND ar-cashl.on-account EQ YES
        AND ar-cashl.inv-no     NE 0
        AND (ar-cashl.amt-paid  NE 0 OR
        ar-cashl.amt-disc  NE 0)
        NO-LOCK,

        FIRST ar-cash
        WHERE ar-cash.c-no EQ ar-cashl.c-no
        NO-LOCK,

        FIRST cust
        WHERE cust.company EQ ar-cash.company
        AND cust.cust-no EQ ar-cash.cust-no
        NO-LOCK

        BREAK BY ar-cashl.cust-no BY ar-cashl.check-no

        WITH FRAME a1:

        IF FIRST-OF(ar-cashl.cust-no) THEN 
        DO:
            xcust = cust.cust-no.
            PUT cust.cust-no SPACE(1)
                cust.name.
        END.

        ELSE IF FIRST-OF(ar-cashl.check-no) THEN PUT SKIP(1).

        IF FIRST-OF(ar-cashl.check-no) THEN 
        DO:
            PUT SKIP(1).
            ASSIGN
                xchk    = ar-cashl.check-no
                ck-onac = 0.
            FOR EACH b-line FIELDS(amt-paid amt-disc)
                WHERE b-line.company    EQ cocode
                AND b-line.cust-no    EQ xcust
                AND b-line.on-account EQ YES
                AND b-line.check-no   EQ xchk:
                ck-onac = ck-onac + b-line.amt-paid - b-line.amt-disc.
            END.
        
            alf-check = STRING(ar-cashl.check-no,"999999999999").
        
            RUN alf-check.
         
            DO i = 1 TO LENGTH(alf-check):
                IF substr(alf-check,i,1) NE "0" THEN v-non-zero = YES.
                ELSE
                    IF NOT v-non-zero THEN substr(alf-check,i,1) = " ".
            END.
        
            PUT alf-check TO 53
                ar-cash.check-date  AT 60
                ck-onac AT 72
                ar-cashl.inv-no FORMAT ">>>>>>>9" AT 94.
            
            v2 = v2 + ck-onac.
        END.

        ELSE PUT ar-cashl.inv-no FORMAT ">>>>>>>9" AT 94.

        IF ar-cashl.amt-paid NE 0 THEN PUT SPACE(3) ar-cashl.amt-paid.
        ELSE PUT SPACE(17).

        IF ar-cashl.amt-disc NE 0 THEN PUT SPACE(3) ar-cashl.amt-disc SKIP.
        ELSE PUT SKIP.

        ASSIGN
            invo-sub = invo-sub + ar-cashl.amt-paid
            disc-sub = disc-sub + ar-cashl.amt-disc
            dsc      = dsc      + ar-cashl.amt-disc
            net-cr   = net-cr   + ar-cashl.amt-paid - ar-cashl.amt-disc.

        IF LAST-OF(ar-cashl.cust-no) THEN 
        DO:
            DISPLAY  "*  CUSTOMER TOTALS"  TO 70
                v2                    AT 72
                invo-sub              AT 104 SPACE(3)
                disc-sub
                "*"                   TO 132 SKIP(1)

                WITH FRAME vtot NO-BOX NO-LABELS WIDTH 132 STREAM-IO.

            ASSIGN
                g1       = g1 + invo-sub
                g2       = g2 + v2
                invo-sub = 0
                disc-sub = 0
                v1       = 0
                v2       = 0.
        END.
        v-postable = YES.
    END. /* each cashline */

    DO WITH FRAME gt WIDTH 132 NO-LABELS NO-UNDERLINE STREAM-IO:
        DISPLAY  "** BEGINNING CASH ON ACCOUNT"  TO 101 g2     TO 118
            "** GROSS AMOUNT APPLIED"       TO 101 net-cr TO 118
            "** TOTAL DISCOUNTS"            TO 101 dsc    TO 118
            "** NET AMOUNT APPLIED"         TO 101 net-cr - dsc    TO 118
            "** ENDING CASH ON ACCOUNT"     TO 101 g2 - (g1 - dsc) TO 118

            WITH NO-LABELS NO-UNDERLINE WIDTH 132 FRAME gt.
        ASSIGN 
            G1     = 0
            G2     = 0
            NET-CR = 0
            DSC    = 0
            .
    END.

    HIDE FRAME f-top.

    ASSIGN
        str-tit3 = "Period " + string(tran-period,"99") + " " +
                string(tran-date) + " - " + "Summary by Account"
        x        = (132 - length(str-tit3)) / 2
        str-tit3 = FILL(" ",x) + str-tit3.

    PAGE.

    FORM HEADER
        "ACCOUNT                                   DATE   CUST #   NAME"
        "                           CHECK#     INV#           AMOUNT" SKIP
        FILL("_",132) FORMAT "x(130)"

        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top2 PAGE-TOP WIDTH 132 STREAM-IO.

    DISPLAY "" WITH FRAME f-top2.

    FOR EACH ar-cashl NO-LOCK
        WHERE ar-cashl.company    EQ cocode
        AND ar-cashl.cust-no    GE fcust
        AND ar-cashl.cust-no    LE tcust
        AND ar-cashl.on-account EQ YES
        AND ar-cashl.inv-no     NE 0,

        FIRST ar-cash
        WHERE ar-cash.c-no EQ ar-cashl.c-no
        NO-LOCK,

        FIRST cust
        WHERE cust.company EQ ar-cash.company
        AND cust.cust-no EQ ar-cash.cust-no
        NO-LOCK

        BREAK BY ar-cashl.actnum
        BY ar-cashl.cust-no
        BY ar-cashl.check-no
        BY ar-cashl.inv-no

        WITH WIDTH 132 NO-LABELS:

        IF FIRST-OF(ar-cashl.actnum) THEN 
        DO:
            FIND FIRST account
                WHERE account.company EQ cocode
                AND account.actnum  EQ ar-cashl.actnum
                NO-LOCK NO-ERROR.
            IF AVAILABLE account THEN
                PUT ar-cashl.actnum + " - " + account.dscr FORMAT "x(39)" .
            ELSE
                PUT ar-cashl.actnum.
        END.

        IF ar-cashl.dscr = "credit" THEN
            tmp-amt-paid = (- ar-cashl.amt-paid).
        ELSE
            tmp-amt-paid = ar-cashl.amt-paid.
        
        alf-check = STRING(ar-cash.check-no,"999999999999").
      
        RUN alf-check.
               
        PUT  ar-cash.check-date AT 41 SPACE(1)
            ar-cash.cust-no          SPACE(1)
            cust.name                SPACE(1)
            alf-check SPACE(1)
            ar-cashl.inv-no FORMAT ">>>>>>>9"  SPACE(1)
            tmp-amt-paid      FORMAT "->>,>>>,>>9.99"
            SKIP.

        ACCUMULATE ar-cashl.amt-paid (TOTAL BY ar-cashl.actnum).
        ACCUMULATE ar-cashl.amt-paid (TOTAL).
        ACCUMULATE ar-cashl.amt-disc (TOTAL).

        IF LAST-OF(ar-cashl.actnum) THEN
            PUT SKIP
                ACCUM TOTAL BY ar-cashl.actnum ar-cashl.amt-paid
                FORMAT "->>,>>>,>>9.99" TO 117 " *" SKIP.

        IF LAST(ar-cashl.actnum) THEN
            PUT SKIP(1)
                "***** TOTAL FOR ALL ACCOUNTS " TO 95
                ACCUM TOTAL ar-cashl.amt-paid TO 117 " **" SKIP
                "***** TOTAL DISCOUNTS " TO 95
                ACCUM TOTAL ar-cashl.amt-disc TO 117.
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
                IF xtrnum EQ gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
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

