&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap\r-recreg.w

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

{sys/inc/VAR.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE NEW SHARED VARIABLE v-trnum           AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-unline          AS CHARACTER FORMAT "x(80)" INIT
    "--------------- ------------------------- ------- ----------- ---" NO-UNDO.
DEFINE            VARIABLE time_stamp        AS ch        NO-UNDO.
DEFINE            VARIABLE v-postable        AS LOG       INIT NO NO-UNDO.

DEFINE            VARIABLE v-invalid         AS LOG       NO-UNDO.


DEFINE            VARIABLE save_id           AS RECID     NO-UNDO.
DEFINE            VARIABLE v-post            AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-matching-record AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-vend-name       LIKE vend.name NO-UNDO.
DEFINE            VARIABLE tb_excel          AS LOGICAL   INITIAL YES NO-UNDO.
DEFINE            VARIABLE cFileName         AS CHARACTER NO-UNDO.
{ap/reconcil.i NEW}

DEFINE STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 end_date bank_code rd-dest ~
fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS end_date bank_code rd-dest fi_file ~
tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 16 BY 1.29.

DEFINE VARIABLE bank_code AS CHARACTER FORMAT "X(8)" 
     LABEL "Enter Bank Code or Leave Blank For All" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\BankReconciliationReg.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 49 BY 1.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To CSV", 3
     SIZE 15 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 4.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 6.19.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV AS LOGICAL INITIAL no 
     LABEL "Open CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81
     BGCOLOR 15 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     end_date AT ROW 3.19 COL 41.8 COLON-ALIGNED
     bank_code AT ROW 4.52 COL 41.8 COLON-ALIGNED HELP
          "Enter Bank code or Leave Blank For all"
     rd-dest AT ROW 8.33 COL 4.8 NO-LABEL
     lv-font-no AT ROW 8.33 COL 32.2 COLON-ALIGNED
     lv-ornt AT ROW 8.38 COL 42 NO-LABEL
     lines-per-page AT ROW 8.38 COL 86.4 COLON-ALIGNED
     lv-font-name AT ROW 9.52 COL 28.2 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 10.81 COL 31
     fi_file AT ROW 11.1 COL 24.2 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 66
     tb_OpenCSV AT ROW 11.29 COL 92.2 RIGHT-ALIGNED WIDGET-ID 68
     tbAutoClose AT ROW 12.57 COL 31.4 WIDGET-ID 64
     btn-ok AT ROW 13.52 COL 31.2
     btn-cancel AT ROW 13.52 COL 52.2
     " Output Destination" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 7.67 COL 4
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.05 COL 4
     RECT-6 AT ROW 8.1 COL 3
     RECT-7 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.8 BY 15.71
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
         TITLE              = "A/P Bank Reconciliation Register"
         HEIGHT             = 14
         WIDTH              = 95
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
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
       bank_code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lines-per-page:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-name:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-no:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-ornt:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       td-show-parm:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

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
ON END-ERROR OF C-Win /* A/P Bank Reconciliation Register */
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
ON WINDOW-CLOSE OF C-Win /* A/P Bank Reconciliation Register */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bank_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bank_code C-Win
ON LEAVE OF bank_code IN FRAME FRAME-A /* Enter Bank Code or Leave Blank For All */
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
                    LEAVE.
                END. /* IF AVAIL gl-ctrl */
            END. /* REPEAT */
        /* gdm - 11050906 */
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
        END CASE.

        IF v-postable THEN 
        DO:    
            lv-post = NO.

            MESSAGE "Do You Want To Purge Reconciled Checks ?"

                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE lv-post.

            IF lv-post THEN 
            DO:  
                RUN post-gl.
                MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
            END.  
        END.

        ELSE 
        DO:
            MESSAGE "No A/P Check Reconciliation available for posting..." VIEW-AS ALERT-BOX ERROR.      
        END.

        IF NOT v-postable OR NOT lv-post THEN 
        DO TRANSACTION:
            /* gdm - 11050906 */
            REPEAT:
                FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                    WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
                IF AVAILABLE gl-ctrl THEN 
                DO:
                    IF gl-ctrl.trnum EQ v-trnum THEN gl-ctrl.trnum = v-trnum - 1.
                    FIND CURRENT gl-ctrl NO-LOCK.
                    LEAVE.
                END. /* IF AVAIL gl-ctrl */
            END. /* REPEAT */
        /* gdm - 11050906 */
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
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

&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
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

    FIND FIRST ap-ctrl WHERE ap-ctrl.company = cocode NO-LOCK.
    FIND FIRST bank WHERE bank.company = cocode
        AND bank.actnum = ap-ctrl.cash-act NO-LOCK NO-ERROR.
    IF AVAILABLE bank THEN
        ASSIGN
            bank_code = bank.bank-code .

    end_date = TODAY.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").   
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "VT2" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    RUN pChangeDest.
    {methods/nowait.i}
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
  DISPLAY end_date bank_code rd-dest fi_file tb_OpenCSV tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 end_date bank_code rd-dest fi_file tb_OpenCSV 
         tbAutoClose btn-ok btn-cancel 
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
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
    postit:
    DO TRANSACTION:
        FOR EACH reconcile WHERE tt-cleared AND tt-date LE end_date
            ON ERROR UNDO postit, LEAVE postit:

            IF tt-type EQ 1 THEN 
            DO:
                FIND ap-pay WHERE ROWID(ap-pay) EQ tt-rowid EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE ap-pay THEN ap-pay.reconciled = YES.
            END.

            ELSE
                IF tt-type EQ 2 THEN
                    FOR EACH tt-cash WHERE tt-trnum EQ INT(SUBSTR(tt-number,4,10)),
                        FIRST ar-cash
                        WHERE ROWID(ar-cash)     EQ tt-cash.row-id
                        AND ar-cash.reconciled EQ NO
                        AND ar-cash.posted     EQ YES
                        AND ar-cash.memo       EQ NO
                        AND ar-cash.bank-code  EQ tt-bank
                        EXCLUSIVE-LOCK:
                        ar-cash.reconciled = YES.
                    END.

                ELSE
                    IF tt-type EQ 3 THEN 
                    DO:
                        FIND gl-jrn WHERE ROWID(gl-jrn) EQ tt-rowid EXCLUSIVE-LOCK NO-ERROR.
                        IF AVAILABLE gl-jrn THEN gl-jrn.reconciled = YES.
                    END.

                    ELSE
                        IF tt-type EQ 4 THEN
                            FOR EACH ar-mcash NO-LOCK WHERE ROWID(ar-mcash) EQ tt-rowid,
                                FIRST ar-mcash-ref
                                WHERE ar-mcash-ref.rec_key  EQ ar-mcash.rec_key
                                AND ar-mcash-ref.reftable EQ "ar-mcash-ref"
                                AND ar-mcash-ref.company  EQ "ar-mcash"
                                EXCLUSIVE-LOCK
                                USE-INDEX rec_key:
                                ar-mcash-ref.val[1] = INT(YES).
                            END.
        END.
    END. /* do postit */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- oe/invpost.p 10/94 gb */
    /* Invoicing  - Edit Register & Post Invoicing Transactions                   */
    /* -------------------------------------------------------------------------- */

    DEFINE VARIABLE v-chk-tot   AS DECIMAL   FORMAT "->,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-jrn-tot   AS DECIMAL   FORMAT "->,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-dep-tot   AS DECIMAL   FORMAT "->,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-unc-tot   AS DECIMAL   FORMAT "->,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-bnk-tot   AS DECIMAL   FORMAT "->,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-bank-code AS CHARACTER NO-UNDO.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
    
    ASSIGN
        time_stamp = STRING(TIME, "hh:mmam")
        tmpstore   = FILL("_",125) .
     
     v-postable = NO.

    {sys/form/r-top3w.f}

    FORM HEADER SKIP
        "Check/Journal#"
        "Date" AT 17
        "        Amount" AT 33
        "Vendor" AT 59 SKIP
        FILL("-",130) FORMAT "x(130)"
        WITH FRAME r-top.

    FORM tt-number FORMAT "x(13)"
        tt-date AT 17 FORMAT "99/99/99"
        tt-amt AT 33 FORMAT "->>,>>>,>>9.99"
        tt-vend AT 59 SPACE(1)
        tt-name
        WITH FRAME f1 DOWN WIDTH 132 NO-BOX NO-LABELS NO-ATTR-SPACE.

    FORM SKIP(2) SPACE(52) "(There are no reconciled checks/journals/deposits)"
        WITH FRAME no-matching-record WIDTH 132 NO-BOX NO-LABELS.

    FORM SKIP(2) SPACE(58) "(End of the report)"
        WITH FRAME end-of-report WIDTH 132 NO-BOX NO-LABELS.


    SESSION:SET-WAIT-STATE("general").

    ASSIGN
        str-tit     = coname + " - " + loname
        str-tit2    = "A/P RECONCILED CHECK/JOURNAL/DEPOSIT REGISTER    " 
        str-tit3    = ""
        x           = (112 - length(str-tit)) / 2
        str-tit     = FILL(" ",x) + str-tit
        x           = (114 - length(str-tit2)) / 2
        str-tit2    = FILL(" ",x) + str-tit2
        x           = (132 - length(str-tit3)) / 2
        str-tit3    = FILL(" ",x) + str-tit3
        v-chk-tot   = 0 
        v-bank-code = bank_code .      

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}
    
    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        excelheader = "Check/Journal#,Date,Amount,Vendor,Name".
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    FIND LAST period NO-LOCK
        WHERE period.company EQ cocode
        AND period.pst     LE end_date
        AND period.pend    GE end_date
        NO-ERROR.

    IF AVAILABLE period THEN 
    DO:
        RUN ap/reconcilrpt.p(INPUT v-bank-code ).

        VIEW FRAME r-top.

        FOR EACH reconcile WHERE tt-date LE end_date
            BREAK BY tt-bank BY tt-date BY tt-type BY tt-number:

            IF FIRST-OF(tt-bank) THEN PAGE.

            IF tt-type EQ 1 THEN tt-amt = tt-amt * -1.

            IF tt-cleared THEN 
            DO: 
                IF tt-type EQ 1 THEN
                    v-chk-tot = v-chk-tot + tt-amt.
                ELSE
                    IF tt-type EQ 2 OR tt-type EQ 4 THEN
                        v-dep-tot = v-dep-tot + tt-amt.
                    ELSE
                        v-jrn-tot = v-jrn-tot + tt-amt.

                DISPLAY tt-number
                    tt-date
                    tt-amt
                    tt-vend
                    tt-name
                    WITH FRAME f1.
                DOWN WITH FRAME f1.
                
                IF tb_excel THEN 
                do:
                 PUT STREAM excel UNFORMATTED
                     '"' tt-number                         '",'
                     '"' tt-date                           '",'
                     '"' tt-amt                            '",'
                     '"' tt-vend                           '",'
                     '"' tt-name                           '",'                     
                     SKIP.
                END.
                v-postable = YES.
            END.

            ELSE v-unc-tot = v-unc-tot + tt-amt.

            IF tt-type EQ 1 THEN tt-amt = tt-amt * -1.

            IF LAST-OF(tt-bank) THEN 
            DO:
                RELEASE account.
                FIND FIRST bank NO-LOCK
                    WHERE bank.company   EQ cocode
                    AND bank.bank-code EQ tt-bank
                    NO-ERROR.
                IF AVAILABLE bank THEN
                    FIND FIRST account NO-LOCK
                        WHERE account.company EQ bank.company
                        AND account.actnum  EQ bank.actnum
                        NO-ERROR.

                IF AVAILABLE account THEN
                    RUN GL_GetAccountOpenBal(ROWID(account), end_date + 1, OUTPUT v-bnk-tot).
        //RUN gl/gl-open2.p (ROWID(account), period.pst, end_date, OUTPUT v-bnk-tot).

                ELSE v-bnk-tot = 0.

                PUT SKIP(1)
                    "Bank:" tt-bank
                    "Total Checks:"           TO 31
                    v-chk-tot                 AT 33 FORMAT "->>,>>>,>>9.99" SKIP
                    "Total Deposits:"         TO 31
                    v-dep-tot                 AT 33 FORMAT "->>,>>>,>>9.99" SKIP
                    "Total GL Entries:"       TO 31
                    v-jrn-tot                 AT 33 FORMAT "->>,>>>,>>9.99" SKIP(1)
                    "Beginning Book Balance:" TO 31
                    v-bnk-tot                 AT 33 FORMAT "->>,>>>,>>9.99" 
                    "Total in Transit:"       TO 31
                    v-unc-tot                 AT 33 FORMAT "->>,>>>,>>9.99" SKIP
                    "Ending Book Balance:"    TO 31
                    v-bnk-tot - v-unc-tot     AT 33 FORMAT "->>,>>>,>>9.99" SKIP(1).
                    
                IF tb_excel THEN 
                DO:
                    PUT STREAM excel UNFORMATTED
                     '"' "Bank:"  tt-bank                  '",'
                     '"' "Total Checks:"                   '",'
                     '"' v-chk-tot                         '",'
                     SKIP
                     '"' ""                                '",'
                     '"' "Total Deposits:"                 '",'                     
                     '"' v-dep-tot                         '",'
                     SKIP
                     '"' ""                                '",'
                     '"' "Total GL Entries:"               '",'                      
                     '"' v-jrn-tot                         '",'
                     SKIP                         
                     '"' ""                                '",'
                     '"' "Beginning Book Balance:"         '",'                       
                     '"' v-bnk-tot                         '",'
                     SKIP                    
                     '"' ""                                '",'
                     '"' "Total in Transit:"               '",'                     
                     '"' v-unc-tot                         '",'
                     SKIP                     
                     '"' ""                                '",'
                     '"' "Ending Book Balance:"            '",'                      
                     '"' v-bnk-tot - v-unc-tot             '",'
                     SKIP  .
                     
                END.

                ASSIGN
                    v-chk-tot = 0
                    v-dep-tot = 0
                    v-jrn-tot = 0
                    v-unc-tot = 0.
            END.
        END. /* for each reconcile record */
    END.

    ELSE MESSAGE "No Period exists for End Date," +
            "reconcilation cannot be completed..."
            VIEW-AS ALERT-BOX ERROR.

    DISPLAY WITH FRAME end-of-report.

    SESSION:SET-WAIT-STATE("").
    
    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
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
            fi_file:SCREEN-VALUE = "c:\tmp\BankReconciliationReg.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
