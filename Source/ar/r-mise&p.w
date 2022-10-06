&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ar\r-mise&p.w

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

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
IF AVAILABLE company THEN lv-comp-curr = company.curr-code.

DEFINE VARIABLE v-invalid  AS LOG       NO-UNDO.
DEFINE VARIABLE v-postable AS LOG       NO-UNDO.

DEFINE VARIABLE time_stamp AS ch        NO-UNDO.
DEFINE VARIABLE qfirst     AS l         NO-UNDO.
DEFINE VARIABLE post       AS LOGICAL   FORMAT "Yes/No"
    LABEL "   Post to G/L files?   " INITIAL NO NO-UNDO.
DEFINE VARIABLE xtrnum     AS INTEGER   NO-UNDO.
DEFINE VARIABLE xcs-acct   AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmp-dir    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName  AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE w-bank NO-UNDO
    FIELD bank   LIKE bank.bank-code  
    FIELD actnum LIKE account.actnum  
    FIELD bal    LIKE bank.bal  .

DEFINE TEMP-TABLE tt-post NO-UNDO 
    FIELD row-id   AS ROWID
    FIELD ex-rate  LIKE currency.ex-rate INIT 1
    FIELD curr-amt LIKE ar-cash.check-amt
    FIELD actnum   LIKE account.actnum.

DO TRANSACTION:
    {sys/inc/postdate.i}
END.

DEF STREAM excel.
DEFINE BUFFER bf-period FOR period.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_date end_date ~
rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_date end_date ~
rd-dest fi_file tb_OpenCSV tbAutoClose 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\MiscCashReceipts.csv" 
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

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Post Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

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
     SIZE 17.2 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 4.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 5.81.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV AS LOGICAL INITIAL no 
     LABEL "Open CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.6 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.43 COL 40 COLON-ALIGNED
     tran-period AT ROW 3.71 COL 40 COLON-ALIGNED
     begin_date AT ROW 5.38 COL 28.4 COLON-ALIGNED HELP
          "Enter Beginning Invoice Date"
     end_date AT ROW 5.38 COL 70 COLON-ALIGNED HELP
          "Enter Ending Invoice Date"
     lv-font-no AT ROW 8.1 COL 33 COLON-ALIGNED
     rd-dest AT ROW 8.14 COL 4.8 NO-LABEL
     lines-per-page AT ROW 8.14 COL 87.6 COLON-ALIGNED
     lv-ornt AT ROW 8.19 COL 43.6 NO-LABEL
     lv-font-name AT ROW 9.29 COL 29.4 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 9.71 COL 29.2
     tb_excel AT ROW 9.81 COL 92.2 RIGHT-ALIGNED WIDGET-ID 2
     fi_file AT ROW 10.76 COL 27.2 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 6
     tb_OpenCSV AT ROW 10.76 COL 93.4 RIGHT-ALIGNED WIDGET-ID 4
     tbAutoClose AT ROW 12.29 COL 29.2 WIDGET-ID 64
     btn-ok AT ROW 13.24 COL 29
     btn-cancel AT ROW 13.24 COL 54
     " Output Destination" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 7.43 COL 4
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.1 COL 4
     RECT-6 AT ROW 7.86 COL 3
     RECT-7 AT ROW 1.62 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.8 BY 15
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
         TITLE              = "Miscellaneous Cash Receipts Register"
         HEIGHT             = 13.71
         WIDTH              = 96
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
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
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

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
       tb_excel:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       td-show-parm:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
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
ON END-ERROR OF C-Win /* Miscellaneous Cash Receipts Register */
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
ON WINDOW-CLOSE OF C-Win /* Miscellaneous Cash Receipts Register */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Receipt Date */
DO:
  assign {&self-name}.
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
  DEF VAR lv-post AS LOG NO-UNDO.
  DEF VAR op-error AS LOG NO-UNDO.

  run check-date.
  if v-invalid then
     return no-apply.
  
  DO WITH FRAME {&FRAME-NAME}:
     {ar/checkPeriod.i begin_date:SCREEN-VALUE tran-date:SCREEN-VALUE 2}
  
     {ar/checkPeriod.i end_date:SCREEN-VALUE tran-date:SCREEN-VALUE 2}
     ASSIGN {&DISPLAYED-OBJECTS}.
  END.     

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

        DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
            /* gdm - 11050906 */
            loop:
            REPEAT:
                FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                    WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.

                IF AVAILABLE gl-ctrl THEN 
                DO:
                    ASSIGN 
                        xtrnum        = gl-ctrl.trnum + 1
                        gl-ctrl.trnum = xtrnum.
                    FIND CURRENT gl-ctrl NO-LOCK.
                    RELEASE gl-ctrl.
                    LEAVE loop.
                END. /* IF AVAIL gl-ctrl */
            END. /* REPEAT */
        /* gdm - 11050906 */
        END.

        RUN run-report(OUTPUT op-error).

        IF op-error = NO THEN
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

            MESSAGE "Post Miscellaneous Cash Receipts?"
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
            MESSAGE "No Miscellaneous Cash Receipts available for posting..."
                VIEW-AS ALERT-BOX ERROR.
            RUN undo-trnum.
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
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


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
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
    {sys/inc/reportsConfigNK1.i "AC4" }
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
        targetfile = lv-audit-dir + "\AR\AC4\Run#"
                    + STRING(xtrnum) + ".txt"
        dirname1   = lv-audit-dir
        dirname2   = lv-audit-dir + "\AR"
        dirname3   = lv-audit-dir + "\AR\AC4".

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
  DISPLAY tran-date tran-period begin_date end_date rd-dest fi_file tb_OpenCSV 
          tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date begin_date end_date rd-dest fi_file tb_OpenCSV 
         tbAutoClose btn-ok btn-cancel 
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
        FIND FIRST ar-ctrl WHERE ar-ctrl.company = cocode NO-LOCK.
        IF NOT AVAILABLE ar-ctrl THEN RETURN.
        xcs-acct = ar-ctrl.cash-act.
        RELEASE ar-ctrl.
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
            fi_file:SCREEN-VALUE = "c:\tmp\MiscCashReceipts.csv".   
    END.

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

    DEFINE VARIABLE li-iter AS INTEGER NO-UNDO.


    FOR EACH w-bank:
        DELETE w-bank.
    END.

    DO TRANSACTION:
        DO WHILE CAN-FIND(FIRST tt-post) AND li-iter LE 100000:
            li-iter = li-iter + 1.

            RELEASE tt-post.

            FOR EACH tt-post,
                FIRST ar-mcash WHERE ROWID(ar-mcash) EQ tt-post.row-id
                BREAK BY tt-post.actnum:

                FIND FIRST bank
                    WHERE bank.company   EQ cocode
                    AND bank.bank-code EQ ar-mcash.bank-code
                EXCLUSIVE NO-WAIT NO-ERROR.
                IF NOT AVAILABLE bank THEN NEXT.

                bank.bal = bank.bal + ar-mcash.check-amt.

                FIND FIRST w-bank WHERE w-bank.bank EQ ar-mcash.bank-code NO-ERROR.
                IF NOT AVAILABLE w-bank THEN 
                DO:
                    CREATE w-bank.
                    ASSIGN
                        w-bank.bank   = bank.bank-code
                        w-bank.actnum = bank.actnum.
                END.
                w-bank.bal = w-bank.bal + ar-mcash.check-amt.

                RUN GL_SpCreateGLHist(cocode,
                    ar-mcash.actnum,
                    "MCSHREC",
                    STRING(ar-mcash.m-no),
                    tran-date,
                    - ar-mcash.check-amt,
                    xtrnum,
                    tran-period,
                    "A",
                    tran-date,
                    "",
                    "AR").

                CREATE ar-ledger.
                ASSIGN
                    ar-ledger.company  = cocode
                    ar-ledger.amt      = ar-mcash.check-amt
                    ar-ledger.ref-num  = STRING(ar-mcash.m-no) + " " + ar-mcash.payer
                    ar-ledger.ref-date = ar-mcash.check-date
                    ar-ledger.tr-date  = tran-date
                    ar-ledger.tr-num   = xtrnum
                    ar-mcash.posted    = YES.
                RELEASE ar-ledger.

                ACCUM tt-post.curr-amt - ar-mcash.check-amt (TOTAL BY tt-post.actnum).

                IF LAST-OF(tt-post.actnum) AND tt-post.actnum NE "" THEN 
                DO:
                    RUN GL_SpCreateGLHist(cocode,
                        tt-post.actnum,
                        "MCSHREC",
                        "MISC CASH RECEIPTS CURRENCY GAIN/LOSS " +
                        STRING(ar-mcash.m-no),
                        tran-date,
                        (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-mcash.check-amt),
                        xtrnum,
                        tran-period,
                        "A",
                        tran-date,
                        "",
                        "AR").
       
                    RUN GL_SpCreateGLHist(cocode,
                        tt-post.actnum,
                        "MCSHREC",
                        "MISC CASH RECEIPTS CURRENCY GAIN/LOSS " +
                        STRING(ar-mcash.m-no),
                        tran-date,
                        - (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-mcash.check-amt),
                        xtrnum,
                        tran-period,
                        "A",
                        tran-date,
                        "",
                        "AR").

                END.

                DELETE tt-post.
            END.
        END.  /* DO WHILE */

        FOR EACH w-bank:
            RUN GL_SpCreateGLHist(cocode,
                w-bank.actnum,
                "MCSHREC",
                "MISC CASH RECEIPTS",
                tran-date,
                w-bank.bal,
                xtrnum,
                tran-period,
                "A",
                tran-date,
                "",
                "AR").

        END.
    END. /* DO TRANS */

    FIND CURRENT ar-mcash NO-LOCK NO-ERROR.
    FIND CURRENT bank NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- ar/ar-creg.p 10/94 gb */
    /* AR Cash  - Edit Register & Post Transactions                   */
    /* -------------------------------------------------------------------------- */

    DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

    DEFINE VARIABLE g1          AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE g2          AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-first     AS LOG       NO-UNDO.

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        excelHeader = 'Rec#,NAME,DATE,AMOUNT,G/L DISTRIBUTION,'.
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelHeader,',','","') '"' SKIP.
    END. /* if rd-dest = 3 */

    IF td-show-parm THEN RUN show-param.

    v-postable = NO.

    FORM HEADER
        "Rec#      NAME                                  DATE        "
        "AMOUNT                 G/L DISTRIBUTION" SKIP FILL("_",131) FORMAT "x(131)"
        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top PAGE-TOP WIDTH 132 STREAM-IO.

    ASSIGN
        time_stamp = STRING(TIME, "HH:MMam")
        tmpstore   = FILL("_",125).

    {sys/form/r-top3w.f}
    SESSION:SET-WAIT-STATE("general").

    ASSIGN
        str-tit  = coname + " - " + loname
        str-tit2 = "MISC. CASH RECEIPTS  -  EDIT REGISTER " + string(xtrnum)
        str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date)
        x        = (112 - length(str-tit)) / 2
        str-tit  = FILL(" ",x) + str-tit
        x        = (114 - length(str-tit2)) / 2
        str-tit2 = FILL(" ",x) + str-tit2
        x        = (132 - length(str-tit3)) / 2
        str-tit3 = FILL(" ",x) + str-tit3
        z        = 0.

    DISPLAY "" WITH FRAME r-top.
    DISPLAY "" WITH FRAME f-top.

    EMPTY TEMP-TABLE tt-post.

    FOR EACH ar-mcash NO-LOCK
        WHERE ar-mcash.company    EQ cocode
        AND ar-mcash.posted     EQ NO
        AND ar-mcash.check-date GE begin_date
        AND ar-mcash.check-date LE end_date
        BREAK BY ar-mcash.bank-code
        WITH FRAME a1:

        IF FIRST-OF(ar-mcash.bank-code) THEN 
        DO:
            ASSIGN 
                g1 = 0.
            FIND FIRST bank WHERE bank.company = cocode AND
                bank.bank-code = ar-mcash.bank-code
                NO-LOCK NO-ERROR.
            IF AVAILABLE bank THEN
            DO:
                PUT bank.bank-name bank.actnum SKIP.
                IF rd-dest = 3 THEN
                    PUT STREAM excel UNFORMATTED
                        '"' bank.bank-name + " " + bank.actnum '",' SKIP.
            END.

            ELSE 
            DO:
                MESSAGE "No Bank Record Available." VIEW-AS ALERT-BOX ERROR.       
                IF rd-dest = 3 THEN
                    OUTPUT STREAM excel CLOSE.
                op-error = YES.
                RETURN.
            END.

            z = z + 1.
        END.

        CREATE tt-post.
        ASSIGN
            tt-post.row-id   = ROWID(ar-mcash)
            tt-post.curr-amt = ar-mcash.check-amt.

        RELEASE currency.
        IF lv-comp-curr NE "" AND lv-comp-curr NE ar-mcash.curr-code[1] THEN
            FIND FIRST currency NO-LOCK
                WHERE currency.company     EQ ar-mcash.company
                AND currency.c-code      EQ ar-mcash.curr-code[1]
                AND currency.ar-ast-acct NE ""
                AND currency.ex-rate     GT 0
                NO-ERROR.

        IF AVAILABLE currency THEN
            ASSIGN
                tt-post.actnum   = currency.ar-ast-acct
                tt-post.ex-rate  = currency.ex-rate
                tt-post.curr-amt = tt-post.curr-amt * tt-post.ex-rate.

        ASSIGN 
            g1 = g1 + tt-post.curr-amt.

        PUT ar-mcash.m-no
            ar-mcash.payer
            ar-mcash.check-date AT 50
            tt-post.curr-amt    AT 62
            ar-mcash.actnum     AT 85  SPACE(1)
            tt-post.curr-amt    TO 125 SKIP.

        IF rd-dest = 3 THEN
            PUT STREAM excel UNFORMATTED
                '"' STRING(ar-mcash.m-no) + " " + ar-mcash.payer '",'
                '"' "" '",'
                '"' STRING(ar-mcash.check-date,"99/99/99") '",'
                '"' STRING(tt-post.curr-amt,"->>,>>>,>>9.99") '",'
                '"' ar-mcash.actnum '",'
                '"' STRING(tt-post.curr-amt,"->>,>>>,>>9.99") '",'
                SKIP.

        IF LAST-OF(ar-mcash.bank-code) THEN 
        DO:
            PUT "**  TOTAL  "  AT 85  g1 TO 125 SKIP.

            IF rd-dest = 3 THEN
                PUT STREAM excel UNFORMATTED
                    '"' "" '",'
                    '"' "" '",'
                    '"' "" '",'
                    '"' "" '",'
                    '"' "TOTAL" '",'
                    '"' STRING(g1,"->>>,>>>,>>9.99") '",'
                    SKIP(2).

            ASSIGN
                g2 = g2 + g1
                g1 = 0.
        END.

        v-postable = YES.
    END. /* each invoice */

    IF z > 1 THEN
    DO:
        DISPLAY  "** GRAND TOTAL  "  AT 85  g2 TO 125
            WITH NO-LABELS NO-UNDERLINE WIDTH 132 FRAME gt.

        IF rd-dest = 3 THEN
            PUT STREAM excel UNFORMATTED
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "GRAND TOTAL" '",'
                '"' STRING(g2,"->>>,>>>,>>9.99") '",'
                SKIP(2).
    END.

    HIDE FRAME f-top.

    ASSIGN
        str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date) + " - " +
                 "Summary by Account"
        x        = (132 - length(str-tit3)) / 2
        str-tit3 = FILL(" ",x) + str-tit3 .
    PAGE.
    FORM HEADER
        "ACCCOUNT                                  DATE     Rec.# PAID BY"
        "                               AMOUNT"
        SKIP
        FILL("_",132) FORMAT "x(130)"
        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top2 PAGE-TOP WIDTH 132.

    DISPLAY "" WITH FRAME f-top2.

    IF rd-dest = 3 THEN 
    DO:
        excelHeader = 'ACCOUNT,DATE,Rec.#,PAID BY,AMOUNT,'.
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelHeader,',','","') '"' SKIP.
    END. /* if rd-dest = 3 */

    FOR EACH tt-post,
        FIRST ar-mcash NO-LOCK WHERE ROWID(ar-mcash) EQ tt-post.row-id
        BREAK BY ar-mcash.actnum
        BY ar-mcash.m-no
        WITH WIDTH 132 NO-LABELS:

        IF FIRST-OF(ar-mcash.actnum) THEN 
        DO:
            FIND FIRST account WHERE account.company = cocode AND
                account.actnum  = ar-mcash.actnum
                NO-LOCK NO-ERROR.

            IF AVAILABLE account THEN
            DO:
                PUT SKIP ar-mcash.actnum + " - " + account.dscr FORMAT "x(39)".
                IF rd-dest = 3 THEN
                DO:
                    v-first = YES.
                    PUT STREAM excel UNFORMATTED
                        '"' ar-mcash.actnum + " - " + account.dscr '",'.
                END.
            END.
            ELSE
            DO:
                MESSAGE "No Account Record Available for Account #: " +
                    ar-mcash.actnum + " for Receipt #: " + STRING(ar-mcash.m-no)
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                IF rd-dest = 3 THEN
                    OUTPUT STREAM excel CLOSE.

                op-error = YES.
                RETURN.
            END.
        END.

        PUT ar-mcash.check-date AT 41     SPACE(1)
            ar-mcash.m-no                 SPACE(1)
            ar-mcash.payer                SPACE(1)
            ar-mcash.check-amt           .

        IF rd-dest = 3 THEN
        DO:
            IF v-first = NO THEN
                PUT STREAM excel UNFORMATTED
                    '"' "" '",'
                    '"' ar-mcash.check-date '",'
                    '"' ar-mcash.m-no '",'
                    '"' ar-mcash.payer '",'
                    '"' STRING(ar-mcash.check-amt,"->>,>>>,>>9.99") '",'
                    SKIP.
            ELSE
            DO:
                PUT STREAM excel UNFORMATTED
                    '"' ar-mcash.check-date '",'
                    '"' ar-mcash.m-no '",'
                    '"' ar-mcash.payer '",'
                    '"' STRING(ar-mcash.check-amt,"->>,>>>,>>9.99") '",'
                    SKIP.
                v-first = NO.
            END.
        END.

        ACCUMULATE ar-mcash.check-amt (TOTAL BY ar-mcash.actnum).
        ACCUMULATE ar-mcash.check-amt (TOTAL).

        IF LAST-OF(ar-mcash.actnum) THEN
        DO:
            PUT SKIP 
                "** TOTAL "  TO 100
                (ACCUM TOTAL BY ar-mcash.actnum ar-mcash.check-amt)
                FORMAT "->>>,>>>,>>9.99" TO 125 SKIP(1).

            IF rd-dest = 3 THEN
                PUT STREAM excel UNFORMATTED
                    '"' "" '",'
                    '"' "" '",'
                    '"' "" '",'
                    '"' "" '",'
                    '"' "** TOTAL" '",'
                    '"' STRING((ACCUM TOTAL BY ar-mcash.actnum ar-mcash.check-amt),"->>>,>>>,>>9.99") '",'
                    SKIP(2).
        END.
    END.

    FOR EACH tt-post WHERE tt-post.actnum NE "",
        FIRST ar-mcash NO-LOCK WHERE ROWID(ar-mcash) EQ tt-post.row-id
        BREAK BY tt-post.actnum
        BY ar-mcash.m-no
        WITH WIDTH 132 NO-LABELS:

        IF FIRST-OF(tt-post.actnum) THEN 
        DO:
            FIND FIRST account WHERE account.company = cocode AND
                account.actnum  = tt-post.actnum
                NO-LOCK NO-ERROR.
            IF AVAILABLE account THEN
            DO:
                PUT SKIP tt-post.actnum + " - " + account.dscr FORMAT "x(39)".
                IF rd-dest = 3 THEN
                DO:
                    v-first = YES.
                    PUT STREAM excel UNFORMATTED
                        '"' tt-post.actnum + " - " + account.dscr '",'.
                END.
            END.
            ELSE
            DO:
                MESSAGE "No Account Record Available for Account #: " +
                    ar-mcash.actnum + " for Receipt #: " + STRING(ar-mcash.m-no)
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                IF rd-dest = 3 THEN
                    OUTPUT STREAM excel CLOSE.
                op-error = YES.
                RETURN.
            END.
        END.

        PUT  ar-mcash.check-date AT 41     SPACE(1)
            ar-mcash.m-no                 SPACE(1)
            ar-mcash.payer                SPACE(1)
            tt-post.curr-amt - ar-mcash.check-amt
            FORMAT "->>,>>>,>>9.99".

        IF rd-dest = 3 THEN
        DO:
            IF v-first = NO THEN
                PUT STREAM excel UNFORMATTED
                    '"' "" '",'
                    '"' ar-mcash.check-date '",'
                    '"' ar-mcash.m-no '",'
                    '"' ar-mcash.payer '",'
                    '"' STRING(tt-post.curr-amt - ar-mcash.check-amt,"->>,>>>,>>9.99") '",'
                    SKIP.
            ELSE
            DO:
                PUT STREAM excel UNFORMATTED
                    '"' ar-mcash.check-date '",'
                    '"' ar-mcash.m-no '",'
                    '"' ar-mcash.payer '",'
                    '"' STRING(tt-post.curr-amt - ar-mcash.check-amt,"->>,>>>,>>9.99") '",'
                    SKIP.
                v-first = NO.
            END.
        END.

        ACCUMULATE tt-post.curr-amt - ar-mcash.check-amt (TOTAL BY tt-post.actnum).
        ACCUMULATE tt-post.curr-amt - ar-mcash.check-amt (TOTAL).

        IF LAST-OF(tt-post.actnum) THEN
        DO:
            PUT SKIP 
                "** TOTAL "  TO 100
                (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-mcash.check-amt)
                FORMAT "->>>,>>>,>>9.99" TO 125 SKIP(1).

            IF rd-dest = 3 THEN
                PUT STREAM excel UNFORMATTED
                    '"' "" '",'
                    '"' "" '",'
                    '"' "" '",'
                    '"' "" '",'
                    '"' "** TOTAL" '",'
                    '"' STRING((ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-mcash.check-amt),"->>>,>>>,>>9.99") '",'
                    SKIP(2).
        END.
    END.

    PUT "***** TOTAL FOR ALL ACCOUNTS " TO 100
        (ACCUM TOTAL ar-mcash.check-amt) +
        (ACCUM TOTAL tt-post.curr-amt - ar-mcash.check-amt)
        FORMAT "->>>,>>>,>>9.99" TO 125 SKIP(1).

    IF rd-dest = 3 THEN
    DO:
        PUT STREAM excel UNFORMATTED
            '"' "" '",'
            '"' "" '",' 
            '"' "" '",'
            '"' "" '",'
            '"' "***** TOTAL FOR ALL ACCOUNTS" '",'
            '"' STRING((ACCUM TOTAL ar-mcash.check-amt) +
            (ACCUM TOTAL tt-post.curr-amt - ar-mcash.check-amt),"->>>,>>>,>>9.99") '",'
            SKIP.

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
                IF xtrnum = gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
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

