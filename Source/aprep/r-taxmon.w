&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aprep\r-taxmon.w

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

DEFINE TEMP-TABLE w-tax NO-UNDO
    FIELD tax-gr LIKE ar-inv.tax-code
    FIELD rec-id AS RECID.

DEFINE TEMP-TABLE tt-stax NO-UNDO LIKE stax.
DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.

DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName      AS CHARACTER NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_year begin_period ~
begin_date end_date rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_year begin_period begin_date ~
end_date rd-dest fi_file tb_OpenCSV tbAutoClose 

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

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/01 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_period   AS INTEGER   FORMAT ">9":U INITIAL 1 
    LABEL "For Period" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_year     AS INTEGER   FORMAT ">>>>":U INITIAL 9999 
    LABEL "For Year" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\MonthlyTax.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

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
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 16 BY 4.71 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.29.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 4.76.

DEFINE VARIABLE tbAutoClose  AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_year AT ROW 2.33 COL 28 COLON-ALIGNED HELP
    "Enter Fiscal Year"
    begin_period AT ROW 3.52 COL 28 COLON-ALIGNED HELP
    "Enter Reporting Period"
    begin_date AT ROW 4.71 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 4.71 COL 63 COLON-ALIGNED HELP
    "Enter Ending Date"
    lv-ornt AT ROW 6.95 COL 45 NO-LABELS
    lv-font-no AT ROW 7 COL 36 COLON-ALIGNED
    lines-per-page AT ROW 7.19 COL 87 COLON-ALIGNED
    rd-dest AT ROW 7.24 COL 5 NO-LABELS
    lv-font-name AT ROW 8.14 COL 29 COLON-ALIGNED NO-LABELS
    tb_excel AT ROW 9.33 COL 91 RIGHT-ALIGNED
    td-show-parm AT ROW 10.05 COL 28
    fi_file AT ROW 10.91 COL 26 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 10.95 COL 92.4 RIGHT-ALIGNED
    tbAutoClose AT ROW 12.19 COL 28 WIDGET-ID 78
    btn-ok AT ROW 13.14 COL 28
    btn-cancel AT ROW 13.14 COL 52
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 6.48 COL 4
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 4
    RECT-6 AT ROW 6.91 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 21.71
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
        TITLE              = "Monthly Tax Report"
        HEIGHT             = 13.71
        WIDTH              = 94.6
        MAX-HEIGHT         = 53.57
        MAX-WIDTH          = 384
        VIRTUAL-HEIGHT     = 53.57
        VIRTUAL-WIDTH      = 384
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
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_period:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_year:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Monthly Tax Report */
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
ON WINDOW-CLOSE OF C-Win /* Monthly Tax Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_period C-Win
ON VALUE-CHANGED OF begin_period IN FRAME FRAME-A /* For Period */
    DO:
        RUN show-period-dates.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_year C-Win
ON VALUE-CHANGED OF begin_year IN FRAME FRAME-A /* For Year */
    DO:
        RUN show-period-dates.
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
        {&WINDOW-NAME}:WINDOW-STATE = WINDOW-MINIMIZED.    

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

        ASSIGN rd-dest.
        IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "southpak"
            THEN is-xprint-form = YES.     
        ELSE is-xprint-form = NO.

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
                    {custom/asifax.i &type= 'begin_cust=begin_year'
                            &begin_cust= "begin_year"
                            &END_cust= "begin_year" 
                            &fax-subject="Monthly Tax Report"
                            &fax-body="Monthly Tax Report"
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = ''
                             &begin_cust=''
                             &END_cust='' 
                             &mail-subject="Monthly Tax Report"
                             &mail-body="Monthly Tax Report"
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = ''
                                  &begin_cust=''
                                  &END_cust='' 
                                  &mail-subject="Monthly Tax Report"
                                  &mail-body="Monthly Tax Report"
                                  &mail-file=list-name }

                    END.
                END. 
            WHEN 6 THEN RUN OUTPUT-to-port.
        END CASE.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
        CURRENT-WINDOW:WINDOW-STATE  = WINDOW-NORMAL.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* Name */
    DO:
        DEFINE VARIABLE ls-filename AS cha NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG NO-UNDO.

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
        begin_year   = YEAR(TODAY)
        begin_period = MONTH(TODAY)
        begin_date   = TODAY
        end_date     = TODAY.

    FIND FIRST period
        WHERE period.company EQ cocode
        AND period.yr      EQ begin_year
        AND period.pst     LE TODAY
        AND period.pend    GE TODAY
        AND period.pstat
        NO-LOCK NO-ERROR.

    IF AVAILABLE period THEN
        ASSIGN
            begin_period = period.pnum
            begin_year   = period.yr
            begin_date   = period.pst.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "VR6" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}


    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        APPLY "entry" TO begin_year.
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
    DISPLAY begin_year begin_period begin_date end_date rd-dest fi_file 
        tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_year begin_period begin_date end_date rd-dest 
        fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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
    RUN custom/dprint.w (list-name).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ---------------------------------------------- ap/rep/monthtax.p 10/96 JLF */
    /* Monthly Tax tt-report                                                         */
    /* -------------------------------------------------------------------------- */

    {sys/form/r-topw2.f}

    DEFINE VARIABLE v-period      LIKE uperiod INIT 1.
    DEFINE VARIABLE v-date        AS DATE      EXTENT 2 FORMAT "99/99/9999"
        INIT [01/01/0001, TODAY].                       
    DEFINE VARIABLE v-year        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-tax-gl      AS CHARACTER.
    DEFINE VARIABLE v-tax-gr      LIKE stax.tax-gr EXTENT 10.
    DEFINE VARIABLE v-head1       AS CHARACTER FORMAT "x(30)" EXTENT 5.
    DEFINE VARIABLE v-head2       AS CHARACTER FORMAT "x(30)" EXTENT 5.
    DEFINE VARIABLE v-head3       AS CHARACTER FORMAT "x(30)" EXTENT 5.
    DEFINE VARIABLE v-sal-amt     AS DECIMAL   FORMAT "->>,>>>,>>9.99" EXTENT 2.
    DEFINE VARIABLE v-taxable     AS DECIMAL   FORMAT "->>,>>>,>>9.99" EXTENT 10.
    DEFINE VARIABLE v-tax-amt     AS DECIMAL   FORMAT "->>,>>>,>>9.99" EXTENT 10.
    DEFINE VARIABLE v-rate        LIKE stax.tax-rate1.
    DEFINE VARIABLE v-frtr        LIKE stax.tax-rate1.
    DEFINE VARIABLE v-frtr-s      AS DECIMAL.
    DEFINE VARIABLE v-rate-t      AS DECIMAL.
    DEFINE VARIABLE v-rate-tt     AS DECIMAL   EXTENT 5.
    DEFINE VARIABLE v-frtr-tt     AS DECIMAL   EXTENT 5.
    DEFINE VARIABLE v-frtr-t      AS DECIMAL.
    DEFINE VARIABLE v-inv-tax     AS DECIMAL.
    DEFINE VARIABLE v-frt-tax     AS DECIMAL.
    DEFINE VARIABLE v-actnum      LIKE ar-cashl.actnum.
    DEFINE VARIABLE ld            AS DECIMAL.
    DEFINE VARIABLE excelheader1  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE excelheader2  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-tot-tax     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-tot-taxable AS DECIMAL   NO-UNDO.
   


    IF tb_excel THEN
        OUTPUT STREAM excel TO VALUE(cFileName).

    FORMAT HEADER
        SKIP(1)
        v-head1[1]        AT 43
        SPACE(0)
        v-head1[2]
        SPACE(0)
        v-head1[3] 
        SPACE(0)
        v-head1[4]
        SPACE(0)
        v-head1[5]      
        SKIP
        "Code Name"
        "Gross Sales $"   AT 30
        SPACE(0)
        v-head2[1]        AT 43
        SPACE(0)
        v-head2[2]
        SPACE(0)
        v-head2[3] 
        SPACE(0)
        v-head2[4] 
        SPACE(0)
        v-head2[5]       
        SKIP
        FILL("-",42)      FORMAT "x(42)"
        SPACE(0)
        v-head3[1]        AT 43
        SPACE(0)
        v-head3[2]
        SPACE(0)
        v-head3[3]               
        SPACE(0)
        v-head3[4]    
        SPACE(0)
        v-head3[5]       
        SKIP
        WITH FRAME r-top.

    {sa/sa-sls01.i}


    ASSIGN
        str-tit2  = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 172}

        v-period  = begin_period
        v-date[1] = begin_date
        v-date[2] = end_date. 

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    FOR EACH stax
        {sys/ref/staxW.i}
            and stax.tax-group  eq stax.tax-code1[1]
            and stax.tax-acc1[1] GT v-tax-gl no-lock
             by stax.tax-acc1[1]:
    CREATE tt-stax.
    BUFFER-COPY stax TO tt-stax NO-ERROR.
END.

FOR EACH cust WHERE cust.company EQ cocode NO-LOCK:
    FOR EACH ar-inv WHERE ar-inv.company        EQ cocode
        AND ar-inv.inv-date       GE v-date[1]
        AND ar-inv.inv-date       LE v-date[2]
        AND ar-inv.cust-no        EQ cust.cust-no
        AND ar-inv.tax-code       NE ""
        AND ar-inv.posted         EQ YES
        USE-INDEX inv-date NO-LOCK,
        EACH ar-invl WHERE ar-invl.posted
        AND ar-invl.company EQ ar-inv.company 
        AND ar-invl.inv-no EQ ar-inv.inv-no:

        CREATE tt-report.
        ASSIGN
            tt-report.term-id = v-term
            tt-report.key-01  = IF ar-invl.taxGroup NE "" THEN ar-invl.taxGroup ELSE ar-inv.tax-code
            tt-report.rec-id  = RECID(ar-invl)
            tt-report.key-05  = STRING(ar-invl.inv-no,"99999999")
            tt-report.key-06  = STRING(ar-invl.LINE,"99").
    END. /* for each ar-inv */

    FOR EACH ar-cash WHERE ar-cash.company    EQ cocode
        AND ar-cash.cust-no    EQ cust.cust-no
        AND ar-cash.check-date GE v-date[1]
        AND ar-cash.check-date LE v-date[2]
        AND ar-cash.posted     EQ YES
        USE-INDEX ar-cash NO-LOCK:

        v-actnum = "".

        FOR EACH ar-cashl WHERE ar-cashl.c-no   EQ ar-cash.c-no
            AND ar-cashl.posted EQ YES
            AND ar-cashl.memo   EQ YES
            USE-INDEX c-no NO-LOCK,
            FIRST ar-inv WHERE ar-inv.company  EQ cocode
            AND ar-inv.inv-no   EQ ar-cashl.inv-no
            AND ar-inv.tax-code NE "" NO-LOCK:

            FIND FIRST stax
                {sys/ref/staxW.i}
                AND stax.tax-group  EQ stax.tax-code1[1]
                AND stax.tax-acc1[1] EQ ar-cashl.actnum NO-LOCK NO-ERROR.

            IF AVAILABLE stax THEN 
            DO:
                v-actnum = stax.tax-acc1[1].
                LEAVE.
            END.
        END. /* for each ar-cashl */

        FOR EACH ar-cashl WHERE ar-cashl.c-no   EQ ar-cash.c-no
            AND ar-cashl.posted EQ YES
            AND ar-cashl.memo   EQ YES
            USE-INDEX c-no NO-LOCK,
            FIRST ar-inv WHERE ar-inv.company  EQ cocode
            AND ar-inv.inv-no   EQ ar-cashl.inv-no
            AND ar-inv.tax-code NE "" NO-LOCK,
            FIRST stax
            {sys/ref/staxW.i}
                  and stax.tax-group eq ar-inv.tax-code no-lock:

        CREATE tt-report.
        ASSIGN
            tt-report.term-id = v-term
            tt-report.key-01  = ar-inv.tax-code
            tt-report.key-02  = IF v-actnum NE "" THEN v-actnum ELSE stax.tax-acc1[1]
            tt-report.rec-id  = RECID(ar-cashl).
    END. /* for each ar-cashl */
END. /* for each ar-cash */
END. /* for each cust */

VIEW FRAME r-top.

DO WHILE TRUE:
    ASSIGN
        v-tax-gr = ""
        v-head1  = ""
        v-head2  = ""
        v-head3  = ""
        i        = 0.
    FOR EACH tt-stax WHERE tt-stax.company = gcompany
        BY tt-stax.tax-acc1[1]:

        ASSIGN
            i           = i + 1
            x           = (27 - length(TRIM(substr(tt-stax.tax-dscr[1],1,23)))) / 2
            v-head1[i]  = "   " + fill("*",x - 1) + " " +
                                  trim(substr(tt-stax.tax-dscr[1],1,23)) +
                                  " " + fill("*",x - 1)    
            v-head2[i]  = "      Taxable $          Tax $"
            v-head3[i]  = FILL("-",30)
            v-tax-gr[i] = tt-stax.tax-group
            v-tax-gl    = tt-stax.tax-acc1[1].

        DELETE tt-stax.
        IF i GT 4 THEN LEAVE.

    END. /* for each stax */

    IF v-tax-gr[1] EQ "" THEN LEAVE.

    IF tb_excel THEN 
    DO:
        ASSIGN
            excelheader1 = ",,," + v-head1[1] + ",," + v-head1[2] + ",," + v-head1[3] + ",," + v-head1[4] + ",," + v-head1[5]
            excelheader2 = "Code,Name,Gross Sales,Taxable $,Tax $,Taxable $,Tax $," + "Taxable $,Tax $," + "Taxable $,Tax $," + "Taxable $,Tax $".
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader1,',','","') '"' SKIP.
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader2,',','","') '"' SKIP.
    END.

    PAGE.

    ASSIGN
        v-sal-amt = 0
        v-taxable = 0
        v-tax-amt = 0.

    FOR EACH tt-report WHERE tt-report.term-id EQ v-term
        BREAK BY tt-report.key-01
        BY tt-report.key-05  /*Invoice No*/
        BY tt-report.key-06: /* Invoice Line*/
                                        
        IF FIRST-OF(tt-report.key-01) THEN 
        DO:
            ASSIGN
                v-rate = 0
                v-frtr = 0.

            FIND FIRST stax
                {sys/ref/staxW.i}
                AND stax.tax-group EQ tt-report.key-01 NO-LOCK NO-ERROR.

            DO i = 1 TO 5:
                IF v-tax-gr[i] NE "" THEN 
                DO:
                    ASSIGN
                        v-rate-t = 0
                        v-frtr-t = 0
                        v-frtr-s = 0.
                    DO j = 1 TO 5:
                        IF v-tax-gr[i] EQ stax.tax-code1[j] THEN 
                        DO:
                            v-rate[i] = v-rate[i] + stax.tax-rate1[j].
                            IF stax.tax-frt1[j] THEN 
                            DO:
                                ASSIGN 
                                    v-frtr[i] = v-frtr[i] + stax.tax-rate1[j]
                                    v-frtr-s  = v-frtr-s + stax.tax-rate1[j].
                            END.

                        END.
                        ASSIGN
                            v-rate-t = v-rate-t + stax.tax-rate1[j]
                            v-frtr-t = v-frtr-t + stax.tax-rate1[j].
                    END.  
                    v-rate-tt[i] = v-rate-t.
                    IF stax.tax-frt1[i] THEN                   
                        v-frtr-tt[i] = v-frtr-t.
                END.
            END.
        END.

        FIND FIRST ar-invl WHERE RECID(ar-invl) EQ tt-report.rec-id NO-LOCK NO-ERROR.

        IF AVAILABLE ar-invl THEN 
        DO:
         
            FIND FIRST ar-inv NO-LOCK 
                WHERE ar-inv.company EQ ar-invl.company
                AND ar-inv.inv-no EQ ar-invl.inv-no NO-ERROR.              
            
            ld = ar-invl.amt.
                     
            v-sal-amt[1] = v-sal-amt[1] + (ld /*- ar-inv.tax-amt*/ ).
            DO i = 1 TO 5:
                IF v-rate[i] EQ 0 THEN NEXT.

                v-taxable[i] = v-taxable[i] + (ld /*- ar-inv.tax-amt*/).
               
                ASSIGN
                    v-inv-tax = ar-invl.amt
                    v-frt-tax = ar-inv.freight.    

                IF v-rate-tt[i] NE 0 THEN
                    v-tax-amt[i] = v-tax-amt[i] + ROUND(v-inv-tax * v-rate[i] / 100,2 /*/ v-rate-tt[i]*/).
                   
                IF FIRST-OF(tt-report.key-05) AND v-frtr-tt[i] NE 0 AND ar-inv.f-bill THEN 
                DO:
                    v-tax-amt[i] = v-tax-amt[i] + ROUND(v-frt-tax *  v-frtr[i] / 100,2 ).
                END.

            END. /* DO i = 1 TO 5: */  
            
        END. /* avail ar-inv */
        ELSE
            IF tt-report.key-02 EQ stax.tax-acc1[1] THEN 
            DO:
                /*        Per Julie, take out anything related to ar-cashl in this column - reversed with 04221408*/
                FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK NO-ERROR.
                /*             
                               if ar-cashl.actnum eq stax.tax-acc1[1] then
                                  do i = 1 to 5:
                                     if v-rate[i] eq 0 then next.
                
                                     v-inv-tax = (ar-cashl.amt-paid - ar-cashl.amt-disc).
                
                                     if v-rate-tt[i] ne 0 then
                                        v-tax-amt[i] = v-tax-amt[i] + (v-inv-tax * (v-rate[i] / v-rate-tt[i])).
                                  end.
                               else do:*/
                v-sal-amt[1] = v-sal-amt[1] + (ar-cashl.amt-paid - ar-cashl.amt-disc).
            /*                  do i = 1 to 5:
                                 if v-rate[i] eq 0 then next.
            
                                 if v-rate-tt[i] ne 0 then
                                    v-taxable[i] = v-taxable[i] + ((ar-cashl.amt-paid - ar-cashl.amt-disc) * (v-rate[i] / v-rate-tt[i])).
            
                              end.
                           end. /* else do: */*/
            END. /* if tt-report.key-02 eq stax.tax-acc1[1] */

        IF LAST-OF(tt-report.key-01) THEN 
        DO:
            DISPLAY 
                stax.tax-group    FORMAT "x(4)"
                stax.tax-dscr[1]  AT 6    FORMAT "x(22)"
                v-sal-amt[1]
                v-taxable[1]
                v-tax-amt[1]
                v-taxable[2]      
                WHEN v-tax-gr[2] NE ""
                v-tax-amt[2]      
                WHEN v-tax-gr[2] NE ""
                v-taxable[3]      
                WHEN v-tax-gr[3] NE ""
                v-tax-amt[3]      
                WHEN v-tax-gr[3] NE ""
                v-taxable[4]      
                WHEN v-tax-gr[4] NE ""
                v-tax-amt[4]      
                WHEN v-tax-gr[4] NE ""
                v-taxable[5]      
                WHEN v-tax-gr[5] NE ""
                v-tax-amt[5]      
                WHEN v-tax-gr[5] NE ""
                WITH FRAME detail NO-BOX NO-LABELS STREAM-IO WIDTH 250.

            IF tb_excel THEN
                PUT STREAM excel UNFORMATTED
                    '"' stax.tax-group                           '",'
                    '"' stax.tax-dscr[1]                         '",'
                    '"' STRING(v-sal-amt[1],"->>,>>>,>>9.99")    '",'
                    '"' STRING(v-taxable[1],"->>,>>>,>>9.99")    '",'
                    '"' STRING(v-tax-amt[1],"->>,>>>,>>9.99")    '",'
                    '"' IF v-tax-gr[2] NE "" THEN
                    STRING(v-taxable[2],"->>,>>>,>>9.99")
                    ELSE ""                                  '",'
                    '"' IF v-tax-gr[2] NE "" THEN
                    STRING(v-tax-amt[2],"->>,>>>,>>9.99")
                    ELSE ""                                  '",'
                    '"' IF v-tax-gr[3] NE "" THEN
                    STRING(v-taxable[3],"->>,>>>,>>9.99")
                    ELSE ""                                  '",'
                    '"' IF v-tax-gr[3] NE "" THEN
                    STRING(v-tax-amt[3],"->>,>>>,>>9.99")
                    ELSE ""                                  '",'
                    '"' IF v-tax-gr[4] NE "" THEN
                    STRING(v-taxable[4],"->>,>>>,>>9.99")
                    ELSE ""                                  '",'
                    '"' IF v-tax-gr[4] NE "" THEN
                    STRING(v-tax-amt[4],"->>,>>>,>>9.99")
                    ELSE ""                                  '",'
                    '"' IF v-tax-gr[5] NE "" THEN
                    STRING(v-taxable[5],"->>,>>>,>>9.99")
                    ELSE ""                                  '",'
                    '"' IF v-tax-gr[5] NE "" THEN
                    STRING(v-tax-amt[5],"->>,>>>,>>9.99")
                    ELSE ""                                  '",'
                    SKIP.

            ASSIGN
                v-sal-amt[2] = v-sal-amt[2] + v-sal-amt[1]
                v-sal-amt[1] = 0.

            DO i = 1 TO 5:
                ASSIGN
                    v-taxable[i + 5] = v-taxable[i + 5] + v-taxable[i]
                    v-tax-amt[i + 5] = v-tax-amt[i + 5] + v-tax-amt[i]
                    v-taxable[i]     = 0
                    v-tax-amt[i]     = 0.

            END.
        END. /* last-of(tt-report.key-01) then do: */
    END. /* for each tt-report */

    CLEAR FRAME totals NO-PAUSE.

    DISPLAY SKIP(1)
        "TOTALS:"                 AT 6
        v-sal-amt[2]              TO 42
        v-taxable[6]
        v-tax-amt[6]
        v-taxable[7]              
        WHEN v-tax-gr[2] NE ""
        v-tax-amt[7]              
        WHEN v-tax-gr[2] NE ""
        v-taxable[8]              
        WHEN v-tax-gr[3] NE ""
        v-tax-amt[8]              
        WHEN v-tax-gr[3] NE ""
        v-taxable[9]              
        WHEN v-tax-gr[4] NE ""
        v-tax-amt[9]              
        WHEN v-tax-gr[4] NE ""
        v-taxable[10]              
        WHEN v-tax-gr[5] NE ""
        v-tax-amt[10]              
        WHEN v-tax-gr[5] NE ""

        WITH FRAME totals NO-BOX NO-LABELS STREAM-IO WIDTH 250.

    IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            SKIP(1)
            '"' "TOTALS:"                                '",'
            '"' ""                                       '",'
            '"' STRING(v-sal-amt[2],"->>,>>>,>>9.99")    '",'
            '"' STRING(v-taxable[6],"->>,>>>,>>9.99")    '",'
            '"' STRING(v-tax-amt[6],"->>,>>>,>>9.99")    '",'
            '"' IF v-tax-gr[2] NE "" THEN
            STRING(v-taxable[7],"->>,>>>,>>9.99")
            ELSE ""                                  '",'
            '"' IF v-tax-gr[2] NE "" THEN
            STRING(v-tax-amt[7],"->>,>>>,>>9.99")
            ELSE ""                                  '",'
            '"' IF v-tax-gr[3] NE "" THEN
            STRING(v-taxable[8],"->>,>>>,>>9.99")
            ELSE ""                                  '",'
            '"' IF v-tax-gr[3] NE "" THEN
            STRING(v-tax-amt[8],"->>,>>>,>>9.99")
            ELSE ""                                  '",'
            '"' IF v-tax-gr[4] NE "" THEN
            STRING(v-taxable[9],"->>,>>>,>>9.99")
            ELSE ""                                  '",'
            '"' IF v-tax-gr[4] NE "" THEN
            STRING(v-tax-amt[9],"->>,>>>,>>9.99")
            ELSE ""                                  '",'
            '"' IF v-tax-gr[5] NE "" THEN
            STRING(v-taxable[10],"->>,>>>,>>9.99")
            ELSE ""                                  '",'
            '"' IF v-tax-gr[5] NE "" THEN
            STRING(v-tax-amt[10],"->>,>>>,>>9.99")
            ELSE ""                                  '",'
            SKIP(1).

END.   /* do while true */

IF tb_excel THEN 
DO:
    OUTPUT STREAM excel CLOSE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-period-dates C-Win 
PROCEDURE show-period-dates :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST period
            WHERE period.company EQ gcompany
            AND period.yr      EQ INT(begin_year:SCREEN-VALUE)
            AND period.pnum    EQ INT(begin_period:SCREEN-VALUE)
            NO-LOCK NO-ERROR.

        IF AVAILABLE period THEN
            ASSIGN
                begin_date:SCREEN-VALUE = STRING(period.pst)
                end_date:SCREEN-VALUE   = STRING(IF period.pend LT TODAY THEN period.pend
                                                                ELSE TODAY).
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
            fi_file:SCREEN-VALUE = "c:\tmp\MonthlyTax.csv".    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

