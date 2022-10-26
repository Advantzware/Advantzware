&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: addon/touch/r-emplog.w

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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

DEFINE NEW SHARED VARIABLE cocode         AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE locode         AS CHARACTER NO-UNDO.

DEFINE            VARIABLE list-name      AS CHARACTER NO-UNDO.
DEFINE            VARIABLE init-dir       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE tmp-dir        AS CHARACTER NO-UNDO.
DEFINE            VARIABLE is-xprint-form AS LOGICAL.
DEFINE            VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE excelheader    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE cFileName      AS CHARACTER NO-UNDO.
DEFINE STREAM excel.


ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE TEMP-TABLE tt-note NO-UNDO
    FIELD employee   LIKE emplogin.employee
    FIELD rec_key    LIKE notes.rec_key
    FIELD note_date  LIKE notes.note_date
    FIELD note_title LIKE notes.note_title
    FIELD note_src   AS CHARACTER
    INDEX noteindex employee note_date.

DEFINE TEMP-TABLE tt-emp NO-UNDO
    FIELD emp AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-6 begin_employee end_employee ~
begin_type end_type begin_end_date end_end_date begin_note-date ~
end_note-date lv-labor-only scr-plant-clock lv-note-only rd-dest fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS selected-company begin_employee ~
end_employee begin_type end_type begin_end_date end_end_date ~
begin_note-date end_note-date lv-labor-only scr-plant-clock lv-note-only ~
rd-dest fi_file tb_OpenCSV tbAutoClose 

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

DEFINE VARIABLE begin_employee   AS CHARACTER FORMAT "X(5)" 
    LABEL "Beginning Employee" 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1.

DEFINE VARIABLE begin_end_date   AS DATE      FORMAT "99/99/9999" 
    LABEL "Beginning Labor Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_note-date  AS DATE      FORMAT "99/99/9999" 
    LABEL "Beginning Note Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_type       AS CHARACTER FORMAT "X(5)" 
    LABEL "Beginning Employee Type" 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1.

DEFINE VARIABLE end_employee     AS CHARACTER FORMAT "X(5)" 
    LABEL "Ending Employee" 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1.

DEFINE VARIABLE end_end_date     AS DATE      FORMAT "99/99/9999" 
    LABEL "Ending Labor Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_note-date    AS DATE      FORMAT "99/99/9999" 
    LABEL "Ending Note Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_type         AS CHARACTER FORMAT "X(5)" INITIAL "zzz" 
    LABEL "Ending Employee Type" 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1.

DEFINE VARIABLE fi_file          AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\LaborReport.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lines-per-page   AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name     AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no       AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE selected-company AS CHARACTER FORMAT "X(3)":U 
    LABEL "Company" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1
    BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE lv-ornt          AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest          AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 16.2 BY 4.81 NO-UNDO.

DEFINE VARIABLE scr-plant-clock  AS INTEGER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Plant Machines", 1,
    "Clock", 2,
    "All", 3
    SIZE 40 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-2
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 8.57.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.24.

DEFINE VARIABLE lv-labor-only AS LOGICAL INITIAL YES 
    LABEL "Print Labor" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE lv-note-only  AS LOGICAL INITIAL YES 
    LABEL "Print Notes" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE tbAutoClose   AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel      AS LOGICAL INITIAL NO 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV    AS LOGICAL INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.8 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm  AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    selected-company AT ROW 2.19 COL 31 COLON-ALIGNED
    begin_employee AT ROW 3.24 COL 31 COLON-ALIGNED HELP
    "Enter Beginning Employee"
    end_employee AT ROW 3.24 COL 70 COLON-ALIGNED HELP
    "Enter Ending Employee"
    begin_type AT ROW 4.29 COL 31 COLON-ALIGNED HELP
    "Enter Beginning Employee Type" WIDGET-ID 10
    end_type AT ROW 4.29 COL 70 COLON-ALIGNED HELP
    "Enter Ending Employee Type" WIDGET-ID 12
    begin_end_date AT ROW 5.33 COL 31 COLON-ALIGNED HELP
    "Enter Beginning Employee"
    end_end_date AT ROW 5.33 COL 70 COLON-ALIGNED HELP
    "Enter Ending Employee"
    begin_note-date AT ROW 6.38 COL 31 COLON-ALIGNED HELP
    "Enter Beginning Employee"
    end_note-date AT ROW 6.38 COL 70 COLON-ALIGNED HELP
    "Enter Ending Employee"
    lv-labor-only AT ROW 7.81 COL 23
    scr-plant-clock AT ROW 8.91 COL 43 NO-LABELS WIDGET-ID 4
    lv-note-only AT ROW 9 COL 23
    rd-dest AT ROW 10.95 COL 4.8 NO-LABELS
    lv-font-no AT ROW 11 COL 36 COLON-ALIGNED
    lv-ornt AT ROW 11 COL 44 NO-LABELS
    lines-per-page AT ROW 11 COL 87 COLON-ALIGNED
    lv-font-name AT ROW 11.95 COL 29 COLON-ALIGNED NO-LABELS
    tb_excel AT ROW 13.14 COL 72
    td-show-parm AT ROW 13.76 COL 28.4
    fi_file AT ROW 14.67 COL 26.4 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 14.71 COL 92.4 RIGHT-ALIGNED
    tbAutoClose AT ROW 16 COL 28.4 WIDGET-ID 64
    btn-ok AT ROW 16.95 COL 28.2
    btn-cancel AT ROW 16.95 COL 53.8
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 5
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 10.29 COL 4
    "Time Calculation Basis:" VIEW-AS TEXT
    SIZE 22 BY .62 AT ROW 8 COL 50.2 WIDGET-ID 8
    RECT-2 AT ROW 1.71 COL 3
    RECT-6 AT ROW 10.76 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 23.05
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
        TITLE              = "Labor Report"
        HEIGHT             = 17.38
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
   FRAME-NAME L-To-R                                                    */
ASSIGN 
    begin_employee:PRIVATE-DATA IN FRAME FRAME-A = "save".

ASSIGN 
    begin_end_date:PRIVATE-DATA IN FRAME FRAME-A = "save".

ASSIGN 
    begin_note-date:PRIVATE-DATA IN FRAME FRAME-A = "save".

ASSIGN 
    begin_type:PRIVATE-DATA IN FRAME FRAME-A = "save".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_employee:PRIVATE-DATA IN FRAME FRAME-A = "save".

ASSIGN 
    end_end_date:PRIVATE-DATA IN FRAME FRAME-A = "save".

ASSIGN 
    end_note-date:PRIVATE-DATA IN FRAME FRAME-A = "save".

ASSIGN 
    end_type:PRIVATE-DATA IN FRAME FRAME-A = "save".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "save".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lines-per-page:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-font-name:HIDDEN IN FRAME FRAME-A       = TRUE
    lv-font-name:PRIVATE-DATA IN FRAME FRAME-A = "save".

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-font-no:HIDDEN IN FRAME FRAME-A       = TRUE
    lv-font-no:PRIVATE-DATA IN FRAME FRAME-A = "save".

ASSIGN 
    lv-labor-only:PRIVATE-DATA IN FRAME FRAME-A = "save".

ASSIGN 
    lv-note-only:PRIVATE-DATA IN FRAME FRAME-A = "save".

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-ornt:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN selected-company IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    selected-company:PRIVATE-DATA IN FRAME FRAME-A = "save".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "save".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "save".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A       = TRUE
    td-show-parm:PRIVATE-DATA IN FRAME FRAME-A = "save".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Labor Report */
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
ON WINDOW-CLOSE OF C-Win /* Labor Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_end_date C-Win
ON LEAVE OF begin_end_date IN FRAME FRAME-A /* Beginning Labor Date */
    DO:
        begin_note-date:SCREEN-VALUE = SELF:SCREEN-VALUE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
    DO:
        APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
    DO:
        ASSIGN {&displayed-objects}.
  
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
                    {custom/asifax.i &type="Labor Report"
                       &begin_cust=begin_employee
                       &end_cust=end_employee
                       &fax-subject=CURRENT-WINDOW:TITLE
                       &fax-body=CURRENT-WINDOW:TITLE
                       &fax-file=list-name}
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &type="Labor Report"
                          &begin_cust=begin_employee
                          &end_cust=end_employee
                          &mail-subject=CURRENT-WINDOW:TITLE
                          &mail-body=CURRENT-WINDOW:TITLE
                          &mail-file=list-name}
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &type="Labor Report"
                           &begin_cust=begin_employee
                           &end_cust=end_employee
                           &mail-subject=CURRENT-WINDOW:TITLE
                           &mail-body=CURRENT-WINDOW:TITLE
                           &mail-file=list-name}
                    END.
                END.
            WHEN 6 THEN RUN output-to-port.
        END CASE.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_end_date C-Win
ON LEAVE OF end_end_date IN FRAME FRAME-A /* Ending Labor Date */
    DO:
        end_note-date:SCREEN-VALUE = SELF:SCREEN-VALUE.
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
        ASSIGN {&SELF-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN windows/l-fonts.w (FOCUS:SCREEN-VALUE,OUTPUT char-val).
        IF char-val NE "" THEN ASSIGN FOCUS:SCREEN-VALUE        = ENTRY(1,char-val)
                lv-font-name:SCREEN-VALUE = ENTRY(2,char-val).
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
    RUN init-proc.
  
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "TR9" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    DO WITH FRAME {&frame-name}:
        {custom/usrprint.i}
    END.
    {methods/nowait.i}
    APPLY 'ENTRY' TO begin_employee IN FRAME {&FRAME-NAME}.
  
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
    DISPLAY selected-company begin_employee end_employee begin_type end_type 
        begin_end_date end_end_date begin_note-date end_note-date 
        lv-labor-only scr-plant-clock lv-note-only rd-dest fi_file tb_OpenCSV 
        tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-2 RECT-6 begin_employee end_employee begin_type end_type 
        begin_end_date end_end_date begin_note-date end_note-date 
        lv-labor-only scr-plant-clock lv-note-only rd-dest fi_file tb_OpenCSV 
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
    ASSIGN
        selected-company = g_company
        begin_employee   = ''
        end_employee     = 'zzzzzzzz'
        end_end_date     = TODAY
        end_note-date    = TODAY.

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
    DEFINE VARIABLE printok   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
    DEFINE VARIABLE result    AS LOGICAL   NO-UNDO.

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
    RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-2 C-Win 
PROCEDURE proc-2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-emp AS CHARACTER NO-UNDO.

    DEFINE VARIABLE li-overtime      AS INTEGER LABEL "OT @x1.5" NO-UNDO.
    DEFINE VARIABLE li-2time         AS INTEGER LABEL "OT @x2" NO-UNDO.
    DEFINE VARIABLE li-day-time      AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-lunch-time    AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-lunch-emp     AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-start-time    AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-end-time      AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-over      AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-2time     AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-tot       AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-tot-hr    AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-tot-min   AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-over-tot-hr   AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-over-tot-min  AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-2time-tot-hr  AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-2time-tot-min AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll-first-emp     AS LOGICAL NO-UNDO.

    FOR EACH emplogin NO-LOCK WHERE 
        emplogin.company EQ cocode AND
        emplogin.employee = ip-emp AND
        emplogin.start_date >= begin_end_date AND
        emplogin.START_date <= end_end_date AND
        emplogin.machine EQ "CLOCK"
        BREAK BY emplogin.employee BY emplogin.start_date BY start_time:

        IF FIRST-OF(emplogin.employee) THEN 
        DO:
            FIND employee WHERE employee.employee = emplogin.employee  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE employee THEN NEXT.
            /*  put "Employee: " employee.employee "  " employee.first_name employee.last_name skip
                  "====================================================================" skip.                    
              IF tb_excel THEN PUT STREAM excel UNFORMATTED 
                 employee.employee "," replace(employee.first_name + " " + employee.last_name,",",".") ",".
              */
            IF lv-labor-only THEN 
                PUT            "                 Login      Logout     Hrs                            " SKIP  
                    "Start Date Shift Time       Time       Worked     OT @x1.5   OT @x2" SKIP
                    "---------- ----- ---------- ---------- ---------  ---------- -------" SKIP.
            ASSIGN 
                ll-first-emp = TRUE.
        END.                      

        ASSIGN 
            li-day-time   = emplogin.total_time
            li-overtime   = 0
            li-2time      = 0
            li-start-time = IF li-start-time = 0 THEN emplogin.start_time ELSE li-start-time.
        li-end-time = emplogin.end_time.
        ACCUMULATE li-day-time (total by emplogin.employee by emplogin.start_date).
        /* ===== daily total display */
        IF LAST-OF(emplogin.start_date) THEN 
        DO:
            FIND shifts WHERE shifts.company = emplogin.company AND
                shifts.shift = emplogin.shift
                NO-LOCK NO-ERROR.
            IF AVAILABLE shifts AND NOT shifts.lunch_paid THEN 
            DO:
                IF  emplogin.end_time <= shifts.lunch_start THEN li-lunch-time = 0.  
                ELSE IF emplogin.start_time >= shifts.lunch_end THEN li-lunch-time = 0.
                    ELSE IF emplogin.start_time <= shifts.lunch_start AND
                            emplogin.end_time >= shifts.lunch_start AND
                            emplogin.end_time <= shifts.lunch_end 
                            THEN li-lunch-time = emplogin.end_time - shifts.lunch_start.
                        ELSE IF emplogin.start_time >= shifts.lunch_start AND
                                emplogin.start_time <= shifts.lunch_end AND
                                emplogin.end_time >= shifts.lunch_end 
                                THEN li-lunch-time = shifts.lunch_end - emplogin.start_time .
                            ELSE IF emplogin.start_time >= shifts.lunch_start AND
                                    emplogin.end_time <= shifts.lunch_end 
                                    THEN li-lunch-time = emplogin.end_time - emplogin.start_time.                    
                                ELSE li-lunch-time = shifts.lunch_end - shifts.lunch_start .          
            END.
            ELSE li-lunch-time = 0.
            IF employee.lunch_paid THEN li-lunch-time = 0. 
            li-lunch-emp = li-lunch-emp + li-lunch-time.
            IF (ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time > 43200  /*12 Hr  (x 3600) */ 
                THEN  ASSIGN li-2time    = (ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time - 43200
                    li-overtime = 14400.
            ELSE IF ((ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time) > 28800  AND 
                    ((ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time)  <= 43200
                    THEN ASSIGN li-2time    = 0
                        li-overtime = (ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time - 28800. /* 8 Hr */
            ASSIGN 
                li-emp-over  = li-emp-over + li-overtime 
                li-emp-2time = li-emp-2time + li-2time .
            IF lv-labor-only THEN 
            DO:        
                DISPLAY emplogin.start_date
                    emplogin.shift FORM "x(5)"
                    STRING(li-start-time,"HH:MM AM") /* column-label "Login!Time" */ FORM "x(10)" 
                    STRING(li-end-time,"HH:MM AM")  /*column-label "Logout!Time" */ FORM "x(10)"
                    IF (ACCUM TOTAL BY emplogin.start_date li-day-time) <> 0 then 
                   string((accum total by emplogin.start_date li-day-time) - li-lunch-time, "HH:MM")
                   else "0"      @ li-day-time
                   string(li-overtime, "HH:MM") when li-overtime <> 0 @ li-overtime
                   string(li-2time, "HH:MM") when li-2time <> 0 @ li-2time
                with frame det stream-io  down no-label.
                IF tb_excel THEN PUT STREAM excel UNFORMATTED
                        (IF ll-first-emp THEN "" ELSE ",,")
                        (IF emplogin.start_date = ? THEN "" ELSE STRING(emplogin.start_date))  ","
                        emplogin.shift ","
                        STRING(li-start-time,"HH:MM AM")  ","
                        STRING(li-end-time,"HH:MM AM")  ","
                  (if  (accum total by emplogin.start_date li-day-time) <> 0
                   AND (accum total by emplogin.start_date li-day-time) <> ?
                       then string((accum total by emplogin.start_date li-day-time) - li-lunch-time, "HH:MM")
                       else "0") ","
                  string(li-overtime, "HH:MM") ","
                  string(li-2time, "HH:MM")  SKIP.
                ASSIGN 
                    ll-first-emp = FALSE.
            END.
            li-start-time = 0.  
        END. 

        IF LAST-OF(emplogin.employee) THEN 
        DO:
            IF lv-labor-only THEN 
            DO:
                DOWN WITH FRAME det.
                ASSIGN 
                    li-emp-tot = IF (ACCUM TOTAL BY emplogin.employee li-day-time) <> 0 then ((accum total by emplogin.employee li-day-time) - li-lunch-emp ) else 0 
                     li-emp-tot = IF li-emp-tot = ? THEN 0 ELSE li-emp-tot
                     li-emp-over = if li-emp-over <> 0 then (li-emp-over /*- li-lunch-emp*/) else 0
                     li-emp-2time = if li-emp-2time <> 0 then (li-emp-2time /*- li-lunch-emp*/ ) else 0
                     li-emp-tot-hr = truncate(li-emp-tot / 3600,0)
                     li-emp-tot-min = truncate((li-emp-tot mod 3600) / 60,0)
                     li-over-tot-hr = if li-emp-over > 0 then truncate(li-emp-over / 3600,0) else 0
                     li-over-tot-min = if li-emp-over > 0 then truncate((li-emp-over mod 3600) / 60,0) else 0
                     li-2time-tot-hr = if li-emp-2time > 0 then truncate(li-emp-2time / 3600,0) else 0
                     li-2time-tot-min = if li-emp-2time > 0 then truncate((li-emp-2time mod 3600) / 60,0) else 0.
                DISPLAY "Total" @ emplogin.start_date
                    STRING(li-emp-tot-hr,">99") + ":" + string(li-emp-tot-min,"99") 
                    WHEN li-emp-tot <> 0 @ li-day-time
                    STRING(li-over-tot-hr,">99") + ":" + string(li-over-tot-min,"99") 
                    WHEN li-emp-over <> 0 @ li-overtime
                    STRING(li-2time-tot-hr,">99") + ":" + string(li-2time-tot-min,"99") 
                    WHEN li-emp-2time <> 0 @ li-2time
                    WITH FRAME det DOWN.
                PUT SKIP(1).  
                IF tb_excel THEN PUT STREAM excel UNFORMATTED
                        ",,Total,,,,"
                        STRING(li-emp-tot-hr,">99") + ":" + string(li-emp-tot-min,"99")  "," 
                        STRING(li-over-tot-hr,">99") + ":" + string(li-over-tot-min,"99")  "," 
                        STRING(li-2time-tot-hr,">99") + ":" + string(li-2time-tot-min,"99")
                        SKIP.
            END.  
            ASSIGN 
                li-lunch-emp = 0
                li-emp-over  = 0
                li-emp-2time = 0.
        /*if lv-note-only then do:
           put "Notes:" skip .
           for each tt-note where tt-note.employee = emplogin.employee
                               by tt-note.note_date:
               put "     " tt-note.note_date " " tt-note.note_title skip.
           end.                       
           put skip(1).
        end.
       */ 
        END.           
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-all C-Win 
PROCEDURE proc-all :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-emp AS CHARACTER NO-UNDO.

    DEFINE VARIABLE li-overtime      AS INTEGER LABEL "OT @x1.5" NO-UNDO.
    DEFINE VARIABLE li-2time         AS INTEGER LABEL "OT @x2" NO-UNDO.
    DEFINE VARIABLE li-day-time      AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-lunch-time    AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-lunch-emp     AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-start-time    AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-end-time      AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-over      AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-2time     AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-tot       AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-tot-hr    AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-tot-min   AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-over-tot-hr   AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-over-tot-min  AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-2time-tot-hr  AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-2time-tot-min AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll-first-emp     AS LOGICAL NO-UNDO.

    FOR EACH emplogin NO-LOCK WHERE 
        emplogin.company EQ cocode AND
        emplogin.employee = ip-emp AND
        emplogin.start_date >= begin_end_date AND
        emplogin.START_date <= end_end_date 
        BREAK BY emplogin.employee BY emplogin.start_date BY start_time:

        IF FIRST-OF(emplogin.employee) THEN 
        DO:
            FIND employee WHERE employee.employee = emplogin.employee  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE employee THEN NEXT.
            /*  put "Employee: " employee.employee "  " employee.first_name employee.last_name skip
                  "====================================================================" skip.                    
              IF tb_excel THEN PUT STREAM excel UNFORMATTED 
                 employee.employee "," replace(employee.first_name + " " + employee.last_name,",",".") ",".
              */
            IF lv-labor-only THEN 
                PUT            "                 Login      Logout     Hrs                            " SKIP  
                    "Start Date Shift Time       Time       Worked     OT @x1.5   OT @x2" SKIP
                    "---------- ----- ---------- ---------- ---------  ---------- -------" SKIP.
            ASSIGN 
                ll-first-emp = TRUE.
        END.                      

        ASSIGN 
            li-day-time   = emplogin.total_time
            li-overtime   = 0
            li-2time      = 0
            li-start-time = IF li-start-time = 0 THEN emplogin.start_time ELSE li-start-time.
        li-end-time = emplogin.end_time.
        ACCUMULATE li-day-time (total by emplogin.employee by emplogin.start_date).
        /* ===== daily total display */
        IF LAST-OF(emplogin.start_date) THEN 
        DO:
            FIND shifts WHERE shifts.company = emplogin.company AND
                shifts.shift = emplogin.shift
                NO-LOCK NO-ERROR.
            IF AVAILABLE shifts AND NOT shifts.lunch_paid THEN 
            DO:
                IF  emplogin.end_time <= shifts.lunch_start THEN li-lunch-time = 0.  
                ELSE IF emplogin.start_time >= shifts.lunch_end THEN li-lunch-time = 0.
                    ELSE IF emplogin.start_time <= shifts.lunch_start AND
                            emplogin.end_time >= shifts.lunch_start AND
                            emplogin.end_time <= shifts.lunch_end 
                            THEN li-lunch-time = emplogin.end_time - shifts.lunch_start.
                        ELSE IF emplogin.start_time >= shifts.lunch_start AND
                                emplogin.start_time <= shifts.lunch_end AND
                                emplogin.end_time >= shifts.lunch_end 
                                THEN li-lunch-time = shifts.lunch_end - emplogin.start_time .
                            ELSE IF emplogin.start_time >= shifts.lunch_start AND
                                    emplogin.end_time <= shifts.lunch_end 
                                    THEN li-lunch-time = emplogin.end_time - emplogin.start_time.                    
                                ELSE li-lunch-time = shifts.lunch_end - shifts.lunch_start .          
            END.
            ELSE li-lunch-time = 0.
            IF employee.lunch_paid THEN li-lunch-time = 0. 
            li-lunch-emp = li-lunch-emp + li-lunch-time.
            IF (ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time > 43200  /*12 Hr  (x 3600) */ 
                THEN  ASSIGN li-2time    = (ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time - 43200
                    li-overtime = 14400.
            ELSE IF ((ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time) > 28800  AND 
                    ((ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time)  <= 43200
                    THEN ASSIGN li-2time    = 0
                        li-overtime = (ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time - 28800. /* 8 Hr */
            ASSIGN 
                li-emp-over  = li-emp-over + li-overtime 
                li-emp-2time = li-emp-2time + li-2time .
            IF lv-labor-only THEN 
            DO:        
                DISPLAY emplogin.start_date
                    emplogin.shift FORM "x(5)"
                    STRING(li-start-time,"HH:MM AM") /* column-label "Login!Time" */ FORM "x(10)" 
                    STRING(li-end-time,"HH:MM AM")  /*column-label "Logout!Time" */ FORM "x(10)"
                    IF (ACCUM TOTAL BY emplogin.start_date li-day-time) <> 0 then 
                   string((accum total by emplogin.start_date li-day-time) - li-lunch-time, "HH:MM")
                   else "0"      @ li-day-time
                   string(li-overtime, "HH:MM") when li-overtime <> 0 @ li-overtime
                   string(li-2time, "HH:MM") when li-2time <> 0 @ li-2time
                with frame det stream-io  down no-label.
                IF tb_excel THEN PUT STREAM excel UNFORMATTED
                        (IF ll-first-emp THEN "" ELSE ",,")
                        (IF emplogin.start_date = ? THEN "" ELSE STRING(emplogin.start_date))  ","
                        emplogin.shift ","
                        STRING(li-start-time,"HH:MM AM")  ","
                        STRING(li-end-time,"HH:MM AM")  ","
                  (if  (accum total by emplogin.start_date li-day-time) <> 0
                   AND (accum total by emplogin.start_date li-day-time) <> ?
                       then string((accum total by emplogin.start_date li-day-time) - li-lunch-time, "HH:MM")
                       else "0") ","
                  string(li-overtime, "HH:MM") ","
                  string(li-2time, "HH:MM")  SKIP.
                ASSIGN 
                    ll-first-emp = FALSE.
            END.
            li-start-time = 0.  
        END. 

        IF LAST-OF(emplogin.employee) THEN 
        DO:
            IF lv-labor-only THEN 
            DO:
                DOWN WITH FRAME det.
                ASSIGN 
                    li-emp-tot = IF (ACCUM TOTAL BY emplogin.employee li-day-time) <> 0 then ((accum total by emplogin.employee li-day-time) - li-lunch-emp ) else 0 
                     li-emp-tot = IF li-emp-tot = ? THEN 0 ELSE li-emp-tot
                     li-emp-over = if li-emp-over <> 0 then (li-emp-over /*- li-lunch-emp*/) else 0
                     li-emp-2time = if li-emp-2time <> 0 then (li-emp-2time /*- li-lunch-emp*/ ) else 0
                     li-emp-tot-hr = truncate(li-emp-tot / 3600,0)
                     li-emp-tot-min = truncate((li-emp-tot mod 3600) / 60,0)
                     li-over-tot-hr = if li-emp-over > 0 then truncate(li-emp-over / 3600,0) else 0
                     li-over-tot-min = if li-emp-over > 0 then truncate((li-emp-over mod 3600) / 60,0) else 0
                     li-2time-tot-hr = if li-emp-2time > 0 then truncate(li-emp-2time / 3600,0) else 0
                     li-2time-tot-min = if li-emp-2time > 0 then truncate((li-emp-2time mod 3600) / 60,0) else 0.
                DISPLAY "Total" @ emplogin.start_date
                    STRING(li-emp-tot-hr,">99") + ":" + string(li-emp-tot-min,"99") 
                    WHEN li-emp-tot <> 0 @ li-day-time
                    STRING(li-over-tot-hr,">99") + ":" + string(li-over-tot-min,"99") 
                    WHEN li-emp-over <> 0 @ li-overtime
                    STRING(li-2time-tot-hr,">99") + ":" + string(li-2time-tot-min,"99") 
                    WHEN li-emp-2time <> 0 @ li-2time
                    WITH FRAME det DOWN.
                PUT SKIP(1).  
                IF tb_excel THEN PUT STREAM excel UNFORMATTED
                        ",,Total,,,,"
                        STRING(li-emp-tot-hr,">99") + ":" + string(li-emp-tot-min,"99")  "," 
                        STRING(li-over-tot-hr,">99") + ":" + string(li-over-tot-min,"99")  "," 
                        STRING(li-2time-tot-hr,">99") + ":" + string(li-2time-tot-min,"99")
                        SKIP.
            END.  
            ASSIGN 
                li-lunch-emp = 0
                li-emp-over  = 0
                li-emp-2time = 0.
        /*if lv-note-only then do:
           put "Notes:" skip .
           for each tt-note where tt-note.employee = emplogin.employee
                               by tt-note.note_date:
               put "     " tt-note.note_date " " tt-note.note_title skip.
           end.                       
           put skip(1).
        end.
       */ 
        END.           
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /*==== Report main body procedure ================================*/

    DEFINE VARIABLE li-overtime      AS INTEGER LABEL "OT @x1.5" NO-UNDO.
    DEFINE VARIABLE li-2time         AS INTEGER LABEL "OT @x2" NO-UNDO.
    DEFINE VARIABLE li-day-time      AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-lunch-time    AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-lunch-emp     AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-start-time    AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-end-time      AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-over      AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-2time     AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-tot       AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-tot-hr    AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-emp-tot-min   AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-over-tot-hr   AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-over-tot-min  AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-2time-tot-hr  AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-2time-tot-min AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll-first-emp     AS LOGICAL NO-UNDO.
    DEFINE VARIABLE v-date           AS DATE    NO-UNDO.
    DEFINE VARIABLE v-date-2         AS DATE    NO-UNDO.

    DEFINE BUFFER bf-employee FOR employee.
    DEFINE BUFFER bf-machemp  FOR machemp.

    SESSION:SET-WAIT-STATE('general').

    EMPTY TEMP-TABLE tt-note.

    {sys/form/r-top3w.f}

    FORM HEADER SKIP(1) WITH FRAME r-top.

    ASSIGN
        str-tit2 = c-win:TITLE + "  (T-R-9)" 
        {sys/inc/ctrtext.i str-tit2 112}
        str-tit3 = FILL('-',132)
        {sys/inc/ctrtext.i str-tit3 132}.

    {sys/inc/print1.i}
    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    VIEW FRAME r-top.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        ASSIGN 
            excelheader = "Employee,Name".
        IF lv-labor-only THEN ASSIGN excelheader = excelheader +
      ",Start Date,Shift,Login Time,Logout Time,Worked,OT @ 1.5, OT @ 2".
        PUT STREAM excel UNFORMATTED excelheader SKIP.
    END.

    EMPTY TEMP-TABLE tt-emp.

    FOR EACH employee FIELDS(employee) WHERE
        employee.company EQ cocode AND
        employee.employee >= begin_employee AND
        employee.employee <= END_employee AND
        employee.emp_type >= begin_type AND
        employee.emp_type <= end_type
        NO-LOCK:

        CREATE tt-emp.
        ASSIGN 
            tt-emp.emp = employee.employee.
        RELEASE tt-emp.
    END.

    FOR EACH tt-emp BREAK BY tt-emp.emp:
        STATUS DEFAULT "Processing .. for employee " + tt-emp.emp.

        IF FIRST-OF(tt-emp.emp) THEN 
        DO:
            FIND employee WHERE employee.employee = tt-emp.emp  NO-LOCK NO-ERROR.
            PUT "Employee: " .
            IF AVAILABLE employee THEN 
                PUT employee.employee "  " employee.first_name employee.last_name.
            ELSE tt-emp.emp.
            PUT SKIP 
                "====================================================================" SKIP.     
            IF tb_excel THEN
                PUT STREAM excel UNFORMATTED 
                    employee.employee "," REPLACE(employee.first_name + " " + employee.last_name,",",".") ",".
        END.
        IF lv-note-only THEN 
        DO:
            FOR EACH bf-employee FIELDS(rec_key employee) WHERE
                bf-employee.company EQ cocode AND
                bf-employee.employee = tt-emp.emp AND
                CAN-FIND(FIRST notes WHERE notes.rec_key = bf-employee.rec_key) NO-LOCK:

                DO v-date-2 = begin_note-date TO end_note-date:
                    FOR EACH notes FIELDS(rec_key note_date note_title) WHERE
                        notes.rec_key = bf-employee.rec_key AND
                        notes.note_date = v-date-2 NO-LOCK:

                        CREATE tt-note.
                        ASSIGN 
                            tt-note.employee   = bf-employee.employee
                            tt-note.rec_key    = notes.rec_key
                            tt-note.note_date  = notes.note_date
                            tt-note.note_title = notes.note_title
                            tt-note.note_src   = "Employee".                
                    END.
                END.
            END.
            FOR EACH emplogin FIELDS(rec_key employee) WHERE
                emplogin.company EQ cocode AND
                emplogin.employee = tt-emp.emp AND
                CAN-FIND(FIRST notes WHERE notes.rec_key = emplogin.rec_key
                AND notes.note_date >= begin_note-date 
                AND notes.note_date <= END_note-date) NO-LOCK:
                DO v-date-2 = begin_note-date TO end_note-date:
                    FOR EACH notes FIELDS(rec_key note_date note_title) WHERE
                        notes.rec_key = emplogin.rec_key AND 
                        notes.note_date = v-date-2 NO-LOCK:

                        CREATE tt-note.
                        ASSIGN 
                            tt-note.employee   = emplogin.employee
                            tt-note.rec_key    = notes.rec_key
                            tt-note.note_date  = notes.note_date
                            tt-note.note_title = notes.note_title
                            tt-note.note_src   = "Log In/Out".

                    END.              
                END.
            END. /*end for each emplogin*/

            FOR EACH bf-machemp FIELDS(rec_key employee) WHERE
                bf-machemp.employee = tt-emp.emp AND
                CAN-FIND(FIRST notes WHERE notes.rec_key = bf-machemp.rec_key
                AND notes.note_date >= begin_note-date
                AND notes.note_date <= END_note-date) NO-LOCK:
                DO v-date-2 = begin_note-date TO end_note-date:
                    FOR EACH notes FIELDS(rec_key note_date note_title) WHERE
                        notes.rec_key = bf-machemp.rec_key AND
                        notes.note_date = v-date-2 NO-LOCK:
                        CREATE tt-note.
                        ASSIGN 
                            tt-note.employee   = bf-machemp.employee
                            tt-note.rec_key    = notes.rec_key
                            tt-note.note_date  = notes.note_date
                            tt-note.note_title = notes.note_title
                            tt-note.note_src   = "Emp. Transaction".

                    END.
                END.
            END.              
        END. /* lv-note-only*/

        IF lv-labor-only THEN 
        DO:
            IF scr-plant-clock = 1 THEN
                FOR EACH emplogin NO-LOCK WHERE 
                    emplogin.company EQ cocode AND
                    emplogin.employee = tt-emp.emp AND
                    emplogin.start_date >= begin_end_date AND
                    emplogin.START_date <= end_end_date AND
                    emplogin.machine NE "CLOCK"
                    BREAK BY emplogin.employee BY emplogin.start_date BY start_time:

                    IF FIRST-OF(emplogin.employee) THEN 
                    DO:
                        /* find employee where employee.employee = emplogin.employee  no-lock no-error.
                         if not avail employee then next.
                         put "Employee: " employee.employee "  " employee.first_name employee.last_name skip
                             "====================================================================" skip.           
                         IF tb_excel THEN PUT STREAM excel UNFORMATTED 
                            employee.employee "," replace(employee.first_name + " " + employee.last_name,",",".") ",".
                         */
                        IF lv-labor-only THEN 
                            PUT            "                 Login      Logout     Hrs                            " SKIP  
                                "Start Date Shift Time       Time       Worked     OT @x1.5   OT @x2" SKIP
                                "---------- ----- ---------- ---------- ---------  ---------- -------" SKIP.
                        ASSIGN 
                            ll-first-emp = TRUE.
                    END.                      

                    ASSIGN 
                        li-day-time   = emplogin.total_time
                        li-overtime   = 0
                        li-2time      = 0
                        li-start-time = IF li-start-time = 0 THEN emplogin.start_time ELSE li-start-time.
                    li-end-time = emplogin.end_time.
                    ACCUMULATE li-day-time (total by emplogin.employee by emplogin.start_date).
                    /* ===== daily total display */
                    IF LAST-OF(emplogin.start_date) THEN 
                    DO:
                        FIND shifts WHERE shifts.company = emplogin.company AND
                            shifts.shift = emplogin.shift
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE shifts AND NOT shifts.lunch_paid THEN 
                        DO:
                            IF  emplogin.end_time <= shifts.lunch_start THEN li-lunch-time = 0.  
                            ELSE IF emplogin.start_time >= shifts.lunch_end THEN li-lunch-time = 0.
                                ELSE IF emplogin.start_time <= shifts.lunch_start AND
                                        emplogin.end_time >= shifts.lunch_start AND
                                        emplogin.end_time <= shifts.lunch_end 
                                        THEN li-lunch-time = emplogin.end_time - shifts.lunch_start.
                                    ELSE IF emplogin.start_time >= shifts.lunch_start AND
                                            emplogin.start_time <= shifts.lunch_end AND
                                            emplogin.end_time >= shifts.lunch_end 
                                            THEN li-lunch-time = shifts.lunch_end - emplogin.start_time .
                                        ELSE IF emplogin.start_time >= shifts.lunch_start AND
                                                emplogin.end_time <= shifts.lunch_end 
                                                THEN li-lunch-time = emplogin.end_time - emplogin.start_time.                    
                                            ELSE li-lunch-time = shifts.lunch_end - shifts.lunch_start .          
                        END.
                        ELSE li-lunch-time = 0.
                        IF employee.lunch_paid THEN li-lunch-time = 0. 
                        li-lunch-emp = li-lunch-emp + li-lunch-time.
                        IF (ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time > 43200  /*12 Hr  (x 3600) */ 
                            THEN  ASSIGN li-2time    = (ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time - 43200
                                li-overtime = 14400.
                        ELSE IF ((ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time) > 28800  AND 
                                ((ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time)  <= 43200
                                THEN ASSIGN li-2time    = 0
                                    li-overtime = (ACCUM TOTAL BY emplogin.start_date li-day-time) - li-lunch-time - 28800. /* 8 Hr */
                        ASSIGN 
                            li-emp-over  = li-emp-over + li-overtime 
                            li-emp-2time = li-emp-2time + li-2time .
                        IF lv-labor-only THEN 
                        DO:        
                            DISPLAY emplogin.start_date
                                emplogin.shift FORM "x(5)"
                                STRING(li-start-time,"HH:MM AM") /* column-label "Login!Time" */ FORM "x(10)" 
                                STRING(li-end-time,"HH:MM AM")  /*column-label "Logout!Time" */ FORM "x(10)"
                                IF (ACCUM TOTAL BY emplogin.start_date li-day-time) <> 0 then 
                   string((accum total by emplogin.start_date li-day-time) - li-lunch-time, "HH:MM")
                   else "0"      @ li-day-time
                   string(li-overtime, "HH:MM") when li-overtime <> 0 @ li-overtime
                   string(li-2time, "HH:MM") when li-2time <> 0 @ li-2time
                with frame det stream-io  down no-label.
                            IF tb_excel THEN PUT STREAM excel UNFORMATTED
                                    (IF ll-first-emp THEN "" ELSE ",,")
                                    (IF emplogin.start_date = ? THEN "" ELSE STRING(emplogin.start_date))  ","
                                    emplogin.shift ","
                                    STRING(li-start-time,"HH:MM AM")  ","
                                    STRING(li-end-time,"HH:MM AM")  ","
                  (if  (accum total by emplogin.start_date li-day-time) <> 0
                   AND (accum total by emplogin.start_date li-day-time) <> ?
                       then string((accum total by emplogin.start_date li-day-time) - li-lunch-time, "HH:MM")
                       else "0") ","
                  string(li-overtime, "HH:MM") ","
                  string(li-2time, "HH:MM")  SKIP.
                            ASSIGN 
                                ll-first-emp = FALSE.
                        END.
                        li-start-time = 0.  
                    END. 

                    IF LAST-OF(emplogin.employee) THEN 
                    DO:
                        IF lv-labor-only THEN 
                        DO:
                            DOWN WITH FRAME det.
                            ASSIGN 
                                li-emp-tot = IF (ACCUM TOTAL BY emplogin.employee li-day-time) <> 0 then ((accum total by emplogin.employee li-day-time) - li-lunch-emp ) else 0 
                     li-emp-tot = IF li-emp-tot = ? THEN 0 ELSE li-emp-tot
                     li-emp-over = if li-emp-over <> 0 then (li-emp-over /*- li-lunch-emp*/) else 0
                     li-emp-2time = if li-emp-2time <> 0 then (li-emp-2time /*- li-lunch-emp*/ ) else 0
                     li-emp-tot-hr = truncate(li-emp-tot / 3600,0)
                     li-emp-tot-min = truncate((li-emp-tot mod 3600) / 60,0)
                     li-over-tot-hr = if li-emp-over > 0 then truncate(li-emp-over / 3600,0) else 0
                     li-over-tot-min = if li-emp-over > 0 then truncate((li-emp-over mod 3600) / 60,0) else 0
                     li-2time-tot-hr = if li-emp-2time > 0 then truncate(li-emp-2time / 3600,0) else 0
                     li-2time-tot-min = if li-emp-2time > 0 then truncate((li-emp-2time mod 3600) / 60,0) else 0.
                            DISPLAY "Total" @ emplogin.start_date
                                STRING(li-emp-tot-hr,">99") + ":" + string(li-emp-tot-min,"99") 
                                WHEN li-emp-tot <> 0 @ li-day-time
                                STRING(li-over-tot-hr,">99") + ":" + string(li-over-tot-min,"99") 
                                WHEN li-emp-over <> 0 @ li-overtime
                                STRING(li-2time-tot-hr,">99") + ":" + string(li-2time-tot-min,"99") 
                                WHEN li-emp-2time <> 0 @ li-2time
                                WITH FRAME det DOWN.
                            PUT SKIP(1).  
                            IF tb_excel THEN PUT STREAM excel UNFORMATTED
                                    ",,Total,,,,"
                                    STRING(li-emp-tot-hr,">99") + ":" + string(li-emp-tot-min,"99")  "," 
                                    STRING(li-over-tot-hr,">99") + ":" + string(li-over-tot-min,"99")  "," 
                                    STRING(li-2time-tot-hr,">99") + ":" + string(li-2time-tot-min,"99")
                                    SKIP.
                        END.  
                        ASSIGN 
                            li-lunch-emp = 0
                            li-emp-over  = 0
                            li-emp-2time = 0.
                    /*if lv-note-only then do:
                       put "Notes:" skip .
                       for each tt-note where tt-note.employee = emplogin.employee
                                           by tt-note.note_date:
                           put "     " tt-note.note_date " " tt-note.note_title skip.
                       end.                       
                       put skip(1).
                    end.
                    */
                    END.           
                END.
            ELSE IF scr-plant-clock = 2 THEN RUN proc-2(INPUT tt-emp.emp).
                ELSE RUN proc-all (tt-emp.emp).

        END.  /*lv-labor-only*/

        IF LAST-OF(tt-emp.emp) THEN 
        DO:
            IF lv-note-only THEN 
            DO:
                PUT "Notes:" SKIP .
                FOR EACH tt-note WHERE tt-note.employee = tt-emp.emp
                    BY tt-note.note_date:
                    PUT "     " tt-note.note_date " " tt-note.note_title SKIP.
                END.                       
                PUT SKIP(1).
            END.
        END.
    END. /*end tt-emp*/
    STATUS DEFAULT "".

    OUTPUT CLOSE.
    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
    END.

    RUN custom/usrprint.p (v-prgmname,FRAME {&FRAME-NAME}:HANDLE).
    SESSION:SET-WAIT-STATE('').

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
        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"save") > 0
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param1 C-Win 
PROCEDURE show-param1 :
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

    MESSAGE 
        NUM-ENTRIES(parm-fld-list,",") SKIP
        parm-fld-list
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

    DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
        IF ENTRY(i,parm-fld-list) NE "" OR
            entry(i,parm-lbl-list) NE "" THEN 
        DO:

            MESSAGE ENTRY(i,parm-lbl-list)
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

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
            fi_file:SCREEN-VALUE = "c:\tmp\LaborReport.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

