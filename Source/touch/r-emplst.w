&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Yoosun Kim

  Created: 01/12/2001

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
DEFINE STREAM excel.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

/*{sys/inc/var.i new shared} */
DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE locode AS CHARACTER NO-UNDO.

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE VARIABLE v-invalid  AS LOG NO-UNDO.
DEFINE VARIABLE v-download AS LOG INIT NO NO-UNDO.
DEFINE VARIABLE v-prior    AS LOG INIT NO NO-UNDO.

DEFINE BUFFER tmp-per FOR period.

DEFINE STREAM st-mach.
DEFINE STREAM st-emp.


DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName      AS CHARACTER NO-UNDO.

&Scoped-define SCOPDEFS post
&SCOPED-DEFINE where-statement machtran.company = g_company


DEFINE TEMP-TABLE ttbl_pc-prdd NO-UNDO LIKE pc-prdd
    INDEX ttbl_pc-prdd IS PRIMARY
    company m-code op-date shift job frm blank-no.
DEFINE TEMP-TABLE ttbl_pc-prdh NO-UNDO LIKE pc-prdh
    INDEX ttbl_pc-prdh IS PRIMARY
    company m-code trans-date shift.
DEFINE TEMP-TABLE ttbl_rowid NO-UNDO
    FIELD pc-prdd_rowid AS ROWID
    FIELD total_time    AS INTEGER.
DEFINE VARIABLE machtotaltime    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE shiftpct         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE i                AS INTEGER   NO-UNDO.
DEFINE VARIABLE waste-qty        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE run-qty          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE selected-company AS CHARACTER FORMAT "X(3)" LABEL "Company" NO-UNDO.

DEFINE VARIABLE show-notes       AS LOGICAL   LABEL "Show Notes" NO-UNDO.
DEFINE VARIABLE notes-type       AS INTEGER   NO-UNDO.
DEFINE VARIABLE show-misc-fields AS LOGICAL   LABEL "Show Misc Fields" NO-UNDO.
DEFINE VARIABLE misc-label       AS CHARACTER FORMAT "X(70)" NO-UNDO.
DEFINE VARIABLE cnt              AS INTEGER   NO-UNDO.
DEFINE VARIABLE show-addresses   AS LOGICAL   LABEL "Show Addresses" NO-UNDO.
DEFINE VARIABLE citystate        AS LOGICAL   LABEL "Show City State" NO-UNDO.
DEFINE VARIABLE show-phones      AS LOGICAL   LABEL "Show Phones" NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-6 begin_employee end_employee ~
begin_employee_last_name end_employee_last_name begin_employee_soc_sec ~
end_employee_soc_sec show-rates show-login-logout show-machines ~
show-emp-notes rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_employee end_employee ~
begin_employee_last_name end_employee_last_name begin_employee_soc_sec ~
end_employee_soc_sec show-rates show-login-logout show-machines ~
show-emp-notes rd-dest fi_file tb_OpenCSV tbAutoClose 

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

DEFINE VARIABLE begin_employee           AS CHARACTER FORMAT "X(5)" 
    LABEL "Beginning Employee" 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1.

DEFINE VARIABLE begin_employee_last_name AS CHARACTER FORMAT "X(30)" 
    LABEL "Beginning Last Name" 
    VIEW-AS FILL-IN 
    SIZE 30 BY 1.

DEFINE VARIABLE begin_employee_soc_sec   AS CHARACTER FORMAT "999-99-9999":U 
    LABEL "Beginning SSN" 
    VIEW-AS FILL-IN 
    SIZE 26 BY 1.

DEFINE VARIABLE end_employee             AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
    LABEL "Ending Employee" 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1.

DEFINE VARIABLE end_employee_last_name   AS CHARACTER FORMAT "X(30)" INITIAL "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" 
    LABEL "Ending Last Name" 
    VIEW-AS FILL-IN 
    SIZE 30 BY 1.

DEFINE VARIABLE end_employee_soc_sec     AS CHARACTER FORMAT "999-99-9999":U INITIAL "999999999" 
    LABEL "Ending SSN" 
    VIEW-AS FILL-IN 
    SIZE 23 BY 1.

DEFINE VARIABLE fi_file                  AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\EmployeesList.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lines-per-page           AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name             AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no               AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt                  AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest                  AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 18 BY 5.71 NO-UNDO.

DEFINE RECTANGLE RECT-2
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 102 BY 9.71.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 102 BY 6.52.

DEFINE VARIABLE show-emp-notes    AS LOGICAL INITIAL NO 
    LABEL "Show Employee Notes *" 
    VIEW-AS TOGGLE-BOX
    SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE show-login-logout AS LOGICAL INITIAL NO 
    LABEL "Show Login/Logout Transactions" 
    VIEW-AS TOGGLE-BOX
    SIZE 36 BY .81 NO-UNDO.

DEFINE VARIABLE show-machines     AS LOGICAL INITIAL NO 
    LABEL "Show Assigned Machines *" 
    VIEW-AS TOGGLE-BOX
    SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE show-rates        AS LOGICAL INITIAL NO 
    LABEL "Show Employee Rates *" 
    VIEW-AS TOGGLE-BOX
    SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE tbAutoClose       AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel          AS LOGICAL INITIAL NO 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV        AS LOGICAL INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm      AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_employee AT ROW 2.67 COL 21.8 COLON-ALIGNED HELP
    "Enter Beginning Employee"
    end_employee AT ROW 2.67 COL 71.2 COLON-ALIGNED HELP
    "Enter Ending Employee"
    begin_employee_last_name AT ROW 3.86 COL 21.8 COLON-ALIGNED HELP
    "Enter Beginning Last Name"
    end_employee_last_name AT ROW 3.86 COL 71.2 COLON-ALIGNED HELP
    "Enter Ending Last Name"
    begin_employee_soc_sec AT ROW 5.05 COL 21.8 COLON-ALIGNED HELP
    "Enter Beginning Social Security #"
    end_employee_soc_sec AT ROW 5.05 COL 71.2 COLON-ALIGNED HELP
    "Enter Ending Social Security #"
    show-rates AT ROW 6.67 COL 40.6 HELP
    "Select to Show Employee Rate"
    show-login-logout AT ROW 7.62 COL 40.6 HELP
    "Select to Show Employee Rate"
    show-machines AT ROW 8.57 COL 40.6 HELP
    "Select to Show Assigned Machines"
    show-emp-notes AT ROW 9.52 COL 40.6 HELP
    "Select to Show Employee Rate"
    lv-ornt AT ROW 11.95 COL 31 NO-LABELS
    lines-per-page AT ROW 11.95 COL 82.8 COLON-ALIGNED
    rd-dest AT ROW 12.33 COL 5 NO-LABELS
    lv-font-no AT ROW 12.91 COL 36 COLON-ALIGNED
    tb_excel AT ROW 12.91 COL 51
    lv-font-name AT ROW 13.86 COL 30 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 15.29 COL 31
    fi_file AT ROW 16.76 COL 29 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 16.86 COL 95.6 RIGHT-ALIGNED
    tbAutoClose AT ROW 18.76 COL 31.2 WIDGET-ID 78
    btn-ok AT ROW 20.05 COL 31
    btn-cancel AT ROW 20.05 COL 51
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 3
    "* (not exported to Excel)" VIEW-AS TEXT
    SIZE 27 BY .62 AT ROW 10.24 COL 44.6
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 11.52 COL 3
    RECT-2 AT ROW 1.52 COL 2
    RECT-6 AT ROW 11.86 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 104.8 BY 22
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
        TITLE              = "Employees List"
        HEIGHT             = 22
        WIDTH              = 104.8
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
    begin_employee:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_employee_last_name:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_employee_soc_sec:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_employee:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_employee_last_name:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_employee_soc_sec:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

ASSIGN 
    show-emp-notes:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    show-login-logout:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    show-machines:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    show-rates:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
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
ON END-ERROR OF C-Win /* Employees List */
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
ON WINDOW-CLOSE OF C-Win /* Employees List */
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
                    {custom/asifax.i &type="Employee List"
                            &begin_cust=begin_employee
                            &END_cust= end_employee
                            &fax-subject="Employee List"
                            &fax-body="Employee List"
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE="Employee List"
                             &begin_cust=begin_employee
                             &END_cust=end_employee
                             &mail-subject="Employee List"
                             &mail-body="Employee List"
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE="Employee List"
                                  &begin_cust=begin_employee
                                  &END_cust=end_employee
                                  &mail-subject="Employee List"
                                  &mail-body="Employee List"
                                  &mail-file=list-name }
                    END.
                END.
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
/*{sys/inc/f3helpw.i} */
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
    {sys/inc/reportsConfigNK1.i "TR2" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    {methods/nowait.i}
    {custom/usrprint.i}
APPLY "entry" TO begin_employee IN FRAME {&FRAME-NAME}.
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
    DISPLAY begin_employee end_employee begin_employee_last_name 
        end_employee_last_name begin_employee_soc_sec end_employee_soc_sec 
        show-rates show-login-logout show-machines show-emp-notes rd-dest 
        fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-2 RECT-6 begin_employee end_employee begin_employee_last_name 
        end_employee_last_name begin_employee_soc_sec end_employee_soc_sec 
        show-rates show-login-logout show-machines show-emp-notes rd-dest 
        fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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

    FIND FIRST employee WHERE employee.company EQ cocode 
        USE-INDEX pi-employee NO-LOCK NO-ERROR.

    IF AVAILABLE employee THEN
        ASSIGN
            begin_employee           = employee.employee
            begin_employee_last_name = employee.last_name
            begin_employee_soc_sec   = employee.soc_sec.

    FIND LAST employee WHERE employee.company EQ cocode 
        USE-INDEX pi-employee NO-LOCK NO-ERROR.

    IF AVAILABLE employee THEN
        ASSIGN
            end_employee           = employee.employee
            end_employee_last_name = employee.last_name
            end_employee_soc_sec   = employee.soc_sec.

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
/*   DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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
    DEFINE VARIABLE printok   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
    DEFINE VARIABLE result    AS LOGICAL   NO-UNDO.

    /*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
         IF NOT printok THEN
         RETURN NO-APPLY.
    */
    /*
      /* Use Progress Print. Always use Font#9 in Registry (set above) */
         RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                                INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                        /* use-dialog(1) and landscape(2) */
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
    RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 


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
            fi_file:SCREEN-VALUE = "c:\tmp\EmployeesList.csv".    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /*==== Report main body procedure ================================
    ==================================================================*/
    DEFINE VARIABLE ld-total-time AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ld-emp-time   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ld-mr-time    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ld-run-time   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li-job-cnt    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li-mr-cnt     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ld-mr-avg     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ld-run-avg    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ld-tot-avg    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-tmp-mr     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-tmp-run    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-tmp-tot    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ls-tot-mr     AS cha       NO-UNDO.
    DEFINE VARIABLE ls-tot-run    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ls-tot-tot    AS cha       NO-UNDO.
    DEFINE VARIABLE ld-waste%     AS DECIMAL   FORM ">,>>9.99" NO-UNDO.
    DEFINE VARIABLE lv-rqty       LIKE machtran.run_qty NO-UNDO.
    DEFINE VARIABLE lv-wqty       LIKE machtran.waste_qty NO-UNDO.
    DEFINE VARIABLE excelheader   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE firstlogin    AS LOGICAL   NO-UNDO.

    SESSION:SET-WAIT-STATE("general").

    {sys/form/r-top3w.f}

    FORM HEADER SKIP(1) WITH FRAME r-top.


    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        str-tit3 = "Employee Time by Job and Machine"
        {sys/inc/ctrtext.i str-tit3 132}.

    {sys/inc/print1.i}
    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    VIEW FRAME r-top.
    selected-company = g_company.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        ASSIGN 
            excelheader = "Emp ID,Employee Name,Soc Sec #,Type,GL Acct,Hire Date,Reference Number,Rate Use,Start Date,Login,Machine,End Date,Logout,Shift,Total".
        PUT STREAM excel UNFORMATTED excelheader SKIP.
    END.

    /*{methods/lstlogic/custom/employe_.i}*/
    FOR EACH employee NO-LOCK WHERE employee.company = gcompany
        AND employee.employee >= begin_employee
        AND employee.employee <= end_employee
                            /*AND employee.LAST_name >= begin_employee_last_name
                            AND employee.LAST_name <= end_employee_last_name
                            AND employee.LAST_name >= begin_employee_soc_sec
                            AND employee.LAST_name >= END_employee_soc_sec*/
        BY employee.employee BY employee.LAST_name BY employee.START_date:


        DISPLAY
            employee.employee
            employee.last_name LABEL 'Employee Name' FORMAT 'X(25)'
            employee.last_name + ', ' + employee.first_name + ' ' + employee.middle_name @
            employee.last_name
            employee.soc_sec
            employee.emp_type LABEL 'Type'
            employee.actnum
            employee.start_date COLUMN-LABEL "Hire Date"
            employee.ref_no
            employee.rate_usage WITH FRAME emp WIDTH 132 STREAM-IO DOWN.
        IF tb_excel THEN PUT STREAM excel UNFORMATTED
                employee.employee ","
                employee.last_name ' - ' employee.first_name ' ' employee.middle_name  ","
                STRING(employee.soc_sec,"xxx-xx-xxxx") ' ,' /* this ' causes numerics to show as text */
                employee.emp_type  ","
                employee.actnum ","
                (IF employee.start_date <> ? THEN STRING(employee.start_date) ELSE "") ","
                employee.ref_no ","
                STRING(employee.rate_usage,"Shift/Machine") ",".
        ASSIGN 
            firstlogin = YES.
        IF show-rates THEN
            FOR EACH rate OF employee NO-LOCK WITH STREAM-IO TITLE '---- Rates ----' COL 5:
                DISPLAY
                    rate.ratetype
                    rate.shift
                    rate.machine
                    rate.rate_usage
                    rate.rate
                    rate.factortype WITH .
            END.

        IF show-login-logout THEN
            FOR EACH emplogin OF employee NO-LOCK WITH STREAM-IO TITLE '---- Login/Logout ----' COL 5:
                DISPLAY
                    emplogin.start_date
                    STRING(emplogin.start_time,'HH:MM am') LABEL 'Login'
                    emplogin.machine
                    emplogin.end_date
                    emplogin.end_time LABEL 'Logout'
                    STRING(emplogin.end_time,'HH:MM am') 
                    WHEN emplogin.end_time NE 0 @ emplogin.end_time
                    '' 
                    WHEN emplogin.end_time = 0 @ emplogin.end_time
                    emplogin.shift
                    STRING(emplogin.total_time,'HH:MM') LABEL 'Total'.
                IF tb_excel THEN PUT STREAM excel UNFORMATTED
                        (IF NOT firstlogin THEN ",,,,,,,," ELSE "")
                        (IF emplogin.start_date = ? THEN "" ELSE STRING(emplogin.start_date)) ","
                        TRIM(STRING(emplogin.start_time,'HH:MM am'))","
                        emplogin.machine ","
                        (IF emplogin.end_date = ? THEN "" ELSE STRING(emplogin.end_date)) ","
                        (IF emplogin.end_time NE 0 THEN TRIM(STRING(emplogin.end_time,'HH:MM am')) ELSE "") ","
                        emplogin.shift ","
                        TRIM(STRING(emplogin.total_time,'HH:MM')) SKIP.
                ASSIGN 
                    firstlogin = NO.
            END.

        IF tb_excel AND firstlogin THEN PUT STREAM excel UNFORMATTED SKIP. 

        IF show-machines THEN 
            FOR EACH empmach OF employee NO-LOCK WITH STREAM-IO TITLE '---- Assigned Machines ----' COL 5:
                FIND mach WHERE mach.company = empmach.company
                    AND mach.m-code = empmach.machine
                    NO-LOCK NO-ERROR.
                DISPLAY
                    empmach.machine
                    mach.m-dscr 
                    WHEN AVAILABLE(mach)
                    empmach.gl_account.
            END.

        IF show-emp-notes THEN 
        DO :    
            FOR EACH notes WHERE notes.rec_key = employee.rec_key  NO-LOCK WITH DOWN STREAM-IO TITLE '---- Notes ----' COL 5
                BY notes.note_date:
                DISPLAY "Employee   " LABEL "Note Type"
                    notes.note_date 
                    notes.note_title WITH WIDTH 132.
            END.
            FOR EACH emplogin WHERE emplogin.company EQ g_company AND
                emplogin.employee = employee.employee NO-LOCK:
                FOR EACH notes WHERE notes.rec_key = emplogin.rec_key /*and
                            note.note_date = machemp.start_date */
                    NO-LOCK           BY notes.note_date :
                    DISPLAY   "Login      " AT 5
                        notes.note_date
                        notes.note_title WITH STREAM-IO WIDTH 132 NO-BOX NO-LABELS.
                END.
            END.    
            FOR EACH machemp WHERE machemp.employee = employee.employee NO-LOCK:
                FOR EACH notes WHERE notes.rec_key = machemp.rec_key /*and
                          note.note_date = machemp.start_date */
                    NO-LOCK           BY notes.note_date :
                    DISPLAY  "Transaction" AT 5 
                        notes.note_date
                        notes.note_title WITH STREAM-IO WIDTH 132 NO-BOX NO-LABELS .
                END.
            END.    

        END.  /* show-emp-notes */
        PUT SKIP(1).

        {methods/lstlogic/shownote.i &db_table="employee" &col="5" &frame-name="f-notes"}
        {methods/lstlogic/showmisc.i &db_table="employee" &col="5" &frame-name="f-miscflds"}
        {methods/lstlogic/showaddr.i &db_table="employee" &col="5" &frame-name="f-addresses"}
        {methods/lstlogic/showphon.i &db_table="employee" &col="5" &frame-name="f-phones"}

    END. /* for each employee*/

    OUTPUT close.

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

