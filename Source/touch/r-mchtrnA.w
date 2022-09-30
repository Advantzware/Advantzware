&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-mchtrnA.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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

DEFINE STREAM s-temp.

DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER     NO-UNDO.
DEFINE VARIABLE cFileName      AS CHARACTER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_machine end_machine begin_start-date ~
end_start-date rd_sort show-employees rd-dest tb_OpenCSV fi_file btn-ok ~
btn-cancel begin_shift end_shift RECT-6 RECT-7 tbAutoClose 
&Scoped-Define DISPLAYED-OBJECTS selected-company begin_machine end_machine ~
begin_start-date end_start-date rd_sort show-employees rd-dest tb_OpenCSV ~
fi_file begin_shift end_shift tbAutoClose 

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

DEFINE VARIABLE begin_machine    AS CHARACTER FORMAT "X(10)" 
    LABEL "Beginning Machine" 
    VIEW-AS FILL-IN 
    SIZE 15.6 BY 1.

DEFINE VARIABLE begin_shift      AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Shift" 
    VIEW-AS FILL-IN 
    SIZE 15.6 BY 1.

DEFINE VARIABLE begin_start-date AS DATE      FORMAT "99/99/9999" INITIAL 01/01/01 
    LABEL "Beginning Start Date" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1.

DEFINE VARIABLE end_machine      AS CHARACTER FORMAT "X(10)" INITIAL "zzzzzzz" 
    LABEL "Ending Machine" 
    VIEW-AS FILL-IN 
    SIZE 15.6 BY 1.

DEFINE VARIABLE end_shift        AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Shift" 
    VIEW-AS FILL-IN 
    SIZE 15.6 BY 1.

DEFINE VARIABLE end_start-date   AS DATE      FORMAT "99/99/9999" INITIAL 12/31/2099 
    LABEL "Ending Start Date" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1.

DEFINE VARIABLE fi_file          AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\MachineTransactionList.csv" 
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
    SIZE 14 BY 6.14 NO-UNDO.

DEFINE VARIABLE rd_sort          AS INTEGER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Start Date / Time", 1,
    "Start Date / Job#", 2,
    "Machine / Date / Time", 3
    SIZE 67 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 6.91.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 9.71.

DEFINE VARIABLE show-employees AS LOGICAL INITIAL NO 
    LABEL "Show Employee Transactions" 
    VIEW-AS TOGGLE-BOX
    SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE tbAutoClose    AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel       AS LOGICAL INITIAL NO 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV     AS LOGICAL INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm   AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    selected-company AT ROW 3.14 COL 25.4 COLON-ALIGNED
    begin_machine AT ROW 4.57 COL 25.4 COLON-ALIGNED HELP
    "Enter Beginning Machine"
    end_machine AT ROW 4.57 COL 67.4 COLON-ALIGNED HELP
    "Enter Ending Machine"
    begin_start-date AT ROW 6 COL 25.4 COLON-ALIGNED HELP
    "Enter Beginning Start Date"
    end_start-date AT ROW 6 COL 67.4 COLON-ALIGNED HELP
    "Enter Beginning Start Date"
    rd_sort AT ROW 8.52 COL 25.6 NO-LABELS
    show-employees AT ROW 10 COL 32.4 HELP
    "Select to Show Employee Transactions"
    rd-dest AT ROW 12.33 COL 6 NO-LABELS
    lv-ornt AT ROW 12.05 COL 29 NO-LABELS
    lines-per-page AT ROW 12.05 COL 82 COLON-ALIGNED
    lv-font-no AT ROW 13 COL 33 COLON-ALIGNED
    td-show-parm AT ROW 16.24 COL 28.2
    lv-font-name AT ROW 13.95 COL 29 COLON-ALIGNED NO-LABELS
    tb_excel AT ROW 13.14 COL 46
    tb_OpenCSV AT ROW 17.29 COL 92.2 RIGHT-ALIGNED
    fi_file AT ROW 17.19 COL 26.2 COLON-ALIGNED HELP
    "Enter File Name"
    btn-ok AT ROW 19.95 COL 28.2
    btn-cancel AT ROW 19.95 COL 48.2
    begin_shift AT ROW 7.29 COL 25.4 COLON-ALIGNED HELP
    "Enter Beginning Shift" WIDGET-ID 2
    end_shift AT ROW 7.29 COL 67.4 COLON-ALIGNED HELP
    "Enter Ending Shift" WIDGET-ID 4
    tbAutoClose AT ROW 19 COL 28.2 WIDGET-ID 78
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 11.62 COL 5
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    "Sort By:" VIEW-AS TEXT
    SIZE 8 BY .62 AT ROW 8.76 COL 15
    RECT-6 AT ROW 11.95 COL 4
    RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 21.1
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
        TITLE              = "Machine Transaction List"
        HEIGHT             = 21.1
        WIDTH              = 95.8
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
   FRAME-NAME Custom                                                    */
ASSIGN 
    begin_machine:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_shift:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_start-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_machine:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_shift:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_start-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

/* SETTINGS FOR FILL-IN selected-company IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    selected-company:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    show-employees:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Machine Transaction List */
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
ON WINDOW-CLOSE OF C-Win /* Machine Transaction List */
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
        RUN run-report. 
        SESSION:SET-WAIT-STATE ("general").

        SESSION:SET-WAIT-STATE ("").

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
                    {custom/asifax.i &type="Machine Transaction"
                            &begin_cust=begin_machine
                            &END_cust= begin_machine
                            &fax-subject="Machine Transaction"
                            &fax-body="Machine Transaction"
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = "Machine Transaction"
                             &begin_cust= begin_machine
                             &END_cust=begin_machine
                             &mail-subject="Machine Transaction"
                             &mail-body="Machine Transaction "
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Machine Transaction"
                             &begin_cust= begin_machine
                             &END_cust=begin_machine
                             &mail-subject=current-window:title
                             &mail-body=CURRENT-WINDOW:TITLE
                             &mail-file=list-name }

                    END.
                END.
        END CASE.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
        SESSION:SET-WAIT-STATE("").
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
    selected-company = g_company.
    RUN init-proc.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "TR3" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    {methods/nowait.i}
 
    DO WITH FRAME {&frame-name}:
        {custom/usrprint.i}
APPLY 'ENTRY' TO begin_machine.
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
    DISPLAY selected-company begin_machine end_machine begin_start-date 
        end_start-date rd_sort show-employees rd-dest tb_OpenCSV fi_file 
        begin_shift end_shift tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE begin_machine end_machine begin_start-date end_start-date rd_sort 
        show-employees rd-dest tb_OpenCSV fi_file btn-ok btn-cancel 
        begin_shift end_shift RECT-6 RECT-7 tbAutoClose 
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
 /*    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY.
*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /*==== Report main body procedure ================================
    ==================================================================*/
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTothours   AS INTEGER   NO-UNDO .
    DEFINE VARIABLE iRunQty     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iWasteQty   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTot        AS CHARACTER NO-UNDO .
    SESSION:SET-WAIT-STATE("general").

    {sys/form/r-top3w.f}

    FORM HEADER SKIP(1) WITH FRAME r-top.

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}
        str-tit3 = FILL("-",132)
        {sys/inc/ctrtext.i str-tit3 132}.


    {sys/inc/print1.i}
    {sys/inc/outprint.i value(lines-per-page)}
    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        ASSIGN 
            excelheader = "Machine,Job,Sub,Form,Blank,Pass,Charge,Start Date,Log In,End Date,Log Out,Shift,Total,Run,Waste".
        IF show-employees THEN ASSIGN
                excelheader = excelheader + ",Emp ID,Name,Start Date,Started,End Date,Ended,Total,Shift Usage,Type,Rate".
        PUT STREAM excel UNFORMATTED excelheader SKIP.
    END.

    VIEW FRAME r-top.
    IF td-show-parm THEN RUN show-param.


    IF rd_sort = 1 THEN 
    DO:

        FOR EACH machtran NO-LOCK WHERE
            machtran.company = selected-company AND
            machtran.machine GE begin_machine AND
            machtran.machine LE end_machine AND
            machtran.start_date GE begin_start-date AND
            machtran.start_date LE END_start-date AND
            machtran.shift GE begin_shift AND
            machtran.shift LE END_shift
            BREAK BY machtran.START_date
            BY machtran.START_time BY machtran.machine BY machtran.job_number
            BY machtran.END_date BY machtran.END_time.

            DISPLAY
                machtran.machine
                machtran.job_number
                machtran.job_sub
                machtran.form_number
                machtran.blank_number
                machtran.pass_sequence
                machtran.charge_code
                machtran.start_date
                STRING(machtran.start_time,'HH:MM am') LABEL 'Log In'
                machtran.end_date
                STRING(machtran.end_time,'HH:MM am') LABEL 'Log Out'
                machtran.shift
                STRING(machtran.total_time ,'HH:MM' ) LABEL 'Total'
                machtran.run_qty
                machtran.waste_qty  
                WITH FRAME mch NO-BOX WIDTH 132 STREAM-IO DOWN.
            IF tb_excel THEN PUT STREAM excel UNFORMATTED
                    machtran.machine ","
                    machtran.job_number ","
                    machtran.job_sub ","
                    machtran.form_number ","
                    machtran.blank_number ","
                    machtran.pass_sequence ","
                    machtran.charge_code ","
                    machtran.start_date ","
                    TRIM(STRING(machtran.start_time,'HH:MM am'))  ","
                    machtran.end_date ","
                    TRIM(STRING(machtran.end_time,'HH:MM am')) ","
                    machtran.shift ","
                    TRIM(STRING(machtran.total_time,'HH:MM')) ","
                    machtran.run_qty ","
                    machtran.waste_qty.
            ASSIGN
                iTothours = iTothours + (machtran.total_time )
                iRunQty   = iRunQty  + machtran.run_qty
                iWasteQty = iWasteQty + machtran.waste_qty .

            IF show-employees THEN RUN show-employees IN THIS-PROCEDURE (BUFFER machtran).
            ELSE IF tb_excel THEN PUT STREAM excel SKIP.
        END.
    END. /* sort by date/time*/
    ELSE IF rd_sort = 2 THEN 
        DO:
            FOR EACH machtran NO-LOCK WHERE machtran.company = selected-company AND
                machtran.machine GE begin_machine AND
                machtran.machine LE end_machine AND
                machtran.start_date GE begin_start-date AND
                machtran.start_date LE END_start-date AND
                machtran.shift GE begin_shift AND
                machtran.shift LE END_shift
                BREAK BY machtran.START_date
                BY machtran.job_number BY machtran.machine BY machtran.START_time
                BY machtran.END_date BY machtran.END_time.

                DISPLAY
                    machtran.machine
                    machtran.job_number
                    machtran.job_sub
                    machtran.form_number
                    machtran.blank_number
                    machtran.pass_sequence
                    machtran.charge_code
                    machtran.start_date
                    STRING(machtran.start_time,'HH:MM am') LABEL 'Log In'
                    machtran.end_date
                    STRING(machtran.end_time,'HH:MM am') LABEL 'Log Out'
                    machtran.shift
                    STRING(machtran.total_time,'HH:MM') LABEL 'Total'
                    machtran.run_qty
                    machtran.waste_qty  
                    WITH FRAME mch2 NO-BOX WIDTH 132 STREAM-IO DOWN.
                IF tb_excel THEN PUT STREAM excel UNFORMATTED
                        machtran.machine ","
                        machtran.job_number ","
                        machtran.job_sub ","
                        machtran.form_number ","
                        machtran.blank_number ","
                        machtran.pass_sequence ","
                        machtran.charge_code ","
                        machtran.start_date ","
                        TRIM(STRING(machtran.start_time,'HH:MM am'))  ","
                        machtran.end_date ","
                        TRIM(STRING(machtran.end_time,'HH:MM am')) ","
                        machtran.shift ","
                        TRIM(STRING(machtran.total_time,'HH:MM')) ","
                        machtran.run_qty ","
                        machtran.waste_qty.
         
                ASSIGN
                    iTothours = iTothours + machtran.total_time 
                    iRunQty   = iRunQty  + machtran.run_qty
                    iWasteQty = iWasteQty + machtran.waste_qty .

                IF show-employees THEN RUN show-employees IN THIS-PROCEDURE (BUFFER machtran).
                ELSE IF tb_excel THEN PUT STREAM excel SKIP.

            END.                                                                                                         

        END.
        ELSE 
            FOR EACH mach FIELDS(m-code) WHERE
                mach.company EQ selected-company AND
                mach.m-code GE begin_machine AND
                mach.m-code LE end_machine
                NO-LOCK,
                EACH machtran NO-LOCK WHERE machtran.company = selected-company AND
                machtran.machine EQ mach.m-code AND
                machtran.start_date GE begin_start-date AND
                machtran.start_date LE END_start-date AND
                machtran.shift GE begin_shift AND
                machtran.shift LE END_shift
                BREAK BY machtran.machine
                BY machtran.START_date
                BY machtran.START_time  BY machtran.job_number
                BY machtran.END_date BY machtran.END_time.

                DISPLAY
                    machtran.machine
                    machtran.job_number
                    machtran.job_sub
                    machtran.form_number
                    machtran.blank_number
                    machtran.pass_sequence
                    machtran.charge_code
                    machtran.start_date
                    STRING(machtran.start_time,'HH:MM am') LABEL 'Log In'
                    machtran.end_date
                    STRING(machtran.end_time,'HH:MM am') LABEL 'Log Out'
                    machtran.shift
                    STRING(machtran.total_time,'HH:MM') LABEL 'Total'
                    machtran.run_qty
                    machtran.waste_qty  
                    WITH FRAME mchst NO-BOX WIDTH 132 STREAM-IO DOWN.
                IF tb_excel THEN PUT STREAM excel UNFORMATTED
                        machtran.machine ","
                        machtran.job_number ","
                        machtran.job_sub ","
                        machtran.form_number ","
                        machtran.blank_number ","
                        machtran.pass_sequence ","
                        machtran.charge_code ","
                        machtran.start_date ","
                        TRIM(STRING(machtran.start_time,'HH:MM am')) ","
                        machtran.end_date ","
                        TRIM(STRING(machtran.end_time,'HH:MM am')) ","
                        machtran.shift ","
                        TRIM(STRING(machtran.total_time,'HH:MM')) ","
                        machtran.run_qty ","
                        machtran.waste_qty.

                ASSIGN
                    iTothours = iTothours + machtran.total_time 
                    iRunQty   = iRunQty  + machtran.run_qty
                    iWasteQty = iWasteQty + machtran.waste_qty .

                IF show-employees THEN RUN show-employees IN THIS-PROCEDURE (BUFFER machtran).
                ELSE IF tb_excel THEN PUT STREAM excel SKIP.
            END. /* sort by machine/date/time*/

    ASSIGN 
        cTot = STRING(TRUNCATE(iTothours / 3600,0),">>>99") + ":" + 
                              STRING(((iTothours MOD 3600) / 60),"99") .

    PUT  "-------- ----------- ------ " AT 89
        " Totals:   " AT 78
        TRIM(cTot)  FORMAT "x(8)" SPACE(1)
        iRunQty FORMAT ">>>>>>>>>>9" SPACE(1)
        iWasteQty FORMAT ">>>>>9"
        SKIP   .
    .


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-machtran C-Win 
PROCEDURE run-report-machtran :
/*=====
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*==== Report main body procedure ================================
==================================================================*/
DEF VAR ld-total-time AS INT NO-UNDO.
DEF VAR ld-emp-time AS INT NO-UNDO.
DEF VAR ld-mr-time AS INT NO-UNDO.
DEF VAR ld-run-time AS INT NO-UNDO.
DEF VAR li-job-cnt AS INT NO-UNDO.
DEF VAR ld-mr-avg AS int NO-UNDO.
DEF VAR ld-run-avg AS int NO-UNDO.
DEF VAR ld-tot-avg AS int NO-UNDO.
def var lv-tmp-mr as int no-undo.
def var lv-tmp-run as int no-undo.
def var lv-tmp-tot as int no-undo.
def var ls-tot-mr as cha  no-undo.
def var ls-tot-run AS CHARACTER NO-UNDO.
def var ls-tot-tot as cha  no-undo.
def var ld-waste% as dec form ">,>>9.99"       no-undo.
def var lv-rqty like machtran.run_qty no-undo.
def var lv-wqty like machtran.waste_qty no-undo.

SESSION:SET-WAIT-STATE("general").

{sys/form/r-top3w.f}

FORM HEADER SKIP(1) WITH FRAME r-top.


assign
   str-tit2 = c-win:TITLE
   {sys/inc/ctrtext.i str-tit2 112}

   str-tit3 = "Employee Time by Job and Machine"
   {sys/inc/ctrtext.i str-tit3 132}.



{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)}
/*
FORM HEADER
     "***** Employee Time by Job and Machine  *****" SKIP
     "AS of " AT 30 TODAY  "Page: " + STRING(PAGE-NUM,">>9") FORM "x(12)" TO 80 SKIP
    FILL("=",80) FORM "x(80)" 
    WITH FRAME hd PAGE-TOP NO-BOX NO-LABEL STREAM-IO.
*/


if td-show-parm then run show-param.



VIEW FRAME r-top.


FOR EACH machemp NO-LOCK WHERE machemp.employee >= begin_emp-no
                             AND machemp.employee <= end_emp-no
                           /*  AND machemp.start_date >= begin_date
                             AND machemp.end_date <= END_date */ ,      
      EACH machtran NO-LOCK WHERE machtran.rec_key = machemp.TABLE_rec_key 
                              AND machtran.end_date >= begin_date
                              AND machtran.end_date <= END_date 
                              and can-find(first emplogin where emplogin.company = machtran.company
                       and emplogin.employee = machemp.employee
                       and emplogin.start_date = machtran.start_date
                       and emplogin.end_date = machtran.end_date 
                       and emplogin.start_time <= machemp.start_time
                       and emplogin.end_time >= machemp.end_time) ,
      FIRST mach NO-LOCK WHERE mach.m-code = machtran.machine AND
                              (mach.industry = rd_industry OR rd_industry = "") 
            BREAK BY machemp.employee BY machtran.machine BY machtran.job_number BY machtran.job_sub by machtran.end_date
                  by machtran.end_time by machtran.charge_code:

      FIND employee WHERE employee.company = machtran.company
                      AND employee.employee = machemp.employee NO-LOCK NO-ERROR.

      /*Employee#, MachineCode, Job#, Start Date, Start Time, Stop Time,
       ChargeCode, MR Time, Run Time, Total Time, Run Qty, Waste Qty. */
      ASSIGN ld-mr-time = 0
             ld-run-time = 0.
      IF FIRST-OF(machtran.machine) THEN ASSIGN ld-total-time = 0
                                                li-job-cnt = 0.
      if first-of(machemp.employee) then assign ld-emp-time = 0.

      ld-total-time = ld-total-time + machtran.TOTAL_time.
      ld-emp-time = ld-emp-time + machtran.TOTAL_time.

      IF machtran.charge_code = "MR" THEN ld-mr-time = machtran.TOTAL_time.
      ELSE IF machtran.charge_code = "RUN" THEN ld-run-time = machtran.TOTAL_time.


      ACCUMULATE ld-mr-time (TOTAL BY machemp.employee BY machtran.machine).
      ACCUMULATE ld-run-time (TOTAL BY machemp.employee BY machtran.machine).

      assign lv-rqty = if last-of(machtran.charge_code) then machtran.run_qty else 0
             lv-wqty = if last-of(machtran.charge_code) then machtran.waste_qty else 0
             .

      ACCUMULATE lv-rqty (TOTAL BY machemp.employee BY machtran.machine).
      ACCUMULATE lv-wqty (TOTAL BY machemp.employee BY machtran.machine).

      DISPLAY machemp.employee
              machtran.machine
              machtran.job_number + "-" + string(machtran.job_sub,"99") FORM "x(10)"  @ machtran.job_number LABEL "Job#"
              /* machtran.form_number
              machtran.blank_number
              machtran.pass_sequence*/
              machtran.start_date
              STRING(machtran.start_time,'HH:MM am') LABEL 'Start Time'
              machtran.end_date
              STRING(machtran.end_time,'HH:MM am') LABEL 'End Time'
              /*machtran.shift */
              machtran.charge_code

              STRING(ld-mr-time,"HH:MM") WHEN ld-mr-time > 0   @ ld-mr-time LABEL "MR Time"
              STRING(ld-run-time,"HH:MM") WHEN ld-run-time > 0 @ ld-run-time LABEL "Run Time"
              STRING(machtran.total_time,'HH:MM')  @ ld-total-time LABEL 'Total Time' 
              lv-rqty
              lv-wqty
              WITH FRAME det DOWN NO-BOX STREAM-IO WIDTH 133.

      /* employee.first_name + ' ' + employee.last_name FORMAT 'X(30)' LABEL 'Name' 
      machemp.start_date
      STRING(machtran.start_time,'HH:MM am') LABEL 'Started'
      machemp.end_date
      STRING(machtran.end_time,'HH:MM am') LABEL 'Ended'
      STRING(machtran.total_time,'HH:MM') LABEL 'Total'
      machemp.shift
      machemp.rate_usage LABEL 'Usage'
      machemp.ratetype
      machemp.rate.
      */
        IF last-of(machtran.job_number) THEN li-job-cnt = li-job-cnt + 1.
        IF LAST-OF(machtran.machine) THEN DO:
           UNDERLINE machtran.machine machtran.job_number
                     ld-mr-time ld-run-time ld-total-time 
                     WITH FRAME det.
           DOWN WITH FRAME det.
           ASSIGN ld-mr-avg = (ACCUM TOTAL BY machtran.machine ld-mr-time) / li-job-cnt
                  ld-run-avg = (ACCUM TOTAL BY machtran.machine ld-run-time) / li-job-cnt
                  ld-tot-avg = ld-total-time / li-job-cnt
                  lv-tmp-mr = (ACCUM TOTAL BY machtran.machine ld-mr-time)
                  lv-tmp-run = (ACCUM TOTAL BY machtran.machine ld-run-time)
                  lv-tmp-tot = ld-total-time.
           run touch/calctime.p (lv-tmp-mr, output ls-tot-mr).
           run touch/calctime.p (lv-tmp-run, output ls-tot-run).
           run touch/calctime.p (lv-tmp-tot, output ls-tot-tot).

           disp "Machine" @ machtran.machine
                "Total Time:" @ machtran.job_number
                ls-tot-mr @ ld-mr-time 
                ls-tot-run @ ld-run-time 
                ls-tot-tot @ ld-total-time 
                (accum total by machtran.machine lv-rqty) @ lv-rqty
                (accum total by machtran.machine lv-wqty) @ lv-wqty
                with frame det.
           down with frame det.                     
           run touch/calctime.p (ld-mr-avg, output ls-tot-mr).
           run touch/calctime.p (ld-run-avg, output ls-tot-run).
           run touch/calctime.p (ld-tot-avg, output ls-tot-tot).
           if ( (accum total by machtran.machine lv-rqty) +
                (accum total by machtran.machine lv-wqty) ) <> 0 
           then                         
           ld-waste% = (accum total by machtran.machine lv-wqty) /
                       ( (accum total by machtran.machine lv-rqty) +
                         (accum total by machtran.machine lv-wqty) ) * 100.
           else ld-waste% = 0.

           disp "Average Time:" @ machtran.job_number
                ls-tot-mr @ ld-mr-time 
                ls-tot-run @ ld-run-time 
                ls-tot-tot @ ld-total-time 
                ld-waste% form ">,>>9.99%" @  lv-wqty 
               WITH FRAME det.
           down with frame det.
           lv-tmp-run = lv-tmp-run / 3600.
           lv-tmp-tot = lv-tmp-tot / 3600.
           put space(16) "Average per Hr Run Time" 
               (accum total by machtran.machine lv-rqty) / lv-tmp-run form ">>>,>>9" at 117
               skip.
           put space(16) "Average per Hr Total Time" 
               (accum total by machtran.machine lv-rqty) / lv-tmp-tot form ">>>,>>9" at 117
               skip.

          IF NOT LAST-OF(machemp.employee) THEN DOWN 2 WITH FRAME det.
          ELSE DOWN WITH FRAME det.

        END.
         IF LAST-OF(machemp.employee) THEN DO:
           UNDERLINE machtran.machine machtran.job_number
                     ld-mr-time ld-run-time ld-total-time 
                     WITH FRAME det.
           DOWN WITH FRAME det.
           ASSIGN ld-mr-avg = (ACCUM TOTAL by machemp.employee ld-mr-time) / li-job-cnt
                  ld-run-avg = (ACCUM TOTAL by machemp.employee ld-run-time) / li-job-cnt
                  ld-tot-avg = ld-emp-time / li-job-cnt
                  lv-tmp-mr = (ACCUM TOTAL BY machemp.employee ld-mr-time)
                  lv-tmp-run = (ACCUM TOTAL BY machemp.employee ld-run-time)
                  lv-tmp-tot = ld-emp-time
                  .
           run touch/calctime.p (lv-tmp-mr, output ls-tot-mr).
           run touch/calctime.p (lv-tmp-run, output ls-tot-run).
           run touch/calctime.p (lv-tmp-tot, output ls-tot-tot).

           disp "Employee" @ machtran.machine
                "Total Time:" @ machtran.job_number
                ls-tot-mr @ ld-mr-time
                ls-tot-run @ ld-run-time
                ls-tot-tot @ ld-total-time
                (accum total by machemp.employee lv-rqty) @ lv-rqty
                (accum total by machemp.employee lv-wqty) @ lv-wqty
                with frame det.
           down with frame det.                     
           run touch/calctime.p (ld-mr-avg, output ls-tot-mr).
           run touch/calctime.p (ld-run-avg, output ls-tot-run). 
           run touch/calctime.p (ld-tot-avg, output ls-tot-tot).                 
           if ( (accum total by machemp.employee lv-rqty) +
                (accum total by machemp.employee lv-wqty) ) <> 0
           then  
           ld-waste% = (accum total by machemp.employee lv-wqty) /
                       ( (accum total by machemp.employee lv-rqty) +
                         (accum total by machemp.employee lv-wqty) ) * 100.
           else ld-waste% = 0.
           disp "Average Time:" @ machtran.job_number   
                ls-tot-mr   WHEN ld-mr-avg > 0 @ ld-mr-time
                ls-tot-run  when ld-run-avg > 0 @ ld-run-time
                ls-tot-tot @ ld-total-time               
                ld-waste% form ">,>>9.99%" @ lv-wqty
               WITH FRAME det.
           down with frame det.    
           lv-tmp-run = lv-tmp-run / 3600.
           lv-tmp-tot = lv-tmp-tot / 3600.
           put space(16) "Average per Hr Run Time" 
               (accum total by machemp.employee lv-rqty) / lv-tmp-run form ">>>,>>9" at 117
               skip.
           put space(16) "Average per Hr Total Time" 
               (accum total by machemp.employee lv-rqty) / lv-tmp-tot form ">>>,>>9" at  117
               skip.

           DOWN 2 WITH FRAME det.

        END.
END.













SESSION:SET-WAIT-STATE("").

*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-employees C-Win 
PROCEDURE show-employees :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER machtran FOR machtran.
    DEFINE VARIABLE v-emp-displayed AS LOGICAL NO-UNDO.
    v-emp-displayed = NO.
    FOR EACH machemp NO-LOCK WHERE machemp.table_rec_key = machtran.rec_key
        WITH FRAME machemp2 STREAM-IO NO-BOX COLUMN 9 WIDTH 132:
        FIND employee WHERE employee.company = machtran.company
            AND employee.employee = machemp.employee NO-LOCK NO-ERROR.
        DISPLAY machemp.employee
            employee.first_name + ' ' + employee.last_name FORMAT 'X(30)' LABEL 'Name'
            machemp.start_date
            STRING(machemp.start_time,'HH:MM am') LABEL 'Started'
            machemp.end_date
            STRING(machemp.end_time,'HH:MM am') LABEL 'Ended'
            STRING(machemp.total_time,'HH:MM') LABEL 'Total'
            machemp.shift
            machemp.rate_usage LABEL 'Usage'
            machemp.ratetype
            machemp.rate.
        IF tb_excel THEN PUT STREAM excel UNFORMATTED
                (IF v-emp-displayed THEN FILL(",",14) ELSE ",") /* get across from parent line */
                machemp.employee ","
                employee.first_name + ' ' + employee.last_name  ","
                machemp.start_date ","
                TRIM(STRING(machemp.start_time,'HH:MM am')) ","
                machemp.end_date ","
                TRIM(STRING(machemp.end_time,'HH:MM am')) ","
                TRIM(STRING(machemp.total_time,'HH:MM')) ","
                machemp.shift ","
                machemp.rate_usage ","
                machemp.ratetype ","
                machemp.rate SKIP.             
        v-emp-displayed = YES.
    END.                      
    IF v-emp-displayed THEN PUT SKIP(1).
    ELSE IF tb_excel THEN PUT STREAM excel SKIP. /* if we're here then the parent display did not skip */

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
            fi_file:SCREEN-VALUE = "c:\tmp\MachineTransactionList.csv".    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
