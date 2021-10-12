&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: pcrep\r-prodlyN.w

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

{sys/inc/custlistform.i ""DE2"" }

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive   AS LOGICAL          NO-UNDO.

/* Variables for excel Automation  */
DEFINE VARIABLE chExcelApplication AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkBook         AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet        AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chFile             AS CHARACTER        NO-UNDO.
DEFINE VARIABLE CurrDir            AS CHARACTER        NO-UNDO.

DEFINE VARIABLE v-this-month       AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-days-this-month  AS INTEGER          NO-UNDO.

&GLOBAL-DEFINE summary-sheet 1
&GLOBAL-DEFINE sales-summary-sheet 2
&GLOBAL-DEFINE raw-sales-sheet 3

DEFINE VARIABLE v-cell             AS CHARACTER        NO-UNDO.
DEFINE VARIABLE inRowCount         AS INTEGER          NO-UNDO INITIAL 3.

DEFINE TEMP-TABLE tt-srt NO-UNDO LIKE mch-srt
    FIELD act-m-code    LIKE mach.m-code
    FIELD tot-run-hours AS DECIMAL
    FIELD tot-mr-hours  AS DECIMAL
    FIELD qty-Ton       AS DECIMAL   FORMAT ">>,>>9.99"
    FIELD qty-msf       AS DECIMAL   FORM ">>,>>9.99"
    FIELD start-time    AS INTEGER 
    FIELD start-date    AS DATE 
    FIELD i-no          LIKE mch-srt.job-no
    FIELD dDate         AS CHARACTER

    INDEX dept-idx dept   m-code  job-no job-no2 frm blank-no
    INDEX job-idx  job-no job-no2.

DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS RECT-7 begin_dept end_dept begin_mach ~
end_mach begin_date end_date begin_Shift end_shift tb_cust-list btnCustList ~
begin_cust-no end_cust-no tb_sched TB_round tb_tot-job rd_alptime ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_dept end_dept begin_mach end_mach ~
begin_date end_date begin_Shift end_shift tb_cust-list begin_cust-no ~
end_cust-no tb_sched TB_round tb_tot-job rd_alptime tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btnCustList 
    LABEL "Preview" 
    SIZE 9.8 BY .81.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_date    AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_dept    AS CHARACTER FORMAT "X(4)" 
    LABEL "Beginning Department" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_mach    AS CHARACTER FORMAT "X(6)" 
    LABEL "Beginning Machine" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_Shift   AS INTEGER   FORMAT ">9" INITIAL 1 
    LABEL "Beginning Shift" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no   AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date      AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_dept      AS CHARACTER FORMAT "X(4)" INITIAL "zzzz" 
    LABEL "Ending Department" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_mach      AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
    LABEL "Ending Machine" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_shift     AS INTEGER   FORMAT ">9" INITIAL 3 
    LABEL "Ending shift" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE rd_alptime    AS CHARACTER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Alphabetically", "AL",
    "By Start Time", "TM"
    SIZE 42 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 11.86.

DEFINE VARIABLE tbAutoClose  AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE TB_round     AS LOGICAL INITIAL NO 
    LABEL "Round Decimals" 
    VIEW-AS TOGGLE-BOX
    SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE tb_sched     AS LOGICAL INITIAL NO 
    LABEL "Print by Scheduled Machine?" 
    VIEW-AS TOGGLE-BOX
    SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE tb_tot-job   AS LOGICAL INITIAL NO 
    LABEL "Show ?" 
    VIEW-AS TOGGLE-BOX
    SIZE 40 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_dept AT ROW 2.67 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Department"
    end_dept AT ROW 2.67 COL 69.6 COLON-ALIGNED HELP
    "Enter Ending Department"
    begin_mach AT ROW 3.62 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Machine"
    end_mach AT ROW 3.62 COL 69.6 COLON-ALIGNED HELP
    "Enter Ending Machine"
    begin_date AT ROW 4.57 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 4.57 COL 69.6 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_Shift AT ROW 5.48 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Shift"
    end_shift AT ROW 5.52 COL 69.6 COLON-ALIGNED HELP
    "Enter Ending Shift"
    tb_cust-list AT ROW 6.67 COL 30 WIDGET-ID 6
    btnCustList AT ROW 6.71 COL 62 WIDGET-ID 8
    begin_cust-no AT ROW 7.95 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust-no AT ROW 7.95 COL 69.6 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    tb_sched AT ROW 9.33 COL 30
    TB_round AT ROW 10.14 COL 30 WIDGET-ID 2
    tb_tot-job AT ROW 10.95 COL 30 WIDGET-ID 4
    rd_alptime AT ROW 12 COL 30 NO-LABELS WIDGET-ID 14
    tbAutoClose AT ROW 13.48 COL 31 WIDGET-ID 64
    btn-ok AT ROW 14.38 COL 31
    btn-cancel AT ROW 14.38 COL 51
    "Sort jobs:" VIEW-AS TEXT
    SIZE 10 BY .62 AT ROW 12.05 COL 20 WIDGET-ID 18
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 15.71
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
        TITLE              = "Production Analysis (D-E-2)"
        HEIGHT             = 15.71
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
   FRAME-NAME                                                           */
ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_dept:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_mach:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_Shift:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_dept:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_mach:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_shift:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_sched:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Production Analysis (D-E-2) */
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
ON WINDOW-CLOSE OF C-Win /* Production Analysis (D-E-2) */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                .

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


&Scoped-define SELF-NAME begin_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_dept C-Win
ON LEAVE OF begin_dept IN FRAME FRAME-A /* Beginning Department */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_Shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_Shift C-Win
ON LEAVE OF begin_Shift IN FRAME FRAME-A /* Beginning Shift */
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
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

        FIND FIRST  ttCustList NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttCustList AND tb_cust-list THEN 
        DO:
            EMPTY TEMP-TABLE ttCustList.
            RUN BuildCustList(INPUT cocode,
                INPUT tb_cust-list AND glCustListActive ,
                INPUT begin_cust-no,
                INPUT end_cust-no).
        END.


        RUN run-report.
  
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
    DO:
        RUN CustList.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                .

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


&Scoped-define SELF-NAME end_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_dept C-Win
ON LEAVE OF end_dept IN FRAME FRAME-A /* Ending Department */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_shift C-Win
ON LEAVE OF end_shift IN FRAME FRAME-A /* Ending shift */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
    DO:
        ASSIGN {&self-name}.
        EMPTY TEMP-TABLE ttCustList.
        RUN SetCustRange(INPUT tb_cust-list).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TB_round
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TB_round C-Win
ON VALUE-CHANGED OF TB_round IN FRAME FRAME-A /* Round Decimals */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sched
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sched C-Win
ON VALUE-CHANGED OF tb_sched IN FRAME FRAME-A /* Print by Scheduled Machine? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_tot-job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tot-job C-Win
ON VALUE-CHANGED OF tb_tot-job IN FRAME FRAME-A /* Show ? */
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
        begin_date = DATE(MONTH(TODAY), 1, YEAR(TODAY))
        end_date   = TODAY.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png"). 
    RUN enable_UI.

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
APPLY "entry" TO Begin_dept.
END.

RUN sys/ref/CustList.p (INPUT cocode,
    INPUT 'DE2',
    INPUT NO,
    OUTPUT glCustListActive).
{sys/inc/chblankcust.i ""DE2""}

IF ou-log THEN 
DO:
    ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME}     = YES
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
        tb_cust-list                                     = YES 
        .
    RUN SetCustRange(INPUT tb_cust-list).
END.
ELSE
    ASSIGN
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME}     = NO
        .

IF ou-log AND ou-cust-int = 0 THEN 
DO:
    ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME}    = YES
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME}     = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
        tb_cust-list                                     = NO
        .
    RUN SetCustRange(tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "YES").
END.

IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildCustList C-Win 
PROCEDURE BuildCustList :
    /*------------------------------------------------------------------------------
      Purpose:     Builds the temp table of customers   
      Parameters:  Company Code, Customer list logical and/or customer range
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplList AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcBeginCust AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEndCust AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-cust FOR cust.

    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.

    IF iplList THEN 
    DO:
        RUN sys/ref/CustList.p (INPUT ipcCompany,
            INPUT 'DE2',
            INPUT YES,
            OUTPUT lActive).
    END.
    ELSE 
    DO:
        FOR EACH bf-cust
            WHERE bf-cust.company EQ ipcCompany
            AND bf-cust.cust-no GE ipcBeginCust
            AND bf-cust.cust-no LE ipcEndCust
            NO-LOCK:
            CREATE ttCustList.
            ASSIGN 
                ttCustList.cust-no = bf-cust.cust-no
                ttCustList.log-fld = YES
                .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CleanUp C-Win 
PROCEDURE CleanUp :
    /*------------------------------------------------------------------------------
      Purpose:    Clean up routine.
      Parameters: <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    /* RELEASE OBJECTS */
    RELEASE OBJECT chWorkbook         NO-ERROR.
    RELEASE OBJECT chWorkSheet        NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustList C-Win 
PROCEDURE CustList :
    /*------------------------------------------------------------------------------
      Purpose:  Display a UI of selected customers   
      Parameters:  
      Notes:       
    ------------------------------------------------------------------------------*/

    RUN sys/ref/CustListManager.w(INPUT cocode,
        INPUT 'DE2').


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
    DISPLAY begin_dept end_dept begin_mach end_mach begin_date end_date 
        begin_Shift end_shift tb_cust-list begin_cust-no end_cust-no tb_sched 
        TB_round tb_tot-job rd_alptime tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-7 begin_dept end_dept begin_mach end_mach begin_date end_date 
        begin_Shift end_shift tb_cust-list btnCustList begin_cust-no 
        end_cust-no tb_sched TB_round tb_tot-job rd_alptime tbAutoClose btn-ok 
        btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeExcel C-Win 
PROCEDURE InitializeExcel :
    /*------------------------------------------------------------------------------
      Purpose   :   Initializes Excel Environment
      Parameters:   None
      Notes     :   
    ------------------------------------------------------------------------------*/

    /* Connect to the running Excel session. */
    CREATE "Excel.Application" chExcelApplication NO-ERROR.
    IF NOT VALID-HANDLE(chExcelApplication) THEN
        RETURN.

    RUN sys/ref/getFileFullPathName.p ("Template\ProdAnalysis.xlt", OUTPUT chFile).
    IF chFile = ? THEN  
        APPLY 'close' TO THIS-PROCEDURE.

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
/*RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).*/
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
/*run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pro-rate-mr C-Win 
PROCEDURE pro-rate-mr :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-mch-act FOR mch-act.
    DEFINE BUFFER b-job-cod FOR job-code.


    FOR EACH b-mch-act NO-LOCK
        WHERE b-mch-act.company  EQ mch-act.company
        AND b-mch-act.job      EQ mch-act.job
        AND b-mch-act.job-no   EQ mch-act.job-no
        AND b-mch-act.job-no2  EQ mch-act.job-no2
        AND b-mch-act.m-code   EQ mch-act.m-code
        AND b-mch-act.dept     EQ mch-act.dept
        AND b-mch-act.pass     EQ mch-act.pass
        AND b-mch-act.frm      EQ mch-act.frm
        AND b-mch-act.blank-no EQ mch-act.blank-no,
        FIRST b-job-cod NO-LOCK
        WHERE b-job-cod.code EQ b-mch-act.code:

        IF b-job-cod.cat EQ "RUN" THEN
            tt-srt.tot-run-hours = tt-srt.tot-run-hours + b-mch-act.hours.
        ELSE
            IF b-job-cod.cat EQ "MR" THEN
                tt-srt.tot-mr-hours  = tt-srt.tot-mr-hours + b-mch-act.hours.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------  pc/rep/mch-dpt.p 8/94 gb */
/* Production by Department Report                                            */
/* -------------------------------------------------------------------------- */
    {sys/form/r-topw.f}

    DEFINE BUFFER b-mch-act FOR mch-act.

    DEFINE VARIABLE v-date         AS DATE      EXTENT 2 FORMAT "99/99/9999" NO-UNDO.
    DEFINE VARIABLE v-dept         AS ch        FORMAT "x(4)" EXTENT 2 INITIAL ["","zzzz"].
    DEFINE VARIABLE v-mach         AS ch        FORMAT "x(6)" EXTENT 2 INITIAL ["","zzzzzz"].
    DEFINE VARIABLE v-shift        LIKE mch-act.shift FORMAT ">>" EXTENT 2 INITIAL ["1", "99"].
    DEFINE VARIABLE v-show         AS LOGICAL   FORMAT "Y/N" INIT YES NO-UNDO.
    DEFINE VARIABLE v-show1        AS LOGICAL   FORMAT "Y/N" INIT YES NO-UNDO.
    DEFINE VARIABLE mch-mr-std     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE mch-run-std    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE mch-mr-act     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE mch-run-act    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE mch-dt-act     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE mch-qty-prod   AS DECIMAL   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE mch-qty-expect AS DECIMAL   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE dpt-mr-std     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE dpt-run-std    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE dpt-mr-act     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE dpt-run-act    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE dpt-dt-act     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE dpt-qty-prod   AS DECIMAL   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE dpt-qty-expect AS DECIMAL   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE shf-mr-std     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE shf-run-std    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE shf-mr-act     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE shf-run-act    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE shf-dt-act     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE shf-qty-prod   AS DECIMAL   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE shf-qty-expect AS DECIMAL   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE shf-jobs       AS INTEGER   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE tot-jobs       AS INTEGER   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE mr-eff         AS DECIMAL   FORMAT ">>>9.9-" NO-UNDO.
    DEFINE VARIABLE run-eff        AS DECIMAL   FORMAT ">>>9.9-" NO-UNDO.
    DEFINE VARIABLE tot-eff        AS DECIMAL   FORMAT ">>>9.9-" NO-UNDO.
    DEFINE VARIABLE dt-eff         AS DECIMAL   FORMAT ">>>9.9-" NO-UNDO.
    DEFINE VARIABLE tot-std-hrs    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE tot-act-hrs    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE a              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-tot-uni-jobs AS LOG       NO-UNDO.
    DEFINE VARIABLE hdr-tit        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdr-tit2       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdr-tit3       AS CHARACTER NO-UNDO.

    DEFINE VARIABLE excelheader    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE mch-qty-ton    AS DECIMAL   FORMAT ">>,>>9.99" NO-UNDO.
    DEFINE VARIABLE shf-qty-ton    AS DECIMAL   FORMAT ">>,>>9.99" NO-UNDO.
    DEFINE VARIABLE dpt-qty-ton    AS DECIMAL   FORMAT ">>,>>9.99" NO-UNDO.
    DEFINE VARIABLE mch-qty-msf    AS DECIMAL   FORMAT ">>,>>9.99" NO-UNDO.
    DEFINE VARIABLE shf-qty-msf    AS DECIMAL   FORMAT ">>,>>9.99" NO-UNDO.
    DEFINE VARIABLE dpt-qty-msf    AS DECIMAL   FORMAT ">>,>>9.99" NO-UNDO.

    DEFINE VARIABLE job-mr-std     AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE job-run-std    AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE job-mr-act     AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE job-run-act    AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE job-dt-act     AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE job-qty-prod   AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE job-qty-expect AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE b              AS CHARACTER NO-UNDO .
    DEFINE VARIABLE v-calmsf       AS DECIMAL   FORMAT ">>>>>>.99" NO-UNDO .
    DEFINE VARIABLE v-num-up       AS INTEGER   FORMAT ">>>,>>9" NO-UNDO .
    DEFINE VARIABLE v-act-lab-cost AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-pic-per-hrs  AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-msf-per-hrs  AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-kik-per-hrs  AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-wst          AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-per-man-hrs  AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-cust-no      AS CHARACTER NO-UNDO .

    DEFINE VARIABLE v-mrcomp       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-runcomp      AS CHARACTER NO-UNDO .
    DEFINE VARIABLE v-mrwaste      AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-runwaste     AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-crew-size    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lSelected      AS LOG       INIT YES NO-UNDO.
    DEFINE BUFFER bf-mch-act FOR mch-act .
    DEFINE VARIABLE fcust AS CHARACTER NO-UNDO.
    DEFINE VARIABLE tcust AS CHARACTER NO-UNDO.
    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        str-tit2       = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-dept[1]      = begin_dept
        v-dept[2]      = end_dept
        v-mach[1]      = begin_mach
        v-mach[2]      = end_mach
        v-shift[1]     = begin_shift
        v-shift[2]     = end_shift
        v-date[1]      = begin_date
        v-date[2]      = end_date
        fcust          = begin_cust-no
        tcust          = end_cust-no
        /*v-show      = tb_show*/
        /*v-show1     = tb_show1*/
        v-tot-uni-jobs = tb_tot-job
        lSelected      = tb_cust-list
        hdr-tit3       = FILL("-", 132).

    inrowcount = 2 .
    IF lselected THEN 
    DO:
        FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
        IF AVAILABLE ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
        FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
        IF AVAILABLE ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
    END.

    RUN InitializeExcel.
    IF NOT VALID-HANDLE(chExcelApplication) THEN 
    DO:
        MESSAGE 
            "Microsoft Excel is required.  This report is unable to be executed."
            VIEW-AS ALERT-BOX ERROR.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN.
    END.


    /* Open our Excel Template. */  
    ASSIGN 
        chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.

    /* Do not display Excel error messages. */
    chExcelApplication:DisplayAlerts = FALSE  NO-ERROR.

    /* Disable screen updating so it will go faster */
    chExcelApplication:ScreenUpdating = FALSE.

    {pcrep/r-prodlys.i}

    /* let the user in */
    ASSIGN 
        chExcelApplication:VISIBLE = TRUE.
    chWorkbook:WorkSheets({&summary-sheet}):Activate NO-ERROR.

    ASSIGN
        chWorkSheet                       = chExcelApplication:Sheets:item({&summary-sheet})
        /* enable screen updating */
        chExcelApplication:ScreenUpdating = TRUE.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    /*run MainLoop.*/
    RUN Cleanup.


    SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCustRange C-Win 
PROCEDURE SetCustRange :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            begin_cust-no:SENSITIVE = NOT iplChecked
            end_cust-no:SENSITIVE   = NOT iplChecked
            begin_cust-no:VISIBLE   = NOT iplChecked
            end_cust-no:VISIBLE     = NOT iplChecked
            btnCustList:SENSITIVE   = iplChecked
            .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

