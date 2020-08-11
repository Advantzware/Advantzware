&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
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
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.
 
{est\ttInputEst.i NEW}

DEFINE VARIABLE hdEstimateCalcProcs AS HANDLE.
DEFINE VARIABLE hFreightProcs       AS HANDLE    NO-UNDO.
DEFINE VARIABLE tmp-dir             AS CHARACTER NO-UNDO .

DEFINE NEW SHARED BUFFER xest FOR est.
DEFINE NEW SHARED BUFFER xef  FOR ef.
DEFINE NEW SHARED BUFFER xeb  FOR eb.
DEFINE NEW SHARED BUFFER xqty FOR est-qty.
DEFINE NEW SHARED VARIABLE xcal   AS DECIMAL       NO-UNDO.
DEFINE NEW SHARED VARIABLE sh-wid AS DECIMAL       NO-UNDO.
DEFINE NEW SHARED VARIABLE sh-len AS DECIMAL       NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-18 begin_est-no end_est-no ~
begin_quo-date end_quo-date begin_ord-date end_ord-date begin_cust-no ~
end_cust-no tb_create-release tb_cal-est tb_create-quote btnCalendar-1 ~
btnCalendar-2 btnCalendar-3 btnCalendar-4  btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_est-no end_est-no begin_quo-date ~
end_quo-date begin_ord-date end_ord-date begin_cust-no end_cust-no ~
tb_create-release tb_cal-est tb_create-quote

&Scoped-define calendarPopup btnCalendar-1 btnCalendar-2  ~
btnCalendar-3 btnCalendar-4

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE            VARIABLE C-Win  AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".
    
DEFINE BUTTON btnCalendar-3 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-4 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".    

DEFINE BUTTON btn-cancel 
    LABEL "Ca&ncel" 
    SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
    LABEL "&Start Process" 
    SIZE 18 BY 1.14.

DEFINE VARIABLE begin_cust-no  AS CHARACTER FORMAT "X(8)" 
    LABEL "Begining Customer#" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1.

DEFINE VARIABLE begin_est-no   AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Estimate#" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1.

DEFINE VARIABLE begin_ord-date AS DATE      FORMAT "99/99/9999" INITIAL 01/01/001 
    LABEL "Beginning Ordered Date" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1.

DEFINE VARIABLE begin_quo-date AS DATE      FORMAT "99/99/9999" INITIAL 01/01/001 
    LABEL "Beginning Quote Date" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1.

DEFINE VARIABLE end_cust-no    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1.

DEFINE VARIABLE end_est-no     AS CHARACTER FORMAT "X(8)" INITIAL "99999999" 
    LABEL "Ending Estimate#" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1.

DEFINE VARIABLE end_ord-date   AS DATE      FORMAT "99/99/9999" INITIAL 12/31/9999 
    LABEL "Ending Ordered Date" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1.

DEFINE VARIABLE end_quo-date   AS DATE      FORMAT "99/99/9999" INITIAL 12/31/9999 
    LABEL "Ending Quote Date" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1.

DEFINE RECTANGLE RECT-18
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 100 BY 10.43.

DEFINE VARIABLE tb_cal-est        AS LOGICAL INITIAL NO 
    LABEL "Calculate Estimate" 
    VIEW-AS TOGGLE-BOX
    SIZE 67.2 BY .81 NO-UNDO.

DEFINE VARIABLE tb_create-quote   AS LOGICAL INITIAL NO 
    LABEL "Create a quote for the quantities from the source quote id" 
    VIEW-AS TOGGLE-BOX
    SIZE 67.2 BY .81 NO-UNDO.

DEFINE VARIABLE tb_create-release AS LOGICAL INITIAL NO 
    LABEL "Create Release for each quantity" 
    VIEW-AS TOGGLE-BOX
    SIZE 43.2 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_est-no AT ROW 2.71 COL 31 COLON-ALIGNED HELP
    "Enter Begining Estimate# " WIDGET-ID 6
    end_est-no AT ROW 2.71 COL 78.8 COLON-ALIGNED HELP
    "Enter Ending Estimate# " WIDGET-ID 8
    begin_quo-date AT ROW 3.86 COL 31 COLON-ALIGNED HELP
    "Enter Beginning Quote Date" WIDGET-ID 2
    btnCalendar-1 AT ROW 3.86 COL 48      
    end_quo-date AT ROW 3.86 COL 78.8 COLON-ALIGNED HELP
    "Enter Ending Quote Date" WIDGET-ID 4
    btnCalendar-2 AT ROW 3.86 COL 96     
    begin_ord-date AT ROW 5.1 COL 31 COLON-ALIGNED HELP
    "Enter Beginning Order Date" WIDGET-ID 10
    btnCalendar-3 AT ROW 5.1 COL 48   
    end_ord-date AT ROW 5.1 COL 78.8 COLON-ALIGNED HELP
    "Enter Ending Order Date" WIDGET-ID 12
    btnCalendar-4 AT ROW 5.1 COL 96      
    begin_cust-no AT ROW 6.29 COL 31 COLON-ALIGNED HELP
    "Enter Begining Customer No"
    end_cust-no AT ROW 6.29 COL 78.8 COLON-ALIGNED HELP
    "Enter Ending Customer No"
    tb_create-release AT ROW 8.19 COL 33.8 WIDGET-ID 60
    tb_cal-est AT ROW 9.52 COL 33.8 WIDGET-ID 62
    tb_create-quote AT ROW 10.81 COL 33.8 WIDGET-ID 64
    btn-process AT ROW 12.91 COL 33.4
    btn-cancel AT ROW 12.91 COL 64.4
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .95 AT ROW 1.24 COL 4
    FONT 4
    RECT-18 AT ROW 1.76 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 102.6 BY 14.24.


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
        TITLE              = "Create Logistics Estimates"
        HEIGHT             = 14.24
        WIDTH              = 102.6
        MAX-HEIGHT         = 26.62
        MAX-WIDTH          = 160
        VIRTUAL-HEIGHT     = 26.62
        VIRTUAL-WIDTH      = 160
        RESIZE             = YES
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = YES
        THREE-D            = YES
        FONT               = 6
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
   
/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME  FRAME-A
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME FRAME-A
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-3 IN FRAME  FRAME-A
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-4 IN FRAME FRAME-A
   3                                                                    */   
ASSIGN 
    begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_est-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ord-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_quo-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-process:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_est-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_ord-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_quo-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Create Logistics Estimates */
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
ON WINDOW-CLOSE OF C-Win /* Create Logistics Estimates */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Begining Customer# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON VALUE-CHANGED OF begin_cust-no IN FRAME FRAME-A /* Begining Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est-no C-Win
ON LEAVE OF begin_est-no IN FRAME FRAME-A /* Beginning Estimate# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est-no C-Win
ON VALUE-CHANGED OF begin_est-no IN FRAME FRAME-A /* Beginning Estimate# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est-no C-Win
ON HELP OF begin_est-no IN FRAME FRAME-A
DO:
   DEF VAR char-val AS cha NO-UNDO.

    RUN windows/l-esttyp.w (g_company,g_loc,"568","EST",FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN begin_est-no:SCREEN-VALUE = ENTRY(1,char-val).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-date C-Win
ON LEAVE OF begin_ord-date IN FRAME FRAME-A /* Beginning Ordered Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_quo-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_quo-date C-Win
ON LEAVE OF begin_quo-date IN FRAME FRAME-A /* Beginning Quote Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 C-Win
ON CHOOSE OF btnCalendar-1 IN FRAME FRAME-A
    DO:
        {methods/btnCalendar.i begin_quo-date }
        APPLY "entry" TO begin_quo-date .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 C-Win
ON CHOOSE OF btnCalendar-2 IN FRAME FRAME-A
    DO:
        {methods/btnCalendar.i end_quo-date}
        APPLY 'entry' TO end_quo-date.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnCalendar-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-3 C-Win
ON CHOOSE OF btnCalendar-3 IN FRAME FRAME-A
    DO:
        {methods/btnCalendar.i begin_ord-date }
        APPLY "entry" TO begin_ord-date .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnCalendar-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-4 C-Win
ON CHOOSE OF btnCalendar-4 IN FRAME FRAME-A
    DO:
        {methods/btnCalendar.i end_ord-date}
        APPLY 'entry' TO end_ord-date.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_quo-date C-Win
ON HELP OF begin_quo-date IN FRAME FRAME-A /* quote Date */
    DO:
        {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_quo-date C-Win
ON HELP OF end_quo-date IN FRAME FRAME-A /* quote Date */
    DO:
        {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-date C-Win
ON HELP OF begin_ord-date IN FRAME FRAME-A /* order Date */
    DO:
        {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-date C-Win
ON HELP OF end_ord-date IN FRAME FRAME-A /* order Date */
    DO:
        {methods/calpopup.i}
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


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
    DO:
        DEFINE VARIABLE v-process AS LOG INIT NO NO-UNDO.
    
        DO WITH FRAME {&FRAME-NAME}:  
            begin_est-no:SCREEN-VALUE = FILL(" ",8 - LENGTH(TRIM(INPUT begin_est-no))) + TRIM(INPUT begin_est-no).
            end_est-no:SCREEN-VALUE = FILL(" ",8 - LENGTH(TRIM(INPUT end_est-no))) + TRIM(INPUT end_est-no).
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.

        MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) 
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE v-process.

        IF v-process THEN RUN run-process.   
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est-no C-Win
ON LEAVE OF end_est-no IN FRAME FRAME-A /* Ending Estimate# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est-no C-Win
ON HELP OF end_est-no IN FRAME FRAME-A
DO:
   DEF VAR char-val AS cha NO-UNDO.

    RUN windows/l-esttyp.w (g_company,g_loc,"568","EST",FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN end_est-no:SCREEN-VALUE = ENTRY(1,char-val).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-date C-Win
ON LEAVE OF end_ord-date IN FRAME FRAME-A /* Ending Ordered Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_quo-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_quo-date C-Win
ON LEAVE OF end_quo-date IN FRAME FRAME-A /* Ending Quote Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cal-est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cal-est C-Win
ON VALUE-CHANGED OF tb_cal-est IN FRAME FRAME-A /* Calculate the estimate using the new estimate calculation engine */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_create-quote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_create-quote C-Win
ON VALUE-CHANGED OF tb_create-quote IN FRAME FRAME-A /* Create a quote for the quantities from the source quote id */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_create-release
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_create-release C-Win
ON VALUE-CHANGED OF tb_create-release IN FRAME FRAME-A /* Create Release for each quantity */
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
    
    RUN enable_UI.

    {methods/nowait.i}

    DO WITH FRAME {&frame-name}:    
        {custom/usrprint.i}          
        APPLY "entry" TO begin_est-no.         
    END.
    
    tb_create-quote:HIDDEN IN FRAME {&FRAME-NAME} = YES .
    IF end_ord-date EQ ? THEN
    ASSIGN
        begin_ord-date = 01/01/2019
        end_ord-date = TODAY.        

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
    DISPLAY begin_est-no end_est-no begin_quo-date end_quo-date begin_ord-date 
        end_ord-date begin_cust-no end_cust-no tb_create-release tb_cal-est 
        tb_create-quote 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-18 begin_est-no end_est-no begin_quo-date end_quo-date 
        begin_ord-date end_ord-date begin_cust-no end_cust-no 
        tb_create-release tb_cal-est tb_create-quote btnCalendar-1 btnCalendar-2
        btnCalendar-3 btnCalendar-4 btn-process btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateInputEst C-Win 
PROCEDURE pCreateInputEst :
    DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO .
    DEFINE BUFFER b-eb FOR eb.
    
    
    FIND FIRST b-eb WHERE ROWID(b-eb) EQ  ipriRowid NO-LOCK NO-ERROR .
    
    IF AVAILABLE b-eb THEN
    DO:
        FIND FIRST ef NO-LOCK
            WHERE ef.company EQ b-eb.company
            AND ef.est-no EQ b-eb.est-no
            AND ef.form-no EQ b-eb.form-no  
            NO-ERROR.
                
        CREATE ttInputEst.
        ASSIGN 
            ttInputEst.cCompany         = cocode
            ttInputEst.cPartID          = b-eb.part-no
            ttInputEst.cStockNo         = b-eb.stock-no
            ttInputEst.cPartName        = b-eb.part-dscr1
            ttInputEst.cPartDescription = b-eb.part-dscr2        
            ttInputEst.cBndlCode        = b-eb.cas-no
            ttInputEst.iUnitCount       = b-eb.cas-cnt
            ttInputEst.iPerPallet       = b-eb.cas-pal
            ttInputEst.iPartial         = b-eb.quantityPartial
            ttInputEst.cPallet          = b-eb.tr-no
            ttInputEst.iFormNo          = 1
            ttInputEst.iBlankNo         = 1 
            ttInputEst.cCustomer        = b-eb.cust-no 
            ttInputEst.cShipTo          = b-eb.ship-id 
            ttInputEst.cStyle           = b-eb.style 
            ttInputEst.dLength          = b-eb.len 
            ttInputEst.dWidth           = b-eb.wid 
            ttInputEst.dDepth           = b-eb.dep 
            ttInputEst.cBoard           = IF AVAILABLE ef THEN ef.board ELSE ""  
            ttInputEst.iQuantity        = b-eb.eqty 
            ttInputEst.cCategory        = b-eb.procat 
            ttInputEst.dWeightPerM      = b-eb.weight
            ttInputEst.iStackHeight     = b-eb.stackHeight
            ttInputEst.iStackCode       = b-eb.stack-code 
            ttInputEst.cEstType         = "MiscEstimate"
            ttInputEst.cSourceEst       = b-eb.est-no 
            .
        
        FIND FIRST est-qty NO-LOCK
            WHERE est-qty.company EQ b-eb.company
            AND est-qty.est-no EQ b-eb.est-no
            AND est-qty.eqty EQ b-eb.eqty NO-ERROR . 
    
        IF AVAILABLE est-qty THEN 
        DO:             
            ASSIGN 
                ttInputEst.copy-qty[2]  = est-qty.qty[2] 
                ttInputEst.copy-qty[3]  = est-qty.qty[3] 
                ttInputEst.copy-qty[4]  = est-qty.qty[4] 
                ttInputEst.copy-qty[5]  = est-qty.qty[5] 
                ttInputEst.copy-qty[6]  = est-qty.qty[6] 
                ttInputEst.copy-qty[7]  = est-qty.qty[7] 
                ttInputEst.copy-qty[8]  = est-qty.qty[8] 
                ttInputEst.copy-qty[9]  = est-qty.qty[9] 
                ttInputEst.copy-qty[10] = est-qty.qty[10]
         
                ttInputEst.copy-qty[11] = est-qty.qty[11] 
                ttInputEst.copy-qty[12] = est-qty.qty[12] 
                ttInputEst.copy-qty[13] = est-qty.qty[13] 
                ttInputEst.copy-qty[14] = est-qty.qty[14] 
                ttInputEst.copy-qty[15] = est-qty.qty[15] 
                ttInputEst.copy-qty[16] = est-qty.qty[16] 
                ttInputEst.copy-qty[17] = est-qty.qty[17] 
                ttInputEst.copy-qty[18] = est-qty.qty[18] 
                ttInputEst.copy-qty[19] = est-qty.qty[19] 
                ttInputEst.copy-qty[20] = est-qty.qty[20] 

                ttInputEst.copy-rel[1]  = est-qty.qty[21]
                ttInputEst.copy-rel[2]  = est-qty.qty[22] 
                ttInputEst.copy-rel[3]  = est-qty.qty[23] 
                ttInputEst.copy-rel[4]  = est-qty.qty[24] 
                ttInputEst.copy-rel[5]  = est-qty.qty[25] 
                ttInputEst.copy-rel[6]  = est-qty.qty[26] 
                ttInputEst.copy-rel[7]  = est-qty.qty[27] 
                ttInputEst.copy-rel[8]  = est-qty.qty[28] 
                ttInputEst.copy-rel[9]  = est-qty.qty[29] 
                ttInputEst.copy-rel[10] = est-qty.qty[30]
         
                ttInputEst.copy-rel[11] = est-qty.qty[31] 
                ttInputEst.copy-rel[12] = est-qty.qty[32] 
                ttInputEst.copy-rel[13] = est-qty.qty[33] 
                ttInputEst.copy-rel[14] = est-qty.qty[34] 
                ttInputEst.copy-rel[15] = est-qty.qty[35] 
                ttInputEst.copy-rel[16] = est-qty.qty[36] 
                ttInputEst.copy-rel[17] = est-qty.qty[37] 
                ttInputEst.copy-rel[18] = est-qty.qty[38] 
                ttInputEst.copy-rel[19] = est-qty.qty[39] 
                ttInputEst.copy-rel[20] = est-qty.qty[40] .    
     
     
            ASSIGN
                ttInputEst.copy-runship[1]  = STRING(est-qty.whsed[1])
                ttInputEst.copy-runship[2]  = STRING(est-qty.whsed[2])
                ttInputEst.copy-runship[3]  = STRING(est-qty.whsed[3])
                ttInputEst.copy-runship[4]  = STRING(est-qty.whsed[4])
                ttInputEst.copy-runship[5]  = STRING(est-qty.whsed[5])
                ttInputEst.copy-runship[6]  = STRING(est-qty.whsed[6])
                ttInputEst.copy-runship[7]  = STRING(est-qty.whsed[7])
                ttInputEst.copy-runship[8]  = STRING(est-qty.whsed[8])
                ttInputEst.copy-runship[9]  = STRING(est-qty.whsed[9])
                ttInputEst.copy-runship[10] = STRING(est-qty.whsed[10])
                ttInputEst.copy-runship[11] = STRING(est-qty.whsed[11])
                ttInputEst.copy-runship[12] = STRING(est-qty.whsed[12])
                ttInputEst.copy-runship[13] = STRING(est-qty.whsed[13])
                ttInputEst.copy-runship[14] = STRING(est-qty.whsed[14])
                ttInputEst.copy-runship[15] = STRING(est-qty.whsed[15])
                ttInputEst.copy-runship[16] = STRING(est-qty.whsed[16])
                ttInputEst.copy-runship[17] = STRING(est-qty.whsed[17])
                ttInputEst.copy-runship[18] = STRING(est-qty.whsed[18])
                ttInputEst.copy-runship[19] = STRING(est-qty.whsed[19])
                ttInputEst.copy-runship[20] = STRING(est-qty.whsed[20]).
        
        END.

    END.
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
    DEFINE VARIABLE riEb          AS ROWID     NO-UNDO .
    DEFINE VARIABLE iEstReleaseID AS INTEGER   NO-UNDO .
    DEFINE VARIABLE lError        AS LOGICAL   NO-UNDO .
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO .
  
    DEFINE VARIABLE list-name     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE init-dir      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFileName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
  
    DEFINE VARIABLE lPurge        AS LOGICAL   NO-UNDO.
  
    DEFINE BUFFER bf-est FOR est.
    DEFINE BUFFER bf-ef  FOR ef.
    DEFINE BUFFER bf-eb  FOR eb.
    DEFINE BUFFER bff-eb FOR eb.
  
    {sys/inc/print1.i}         
  
    RUN est\EstimateCalcProcs.p PERSISTENT SET hdEstimateCalcProcs.
    THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdEstimateCalcProcs).
  
    RUN system/FreightProcs.p PERSISTENT SET hFreightProcs.
    THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hFreightProcs).      
  
    cFileName =  init-dir + "\" + "CreateMiscEstimate.log" .
 
    OUTPUT TO value(cFileName) .  
    iCount = 0. 
    FOR EACH bf-est NO-LOCK
        WHERE bf-est.company EQ cocode
        AND bf-est.est-type GE 5         
        AND bf-est.est-no GE begin_est-no
        AND bf-est.est-no LE end_est-no
        AND bf-est.ord-date GE begin_ord-date
        AND bf-est.ord-date LE end_ord-date,
        FIRST bf-eb NO-LOCK
        WHERE bf-eb.company EQ cocode
        AND bf-eb.form-no NE 0
        AND bf-eb.est-no EQ bf-est.est-no 
        AND bf-eb.cust-no GE begin_cust-no
        AND bf-eb.cust-no LE end_cust-no,
        FIRST quotehd NO-LOCK
        WHERE quotehd.company  EQ bf-est.company          
        AND quotehd.loc EQ bf-est.loc
        AND quotehd.est-no    EQ bf-est.est-no
        AND quotehd.quo-date GE begin_quo-date
        AND quotehd.quo-date LE end_quo-date
        AND (quotehd.expireDate GE TODAY OR quotehd.expireDate EQ ?)
        AND quotehd.quo-date LE TODAY
        :
        PUT UNFORMATTED  
            'Procesing estimate ' STRING(bf-est.est-no)   SKIP. 
        EMPTY TEMP-TABLE ttInputEst .
        RUN pCreateInputEst(ROWID(bf-eb)) .        
               
        RUN est/BuildEstimate.p ("C", OUTPUT riEb).             
       
        FIND FIRST bff-eb NO-LOCK
            WHERE bff-eb.company EQ cocode
            AND ROWID(bff-eb) EQ riEb NO-ERROR .
        
        IF AVAILABLE bff-eb THEN
        DO:
            PUT UNFORMATTED  
                'Creating Misc estimate ' STRING(bff-eb.est-no)   SKIP.
            iCount = iCount + 1.
        END.
        IF AVAILABLE bff-eb THEN 
        DO:
      
            RUN est/BuildFarmForLogistics.p (INPUT riEb,INPUT NO).
      
            PUT UNFORMATTED  
                'Vendor Item Cost Created ' STRING(bff-eb.est-no)   SKIP.
        END.
     
        IF AVAILABLE bff-eb THEN 
        DO:
            IF tb_create-release EQ NO THEN
                RUN CreateEstReleaseForEstBlank (INPUT riEb, INPUT NO, OUTPUT iEstReleaseID ,
                    OUTPUT lError,OUTPUT cMessage) .
            ELSE 
                RUN CreateEstReleaseForEstBlank (INPUT riEb, INPUT YES, OUTPUT iEstReleaseID ,
                    OUTPUT lError,OUTPUT cMessage) .
            IF iEstReleaseID NE 0 THEN
                PUT UNFORMATTED  'Release created for ' STRING(bff-eb.est-no)   SKIP.
        END.
     
        IF tb_cal-est EQ TRUE AND AVAILABLE bff-eb THEN 
        DO:
            RUN CalculateEstimate IN hdEstimateCalcProcs (bff-eb.company, bff-eb.est-no, lPurge).   
            PUT UNFORMATTED  
                'Calculate Estimate ' STRING(bff-eb.est-no)   SKIP.  
        END.         
     
     
    END.     
        
    SESSION:SET-WAIT-STATE("").      
    
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  
    MESSAGE TRIM(c-win:TITLE) + " Created [" STRING(iCount) "] estimates from [" STRING(iCount)"] source estimates.  View log in " STRING(cFileName) " file." VIEW-AS ALERT-BOX.  
    
    IF iCount EQ 0 AND begin_est-no EQ end_est-no THEN
    DO:
      FIND FIRST est NO-LOCK
        WHERE est.company EQ cocode
        AND est.est-type GE 5         
        AND est.est-no EQ begin_est-no NO-ERROR .
        
        IF NOT AVAIL est THEN
        DO:
             PUT UNFORMATTED  
            'Invalid estimate' STRING(begin_est-no)   SKIP.
        END.
        
        IF AVAIL est THEN
        DO:
          IF est.ord-date EQ ? OR (est.ord-date LT begin_ord-date
                  AND est.ord-date GT end_ord-date) THEN
         PUT UNFORMATTED  
            'No order exists on estimate' STRING(begin_est-no)   SKIP.
          FIND FIRST quotehd NO-LOCK
                WHERE quotehd.company  EQ est.company          
                AND quotehd.loc EQ est.loc
                AND quotehd.est-no EQ est.est-no
                AND quotehd.quo-date GE begin_quo-date
                AND quotehd.quo-date LE end_quo-date
                AND (quotehd.expireDate GE TODAY OR quotehd.expireDate EQ ?)
                AND quotehd.quo-date LE TODAY   NO-ERROR .
                
                IF NOT AVAIL quotehd THEN
                 PUT UNFORMATTED  
                 'No Quote exists within quote date range' STRING(begin_est-no)   SKIP.                
        END.          
    END.
    
    OUTPUT CLOSE.
  
    THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdEstimateCalcProcs).
    THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hFreightProcs).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

