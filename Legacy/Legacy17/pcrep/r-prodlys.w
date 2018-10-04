&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: pcrep\r-prodep.w

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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

{sys/inc/custlistform.i ""DE2"" }

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

/* Variables for excel Automation  */
DEF VARIABLE chExcelApplication   AS COM-HANDLE   NO-UNDO.
DEF VARIABLE chWorkBook           AS COM-HANDLE   NO-UNDO.
DEF VARIABLE chWorksheet          AS COM-HANDLE   NO-UNDO.
DEF VARIABLE chFile               AS CHAR         NO-UNDO.
DEF VARIABLE CurrDir              AS CHARACTER    NO-UNDO.

DEF VAR v-this-month AS INT NO-UNDO.
DEF VAR v-days-this-month AS INT NO-UNDO.

&GLOBAL-DEFINE summary-sheet 1
&GLOBAL-DEFINE sales-summary-sheet 2
&GLOBAL-DEFINE raw-sales-sheet 3

DEFINE VARIABLE v-cell                          AS CHARACTER                                                    NO-UNDO.
DEFINE VARIABLE inRowCount                      AS INTEGER                                                      NO-UNDO    INITIAL 3.

DEF TEMP-TABLE tt-srt NO-UNDO LIKE mch-srt
                              FIELD act-m-code LIKE mach.m-code
                              FIELD tot-run-hours AS DEC
                              FIELD tot-mr-hours AS DEC
                              FIELD qty-Ton AS dec format ">>,>>9.99"
                              FIELD qty-msf AS DEC FORM ">>,>>9.99"
                              FIELD start-time AS INT 
                              FIELD start-date AS DATE 
                              FIELD i-no LIKE mch-srt.job-no

    INDEX dept-idx dept m-code job-no job-no2 frm blank-no
    INDEX job-idx job-no job-no2.

DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF STREAM excel.

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
begin_cust-no end_cust-no tb_sched TB_round tb_tot-job rd_alptime btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_dept end_dept begin_mach end_mach ~
begin_date end_date begin_Shift end_shift tb_cust-list begin_cust-no ~
end_cust-no tb_sched TB_round tb_tot-job rd_alptime 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel /*AUTO-END-KEY */
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_dept AS CHARACTER FORMAT "X(4)" 
     LABEL "Beginning Department" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_Shift AS INTEGER FORMAT ">9" INITIAL 1 
     LABEL "Beginning Shift" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_dept AS CHARACTER FORMAT "X(4)" INITIAL "zzzz" 
     LABEL "Ending Department" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_shift AS INTEGER FORMAT ">9" INITIAL 3 
     LABEL "Ending shift" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE rd_alptime AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Alphabetically", "AL",
"By Start Time", "TM"
     SIZE 42 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 12.86.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE TB_round AS LOGICAL INITIAL no 
     LABEL "Round Decimals" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE tb_sched AS LOGICAL INITIAL no 
     LABEL "Print by Scheduled Machine?" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE tb_tot-job AS LOGICAL INITIAL no 
     LABEL "Show ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_dept AT ROW 2.67 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Department"
     end_dept AT ROW 2.67 COL 69 COLON-ALIGNED HELP
          "Enter Ending Department"
     begin_mach AT ROW 3.62 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_mach AT ROW 3.62 COL 69 COLON-ALIGNED HELP
          "Enter Ending Machine"
     begin_date AT ROW 4.57 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 4.57 COL 69 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_Shift AT ROW 5.48 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Shift"
     end_shift AT ROW 5.52 COL 69 COLON-ALIGNED HELP
          "Enter Ending Shift"
     tb_cust-list AT ROW 6.67 COL 30.4 WIDGET-ID 6
     btnCustList AT ROW 6.71 COL 62.4 WIDGET-ID 8
     begin_cust-no AT ROW 7.95 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 7.95 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     tb_sched AT ROW 9.33 COL 30
     TB_round AT ROW 10.14 COL 30 WIDGET-ID 2
     tb_tot-job AT ROW 10.95 COL 30 WIDGET-ID 4
     rd_alptime AT ROW 12 COL 30 NO-LABEL WIDGET-ID 14
     btn-ok AT ROW 14.81 COL 21
     btn-cancel AT ROW 14.81 COL 57
     "Sort jobs:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 12.05 COL 20 WIDGET-ID 18
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 94.4 BY 16.48.


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
         HEIGHT             = 16.91
         WIDTH              = 95.6
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

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_Shift:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_shift:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sched:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Production Analysis (D-E-3) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Production Analysis (D-E-3) */
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
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_dept C-Win
ON LEAVE OF begin_dept IN FRAME FRAME-A /* Beginning Department */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_Shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_Shift C-Win
ON LEAVE OF begin_Shift IN FRAME FRAME-A /* Beginning Shift */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust-no,
                    INPUT end_cust-no).
  END.


  run run-report.

     {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
DO:
  RUN CustList.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
  assign {&self-name}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_dept C-Win
ON LEAVE OF end_dept IN FRAME FRAME-A /* Ending Department */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_shift C-Win
ON LEAVE OF end_shift IN FRAME FRAME-A /* Ending shift */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
DO:
  assign {&self-name}.
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
  assign {&self-name}.
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

 /* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.


  assign
   begin_date = date(month(TODAY), 1, year(TODAY))
   end_date   = today.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO Begin_dept.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'DE2',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""DE2""}

  IF ou-log THEN DO:
      ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
        tb_cust-list = YES 
        .
      RUN SetCustRange(INPUT tb_cust-list).
  END.
  ELSE
      ASSIGN
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        .

   IF ou-log AND ou-cust-int = 0 THEN do:
       ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
        tb_cust-list = NO
        .
      RUN SetCustRange(tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "YES").
   END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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

DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.

IF iplList THEN DO:
    RUN sys/ref/CustList.p (INPUT ipcCompany,
                            INPUT 'DE2',
                            INPUT YES,
                            OUTPUT lActive).
END.
ELSE DO:
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
          TB_round tb_tot-job rd_alptime 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 begin_dept end_dept begin_mach end_mach begin_date end_date 
         begin_Shift end_shift tb_cust-list btnCustList begin_cust-no 
         end_cust-no tb_sched TB_round tb_tot-job rd_alptime btn-ok btn-cancel 
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
  CREATE "Excel.Application" chExcelApplication.

  FILE-INFO:FILE-NAME = "template\ProdAnalysis.xlt".

  /* Set the Excel Template to be used. */
  ASSIGN chFile = search (FILE-INFO:FULL-PATHNAME) no-error.

  if search (chFile) = ? then do:
    MESSAGE 'Spreadsheet File: ' FILE-INFO:FULL-PATHNAME
            'cannot be found. Please verify that the file exists.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    apply 'CLOSE':U to this-procedure.
  end.

  /* Make Excel visible. */
  ASSIGN
     chFile = FILE-INFO:FULL-PATHNAME.

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
  DEF BUFFER b-mch-act FOR mch-act.
  DEF BUFFER b-job-cod FOR job-code.


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

DEF BUFFER b-mch-act FOR mch-act.

def var v-date as date extent 2 format "99/99/9999" no-undo.
def var v-dept as ch format "x(4)" extent 2 initial ["","zzzz"].
def var v-mach as ch format "x(6)" extent 2 initial ["","zzzzzz"].
def var v-shift like mch-act.shift format ">>" extent 2 initial ["1", "99"].
def var v-show as logical format "Y/N" init yes no-undo.
def var v-show1 as logical format "Y/N" init yes no-undo.
def var mch-mr-std as dec format ">>>>9.9" no-undo.
def var mch-run-std as dec format ">>>>9.9" no-undo.
def var mch-mr-act as dec format ">>>>9.9" no-undo.
def var mch-run-act as dec format ">>>>9.9" no-undo.
def var mch-dt-act as dec format ">>>>9.9" no-undo.
def var mch-qty-prod as dec format ">,>>>,>>9" no-undo.
def var mch-qty-expect as dec format ">,>>>,>>9" no-undo.
def var dpt-mr-std as dec format ">>>>9.9" no-undo.
def var dpt-run-std as dec format ">>>>9.9" no-undo.
def var dpt-mr-act as dec format ">>>>9.9" no-undo.
def var dpt-run-act as dec format ">>>>9.9" no-undo.
def var dpt-dt-act as dec format ">>>>9.9" no-undo.
def var dpt-qty-prod as dec format ">,>>>,>>9" no-undo.
def var dpt-qty-expect as dec format ">,>>>,>>9" no-undo.
def var shf-mr-std as dec format ">>>>9.9" no-undo.
def var shf-run-std as dec format ">>>>9.9" no-undo.
def var shf-mr-act as dec format ">>>>9.9" no-undo.
def var shf-run-act as dec format ">>>>9.9" no-undo.
def var shf-dt-act as dec format ">>>>9.9" no-undo.
def var shf-qty-prod as dec format ">,>>>,>>9" no-undo.
def var shf-qty-expect as dec format ">,>>>,>>9" no-undo.
def var shf-jobs as int format ">,>>>,>>9" no-undo.
DEF VAR tot-jobs AS INT format ">,>>>,>>9" no-undo.
def var mr-eff as dec format ">>>9.9-" no-undo.
def var run-eff as dec format ">>>9.9-" no-undo.
def var tot-eff as dec format ">>>9.9-" no-undo.
def var dt-eff as dec format ">>>9.9-" no-undo.
def var tot-std-hrs as dec format ">>>>9.9" no-undo.
def var tot-act-hrs as dec format ">>>>9.9" no-undo.
def var a as char no-undo.
DEF VAR v-tot-uni-jobs AS LOG NO-UNDO.
def var hdr-tit as char no-undo.
def var hdr-tit2 as char no-undo.
def var hdr-tit3 as char no-undo.

DEF VAR excelheader AS CHAR NO-UNDO.
def var mch-qty-ton as dec format ">>,>>9.99" no-undo.
def var shf-qty-ton as dec format ">>,>>9.99" no-undo.
def var dpt-qty-ton as dec format ">>,>>9.99" no-undo.
def var mch-qty-msf as dec format ">>,>>9.99" no-undo.
def var shf-qty-msf as dec format ">>,>>9.99" no-undo.
def var dpt-qty-msf as dec format ">>,>>9.99" no-undo.

def var job-mr-std as dec format "->>>9.9" no-undo.
def var job-run-std as dec format "->>>9.9" no-undo.
def var job-mr-act as dec format "->>>9.9" no-undo.
def var job-run-act as dec format "->>>9.9" no-undo.
def var job-dt-act as dec format "->>>9.9" no-undo.
def var job-qty-prod as dec format "->>,>>>,>>9" no-undo.
def var job-qty-expect as dec format "->>,>>>,>>9" no-undo.
DEF VAR b AS CHAR NO-UNDO .
DEF VAR v-calmsf as dec format ">>>>>>.99" NO-UNDO .
DEF VAR v-num-up AS INT FORMAT ">>>,>>9" NO-UNDO .
DEF VAR v-act-lab-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-pic-per-hrs AS DEC NO-UNDO .
DEF VAR v-msf-per-hrs AS DEC NO-UNDO .
DEF VAR v-kik-per-hrs AS DEC NO-UNDO .
DEF VAR v-wst AS DEC NO-UNDO .
DEF VAR v-per-man-hrs AS DEC NO-UNDO .
DEF VAR v-cust-no AS CHAR NO-UNDO .

DEF VAR v-mrcomp AS CHAR NO-UNDO.
DEF VAR v-runcomp AS CHAR NO-UNDO .
DEF VAR v-mrwaste AS DEC NO-UNDO .
DEF VAR v-runwaste AS DEC NO-UNDO .
DEF VAR v-crew-size AS DEC NO-UNDO.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.
DEF BUFFER bf-mch-act FOR mch-act .
DEF VAR fcust AS CHAR NO-UNDO.
DEF VAR tcust AS CHAR NO-UNDO.
SESSION:SET-WAIT-STATE ("general").

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-dept[1]   = begin_dept
 v-dept[2]   = end_dept
 v-mach[1]   = begin_mach
 v-mach[2]   = end_mach
 v-shift[1]  = begin_shift
 v-shift[2]  = end_shift
 v-date[1]   = begin_date
 v-date[2]   = end_date
 fcust       = begin_cust-no
 tcust       = end_cust-no
 /*v-show      = tb_show*/
 /*v-show1     = tb_show1*/
 v-tot-uni-jobs = tb_tot-job
 lSelected   = tb_cust-list
 hdr-tit3 = fill("-", 132).

  inrowcount = 2 .
  IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
  END.

run InitializeExcel.


/* Open our Excel Template. */  
  assign chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.

  /* Do not display Excel error messages. */
  chExcelApplication:DisplayAlerts = false  no-error.

  /* Disable screen updating so it will go faster */
  chExcelApplication:ScreenUpdating = False.

{pcrep/r-prodlys.i}

    /* let the user in */
  ASSIGN chExcelApplication:VISIBLE = TRUE.
  chWorkbook:WorkSheets({&summary-sheet}):Activate no-error.

  ASSIGN
     chWorkSheet = chExcelApplication:Sheets:item({&summary-sheet})
     /* enable screen updating */
     chExcelApplication:ScreenUpdating = TRUE.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    /*run MainLoop.*/
    run Cleanup.


SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

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
        end_cust-no:SENSITIVE = NOT iplChecked
        begin_cust-no:VISIBLE = NOT iplChecked
        end_cust-no:VISIBLE = NOT iplChecked
        btnCustList:SENSITIVE = iplChecked
       .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

