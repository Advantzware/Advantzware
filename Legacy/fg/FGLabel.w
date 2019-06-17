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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 fiBeginJob fiBeginJob-2 fiEndJob ~
fiEndJob-2 fiBeginItem fiEndItem btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiBeginJob fiBeginJob-2 fiEndJob ~
fiEndJob-2 fiBeginItem fiEndItem 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFormatJob C-Win 
FUNCTION fFormatJob RETURNS CHARACTER PRIVATE
  (ipcJob AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE fiBeginItem AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beginning Item" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiBeginJob AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beginning Job" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiBeginJob-2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndItem AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndJob AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzz" 
     LABEL "Ending Job" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndJob-2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 5.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fiBeginJob AT ROW 3.14 COL 17 COLON-ALIGNED WIDGET-ID 28
     fiBeginJob-2 AT ROW 3.14 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     fiEndJob AT ROW 3.14 COL 62 COLON-ALIGNED WIDGET-ID 30
     fiEndJob-2 AT ROW 3.14 COL 79 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     fiBeginItem AT ROW 4.57 COL 17 COLON-ALIGNED WIDGET-ID 10
     fiEndItem AT ROW 4.57 COL 62.2 COLON-ALIGNED WIDGET-ID 12
     btn-process AT ROW 7.43 COL 24
     btn-cancel AT ROW 7.43 COL 54
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.48 COL 3
     RECT-17 AT ROW 1.71 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101.4 BY 8.71.


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
         TITLE              = "Print FG Labels"
         HEIGHT             = 8.81
         WIDTH              = 101.2
         MAX-HEIGHT         = 22.67
         MAX-WIDTH          = 171.6
         VIRTUAL-HEIGHT     = 22.67
         VIRTUAL-WIDTH      = 171.6
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
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Print FG Labels */
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
ON WINDOW-CLOSE OF C-Win /* Print FG Labels */
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


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
        RUN pProcess.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBeginItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBeginItem C-Win
ON HELP OF fiBeginItem IN FRAME FRAME-A /* Beginning Item */
DO:
        DEFINE VARIABLE cFGItemID AS CHARACTER NO-UNDO.
        RUN pLookupFGItem(g_company, FOCUS:SCREEN-VALUE, OUTPUT cFGItemID).
        fiBeginItem:SCREEN-VALUE = cFGItemID.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBeginJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBeginJob C-Win
ON HELP OF fiBeginJob IN FRAME FRAME-A /* Beginning Job */
DO:
        DEFINE VARIABLE cJob AS CHARACTER NO-UNDO.
        RUN pLookupJob(g_company, FOCUS:SCREEN-VALUE, OUTPUT cJob).
        fiBeginJob:SCREEN-VALUE = cJob.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBeginJob-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBeginJob-2 C-Win
ON HELP OF fiBeginJob-2 IN FRAME FRAME-A
DO:
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEndItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEndItem C-Win
ON HELP OF fiEndItem IN FRAME FRAME-A /* Ending Item */
DO:
        DEFINE VARIABLE cFGItemID AS CHARACTER NO-UNDO.
        RUN pLookupFGItem(g_company, FOCUS:SCREEN-VALUE, OUTPUT cFGItemID).
        fiEndItem:SCREEN-VALUE = cFGItemID.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEndJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEndJob C-Win
ON HELP OF fiEndJob IN FRAME FRAME-A /* Ending Job */
DO:
        DEFINE VARIABLE cJob AS CHARACTER NO-UNDO.
        RUN pLookupJob(g_company, FOCUS:SCREEN-VALUE, OUTPUT cJob).
        fiEndJob:SCREEN-VALUE = cJob.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEndJob-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEndJob-2 C-Win
ON HELP OF fiEndJob-2 IN FRAME FRAME-A
DO:

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
  DISPLAY fiBeginJob fiBeginJob-2 fiEndJob fiEndJob-2 fiBeginItem fiEndItem 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 fiBeginJob fiBeginJob-2 fiEndJob fiEndJob-2 fiBeginItem 
         fiEndItem btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLookupFGItem C-Win 
PROCEDURE pLookupFGItem PRIVATE :
/*------------------------------------------------------------------------------
     Purpose: Runs a lookup on FG Item and returns the FG Item ID if selected
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcInitial AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcReturn AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFieldsValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

    RUN system/openlookup.p (ipcCompany, "i-no", OUTPUT cFieldsValue, OUTPUT cFoundValue, OUTPUT recFoundRecID).
/*    RUN windows/l-itemf2.w (ipcCompany,"", ipcInitial, "", OUTPUT cFoundValue, OUTPUT recFoundRecID).*/
    opcReturn = cFoundValue.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLookupJob C-Win 
PROCEDURE pLookupJob PRIVATE :
/*------------------------------------------------------------------------------
     Purpose: Runs a lookup on Job and returns the Job if selected
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcInitial AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcReturn AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFieldsValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

    RUN system/openlookup.p (ipcCompany, "job-no", OUTPUT cFieldsValue, OUTPUT cFoundValue, OUTPUT recFoundRecID).
/*    RUN windows/l-itemf2.w (ipcCompany,"", ipcInitial, "", OUTPUT cFoundValue, OUTPUT recFoundRecID).*/
    opcReturn = cFoundValue.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcess C-Win 
PROCEDURE pProcess PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:  Main Processor for the Program
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cJobStart AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobEnd   AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            fiBeginJob
            fiEndJob
            fiBeginJob-2
            fiEndJob-2
            fiBeginItem
            fiEndItem
            cJobStart = TRIM(fiBeginJob)
            cJobEnd = TRIM(fiEndJob) 
            .
    END.
    IF cJobStart NE "" THEN DO:
        cJobStart = fFormatJob(cJobStart).
    END.
    IF NOT cJobEnd BEGINS "z" THEN DO:
        cJobEnd = fFormatJob(cJobEnd).
    END.
    RUN fg/FGLabelProcs.p (g_company, cJobStart, cJobEnd, fiBeginJob-2, fiEndJob-2, fiBeginItem, fiEndItem, YES, YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFormatJob C-Win 
FUNCTION fFormatJob RETURNS CHARACTER PRIVATE
  (ipcJob AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:  Test is job is valid with leading spaces.  
    If so, returns job with leading spaces.
 Notes:
------------------------------------------------------------------------------*/
                DEFINE VARIABLE cJob AS CHARACTER NO-UNDO.
                DEFINE VARIABLE iLen AS INTEGER NO-UNDO.
                
                iLen = LENGTH(TRIM(ipcJob)).
                IF iLen LT 6 THEN DO:
                  cJob = FILL(" ", 6 - iLen) + TRIM(ipcJob).
                  IF NOT CAN-FIND(FIRST job NO-LOCK WHERE job.company EQ g_company AND job.job-no EQ cJob) THEN 
                      cJob = ipcJob.
        END.
        ELSE 
            cJob = ipcJob.
                
                RETURN cJob.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

