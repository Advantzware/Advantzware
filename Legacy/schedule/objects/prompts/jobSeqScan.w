&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: jobSeqScan.w

  Description: Job Sequence Scan

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 2.22.2018 (55th Birthday)

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/ttblJob.i}
{{&includes}/specialTime.i}

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER iphContainer AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE lJobSeqScan AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE iJobYCoord  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxHeight  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxWidth   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTitle      AS CHARACTER NO-UNDO.
DEFINE VARIABLE hJob        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hToggleBox  AS HANDLE    NO-UNDO.
DEFINE VARIABLE lSave       AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE ttResource NO-UNDO
    FIELD resource AS CHARACTER
    FIELD sHTML AS LOGICAL INITIAL YES
    FIELD sToggle AS HANDLE
    FIELD pHTML AS LOGICAL INITIAL YES 
    FIELD pToggle AS HANDLE
    FIELD resourceHandle AS HANDLE
        INDEX ttResource IS PRIMARY resource
        .
DEFINE TEMP-TABLE ttJob NO-UNDO
    FIELD jobType AS CHARACTER
    FIELD resource AS CHARACTER
    FIELD jobSequence AS DECIMAL LABEL "Seq" FORMAT ">>>9"
    FIELD job AS CHARACTER LABEL "Job-# | .S | .B | .P" FORMAT "x(20)"
    FIELD dueDate AS DATE 
    FIELD startDate AS DATE
    FIELD startTime AS INTEGER
    FIELD endDate AS DATE
    FIELD endTime AS INTEGER
    FIELD timeSpan AS INTEGER
    FIELD startDateTime AS DECIMAL
    FIELD endDateTime AS DECIMAL
    FIELD jobFlag AS LOGICAL
    FIELD jobTable AS CHARACTER
    FIELD jobRowID AS ROWID
        INDEX ttJobs IS PRIMARY jobType resource jobSequence
        INDEX job job
        .
DEFINE BUFFER bttJob FOR ttJob.

{{&includes}/lockWindowUpdate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME jobs

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttJob

/* Definitions for BROWSE jobs                                          */
&Scoped-define FIELDS-IN-QUERY-jobs STRING(ttJob.jobSequence,">>>>9") ttJob.job   
&Scoped-define ENABLED-FIELDS-IN-QUERY-jobs   
&Scoped-define SELF-NAME jobs
&Scoped-define QUERY-STRING-jobs FOR EACH ttJob     WHERE ttJob.jobType EQ "S"       AND ttJob.resource EQ svResource
&Scoped-define OPEN-QUERY-jobs OPEN QUERY {&SELF-NAME} FOR EACH ttJob     WHERE ttJob.jobType EQ "S"       AND ttJob.resource EQ svResource     .
&Scoped-define TABLES-IN-QUERY-jobs ttJob
&Scoped-define FIRST-TABLE-IN-QUERY-jobs ttJob


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS svResource iHTMLColumns lScanCodes ~
lResources btnHTML btnJobSeqScan lScheduled lPending 
&Scoped-Define DISPLAYED-OBJECTS svResource iHTMLColumns lScanCodes ~
lResources pendingText lScheduled lPending 

/* Custom List Definitions                                              */
/* jobMode,List-2,List-3,List-4,List-5,List-6                           */
&Scoped-define jobMode btnSave svJob btnAdd svMode btnBottom btnClear ~
btnDelete btnDown btnTop btnUp btnFirst btnInsert btnLast btnNext ~
btnPrevious btnReset RECT-2 jobs 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fBarCode C-Win 
FUNCTION fBarCode RETURNS CHARACTER
  (ipiIndex AS INTEGER, ipcValue AS CHARACTER, ipcCharacter AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD numericDateTime C-Win 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "Graphics/32x32/navigate_plus.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Add".

DEFINE BUTTON btnBottom 
     IMAGE-UP FILE "Graphics/32x32/navigate_down2.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Move to Bottom".

DEFINE BUTTON btnClear 
     IMAGE-UP FILE "Graphics/32x32/layout.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Clear".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Delete (Return to Pending)".

DEFINE BUTTON btnDown 
     IMAGE-UP FILE "Graphics/32x32/navigate_down.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Move Down".

DEFINE BUTTON btnFirst 
     IMAGE-UP FILE "Graphics/32x32/navigate_beginning.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Navigate to First".

DEFINE BUTTON btnHTML 
     IMAGE-UP FILE "Graphics/32x32/html_tag.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Generate HTML Scan Pages".

DEFINE BUTTON btnInsert 
     IMAGE-UP FILE "Graphics/32x32/indent_increase.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Insert".

DEFINE BUTTON btnJobSeqScan 
     IMAGE-UP FILE "Graphics/32x32/barcode_scanner.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Turn Scanner Mode On/Off".

DEFINE BUTTON btnLast 
     IMAGE-UP FILE "Graphics/32x32/navigate_end.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Navigate to Last".

DEFINE BUTTON btnNext 
     IMAGE-UP FILE "Graphics/32x32/navigate_right.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Navigate to Next".

DEFINE BUTTON btnPrevious 
     IMAGE-UP FILE "Graphics/32x32/navigate_left.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Navigate to Previous".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/undo.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Reset".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Save".

DEFINE BUTTON btnTop 
     IMAGE-UP FILE "Graphics/32x32/navigate_up2.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Move to Top".

DEFINE BUTTON btnUp 
     IMAGE-UP FILE "Graphics/32x32/navigate_up.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Move Up".

DEFINE VARIABLE iHTMLColumns AS INTEGER FORMAT ">9":U INITIAL 4 
     LABEL "HTML Columns" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .95 TOOLTIP "HTML Columns" NO-UNDO.

DEFINE VARIABLE pendingText AS CHARACTER FORMAT "X(256)":U INITIAL "  Pending" 
      VIEW-AS TEXT 
     SIZE 22 BY .62
     BGCOLOR 8 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE svJob AS CHARACTER FORMAT "X(256)":U 
     LABEL "Job" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE svMode AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1
     BGCOLOR 0 FGCOLOR 14  NO-UNDO.

DEFINE VARIABLE svResource AS CHARACTER FORMAT "X(256)":U 
     LABEL "Resource" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 28.57.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 50 BY .48
     BGCOLOR 0 .

DEFINE VARIABLE lPending AS LOGICAL INITIAL yes 
     LABEL "Pending" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .81 TOOLTIP "Set ALL Pending" NO-UNDO.

DEFINE VARIABLE lResources AS LOGICAL INITIAL yes 
     LABEL "Resources" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 TOOLTIP "HTML Resource Bar Codes" NO-UNDO.

DEFINE VARIABLE lScanCodes AS LOGICAL INITIAL yes 
     LABEL "Modes" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 TOOLTIP "HTML Mode Bar Codes" NO-UNDO.

DEFINE VARIABLE lScheduled AS LOGICAL INITIAL yes 
     LABEL "Scheduled" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 TOOLTIP "Set ALL Scheduled" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY jobs FOR 
      ttJob SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE jobs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS jobs C-Win _FREEFORM
  QUERY jobs DISPLAY
      STRING(ttJob.jobSequence,">>>>9") LABEL "  Seq"
ttJob.job
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 39.4 BY 19.52
         FONT 6
         TITLE "Sequenced Jobs" ROW-HEIGHT-CHARS 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSave AT ROW 4.81 COL 42 WIDGET-ID 18
     svResource AT ROW 1.24 COL 23 COLON-ALIGNED HELP
          "Scan/Enter Resource" WIDGET-ID 4
     svJob AT ROW 2.43 COL 16 COLON-ALIGNED HELP
          "Scan/Enter Job" WIDGET-ID 40
     btnAdd AT ROW 4.81 COL 12 WIDGET-ID 12
     iHTMLColumns AT ROW 1 COL 92 COLON-ALIGNED HELP
          "Enter Number of HTML Columns" WIDGET-ID 54
     lScanCodes AT ROW 1.1 COL 101 WIDGET-ID 56
     lResources AT ROW 1.1 COL 113 WIDGET-ID 58
     svMode AT ROW 3.62 COL 2 NO-LABEL WIDGET-ID 44
     btnBottom AT ROW 23.38 COL 42 WIDGET-ID 32
     btnClear AT ROW 4.81 COL 2 WIDGET-ID 20
     btnDelete AT ROW 17.19 COL 42 WIDGET-ID 16
     btnDown AT ROW 20.29 COL 42 WIDGET-ID 34
     btnHTML AT ROW 1.24 COL 42 WIDGET-ID 46
     btnTop AT ROW 11 COL 42 WIDGET-ID 30
     btnUp AT ROW 14.1 COL 42 WIDGET-ID 36
     btnFirst AT ROW 7.67 COL 2 WIDGET-ID 26
     btnInsert AT ROW 4.81 COL 22 WIDGET-ID 14
     btnJobSeqScan AT ROW 1.24 COL 2 WIDGET-ID 2
     btnLast AT ROW 7.67 COL 32 WIDGET-ID 24
     btnNext AT ROW 7.67 COL 22 WIDGET-ID 28
     btnPrevious AT ROW 7.67 COL 12 WIDGET-ID 22
     btnReset AT ROW 4.81 COL 32 WIDGET-ID 38
     pendingText AT ROW 6.14 COL 52 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     lScheduled AT ROW 1.1 COL 130 WIDGET-ID 62
     lPending AT ROW 1.1 COL 147 WIDGET-ID 64
     jobs AT ROW 10.05 COL 2 WIDGET-ID 200
     "  Resources" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 1.24 COL 54 WIDGET-ID 48
          BGCOLOR 8 FGCOLOR 1 
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 42
     RECT-2 AT ROW 7.19 COL 2 WIDGET-ID 52
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FONT 6 WIDGET-ID 100.


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
         TITLE              = "Job Sequence Scan"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB jobs RECT-2 DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnAdd IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnAdd:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnBottom IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnBottom:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnClear IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnClear:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnDelete IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnDelete:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnDown IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnDown:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnFirst IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnFirst:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnInsert IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnInsert:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnLast IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnLast:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnNext IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnNext:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnPrevious IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnPrevious:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnReset IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnReset:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnSave IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnSave:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnTop IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnTop:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnUp IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnUp:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BROWSE jobs IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       jobs:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR FILL-IN pendingText IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       RECT-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN svJob IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
       svJob:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN svMode IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L 1                                       */
ASSIGN 
       svMode:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       svMode:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE jobs
/* Query rebuild information for BROWSE jobs
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttJob
    WHERE ttJob.jobType EQ "S"
      AND ttJob.resource EQ svResource
    .
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE jobs */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Job Sequence Scan */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Job Sequence Scan */
DO:
  /* This event will close the window and terminate the procedure.  */
  IF lSave THEN DO:
      MESSAGE
          "Unsaved Changes Exist! SAVE Changes?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
      UPDATE lSaveChanges AS LOGICAL.
      IF lSaveChanges EQ YES THEN
      APPLY "CHOOSE":U TO btnSave IN FRAME {&FRAME-NAME}.
      ELSE IF lSaveChanges EQ ? THEN
      RETURN NO-APPLY.      
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME DEFAULT-FRAME
DO:
    svJob:SCREEN-VALUE = "Mode:Add".
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBottom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBottom C-Win
ON CHOOSE OF btnBottom IN FRAME DEFAULT-FRAME
DO:
    svJob:SCREEN-VALUE = "Mode:Bottom".
    RUN moveJob (99999).
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClear C-Win
ON CHOOSE OF btnClear IN FRAME DEFAULT-FRAME
DO:
    svJob:SCREEN-VALUE = "Mode:Clear".
    RUN moveToPending (?).
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME
DO:
    svJob:SCREEN-VALUE = "Mode:Delete".
    IF AVAILABLE ttJob THEN
    RUN moveToPending (ROWID(ttJob)).
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDown C-Win
ON CHOOSE OF btnDown IN FRAME DEFAULT-FRAME
DO:
    svJob:SCREEN-VALUE = "Mode:Down".
    RUN moveJob (1).
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst C-Win
ON CHOOSE OF btnFirst IN FRAME DEFAULT-FRAME
DO:
    svJob:SCREEN-VALUE = "Mode:First".
    RUN navigateJob ("First").
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHTML C-Win
ON CHOOSE OF btnHTML IN FRAME DEFAULT-FRAME
DO:
    svJob:SCREEN-VALUE = "Mode:HTML".
    RUN pHTMLPages.
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnInsert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnInsert C-Win
ON CHOOSE OF btnInsert IN FRAME DEFAULT-FRAME
DO:
    svJob:SCREEN-VALUE = "Mode:Insert".
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnJobSeqScan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJobSeqScan C-Win
ON CHOOSE OF btnJobSeqScan IN FRAME DEFAULT-FRAME
DO:
    lJobSeqScan = NOT lJobSeqScan.
    SELF:LOAD-IMAGE("Graphics\32x32\barcode_scanner" + TRIM(STRING(lJobSeqScan,"/_not")) + ".ico").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast C-Win
ON CHOOSE OF btnLast IN FRAME DEFAULT-FRAME
DO:
    svJob:SCREEN-VALUE = "Mode:Last".
    RUN navigateJob ("Last").
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext C-Win
ON CHOOSE OF btnNext IN FRAME DEFAULT-FRAME
DO:
    svJob:SCREEN-VALUE = "Mode:Next".
    RUN navigateJob ("Next").
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevious C-Win
ON CHOOSE OF btnPrevious IN FRAME DEFAULT-FRAME
DO:
    svJob:SCREEN-VALUE = "Mode:Previous".
    RUN navigateJob ("Previous").
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME DEFAULT-FRAME
DO:
    svJob:SCREEN-VALUE = "Mode:Reset".
    RUN createttJob.
    RUN getResources.
    RUN getJobs (svResource).
    lSave = NO.
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME
DO:
    svJob:SCREEN-VALUE = "Mode:Save".
    RUN saveJobs.
    IF VALID-HANDLE(iphContainer) THEN 
    RUN buildBoard IN iphContainer (NO).
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTop C-Win
ON CHOOSE OF btnTop IN FRAME DEFAULT-FRAME
DO:
    svJob:SCREEN-VALUE = "Mode:Top".
    RUN moveJob (0).
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUp C-Win
ON CHOOSE OF btnUp IN FRAME DEFAULT-FRAME
DO:
    svJob:SCREEN-VALUE = "Mode:Up".
    RUN moveJob (-1).
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iHTMLColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iHTMLColumns C-Win
ON VALUE-CHANGED OF iHTMLColumns IN FRAME DEFAULT-FRAME /* HTML Columns */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME jobs
&Scoped-define SELF-NAME jobs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobs C-Win
ON DEFAULT-ACTION OF jobs IN FRAME DEFAULT-FRAME /* Sequenced Jobs */
DO:
    APPLY "CHOOSE":U TO btnDelete.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobs C-Win
ON VALUE-CHANGED OF jobs IN FRAME DEFAULT-FRAME /* Sequenced Jobs */
DO:
    svJob:SCREEN-VALUE = "Mode:Browser".
    APPLY "LEAVE":U TO svJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lPending
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lPending C-Win
ON VALUE-CHANGED OF lPending IN FRAME DEFAULT-FRAME /* Pending */
DO:
    ASSIGN {&SELF-NAME}.
    FOR EACH ttResource:
        ASSIGN
            ttResource.pHTML = SELF:SCREEN-VALUE EQ "YES"
            hToggleBox = ttResource.pToggle
            hToggleBox:SCREEN-VALUE = STRING(ttResource.pHTML)
            .
    END. /* each ttresource */    
    APPLY "ENTRY":U TO svResource.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lResources
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lResources C-Win
ON VALUE-CHANGED OF lResources IN FRAME DEFAULT-FRAME /* Resources */
DO:
    ASSIGN {&SELF-NAME}.
    APPLY "ENTRY":U TO svResource.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lScanCodes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lScanCodes C-Win
ON VALUE-CHANGED OF lScanCodes IN FRAME DEFAULT-FRAME /* Modes */
DO:
    ASSIGN {&SELF-NAME}.
    APPLY "ENTRY":U TO svResource.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lScheduled
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lScheduled C-Win
ON VALUE-CHANGED OF lScheduled IN FRAME DEFAULT-FRAME /* Scheduled */
DO:
    ASSIGN {&SELF-NAME}.
    FOR EACH ttResource:
        ASSIGN
            ttResource.sHTML = SELF:SCREEN-VALUE EQ "YES"
            hToggleBox = ttResource.sToggle
            hToggleBox:SCREEN-VALUE = STRING(ttResource.sHTML)
            .
    END. /* each ttresource */
    APPLY "ENTRY":U TO svResource.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svJob C-Win
ON LEAVE OF svJob IN FRAME DEFAULT-FRAME /* Job */
DO:
    ASSIGN {&SELF-NAME}.
    RUN processScan.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svJob C-Win
ON RETURN OF svJob IN FRAME DEFAULT-FRAME /* Job */
DO:
    APPLY "LEAVE":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svMode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svMode C-Win
ON ENTRY OF svMode IN FRAME DEFAULT-FRAME
DO:
    APPLY "ENTRY":U TO svJob.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svResource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svResource C-Win
ON ENTRY OF svResource IN FRAME DEFAULT-FRAME /* Resource */
DO:
    IF SELF:SCREEN-VALUE NE "" THEN DO:
        APPLY "ENTRY":U TO svJob.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svResource C-Win
ON LEAVE OF svResource IN FRAME DEFAULT-FRAME /* Resource */
DO:
    ASSIGN {&SELF-NAME}
        {&SELF-NAME} = REPLACE({&SELF-NAME},"-","")
        SELF:SCREEN-VALUE = {&SELF-NAME}
        .
    FIND FIRST mach NO-LOCK
         WHERE mach.company EQ ipcCompany
           AND mach.m-code  EQ {&SELF-NAME}
         NO-ERROR.
    IF AVAILABLE mach THEN DO:
        ASSIGN
            {&WINDOW-NAME}:TITLE = cTitle + " - Resource: " + mach.m-dscr
            svResource:READ-ONLY = YES
            .
        RUN getJobs ({&SELF-NAME}).
        ENABLE {&jobMode} WITH FRAME {&FRAME-NAME}.
        APPLY "ENTRY":U TO svJob.
        RETURN NO-APPLY.
    END. /* if avail */
    ELSE DO:
        IF {&SELF-NAME} NE "" THEN
        MESSAGE
            "Invalid Resource, Please Try Again..."
        VIEW-AS ALERT-BOX ERROR.
        SELF:SET-SELECTION(1,LENGTH(SELF:SCREEN-VALUE) + 1).
        APPLY "ENTRY":U TO SELF.
    END. /* else */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svResource C-Win
ON RETURN OF svResource IN FRAME DEFAULT-FRAME /* Resource */
DO:
    APPLY "LEAVE":U TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

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
  ASSIGN
      cTitle = {&WINDOW-NAME}:TITLE
      pendingText = "  Pending Jobs"
      .
  RUN winReSize.
  RUN enable_UI.
  RUN createttJob.
  RUN getResources.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clickButton C-Win 
PROCEDURE clickButton :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        IF iphWidget:PRIVATE-DATA EQ "Resource" THEN DO:
            ASSIGN
                svResource = iphWidget:LABEL
                svResource:SCREEN-VALUE = svResource
                svJob = ""
                svJob:SCREEN-VALUE = svJob
                svMode = ""
                svMode:SCREEN-VALUE = svMode
                hJob = ?
                .
            APPLY "LEAVE":U TO svResource.
            APPLY "ENTRY":U TO svJob.
        END. /* if resource */
        ELSE DO:
            ASSIGN
                svJob:SCREEN-VALUE = iphWidget:LABEL
                hJob = iphWidget
                .
            APPLY "LEAVE":U TO svJob.
        END. /* else */
    END. /* frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clickToggle C-Win 
PROCEDURE clickToggle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphToggle AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iprRowID  AS ROWID  NO-UNDO.
    
    FIND FIRST ttResource
         WHERE ROWID(ttResource) EQ iprRowID
         NO-ERROR.
    IF NOT AVAILABLE ttResource THEN RETURN.
    IF iphToggle:LABEL EQ "Scheduled" THEN
    ttResource.sHTML = iphToggle:SCREEN-VALUE EQ "YES".
    ELSE /* pending */
    ttResource.pHTML = iphToggle:SCREEN-VALUE EQ "YES".    
    APPLY "ENTRY":U TO svResource IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createButton C-Win 
PROCEDURE createButton :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPool     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcResource AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiXCoord   AS INTEGER  NO-UNDO.
    DEFINE INPUT PARAMETER ipiYCoord   AS INTEGER  NO-UNDO.
    DEFINE INPUT PARAMETER iprRowID    AS ROWID    NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hToggle AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cType   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iYCoord AS INTEGER   NO-UNDO.
  
    CREATE BUTTON hWidget IN WIDGET-POOL ipcPool
        ASSIGN
          FRAME = FRAME {&FRAME-NAME}:HANDLE
          SENSITIVE = YES
          HIDDEN = NO
          X = ipiXCoord
          Y = ipiYCoord
          WIDTH-PIXELS = 110
          HEIGHT-PIXELS = 48
          LABEL = ipcResource
          NAME = ipcResource
          PRIVATE-DATA = IF ipcType EQ "Resource" THEN ipcType
                         ELSE STRING(iprRowID)
    TRIGGERS:
      ON CHOOSE
         PERSISTENT RUN clickButton IN THIS-PROCEDURE (hWidget:HANDLE).
    END TRIGGERS.
    hWidget:MOVE-TO-TOP().

    IF ipcType EQ "Resource" THEN DO:
        CREATE ttResource.
        ASSIGN
            ttResource.resource = ipcResource
            ttResource.resourceHandle = hWidget
            iYCoord = ipiYCoord + hWidget:HEIGHT-PIXELS + 2
            .
        DO idx = 1 TO 2:
            cType = IF idx EQ 1 THEN "Scheduled" ELSE "Pending".
            CREATE TOGGLE-BOX hToggle IN WIDGET-POOL ipcPool
                ASSIGN
                    FRAME = FRAME {&FRAME-NAME}:HANDLE
                    SENSITIVE = YES
                    HIDDEN = NO
                    X = ipiXCoord
                    Y = iYCoord
                    WIDTH-PIXELS = 80
                    HEIGHT-PIXELS = 17
                    LABEL = cType
                    NAME = ipcResource + cType
                    SCREEN-VALUE = "YES"
            TRIGGERS:
              ON VALUE-CHANGED
                 PERSISTENT RUN clickToggle IN THIS-PROCEDURE (hToggle:HANDLE, ROWID(ttResource)).
            END TRIGGERS.
            iYCoord = iYCoord + hToggle:HEIGHT-PIXELS + 2.
            IF idx EQ 1 THEN
            ttResource.sToggle = hToggle.
            ELSE
            ttResource.pToggle = hToggle.
        END. /* do idx */
    END. /* type resource */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createttJob C-Win 
PROCEDURE createttJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttJob.
    EMPTY TEMP-TABLE ttResource.

    FOR EACH ttblJob:
        CREATE ttJob.
        ASSIGN
            ttJob.jobType = "S"
            ttJob.resource = ttblJob.resource
            ttJob.jobSequence = ttblJob.jobSequence
            ttJob.job = ttblJob.job + "."
                      + STRING(INTEGER(ttblJob.userField19)) + "."
                      + STRING(INTEGER(ttblJob.userField20))
            ttJob.dueDate = ttblJob.dueDate
            ttJob.startDate = ttblJob.startDate
            ttJob.startTime = ttblJob.startTime
            ttJob.endDate = ttblJob.endDate
            ttJob.endTime = ttblJob.endTime
            ttJob.timeSpan = ttblJob.timeSpan
            ttJob.startDateTime = ttblJob.startDateTime
            ttJob.endDateTime = ttblJob.endDateTime
            ttJob.jobTable = "Job"
            ttJob.jobRowID = ROWID(ttblJob)
            .
    END. /* each ttblJob */

    FOR EACH pendingJob:
        CREATE ttJob.
        ASSIGN
            ttJob.jobType = "P"
            ttJob.resource = pendingJob.resource
            ttJob.jobSequence = pendingJob.jobSequence
            ttJob.job = pendingJob.job + "."
                      + STRING(INTEGER(pendingJob.userField19)) + "."
                      + STRING(INTEGER(pendingJob.userField20))
            ttJob.dueDate = pendingJob.dueDate
            ttJob.startDate = pendingJob.startDate
            ttJob.startTime = pendingJob.startTime
            ttJob.endDate = pendingJob.endDate
            ttJob.endTime = pendingJob.endTime
            ttJob.timeSpan = pendingJob.timeSpan
            ttJob.startDateTime = pendingJob.startDateTime
            ttJob.endDateTime = pendingJob.endDateTime
            ttJob.jobTable = "Pending"
            ttJob.jobRowID = ROWID(pendingJob)
            .
    END. /* each pendingJob */

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
  DISPLAY svResource iHTMLColumns lScanCodes lResources pendingText lScheduled 
          lPending 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE svResource iHTMLColumns lScanCodes lResources btnHTML btnJobSeqScan 
         lScheduled lPending 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getJobs C-Win 
PROCEDURE getJobs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcResource AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cJob    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iXCoord AS INTEGER   NO-UNDO INITIAL 265. /*265,505*/
    DEFINE VARIABLE iYCoord AS INTEGER   NO-UNDO INITIAL 20.
    DEFINE VARIABLE i       AS INTEGER   NO-UNDO.
    
    SESSION:SET-WAIT-STATE("General").
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).

    DELETE WIDGET-POOL "jobScanPool" NO-ERROR.
    CREATE WIDGET-POOL "jobScanPool" PERSISTENT.
    
    ASSIGN
        pendingText:Y IN FRAME {&FRAME-NAME} = iJobYCoord
        iYCoord = iJobYCoord + pendingText:HEIGHT-PIXELS + 2
        .
    FOR EACH ttJob
        WHERE ttJob.jobType EQ "P"
          AND ttJob.resource EQ ipcResource
        BY ttJob.job
        :
        RUN createButton ("jobScanPool","Job", ttJob.job,iXCoord,iYCoord,ROWID(ttJob)).
        iYCoord = iYCoord + 53.
        IF iYCoord + 48 GT iMaxHeight THEN
        ASSIGN
            iXCoord = iXCoord + 115
            iYCoord = iJobYCoord + pendingText:HEIGHT-PIXELS + 2
            .
        IF iXCoord + 110 GT iMaxWidth THEN
        LEAVE.
    END. /* each ttJob */
    {&OPEN-QUERY-{&BROWSE-NAME}}

    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getResources C-Win 
PROCEDURE getResources :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cResource AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iXCoord   AS INTEGER   NO-UNDO INITIAL 265.
    DEFINE VARIABLE iYCoord   AS INTEGER   NO-UNDO INITIAL 20.
    DEFINE VARIABLE i         AS INTEGER   NO-UNDO.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).

    DELETE WIDGET-POOL "resScanPool" NO-ERROR.
    CREATE WIDGET-POOL "resScanPool" PERSISTENT.
    
    INPUT FROM VALUE("{&data}/" + ID + "/resourceList.dat").
    IMPORT ^.
    REPEAT:
        IMPORT cResource.
        RUN createButton ("resScanPool","Resource",cResource,iXCoord,iYCoord,?).
        iXCoord = iXCoord + 115.
        IF iXCoord + 110 GT iMaxWidth THEN
        ASSIGN
            iYCoord = iYCoord + 93
            iXCoord = 265
            .
    END. /* repeat */
    INPUT CLOSE.
    ASSIGN
        iJobYCoord = iYCoord + 90
        pendingText:Y IN FRAME {&FRAME-NAME} = iJobYCoord
        .

    RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveJob C-Win 
PROCEDURE moveJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiMove AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE rRowID AS ROWID NO-UNDO.

    IF NOT AVAILABLE ttJob THEN RETURN.
    
    CASE ipiMove:
        WHEN 0 OR WHEN 99999 THEN DO:
            ASSIGN
                ttJob.jobSequence = ipiMove
                rRowID            = ROWID(ttJob)
                .
            RUN sequenceJobs (svResource).
        END. /* top or bottom */
        OTHERWISE DO:
            /* already at top */
            IF ipiMove EQ -1 AND ttJob.jobSequence EQ 1 THEN RETURN.
            IF ipiMove EQ 1 THEN DO:
                FIND LAST bttJob
                     WHERE bttJob.jobType EQ "S"
                       AND bttJob.resource EQ svResource
                     NO-ERROR.
                IF NOT AVAILABLE bttJob THEN RETURN.
                /* already at bottom */
                IF bttJob.jobSequence EQ ttJob.jobSequence THEN RETURN.
            END. /* eq 1 */
            FIND FIRST bttJob
                 WHERE bttJob.jobType EQ "S"
                   AND bttJob.resource EQ svResource
                   AND bttJob.jobSequence EQ ttJob.jobSequence + ipiMove
                 NO-ERROR.
            IF NOT AVAILABLE bttJob THEN RETURN.
            ASSIGN
                bttJob.jobSequence = ttJob.jobSequence
                ttJob.jobSequence  = ttJob.jobSequence + ipiMove
                rRowID             = ROWID(ttJob)
                .
            RUN sequenceJobs (svResource).
        END. /* otherwise */
    END CASE.
    {&OPEN-QUERY-{&BROWSE-NAME}}
    REPOSITION {&BROWSE-NAME} TO ROWID(rRowID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveToPending C-Win 
PROCEDURE moveToPending :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.
    
    IF iprRowID EQ ? THEN
    FOR EACH ttJob
        WHERE ttJob.jobType EQ "S"
          AND ttJob.resource EQ svResource
        :
        ASSIGN
            ttJob.jobType = "P"
            ttJob.jobSequence = 0
            ttJob.startDate = ?
            ttJob.startTime = 0
            ttJob.endDate = ?
            ttJOb.endTime = 0
            .
    END. /* each ttJob */
    ELSE DO:
        ASSIGN
            ttJob.jobType = "P"
            ttJob.jobSequence = 0
            ttJob.startDate = ?
            ttJob.startTime = 0
            ttJob.endDate = ?
            ttJOb.endTime = 0
            .
        RUN sequenceJobs (svResource).
    END. /* else */
    lSave = YES.
    RUN getJobs (svResource).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE navigateJob C-Win 
PROCEDURE navigateJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE rRowID AS ROWID NO-UNDO.
    
    CASE ipcType:
        WHEN "First" THEN
        GET FIRST {&BROWSE-NAME}.
        WHEN "Previous" THEN
        GET PREV {&BROWSE-NAME}.
        WHEN "Next" THEN
        GET NEXT {&BROWSE-NAME}.
        WHEN "Last" THEN
        GET LAST {&BROWSE-NAME}.
    END CASE.
    IF NOT AVAILABLE ttJob THEN RETURN.
    rRowID = ROWID(ttJob).
    REPOSITION {&BROWSE-NAME} TO ROWID(rRowID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newEnd C-Win 
PROCEDURE newEnd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{{&includes}/{&Board}/newEnd.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHTMLBranding C-Win 
PROCEDURE pHTMLBranding :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    PUT UNFORMATTED
        '  <img src="' SEARCH("Graphics/asiicon.ico")
        '" align="middle">~&nbsp;<b><a href="http://www.advantzware.com" target="_blank">'
        '<font face="{&fontFace}">Advantzware, Inc.</a>~&nbsp;~&copy;</b></font>' SKIP
        . 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHTMLFooter C-Win 
PROCEDURE pHTMLFooter :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    PUT UNFORMATTED
        '  </table>' SKIP
        '  <div align="left"><font face="{&fontFace}"><a href="#Top">Top</a></font>' SKIP
        '  <div align="right"><font face="{&fontFace}">~&copy; Advantzware, Inc., All Rights Reserved</font></div>' SKIP
        '</fieldset>' SKIP
        '</form>' SKIP
        '</html>' SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHTMLHeader C-Win 
PROCEDURE pHTMLHeader :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcHTMLTitle  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcHTMLLegend AS CHARACTER NO-UNDO.
    
    PUT UNFORMATTED
        '<html>' SKIP
        '<head>' SKIP
        '  <title>' ipcHTMLTitle '</title>' SKIP
        '  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' SKIP
        '  <meta http-equiv="Refresh" content="120">' SKIP
        '  <script type="text/javascript" src="' SEARCH("schedule\JsBarcode.code128.min.js") '"></script>' SKIP 
        '</head>' SKIP
        '<a name="Top"></a>' SKIP
        '<form>' SKIP
        '<fieldset>' SKIP
        '  <legend><font face="{&fontFace}">' ipcHTMLLegend ' (updated '
        STRING(TODAY,'99.99.9999') ' @ ' STRING(TIME,'hh:mm:ss am') ')</font>'
        '~&nbsp;</legend>' SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHTMLPages C-Win 
PROCEDURE pHTMLPages :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &Scoped-define fontFace Arial, Helvetica, sans-serif
    
    DEFINE VARIABLE cHTMLTitle  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHTMLLegend AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHTMLFile   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE kdx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cHeader     AS CHARACTER NO-UNDO INITIAL "Mode,Navigation,Update".
    DEFINE VARIABLE cOption     AS CHARACTER NO-UNDO EXTENT 3 initial
        ["CLEAR,ADD,INSERT,RESET,SAVE","FIRST,PREVIOUS,NEXT,LAST,HTML","TOP,UP,DELETE,DOWN,BOTTOM"].

    IF lScanCodes THEN 
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            cHTMLTitle  = "SB Scan Modes"
            cHTMLLegend = "Mode Scan Codes"
            cHTMLFile   = "c:\tmp\scanCodes.htm" 
            .
        OUTPUT TO VALUE(cHTMLFile).
        RUN pHTMLHeader (cHTMLTitle,cHTMLLegend).
        RUN pHTMLBranding.
        PUT UNFORMATTED
            '  <table border="1" cellspacing="0" cellpadding="12" width="80%">' SKIP
            '    <tr>' SKIP. 
        DO idx = 1 TO NUM-ENTRIES(cHeader):
            PUT UNFORMATTED
                '      <td align="center" nowrap><font face="{&fontFace}"><b>'
                ENTRY(idx,cHeader) '</b></font></td>' SKIP.
        END. /* do idx */
        PUT UNFORMATTED '    </tr>' SKIP.
        DO idx = 1 TO NUM-ENTRIES(cOption[1]):
            PUT UNFORMATTED '    <tr>' SKIP.
            DO jdx = 1 TO NUM-ENTRIES(cHeader):
                kdx = kdx + 1.
                PUT UNFORMATTED
                    '      <td align="center" nowrap>' SKIP
                    fBarCode(kdx,ENTRY(idx,cOption[jdx]),"+") SKIP
                    '      </td>' SKIP.
            END. /* do jdx */
            PUT UNFORMATTED '    </tr>' SKIP.
        END. /* do idx */
        PUT UNFORMATTED '  </table>' SKIP.
        RUN pHTMLFooter.
        OUTPUT CLOSE.
        OS-COMMAND NO-WAIT START VALUE(cHTMLFile).
    END. /* with frame */

    IF lResources THEN DO: 
        ASSIGN 
            cHTMLTitle  = "SB Resources"
            cHTMLLegend = "Resource Scan Codes"
            cHTMLFile   = "c:\tmp\scanResources.htm"
            idx         = 0
            jdx         = 0
            kdx         = 0
            .
        OUTPUT TO VALUE(cHTMLFile).
        RUN pHTMLHeader (cHTMLTitle,cHTMLLegend).
        RUN pHTMLBranding.
        PUT UNFORMATTED
            '  <table border="1" cellspacing="0" cellpadding="12" width="100%">' SKIP
            .
        FOR EACH ttResource:
            FIND FIRST mach NO-LOCK
                 WHERE mach.company EQ ipcCompany
                   AND mach.m-code  EQ ttResource.resource
                 NO-ERROR.
            IF idx EQ 0 THEN
            PUT UNFORMATTED '    <tr>' SKIP.
            kdx = kdx + 1.
            PUT UNFORMATTED
                '      <td align="center" nowrap><font face="{&fontFace}">'
                (IF AVAILABLE mach THEN mach.m-dscr ELSE ttResource.resource) '</font><br><br>' SKIP 
                fBarCode(kdx,CAPS(ttResource.resource),"-") SKIP 
                '</td>' SKIP.
            idx = idx + 1.
            IF idx EQ iHTMLColumns THEN DO:
                PUT UNFORMATTED '    </tr>' SKIP.
                idx = 0.
            END. /* if idx */
        END. /* each ttresource */
        PUT UNFORMATTED '  </table>' SKIP.
        RUN pHTMLFooter.
        OUTPUT CLOSE.
        OS-COMMAND NO-WAIT START VALUE(cHTMLFile).
    END. /* if lresources */
    
    FOR EACH ttResource:
        FIND FIRST mach NO-LOCK
             WHERE mach.company EQ ipcCompany
               AND mach.m-code  EQ ttResource.resource
             NO-ERROR.
        IF ttResource.sHTML AND
           CAN-FIND(FIRST ttJob
                    WHERE ttJob.jobType EQ "S"
                      AND ttJob.resource EQ ttResource.resource) THEN DO: 
            ASSIGN 
                cHTMLTitle  = ttResource.resource + " Jobs"
                cHTMLLegend = "Jobs for " + ttResource.resource
                            + IF AVAILABLE mach THEN " - " + mach.m-dscr ELSE ""
                cHTMLFile   = "c:\tmp\scan" + ttResource.resource + ".Jobs.htm"
                idx         = 0
                jdx         = 0
                kdx         = 0
                .
            OUTPUT TO VALUE(cHTMLFile).
            RUN pHTMLHeader (cHTMLTitle,cHTMLLegend).
            RUN pHTMLBranding.
            PUT UNFORMATTED
                '  <table border="1" cellspacing="0" cellpadding="5">' SKIP
                .
            FOR EACH ttJob
                WHERE ttJob.jobType EQ "S"
                  AND ttJob.resource EQ ttResource.resource
                BY ttJob.jobSequence
                :
                IF idx EQ 0 THEN
                PUT UNFORMATTED '    <tr>' SKIP.
                ASSIGN 
                    kdx = kdx + 1
                    idx = idx + 1.
                PUT UNFORMATTED
                    '      <td nowrap><font face="{&fontFace}">Seq. <u>' ttJob.jobSequence
                    '</u>~&nbsp;~&nbsp;~&nbsp;~&nbsp;~&nbsp;~&nbsp;~&nbsp;Due Date: <u>'
                    STRING(ttJob.dueDate,"99/99/9999") '</u></font><br><br>' SKIP
                    fBarCode(kdx,ttJob.job,"") SKIP
                    '</td>' SKIP.
                IF idx EQ iHTMLColumns THEN DO:
                    PUT UNFORMATTED '    </tr>' SKIP.
                    idx = 0.
                END. /* if idx */
            END. /* each ttjob */
            PUT UNFORMATTED '  </table>' SKIP.
            RUN pHTMLFooter.
            OUTPUT CLOSE.
            OS-COMMAND NO-WAIT START VALUE(cHTMLFile).
        END. /* if shtml */
        
        IF ttResource.pHTML AND
           CAN-FIND(FIRST ttJob
                    WHERE ttJob.jobType EQ "P"
                      AND ttJob.resource EQ ttResource.resource) THEN DO: 
            ASSIGN 
                cHTMLTitle  = ttResource.resource + " Pending"
                cHTMLLegend = "Pending for " + ttResource.resource
                            + IF AVAILABLE mach THEN " - " + mach.m-dscr ELSE ""
                cHTMLFile   = "c:\tmp\scan" + ttResource.resource + ".Pending.htm"
                idx         = 0
                jdx         = 0
                kdx         = 0
                .
            OUTPUT TO VALUE(cHTMLFile).
            RUN pHTMLHeader (cHTMLTitle,cHTMLLegend).
            RUN pHTMLBranding.
            PUT UNFORMATTED
                '  <table border="1" cellspacing="0" cellpadding="5">' SKIP
                .
            FOR EACH ttJob
                WHERE ttJob.jobType EQ "P"
                  AND ttJob.resource EQ ttResource.resource
                BY ttJob.jobSequence
                :
                IF idx EQ 0 THEN
                PUT UNFORMATTED '    <tr>' SKIP.
                ASSIGN 
                    kdx = kdx + 1
                    idx = idx + 1.
                PUT UNFORMATTED
                    '      <td nowrap><font face="{&fontFace}">Due Date: <u>'
                    STRING(ttJob.dueDate,"99/99/9999") '</u></font><br><br>' SKIP
                    fBarCode(kdx,ttJob.job,"") SKIP
                    '</td>' SKIP.
                IF idx EQ iHTMLColumns THEN DO:
                    PUT UNFORMATTED '    </tr>' SKIP.
                    idx = 0.
                END. /* if idx */
            END. /* each ttjob */
            PUT UNFORMATTED '  </table>' SKIP.
            RUN pHTMLFooter.
            OUTPUT CLOSE.
            OS-COMMAND NO-WAIT START VALUE(cHTMLFile).
        END. /* if phtml */        
    END. /* each ttresource */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processScan C-Win 
PROCEDURE processScan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWidget AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cType   AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        cType = SUBSTR(svJob,1,1).
        CASE cType:
            WHEN "-" THEN DO:
                FIND FIRST ttResource
                     WHERE ttResource.resource EQ SUBSTR(svJob,2)
                     NO-ERROR.
                IF NOT AVAILABLE ttResource THEN RETURN.
                RUN clickButton (ttResource.resourceHandle).
            END. /* resource */
            WHEN "+" THEN DO:
                CASE SUBSTR(svJob,2):
                    WHEN "Add" THEN
                    APPLY "CHOOSE":U TO btnAdd.
                    WHEN "Bottom" THEN
                    APPLY "CHOOSE":U TO btnBottom.
                    WHEN "Clear" THEN
                    APPLY "CHOOSE":U TO btnClear.
                    WHEN "Delete" THEN
                    APPLY "CHOOSE":U TO btnDelete.
                    WHEN "Down" THEN
                    APPLY "CHOOSE":U TO btnDown.
                    WHEN "First" THEN
                    APPLY "CHOOSE":U TO btnFirst.
                    WHEN "HTML" THEN
                    APPLY "CHOOSE":U TO btnHTML.
                    WHEN "Insert" THEN
                    APPLY "CHOOSE":U TO btnInsert.
                    WHEN "Last" THEN
                    APPLY "CHOOSE":U TO btnLast.
                    WHEN "Next" THEN
                    APPLY "CHOOSE":U TO btnNext.
                    WHEN "Previous" THEN
                    APPLY "CHOOSE":U TO btnPrevious.
                    WHEN "Reset" THEN
                    APPLY "CHOOSE":U TO btnReset.
                    WHEN "Save" THEN
                    APPLY "CHOOSE":U TO btnSave.
                    WHEN "Top" THEN
                    APPLY "CHOOSE":U TO btnTop.
                    WHEN "Up" THEN
                    APPLY "CHOOSE":U TO btnUp.
                END CASE.
                svJob = REPLACE(svJob,"+","Mode:").
            END. /* scan */
        END CASE.
        IF svJob NE "" THEN DO:
            IF NOT CAN-DO("Add,Insert",svMode) THEN DO:
                IF CAN-DO(",Clear,Delete,HTML,Reset,Save",svMode) THEN
                svMode = "Add".
                ELSE svMode = "Insert".
                svMode:SCREEN-VALUE = FILL(" ",25) + "Mode: " + CAPS(svMode).
            END. /* if not add and not insert */
            IF ENTRY(1,svJob,":") EQ "Mode" THEN DO:
                ASSIGN
                    svMode = ENTRY(2,svJob,":")
                    svMode:SCREEN-VALUE = FILL(" ",25) + "Mode: " + CAPS(svMode)
                    svJob = ""
                    svJob:SCREEN-VALUE = svJob
                    .
            END. /* if mode */
            ELSE DO:
                CASE svMode:
                    WHEN "Add" THEN
                    RUN scheduleJob ("Add").
                    WHEN "Insert" THEN
                    RUN scheduleJob ("Insert").
                    OTHERWISE
                    ASSIGN
                        svMode = ""
                        svMode:SCREEN-VALUE = svMode
                        .
                END CASE.
                ASSIGN
                    svJob = ""
                    svJob:SCREEN-VALUE = svJob
                    hJob = ?
                    .
            END. /* else */
            APPLY "ENTRY":U TO svJob.
        END. /* not blank */
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveJobs C-Win 
PROCEDURE saveJobs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ttJob:
        IF ttJob.jobTable EQ "Job" THEN DO:
            FIND FIRST ttblJob
                 WHERE ROWID(ttblJob) EQ ttJob.jobRowID
                 NO-ERROR.
            IF NOT AVAILABLE ttblJob THEN NEXT.
            IF ttJob.jobType EQ "P" THEN DO:
                CREATE pendingJob.
                BUFFER-COPY ttblJob TO pendingJob
                    ASSIGN pendingJob.origStartTime = ttblJob.timeSpan.
                DELETE ttblJob.
            END. /* jobtype P */
        END. /* jobtable job */
        ELSE IF ttJob.jobType EQ "S" THEN DO:
                FIND FIRST pendingJob
                     WHERE ROWID(pendingJob) EQ ttJob.jobRowID
                     NO-ERROR.
                IF NOT AVAILABLE pendingJob THEN NEXT.
                CREATE ttblJob.
                BUFFER-COPY pendingJob TO ttblJob.
                DELETE pendingJob.
        END. /* else if jobtype S */
        IF AVAILABLE ttblJob THEN
        ASSIGN
            ttblJob.jobSequence = ttJob.jobSequence
            ttblJob.startDate = ttJob.startDate
            ttblJob.startTime = ttJob.startTime
            ttblJob.endDate = ttJob.endDate
            ttblJob.endTime = ttJob.endTime
            ttblJob.startDateTime = ttblJob.startDateTime
            ttblJob.endDateTime = ttblJob.endDateTime
            .
        RELEASE ttblJob.
    END. /* each ttJob */
    lSave = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE scheduleJob C-Win 
PROCEDURE scheduleJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE rRowID        AS ROWID     NO-UNDO.
    DEFINE VARIABLE iSeq          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cResourceList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResource     AS CHARACTER NO-UNDO.
    
    IF ipcType EQ "Insert" THEN DO:
        IF NOT AVAILABLE ttJob THEN RETURN.
        iSeq = ttJob.jobSequence - .1.
    END. /* insert */
    ELSE iSeq = 99999.
    
    IF VALID-HANDLE(hJob) THEN DO:
        rRowID = TO-ROWID(hJob:PRIVATE-DATA).
        FIND FIRST ttJob
             WHERE ROWID(ttJob) EQ rRowID
             NO-ERROR.
    END. /* if valid handle */
    ELSE DO:
        FIND FIRST ttJob
             WHERE ttJob.resource EQ svResource
               AND ttJob.job EQ svJob
             NO-ERROR.
        IF NOT AVAILABLE ttJob THEN DO:
            /* transfer? */
            FOR EACH ttJob
                WHERE ttJob.job EQ svJob,
                FIRST mach NO-LOCK
                WHERE mach.company EQ ipcCompany
                  AND mach.m-code  EQ ttJob.resource
                BY ttJob.resource:
                /* list of resource(s) where job exists */
                cResourceList = cResourceList
                              + ttJob.resource
                              + (IF AVAILABLE mach THEN " - " + mach.m-dscr ELSE "") + ","
                              + ttJob.resource + ","
                              .
            END. /* each ttjob */
            IF cResourceList NE "" THEN DO:
                /* found job on other resource(s) */
                ASSIGN
                    cResourceList = TRIM(cResourceList,",")
                    cResource     = ENTRY(1,cResourceList)
                    .
                /* prompt for which resource to transfer from */
                IF NUM-ENTRIES(cResourceList) GT 1 THEN 
                RUN {&prompts}/jobSeqScanTrans.w (cResourceList, OUTPUT cResource).
                IF cResource EQ ? THEN RETURN. /* user canceled */
                /* scan has leading +, remove it */
                cResource = REPLACE(cResource,"+","").
                FIND FIRST ttJob
                     WHERE ttJob.resource EQ cResource
                       AND ttJob.job EQ svJob
                     NO-ERROR.
                IF AVAILABLE ttJob THEN DO:
                    /* move job to pending */
                    ASSIGN 
                        ttJob.resource = svResource
                        ttJob.jobType  = "P"
                        rRowID         = ROWID(ttJob)
                        .
                    /* resequence resource where job transferred from */
                    RUN sequenceJobs (cResource).
                    FIND ttJob WHERE ROWID(ttJob) EQ rRowID.
                END. /* avail ttjob */
            END. /* cresourcelist not empty */
        END. /* not avail */
    END. /* else */    
    IF AVAILABLE ttJob THEN DO:
        rRowID = ROWID(ttJob).
        IF ttJob.jobType EQ "P"  THEN DO:
            ASSIGN
                ttJob.jobType = "S"
                ttJob.jobSequence = iSeq
                .
            RUN sequenceJobs (svResource).
            RUN getJobs (svResource).
        END. /* if jobtype P */
        REPOSITION {&BROWSE-NAME} TO ROWID(rRowID).
    END. /* if avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sequenceJobs C-Win 
PROCEDURE sequenceJobs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcResource AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dtStartDate AS DATE    NO-UNDO.
    DEFINE VARIABLE iStartTime  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iSeq        AS INTEGER NO-UNDO.
    
    FIND FIRST ttJob
         WHERE ttJob.jobType     EQ "S"
           AND ttJob.resource    EQ ipcResource
           AND ttJob.jobSequence GE 0
         NO-ERROR.
    IF AVAILABLE ttJob THEN DO:
        ASSIGN
            dtStartDate = ttJob.startDate
            iStartTime  = ttJob.startTime
            .
        IF dtStartDate EQ ? THEN
        ASSIGN
            dtStartDate = TODAY
            iStartTime  = TIME
            .
        FOR EACH ttJob
            WHERE ttJob.jobType  EQ "S"
              AND ttJob.resource EQ ipcResource
              AND ttJob.jobFlag  EQ NO
            :
             ttJob.jobFlag = YES.
        END. /* each ttJob */
        
        FOR EACH ttJob
            WHERE ttJob.jobType  EQ "S"
              AND ttJob.resource EQ ipcResource
              AND ttJob.jobFlag  EQ YES
            :
            ASSIGN
                iSeq = iSeq + 1
                ttJob.jobSequence = iSeq
                ttJob.jobFlag = NO
                ttJob.startDate = dtStartDate
                ttJob.startTime = iStartTime
                .
            RUN newEnd (ttJob.timeSpan,ttJob.startDate,ttJob.startTime,
                        OUTPUT ttJob.endDate, OUTPUT ttJob.endTime).
            ASSIGN
                ttJob.startDateTime = numericDateTime(ttJob.startDate,ttJob.startTime)
                ttJob.endDateTime = numericDateTime(ttJob.endDate,ttJob.endTime)
                dtStartDate = ttJob.endDate
                iStartTime = ttJob.endTime
                .
        END. /* each ttJob */
        lSave = YES.
    END. /* if avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE winReSize C-Win 
PROCEDURE winReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
        {&WINDOW-NAME}:WINDOW-STATE = 1
        {&WINDOW-NAME}:HEIGHT = {&WINDOW-NAME}:HEIGHT - 2
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
        FRAME {&FRAME-NAME}:HEIGHT = {&WINDOW-NAME}:HEIGHT
        FRAME {&FRAME-NAME}:WIDTH  = {&WINDOW-NAME}:WIDTH
        iMaxHeight = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
        iMaxWidth  = FRAME {&FRAME-NAME}:WIDTH-PIXELS
        RECT-1:HEIGHT-PIXELS = iMaxHeight
        pendingText:WIDTH-PIXELS = iMaxWidth - pendingText:X - 2
        BROWSE jobs:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
                                  - BROWSE jobs:Y
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fBarCode C-Win 
FUNCTION fBarCode RETURNS CHARACTER
  (ipiIndex AS INTEGER, ipcValue AS CHARACTER, ipcCharacter AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN '<div id="bc' + STRING(ipiIndex) + '">'
         + '<svg id="barcode' + STRING(ipiIndex) + '"></svg>'
         + '</div>' + CHR(10)
         + '<script>JsBarcode("#barcode' + STRING(ipiIndex) + '", "' + ipcCharacter + ipcValue + '")'
         + '</script>' + CHR(10)
         .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION numericDateTime C-Win 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER) :
  {{&includes}/numericDateTime.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

