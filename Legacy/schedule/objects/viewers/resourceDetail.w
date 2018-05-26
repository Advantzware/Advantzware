&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: resourceDetail.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 5.15.2004

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

&SCOPED-DEFINE useTtbl ttblJob
&SCOPED-DEFINE browseName resourceDetail

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/filterVars.i}
{{&includes}/ttblJob.i}
{{&viewers}/includes/sharedVars.i NEW}

&SCOPED-DEFINE detailButton YES

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE browserTitle AS CHARACTER NO-UNDO.
DEFINE VARIABLE lockRelatedJobs AS LOGICAL NO-UNDO.
DEFINE VARIABLE resourceValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE updateMode AS LOGICAL NO-UNDO.
DEFINE VARIABLE updateEnabled AS LOGICAL NO-UNDO.

{{&viewers}/includes/browseDef.i}

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
{{&includes}/lockWindowUpdate.i}
&ENDIF

ON 'CTRL-L':U ANYWHERE
DO:
  RUN lagTime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME browseJob

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttblJob

/* Definitions for BROWSE browseJob                                     */
&Scoped-define FIELDS-IN-QUERY-browseJob ttblJob.jobSequence ttblJob.jobLocked ttblJob.job ttblJob.resourceSequence ttblJob.jobCompleted   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseJob ttblJob.jobSequence  ttblJob.jobLocked   
&Scoped-define ENABLED-TABLES-IN-QUERY-browseJob ttblJob
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-browseJob ttblJob
&Scoped-define SELF-NAME browseJob
&Scoped-define QUERY-STRING-browseJob FOR EACH ttblJob NO-LOCK WHERE ttblJob.resource EQ resourceValue AND ttblJob.job GE jobValueLo AND ttblJob.job LE jobValueHi AND (ttblJob.job BEGINS jobPhrase OR jobPhrase EQ '') AND ttblJob.resourceDetailFlag EQ YES ~{&dateRangePhrase} BY ttblJob.jobSequence
&Scoped-define OPEN-QUERY-browseJob OPEN QUERY {&SELF-NAME} FOR EACH ttblJob NO-LOCK WHERE ttblJob.resource EQ resourceValue AND ttblJob.job GE jobValueLo AND ttblJob.job LE jobValueHi AND (ttblJob.job BEGINS jobPhrase OR jobPhrase EQ '') AND ttblJob.resourceDetailFlag EQ YES ~{&dateRangePhrase} BY ttblJob.jobSequence.
&Scoped-define TABLES-IN-QUERY-browseJob ttblJob
&Scoped-define FIRST-TABLE-IN-QUERY-browseJob ttblJob


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-browseJob}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnFilter btnPrint btnDataCollection ~
btnLiveUpdate btnPendingReturn btnRefresh btnUnlock btnLock btnDetail ~
btnDatePrompt btnRelatedJobs RECT-1 jobPhrase btnSort btnGoTo btnUpdate ~
btnSetSeq browseJob btnJobNotes btnComplete btnUp btnDown btnPackJob ~
btnSave 
&Scoped-Define DISPLAYED-OBJECTS jobPhrase sortableColumns 

/* Custom List Definitions                                              */
/* ttblResourceFields,phraseFields,List-3,List-4,List-5,List-6          */
&Scoped-define ttblResourceFields btnPrint 
&Scoped-define phraseFields btnPrint jobPhrase 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcPriority sObject 
FUNCTION calcPriority RETURNS INTEGER
  (ipPriority AS INTEGER,ipResource AS CHARACTER,
   ipJobSequence AS INTEGER,ipResourceSequence AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD changedJob sObject 
FUNCTION changedJob RETURNS LOGICAL
  (ipStartDate AS DATE,ipStartTime AS INTEGER,
   ipEndDate AS DATE,ipEndTime AS INTEGER,
   ipOrigStartDate AS DATE,ipOrigStartTime AS INTEGER,
   ipOrigEndDate AS DATE,ipOrigEndTime AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pixelDate sObject 
FUNCTION pixelDate RETURNS DATE
  (ipDateTime AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pixelTime sObject 
FUNCTION pixelTime RETURNS INTEGER
  (ipDateTime AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD resourcePriority sObject 
FUNCTION resourcePriority RETURNS INTEGER
  (ipResource AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnComplete 
     IMAGE-UP FILE "schedule/images/save.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Complete Job".

DEFINE BUTTON btnDataCollection 
     IMAGE-UP FILE "schedule/images/datacollection.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Access Data Collection".

DEFINE BUTTON btnDatePrompt 
     IMAGE-UP FILE "schedule/images/dateon.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Turn Date Prompt Popup Off".

DEFINE BUTTON btnDetail 
     IMAGE-UP FILE "schedule/images/detailwinon.bmp":U
     LABEL "Detail" 
     SIZE 5.2 BY 1.1 TOOLTIP "Turn Job Detail Window Display Off".

DEFINE BUTTON btnDown 
     IMAGE-UP FILE "schedule/images/down.bmp":U
     LABEL "&DN" 
     SIZE 5 BY 1.14 TOOLTIP "Increment Job Sequence Down".

DEFINE BUTTON btnFilter 
     IMAGE-UP FILE "schedule/images/filterwindow.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Set Filter Values".

DEFINE BUTTON btnGoTo 
     LABEL "&Go To" 
     SIZE 10 BY 1 TOOLTIP "Reposition Board to Selected Row Date".

DEFINE BUTTON btnJobNotes 
     IMAGE-UP FILE "schedule/images/notetack.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Job Notes".

DEFINE BUTTON btnLiveUpdate 
     IMAGE-UP FILE "schedule/images/liveupdate.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Toggle Live Update Setting".

DEFINE BUTTON btnLock 
     IMAGE-UP FILE "schedule/images/locked.gif":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Lock Jobs".

DEFINE BUTTON btnPackJob 
     IMAGE-UP FILE "schedule/images/entireboard.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Pack Selected Job"
     FONT 6.

DEFINE BUTTON btnPendingReturn 
     IMAGE-UP FILE "schedule/images/pending.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.05 TOOLTIP "Return Job to Pending".

DEFINE BUTTON btnPrint 
     IMAGE-UP FILE "schedule/images/print.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Print".

DEFINE BUTTON btnRefresh 
     IMAGE-UP FILE "schedule/images/refresh.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Refresh".

DEFINE BUTTON btnRelatedJobs 
     IMAGE-UP FILE "schedule/images/unlocked.gif":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Lock Related Jobs".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "" 
     SIZE 5 BY 1.14 TOOLTIP "Save (Pack Resource)".

DEFINE BUTTON btnSetSeq 
     IMAGE-UP FILE "schedule/images/setseq.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Auto Sequence".

DEFINE BUTTON btnSort 
     LABEL "&Ascending" 
     SIZE 13 BY 1 TOOLTIP "Toggle Ascending/Descending Sort".

DEFINE BUTTON btnUnlock 
     IMAGE-UP FILE "schedule/images/unlocked.gif":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Unlock Jobs".

DEFINE BUTTON btnUp 
     IMAGE-UP FILE "schedule/images/up.bmp":U
     LABEL "&UP" 
     SIZE 5 BY 1.14 TOOLTIP "Increment Job Sequence Up".

DEFINE BUTTON btnUpdate 
     LABEL "&Update" 
     SIZE 10 BY 1 TOOLTIP "Update Current Job".

DEFINE VARIABLE jobPhrase AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Job" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE sortableColumns AS CHARACTER FORMAT "X(256)":U INITIAL " Sortable Columns" 
      VIEW-AS TEXT 
     SIZE 18 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 149.6 BY 1.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseJob FOR 
      ttblJob SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseJob sObject _FREEFORM
  QUERY browseJob DISPLAY
      ttblJob.jobSequence LABEL-BGCOLOR 14
  ttblJob.jobLocked
  ttblJob.job LABEL-BGCOLOR 14
  ttblJob.resourceSequence LABEL-BGCOLOR 14
  ttblJob.jobCompleted
ENABLE
  ttblJob.jobSequence
  ttblJob.jobLocked
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 143.6 BY 8.33
         TITLE "Jobs by Resource".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnFilter AT ROW 1.05 COL 31 HELP
          "Click to Set Filter Values"
     btnPrint AT ROW 1.05 COL 69 HELP
          "Click to Access Print Utility"
     btnDataCollection AT ROW 1.05 COL 98 HELP
          "Click to Access Data Collection"
     btnLiveUpdate AT ROW 1.05 COL 103 HELP
          "Click to Toggle Live Update Setting"
     btnPendingReturn AT ROW 1.05 COL 109 HELP
          "Click to Return Job to Pending"
     btnRefresh AT ROW 1.05 COL 116 HELP
          "Click to Refresh Resource Browser"
     btnUnlock AT ROW 1.05 COL 124 HELP
          "Click to Unlock Jobs"
     btnLock AT ROW 1.05 COL 129 HELP
          "Click to Lock Jobs"
     btnDetail AT ROW 1.05 COL 135 HELP
          "Turn Job Detail Window Display On/Off"
     btnDatePrompt AT ROW 1.05 COL 140 HELP
          "Turn Job Detail Window Display On/Off"
     btnRelatedJobs AT ROW 1.05 COL 145 HELP
          "Click to Lock/Unlock Related Jobs"
     jobPhrase AT ROW 1.1 COL 5 COLON-ALIGNED
     btnSort AT ROW 1.1 COL 55
     btnGoTo AT ROW 1.1 COL 75
     btnUpdate AT ROW 1.1 COL 86
     btnSetSeq AT ROW 2.19 COL 1.2 HELP
          "Click to Auto Set Sequence Based on Currently Sorted Order"
     browseJob AT ROW 2.19 COL 7
     btnJobNotes AT ROW 3.38 COL 1.2 HELP
          "Click to Access Notes"
     btnComplete AT ROW 4.57 COL 1.2 HELP
          "Click to Complete Job"
     btnUp AT ROW 5.76 COL 1.2 HELP
          "Click to Increment Job Sequence Up"
     btnDown AT ROW 6.95 COL 1.2 HELP
          "Click to Increment Job Sequence Down"
     btnPackJob AT ROW 8.14 COL 1.2 HELP
          "Click to Pack Selected Job"
     btnSave AT ROW 9.33 COL 1.2 HELP
          "Click to Save (Pack Resource)"
     sortableColumns AT ROW 1.1 COL 35 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW sObject ASSIGN
         HEIGHT             = 9.67
         WIDTH              = 150.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB sObject 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW sObject
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB browseJob btnSetSeq F-Main */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:HEIGHT           = 9.67
       FRAME F-Main:WIDTH            = 150.

ASSIGN 
       browseJob:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 5
       browseJob:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

ASSIGN 
       btnComplete:PRIVATE-DATA IN FRAME F-Main     = 
                "Complete Job".

ASSIGN 
       btnDatePrompt:PRIVATE-DATA IN FRAME F-Main     = 
                "Turn Date Prompt Popup".

ASSIGN 
       btnDetail:PRIVATE-DATA IN FRAME F-Main     = 
                "Turn Job Detail Window Display".

ASSIGN 
       btnJobNotes:PRIVATE-DATA IN FRAME F-Main     = 
                "Job Notes".

/* SETTINGS FOR BUTTON btnPrint IN FRAME F-Main
   1 2                                                                  */
ASSIGN 
       btnPrint:PRIVATE-DATA IN FRAME F-Main     = 
                "boardObject".

ASSIGN 
       btnRelatedJobs:PRIVATE-DATA IN FRAME F-Main     = 
                "Lock/Unlock Related Jobs".

/* SETTINGS FOR FILL-IN jobPhrase IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN sortableColumns IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseJob
/* Query rebuild information for BROWSE browseJob
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttblJob NO-LOCK
WHERE ttblJob.resource EQ resourceValue
AND ttblJob.job GE jobValueLo
AND ttblJob.job LE jobValueHi
AND (ttblJob.job BEGINS jobPhrase OR jobPhrase EQ '')
AND ttblJob.resourceDetailFlag EQ YES
~{&dateRangePhrase} BY ttblJob.jobSequence.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE browseJob */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME browseJob
&Scoped-define SELF-NAME browseJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON ANY-PRINTABLE OF browseJob IN FRAME F-Main /* Jobs by Resource */
DO:
  jobPhrase:SCREEN-VALUE = jobPhrase:SCREEN-VALUE + CHR(LASTKEY).
  APPLY 'VALUE-CHANGED' TO jobPhrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON BACKSPACE OF browseJob IN FRAME F-Main /* Jobs by Resource */
DO:
  jobPhrase:SCREEN-VALUE = SUBSTR(jobPhrase:SCREEN-VALUE,1,LENGTH(jobPhrase:SCREEN-VALUE) - 1).
  APPLY 'VALUE-CHANGED' TO jobPhrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON DEFAULT-ACTION OF browseJob IN FRAME F-Main /* Jobs by Resource */
DO:
  IF resourceBrowseAction THEN
  APPLY 'CHOOSE' TO btnGoTo.
  ELSE
  APPLY 'CHOOSE' TO btnUpdate.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON ENTRY OF browseJob IN FRAME F-Main /* Jobs by Resource */
DO:
  APPLY 'VALUE-CHANGED' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON RIGHT-MOUSE-CLICK OF browseJob IN FRAME F-Main /* Jobs by Resource */
DO:
  IF AVAILABLE ttblJob THEN
  RUN packJob (ttblJob.job).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON ROW-DISPLAY OF browseJob IN FRAME F-Main /* Jobs by Resource */
DO:
  {{&viewers}/includes/rowDisplay.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON ROW-ENTRY OF browseJob IN FRAME F-Main /* Jobs by Resource */
DO:
  IF NOT updateMode THEN
  DO:
    APPLY 'ENTRY' TO btnUpdate.
    APPLY 'ENTRY' TO {&BROWSE-NAME}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON ROW-LEAVE OF browseJob IN FRAME F-Main /* Jobs by Resource */
DO:
  RUN updateJob.
  ASSIGN
    updateMode = NO
    btnUpdate:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON START-SEARCH OF browseJob IN FRAME F-Main /* Jobs by Resource */
DO:
  IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN
  DO:
    columnLabel = {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
    RUN reopenBrowse.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON VALUE-CHANGED OF browseJob IN FRAME F-Main /* Jobs by Resource */
DO:
  IF resourceJobDetail AND AVAILABLE ttblJob THEN
  DO:
    sharedRowIDs = ttblJob.rowIDs.
    RUN detailJob IN boardHandle (ROWID(ttblJob),ttblJob.rowIDs).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnComplete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnComplete sObject
ON CHOOSE OF btnComplete IN FRAME F-Main
DO:
  IF NOT AVAILABLE ttblJob THEN RETURN NO-APPLY.
  {{&includes}/{&Board}/btnComplete.i ROWID(ttblJob) boardHandle}
  APPLY 'ROW-DISPLAY' TO {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDataCollection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDataCollection sObject
ON CHOOSE OF btnDataCollection IN FRAME F-Main
DO:
  RUN dataCollection IN boardHandle (ttblJob.rowIDs).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDatePrompt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDatePrompt sObject
ON CHOOSE OF btnDatePrompt IN FRAME F-Main
DO:
  updateEnabled = NOT updateEnabled.
  ldummy = SELF:LOAD-IMAGE('{&images}/date' + (IF updateEnabled THEN 'On'
                           ELSE 'Off') + '.bmp').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDetail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDetail sObject
ON CHOOSE OF btnDetail IN FRAME F-Main /* Detail */
DO:
  ASSIGN
    resourceJobDetail = NOT resourceJobDetail
    SELF:TOOLTIP = SELF:PRIVATE-DATA + ' ' + STRING(NOT resourceJobDetail,'On/Off')
    ldummy = SELF:LOAD-IMAGE('{&images}/detailWin' +
             TRIM(STRING(resourceJobDetail,'On/Off')) + '.bmp').
  RUN setResourceJobDetail IN boardHandle (resourceJobDetail).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDown sObject
ON CHOOSE OF btnDown IN FRAME F-Main /* DN */
DO:
  RUN moveSeq (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilter sObject
ON CHOOSE OF btnFilter IN FRAME F-Main
DO:
  IF NOT VALID-HANDLE(filterHandle) THEN
  RUN {&prompts}/fieldFilter.w PERSISTENT SET filterHandle ('{&Board}','','',NO,NO,THIS-PROCEDURE,'fieldFilter').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGoTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGoTo sObject
ON CHOOSE OF btnGoTo IN FRAME F-Main /* Go To */
DO:
  IF NOT AVAILABLE ttblJob THEN RETURN NO-APPLY.
  RUN positionBoard (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGoTo sObject
ON RIGHT-MOUSE-CLICK OF btnGoTo IN FRAME F-Main /* Go To */
DO:
  RUN {&prompts}/sharedVars.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnJobNotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJobNotes sObject
ON CHOOSE OF btnJobNotes IN FRAME F-Main
DO:
  IF NOT AVAILABLE ttblJob THEN RETURN NO-APPLY.
  {{&includes}/{&Board}/btnJobNotes.i ROWID(ttblJob)}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLiveUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLiveUpdate sObject
ON CHOOSE OF btnLiveUpdate IN FRAME F-Main
DO:
  IF NOT AVAILABLE ttblJob THEN
  RETURN NO-APPLY.
  FIND CURRENT ttblJob EXCLUSIVE-LOCK.
  ttblJob.liveUpdate = NOT ttblJob.liveUpdate.
  FIND CURRENT ttblJob NO-LOCK.
  BROWSE {&BROWSE-NAME}:REFRESH().
  APPLY 'ENTRY':U TO {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLock
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLock sObject
ON CHOOSE OF btnLock IN FRAME F-Main
DO:
  RUN setLockStatus (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPackJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPackJob sObject
ON CHOOSE OF btnPackJob IN FRAME F-Main
DO:
  IF AVAILABLE ttblJob THEN
  RUN packJob (ttblJob.job).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPendingReturn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPendingReturn sObject
ON CHOOSE OF btnPendingReturn IN FRAME F-Main
DO:
  IF NOT AVAILABLE ttblJob THEN RETURN NO-APPLY.
  RUN pendingReturn IN boardHandle (ttblJob.job,OUTPUT continue).
  IF continue THEN
  DO:
    APPLY 'CHOOSE':U TO btnRefresh.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint sObject
ON CHOOSE OF btnPrint IN FRAME F-Main
DO:
  RUN print IN boardHandle (resourceValue).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRefresh sObject
ON CHOOSE OF btnRefresh IN FRAME F-Main
DO:
  RUN reopenBrowse.
  RUN returnToPending(YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRelatedJobs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRelatedJobs sObject
ON CHOOSE OF btnRelatedJobs IN FRAME F-Main
DO:
  ASSIGN
    lockRelatedJobs = NOT lockRelatedJobs
    {&BROWSE-NAME}:TITLE = browserTitle + (IF lockRelatedJobs THEN ' (' + SELF:PRIVATE-DATA + ')' ELSE '')
    SELF:TOOLTIP = (IF lockRelatedJobs THEN 'Un' ELSE '') + 'Lock Related Jobs'
    ldummy = SELF:LOAD-IMAGE('{&images}/' + (IF lockRelatedJobs THEN '' ELSE 'un') + 'locked.gif').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave sObject
ON CHOOSE OF btnSave IN FRAME F-Main
DO:
  IF AVAILABLE ttblJob THEN
  RUN packResource (resourceValue).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSetSeq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSetSeq sObject
ON CHOOSE OF btnSetSeq IN FRAME F-Main
DO:
  MESSAGE 'Set Job Sequence on Currently Sorted Order?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO UPDATE setJobSequence AS LOGICAL.
  IF setJobSequence THEN
  RUN setJobSequence (ttblJob.resource).
  RUN returnToPending(NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSort sObject
ON CHOOSE OF btnSort IN FRAME F-Main /* Ascending */
DO:
  ASSIGN
    ascendingSort = NOT ascendingSort
    SELF:LABEL = IF ascendingSort THEN '&Ascending' ELSE '&Descending'.
  RUN reopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUnlock
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUnlock sObject
ON CHOOSE OF btnUnlock IN FRAME F-Main
DO:
  RUN setLockStatus (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUp sObject
ON CHOOSE OF btnUp IN FRAME F-Main /* UP */
DO:
  RUN moveSeq (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate sObject
ON CHOOSE OF btnUpdate IN FRAME F-Main /* Update */
DO:
  IF NOT AVAILABLE ttblJob THEN RETURN NO-APPLY.
  ASSIGN
    updateMode = YES
    SELF:SENSITIVE = NO.
  APPLY 'ENTRY' TO cellColumn[1].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME jobPhrase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobPhrase sObject
ON VALUE-CHANGED OF jobPhrase IN FRAME F-Main /* Job */
DO:
  ASSIGN {&SELF-NAME}.
  RUN reopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK sObject 


/* ***************************  Main Block  *************************** */

ON 'ENTRY':U OF jobSequence IN BROWSE {&BROWSE-NAME}
DO:
  IF updateEnabled THEN
  DO:
    APPLY 'ENTRY' TO ttblJob.jobLocked IN BROWSE {&BROWSE-NAME}.
    RETURN NO-APPLY.
  END.
END.

ON 'VALUE-CHANGED':U OF jobLocked IN BROWSE {&BROWSE-NAME}
DO:
  IF NOT lockRelatedJobs THEN
  RETURN NO-APPLY.
  FOR EACH buffJob EXCLUSIVE-LOCK WHERE buffJob.job EQ ttblJob.job
                                    AND ROWID(buffJob) NE ROWID(ttblJob):
    buffJob.jobLocked = NOT ttblJob.jobLocked. /* true value not yet written */
  END. /* each buffJob */
  MESSAGE 'All Related Jobs are now' (IF ttblJob.jobLocked THEN 'Un' ELSE '')
    'Locked!' VIEW-AS ALERT-BOX.
END.

{{&viewers}/includes/winTitle.i}
{{&viewers}/includes/viewersInclude.i}
{{&includes}/addTime.i}

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adjustSequence sObject 
PROCEDURE adjustSequence :
{{&includes}/{&Board}/adjustSequence.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDowntime sObject 
PROCEDURE afterDowntime :
{{&includes}/{&Board}/afterDowntime.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeDowntime sObject 
PROCEDURE beforeDowntime :
{{&includes}/{&Board}/beforeDowntime.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI sObject  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFields sObject 
PROCEDURE displayFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipResourceDescription AS CHARACTER NO-UNDO.
  
  ASSIGN
    resourceValue = ipResource
    browserTitle = 'Jobs for ' + ipResource +
                   IF ipResourceDescription EQ ? THEN ''
                   ELSE ' - ' + ipResourceDescription.
  RUN getCellColumns.
  DO WITH FRAME {&FRAME-NAME}:
    DISPLAY sortableColumns.
    {&BROWSE-NAME}:TITLE = browserTitle + (IF lockRelatedJobs THEN ' (' + btnRelatedJobs:PRIVATE-DATA + ')' ELSE '').
    {&OPEN-QUERY-{&BROWSE-NAME}}
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE downtimeSpan sObject 
PROCEDURE downtimeSpan :
{{&includes}/{&Board}/downtimeSpan.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE firstAvailable sObject 
PROCEDURE firstAvailable :
{{&includes}/{&Board}/firstAvailable.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPriorJobResource sObject 
PROCEDURE getPriorJobResource :
{{&includes}/{&Board}/getPriorJobResource.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPriorJobSequence sObject 
PROCEDURE getPriorJobSequence :
{{&includes}/{&Board}/getPriorJobSequence.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initJobSequence sObject 
PROCEDURE initJobSequence :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipSequence AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.

  IF ipSequence EQ 0 THEN
  FOR EACH buffJob EXCLUSIVE-LOCK WHERE buffJob.resource EQ ipResource:
    buffJob.jobSequence = 0.
  END.
  ELSE
  FOR EACH buffJob EXCLUSIVE-LOCK
      WHERE buffJob.resource EQ ipResource
        AND buffJob.jobSequence EQ 0 BY buffJob.startDateTime:
    ASSIGN
      ipSequence = ipSequence + 1
      buffJob.jobSequence = ipSequence.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobReset sObject 
PROCEDURE jobReset :
/*------------------------------------------------------------------------------
  Purpose:     do nothing procedure, here because it's in include used in
               pro/boardProc.i (pro/jobEndMove.i}
  Parameters:  character of ''
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipValue AS CHARACTER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobStackerCreate sObject 
PROCEDURE jobStackerCreate :
{{&includes}/{&Board}/jobStackerCreate.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lagTime sObject 
PROCEDURE lagTime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAILABLE ttblJob THEN RETURN.
  RUN {&prompts}/lagTime.w (ROWID(ttblJob)).
  IF lagTimeCol NE 0 THEN
  cellColumn[lagTimeCol]:SCREEN-VALUE = STRING(ttblJob.lagTime).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveExisting sObject 
PROCEDURE moveExisting :
&SCOPED-DEFINE endDateTime numericDateTime(~{~{&includes}/lastDate.i},86400)
{{&includes}/{&Board}/moveExisting.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveSeq sObject 
PROCEDURE moveSeq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipMove AS INTEGER NO-UNDO.

  DEFINE VARIABLE lowValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE hiValue AS INTEGER NO-UNDO.
  
  IF ipMove EQ -1 AND ttblJob.jobSequence EQ 1 THEN RETURN.
  ASSIGN
    cellColumn[1]:SCREEN-VALUE = STRING(INTEGER(cellColumn[1]:SCREEN-VALUE) + ipMove)
    lowValue = IF ipMove EQ 1 THEN ttblJob.jobSequence ELSE INTEGER(cellColumn[1]:SCREEN-VALUE)
    hiValue = IF ipMove EQ -1 THEN ttblJob.jobSequence ELSE INTEGER(cellColumn[1]:SCREEN-VALUE)
    ttblJob.jobSequence = INTEGER(cellColumn[1]:SCREEN-VALUE).
  RUN newSequence (resourceValue,lowValue,hiValue,ipMove * -1).
  RUN returnToPending(NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newEnd sObject 
PROCEDURE newEnd :
{{&includes}/{&Board}/newEnd.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newJobSequence sObject 
PROCEDURE newJobSequence :
{{&includes}/{&Board}/newJobSequence.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newSequence sObject 
PROCEDURE newSequence :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipLowSeq AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipHiSeq AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipSeqChange AS INTEGER NO-UNDO.

  REPEAT PRESELECT EACH buffJob EXCLUSIVE-LOCK
       WHERE buffJob.jobSequence GE ipLowSeq
         AND buffJob.jobSequence LE ipHiSeq:
    FIND NEXT buffJob.
    IF ROWID(buffJob) NE ROWID(ttblJob) AND buffJob.resource EQ ipResource THEN
    buffJob.jobSequence = buffJob.jobSequence + ipSeqChange.
  END.
  ldummy = BROWSE {&BROWSE-NAME}:REFRESH().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newStart sObject 
PROCEDURE newStart :
{{&includes}/{&Board}/newStart.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE packJob sObject 
PROCEDURE packJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipJob AS CHARACTER NO-UNDO.

  RUN objectName IN boardHandle ('Job',ipJob).
  RUN packJob IN boardHandle (ipJob).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE packResource sObject 
PROCEDURE packResource :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.

  RUN objectName IN boardHandle ('Resource',ipResource).
  RUN packResource IN boardHandle (ipResource).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE passResource sObject 
PROCEDURE passResource :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipResourceDescription AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipHandle AS HANDLE NO-UNDO.

  boardHandle = ipHandle.
  RUN getConfiguration.
  ASSIGN
    resourceJobDetail = NOT resourceJobDetail
    datePrompt = NOT datePrompt
    updateEnabled = datePrompt
    btnDataCollection:HIDDEN IN FRAME {&FRAME-NAME} = NOT CONNECTED('emptrack').
  APPLY 'CHOOSE' TO btnDetail.
  APPLY 'CHOOSE' TO btnDatePrompt.
  RUN displayFields (ipResource,ipResourceDescription) NO-ERROR.
  RUN reopenBrowse.
  APPLY 'ENTRY':U TO BROWSE {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE returnToPending sObject 
PROCEDURE returnToPending :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipPendingReturn AS LOGICAL NO-UNDO.

  ASSIGN
    btnPendingReturn:HIDDEN IN FRAME {&FRAME-NAME} = NOT ipPendingReturn
    btnPendingReturn:SENSITIVE = ipPendingReturn.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setJobDateTime sObject 
PROCEDURE setJobDateTime :
{{&includes}/{&Board}/setJobDateTime.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setJobPriority sObject 
PROCEDURE setJobPriority :
{{&includes}/{&Board}/setJobPriority.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setJobSequence sObject 
PROCEDURE setJobSequence :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  RUN initJobSequence (0,ipResource).
  GET FIRST {&BROWSE-NAME}.
  DO WHILE AVAILABLE(ttblJob):
    ASSIGN
      i = i + 1
      ttblJob.jobSequence = i.
    GET NEXT {&BROWSE-NAME}.
  END.
  RUN initJobSequence (i,ipResource).
  RUN reopenBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLockStatus sObject 
PROCEDURE setLockStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipLocked AS LOGICAL NO-UNDO.

  GET FIRST {&BROWSE-NAME}.
  DO WHILE AVAILABLE ttblJob:
    ttblJob.jobLocked = ipLocked.
    GET NEXT {&BROWSE-NAME}.
  END.
  RUN reopenBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE shiftJobs sObject 
PROCEDURE shiftJobs :
&SCOPED-DEFINE endDateTime numericDateTime(~{~{&includes}/lastDate.i},86400)
{{&includes}/{&Board}/shiftJobs.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE shiftJobSequence sObject 
PROCEDURE shiftJobSequence :
{{&includes}/{&Board}/shiftJobSequence.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateJob sObject 
PROCEDURE updateJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE checkResource AS CHARACTER NO-UNDO.
  DEFINE VARIABLE conflictChoice AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvDowntimeSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvLagTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lowValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE hiValue AS INTEGER NO-UNDO.
  
  IF INTEGER(cellColumn[1]:SCREEN-VALUE) NE ttblJob.jobSequence THEN
  DO:
    ASSIGN
      i = IF ttblJob.jobSequence LT INTEGER(cellColumn[1]:SCREEN-VALUE) THEN -1
          ELSE 1
      lowValue = IF i EQ -1 THEN ttblJob.jobSequence ELSE INTEGER(cellColumn[1]:SCREEN-VALUE)
      hiValue = IF i EQ 1 THEN ttblJob.jobSequence ELSE INTEGER(cellColumn[1]:SCREEN-VALUE)
      ttblJob.jobSequence = INTEGER(cellColumn[1]:SCREEN-VALUE).
    RUN newSequence (resourceValue,lowValue,hiValue,i).
    RUN returnToPending(NO).
  END.
  
  IF NOT updateEnabled THEN RETURN.

  ASSIGN
    ttblJob.sequenced = INTEGER(cellColumn[1]:SCREEN-VALUE) NE 0
    lvStartDate = ttblJob.startDate
    lvStartTime = ttblJob.startTime
    lvEndDate = ttblJob.endDate
    lvEndTime = ttblJob.endTime
    lvLagTime = ttblJob.lagTime
    checkResource = ttblJob.resource.
  RUN {&prompts}/move.w (ROWID(ttblJob),endDateMove,
                         INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime,
                         INPUT-OUTPUT lvEndDate,INPUT-OUTPUT lvEndTime,
                         INPUT-OUTPUT lvLagTime,OUTPUT updateMode).
  IF updateMode THEN
  DO:
    IF NOT endDateMove THEN /* need to auto calculate end date & time */
    RUN downtimeSpan (ttblJob.resource,ttblJob.timeSpan,lvStartDate,lvStartTime,
                      OUTPUT lvEndDate,OUTPUT lvEndTime,OUTPUT lvDowntimeSpan).
    ASSIGN
      lvStartDateTime = numericDateTime(lvStartDate,lvStartTime)
      lvEndDateTime = numericDateTime(lvEndDate,lvEndTime).
    {{&includes}/{&Board}/jobEndMove.i '' updateEnabled}
    ASSIGN
      updateMode = changedJob(lvStartDate,lvStartTime,
                              lvEndDate,lvEndTime,
                              ttblJob.origStartDate,ttblJob.origStartTime,
                              ttblJob.origEndDate,ttblJob.origEndTime)
      ttblJob.startDate = lvStartDate
      ttblJob.startTime = lvStartTime
      ttblJob.endDate = lvEndDate
      ttblJob.endTime = lvEndTime
      ttblJob.lagTime = lvLagTime
      ttblJob.downtimeSpan = lvDowntimeSpan.
    IF startDateCol NE 0 THEN
    cellColumn[startDateCol]:SCREEN-VALUE = STRING(lvStartDate).
    IF startTimeCol NE 0 THEN
    cellColumn[startTimeCol]:SCREEN-VALUE = STRING(lvStartTime,'HH:MM:SS am').
    IF endDateCol NE 0 THEN
    cellColumn[endDateCol]:SCREEN-VALUE = STRING(lvEndDate).
    IF endTimeCol NE 0 THEN
    cellColumn[endTimeCol]:SCREEN-VALUE = STRING(lvEndTime,'HH:MM:SS am').
    IF lagTimeCol NE 0 THEN
    cellColumn[lagTimeCol]:SCREEN-VALUE = STRING(lvLagTime).
    ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
    ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
    RUN reopenBrowse.
    RUN positionBoard (YES).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcPriority sObject 
FUNCTION calcPriority RETURNS INTEGER
  (ipPriority AS INTEGER,ipResource AS CHARACTER,
   ipJobSequence AS INTEGER,ipResourceSequence AS INTEGER) :
  {{&includes}/{&Board}/calcPriority.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION changedJob sObject 
FUNCTION changedJob RETURNS LOGICAL
  (ipStartDate AS DATE,ipStartTime AS INTEGER,
   ipEndDate AS DATE,ipEndTime AS INTEGER,
   ipOrigStartDate AS DATE,ipOrigStartTime AS INTEGER,
   ipOrigEndDate AS DATE,ipOrigEndTime AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN ipStartDate NE ipOrigStartDate OR
         ipStartTime NE ipOrigStartTime OR
         ipEndDate NE ipOrigEndDate OR
         ipEndTime NE ipOrigEndTime.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pixelDate sObject 
FUNCTION pixelDate RETURNS DATE
  (ipDateTime AS DECIMAL) :
  {{&includes}/pixelDate.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pixelTime sObject 
FUNCTION pixelTime RETURNS INTEGER
  (ipDateTime AS DECIMAL) :
  {{&includes}/pixelTime.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION resourcePriority sObject 
FUNCTION resourcePriority RETURNS INTEGER
  (ipResource AS CHARACTER) :
  {{&includes}/{&Board}/resourcePriority.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

