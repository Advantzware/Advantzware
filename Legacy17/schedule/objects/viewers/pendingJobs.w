&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject
/*------------------------------------------------------------------------

  File: jobBrowse.w

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

&SCOPED-DEFINE useTtbl pendingJob
&SCOPED-DEFINE browseName pendingJob
&SCOPED-DEFINE colorJobTable pendingJob

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/filterVars.i}
{{&includes}/ttblJob.i}
{{&viewers}/includes/sharedVars.i NEW}
&SCOPED-DEFINE useTable pendingJob
{{&includes}/jobStatusFunc.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE correct-error AS LOGICAL NO-UNDO.
&SCOPED-DEFINE colorJobTable pendingJob
DEFINE VARIABLE fgJobColor AS INTEGER NO-UNDO.
DEFINE VARIABLE newTime AS INTEGER NO-UNDO.
DEFINE VARIABLE resourceValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE selectedID AS ROWID NO-UNDO.
DEFINE VARIABLE selectedRowID AS ROWID NO-UNDO.
DEFINE VARIABLE updateMode AS LOGICAL NO-UNDO.

DEFINE BUFFER bPendingJob FOR pendingJob.
DEFINE BUFFER bPJob FOR pendingJob.

{{&viewers}/includes/browseDef.i}

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

{{&includes}/lockWindowUpdate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME browseJob

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES pendingJob

/* Definitions for BROWSE browseJob                                     */
&Scoped-define FIELDS-IN-QUERY-browseJob pendingJob.jobSequence pendingJob.job pendingJob.resource pendingJob.resourceSequence pendingJob.jobCompleted   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseJob pendingJob.jobSequence   
&Scoped-define ENABLED-TABLES-IN-QUERY-browseJob pendingJob
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-browseJob pendingJob
&Scoped-define SELF-NAME browseJob
&Scoped-define QUERY-STRING-browseJob FOR EACH pendingJob NO-LOCK WHERE (pendingJob.job BEGINS jobPhrase OR jobPhrase EQ '') AND pendingJob.job GE jobValueLo AND pendingJob.job LE jobValueHi AND (pendingJob.resource EQ resources OR resources EQ '<Select ...>') AND pendingJob.pendingJobFlag EQ YES ~{&dateRangePhrase} BY pendingJob.jobSequence BY pendingJob.jobSort
&Scoped-define OPEN-QUERY-browseJob OPEN QUERY {&SELF-NAME} FOR EACH pendingJob NO-LOCK WHERE (pendingJob.job BEGINS jobPhrase OR jobPhrase EQ '') AND pendingJob.job GE jobValueLo AND pendingJob.job LE jobValueHi AND (pendingJob.resource EQ resources OR resources EQ '<Select ...>') AND pendingJob.pendingJobFlag EQ YES ~{&dateRangePhrase} BY pendingJob.jobSequence BY pendingJob.jobSort.
&Scoped-define TABLES-IN-QUERY-browseJob pendingJob
&Scoped-define FIRST-TABLE-IN-QUERY-browseJob pendingJob


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-browseJob}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS resources btnFilter btnPrint btnComplete ~
btnMoveResource btnRefresh RECT-1 RECT-2 RECT-3 RECT-5 RECT-6 jobPhrase ~
btnSort browseJob instructions4Steps 
&Scoped-Define DISPLAYED-OBJECTS resources jobPhrase dateTime newDate ~
newHour newMinute newAMPM setDateTimeOptions relatedJobs ~
setPlacementOptions instructions4Steps sortableColumns 

/* Custom List Definitions                                              */
/* ttblResourceFields,phraseFields,dateTimeFields,btnFunctions,List-5,List-6 */
&Scoped-define ttblResourceFields btnPrint 
&Scoped-define phraseFields btnPrint jobPhrase 
&Scoped-define dateTimeFields newDate newHour newMinute newAMPM 
&Scoped-define btnFunctions btnSetSeq RECT-2 RECT-3 RECT-5 RECT-6 btnUpdate ~
dateTime newDate btnCalendar newHour newMinute newAMPM btnCurrentDateTime ~
setDateTimeOptions btnSetDateTime relatedJobs setPlacementOptions ~
btnScheduleJob btn4Steps btnSelectedOnly 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD bJobBGColor sObject 
FUNCTION bJobBGColor RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD bJobFGColor sObject 
FUNCTION bJobFGColor RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkDates sObject 
FUNCTION checkDates RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD jobBGColor sObject 
FUNCTION jobBGColor RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD jobFGColor sObject 
FUNCTION jobFGColor RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn4Steps 
     LABEL "&Execute Steps 1-3" 
     SIZE 22 BY 1.

DEFINE BUTTON btnCalendar 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.4 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnComplete 
     IMAGE-UP FILE "schedule/images/save.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Complete Job".

DEFINE BUTTON btnCurrentDateTime 
     IMAGE-UP FILE "schedule/images/clock1.bmp":U
     LABEL "&1" 
     SIZE 4.4 BY 1.05 TOOLTIP "Set Using System Current Date && Time".

DEFINE BUTTON btnFilter 
     IMAGE-UP FILE "schedule/images/filterwindow.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Set Filter Values".

DEFINE BUTTON btnMoveResource 
     IMAGE-UP FILE "schedule/images/moveresource.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Access Move Resource".

DEFINE BUTTON btnPrint 
     IMAGE-UP FILE "schedule/images/print.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Print".

DEFINE BUTTON btnRefresh 
     IMAGE-UP FILE "schedule/images/refresh.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Refresh".

DEFINE BUTTON btnScheduleJob 
     LABEL "&3 Schedule Jobs" 
     SIZE 22 BY 1.

DEFINE BUTTON btnSelectedOnly 
     LABEL "&4 Selected Only" 
     SIZE 22 BY 1.

DEFINE BUTTON btnSetDateTime 
     LABEL "&2 Set Job Date&&Time" 
     SIZE 22 BY 1.

DEFINE BUTTON btnSetSeq 
     IMAGE-UP FILE "schedule/images/setseq.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Auto Sequence".

DEFINE BUTTON btnSort 
     LABEL "&Ascending" 
     SIZE 13 BY 1 TOOLTIP "Toggle Ascending/Descending Sort".

DEFINE BUTTON btnUpdate 
     LABEL "&Update" 
     SIZE 10 BY 1 TOOLTIP "Update Current Job".

DEFINE VARIABLE newAMPM AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE resources AS CHARACTER FORMAT "X(256)":U INITIAL "<Select ...>" 
     LABEL "&Resource" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 30
     LIST-ITEMS "<Select ...>" 
     DROP-DOWN-LIST
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE instructions4Steps AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 22 BY 4.76
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE jobPhrase AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Job" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE newDate AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE newHour AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE newMinute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE sortableColumns AS CHARACTER FORMAT "X(256)":U INITIAL " Sortable Columns" 
      VIEW-AS TEXT 
     SIZE 18 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE dateTime AS CHARACTER INITIAL "Start" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Start", "Start",
"End", "End"
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE setDateTimeOptions AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "&All Jobs", 1,
"&This Job Only", 2
     SIZE 22 BY 1.67
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE setPlacementOptions AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Slide &1st Available", 1,
"Insert/&PushExisting", 2,
"&Shift Jobs", 3,
"Schedule As &Is", 4,
"&Last", 5
     SIZE 22 BY 3.33
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 149 BY 1.19.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 24 BY 2.62
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 24 BY 3.33
     BGCOLOR 9 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 24 BY 5
     BGCOLOR 2 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 24 BY 1.91
     BGCOLOR 14 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 24 BY 7.62
     BGCOLOR 12 .

DEFINE VARIABLE relatedJobs AS LOGICAL INITIAL no 
     LABEL "Schedule All of a" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseJob FOR 
      pendingJob SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseJob sObject _FREEFORM
  QUERY browseJob DISPLAY
      pendingJob.jobSequence LABEL-BGCOLOR 14
      pendingJob.job LABEL-BGCOLOR 14
      pendingJob.resource LABEL-BGCOLOR 14
      pendingJob.resourceSequence LABEL-BGCOLOR 14
      pendingJob.jobCompleted
  ENABLE
      pendingJob.jobSequence
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 125 BY 21.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     resources AT ROW 1 COL 109.4 HELP
          "Select Resource"
     btnFilter AT ROW 1.05 COL 31 HELP
          "Click to Set Filter Values"
     btnPrint AT ROW 1.05 COL 69 HELP
          "Click to Access Print Utility"
     btnComplete AT ROW 1.05 COL 75 HELP
          "Click to Complete Job"
     btnMoveResource AT ROW 1.05 COL 81 HELP
          "Click to Access Move Resource"
     btnSetSeq AT ROW 1.05 COL 87 HELP
          "Click to Auto Set Sequence Based on Currently Sorted Order"
     btnRefresh AT ROW 1.05 COL 104 HELP
          "Click to Refresh Resource Browser"
     jobPhrase AT ROW 1.1 COL 4 COLON-ALIGNED
     btnSort AT ROW 1.1 COL 55
     btnUpdate AT ROW 1.1 COL 93
     dateTime AT ROW 2.19 COL 9 NO-LABEL
     browseJob AT ROW 2.19 COL 25
     newDate AT ROW 3.62 COL 2 NO-LABEL
     btnCalendar AT ROW 3.62 COL 20 HELP
          "Click to Access Popup Calendar"
     newHour AT ROW 4.81 COL 1.4 HELP
          "Enter Starting Hour" NO-LABEL
     newMinute AT ROW 4.81 COL 5 COLON-ALIGNED HELP
          "Enter Starting Minute"
     newAMPM AT ROW 4.81 COL 10 COLON-ALIGNED NO-LABEL
     btnCurrentDateTime AT ROW 4.81 COL 20 HELP
          "Set Using System Current Date && Time"
     setDateTimeOptions AT ROW 6.48 COL 2 NO-LABEL
     btnSetDateTime AT ROW 8.38 COL 2
     relatedJobs AT ROW 10.05 COL 2
     setPlacementOptions AT ROW 12.19 COL 2 NO-LABEL
     btnScheduleJob AT ROW 15.76 COL 2
     instructions4Steps AT ROW 17.43 COL 2 NO-LABEL
     btn4Steps AT ROW 22.43 COL 2 HELP
          "Click to Execute 4 Step Scheduling of Job"
     btnSelectedOnly AT ROW 23.62 COL 2
     sortableColumns AT ROW 1.1 COL 35 COLON-ALIGNED NO-LABEL
     "      Job's Resources" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 10.76 COL 2
     "&1 Job:" VIEW-AS TEXT
          SIZE 6 BY 1 AT ROW 2.19 COL 2
     RECT-1 AT ROW 1 COL 1
     RECT-2 AT ROW 3.38 COL 1
     RECT-3 AT ROW 6.24 COL 1
     RECT-5 AT ROW 11.95 COL 1
     RECT-6 AT ROW 9.81 COL 1
     RECT-7 AT ROW 17.19 COL 1
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
         HEIGHT             = 24
         WIDTH              = 151.4.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB browseJob dateTime F-Main */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:HEIGHT           = 24
       FRAME F-Main:WIDTH            = 151.4.

ASSIGN 
       browseJob:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 4
       browseJob:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR BUTTON btn4Steps IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON btnCalendar IN FRAME F-Main
   NO-ENABLE 4                                                          */
ASSIGN 
       btnComplete:PRIVATE-DATA IN FRAME F-Main     = 
                "Complete Job".

/* SETTINGS FOR BUTTON btnCurrentDateTime IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON btnPrint IN FRAME F-Main
   1 2                                                                  */
ASSIGN 
       btnPrint:PRIVATE-DATA IN FRAME F-Main     = 
                "boardObject".

/* SETTINGS FOR BUTTON btnScheduleJob IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON btnSelectedOnly IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON btnSetDateTime IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON btnSetSeq IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON btnUpdate IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR RADIO-SET dateTime IN FRAME F-Main
   NO-ENABLE 4                                                          */
ASSIGN 
       instructions4Steps:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN jobPhrase IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR COMBO-BOX newAMPM IN FRAME F-Main
   NO-ENABLE 3 4                                                        */
/* SETTINGS FOR FILL-IN newDate IN FRAME F-Main
   NO-ENABLE ALIGN-L 3 4                                                */
/* SETTINGS FOR FILL-IN newHour IN FRAME F-Main
   NO-ENABLE ALIGN-L 3 4                                                */
/* SETTINGS FOR FILL-IN newMinute IN FRAME F-Main
   NO-ENABLE 3 4                                                        */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR RECTANGLE RECT-7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX relatedJobs IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR COMBO-BOX resources IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR RADIO-SET setDateTimeOptions IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR RADIO-SET setPlacementOptions IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN sortableColumns IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseJob
/* Query rebuild information for BROWSE browseJob
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH pendingJob NO-LOCK
WHERE (pendingJob.job BEGINS jobPhrase OR jobPhrase EQ '')
AND pendingJob.job GE jobValueLo
AND pendingJob.job LE jobValueHi
AND (pendingJob.resource EQ resources OR resources EQ '<Select ...>')
AND pendingJob.pendingJobFlag EQ YES
~{&dateRangePhrase} BY pendingJob.jobSequence BY pendingJob.jobSort.
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
ON ANY-PRINTABLE OF browseJob IN FRAME F-Main
DO:
  jobPhrase:SCREEN-VALUE = jobPhrase:SCREEN-VALUE + CHR(LASTKEY).
  APPLY 'VALUE-CHANGED' TO jobPhrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON BACKSPACE OF browseJob IN FRAME F-Main
DO:
  jobPhrase:SCREEN-VALUE = SUBSTR(jobPhrase:SCREEN-VALUE,1,LENGTH(jobPhrase:SCREEN-VALUE) - 1).
  APPLY 'VALUE-CHANGED' TO jobPhrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON DEFAULT-ACTION OF browseJob IN FRAME F-Main
DO:
  APPLY 'CHOOSE' TO btnUpdate.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON ENTRY OF browseJob IN FRAME F-Main
DO:
  APPLY 'VALUE-CHANGED' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON ROW-DISPLAY OF browseJob IN FRAME F-Main
DO:
  {{&viewers}/includes/rowDisplay.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON ROW-ENTRY OF browseJob IN FRAME F-Main
DO:
  IF NOT updateMode THEN DO:
    APPLY 'ENTRY' TO btnUpdate.
    APPLY 'ENTRY' TO {&BROWSE-NAME}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON ROW-LEAVE OF browseJob IN FRAME F-Main
DO:
  RUN updateJob.
  ASSIGN
    updateMode = NO
    btnUpdate:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON START-SEARCH OF browseJob IN FRAME F-Main
DO:
  IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
    columnLabel = {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
    RUN reopenBrowse.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON VALUE-CHANGED OF browseJob IN FRAME F-Main
DO:
  IF NOT AVAILABLE pendingJob THEN RETURN NO-APPLY.
  selectedID = ROWID(pendingJob).

  IF dateTime EQ 'Start' THEN
  ASSIGN
    newDate:SCREEN-VALUE = STRING(pendingJob.startDate)
    newHour:SCREEN-VALUE = SUBSTR(STRING(pendingJob.startTime,'HH:MM:SSam'),1,2)
    newMinute:SCREEN-VALUE = SUBSTR(STRING(pendingJob.startTime,'HH:MM:SSam'),4,2)
    newAMPM:SCREEN-VALUE = SUBSTR(STRING(pendingJob.startTime,'HH:MM:SSam'),9,2).
  ELSE
  ASSIGN
    newDate:SCREEN-VALUE = STRING(pendingJob.endDate)
    newHour:SCREEN-VALUE = SUBSTR(STRING(pendingJob.endTime,'HH:MM:SSam'),1,2)
    newMinute:SCREEN-VALUE = SUBSTR(STRING(pendingJob.endTime,'HH:MM:SSam'),4,2)
    newAMPM:SCREEN-VALUE = SUBSTR(STRING(pendingJob.endTime,'HH:MM:SSam'),9,2).
  RUN detailJob IN boardHandle (ROWID(pendingJob),pendingJob.rowIDs).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn4Steps
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn4Steps sObject
ON CHOOSE OF btn4Steps IN FRAME F-Main /* Execute Steps 1-3 */
DO:
  APPLY 'CHOOSE':U TO btnCurrentDateTime.
  APPLY 'CHOOSE':U TO btnSetDateTime.
  APPLY 'CHOOSE':U TO btnScheduleJob.
  APPLY 'ENTRY':U TO jobPhrase.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar sObject
ON CHOOSE OF btnCalendar IN FRAME F-Main
DO:
  APPLY 'HELP' TO newDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnComplete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnComplete sObject
ON CHOOSE OF btnComplete IN FRAME F-Main
DO:
  IF AVAILABLE pendingJob THEN
  RUN {&prompts}/completePending.w (ROWID(pendingJob),'{&Board}').
  {&OPEN-QUERY-browseJob}
  IF AVAILABLE pendingJob THEN
  APPLY 'VALUE-CHANGED' TO BROWSE browseJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCurrentDateTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCurrentDateTime sObject
ON CHOOSE OF btnCurrentDateTime IN FRAME F-Main /* 1 */
DO:
  ASSIGN
    newDate:SCREEN-VALUE = STRING(TODAY)
    newHour:SCREEN-VALUE = SUBSTR(STRING(TIME,'HH:MM:SSam'),1,2)
    newMinute:SCREEN-VALUE = SUBSTR(STRING(TIME,'HH:MM:SSam'),4,2)
    newAMPM:SCREEN-VALUE = SUBSTR(STRING(TIME,'HH:MM:SSam'),9,2).
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


&Scoped-define SELF-NAME btnMoveResource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveResource sObject
ON CHOOSE OF btnMoveResource IN FRAME F-Main
DO:
  RUN movePendingRes IN boardHandle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint sObject
ON CHOOSE OF btnPrint IN FRAME F-Main
DO:
  resourceValue = IF resources EQ '<Select ...>' THEN 'ALL' ELSE resources.
  RUN print IN boardHandle (resourceValue).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRefresh sObject
ON CHOOSE OF btnRefresh IN FRAME F-Main
DO:
  RUN reopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnScheduleJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnScheduleJob sObject
ON CHOOSE OF btnScheduleJob IN FRAME F-Main /* 3 Schedule Jobs */
DO:
  IF NOT checkDates() THEN RETURN NO-APPLY.
  IF relatedJobs THEN
  RUN relatedJobs (setPlacementOptions).
  ELSE
  RUN scheduleJobs (setPlacementOptions).
  {&OPEN-QUERY-browseJob}
  IF AVAILABLE pendingJob THEN
  APPLY 'VALUE-CHANGED' TO BROWSE browseJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectedOnly
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectedOnly sObject
ON CHOOSE OF btnSelectedOnly IN FRAME F-Main /* 4 Selected Only */
DO:
  APPLY 'CHOOSE':U TO btnCurrentDateTime.
  ASSIGN
    selectedRowID = selectedID
    setDateTimeOptions:SCREEN-VALUE = '2'
    setDateTimeOptions.
  APPLY 'CHOOSE':U TO btnSetDateTime.
  APPLY 'CHOOSE':U TO btnScheduleJob.
  APPLY 'ENTRY':U TO jobPhrase.
  selectedRowID = ?.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSetDateTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSetDateTime sObject
ON CHOOSE OF btnSetDateTime IN FRAME F-Main /* 2 Set Job DateTime */
DO:
  IF NOT AVAILABLE pendingJob THEN RETURN NO-APPLY.
  FIND bPendingJob WHERE ROWID(bPendingJob) EQ ROWID(pendingJob).
  ASSIGN {&dateTimeFields}
    newTime = (INTEGER(newHour) + (IF newAMPM EQ 'PM' AND INTEGER(newHour) NE 12 THEN 12 ELSE
                                   IF newAMPM EQ 'AM' AND INTEGER(newHour) EQ 12 THEN -12 ELSE 0)) * 3600 +
               INTEGER(newMinute) * 60.
  IF dateTime EQ 'Start' THEN DO:
    ASSIGN
      bPendingJob.startDate = newDate
      bPendingJob.startTime = newTime.
    RUN newEnd (bPendingJob.timeSpan,bPendingJob.startDate,bPendingJob.startTime,
                OUTPUT bPendingJob.endDate,OUTPUT bPendingJob.endTime).
  END.
  ELSE DO:
    ASSIGN
      bPendingJob.endDate = newDate
      bPendingJob.endTime = newTime.
    RUN newStart (bPendingJob.timeSpan,bPendingJob.endDate,bPendingJob.endTime,
                  OUTPUT bPendingJob.startDate,OUTPUT bPendingJob.startTime).
  END.
  IF setDateTimeOptions EQ 1 THEN /* all */ DO:
    RUN setPriorDateTime (ROWID(bPendingJob),
                          bPendingJob.startDate,bPendingJob.startTime).
    RUN setNextDateTime (ROWID(bPendingJob),
                         bPendingJob.endDate,bPendingJob.endTime).
    IF relatedJobs THEN DO:
      GET FIRST {&BROWSE-NAME}.
      DO WHILE AVAILABLE(pendingJob):
        RUN setPriorRelated (pendingJob.job,pendingJob.resourceSequence,
                             pendingJob.startDate,pendingJob.startTime).
        RUN setNextRelated (pendingJob.job,pendingJob.resourceSequence,
                            pendingJob.endDate,pendingJob.endTime).
        GET NEXT {&BROWSE-NAME}.
      END. /* do while */
    END. /* if relatedjobs */
  END. /* if setdatetimeoptions */
  RUN reopenBrowse.
  APPLY 'VALUE-CHANGED' TO BROWSE browseJob.
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
  RUN setJobSequence.
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


&Scoped-define SELF-NAME btnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate sObject
ON CHOOSE OF btnUpdate IN FRAME F-Main /* Update */
DO:
  IF NOT AVAILABLE pendingJob OR
     resources BEGINS '<Select' THEN RETURN NO-APPLY.
  ASSIGN
    updateMode = YES
    SELF:SENSITIVE = NO.
  APPLY 'ENTRY' TO cellColumn[1].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dateTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dateTime sObject
ON VALUE-CHANGED OF dateTime IN FRAME F-Main
DO:
  ASSIGN {&SELF-NAME}.
  IF AVAILABLE pendingJob THEN
  APPLY 'VALUE-CHANGED' TO BROWSE browseJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME jobPhrase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobPhrase sObject
ON VALUE-CHANGED OF jobPhrase IN FRAME F-Main /* Job */
DO:
  ASSIGN {&SELF-NAME}.
  RUN reopenBrowse.
  APPLY 'VALUE-CHANGED':U TO browseJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME newDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newDate sObject
ON HELP OF newDate IN FRAME F-Main
DO:
  {{&includes}/calendar.i}
  APPLY 'LEAVE' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newDate sObject
ON LEAVE OF newDate IN FRAME F-Main
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME newHour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newHour sObject
ON LEAVE OF newHour IN FRAME F-Main
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 1 OR INTEGER(SELF:SCREEN-VALUE) GT 12.
  {{&includes}/Pro/entryerr.i &error-message="Invalid Hour, range = 1 to 12"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newHour sObject
ON RETURN OF newHour IN FRAME F-Main
DO:
  APPLY 'LEAVE' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME newMinute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newMinute sObject
ON LEAVE OF newMinute IN FRAME F-Main
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {{&includes}/Pro/entryerr.i &error-message="Invalid Minute, range = 0 to 59"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newMinute sObject
ON RETURN OF newMinute IN FRAME F-Main
DO:
  APPLY 'LEAVE' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME relatedJobs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL relatedJobs sObject
ON VALUE-CHANGED OF relatedJobs IN FRAME F-Main /* Schedule All of a */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME resources
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL resources sObject
ON VALUE-CHANGED OF resources IN FRAME F-Main /* Resource */
DO:
  ASSIGN {&SELF-NAME}.
  RUN reopenBrowse.
  IF {&SELF-NAME} BEGINS '<Select' THEN
  DISABLE {&btnFunctions} WITH FRAME {&FRAME-NAME}.
  ELSE
  ENABLE {&btnFunctions} WITH FRAME {&FRAME-NAME}.
  APPLY 'ENTRY' TO BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setDateTimeOptions
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setDateTimeOptions sObject
ON VALUE-CHANGED OF setDateTimeOptions IN FRAME F-Main
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setPlacementOptions
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setPlacementOptions sObject
ON VALUE-CHANGED OF setPlacementOptions IN FRAME F-Main
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK sObject 


/* ***************************  Main Block  *************************** */

{{&viewers}/includes/winTitle.i}
{{&viewers}/includes/viewersInclude.i}

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  RUN getCellColumns.
  DO WITH FRAME {&FRAME-NAME}:
    DISPLAY sortableColumns.
    {&OPEN-QUERY-{&BROWSE-NAME}}
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE downtimeSpan sObject 
PROCEDURE downtimeSpan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/{&Board}/downtimeSpan.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getResources sObject 
PROCEDURE getResources :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/getResources.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newEnd sObject 
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

  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

  REPEAT PRESELECT EACH bPendingJob EXCLUSIVE-LOCK
                  WHERE bPendingJob.jobSequence GE ipLowSeq
                    AND bPendingJob.jobSequence LE ipHiSeq:
    FIND NEXT bPendingJob.
    IF ROWID(bPendingJob) NE ROWID(pendingJob) AND bPendingJob.resource EQ ipResource THEN
    bPendingJob.jobSequence = bPendingJob.jobSequence + ipSeqChange.
  END.
  ldummy = BROWSE {&BROWSE-NAME}:REFRESH().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newStart sObject 
PROCEDURE newStart :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/{&Board}/newStart.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE passHandle sObject 
PROCEDURE passHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipHandle AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER ipBoard AS CHARACTER NO-UNDO.

  ASSIGN
    boardHandle = ipHandle
    relatedJobs:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'
    relatedJobs
    instructions4Steps:SCREEN-VALUE = 'Select Resource, ' +
      'Click to Quick Schedule:' + CHR(10) + CHR(10) +
      '1. Current Date&Time' + CHR(10) +
      '2. Set Job Date&Time' + CHR(10) +
      '3. Schedule Jobs'.
  RUN getConfiguration.
  RUN setPendingJob.
  RUN getResources.
  RUN displayFields NO-ERROR.
  RUN reopenBrowse.
  APPLY 'VALUE-CHANGED' TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
  APPLY 'ENTRY':U TO BROWSE {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE relatedJobs sObject 
PROCEDURE relatedJobs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipOption AS INTEGER NO-UNDO.

  DEFINE VARIABLE scheduledJob AS LOGICAL NO-UNDO.
  DEFINE VARIABLE priorEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE priorEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE priorDateTime AS INTEGER NO-UNDO INITIAL ?.
  DEFINE VARIABLE saveDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE pendingRowID AS ROWID NO-UNDO.
  
  IF selectedRowID EQ ? THEN
  pendingRowID = IF AVAILABLE pendingJob AND setDateTimeOptions EQ 2 THEN ROWID(pendingJob)
                 ELSE ?.

  SESSION:SET-WAIT-STATE('General').
  GET FIRST {&BROWSE-NAME}.
  saveDateTime = IF AVAILABLE pendingJob THEN numericDateTime(pendingJob.endDate,pendingJob.endTime)
                 ELSE numericDateTime(TODAY,TIME).
  DO WHILE AVAILABLE(pendingJob):
    IF selectedRowID EQ ? OR ROWID(pendingJob) EQ selectedRowID THEN DO:
      IF pendingRowID EQ ? OR ROWID(pendingJob) EQ pendingRowID THEN
      FOR EACH bPJob EXCLUSIVE-LOCK
          WHERE bPJob.job EQ pendingJob.job BY bPJob.resourceSequence:
        CREATE ttblJob.
        BUFFER-COPY bPJob TO ttblJob.
        ASSIGN
          scheduledJob = YES
          ttblJob.origStartDate = bPJob.startDate
          ttblJob.origStartTime = bPJob.startTime
          ttblJob.origEndDate = bPJob.endDate
          ttblJob.origEndTime = bPJob.endTime.
        IF selectedRowID NE ? THEN DO:
          ASSIGN
            ttblJob.startDate = TODAY
            ttblJob.startTime = TIME.
          RUN newEnd (ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                      OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
        END. /* selectedrowid */
        ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
        ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
        ASSIGN
          ttblJob.jobBGColor = bJobBGColor()
          ttblJob.jobFGColor = bJobFGColor().
        CASE ipOption:
          /* slide 1st available */
          WHEN 1 THEN DO:
            IF priorDateTime NE ? AND priorDateTime GE ttblJob.startDateTime THEN DO:
              ASSIGN
                ttblJob.startDate = priorEndDate
                ttblJob.startTime = priorEndTime.
              RUN newEnd (ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                          OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
              ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
              ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
            END.
            RUN firstAvailable IN boardHandle (ttblJob.resource,ROWID(ttblJob),ttblJob.timeSpan,
                                               INPUT-OUTPUT ttblJob.startDateTime,
                                               INPUT-OUTPUT ttblJob.endDateTime,
                                               INPUT-OUTPUT ttblJob.startDate,
                                               INPUT-OUTPUT ttblJob.startTime,
                                               INPUT-OUTPUT ttblJob.endDate,
                                               INPUT-OUTPUT ttblJob.endTime).
            ASSIGN
              ttblJob.jobBGColor = bJobBGColor()
              ttblJob.jobFGColor = bJobFGColor()
              priorEndDate = ttblJob.endDate
              priorEndTime = ttblJob.endTime
              priorDateTime = ttblJob.endDateTime.
          END. /* if ipOption */
          /* insert/push */
          WHEN 2 THEN
          RUN moveExisting IN boardHandle (ttblJob.resource,ttblJob.startDateTime,
                                           ttblJob.endDateTime,ROWID(ttblJob),
                                           ttblJob.jobSequence,YES,YES).
          WHEN 5 THEN DO:
            FIND LAST buffJob WHERE buffJob.resource EQ ttblJob.resource NO-ERROR.
            ASSIGN
              ttblJob.startDate = IF AVAILABLE buffJob THEN buffJob.endDate ELSE TODAY
              ttblJob.startTime = IF AVAILABLE buffJob THEN buffJob.endTime ELSE TIME.
            RUN newEnd (ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                        OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
            ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
            ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
            RUN getPriorJobResource IN boardHandle (ttblJob.job,ttblJob.resourceSequence,ttblJob.startDateTime,
                                                    INPUT-OUTPUT ttblJob.startDate,INPUT-OUTPUT ttblJob.startTime).
            RUN newEnd (ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                        OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
            RUN downtimeSpan (ttblJob.resource,ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                              OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime,OUTPUT ttblJob.downtimeSpan).
            ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
            ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
            ASSIGN
              ttblJob.jobBGColor = bJobBGColor()
              ttblJob.jobFGColor = bJobFGColor().
          END.
        END CASE.
        RUN setResourceSequence (bPJob.resource).
        DELETE bPJob.
      END. /* each bPJob */
    END. /* selectedrowid */
    IF AVAILABLE pendingJob THEN
    GET NEXT {&BROWSE-NAME}.
    ELSE DO:
      {&OPEN-QUERY-browseJob}
      GET FIRST {&BROWSE-NAME}.
    END. /* else */
  END. /* do while */
  selectedRowID = ?.
  SESSION:SET-WAIT-STATE('').
  IF ipOption EQ 2 THEN
  RUN unknownResource IN boardHandle (saveDateTime).
  IF scheduledJob THEN
  RUN buildBoard IN boardHandle (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-Browse sObject 
PROCEDURE reopen-Browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE scheduleJobs sObject 
PROCEDURE scheduleJobs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipOption AS INTEGER NO-UNDO.

  DEFINE VARIABLE scheduledJob AS LOGICAL NO-UNDO.
  DEFINE VARIABLE priorEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE priorEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE priorDateTime AS INTEGER NO-UNDO INITIAL ?.
  DEFINE VARIABLE saveDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE pendingRowID AS ROWID NO-UNDO.
  
  IF selectedRowID EQ ? THEN
  pendingRowID = IF AVAILABLE pendingJob AND setDateTimeOptions EQ 2 THEN ROWID(pendingJob)
                 ELSE ?.
  SESSION:SET-WAIT-STATE('General').
  GET FIRST {&BROWSE-NAME}.
  saveDateTime = IF AVAILABLE pendingJob THEN numericDateTime(pendingJob.endDate,pendingJob.endTime)
                 ELSE numericDateTime(TODAY,TIME).
  DO WHILE AVAILABLE(pendingJob):
    IF selectedRowID EQ ? OR ROWID(pendingJob) EQ selectedRowID THEN DO:
      IF pendingRowID EQ ? OR ROWID(pendingJob) EQ pendingRowID THEN DO:
        CREATE ttblJob.
        BUFFER-COPY pendingJob TO ttblJob.
        ASSIGN
          scheduledJob = YES
          ttblJob.origStartDate = pendingJob.startDate
          ttblJob.origStartTime = pendingJob.startTime
          ttblJob.origEndDate = pendingJob.endDate
          ttblJob.origEndTime = pendingJob.endTime.
        IF selectedRowID NE ? THEN DO:
          ASSIGN
            ttblJob.startDate = TODAY
            ttblJob.startTime = TIME.
          RUN newEnd (ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                      OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
        END. /* selectedrowid */
        ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
        ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
        ASSIGN
          ttblJob.jobBGColor = jobBGColor()
          ttblJob.jobFGColor = jobFGColor().
        CASE ipOption:
          /* slide 1st available */
          WHEN 1 THEN DO:
            IF priorDateTime NE ? AND priorDateTime GE ttblJob.startDateTime THEN DO:
              ASSIGN
                ttblJob.startDate = priorEndDate
                ttblJob.startTime = priorEndTime.
              RUN newEnd (ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                          OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
              ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
              ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
              ASSIGN
                ttblJob.jobBGColor = jobBGColor()
                ttblJob.jobFGColor = jobFGColor().
            END.
            RUN firstAvailable IN boardHandle (ttblJob.resource,ROWID(ttblJob),ttblJob.timeSpan,
                                               INPUT-OUTPUT ttblJob.startDateTime,
                                               INPUT-OUTPUT ttblJob.endDateTime,
                                               INPUT-OUTPUT ttblJob.startDate,
                                               INPUT-OUTPUT ttblJob.startTime,
                                               INPUT-OUTPUT ttblJob.endDate,
                                               INPUT-OUTPUT ttblJob.endTime).
            RUN getPriorJobResource IN boardHandle (ttblJob.job,ttblJob.resourceSequence,ttblJob.startDateTime,
                                                    INPUT-OUTPUT ttblJob.startDate,INPUT-OUTPUT ttblJob.startTime).
            RUN newEnd (ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                        OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
            RUN downtimeSpan (ttblJob.resource,ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                              OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime,OUTPUT ttblJob.downtimeSpan).
            ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
            ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
            ASSIGN
              ttblJob.jobBGColor = jobBGColor()
              ttblJob.jobFGColor = jobFGColor()
              priorEndDate = ttblJob.endDate
              priorEndTime = ttblJob.endTime
              priorDateTime = ttblJob.endDateTime.
          END. /* if ipOption */
          /* insert/push */
          WHEN 2 THEN
          RUN moveExisting IN boardHandle (ttblJob.resource,ttblJob.startDateTime,
                                           ttblJob.endDateTime,ROWID(ttblJob),
                                           ttblJob.jobSequence,YES,YES).
          /* shift jobs */
          WHEN 3 THEN
          RUN shiftJobs IN boardHandle (ttblJob.resource,ttblJob.startDateTime,
                                        ttblJob.endDateTime,ROWID(ttblJob),YES,YES).
          WHEN 5 THEN DO:
            FIND LAST buffJob WHERE buffJob.resource EQ ttblJob.resource NO-ERROR.
            ASSIGN
              ttblJob.startDate = IF AVAILABLE buffJob THEN buffJob.endDate ELSE TODAY
              ttblJob.startTime = IF AVAILABLE buffJob THEN buffJob.endTime ELSE TIME.
            RUN newEnd (ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                        OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
            ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
            ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
            RUN getPriorJobResource IN boardHandle (ttblJob.job,ttblJob.resourceSequence,ttblJob.startDateTime,
                                                    INPUT-OUTPUT ttblJob.startDate,INPUT-OUTPUT ttblJob.startTime).
            RUN newEnd (ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                        OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
            RUN downtimeSpan (ttblJob.resource,ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                              OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime,OUTPUT ttblJob.downtimeSpan).
            ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
            ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
            ASSIGN
              ttblJob.jobBGColor = bJobBGColor()
              ttblJob.jobFGColor = bJobFGColor().
          END.
        END CASE.
        RUN setResourceSequence (pendingJob.resource).
        DELETE pendingJob.
      END. /* if pendingrowid */
    END. /* selectedrowid */
    GET NEXT {&BROWSE-NAME}.
  END. /* do while */
  selectedRowID = ?.
  SESSION:SET-WAIT-STATE('').
  IF ipOption EQ 2 THEN
  RUN unknownResource IN boardHandle (saveDateTime).
  IF scheduledJob THEN
  RUN buildBoard IN boardHandle (NO).

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
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  GET FIRST {&BROWSE-NAME}.
  DO WHILE AVAILABLE(pendingJob):
    ASSIGN
      i = i + 1
      pendingJob.jobSequence = i.
    GET NEXT {&BROWSE-NAME}.
  END.
  RUN reopenBrowse.
  APPLY 'VALUE-CHANGED' TO BROWSE browseJob.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setNextDateTime sObject 
PROCEDURE setNextDateTime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipTime AS INTEGER NO-UNDO.

  GET FIRST {&BROWSE-NAME}.
  DO WHILE AVAILABLE(pendingJob):
    IF ROWID(pendingJob) EQ ipRowID THEN LEAVE.
    GET NEXT {&BROWSE-NAME}.
  END.
  
  IF AVAILABLE pendingJob THEN
  GET NEXT {&BROWSE-NAME}.
  DO WHILE AVAILABLE(pendingJob):
    ASSIGN
      pendingJob.startDate = ipDate 
      pendingJob.startTime = ipTime.
    RUN newEnd (pendingJob.timeSpan,pendingJob.startDate,pendingJob.startTime,
                OUTPUT pendingJob.endDate,OUTPUT pendingJob.endTime).
    ASSIGN
      ipDate = pendingJob.endDate
      ipTime = pendingJob.endTime.
    GET NEXT {&BROWSE-NAME}.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setNextRelated sObject 
PROCEDURE setNextRelated :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipJob AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSeq AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipTime AS INTEGER NO-UNDO.

  FOR EACH bPJob EXCLUSIVE-LOCK
      WHERE bPJob.job EQ ipJob
        AND bPJob.resourceSequence GT ipSeq
      BY bPJob.job EQ ipJob BY bPJob.resourceSequence:
    ASSIGN
      bPJob.startDate = ipDate 
      bPJob.startTime = ipTime.
    RUN newEnd (bPJob.timeSpan,bPJob.startDate,bPJob.startTime,
                OUTPUT bPJob.endDate,OUTPUT bPJob.endTime).
    ASSIGN
      ipDate = bPJob.endDate
      ipTime = bPJob.endTime.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPendingJob sObject 
PROCEDURE setPendingJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE priorEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE priorEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE priorTimeSpan AS INTEGER NO-UNDO.

  RUN getConfiguration.
  FOR EACH pendingJob EXCLUSIVE-LOCK
      BREAK BY pendingJob.job DESCENDING
            BY pendingJob.resourceSequence DESCENDING:
    IF FIRST-OF(pendingJob.job) THEN
    DO:
      ASSIGN
        priorEndDate = pendingJob.dueDate
        priorEndTime = pendingJob.dueTime
        priorTimeSpan = pendingDays * 86400.
    END.
    RUN newStart (priorTimeSpan,priorEndDate,priorEndTime,
                  OUTPUT pendingJob.endDate,OUTPUT pendingJob.endTime).
    RUN newStart (pendingjob.origStartTime,pendingJob.endDate,pendingJob.endTime,
                  OUTPUT pendingJob.startDate,OUTPUT pendingJob.startTime).
    ASSIGN
      pendingJob.timeSpan = pendingJob.origStartTime
      priorEndDate = pendingJob.startDate
      priorEndTime = pendingJob.startTime
      priorTimeSpan = 0.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPriorDateTime sObject 
PROCEDURE setPriorDateTime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipTime AS INTEGER NO-UNDO.

  GET LAST {&BROWSE-NAME}.
  DO WHILE AVAILABLE(pendingJob):
    IF ROWID(pendingJob) EQ ipRowID THEN LEAVE.
    GET PREV {&BROWSE-NAME}.
  END.
  
  IF AVAILABLE pendingJob THEN
  GET PREV {&BROWSE-NAME}.
  DO WHILE AVAILABLE(pendingJob):
    ASSIGN
      pendingJob.endDate = ipDate 
      pendingJob.endTime = ipTime.
    RUN newStart (pendingJob.timeSpan,pendingJob.endDate,pendingJob.endTime,
                  OUTPUT pendingJob.startDate,OUTPUT pendingJob.startTime).
    ASSIGN
      ipDate = pendingJob.startDate
      ipTime = pendingJob.startTime.
    GET PREV {&BROWSE-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPriorRelated sObject 
PROCEDURE setPriorRelated :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipJob AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSeq AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipTime AS INTEGER NO-UNDO.

  FOR EACH bPJob EXCLUSIVE-LOCK
      WHERE bPJob.job EQ ipJob
        AND bPJob.resourceSequence LT ipSeq
      BY bPJob.job DESCENDING BY bPJob.resourceSequence DESCENDING:
    ASSIGN
      bPJob.endDate = ipDate 
      bPJob.endTime = ipTime.
    RUN newStart (bPJob.timeSpan,bPJob.endDate,bPJob.endTime,
                  OUTPUT bPJob.startDate,OUTPUT bPJob.startTime).
    ASSIGN
      ipDate = bPJob.startDate
      ipTime = bPJob.startTime.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setResourceSequence sObject 
PROCEDURE setResourceSequence :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  FOR EACH buffJob EXCLUSIVE-LOCK WHERE buffJob.resource EQ ipResource
      BY buffJob.startDate BY buffJob.startTime:
    ASSIGN
      i = i + 1
      buffJob.jobSequence = i.
  END.

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
  DEFINE VARIABLE lowValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE hiValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  
  IF INTEGER(cellColumn[1]:SCREEN-VALUE) NE pendingJob.jobSequence THEN
  DO:
    ASSIGN
      i = IF pendingJob.jobSequence LT INTEGER(cellColumn[1]:SCREEN-VALUE) THEN -1
          ELSE 1
      lowValue = IF i EQ -1 THEN pendingJob.jobSequence ELSE INTEGER(cellColumn[1]:SCREEN-VALUE)
      hiValue = IF i EQ 1 THEN pendingJob.jobSequence ELSE INTEGER(cellColumn[1]:SCREEN-VALUE)
      pendingJob.jobSequence = INTEGER(cellColumn[1]:SCREEN-VALUE).
    RUN newSequence (pendingJob.resource,lowValue,hiValue,i).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION bJobBGColor sObject 
FUNCTION bJobBGColor RETURNS INTEGER
  ( /* parameter-definitions */ ) :
  &SCOPED-DEFINE colorJobTable bPJob
  {{&includes}/jobBGColor.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION bJobFGColor sObject 
FUNCTION bJobFGColor RETURNS INTEGER
  ( /* parameter-definitions */ ) :
  &SCOPED-DEFINE colorJobTable bPJob
  {{&includes}/jobFGColor.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkDates sObject 
FUNCTION checkDates RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE continue AS LOGICAL NO-UNDO INITIAL TRUE.

  IF selectedRowID NE ? THEN RETURN continue.

  SESSION:SET-WAIT-STATE('General').
  GET FIRST {&BROWSE-NAME}.
  DO WHILE AVAILABLE(pendingJob):
    IF pendingJob.startDate GT TODAY + pendingLastDay OR
       pendingJob.endDate GT TODAY + pendingLastDay THEN DO:
      MESSAGE 'Job' pendingJob.job 'has a Start:'
        pendingJob.startDate 'or End:' pendingJob.endDate
        'Date exceeding' TODAY + pendingLastDay SKIP(1)
        'Job cannot be scheduled this far in Advance!!!' VIEW-AS ALERT-BOX
        TITLE 'Based on Configuration Value Setting'.
      continue = NO.
      LEAVE.
    END.
    ELSE
    IF pendingJob.startDate GT TODAY + pendingOver OR
       pendingJob.endDate GT TODAY + pendingOver THEN DO:
      MESSAGE 'Job' pendingJob.job 'has a Start:'
        pendingJob.startDate 'or End:' pendingJob.endDate
        'Date exceeding' TODAY + pendingOver SKIP(1)
        'Continue to Schedule Selected Jobs?' VIEW-AS ALERT-BOX
        QUESTION BUTTONS YES-NO UPDATE continue.
      LEAVE.
    END.
    GET NEXT {&BROWSE-NAME}.
  END. /* each bjobid */
  SESSION:SET-WAIT-STATE('').
  RETURN continue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION jobBGColor sObject 
FUNCTION jobBGColor RETURNS INTEGER
  ( /* parameter-definitions */ ) :
  &SCOPED-DEFINE colorJobTable pendingJob
  {{&includes}/jobBGColor.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION jobFGColor sObject 
FUNCTION jobFGColor RETURNS INTEGER
  ( /* parameter-definitions */ ) :
  &SCOPED-DEFINE colorJobTable pendingJob
  {{&includes}/jobFGColor.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

