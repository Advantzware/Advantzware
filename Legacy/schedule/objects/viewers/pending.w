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

&SCOPED-DEFINE colorJobTable pendingJob

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/filterVars.i}
{{&includes}/ttblJob.i}
{{&viewers}/includes/sharedVars.i NEW}
&SCOPED-DEFINE useTable ttblJob
{{&includes}/jobStatusFunc.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE ascendingFlag AS LOGICAL NO-UNDO INITIAL YES.
DEFINE VARIABLE bgColorJob AS INTEGER NO-UNDO.
DEFINE VARIABLE boardHandle AS WIDGET NO-UNDO.
DEFINE VARIABLE columnLabel AS CHARACTER NO-UNDO INITIAL 'job'.
DEFINE VARIABLE correct-error AS LOGICAL NO-UNDO.
DEFINE VARIABLE fgJobColor AS INTEGER NO-UNDO.
DEFINE VARIABLE gapSpan AS INTEGER NO-UNDO.
DEFINE VARIABLE idx AS INTEGER NO-UNDO.
DEFINE VARIABLE newTime AS INTEGER NO-UNDO.
DEFINE VARIABLE resourceValue AS CHARACTER NO-UNDO.

DEFINE BUFFER bPendingJob FOR pendingJob.

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
{{&includes}/lockWindowUpdate.i}
&ENDIF
{{&viewers}/includes/pendingJob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME jobID

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES jobID pendingJob

/* Definitions for BROWSE jobID                                         */
&Scoped-define FIELDS-IN-QUERY-jobID jobID.job jobID.jobSelected jobID.dueDate   
&Scoped-define ENABLED-FIELDS-IN-QUERY-jobID   
&Scoped-define SELF-NAME jobID
&Scoped-define QUERY-STRING-jobID FOR EACH jobID NO-LOCK   WHERE jobID.job BEGINS jobPhrase OR jobPhrase EQ '' BY jobID.jobSort
&Scoped-define OPEN-QUERY-jobID OPEN QUERY {&SELF-NAME} FOR EACH jobID NO-LOCK   WHERE jobID.job BEGINS jobPhrase OR jobPhrase EQ '' BY jobID.jobSort.
&Scoped-define TABLES-IN-QUERY-jobID jobID
&Scoped-define FIRST-TABLE-IN-QUERY-jobID jobID


/* Definitions for BROWSE pendingJob                                    */
&Scoped-define FIELDS-IN-QUERY-pendingJob pendingJob.resourceSequence pendingJob.resource pendingJob.startDate STRING(pendingJob.startTime,'HH:MM:SSam') pendingJob.endDate STRING(pendingJob.endTime,'HH:MM:SSam') /* pendingJob.dueDate STRING(pendingJob.dueTime,'HH:MM:SSam') */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-pendingJob   
&Scoped-define SELF-NAME pendingJob
&Scoped-define QUERY-STRING-pendingJob FOR EACH pendingJob NO-LOCK   WHERE pendingJob.job EQ jobID.job BY pendingJob.resourceSequence
&Scoped-define OPEN-QUERY-pendingJob OPEN QUERY {&SELF-NAME} FOR EACH pendingJob NO-LOCK   WHERE pendingJob.job EQ jobID.job BY pendingJob.resourceSequence.
&Scoped-define TABLES-IN-QUERY-pendingJob pendingJob
&Scoped-define FIRST-TABLE-IN-QUERY-pendingJob pendingJob


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-jobID}~
    ~{&OPEN-QUERY-pendingJob}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnPrint btnComplete btnMoveResource ~
btnRefresh RECT-1 RECT-2 RECT-3 RECT-5 RECT-4 jobPhrase dateTime jobID ~
pendingJob newDate btnCalendar newHour newMinute newAMPM btnCurrentDateTime ~
gapDay gapHour gapMinute gapTime setDateTimeOptions btnSetDateTime ~
btnSelectUnselect btnClearSelections setPlacementOptions btnScheduleJob ~
instructions4Steps btn4Steps 
&Scoped-Define DISPLAYED-OBJECTS jobPhrase dateTime newDate newHour ~
newMinute newAMPM gapDay gapHour gapMinute gapTime setDateTimeOptions ~
setPlacementOptions instructions4Steps pendingMessage 

/* Custom List Definitions                                              */
/* dateTimeFields,List-2,List-3,List-4,List-5,List-6                    */
&Scoped-define dateTimeFields newDate newHour newMinute newAMPM gapDay ~
gapHour gapMinute 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD numericDateTime sObject 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn4Steps 
     LABEL "&Execute Steps 1 thru 4" 
     SIZE 28 BY 1.

DEFINE BUTTON btnCalendar 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.4 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnClearSelections 
     LABEL "&Clear Jobs Selected" 
     SIZE 28 BY 1.

DEFINE BUTTON btnComplete 
     IMAGE-UP FILE "schedule/images/save.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Complete Job".

DEFINE BUTTON btnCurrentDateTime 
     IMAGE-UP FILE "schedule/images/clock1.bmp":U
     LABEL "&1" 
     SIZE 4.4 BY 1.05 TOOLTIP "Set Using System Current Date && Time".

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
     LABEL "&4. Schedule Selected Jobs" 
     SIZE 28 BY 1.

DEFINE BUTTON btnSelectUnselect 
     LABEL "&3. Select / Unselect Job" 
     SIZE 28 BY 1.

DEFINE BUTTON btnSetDateTime 
     LABEL "&2. Set Job Date && Time" 
     SIZE 28 BY 1.

DEFINE VARIABLE newAMPM AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE instructions4Steps AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 28 BY 4.76
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE gapDay AS CHARACTER FORMAT "X(2)":U 
     LABEL "&Gap" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE gapHour AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE gapMinute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE jobPhrase AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Job" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE newDate AS DATE FORMAT "99/99/9999":U 
     LABEL "&Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE newHour AS CHARACTER FORMAT "X(2)":U 
     LABEL "&Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE newMinute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE pendingMessage AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 100 BY .62
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE dateTime AS CHARACTER INITIAL "Start" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Start", "Start",
"End", "End"
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE gapTime AS CHARACTER INITIAL "Start" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Start", "Start",
"End", "End"
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE setDateTimeOptions AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "&All Resources", 1,
"This &Resource Only", 2
     SIZE 28 BY 1.67
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE setPlacementOptions AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Slide &1st Available", 1,
"Insert / &Push Existing", 2,
"&Shift Jobs", 3,
"Schedule As &Is", 4,
"&Last", 5
     SIZE 28 BY 3.33
     BGCOLOR 2 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 149.6 BY 1.19.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 30 BY 3.81
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 30 BY 3.1
     BGCOLOR 9 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 30 BY 2.38
     BGCOLOR 14 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 30 BY 4.76
     BGCOLOR 2 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 30 BY 6.19
     BGCOLOR 12 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY jobID FOR 
      jobID SCROLLING.

DEFINE QUERY pendingJob FOR 
      pendingJob SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE jobID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS jobID sObject _FREEFORM
  QUERY jobID DISPLAY
      jobID.job FORMAT 'X(19)' LABEL-BGCOLOR 14
      jobID.jobSelected FORMAT 'Y/' LABEL 'S'
      jobID.dueDate FORMAT '99/99/9999' LABEL 'Due' LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 39 BY 21.19.

DEFINE BROWSE pendingJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS pendingJob sObject _FREEFORM
  QUERY pendingJob DISPLAY
      pendingJob.resourceSequence COLUMN-LABEL 'Seq'
  pendingJob.resource
  pendingJob.startDate FORMAT '99/99/9999' LABEL 'Start'
  STRING(pendingJob.startTime,'HH:MM:SSam') FORMAT 'X(11)' LABEL 'Time'
  pendingJob.endDate FORMAT '99/99/9999' LABEL 'End'
  STRING(pendingJob.endTime,'HH:MM:SSam') FORMAT 'X(11)' LABEL 'Time'
        /*
  pendingJob.dueDate FORMAT '99/99/9999' LABEL 'Due'
  STRING(pendingJob.dueTime,'HH:MM:SSam') FORMAT 'X(11)' LABEL 'Time'
  */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 78.6 BY 21.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnPrint AT ROW 1.05 COL 30 HELP
          "Click to Access Print Utility"
     btnComplete AT ROW 1.05 COL 35 HELP
          "Click to Complete Job"
     btnMoveResource AT ROW 1.05 COL 40 HELP
          "Click to Access Move Resource"
     btnRefresh AT ROW 1.05 COL 45 HELP
          "Click to Refresh Resource Browser"
     jobPhrase AT ROW 1.1 COL 4 COLON-ALIGNED
     dateTime AT ROW 2.19 COL 15 NO-LABEL
     jobID AT ROW 2.19 COL 33
     pendingJob AT ROW 2.19 COL 72
     newDate AT ROW 3.38 COL 6 COLON-ALIGNED
     btnCalendar AT ROW 3.38 COL 27 HELP
          "Click to Access Popup Calendar"
     newHour AT ROW 4.57 COL 6 COLON-ALIGNED HELP
          "Enter Starting Hour"
     newMinute AT ROW 4.57 COL 12 COLON-ALIGNED HELP
          "Enter Starting Minute"
     newAMPM AT ROW 4.57 COL 17 COLON-ALIGNED NO-LABEL
     btnCurrentDateTime AT ROW 4.57 COL 27 HELP
          "Set Using System Current Date && Time"
     gapDay AT ROW 5.76 COL 6 COLON-ALIGNED HELP
          "Enter Number of Gap Days"
     gapHour AT ROW 5.76 COL 12 COLON-ALIGNED HELP
          "Enter Number of Gap Hours"
     gapMinute AT ROW 5.76 COL 17 COLON-ALIGNED HELP
          "Enter Number of Gap Minutes"
     gapTime AT ROW 5.76 COL 23 NO-LABEL
     setDateTimeOptions AT ROW 7.19 COL 3 NO-LABEL
     btnSetDateTime AT ROW 8.86 COL 3
     btnSelectUnselect AT ROW 10.29 COL 3
     btnClearSelections AT ROW 11.24 COL 3 HELP
          "Clear Pending Job Selections"
     setPlacementOptions AT ROW 12.67 COL 3 NO-LABEL
     btnScheduleJob AT ROW 16 COL 3
     instructions4Steps AT ROW 17.43 COL 3 NO-LABEL
     btn4Steps AT ROW 22.19 COL 3 HELP
          "Click to Execute 4 Step Scheduling of Job"
     pendingMessage AT ROW 1.24 COL 48 COLON-ALIGNED NO-LABEL
     "&1. Date&&Time:" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 2.19 COL 1
     RECT-1 AT ROW 1 COL 1
     RECT-2 AT ROW 3.14 COL 2
     RECT-3 AT ROW 6.95 COL 2
     RECT-5 AT ROW 12.43 COL 2
     RECT-4 AT ROW 10.05 COL 2
     RECT-6 AT ROW 17.19 COL 2
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
         HEIGHT             = 22.52
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
/* BROWSE-TAB jobID dateTime F-Main */
/* BROWSE-TAB pendingJob jobID F-Main */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:HEIGHT           = 22.52
       FRAME F-Main:WIDTH            = 150.

ASSIGN 
       btnComplete:PRIVATE-DATA IN FRAME F-Main     = 
                "Complete Job".

ASSIGN 
       btnPrint:PRIVATE-DATA IN FRAME F-Main     = 
                "boardObject".

/* SETTINGS FOR FILL-IN gapDay IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN gapHour IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN gapMinute IN FRAME F-Main
   1                                                                    */
ASSIGN 
       instructions4Steps:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       jobID:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR COMBO-BOX newAMPM IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN newDate IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN newHour IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN newMinute IN FRAME F-Main
   1                                                                    */
ASSIGN 
       pendingJob:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN pendingMessage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE jobID
/* Query rebuild information for BROWSE jobID
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH jobID NO-LOCK
  WHERE jobID.job BEGINS jobPhrase OR jobPhrase EQ '' BY jobID.jobSort
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE jobID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE pendingJob
/* Query rebuild information for BROWSE pendingJob
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH pendingJob NO-LOCK
  WHERE pendingJob.job EQ jobID.job BY pendingJob.resourceSequence
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE pendingJob */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btn4Steps
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn4Steps sObject
ON CHOOSE OF btn4Steps IN FRAME F-Main /* Execute Steps 1 thru 4 */
DO:
  APPLY 'CHOOSE':U TO btnCurrentDateTime.
  APPLY 'CHOOSE':U TO btnSetDateTime.
  IF AVAILABLE jobID THEN
  jobID.jobSelected = NO.
  APPLY 'CHOOSE':U TO btnSelectUnselect.
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


&Scoped-define SELF-NAME btnClearSelections
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearSelections sObject
ON CHOOSE OF btnClearSelections IN FRAME F-Main /* Clear Jobs Selected */
DO:
  FOR EACH jobID EXCLUSIVE-LOCK:
    jobID.jobSelected = NO.
  END.
  RUN openQueryJobID.
  /*{&OPEN-QUERY-jobID}*/
  APPLY 'VALUE-CHANGED' TO BROWSE jobID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnComplete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnComplete sObject
ON CHOOSE OF btnComplete IN FRAME F-Main
DO:
  IF AVAILABLE pendingJob THEN
  RUN {&prompts}/completePending.w (ROWID(pendingJob),'{&Board}').
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
ON CHOOSE OF btnScheduleJob IN FRAME F-Main /* 4. Schedule Selected Jobs */
DO:
  IF NOT checkDates() THEN RETURN NO-APPLY.
  RUN scheduleJobs (setPlacementOptions).
  RUN openQueryJobID.
  APPLY 'VALUE-CHANGED' TO BROWSE jobID.
  APPLY 'ENTRY':U TO jobPhrase.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectUnselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectUnselect sObject
ON CHOOSE OF btnSelectUnselect IN FRAME F-Main /* 3. Select / Unselect Job */
DO:
  IF AVAILABLE jobID THEN
  ASSIGN
    jobID.jobSelected = NOT jobID.jobSelected
    cellColumn[1]:BGCOLOR = IF jobID.jobSelected THEN 14 ELSE ?
    cellColumn[2]:BGCOLOR = IF jobID.jobSelected THEN 14 ELSE ?
    cellColumn[2]:SCREEN-VALUE = IF jobID.jobSelected THEN 'Y' ELSE ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSetDateTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSetDateTime sObject
ON CHOOSE OF btnSetDateTime IN FRAME F-Main /* 2. Set Job Date  Time */
DO:
  IF NOT AVAILABLE pendingJob THEN RETURN NO-APPLY.
  ASSIGN {&dateTimeFields}
    newTime = (INTEGER(newHour) + (IF newAMPM EQ 'PM' AND INTEGER(newHour) NE 12 THEN 12 ELSE
                                   IF newAMPM EQ 'AM' AND INTEGER(newHour) EQ 12 THEN -12 ELSE 0)) * 3600 +
               INTEGER(newMinute) * 60
    gapSpan = INTEGER(gapDay) * 86400 + INTEGER(gapHour) * 3600 + INTEGER(gapMinute) * 60.
  IF dateTime EQ 'Start' THEN
  DO:
    ASSIGN
      pendingJob.startDate = newDate
      pendingJob.startTime = newTime.
    RUN newEnd (pendingJob.timeSpan,pendingJob.startDate,pendingJob.startTime,
                OUTPUT pendingJob.endDate,OUTPUT pendingJob.endTime).
  END.
  ELSE
  DO:
    ASSIGN
      pendingJob.endDate = newDate
      pendingJob.endTime = newTime.
    RUN newStart (pendingJob.timeSpan,pendingJob.endDate,pendingJob.endTime,
                  OUTPUT pendingJob.startDate,OUTPUT pendingJob.startTime).
  END.
  IF setDateTimeOptions EQ 1 THEN /* all */
  DO:
    RUN setPriorDateTime (pendingJob.job,pendingJob.resourceSequence,
                          pendingJob.startDate,pendingJob.startTime,
                          gapSpan,gapTime).
    
    IF gapSpan NE 0 AND gapTime EQ 'Start' THEN
    RUN setNextDateTime (pendingJob.job,pendingJob.resourceSequence,
                         pendingJob.startDate,pendingJob.startTime,
                         gapSpan,gapTime).
    ELSE
    RUN setNextDateTime (pendingJob.job,pendingJob.resourceSequence,
                         pendingJob.endDate,pendingJob.endTime,
                         gapSpan,gapTime).
  END.
  {&OPEN-QUERY-pendingJob}
  APPLY 'VALUE-CHANGED' TO BROWSE pendingJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dateTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dateTime sObject
ON VALUE-CHANGED OF dateTime IN FRAME F-Main
DO:
  ASSIGN {&SELF-NAME}.
  IF AVAILABLE pendingJob THEN
  APPLY 'VALUE-CHANGED' TO BROWSE pendingJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gapDay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gapDay sObject
ON LEAVE OF gapDay IN FRAME F-Main /* Gap */
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 30.
  {{&includes}/Pro/entryerr.i &error-message="Invalid Days, range = 1 to 30"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gapDay sObject
ON RETURN OF gapDay IN FRAME F-Main /* Gap */
DO:
  APPLY 'LEAVE' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gapHour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gapHour sObject
ON LEAVE OF gapHour IN FRAME F-Main
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 23.
  {{&includes}/Pro/entryerr.i &error-message="Invalid Hour, range = 1 to 23"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gapHour sObject
ON RETURN OF gapHour IN FRAME F-Main
DO:
  APPLY 'LEAVE' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gapMinute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gapMinute sObject
ON LEAVE OF gapMinute IN FRAME F-Main
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {{&includes}/Pro/entryerr.i &error-message="Invalid Minute, range = 0 to 59"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gapMinute sObject
ON RETURN OF gapMinute IN FRAME F-Main
DO:
  APPLY 'LEAVE' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gapTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gapTime sObject
ON VALUE-CHANGED OF gapTime IN FRAME F-Main
DO:
  ASSIGN {&SELF-NAME}.
  IF AVAILABLE pendingJob THEN
  APPLY 'VALUE-CHANGED' TO BROWSE pendingJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME jobID
&Scoped-define SELF-NAME jobID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobID sObject
ON ANY-PRINTABLE OF jobID IN FRAME F-Main
DO:
  jobPhrase:SCREEN-VALUE = jobPhrase:SCREEN-VALUE + CHR(LASTKEY).
  APPLY 'VALUE-CHANGED' TO jobPhrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobID sObject
ON BACKSPACE OF jobID IN FRAME F-Main
DO:
  jobPhrase:SCREEN-VALUE = SUBSTR(jobPhrase:SCREEN-VALUE,1,LENGTH(jobPhrase:SCREEN-VALUE) - 1).
  APPLY 'VALUE-CHANGED' TO jobPhrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobID sObject
ON DEFAULT-ACTION OF jobID IN FRAME F-Main
DO:
  APPLY 'CHOOSE' TO btnSelectUnselect.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobID sObject
ON ROW-DISPLAY OF jobID IN FRAME F-Main
DO:
  cellColumn[1]:BGCOLOR = IF jobID.jobSelected THEN 14 ELSE ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobID sObject
ON START-SEARCH OF jobID IN FRAME F-Main
DO:
  IF {&BROWSE-NAME}:CURRENT-COLUMN:LABEL-BGCOLOR EQ 14 THEN DO:
    ASSIGN
      columnLabel = {&BROWSE-NAME}:CURRENT-COLUMN:NAME
      ascendingFlag = NOT ascendingFlag.
    RUN openQueryJobID.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobID sObject
ON VALUE-CHANGED OF jobID IN FRAME F-Main
DO:
  {&OPEN-QUERY-pendingJob}
  IF NOT AVAILABLE jobID THEN RETURN NO-APPLY.
  APPLY 'VALUE-CHANGED' TO BROWSE pendingJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME jobPhrase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobPhrase sObject
ON VALUE-CHANGED OF jobPhrase IN FRAME F-Main /* Job */
DO:
  ASSIGN {&SELF-NAME}.
  RUN openQueryJobID.
  /*{&OPEN-QUERY-jobID}*/
  APPLY 'VALUE-CHANGED' TO jobID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME newDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newDate sObject
ON HELP OF newDate IN FRAME F-Main /* Date */
DO:
  {{&includes}/calendar.i}
  APPLY 'LEAVE' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newDate sObject
ON LEAVE OF newDate IN FRAME F-Main /* Date */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME newHour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newHour sObject
ON LEAVE OF newHour IN FRAME F-Main /* Time */
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 1 OR INTEGER(SELF:SCREEN-VALUE) GT 12.
  {{&includes}/Pro/entryerr.i &error-message="Invalid Hour, range = 1 to 12"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newHour sObject
ON RETURN OF newHour IN FRAME F-Main /* Time */
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


&Scoped-define BROWSE-NAME pendingJob
&Scoped-define SELF-NAME pendingJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pendingJob sObject
ON ANY-PRINTABLE OF pendingJob IN FRAME F-Main
DO:
  jobPhrase:SCREEN-VALUE = jobPhrase:SCREEN-VALUE + CHR(LASTKEY).
  APPLY 'VALUE-CHANGED' TO jobPhrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pendingJob sObject
ON BACKSPACE OF pendingJob IN FRAME F-Main
DO:
  jobPhrase:SCREEN-VALUE = SUBSTR(jobPhrase:SCREEN-VALUE,1,LENGTH(jobPhrase:SCREEN-VALUE) - 1).
  APPLY 'VALUE-CHANGED' TO jobPhrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pendingJob sObject
ON ROW-DISPLAY OF pendingJob IN FRAME F-Main
DO:
  bgColorJob = jobBGColor().
  DO idx = 10 TO 11:
    IF NOT VALID-HANDLE(cellColumn[idx]) THEN LEAVE.
    ASSIGN
      cellColumn[idx]:BGCOLOR = bgColorJob
      cellColumn[idx]:FGCOLOR = jobFGColor().
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pendingJob sObject
ON VALUE-CHANGED OF pendingJob IN FRAME F-Main
DO:
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


&Scoped-define BROWSE-NAME jobID
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK sObject 


/* ***************************  Main Block  *************************** */

{{&viewers}/includes/winTitle.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueryJobID sObject 
PROCEDURE openQueryJobID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CASE columnLabel:
    WHEN 'job' THEN
      IF ascendingFlag THEN
      OPEN QUERY jobID FOR EACH jobID NO-LOCK
        WHERE jobID.job BEGINS jobPhrase OR jobPhrase EQ ''
        BY jobID.jobSort.
      ELSE
      OPEN QUERY jobID FOR EACH jobID NO-LOCK
        WHERE jobID.job BEGINS jobPhrase OR jobPhrase EQ ''
        BY jobID.jobSort DESCENDING.
    WHEN 'dueDate' THEN
      IF ascendingFlag THEN
      OPEN QUERY jobID FOR EACH jobID NO-LOCK
        WHERE jobID.job BEGINS jobPhrase OR jobPhrase EQ ''
        BY jobID.dueDate BY jobID.jobSort.
      ELSE
      OPEN QUERY jobID FOR EACH jobID NO-LOCK
        WHERE jobID.job BEGINS jobPhrase OR jobPhrase EQ ''
        BY jobID.dueDate DESCENDING BY jobID.jobSort DESCENDING.
  END CASE.

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

  RUN setPendingJob. /* found in includes/pendingJob.i */
  ASSIGN
    cellColumn[1] = jobID:GET-BROWSE-COLUMN(1) IN FRAME {&FRAME-NAME}
    cellColumn[2] = jobID:GET-BROWSE-COLUMN(2)
    cellColumn[10] = pendingJob:GET-BROWSE-COLUMN(1)
    cellColumn[11] = pendingJob:GET-BROWSE-COLUMN(2)
    boardHandle = ipHandle
    instructions4Steps:SCREEN-VALUE = 'After Highlighting Job,' + CHR(10) +
      'Click to Quick Schedule:' + CHR(10) + CHR(10) +
      '1. Get Current Date & Time' + CHR(10) +
      '2. Set Job Date & Time' + CHR(10) +
      '3. Select / Unselect Job' + CHR(10) +
      '4. Schedule Selected Job'
    pendingMessage:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
        'Start/End Date/Time are based on ' +
        STRING(pendingDays) + ' days prior to last Job Sequence Due Date/Time'.
  RUN openQueryJobID.
  /*{&OPEN-QUERY-jobID}*/
  APPLY 'VALUE-CHANGED' TO jobID IN FRAME {&FRAME-NAME}.
  APPLY 'ENTRY':U TO BROWSE jobID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopenBrowse sObject 
PROCEDURE reopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN setPendingJob.
  RUN openQueryJobID.
  /*{&OPEN-QUERY-jobID}*/
  {&OPEN-QUERY-pendingJob}

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
  DEFINE VARIABLE saveDateTime AS DECIMAL NO-UNDO INITIAL ?.
  DEFINE VARIABLE dueDateTime AS DECIMAL NO-UNDO INITIAL ?.
  
  DEFINE BUFFER bJobID FOR jobID.
  
  SESSION:SET-WAIT-STATE('General').
  FOR EACH bJobID EXCLUSIVE-LOCK WHERE bJobID.jobSelected EQ YES:
    FOR EACH bPendingJob OF bJobID EXCLUSIVE-LOCK
        BY bPendingJob.job BY bPendingJob.resourceSequence:
      IF saveDateTime EQ ? THEN
      saveDateTime = numericDateTime(bPendingJob.endDate,bPendingJob.endTime).
      IF dueDateTime EQ ? THEN
      dueDateTime = numericDateTime(bPendingJob.dueDate,bPendingJob.dueTime).
      CREATE ttblJob.
      BUFFER-COPY bPendingJob TO ttblJob.
      ASSIGN
        scheduledJob = YES
        ttblJob.origStartDate = bPendingJob.startDate
        ttblJob.origStartTime = bPendingJob.startTime
        ttblJob.origEndDate = bPendingJob.endDate
        ttblJob.origEndTime = bPendingJob.endTime.
      ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
      ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
      ASSIGN
        ttblJob.jobBGColor = bJobBGColor()
        ttblJob.jobFGColor = bJobFGColor()
        ttblJob.statusLabel = jobStatus()
        .
      CASE ipOption:
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
              ttblJob.jobBGColor = bJobBGColor()
              ttblJob.jobFGColor = bJobFGColor()
              ttblJob.statusLabel = jobStatus()
              .
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
            ttblJob.jobBGColor = bJobBGColor()
            ttblJob.jobFGColor = bJobFGColor()
            ttblJob.statusLabel = jobStatus()
            .
          ASSIGN
            priorEndDate = ttblJob.endDate
            priorEndTime = ttblJob.endTime
            priorDateTime = ttblJob.endDateTime
            .
        END. /* if ipOption */
        WHEN 2 THEN
        RUN moveExisting IN boardHandle (ttblJob.resource,ttblJob.startDateTime,
                                         ttblJob.endDateTime,ROWID(ttblJob),
                                         ttblJob.jobSequence,YES,YES).
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
            ttblJob.jobFGColor = bJobFGColor()
            ttblJob.statusLabel = jobStatus()
            .
        END.
      END CASE.
      RUN setResourceSequence (bPendingJob.resource).
      ttblJob.sequenced = YES.
      dueDateTime = numericDateTime(ttblJob.dueDate,ttblJob.dueTime).
      DELETE bPendingJob.
    END. /* each bPendingJob */
    IF ttblJob.endDateTime GT dueDateTime THEN
    MESSAGE 'Job:' ttblJob.job 'is Due:' ttblJob.dueDate '@'
      STRING(ttblJob.dueTime,'HH:MM:SS am') SKIP(1)
      'Scheduled to Finish:' ttblJob.endDate '@' STRING(ttblJob.endTime,'HH:MM:SS am')
        VIEW-AS ALERT-BOX TITLE 'Scheduled Past Due'.
    priorDateTime = ?.
    DELETE bJobID.
  END. /* each bjobid */
  SESSION:SET-WAIT-STATE('').
  IF ipOption EQ 2 THEN
  RUN unknownResource IN boardHandle (saveDateTime).
  IF scheduledJob THEN
  RUN buildBoard IN boardHandle (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setDateTime sObject 
PROCEDURE setDateTime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE priorEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE priorEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE priorTimeSpan AS INTEGER NO-UNDO.

  FOR EACH pendingJob EXCLUSIVE-LOCK
      BREAK BY pendingJob.job DESCENDING
            BY pendingJob.resourceSequence DESCENDING:
    IF FIRST-OF(pendingJob.job) THEN
    DO:
      CREATE jobID.
      ASSIGN
        jobID.job = pendingJob.job
        jobID.jobSort = pendingJob.jobSort
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setNextDateTime sObject 
PROCEDURE setNextDateTime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipJob AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSeq AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipTime AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipGapSpan AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipGapTime AS CHARACTER NO-UNDO.

  IF ipGapSpan NE 0 THEN
  RUN newEnd (ipGapSpan,ipDate,ipTime,OUTPUT ipDate,OUTPUT ipTime).
  FOR EACH bPendingJob EXCLUSIVE-LOCK
      WHERE bPendingJob.job EQ ipJob
        AND bPendingJob.resourceSequence GT ipSeq
      BY bPendingJob.job EQ ipJob BY bPendingJob.resourceSequence:
    ASSIGN
      bPendingJob.startDate = ipDate 
      bPendingJob.startTime = ipTime.
    RUN newEnd (bPendingJob.timeSpan,bPendingJob.startDate,bPendingJob.startTime,
                OUTPUT bPendingJob.endDate,OUTPUT bPendingJob.endTime).
    ASSIGN
      ipDate = bPendingJob.endDate
      ipTime = bPendingJob.endTime.
    IF ipGapSpan NE 0 THEN DO:
      IF ipGapTime EQ 'Start' THEN
      RUN newEnd (ipGapSpan,bPendingJob.startDate,bPendingJob.startTime,
                  OUTPUT ipDate,OUTPUT ipTime).
      ELSE
      RUN newEnd (ipGapSpan,ipDate,ipTime,OUTPUT ipDate,OUTPUT ipTime).
    END.
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
  DEFINE INPUT PARAMETER ipJob AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSeq AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipTime AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipGapSpan AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipGapTime AS CHARACTER NO-UNDO.

  IF ipGapSpan NE 0 THEN
  RUN newStart (ipGapSpan,ipDate,ipTime,OUTPUT ipDate,OUTPUT ipTime).
  FOR EACH bPendingJob EXCLUSIVE-LOCK
      WHERE bPendingJob.job EQ ipJob
        AND bPendingJob.resourceSequence LT ipSeq
      BY bPendingJob.job DESCENDING BY bPendingJob.resourceSequence DESCENDING:
    ASSIGN
      bPendingJob.endDate = ipDate 
      bPendingJob.endTime = ipTime.
    RUN newStart (bPendingJob.timeSpan,bPendingJob.endDate,bPendingJob.endTime,
                  OUTPUT bPendingJob.startDate,OUTPUT bPendingJob.startTime).
    ASSIGN
      ipDate = bPendingJob.startDate
      ipTime = bPendingJob.startTime.
    IF ipGapSpan NE 0 THEN
    RUN newStart (ipGapSpan,ipDate,ipTime,OUTPUT ipDate,OUTPUT ipTime).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSize sObject 
PROCEDURE setSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipHeight AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipWidth AS DECIMAL NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN lockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF
  ASSIGN
    pendingJob:HIDDEN IN FRAME {&FRAME-NAME} = YES
    jobID:HIDDEN = YES
    RECT-1:HIDDEN = YES
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = ipHeight
    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = ipHeight
    jobID:HEIGHT-PIXELS = ipHeight - 28
    pendingJob:HEIGHT-PIXELS = ipHeight - 28
    RECT-1:HIDDEN = NO
    jobID:HIDDEN = NO
    pendingJob:HIDDEN = NO.
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN lockWindowUpdate (0,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = FALSE.
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION bJobBGColor sObject 
FUNCTION bJobBGColor RETURNS INTEGER
  ( /* parameter-definitions */ ) :
  &SCOPED-DEFINE colorJobTable bPendingJob
  {{&includes}/jobBGColor.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION bJobFGColor sObject 
FUNCTION bJobFGColor RETURNS INTEGER
  ( /* parameter-definitions */ ) :
  &SCOPED-DEFINE colorJobTable bPendingJob
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

  DEFINE BUFFER bJobID FOR jobID.

  SESSION:SET-WAIT-STATE('General').
  FOR EACH bJobID NO-LOCK WHERE bJobID.jobSelected EQ YES,
      EACH bPendingJob OF bJobID NO-LOCK:
    IF bPendingJob.startDate GT TODAY + pendingLastDay OR
       bPendingJob.endDate GT TODAY + pendingLastDay THEN
    DO:
      MESSAGE 'Job' bPendingJob.job 'has a Start:'
        bPendingJob.startDate 'or End:' bPendingJob.endDate
        'Date exceeding' TODAY + pendingLastDay SKIP(1)
        'Job cannot be scheduled this far in Advance!!!' VIEW-AS ALERT-BOX
        TITLE 'Based on Configuration Value Setting'.
      continue = NO.
      LEAVE.
    END.
    ELSE
    IF bPendingJob.startDate GT TODAY + pendingOver OR
       bPendingJob.endDate GT TODAY + pendingOver THEN
    DO:
      MESSAGE 'Job' bPendingJob.job 'has a Start:'
        bPendingJob.startDate 'or End:' bPendingJob.endDate
        'Date exceeding' TODAY + pendingOver SKIP(1)
        'Continue to Schedule Selected Jobs?' VIEW-AS ALERT-BOX
        QUESTION BUTTONS YES-NO UPDATE continue.
      LEAVE.
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION numericDateTime sObject 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER) :
  {{&includes}/numericDateTime.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

