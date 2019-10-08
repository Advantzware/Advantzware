&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/ttblJob.i}

/* Parameters Definitions ---                                           */

DEFINE OUTPUT PARAMETER opResSeqChanged AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE jobID NO-UNDO
  FIELD job LIKE ttblJob.job
  FIELD jobSort LIKE ttblJob.jobSort
    INDEX jobSort IS PRIMARY UNIQUE jobSort
    INDEX jobID IS UNIQUE job.

PROCEDURE newEnd:
  {{&includes}/{&Board}/newEnd.i}
END PROCEDURE.

PROCEDURE newStart:
  {{&includes}/{&Board}/newStart.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME jobID

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES jobID ttblJob

/* Definitions for BROWSE jobID                                         */
&Scoped-define FIELDS-IN-QUERY-jobID jobID.job   
&Scoped-define ENABLED-FIELDS-IN-QUERY-jobID   
&Scoped-define SELF-NAME jobID
&Scoped-define QUERY-STRING-jobID FOR EACH jobID NO-LOCK   WHERE jobID.job BEGINS jobPhrase OR jobPhrase EQ '' BY jobID.jobSort
&Scoped-define OPEN-QUERY-jobID OPEN QUERY {&SELF-NAME} FOR EACH jobID NO-LOCK   WHERE jobID.job BEGINS jobPhrase OR jobPhrase EQ '' BY jobID.jobSort.
&Scoped-define TABLES-IN-QUERY-jobID jobID
&Scoped-define FIRST-TABLE-IN-QUERY-jobID jobID


/* Definitions for BROWSE ttblJob                                       */
&Scoped-define FIELDS-IN-QUERY-ttblJob ttblJob.resourceSequence ttblJob.resource ttblJob.startDate STRING(ttblJob.startTime,'HH:MM:SSam') ttblJob.endDate STRING(ttblJob.endTime,'HH:MM:SSam') ttblJob.dueDate STRING(ttblJob.dueTime,'HH:MM:SSam') origResSeq   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttblJob   
&Scoped-define SELF-NAME ttblJob
&Scoped-define QUERY-STRING-ttblJob FOR EACH ttblJob NO-LOCK   WHERE ttblJob.job EQ jobID.job BY ttblJob.resourceSequence
&Scoped-define OPEN-QUERY-ttblJob OPEN QUERY {&SELF-NAME} FOR EACH ttblJob NO-LOCK   WHERE ttblJob.job EQ jobID.job BY ttblJob.resourceSequence.
&Scoped-define TABLES-IN-QUERY-ttblJob ttblJob
&Scoped-define FIRST-TABLE-IN-QUERY-ttblJob ttblJob


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-jobID}~
    ~{&OPEN-QUERY-ttblJob}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS jobPhrase jobID ttblJob btnUp btnDown ~
btnRefresh btnExit 
&Scoped-Define DISPLAYED-OBJECTS jobPhrase 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDown 
     IMAGE-UP FILE "schedule/images/down.bmp":U
     LABEL "&DN" 
     SIZE 5 BY 1.14 TOOLTIP "Move Current Column and Row Down (Alt-D)".

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "schedule/images/exit1.bmp":U
     LABEL "E&xit" 
     SIZE 5 BY 1.19 TOOLTIP "Exit (Alt-X)"
     BGCOLOR 8 .

DEFINE BUTTON btnRefresh 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "&RF" 
     SIZE 5 BY 1.14 TOOLTIP "Refresh (Alt-R)".

DEFINE BUTTON btnUp 
     IMAGE-UP FILE "schedule/images/up.bmp":U
     LABEL "&UP" 
     SIZE 5 BY 1.14 TOOLTIP "Move Current Column and Row Up (Alt-U)".

DEFINE VARIABLE jobPhrase AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Job" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7 BY 4.29
     BGCOLOR 5 .

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7 BY 1.67
     BGCOLOR 4 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY jobID FOR 
      jobID SCROLLING.

DEFINE QUERY ttblJob FOR 
      ttblJob SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE jobID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS jobID Dialog-Frame _FREEFORM
  QUERY jobID DISPLAY
      jobID.job
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 25 BY 21.19.

DEFINE BROWSE ttblJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttblJob Dialog-Frame _FREEFORM
  QUERY ttblJob DISPLAY
      ttblJob.resourceSequence COLUMN-LABEL 'Seq'
  ttblJob.resource
  ttblJob.startDate FORMAT '99/99/9999' LABEL 'Start'
  STRING(ttblJob.startTime,'HH:MM:SSam') FORMAT 'X(11)' LABEL 'Time'
  ttblJob.endDate FORMAT '99/99/9999' LABEL 'End'
  STRING(ttblJob.endTime,'HH:MM:SSam') FORMAT 'X(11)' LABEL 'Time'
  ttblJob.dueDate FORMAT '99/99/9999' LABEL 'Due'
  STRING(ttblJob.dueTime,'HH:MM:SSam') FORMAT 'X(11)' LABEL 'Time'
  origResSeq LABEL 'OrigSeq' FORMAT '>>9'
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 100 BY 21.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     jobPhrase AT ROW 1.1 COL 5 COLON-ALIGNED
     jobID AT ROW 2.19 COL 1
     ttblJob AT ROW 2.19 COL 26
     btnUp AT ROW 2.43 COL 127 HELP
          "Click to Move Current Column and Row Up"
     btnDown AT ROW 3.62 COL 127 HELP
          "Click to Move Current Column and Row Down"
     btnRefresh AT ROW 5.05 COL 127 HELP
          "Click to Refresh"
     btnExit AT ROW 6.71 COL 127 HELP
          "Click to Exit"
     RECT-11 AT ROW 2.19 COL 126
     RECT-14 AT ROW 6.48 COL 126
     SPACE(0.00) SKIP(15.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Resource Sequencer".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB jobID jobPhrase Dialog-Frame */
/* BROWSE-TAB ttblJob jobID Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       jobID:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR RECTANGLE RECT-11 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-14 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       ttblJob:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE jobID
/* Query rebuild information for BROWSE jobID
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH jobID NO-LOCK
  WHERE jobID.job BEGINS jobPhrase OR jobPhrase EQ '' BY jobID.jobSort
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE jobID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttblJob
/* Query rebuild information for BROWSE ttblJob
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttblJob NO-LOCK
  WHERE ttblJob.job EQ jobID.job BY ttblJob.resourceSequence
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttblJob */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Resource Sequencer */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDown Dialog-Frame
ON CHOOSE OF btnDown IN FRAME Dialog-Frame /* DN */
DO:
  RUN moveSeq (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit Dialog-Frame
ON CHOOSE OF btnExit IN FRAME Dialog-Frame /* Exit */
DO:
  APPLY 'GO':U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRefresh Dialog-Frame
ON CHOOSE OF btnRefresh IN FRAME Dialog-Frame /* RF */
DO:
  RUN reopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUp Dialog-Frame
ON CHOOSE OF btnUp IN FRAME Dialog-Frame /* UP */
DO:
  RUN moveSeq (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME jobID
&Scoped-define SELF-NAME jobID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobID Dialog-Frame
ON ANY-PRINTABLE OF jobID IN FRAME Dialog-Frame
DO:
  jobPhrase:SCREEN-VALUE = jobPhrase:SCREEN-VALUE + CHR(LASTKEY).
  APPLY 'VALUE-CHANGED' TO jobPhrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobID Dialog-Frame
ON BACKSPACE OF jobID IN FRAME Dialog-Frame
DO:
  jobPhrase:SCREEN-VALUE = SUBSTR(jobPhrase:SCREEN-VALUE,1,LENGTH(jobPhrase:SCREEN-VALUE) - 1).
  APPLY 'VALUE-CHANGED' TO jobPhrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobID Dialog-Frame
ON VALUE-CHANGED OF jobID IN FRAME Dialog-Frame
DO:
  {&OPEN-QUERY-ttblJob}
  IF NOT AVAILABLE jobID THEN RETURN NO-APPLY.
  APPLY 'VALUE-CHANGED' TO BROWSE ttblJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME jobPhrase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobPhrase Dialog-Frame
ON VALUE-CHANGED OF jobPhrase IN FRAME Dialog-Frame /* Job */
DO:
  ASSIGN {&SELF-NAME}.
  {&OPEN-QUERY-jobID}
  APPLY 'VALUE-CHANGED' TO jobID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttblJob
&Scoped-define SELF-NAME ttblJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttblJob Dialog-Frame
ON ANY-PRINTABLE OF ttblJob IN FRAME Dialog-Frame
DO:
  jobPhrase:SCREEN-VALUE = jobPhrase:SCREEN-VALUE + CHR(LASTKEY).
  APPLY 'VALUE-CHANGED' TO jobPhrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttblJob Dialog-Frame
ON BACKSPACE OF ttblJob IN FRAME Dialog-Frame
DO:
  jobPhrase:SCREEN-VALUE = SUBSTR(jobPhrase:SCREEN-VALUE,1,LENGTH(jobPhrase:SCREEN-VALUE) - 1).
  APPLY 'VALUE-CHANGED' TO jobPhrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME jobID
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN createJobID.
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createJobID Dialog-Frame 
PROCEDURE createJobID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH ttblJob NO-LOCK BREAK BY ttblJob.job BY ttblJob.resourceSequence:
    IF FIRST-OF(ttblJob.job) THEN
    DO:
      CREATE jobID.
      ASSIGN
        jobID.job = ttblJob.job
        jobID.jobSort = ttblJob.jobSort.
    END. /* if first-of */
  END. /* each ttbljob */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY jobPhrase 
      WITH FRAME Dialog-Frame.
  ENABLE jobPhrase jobID ttblJob btnUp btnDown btnRefresh btnExit 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveSeq Dialog-Frame 
PROCEDURE moveSeq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipMove AS INTEGER NO-UNDO.

  DEFINE VARIABLE lvResourceSequence AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvAltResSeq AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvRowID AS ROWID NO-UNDO.

  FIND FIRST buffJob EXCLUSIVE-LOCK WHERE buffJob.job EQ ttblJob.job
       AND buffJob.resourceSequence EQ ttblJob.resourceSequence + ipMove NO-ERROR.
  IF AVAILABLE buffJob THEN
  DO:
    ASSIGN
      lvRowID = ROWID(ttblJob)
      lvResourceSequence = ttblJob.resourceSequence
      lvAltResSeq = ttblJob.altResSeq
      ttblJob.resourceSequence = buffJob.resourceSequence
      ttblJob.altResSeq = buffJob.altResSeq
      buffJob.resourceSequence = lvResourceSequence
      buffJob.altResSeq = lvAltResSeq
      opResSeqChanged = YES.
    {&OPEN-QUERY-ttblJob}
    REPOSITION ttblJob TO ROWID(lvRowID) NO-ERROR.
  END. /* avail buffJob */
  APPLY 'ENTRY':U TO ttblJob IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopenBrowse Dialog-Frame 
PROCEDURE reopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  MESSAGE 'Reset All Jobs (Yes) or Only Job' buffJob.job '(No)?'
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE allJobs AS LOGICAL.
  CASE allJobs:
    WHEN YES THEN
    FOR EACH buffJob EXCLUSIVE-LOCK:
      IF buffJob.resourceSequence NE buffJob.origResSeq THEN
      buffJob.resourceSequence = buffJob.origResSeq.
    END. /* each buffjob */
    WHEN NO THEN
    FOR EACH buffJob EXCLUSIVE-LOCK WHERE buffJob.job EQ ttblJob.job:
      IF buffJob.resourceSequence NE buffJob.origResSeq THEN
      buffJob.resourceSequence = buffJob.origResSeq.
    END. /* each buffjob where */
    OTHERWISE
    RETURN.
  END CASE.
  IF allJobs THEN opResSeqChanged = NO.
  {&OPEN-QUERY-ttblJob}
  APPLY 'ENTRY':U TO ttblJob IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

