&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          jobs             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: sbNotes.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 8.10.2017

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

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/filterVars.i}
{{&includes}/ttblJob.i}
{{&viewers}/includes/sharedVars.i NEW}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE resourceValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE popupHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE ttblJobRowID AS ROWID NO-UNDO.
DEFINE VARIABLE noteRowID AS ROWID NO-UNDO.
DEFINE VARIABLE buffNoteRowID AS ROWID NO-UNDO.
DEFINE VARIABLE sendChange AS LOGICAL NO-UNDO.

{{&viewers}/includes/browseDef.i}

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

{{&includes}/lockWindowUpdate.i}

DEFINE BUFFER buffJobNotes FOR TEMP-TABLE jobNotes.

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
&Scoped-define INTERNAL-TABLES ttblJob jobNotes

/* Definitions for BROWSE browseJob                                     */
&Scoped-define FIELDS-IN-QUERY-browseJob ttblJob.jobSequence ttblJob.job ttblJob.resource ttblJob.jobType   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseJob   
&Scoped-define SELF-NAME browseJob
&Scoped-define QUERY-STRING-browseJob FOR EACH ttblJob NO-LOCK WHERE (ttblJob.job BEGINS jobPhrase OR jobPhrase EQ '') AND ttblJob.job GE jobValueLo AND ttblJob.job LE jobValueHi AND (ttblJob.resource EQ resources OR resources EQ '<Select ...>') ~{&dateRangePhrase}
&Scoped-define OPEN-QUERY-browseJob OPEN QUERY {&SELF-NAME} FOR EACH ttblJob NO-LOCK WHERE (ttblJob.job BEGINS jobPhrase OR jobPhrase EQ '') AND ttblJob.job GE jobValueLo AND ttblJob.job LE jobValueHi AND (ttblJob.resource EQ resources OR resources EQ '<Select ...>') ~{&dateRangePhrase}.
&Scoped-define TABLES-IN-QUERY-browseJob ttblJob
&Scoped-define FIRST-TABLE-IN-QUERY-browseJob ttblJob


/* Definitions for BROWSE notesBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-notesBrowse jobNotes.noteDate STRING(jobNotes.noteTime,'HH:MM:SS am')   
&Scoped-define ENABLED-FIELDS-IN-QUERY-notesBrowse   
&Scoped-define SELF-NAME notesBrowse
&Scoped-define QUERY-STRING-notesBrowse FOR EACH jobNotes WHERE jobNotes.jobRowID EQ noteRowID                                             AND jobNotes.jobStatus EQ NO                                             AND jobNotes.deleteNote EQ NO
&Scoped-define OPEN-QUERY-notesBrowse OPEN QUERY {&SELF-NAME} FOR EACH jobNotes WHERE jobNotes.jobRowID EQ noteRowID                                             AND jobNotes.jobStatus EQ NO                                             AND jobNotes.deleteNote EQ NO.
&Scoped-define TABLES-IN-QUERY-notesBrowse jobNotes
&Scoped-define FIRST-TABLE-IN-QUERY-notesBrowse jobNotes


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-browseJob}~
    ~{&OPEN-QUERY-notesBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS jobPhrase resources btnUpdatesPending ~
browseJob notesBrowse jobNotesText btnAdd btnDelete btnSave btnReset ~
btnExit 
&Scoped-Define DISPLAYED-OBJECTS jobPhrase resources jobNotesText 

/* Custom List Definitions                                              */
/* ttblResourceFields,phraseFields,List-3,List-4,List-5,List-6          */
&Scoped-define ttblResourceFields btnAdd btnDelete btnSave btnReset 
&Scoped-define phraseFields jobPhrase 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "schedule/images/add.bmp":U
     LABEL "" 
     SIZE 5 BY 1.19 TOOLTIP "Add New Job Note"
     BGCOLOR 8 .

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "schedule/images/cancel.bmp":U
     LABEL "" 
     SIZE 5 BY 1.19 TOOLTIP "Delete Job Note"
     BGCOLOR 8 .

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "schedule/images/exit1.bmp":U
     LABEL "" 
     SIZE 5 BY 1.19
     BGCOLOR 8 .

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "" 
     SIZE 5 BY 1.19 TOOLTIP "Reset Job Note"
     BGCOLOR 8 .

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "" 
     SIZE 5 BY 1.19 TOOLTIP "Save Job Note"
     BGCOLOR 8 .

DEFINE BUTTON btnUpdatesPending 
     LABEL "Send Changes to Scheduler" 
     SIZE 31 BY 1.

DEFINE VARIABLE resources AS CHARACTER FORMAT "X(256)":U INITIAL "<Select ...>" 
     LABEL "&Resource" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 30
     LIST-ITEMS "<Select ...>" 
     DROP-DOWN-LIST
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE jobNotesText AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 77 BY 20.95
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE jobPhrase AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Job" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 149.6 BY 1.19.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 1.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseJob FOR 
      ttblJob SCROLLING.

DEFINE QUERY notesBrowse FOR 
      jobNotes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseJob sObject _FREEFORM
  QUERY browseJob DISPLAY
      ttblJob.jobSequence LABEL-BGCOLOR 14
  ttblJob.job LABEL-BGCOLOR 14
  ttblJob.resource
  ttblJob.jobType FORMAT 'X' LABEL 'T'
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 42 BY 21.19.

DEFINE BROWSE notesBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS notesBrowse sObject _FREEFORM
  QUERY notesBrowse DISPLAY
      jobNotes.noteDate
 STRING(jobNotes.noteTime,'HH:MM:SS am') FORMAT 'X(11)' LABEL 'Note Time'
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 28 BY 19.05 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     jobPhrase AT ROW 1.1 COL 4 COLON-ALIGNED
     resources AT ROW 1.1 COL 29.6 HELP
          "Select Resource"
     btnUpdatesPending AT ROW 1.1 COL 73
     browseJob AT ROW 2.19 COL 1
     notesBrowse AT ROW 2.43 COL 44 WIDGET-ID 100
     jobNotesText AT ROW 2.43 COL 73 NO-LABEL WIDGET-ID 2
     btnAdd AT ROW 21.95 COL 45 WIDGET-ID 4
     btnDelete AT ROW 21.95 COL 50 WIDGET-ID 6
     btnSave AT ROW 21.95 COL 55 WIDGET-ID 12
     btnReset AT ROW 21.95 COL 60 WIDGET-ID 10
     btnExit AT ROW 21.95 COL 66 WIDGET-ID 8
     RECT-1 AT ROW 1 COL 1
     RECT-2 AT ROW 21.71 COL 44 WIDGET-ID 14
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB browseJob btnUpdatesPending F-Main */
/* BROWSE-TAB notesBrowse browseJob F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       browseJob:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 3
       browseJob:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR BUTTON btnAdd IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnDelete IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnReset IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnSave IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN jobPhrase IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX resources IN FRAME F-Main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseJob
/* Query rebuild information for BROWSE browseJob
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttblJob NO-LOCK
WHERE (ttblJob.job BEGINS jobPhrase OR jobPhrase EQ '')
AND ttblJob.job GE jobValueLo
AND ttblJob.job LE jobValueHi
AND (ttblJob.resource EQ resources OR resources EQ '<Select ...>')
~{&dateRangePhrase}.
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE notesBrowse
/* Query rebuild information for BROWSE notesBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH jobNotes WHERE jobNotes.jobRowID EQ noteRowID
                                            AND jobNotes.jobStatus EQ NO
                                            AND jobNotes.deleteNote EQ NO.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE notesBrowse */
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
ON ROW-LEAVE OF browseJob IN FRAME F-Main
DO:
  RUN updateJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON START-SEARCH OF browseJob IN FRAME F-Main
DO:
  IF CAN-DO('job,jobSequence',{&BROWSE-NAME}:CURRENT-COLUMN:NAME) THEN DO:
    columnLabel = {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
    RUN reopenBrowse.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd sObject
ON CHOOSE OF btnAdd IN FRAME F-Main
DO:
  CREATE jobNotes.
  ASSIGN
    jobNotes.jobRowID = noteRowID
    jobNotes.noteDate = TODAY
    jobNotes.noteTime = TIME
    jobNotesText:SCREEN-VALUE = ''.
  {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
  APPLY 'ENTRY' TO jobNotesText.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete sObject
ON CHOOSE OF btnDelete IN FRAME F-Main
DO:
  IF NOT AVAILABLE jobNotes THEN
  RETURN NO-APPLY.
  MESSAGE 'Delete Job Note?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO UPDATE deleteOK AS LOGICAL.
  IF NOT deleteOK THEN
  RETURN NO-APPLY.
  FOR EACH buffJob NO-LOCK WHERE buffJob.job EQ ttblJob.job
                             AND ROWID(buffJob) NE ROWID(ttblJob):
    buffNoteRowID = TO-ROWID(ENTRY(NUM-ENTRIES(buffJob.rowIDs),buffJob.rowIDs)).
    FIND FIRST buffJobNotes EXCLUSIVE-LOCK
         WHERE buffJobNotes.jobRowID EQ buffNoteRowID
           AND buffJobNotes.noteDate EQ jobNotes.noteDate
           AND buffJobNotes.noteTime EQ jobNotes.noteTime NO-ERROR.
    IF AVAILABLE buffJobNotes THEN
    buffJobNotes.deleteNote = YES.
  END. /* each buffjob */
  ASSIGN
    jobNotes.deleteNote = YES
    jobNotesText:SCREEN-VALUE = ''.
  {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
  APPLY 'VALUE-CHANGED' TO {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit sObject
ON CHOOSE OF btnExit IN FRAME F-Main
DO:
  IF sendChange THEN RUN sendChange.
  APPLY 'END-ERROR':U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset sObject
ON CHOOSE OF btnReset IN FRAME F-Main
DO:
  IF NOT AVAILABLE jobNotes THEN
  RETURN NO-APPLY.
  jobNotesText:SCREEN-VALUE = jobNotes.noteText.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave sObject
ON CHOOSE OF btnSave IN FRAME F-Main
DO:
  IF NOT AVAILABLE jobNotes THEN
  RETURN NO-APPLY.
  jobNotes.noteText = jobNotesText:SCREEN-VALUE.
  MESSAGE 'Apply Note to All this Job~'s Resources?'
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE allResources AS LOGICAL.
  IF allResources THEN DO:
    FOR EACH buffJob NO-LOCK WHERE buffJob.job EQ ttblJob.job
                               AND ROWID(buffJob) NE ROWID(ttblJob):
      buffNoteRowID = TO-ROWID(ENTRY(NUM-ENTRIES(buffJob.rowIDs),buffJob.rowIDs)).
      FIND FIRST buffJobNotes EXCLUSIVE-LOCK
           WHERE buffJobNotes.jobRowID EQ buffNoteRowID
             AND buffJobNotes.noteDate EQ jobNotes.noteDate
             AND buffJobNotes.noteTime EQ jobNotes.noteTime NO-ERROR.
      IF NOT AVAILABLE buffJobNotes THEN DO:
        CREATE buffJobNotes.
        ASSIGN
          buffJobNotes.jobRowID = buffNoteRowID
          buffJobNotes.noteDate = jobNotes.noteDate
          buffJobNotes.noteTime = jobNotes.noteTime
          buffJobNotes.jobStatus = jobNotes.jobStatus
          buffJobNotes.deleteNote = jobNotes.deleteNote.
      END. /* not avail */
      buffJobNotes.noteText = jobNotes.noteText.
    END. /* each buffjob */
  END. /* if allresources */
  MESSAGE 'Note Text Saved!' VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUpdatesPending
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdatesPending sObject
ON CHOOSE OF btnUpdatesPending IN FRAME F-Main /* Send Changes to Scheduler */
DO:
  RUN updatesPending.
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


&Scoped-define BROWSE-NAME notesBrowse
&Scoped-define SELF-NAME notesBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notesBrowse sObject
ON VALUE-CHANGED OF notesBrowse IN FRAME F-Main
DO:
  IF NOT AVAILABLE jobNotes THEN
  RETURN NO-APPLY.
  jobNotesText:SCREEN-VALUE = jobNotes.noteText.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME resources
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL resources sObject
ON VALUE-CHANGED OF resources IN FRAME F-Main /* Resource */
DO:
  ASSIGN {&SELF-NAME}.
  columnLabel = IF {&SELF-NAME} BEGINS '<Select' THEN 'job' ELSE 'jobSequence'.
  RUN reopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseJob
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK sObject 


/* ***************************  Main Block  *************************** */

{{&viewers}/includes/winTitle.i}
{{&viewers}/includes/viewersInclude.i}

columnLabel = 'job'.

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
  RUN setCellLabels.
  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getNotes sObject 
PROCEDURE getNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRowID AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dDate AS DATE NO-UNDO.
  DEFINE VARIABLE iTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE cNote AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cKey AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cJob AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMCode AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iJob AS INTEGER NO-UNDO.
  DEFINE VARIABLE cJobNo AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iJobNo2 AS INTEGER NO-UNDO.
  DEFINE VARIABLE iFrm AS INTEGER NO-UNDO.
  DEFINE VARIABLE idx AS INTEGER NO-UNDO.

  SESSION:SET-WAIT-STATE('General').
  DO idx = 1 TO 2:
      IF idx EQ 1 THEN DO:
          IF SEARCH('{&data}\' + ID + '\jobNotes.dat') EQ ? THEN NEXT.
          INPUT FROM VALUE(SEARCH('{&data}\' + ID + '\jobNotes.dat')) APPEND.
      END. /* idx 2 */
      IF idx EQ 2 THEN DO:
          IF SEARCH('{&updates}\' + ID + '\notesPending.dat') EQ ? THEN NEXT.
          INPUT FROM VALUE(SEARCH('{&updates}\' + ID + '\notesPending.dat')) APPEND.
      END. /* idx 2 */
      REPEAT:
        IMPORT cRowID dDate iTime cNote cKey.
        ASSIGN
          cCompany = ENTRY(1,cKey)
          cMCode = ENTRY(2,cKey)
          iJob = INT(ENTRY(3,cKey))
          cJobNo = ENTRY(4,cKey)
          iJobNo2 = INT(ENTRY(5,cKey))
          iFrm = INT(ENTRY(6,cKey))
          cJob = cJobNo + "-" + STRING(iJobNo2) + "." + STRING(iFrm)
          .
        FIND FIRST ttblJob EXCLUSIVE-LOCK
             WHERE ttblJob.job EQ cJob
               AND ttblJob.resource EQ cMCode
             NO-ERROR.
        MESSAGE AVAIL(ttblJob)
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        IF NOT AVAILABLE ttblJob THEN NEXT.
        FIND FIRST jobNotes
             WHERE jobNotes.jobRowID EQ ROWID(ttblJob)
               AND jobNotes.noteDate EQ dDate
               AND jobNotes.noteTime EQ iTime
             NO-ERROR.
        IF NOT AVAILABLE jobNotes THEN DO:
            MESSAGE cRowID
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            CREATE jobNotes.
            ASSIGN
              jobNotes.jobRowID = ROWID(ttblJob)
              jobNotes.noteDate = dDate
              jobNotes.noteTime = iTime
              jobNotes.noteKey = cKey
              .
        END. /* not avail */
        jobNotes.noteText = cNote.
      END. /* repeat */
      INPUT CLOSE.
  END. /* do idx */
  SESSION:SET-WAIT-STATE('').

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize sObject 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FOR EACH pendingJob NO-LOCK:
    CREATE ttblJob.
    BUFFER-COPY pendingJob EXCEPT jobType TO ttblJob
      ASSIGN
        ttblJob.jobSequence = 999
        ttblJob.jobType = 'P'
        ttblJob.startDate = {{&includes}/firstDate.i}
        ttblJob.endDate = {{&includes}/lastDate.i}.
  END.
  RUN setColorDynamic.
  RUN getConfiguration.
  RUN getResources.
  RUN getNotes.
  RUN displayFields NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendChange sObject 
PROCEDURE sendChange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  MESSAGE 'Send Changes to Scheduler before Closing?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO UPDATE sendChange AS LOGICAL.
  IF sendChange THEN RUN updatesPending.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setCellLabels sObject 
PROCEDURE setCellLabels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = 5 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    ASSIGN
      cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i)
      cellColumn[i]:LABEL = customLabel[i - 4]
      cellColumn[i]:LABEL-BGCOLOR = customBGColor[i - 4]
      cellColumn[i]:LABEL-FGCOLOR = customFGColor[i - 4]
      cellColumn[i]:WIDTH-CHARS = IF LENGTH(cellColumn[i]:LABEL) GT 4 THEN
                                  LENGTH(cellColumn[i]:LABEL) * 1.2 ELSE 4.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setColorDynamic sObject 
PROCEDURE setColorDynamic :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}\setColorDynamic.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSendChange sObject 
PROCEDURE setSendChange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipSendChange AS LOGICAL NO-UNDO.

  DEFINE VARIABLE charHandle AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.
  
  IF VALID-HANDLE(adm-broker-hdl) THEN DO:
    RUN get-link-handle IN adm-broker-hdl
        (THIS-PROCEDURE,'CONTAINER-SOURCE':U,OUTPUT charHandle).
    pHandle = WIDGET-HANDLE(charHandle).
    IF VALID-HANDLE(pHandle) THEN
    RUN sendChange IN pHandle (ipSendChange).
  END. /* if valid-handle */

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
  DEFINE VARIABLE sendChange AS LOGICAL NO-UNDO.
  DEFINE VARIABLE statusValue AS LOGICAL NO-UNDO EXTENT 14.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  IF NOT AVAILABLE ttblJob THEN RETURN.
  DO i = 1 TO 14:
    statusValue[i] = BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(i + 4):SCREEN-VALUE EQ 'Yes'.
    IF ttblJob.jobStatus[i] NE statusValue[i] THEN
    ASSIGN
      ttblJob.sbStatus = YES
      sendChange = YES.
  END. /* do i */

  IF sendChange THEN
  RUN setSendChange (sendChange).

  IF ttblJob.sbStatus AND customCheckoff THEN
  FOR EACH buffJob EXCLUSIVE-LOCK WHERE buffJob.job EQ ttblJob.job
                                    AND ROWID(buffJob) NE ROWID(ttblJob):
    buffJob.sbStatus = YES.
    DO i = 1 TO 14:
      buffJob.jobStatus[i] = statusValue[i].
    END. /* do i */
  END. /* each buffjob */
  BROWSE {&BROWSE-NAME}:REFRESH().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updatesPending sObject 
PROCEDURE updatesPending :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  SESSION:SET-WAIT-STATE('General').
  OUTPUT TO VALUE('{&updates}\' + ID + '\notesPending.dat') APPEND.
  FOR EACH ttblJob NO-LOCK WHERE ttblJob.sbStatus EQ YES:
    EXPORT {&FIELDS-IN-QUERY-{&BROWSE-NAME}}.
  END. /* each ttbljob */
  OUTPUT CLOSE.
  RUN setSendChange (NO).
  SESSION:SET-WAIT-STATE('').
  MESSAGE 'Updates Pending Sent to Scheduler!' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

