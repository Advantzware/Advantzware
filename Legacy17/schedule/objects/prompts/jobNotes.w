&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME notesFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS notesFrame 
/*------------------------------------------------------------------------

  File: jobNotes.w

  Description: schedule board job notes 

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 7.14.2004
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipBoard AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE noteRowID AS ROWID NO-UNDO.
DEFINE VARIABLE buffNoteRowID AS ROWID NO-UNDO.

{{&includes}/ttblJob.i}

DEFINE BUFFER buffJobNotes FOR TEMP-TABLE jobNotes.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME notesFrame
&Scoped-define BROWSE-NAME notesBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES jobNotes

/* Definitions for BROWSE notesBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-notesBrowse jobNotes.noteDate STRING(jobNotes.noteTime,'HH:MM:SS am')   
&Scoped-define ENABLED-FIELDS-IN-QUERY-notesBrowse   
&Scoped-define SELF-NAME notesBrowse
&Scoped-define QUERY-STRING-notesBrowse FOR EACH jobNotes WHERE jobNotes.jobRowID EQ noteRowID                                             AND jobNotes.jobStatus EQ NO                                             AND jobNotes.deleteNote EQ NO
&Scoped-define OPEN-QUERY-notesBrowse OPEN QUERY {&SELF-NAME} FOR EACH jobNotes WHERE jobNotes.jobRowID EQ noteRowID                                             AND jobNotes.jobStatus EQ NO                                             AND jobNotes.deleteNote EQ NO.
&Scoped-define TABLES-IN-QUERY-notesBrowse jobNotes
&Scoped-define FIRST-TABLE-IN-QUERY-notesBrowse jobNotes


/* Definitions for DIALOG-BOX notesFrame                                */
&Scoped-define OPEN-BROWSERS-IN-QUERY-notesFrame ~
    ~{&OPEN-QUERY-notesBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS notesBrowse jobNotesText btnAdd btnDelete ~
btnSave btnReset btnExit 
&Scoped-Define DISPLAYED-OBJECTS jobNotesText 

/* Custom List Definitions                                              */
/* noteButton,List-2,List-3,List-4,List-5,List-6                        */
&Scoped-define noteButton btnAdd btnDelete btnSave btnReset 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

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

DEFINE VARIABLE jobNotesText AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 69 BY 16.67
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY 1.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY notesBrowse FOR 
      jobNotes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE notesBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS notesBrowse notesFrame _FREEFORM
  QUERY notesBrowse DISPLAY
      jobNotes.noteDate
 STRING(jobNotes.noteTime,'HH:MM:SS am') FORMAT 'X(11)' LABEL 'Note Time'
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 28 BY 14.76 ROW-HEIGHT-CHARS .52 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME notesFrame
     notesBrowse AT ROW 1.24 COL 2
     jobNotesText AT ROW 1.24 COL 31 NO-LABEL
     btnAdd AT ROW 16.48 COL 3
     btnDelete AT ROW 16.48 COL 8
     btnSave AT ROW 16.48 COL 13
     btnReset AT ROW 16.48 COL 18
     btnExit AT ROW 16.48 COL 24
     RECT-1 AT ROW 16.24 COL 2
     SPACE(70.39) SKIP(0.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Job Notes".


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
/* SETTINGS FOR DIALOG-BOX notesFrame
                                                                        */
/* BROWSE-TAB notesBrowse RECT-1 notesFrame */
ASSIGN 
       FRAME notesFrame:SCROLLABLE       = FALSE
       FRAME notesFrame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnAdd IN FRAME notesFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnDelete IN FRAME notesFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnReset IN FRAME notesFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnSave IN FRAME notesFrame
   1                                                                    */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME notesFrame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

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

&Scoped-define SELF-NAME notesFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notesFrame notesFrame
ON WINDOW-CLOSE OF FRAME notesFrame /* Job Notes */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd notesFrame
ON CHOOSE OF btnAdd IN FRAME notesFrame
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete notesFrame
ON CHOOSE OF btnDelete IN FRAME notesFrame
DO:
  IF NOT AVAILABLE jobNotes THEN
  RETURN NO-APPLY.
  MESSAGE 'Delete Job Note?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO UPDATE deleteOK AS LOGICAL.
  IF NOT deleteOK THEN
  RETURN NO-APPLY.
  FOR EACH buffJob NO-LOCK WHERE buffJob.job EQ ttblJob.job
                             AND ROWID(buffJob) NE ROWID(ttblJob):
    buffNoteRowID = TO-ROWID(ENTRY(2,buffJob.rowIDs)).
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit notesFrame
ON CHOOSE OF btnExit IN FRAME notesFrame
DO:
  APPLY 'END-ERROR':U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset notesFrame
ON CHOOSE OF btnReset IN FRAME notesFrame
DO:
  IF NOT AVAILABLE jobNotes THEN
  RETURN NO-APPLY.
  jobNotesText:SCREEN-VALUE = jobNotes.noteText.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave notesFrame
ON CHOOSE OF btnSave IN FRAME notesFrame
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
      buffNoteRowID = TO-ROWID(ENTRY(2,buffJob.rowIDs)).
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


&Scoped-define BROWSE-NAME notesBrowse
&Scoped-define SELF-NAME notesBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notesBrowse notesFrame
ON VALUE-CHANGED OF notesBrowse IN FRAME notesFrame
DO:
  IF NOT AVAILABLE jobNotes THEN
  RETURN NO-APPLY.
  jobNotesText:SCREEN-VALUE = jobNotes.noteText.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK notesFrame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF ipBoard NE '{&Board}' THEN
  DISABLE {&noteButton} WITH FRAME {&FRAME-NAME}.
  FIND ttblJob NO-LOCK WHERE ROWID(ttblJob) EQ ipRowID NO-ERROR.
  IF NOT AVAILABLE ttblJob THEN
  DO:
    MESSAGE 'No Job Selected on Board' VIEW-AS ALERT-BOX.
    RUN disable_UI.
    RETURN.
  END.
  ASSIGN
    noteRowID = TO-ROWID(ENTRY(2,ttblJob.rowIDs))
    FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + ' - ' +
                                ttblJob.job + ' (' + ttblJob.resource + ')'.
  {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
  APPLY 'VALUE-CHANGED' TO {&BROWSE-NAME}.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI notesFrame  _DEFAULT-DISABLE
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
  HIDE FRAME notesFrame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI notesFrame  _DEFAULT-ENABLE
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
  DISPLAY jobNotesText 
      WITH FRAME notesFrame.
  ENABLE notesBrowse jobNotesText btnAdd btnDelete btnSave btnReset btnExit 
      WITH FRAME notesFrame.
  VIEW FRAME notesFrame.
  {&OPEN-BROWSERS-IN-QUERY-notesFrame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

