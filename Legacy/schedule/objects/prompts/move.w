&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: move.w

  Description: Confirm Bar Move

  Input Parameters: Senerio, Resource, Job, Start Date, Time, New Date, Time

  Output Parameters: Accept Move (yes/no)

  Author: Ron Stark

  Created: 9.19.2001
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipEndPrompt AS LOGICAL NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER iopNewStartDate AS DATE NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopNewStartTime AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopNewEndDate AS DATE NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopNewEndTime AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopNewLagTime AS INTEGER NO-UNDO.

DEFINE OUTPUT PARAMETER opMoveOK AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE correct-error AS LOGICAL NO-UNDO.
DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.
DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.

{{&includes}/ttblJob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 selectDateTime newStartDate ~
newStartHour newStartMinute newStartAMPM newLagHour newLagMinute btnSave ~
btnRestore btnCancel 
&Scoped-Define DISPLAYED-OBJECTS resource jobSequence job resourceSequence ~
startDate startHour startMinute startAMPM endDate endHour endMinute endAMPM ~
selectDateTime newStartDate newStartHour newStartMinute newStartAMPM ~
newEndDate newEndHour newEndMinute newEndAMPM newLagHour newLagMinute ~
origStartDate origStartHour origStartMinute origStartAMPM origEndDate ~
origEndHour origEndMinute origEndAMPM dueDate dueHour dueMinute dueAMPM ~
origLagHour origLagMinute 

/* Custom List Definitions                                              */
/* endDateFields,startDateFields,List-3,List-4,timeFields,List-6        */
&Scoped-define endDateFields newEndDate newEndHour newEndMinute newEndAMPM 
&Scoped-define startDateFields newStartDate newStartHour newStartMinute ~
newStartAMPM 
&Scoped-define timeFields startHour startMinute startAMPM endHour endMinute ~
endAMPM newStartDate newStartHour newStartMinute newStartAMPM newEndDate ~
newEndHour newEndMinute newEndAMPM newLagHour newLagMinute origStartHour ~
origStartMinute origStartAMPM origEndHour origEndMinute origEndAMPM dueHour ~
dueMinute dueAMPM origLagHour origLagMinute 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD numericDateTime Dialog-Frame 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "schedule/images/exit1.bmp":U
     LABEL "&Cancel" 
     SIZE 11 BY 1.43 TOOLTIP "Cancel with No Changes"
     BGCOLOR 8 .

DEFINE BUTTON btnRestore 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "&Restore" 
     SIZE 12 BY 1.43 TOOLTIP "Restore to Original Date and Time"
     BGCOLOR 8 .

DEFINE BUTTON btnSave AUTO-GO 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "" 
     SIZE 12 BY 1.43 TOOLTIP "Save New Starting Date and Time"
     BGCOLOR 8 .

DEFINE VARIABLE dueAMPM AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE endAMPM AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE newEndAMPM AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE newStartAMPM AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE origEndAMPM AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE origStartAMPM AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE startAMPM AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE dueDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Due Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE dueHour AS CHARACTER FORMAT "X(2)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE dueMinute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE endDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Current End" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE endHour AS CHARACTER FORMAT "X(2)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE endMinute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE job AS CHARACTER FORMAT "X(256)":U 
     LABEL "Job" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE jobSequence AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Job Sequence" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE newEndDate AS DATE FORMAT "99/99/9999":U 
     LABEL "New &End" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE newEndHour AS CHARACTER FORMAT "X(2)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE newEndMinute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE newLagHour AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "&Lag Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE newLagMinute AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE newStartDate AS DATE FORMAT "99/99/9999":U 
     LABEL "&New Start" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE newStartHour AS CHARACTER FORMAT "X(2)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE newStartMinute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE origEndDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Original End" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE origEndHour AS CHARACTER FORMAT "X(2)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE origEndMinute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE origLagHour AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Original Lag Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE origLagMinute AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE origStartDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Original Start" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE origStartHour AS CHARACTER FORMAT "X(2)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE origStartMinute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE resource AS CHARACTER FORMAT "X(256)":U 
     LABEL "Resource" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE resourceSequence AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Resource Seq" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE startDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Current Start" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE startHour AS CHARACTER FORMAT "X(2)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE startMinute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE selectDateTime AS CHARACTER INITIAL "Start" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Start", "Start",
"End", "End"
     SIZE 3 BY 2.38 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 39 BY 1.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     resource AT ROW 1.24 COL 13 COLON-ALIGNED
     jobSequence AT ROW 1.24 COL 46 COLON-ALIGNED
     job AT ROW 2.43 COL 13 COLON-ALIGNED
     resourceSequence AT ROW 2.43 COL 46 COLON-ALIGNED
     startDate AT ROW 4.1 COL 13 COLON-ALIGNED
     startHour AT ROW 4.1 COL 33 COLON-ALIGNED
     startMinute AT ROW 4.1 COL 39 COLON-ALIGNED
     startAMPM AT ROW 4.1 COL 44 COLON-ALIGNED NO-LABEL
     endDate AT ROW 5.29 COL 13 COLON-ALIGNED
     endHour AT ROW 5.29 COL 33 COLON-ALIGNED
     endMinute AT ROW 5.29 COL 39 COLON-ALIGNED
     endAMPM AT ROW 5.29 COL 44 COLON-ALIGNED NO-LABEL
     selectDateTime AT ROW 6.95 COL 1 NO-LABEL
     newStartDate AT ROW 6.95 COL 13 COLON-ALIGNED
     newStartHour AT ROW 6.95 COL 33 COLON-ALIGNED HELP
          "Enter Starting Hour"
     newStartMinute AT ROW 6.95 COL 39 COLON-ALIGNED HELP
          "Enter Starting Minute"
     newStartAMPM AT ROW 6.95 COL 44 COLON-ALIGNED NO-LABEL
     newEndDate AT ROW 8.14 COL 13 COLON-ALIGNED
     newEndHour AT ROW 8.14 COL 33 COLON-ALIGNED HELP
          "Enter Starting Hour"
     newEndMinute AT ROW 8.14 COL 39 COLON-ALIGNED HELP
          "Enter Starting Minute"
     newEndAMPM AT ROW 8.14 COL 44 COLON-ALIGNED NO-LABEL
     newLagHour AT ROW 9.33 COL 33 COLON-ALIGNED HELP
          "Enter New Lag Time Hours"
     newLagMinute AT ROW 9.33 COL 39 COLON-ALIGNED HELP
          "Enter Lag Time Minutes"
     btnSave AT ROW 10.76 COL 16
     btnRestore AT ROW 10.76 COL 29
     btnCancel AT ROW 10.76 COL 42
     origStartDate AT ROW 12.67 COL 13 COLON-ALIGNED
     origStartHour AT ROW 12.67 COL 33 COLON-ALIGNED
     origStartMinute AT ROW 12.67 COL 39 COLON-ALIGNED
     origStartAMPM AT ROW 12.67 COL 44 COLON-ALIGNED NO-LABEL
     origEndDate AT ROW 13.86 COL 13 COLON-ALIGNED
     origEndHour AT ROW 13.86 COL 33 COLON-ALIGNED
     origEndMinute AT ROW 13.86 COL 39 COLON-ALIGNED
     origEndAMPM AT ROW 13.86 COL 44 COLON-ALIGNED NO-LABEL
     dueDate AT ROW 15.05 COL 13 COLON-ALIGNED
     dueHour AT ROW 15.05 COL 33 COLON-ALIGNED
     dueMinute AT ROW 15.05 COL 39 COLON-ALIGNED
     dueAMPM AT ROW 15.05 COL 44 COLON-ALIGNED NO-LABEL
     origLagHour AT ROW 16.24 COL 33 COLON-ALIGNED
     origLagMinute AT ROW 16.24 COL 39 COLON-ALIGNED
     RECT-1 AT ROW 10.52 COL 15
     SPACE(0.99) SKIP(4.89)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Confirm Move"
         CANCEL-BUTTON btnCancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX dueAMPM IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN dueDate IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dueHour IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN dueMinute IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR COMBO-BOX endAMPM IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN endDate IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endHour IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN endMinute IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN job IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN jobSequence IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX newEndAMPM IN FRAME Dialog-Frame
   NO-ENABLE 1 5                                                        */
/* SETTINGS FOR FILL-IN newEndDate IN FRAME Dialog-Frame
   NO-ENABLE 1 5                                                        */
/* SETTINGS FOR FILL-IN newEndHour IN FRAME Dialog-Frame
   NO-ENABLE 1 5                                                        */
/* SETTINGS FOR FILL-IN newEndMinute IN FRAME Dialog-Frame
   NO-ENABLE 1 5                                                        */
/* SETTINGS FOR FILL-IN newLagHour IN FRAME Dialog-Frame
   5                                                                    */
/* SETTINGS FOR FILL-IN newLagMinute IN FRAME Dialog-Frame
   5                                                                    */
/* SETTINGS FOR COMBO-BOX newStartAMPM IN FRAME Dialog-Frame
   2 5                                                                  */
/* SETTINGS FOR FILL-IN newStartDate IN FRAME Dialog-Frame
   2 5                                                                  */
/* SETTINGS FOR FILL-IN newStartHour IN FRAME Dialog-Frame
   2 5                                                                  */
/* SETTINGS FOR FILL-IN newStartMinute IN FRAME Dialog-Frame
   2 5                                                                  */
/* SETTINGS FOR COMBO-BOX origEndAMPM IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN origEndDate IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN origEndHour IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN origEndMinute IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN origLagHour IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN origLagMinute IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR COMBO-BOX origStartAMPM IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN origStartDate IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN origStartHour IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN origStartMinute IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN resource IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN resourceSequence IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX startAMPM IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN startDate IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startHour IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN startMinute IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Confirm Move */
DO:
                                                                                                                                                                                                                                                                                                                                            APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
  opMoveOK = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRestore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestore Dialog-Frame
ON CHOOSE OF btnRestore IN FRAME Dialog-Frame /* Restore */
DO:
  ASSIGN
    newStartDate = origStartDate
    newStartHour = origStartHour
    newStartMinute = origStartMinute
    newStartAMPM = origStartAMPM
    newEndDate = origEndDate
    newEndHour = origEndHour
    newEndMinute = origEndMinute
    newEndAMPM = origEndAMPM
    newLagHour = origLagHour
    newLagMinute = origLagMinute.
  DISPLAY {&timeFields} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame
DO:
  ASSIGN {&timeFields} newStartDate newEndDate
    iopNewStartDate = newStartDate
    iopNewEndDate = newEndDate
    iopNewLagTime = newLagHour * 60 + newLagMinute
    opMoveOK = TRUE.
  {{&includes}/{&Board}/setTime.i
      &field="iopNewStartTime"
      &hour="newStartHour"
      &minute="newStartMinute"
      &ampm="newStartAMPM"}
  {{&includes}/{&Board}/setTime.i
      &field="iopNewEndTime"
      &hour="newEndHour"
      &minute="newEndMinute"
      &ampm="newEndAMPM"}
  ASSIGN
    lvStartDateTime = numericDateTime(iopNewStartDate,iopNewStartTime)
    lvEndDateTime = numericDateTime(iopNewEndDate,iopNewEndTime).
  IF ipEndPrompt AND lvEndDateTime LE lvStartDateTime THEN
  DO:
    MESSAGE 'End Date and Time can not be' SKIP
      'prior to Start Date and Time!!!' VIEW-AS ALERT-BOX ERROR.
    APPLY 'ENTRY' TO newStartDate.
    RETURN NO-APPLY.
  END. /* if */
  IF newStartDate - TODAY GE 90 OR newEndDate - TODAY GE 90 THEN
  DO:
    MESSAGE 'Start and/or End Date exceeds 90 days!!!' SKIP(1)
      'Accept this date setting?' VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE accepted AS LOGICAL.
    IF NOT accepted THEN
    DO:
      IF selectDateTime EQ 'Start' THEN APPLY 'ENTRY' TO newStartDate.
      ELSE APPLY 'ENTRY' TO newEndDate.
      RETURN NO-APPLY.
    END. /* not accepted */
  END. /* if gt 90 days */
  FIND CURRENT ttblJob EXCLUSIVE-LOCK.
  ttblJob.lagTime = iopNewLagTime.
  FIND CURRENT ttblJob NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME newEndAMPM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newEndAMPM Dialog-Frame
ON RETURN OF newEndAMPM IN FRAME Dialog-Frame
DO:
  APPLY 'CHOOSE' TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newEndAMPM Dialog-Frame
ON VALUE-CHANGED OF newEndAMPM IN FRAME Dialog-Frame
DO:
  RUN dateTimeChange.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME newEndDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newEndDate Dialog-Frame
ON LEAVE OF newEndDate IN FRAME Dialog-Frame /* New End */
DO:
  RUN dateTimeChange.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newEndDate Dialog-Frame
ON RETURN OF newEndDate IN FRAME Dialog-Frame /* New End */
DO:
  APPLY 'LEAVE' TO SELF.
  APPLY 'CHOOSE' TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME newEndHour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newEndHour Dialog-Frame
ON LEAVE OF newEndHour IN FRAME Dialog-Frame /* @ */
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 1 OR INTEGER(SELF:SCREEN-VALUE) GT 12.
  {{&includes}/Pro/entryerr.i &error-message="Invalid Hour, range = 1 to 12"}
  RUN dateTimeChange.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newEndHour Dialog-Frame
ON RETURN OF newEndHour IN FRAME Dialog-Frame /* @ */
DO:
  APPLY 'LEAVE' TO SELF.
  APPLY 'CHOOSE' TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME newEndMinute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newEndMinute Dialog-Frame
ON LEAVE OF newEndMinute IN FRAME Dialog-Frame
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {{&includes}/Pro/entryerr.i &error-message="Invalid Minute, range = 0 to 59"}
  RUN dateTimeChange.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newEndMinute Dialog-Frame
ON RETURN OF newEndMinute IN FRAME Dialog-Frame
DO:
  APPLY 'LEAVE' TO SELF.
  APPLY 'CHOOSE' TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME newLagHour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newLagHour Dialog-Frame
ON RETURN OF newLagHour IN FRAME Dialog-Frame /* Lag Time */
DO:
  APPLY 'LEAVE' TO SELF.
  APPLY 'CHOOSE' TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME newLagMinute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newLagMinute Dialog-Frame
ON LEAVE OF newLagMinute IN FRAME Dialog-Frame
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {{&includes}/Pro/entryerr.i &error-message="Invalid Minute, range = 0 to 59"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newLagMinute Dialog-Frame
ON RETURN OF newLagMinute IN FRAME Dialog-Frame
DO:
  APPLY 'LEAVE' TO SELF.
  APPLY 'CHOOSE' TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME newStartAMPM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newStartAMPM Dialog-Frame
ON RETURN OF newStartAMPM IN FRAME Dialog-Frame
DO:
  APPLY 'CHOOSE' TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newStartAMPM Dialog-Frame
ON VALUE-CHANGED OF newStartAMPM IN FRAME Dialog-Frame
DO:
  RUN dateTimeChange.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME newStartDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newStartDate Dialog-Frame
ON LEAVE OF newStartDate IN FRAME Dialog-Frame /* New Start */
DO:
  RUN dateTimeChange.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newStartDate Dialog-Frame
ON RETURN OF newStartDate IN FRAME Dialog-Frame /* New Start */
DO:
  APPLY 'LEAVE' TO SELF.
  APPLY 'CHOOSE' TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME newStartHour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newStartHour Dialog-Frame
ON LEAVE OF newStartHour IN FRAME Dialog-Frame /* @ */
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 1 OR INTEGER(SELF:SCREEN-VALUE) GT 12.
  {{&includes}/Pro/entryerr.i &error-message="Invalid Hour, range = 1 to 12"}
  RUN dateTimeChange.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newStartHour Dialog-Frame
ON RETURN OF newStartHour IN FRAME Dialog-Frame /* @ */
DO:
  APPLY 'LEAVE' TO SELF.
  APPLY 'CHOOSE' TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME newStartMinute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newStartMinute Dialog-Frame
ON LEAVE OF newStartMinute IN FRAME Dialog-Frame
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {{&includes}/Pro/entryerr.i &error-message="Invalid Minute, range = 0 to 59"}
  RUN dateTimeChange.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL newStartMinute Dialog-Frame
ON RETURN OF newStartMinute IN FRAME Dialog-Frame
DO:
  APPLY 'LEAVE' TO SELF.
  APPLY 'CHOOSE' TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selectDateTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selectDateTime Dialog-Frame
ON VALUE-CHANGED OF selectDateTime IN FRAME Dialog-Frame
DO:
  ASSIGN {&SELF-NAME}.
  CASE {&SELF-NAME}:
    WHEN 'Start' THEN
    DO WITH FRAME {&FRAME-NAME}:
      ENABLE {&startDateFields}.
      DISABLE {&endDateFields}.
    END.
    WHEN 'End' THEN
    DO WITH FRAME {&FRAME-NAME}:
      DISABLE {&startDateFields}.
      ENABLE {&endDateFields}.
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  FIND ttblJob NO-LOCK WHERE ROWID(ttblJob) EQ ipRowID NO-ERROR.
  ASSIGN
    resource = ttblJob.resource
    jobSequence = ttblJob.jobSequence
    job = ttblJob.job
    resourceSequence = ttblJob.resourceSequence
    startDate = ttblJob.startDate
    endDate = ttblJob.endDate
    newStartDate = iopNewStartDate
    newEndDate = iopNewEndDate
    origStartDate = ttblJob.origStartDate
    origEndDate = ttblJob.origEndDate
    dueDate = ttblJob.dueDate
    origLagHour = TRUNCATE(ttblJob.lagTime / 60,0)
    origLagMinute = ttblJob.lagTime MOD 60
    newLagHour = origLagHour
    newLagMinute = origLagMinute.
  {{&includes}/{&Board}/getTime.i &field="ttblJob.startTime" &hour="startHour" &minute="startMinute" &ampm="startAMPM"}
  {{&includes}/{&Board}/getTime.i &field="ttblJob.endTime" &hour="endHour" &minute="endMinute" &ampm="endAMPM"}
  {{&includes}/{&Board}/getTime.i &field="iopNewStartTime" &hour="newStartHour" &minute="newStartMinute" &ampm="newStartAMPM"}
  {{&includes}/{&Board}/getTime.i &field="iopNewEndTime" &hour="newEndHour" &minute="newEndMinute" &ampm="newEndAMPM"}
  {{&includes}/{&Board}/getTime.i &field="ttblJob.origStartTime" &hour="origStartHour" &minute="origStartMinute" &ampm="origStartAMPM"}
  {{&includes}/{&Board}/getTime.i &field="ttblJob.origEndTime" &hour="origEndHour" &minute="origEndMinute" &ampm="origEndAMPM"}
  {{&includes}/{&Board}/getTime.i &field="ttblJob.dueTime" &hour="dueHour" &minute="dueMinute" &ampm="dueAMPM"}
  RUN enable_UI.
  IF ipEndPrompt THEN
  DO:
    ENABLE {&endDateFields} WITH FRAME {&FRAME-NAME}.
    DISABLE selectDateTime WITH FRAME {&FRAME-NAME}.
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dateTimeChange Dialog-Frame 
PROCEDURE dateTimeChange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT ipEndPrompt THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&timeFields}.
    CASE selectDateTime:
      WHEN 'Start' THEN
      DO:
        {{&includes}/{&Board}/setTime.i
            &field="lvStartTime"
            &hour="newStartHour"
            &minute="newStartMinute"
            &ampm="newStartAMPM"}
        RUN newEnd (ttblJob.timeSpan,newStartDate,lvStartTime,
                    OUTPUT newEndDate,OUTPUT lvEndTime).
      END.
      WHEN 'End' THEN
      DO:
        {{&includes}/{&Board}/setTime.i
            &field="lvEndTime"
            &hour="newEndHour"
            &minute="newEndMinute"
            &ampm="newEndAMPM"}
        RUN newStart (ttblJob.timeSpan,newEndDate,lvEndTime,
                      OUTPUT newStartDate,OUTPUT lvStartTime).
      END.
    END CASE.
    {{&includes}/{&Board}/getTime.i &field="lvStartTime" &hour="newStartHour" &minute="newStartMinute" &ampm="newStartAMPM"}
    {{&includes}/{&Board}/getTime.i &field="lvEndTime" &hour="newEndHour" &minute="newEndMinute" &ampm="newEndAMPM"}
    DISPLAY {&timeFields}.
  END.

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
  DISPLAY resource jobSequence job resourceSequence startDate startHour 
          startMinute startAMPM endDate endHour endMinute endAMPM selectDateTime 
          newStartDate newStartHour newStartMinute newStartAMPM newEndDate 
          newEndHour newEndMinute newEndAMPM newLagHour newLagMinute 
          origStartDate origStartHour origStartMinute origStartAMPM origEndDate 
          origEndHour origEndMinute origEndAMPM dueDate dueHour dueMinute 
          dueAMPM origLagHour origLagMinute 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 selectDateTime newStartDate newStartHour newStartMinute 
         newStartAMPM newLagHour newLagMinute btnSave btnRestore btnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newEnd Dialog-Frame 
PROCEDURE newEnd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/{&board}/newEnd.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newStart Dialog-Frame 
PROCEDURE newStart :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/{&board}/newStart.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION numericDateTime Dialog-Frame 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER) :
  {{&includes}/numericDateTime.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

