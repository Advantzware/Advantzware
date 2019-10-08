&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: lagTime.w

  Description: Update Lag Time Value

  Input Parameters: ttblJob RowID

  Output Parameters: <none>

  Author: Ron Stark

  Created: 11.30.2006
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE correct-error AS LOGICAL NO-UNDO.

{{&includes}/ttblJob.i}

DEFINE BUFFER bTtblJob FOR ttblJob.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 newLagHour newLagMinute btnSave ~
btnRestore btnCancel applyLagTime 
&Scoped-Define DISPLAYED-OBJECTS resource jobSequence job resourceSequence ~
newLagHour newLagMinute origLagHour origLagMinute applyLagTime 

/* Custom List Definitions                                              */
/* endDateFields,startDateFields,List-3,List-4,timeFields,List-6        */
&Scoped-define timeFields newLagHour newLagMinute origLagHour origLagMinute ~
applyLagTime 

/* _UIB-PREPROCESSOR-BLOCK-END */
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
     SIZE 12 BY 1.43 TOOLTIP "Restore Original Lag Time Value"
     BGCOLOR 8 .

DEFINE BUTTON btnSave AUTO-GO 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "" 
     SIZE 12 BY 1.43 TOOLTIP "Save New Lag Time Value"
     BGCOLOR 8 .

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

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 1.91.

DEFINE VARIABLE applyLagTime AS LOGICAL INITIAL no 
     LABEL "Apply Lag Time to ALL Jobs of this Resource" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     resource AT ROW 1.24 COL 10 COLON-ALIGNED
     jobSequence AT ROW 1.24 COL 43 COLON-ALIGNED
     job AT ROW 2.43 COL 10 COLON-ALIGNED
     resourceSequence AT ROW 2.43 COL 43 COLON-ALIGNED
     newLagHour AT ROW 3.62 COL 10 COLON-ALIGNED HELP
          "Enter New Lag Time Hours"
     newLagMinute AT ROW 3.62 COL 16 COLON-ALIGNED HELP
          "Enter Lag Time Minutes"
     origLagHour AT ROW 3.62 COL 39 COLON-ALIGNED
     origLagMinute AT ROW 3.62 COL 45 COLON-ALIGNED
     btnSave AT ROW 5.05 COL 13
     btnRestore AT ROW 5.05 COL 26
     btnCancel AT ROW 5.05 COL 39
     applyLagTime AT ROW 6.95 COL 4 WIDGET-ID 2
     RECT-1 AT ROW 4.81 COL 12
     SPACE(1.19) SKIP(1.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Job Lag Time"
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX applyLagTime IN FRAME Dialog-Frame
   5                                                                    */
/* SETTINGS FOR FILL-IN job IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN jobSequence IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN newLagHour IN FRAME Dialog-Frame
   5                                                                    */
/* SETTINGS FOR FILL-IN newLagMinute IN FRAME Dialog-Frame
   5                                                                    */
/* SETTINGS FOR FILL-IN origLagHour IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN origLagMinute IN FRAME Dialog-Frame
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN resource IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN resourceSequence IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Job Lag Time */
DO:
                                                                                                                                                                                                                                                                                                                                            APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRestore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestore Dialog-Frame
ON CHOOSE OF btnRestore IN FRAME Dialog-Frame /* Restore */
DO:
  ASSIGN
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
  ASSIGN
      {&timeFields}
      ttblJob.lagTime = newLagHour * 60 + newLagMinute
      .
  IF applyLagTime THEN
  FOR EACH bTtblJob
      WHERE bTtblJob.resource EQ ttblJob.resource
      :
      bTtblJob.lagTime = newLagHour * 60 + newLagMinute.
  END. /* each ttbljob */
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
    origLagHour = TRUNCATE(ttblJob.lagTime / 60,0)
    origLagMinute = ttblJob.lagTime MOD 60
    newLagHour = origLagHour
    newLagMinute = origLagMinute.
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY resource jobSequence job resourceSequence newLagHour newLagMinute 
          origLagHour origLagMinute applyLagTime 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 newLagHour newLagMinute btnSave btnRestore btnCancel 
         applyLagTime 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

