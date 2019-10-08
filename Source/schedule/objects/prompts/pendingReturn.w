&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: jobConflict.w

  Description: conflict with existing job resolution

  Input Parameters: <none>

  Output Parameters: resolution selected

  Author: Ron Stark

  Created: 5.14.2004
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */
 
&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT  PARAMETER ipJob AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opContinue AS LOGICAL NO-UNDO.

{{&includes}/sharedVars.i}
{{&includes}/ttblJob.i}
&ELSE
DEFINE VARIABLE ipJob AS CHARACTER NO-UNDO INIT '1502-0.1'.
DEFINE VARIABLE opContinue AS LOGICAL NO-UNDO.

{{&includes}/sharedVars.i NEW}
{{&includes}/ttblJob.i NEW}
CREATE ttblJob.
ASSIGN
  ttblJob.resource = '100'
  ttblJob.job = ipJob
  ttblJob.startDate = TODAY
  ttblJob.startTime = 0
  ttblJob.endDate = TODAY
  ttblJob.endTime = 43200
  .
CREATE ttblJob.
ASSIGN
  ttblJob.resource = '200'
  ttblJob.job = ipJob
  ttblJob.startDate = TODAY
  ttblJob.startTime = 43200
  ttblJob.endDate = TODAY
  ttblJob.endTime = 86340
  .
CREATE ttblJob.
ASSIGN
  ttblJob.resource = '300'
  ttblJob.job = ipJob
  ttblJob.startDate = TODAY + 1
  ttblJob.startTime = 0
  ttblJob.endDate = TODAY + 1
  ttblJob.endTime = 43200
  .
CREATE ttblJob.
ASSIGN
  ttblJob.resource = '400'
  ttblJob.job = ipJob
  ttblJob.startDate = TODAY + 1
  ttblJob.startTime = 43200
  ttblJob.endDate = TODAY + 1
  ttblJob.endTime = 86340
  .
CREATE ttblJob.
ASSIGN
  ttblJob.resource = '500'
  ttblJob.job = ipJob
  ttblJob.startDate = TODAY + 2
  ttblJob.startTime = 0
  ttblJob.endDate = TODAY + 2
  ttblJob.endTime = 43200
  .
&ENDIF

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnOK btnCancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "schedule/images/cancel.bmp":U
     LABEL "&Cancel Move" 
     SIZE 6 BY 1.43 TOOLTIP "Cancel Return"
     BGCOLOR 8 .

DEFINE BUTTON btnOK AUTO-END-KEY 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "&OK" 
     SIZE 6 BY 1.43 TOOLTIP "Return Selections"
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnOK AT ROW 1 COL 2
     btnCancel AT ROW 1 COL 8
     SPACE(63.99) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Return to Pending".


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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Return to Pending */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel Move */
DO:
  opContinue = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
  RUN moveToPending.
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
  RUN enable_UI.
  FRAME {&FRAME-NAME}:TITLE = 'Return Job ' + ipJob + ' to Pending?'.
  RUN createJobsToggleBoxes.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createJobsToggleBoxes Dialog-Frame 
PROCEDURE createJobsToggleBoxes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE jobWidget AS WIDGET NO-UNDO.
  DEFINE VARIABLE xPos AS INTEGER NO-UNDO.
  DEFINE VARIABLE yPos AS INTEGER NO-UNDO INIT 5.

  DELETE WIDGET-POOL 'jobs' NO-ERROR.
  CREATE WIDGET-POOL 'jobs' PERSISTENT.

  DISABLE btnOk btnCancel WITH FRAME {&FRAME-NAME}.
  ASSIGN
    btnOK:HIDDEN = YES
    btnCancel:HIDDEN = YES
    .
  FOR EACH ttblJob EXCLUSIVE-LOCK
      WHERE ttblJob.job EQ ipJob
         BY ttblJob.startDate
         BY ttblJob.startTime:
    CREATE TOGGLE-BOX jobWidget IN WIDGET-POOL 'jobs'
        ASSIGN
          FRAME = FRAME {&FRAME-NAME}:HANDLE
          FORMAT = 'X(256)'
          X = 5
          Y = yPos
          WIDTH-PIXELS = 300
          HEIGHT-PIXELS = 17
          SENSITIVE = NO
          HIDDEN = YES
          PRIVATE-DATA = ttblJob.resource + ',' + ttblJob.job
          LABEL = '[ ' + ttblJob.resource + ' ] @ '
                + STRING(ttblJob.startDate) + ' '
                + STRING(ttblJob.startTime,'hh:mm:ss am') + ' - '
                + STRING(ttblJob.endDate) + ' '
                + STRING(ttblJob.endTime,'hh:mm:ss am')
          .
    ASSIGN
      yPos = yPos + jobWidget:HEIGHT-PIXELS + 5
      FRAME {&FRAME-NAME}:HEIGHT-PIXELS = yPos + 65
      btnOK:Y = yPos
      btnCancel:Y = yPos
      jobWidget:HIDDEN = NO
      jobWidget:SENSITIVE = YES
      jobWidget:CHECKED = YES
      .
  END. /* each ttbljob */
  ENABLE btnOk btnCancel WITH FRAME {&FRAME-NAME}.

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
  ENABLE btnOK btnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveToPending Dialog-Frame 
PROCEDURE moveToPending :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentWidget AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE lvResource AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvJob AS CHARACTER NO-UNDO.

  ASSIGN
    currentWidget = FRAME {&FRAME-NAME}:HANDLE
    currentWidget = currentWidget:FIRST-CHILD
    currentWidget = currentWidget:FIRST-CHILD
    .
  DO WHILE currentWidget NE ?:
    IF currentWidget:TYPE EQ 'TOGGLE-BOX' AND
       currentWidget:CHECKED THEN DO:
      ASSIGN
        lvResource = ENTRY(1,currentWidget:PRIVATE-DATA)
        lvJob = ENTRY(2,currentWidget:PRIVATE-DATA)
        .
      FIND FIRST ttblJob EXCLUSIVE-LOCK
           WHERE ttblJob.resource EQ lvResource
             AND ttblJob.job EQ lvJob
           NO-ERROR.
      IF AVAILABLE ttblJob THEN DO:
        CREATE pendingJob.
        BUFFER-COPY ttblJob TO pendingJob
          ASSIGN pendingJob.origStartTime = ttblJob.timeSpan.
        DELETE ttblJob.
      END. /* if avail */
    END. /* toggle-box */
    currentWidget = currentWidget:NEXT-SIBLING.
  END. /* do while */
  opContinue = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

