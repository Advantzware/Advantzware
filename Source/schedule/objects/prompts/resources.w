&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: resources.w

  Description: Resource List for Selection

  Input Parameters: Company, Job Number

  Output Parameters: Resource List

  Author: Ron Stark

  Created: 1.24.2022
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
 
&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcJobNo     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiJobNo2    AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiForm      AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcResources AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcChanges   AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipcCompany   AS CHARACTER NO-UNDO INIT "001".
DEFINE VARIABLE ipcJobNo     AS CHARACTER NO-UNDO INIT "W14349".
DEFINE VARIABLE ipiJobNo2    AS INTEGER   NO-UNDO INIT 0.
DEFINE VARIABLE ipiForm      AS INTEGER   NO-UNDO INIT 1.
DEFINE VARIABLE opcResources AS CHARACTER NO-UNDO.
DEFINE VARIABLE opcChanges   AS CHARACTER NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
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
     btnOK AT ROW 1.95 COL 2
     btnCancel AT ROW 1.95 COL 8
     "Replace With:" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 1.24 COL 56
     "Routing:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.24 COL 6
     SPACE(90.99) SKIP(1.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Job Routing(s)".


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
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Job Routing(s) */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel Move */
DO:
    ASSIGN
        opcResources = ""
        opcChanges   = ""
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    RUN pResourceList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  FRAME {&FRAME-NAME}:TITLE = 'Job '
                            + ipcJobNo + '-'
                            + STRING(ipiJobNo2) + '.'
                            + STRING(ipiForm)
                            + ' Routing(s)'
                            .
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
  DEFINE VARIABLE cMachineList  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE machineWidget AS WIDGET    NO-UNDO.
  DEFINE VARIABLE routingWidget AS WIDGET    NO-UNDO.
  DEFINE VARIABLE xPos          AS INTEGER   NO-UNDO.
  DEFINE VARIABLE yPos          AS INTEGER   NO-UNDO INIT 20.

  DELETE WIDGET-POOL 'routing' NO-ERROR.
  CREATE WIDGET-POOL 'routing' PERSISTENT.

  FOR EACH mach NO-LOCK
      WHERE mach.company EQ ipcCompany
      BREAK BY mach.sch-m-code
      :
      IF FIRST-OF(mach.sch-m-code) THEN
      cMachineList = cMachineList
                   + mach.sch-m-code
                   + ' - '
                   + REPLACE(mach.m-dscr,',','')
                   + ','
                   + mach.sch-m-code
                   + ','
                   .
  END. // each mach
  cMachineList = TRIM(cMachineList,',').

  DISABLE btnOk btnCancel WITH FRAME {&FRAME-NAME}.
  ASSIGN
    btnOK:HIDDEN = YES
    btnCancel:HIDDEN = YES
    .
  FOR EACH job-mch NO-LOCK
      WHERE job-mch.company EQ ipcCompany 
        AND job-mch.job-no  EQ ipcJobNo
        AND job-mch.job-no2 EQ ipiJobNo2
        AND job-mch.frm     EQ ipiForm,
      FIRST mach NO-LOCK
      WHERE mach.company EQ job-mch.company
        AND mach.m-code  EQ job-mch.m-code
         BY job-mch.seq
      :
    CREATE TOGGLE-BOX routingWidget IN WIDGET-POOL 'routing'
        ASSIGN
          FRAME = FRAME {&FRAME-NAME}:HANDLE
          FORMAT = 'X(256)'
          X = 5
          Y = yPos
          WIDTH-PIXELS = 250
          HEIGHT-PIXELS = 21
          SENSITIVE = NO
          HIDDEN = YES
          PRIVATE-DATA = job-mch.m-code
          LABEL = '[ ' + job-mch.m-code + ' - ' + mach.m-dscr + ' ]'
          .
    CREATE COMBO-BOX machineWidget IN WIDGET-POOL 'routing'
        ASSIGN
          FRAME = FRAME {&FRAME-NAME}:HANDLE
          FORMAT = 'X(256)'
          X = routingWidget:X + routingWidget:WIDTH-PIXELS + 20
          Y = yPos
          WIDTH-PIXELS = 250
          INNER-LINES = 100
          LIST-ITEM-PAIRS = cMachineList
          SENSITIVE = NO
          HIDDEN = YES
          PRIVATE-DATA = routingWidget:PRIVATE-DATA
          .
    ASSIGN
      yPos = yPos + routingWidget:HEIGHT-PIXELS + 5
      FRAME {&FRAME-NAME}:HEIGHT-PIXELS = yPos + 65
      btnOK:Y = yPos
      btnCancel:Y = yPos
      routingWidget:HIDDEN = NO
      routingWidget:SENSITIVE = YES
      routingWidget:CHECKED = YES
      machineWidget:HIDDEN = NO
      machineWidget:SENSITIVE = YES
      machineWidget:SCREEN-VALUE = routingWidget:PRIVATE-DATA
      .
  END. /* each job-mch */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResourceList Dialog-Frame 
PROCEDURE pResourceList :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    
    ASSIGN
        hWidget = FRAME {&FRAME-NAME}:HANDLE
        hWidget = hWidget:FIRST-CHILD
        hWidget = hWidget:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hWidget):
        IF hWidget:TYPE EQ "TOGGLE-BOX" AND
           hWidget:CHECKED THEN
        opcResources = opcResources + hWidget:PRIVATE-DATA + ",".
        IF hWidget:TYPE EQ "COMBO-BOX" AND
           hWidget:PRIVATE-DATA NE hWidget:SCREEN-VALUE THEN
        opcChanges = opcChanges + hWidget:PRIVATE-DATA + "," + hWidget:SCREEN-VALUE + ",".
        hWidget = hWidget:NEXT-SIBLING.
    END. // do while
    ASSIGN
        opcResources = TRIM(opcResources,",")
        opcChanges   = TRIM(opcChanges,",")
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

