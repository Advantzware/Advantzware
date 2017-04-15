&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          jobs             PROGRESS
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

&SCOPED-DEFINE useTtbl ttblJob

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/ttblJob.i}
{{&includes}/filterVars.i}
{{&viewers}/includes/sharedVars.i NEW}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE containerHandle AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE jobStatus NO-UNDO
  FIELD jobSequence LIKE ttblJob.jobSequence
  FIELD job LIKE ttblJob.job
  FIELD resource LIKE ttblJob.resource
  FIELD jobType AS CHARACTER FORMAT 'X'
  FIELD jobStatus AS LOGICAL FORMAT 'Yes/No' EXTENT {&statusExtent}
    INDEX jobStatus IS PRIMARY job jobSequence.

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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME browseJob

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES jobStatus

/* Definitions for BROWSE browseJob                                     */
&Scoped-define FIELDS-IN-QUERY-browseJob jobStatus.jobSequence jobStatus.job jobStatus.resource jobStatus.jobType jobStatus.jobStatus[1] jobStatus.jobStatus[2] jobStatus.jobStatus[3] jobStatus.jobStatus[4] jobStatus.jobStatus[5] jobStatus.jobStatus[6] jobStatus.jobStatus[7] jobStatus.jobStatus[8] jobStatus.jobStatus[9] jobStatus.jobStatus[10] jobStatus.jobStatus[11] jobStatus.jobStatus[12] jobStatus.jobStatus[13] jobStatus.jobStatus[14]   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseJob   
&Scoped-define SELF-NAME browseJob
&Scoped-define QUERY-STRING-browseJob FOR EACH jobStatus NO-LOCK
&Scoped-define OPEN-QUERY-browseJob OPEN QUERY {&SELF-NAME} FOR EACH jobStatus NO-LOCK.
&Scoped-define TABLES-IN-QUERY-browseJob jobStatus
&Scoped-define FIRST-TABLE-IN-QUERY-browseJob jobStatus


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-browseJob}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS browseJob btnApply btnCancel btnExit 

/* Custom List Definitions                                              */
/* ttblResourceFields,phraseFields,List-3,List-4,List-5,List-6          */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnApply 
     LABEL "&Apply Updates" 
     SIZE 17 BY 1.24.

DEFINE BUTTON btnCancel 
     LABEL "&Cancel Updates" 
     SIZE 17 BY 1.24.

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "schedule/images/exit1.bmp":U
     LABEL "E&xit" 
     SIZE 5.2 BY 1.24 TOOLTIP "Exit (Alt-X)"
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 149.6 BY 1.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseJob FOR 
      jobStatus SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseJob sObject _FREEFORM
  QUERY browseJob DISPLAY
      jobStatus.jobSequence
  jobStatus.job
  jobStatus.resource
  jobStatus.jobType FORMAT 'X' LABEL 'T'
  jobStatus.jobStatus[1]
  jobStatus.jobStatus[2]
  jobStatus.jobStatus[3]
  jobStatus.jobStatus[4]
  jobStatus.jobStatus[5]
  jobStatus.jobStatus[6]
  jobStatus.jobStatus[7]
  jobStatus.jobStatus[8]
  jobStatus.jobStatus[9]
  jobStatus.jobStatus[10]
  jobStatus.jobStatus[11]
  jobStatus.jobStatus[12]
  jobStatus.jobStatus[13]
  jobStatus.jobStatus[14]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 149.6 BY 21.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     browseJob AT ROW 1 COL 1
     btnApply AT ROW 22.19 COL 109
     btnCancel AT ROW 22.19 COL 127
     btnExit AT ROW 22.19 COL 145 HELP
          "Click to Exit"
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS TOP-ONLY NO-UNDERLINE THREE-D 
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
/* BROWSE-TAB browseJob 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       browseJob:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 3
       browseJob:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseJob
/* Query rebuild information for BROWSE browseJob
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH jobStatus NO-LOCK.
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

&Scoped-define SELF-NAME btnApply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnApply sObject
ON CHOOSE OF btnApply IN FRAME F-Main /* Apply Updates */
DO:
  RUN processUpdates (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel sObject
ON CHOOSE OF btnCancel IN FRAME F-Main /* Cancel Updates */
DO:
  RUN processUpdates (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit sObject
ON CHOOSE OF btnExit IN FRAME F-Main /* Exit */
DO:
  RUN closePending IN containerHandle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseJob
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
  RUN setCellLabels.
  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSBStatus sObject 
PROCEDURE getSBStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lvJobSequence AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvJob AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvResource AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvType AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvStatus AS LOGICAL NO-UNDO EXTENT 14.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  IF SEARCH('{&updates}\' + ID + '\updatesPending.dat') EQ ? THEN RETURN.
  SESSION:SET-WAIT-STATE('General').
  INPUT FROM VALUE('{&updates}\' + ID + '\updatesPending.dat') APPEND.
  REPEAT:
    IMPORT lvJobSequence lvJob lvResource lvType lvStatus.
    CREATE jobStatus.
    ASSIGN
      jobStatus.jobSequence = lvJobSequence
      jobStatus.job = lvJob
      jobStatus.resource = lvResource
      jobStatus.jobType = lvType.
    DO i = 1 TO EXTENT(lvStatus):
      jobStatus.jobStatus[i] = lvStatus[i].
    END.
  END. /* repeat */
  INPUT CLOSE.
  SESSION:SET-WAIT-STATE('').

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
  RUN getConfiguration.
  RUN getSBStatus.
  RUN displayFields NO-ERROR.
  APPLY 'ENTRY':U TO BROWSE {&BROWSE-NAME}.

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
  DEFINE INPUT PARAMETER ipContainerHandle AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER ipBoardHandle AS HANDLE NO-UNDO.

  ASSIGN
    containerHandle = ipContainerHandle
    boardHandle = ipBoardHandle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processUpdates sObject 
PROCEDURE processUpdates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipProcessUpdates AS LOGICAL NO-UNDO.

  DEFINE VARIABLE lvJobSequence AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvJob AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvResource AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvJobType AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvJobStatus AS LOGICAL NO-UNDO EXTENT {&statusExtent}.
  DEFINE VARIABLE lvStatusTimeStamp AS CHARACTER NO-UNDO EXTENT {&statusExtent}.
  DEFINE VARIABLE updatesFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.


  updatesFile = SEARCH('{&updates}\' + ID + '\updatesPending.dat').
  IF updatesFile EQ ? THEN RETURN.

  SESSION:SET-WAIT-STATE('General').
  IF ipProcessUpdates THEN
  DO:
    INPUT FROM VALUE(updatesFile) NO-ECHO.
    REPEAT:
      IMPORT lvJobSequence lvJob lvResource lvJobType lvJobStatus lvStatusTimeStamp.
      IF lvJobType EQ 'S' THEN /* schedule board */
      FOR EACH ttblJob EXCLUSIVE-LOCK
          WHERE ttblJob.job EQ lvJob
            AND ttblJob.resource EQ lvResource:
        DO i = 1 TO EXTENT(lvJobStatus):
          ASSIGN
            ttblJob.jobStatus[i] = lvJobStatus[i]
            ttblJob.statusTimeStamp[i] = lvStatusTimeStamp[i].
        END. /* do i */
      END. /* each ttbljob */
      ELSE /* jobtype eq 'P' - pending job */
      FOR EACH pendingJob EXCLUSIVE-LOCK
          WHERE pendingJob.job EQ lvJob
            AND pendingJob.resource EQ lvResource:
        IF NOT AVAILABLE pendingJob THEN NEXT.
        DO i = 1 TO EXTENT(lvJobStatus):
          ASSIGN
            pendingJob.jobStatus[i] = lvJobStatus[i]
            pendingJob.statusTimeStamp[i] = lvStatusTimeStamp[i].
        END. /* do i */
      END. /* each pendingjob */
    END. /* repeat */
    INPUT CLOSE.
    MESSAGE 'Updates Pending Applied!' VIEW-AS ALERT-BOX.
    RUN buildBoard IN boardHandle (YES).
  END. /* if ipprocessupdates */
  OS-DELETE VALUE(REPLACE(updatesFile,'.dat','.txt')).
  OS-RENAME VALUE(updatesFile) VALUE(REPLACE(updatesFile,'.dat','.txt')).
  SESSION:SET-WAIT-STATE('').
  RUN closePending IN containerHandle.

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

