&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: moveResource.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 2.12.2004

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
&SCOPED-DEFINE browseName jobBrowse

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
DEFINE VARIABLE useDeptSort AS LOGICAL NO-UNDO.

{{&viewers}/includes/browseDef.i}

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
{{&includes}/lockWindowUpdate.i}
&ENDIF

useDeptSort = SEARCH(findProgram('{&data}/',ID,'/useDeptSort.dat')) NE ?.

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
&Scoped-define INTERNAL-TABLES pendingJob

/* Definitions for BROWSE browseJob                                     */
&Scoped-define FIELDS-IN-QUERY-browseJob pendingJob.jobSequence pendingJob.job pendingJob.resource pendingJob.resourceSequence pendingJob.jobCompleted   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseJob   
&Scoped-define SELF-NAME browseJob
&Scoped-define QUERY-STRING-browseJob FOR EACH pendingJob NO-LOCK WHERE (pendingJob.job BEGINS jobPhrase OR jobPhrase EQ '') AND pendingJob.job GE jobValueLo AND pendingJob.job LE jobValueHi AND (pendingJob.resource EQ resources OR resources EQ '<Select ...>') AND pendingJob.jobBrowseFlag EQ YES ~{&dateRangePhrase} BY pendingJob.jobSequence
&Scoped-define OPEN-QUERY-browseJob OPEN QUERY {&SELF-NAME} FOR EACH pendingJob NO-LOCK WHERE (pendingJob.job BEGINS jobPhrase OR jobPhrase EQ '') AND pendingJob.job GE jobValueLo AND pendingJob.job LE jobValueHi AND (pendingJob.resource EQ resources OR resources EQ '<Select ...>') AND pendingJob.jobBrowseFlag EQ YES ~{&dateRangePhrase} BY pendingJob.jobSequence.
&Scoped-define TABLES-IN-QUERY-browseJob pendingJob
&Scoped-define FIRST-TABLE-IN-QUERY-browseJob pendingJob


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-browseJob}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS resources btnFilter btnPrint btnMoveResource ~
btnRefresh RECT-1 jobPhrase btnSort browseJob 
&Scoped-Define DISPLAYED-OBJECTS resources jobPhrase sortableColumns 

/* Custom List Definitions                                              */
/* ttblResourceFields,phraseFields,List-3,List-4,List-5,List-6          */
&Scoped-define ttblResourceFields btnPrint 
&Scoped-define phraseFields btnPrint jobPhrase 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnFilter 
     IMAGE-UP FILE "schedule/images/filterwindow.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Set Filter Values".

DEFINE BUTTON btnMoveResource 
     IMAGE-UP FILE "schedule/images/moveresource.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Move Resource".

DEFINE BUTTON btnPrint 
     IMAGE-UP FILE "schedule/images/print.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Print".

DEFINE BUTTON btnRefresh 
     IMAGE-UP FILE "schedule/images/refresh.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Refresh".

DEFINE BUTTON btnSort 
     LABEL "&Ascending" 
     SIZE 13 BY 1 TOOLTIP "Toggle Ascending/Descending Sort".

DEFINE VARIABLE resources AS CHARACTER FORMAT "X(256)":U INITIAL "<Select ...>" 
     LABEL "&Resource" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 30
     LIST-ITEMS "<Select ...>" 
     DROP-DOWN-LIST
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE jobPhrase AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Job" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE sortableColumns AS CHARACTER FORMAT "X(256)":U INITIAL " Sortable Columns" 
      VIEW-AS TEXT 
     SIZE 18 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 149.6 BY 1.19.

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
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 149.6 BY 8.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     resources AT ROW 1 COL 109.4 HELP
          "Select Resource"
     btnFilter AT ROW 1.05 COL 31 HELP
          "Click to Set Filter Values"
     btnPrint AT ROW 1.05 COL 69 HELP
          "Click to Access Print Utility"
     btnMoveResource AT ROW 1.05 COL 95 HELP
          "Click to Move Resource"
     btnRefresh AT ROW 1.05 COL 102 HELP
          "Click to Refresh Resource Browser"
     jobPhrase AT ROW 1.1 COL 5 COLON-ALIGNED
     btnSort AT ROW 1.1 COL 55
     browseJob AT ROW 2.19 COL 1
     sortableColumns AT ROW 1.1 COL 35 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1 COL 1
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
         HEIGHT             = 9.67
         WIDTH              = 150.4.
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
/* BROWSE-TAB browseJob btnSort F-Main */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:HEIGHT           = 9.67
       FRAME F-Main:WIDTH            = 150.4.

ASSIGN 
       browseJob:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 5
       browseJob:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR BUTTON btnPrint IN FRAME F-Main
   1 2                                                                  */
ASSIGN 
       btnPrint:PRIVATE-DATA IN FRAME F-Main     = 
                "boardObject".

/* SETTINGS FOR FILL-IN jobPhrase IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR COMBO-BOX resources IN FRAME F-Main
   ALIGN-L                                                              */
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
AND pendingJob.jobBrowseFlag EQ YES
~{&dateRangePhrase} BY pendingJob.jobSequence.
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
ON ROW-DISPLAY OF browseJob IN FRAME F-Main
DO:
  {{&viewers}/includes/rowDisplay.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob sObject
ON START-SEARCH OF browseJob IN FRAME F-Main
DO:
  IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN
  DO:
    columnLabel = {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
    RUN reopenBrowse.
  END.
  RETURN NO-APPLY.
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
  RUN moveResource.
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


&Scoped-define SELF-NAME jobPhrase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobPhrase sObject
ON VALUE-CHANGED OF jobPhrase IN FRAME F-Main /* Job */
DO:
  ASSIGN {&SELF-NAME}.
  RUN reopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME resources
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL resources sObject
ON VALUE-CHANGED OF resources IN FRAME F-Main /* Resource */
DO:
  ASSIGN {&SELF-NAME}.
  RUN reopenBrowse.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveResource sObject 
PROCEDURE moveResource :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE newResource AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  IF BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS EQ 0 THEN RETURN.
  RUN {&prompts}/moveResource.w (OUTPUT newResource).
  IF newResource EQ ? THEN RETURN.
  MESSAGE 'Move Selected Job(s) to Resource "' + newResource + '"?'
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE moveToResource AS LOGICAL.
  IF NOT moveToResource THEN RETURN.
  FIND FIRST ttblResource
       WHERE ttblResource.resource EQ newResource NO-ERROR.
  DO i = 1 TO BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS:
    BROWSE {&BROWSE-NAME}:FETCH-SELECTED-ROW(i).
    ASSIGN
      pendingJob.resource = newResource
      pendingJob.altResource = newResource
      pendingJob.department = IF useDeptSort THEN ttblResource.department ELSE '0000'
      .
  END. /* do i */
  RUN reopenBrowse.

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
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.

  boardHandle = ipHandle.
  RUN getConfiguration.
  RUN getResources.
  ASSIGN
    resources:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ipResource
    resources = ipResource.
  RUN displayFields NO-ERROR.
  RUN reopenBrowse.
  APPLY 'ENTRY':U TO BROWSE {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

