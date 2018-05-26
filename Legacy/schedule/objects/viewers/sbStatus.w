&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
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
{{&includes}/filterVars.i}
{{&includes}/ttblJob.i}
{{&viewers}/includes/sharedVars.i NEW}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE resourceValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE popupHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE ttblJobRowID AS ROWID NO-UNDO.

{{&viewers}/includes/browseDef.i}

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
{{&includes}/lockWindowUpdate.i}
&ENDIF

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
&Scoped-define INTERNAL-TABLES ttblJob

/* Definitions for BROWSE browseJob                                     */
&Scoped-define FIELDS-IN-QUERY-browseJob ttblJob.jobSequence ttblJob.job ttblJob.resource ttblJob.jobType ttblJob.jobStatus[1] ttblJob.jobStatus[2] ttblJob.jobStatus[3] ttblJob.jobStatus[4] ttblJob.jobStatus[5] ttblJob.jobStatus[6] ttblJob.jobStatus[7] ttblJob.jobStatus[8] ttblJob.jobStatus[9] ttblJob.jobStatus[10] ttblJob.jobStatus[11] ttblJob.jobStatus[12] ttblJob.jobStatus[13] ttblJob.jobStatus[14]   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseJob ttblJob.jobStatus[1]  ttblJob.jobStatus[2]  ttblJob.jobStatus[3]  ttblJob.jobStatus[4]  ttblJob.jobStatus[5]  ttblJob.jobStatus[6]  ttblJob.jobStatus[7]  ttblJob.jobStatus[8]  ttblJob.jobStatus[9]  ttblJob.jobStatus[10]  ttblJob.jobStatus[11]  ttblJob.jobStatus[12]  ttblJob.jobStatus[13]  ttblJob.jobStatus[14]   
&Scoped-define ENABLED-TABLES-IN-QUERY-browseJob ttblJob
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-browseJob ttblJob
&Scoped-define SELF-NAME browseJob
&Scoped-define QUERY-STRING-browseJob FOR EACH ttblJob NO-LOCK WHERE (ttblJob.job BEGINS jobPhrase OR jobPhrase EQ '') AND ttblJob.job GE jobValueLo AND ttblJob.job LE jobValueHi AND (ttblJob.resource EQ resources OR resources EQ '<Select ...>') ~{&dateRangePhrase}
&Scoped-define OPEN-QUERY-browseJob OPEN QUERY {&SELF-NAME} FOR EACH ttblJob NO-LOCK WHERE (ttblJob.job BEGINS jobPhrase OR jobPhrase EQ '') AND ttblJob.job GE jobValueLo AND ttblJob.job LE jobValueHi AND (ttblJob.resource EQ resources OR resources EQ '<Select ...>') ~{&dateRangePhrase}.
&Scoped-define TABLES-IN-QUERY-browseJob ttblJob
&Scoped-define FIRST-TABLE-IN-QUERY-browseJob ttblJob


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-browseJob}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS jobPhrase resources btnUpdatesPending ~
customCheckoffValue browseJob 
&Scoped-Define DISPLAYED-OBJECTS jobPhrase resources customCheckoffValue 

/* Custom List Definitions                                              */
/* ttblResourceFields,phraseFields,List-3,List-4,List-5,List-6          */
&Scoped-define ttblResourceFields customCheckoffValue 
&Scoped-define phraseFields jobPhrase 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
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

DEFINE VARIABLE jobPhrase AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Job" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 149.6 BY 1.19.

DEFINE VARIABLE customCheckoffValue AS LOGICAL INITIAL no 
     LABEL "&Apply Custom Value Checkoff to Whole Job" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseJob FOR 
      ttblJob SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseJob sObject _FREEFORM
  QUERY browseJob DISPLAY
      ttblJob.jobSequence LABEL-BGCOLOR 14
  ttblJob.job LABEL-BGCOLOR 14
  ttblJob.resource
  ttblJob.jobType FORMAT 'X' LABEL 'T'
  ttblJob.jobStatus[1]
  ttblJob.jobStatus[2]
  ttblJob.jobStatus[3]
  ttblJob.jobStatus[4]
  ttblJob.jobStatus[5]
  ttblJob.jobStatus[6]
  ttblJob.jobStatus[7]
  ttblJob.jobStatus[8]
  ttblJob.jobStatus[9]
  ttblJob.jobStatus[10]
  ttblJob.jobStatus[11]
  ttblJob.jobStatus[12]
  ttblJob.jobStatus[13]
  ttblJob.jobStatus[14]
ENABLE
  ttblJob.jobStatus[1]
  ttblJob.jobStatus[2]
  ttblJob.jobStatus[3]
  ttblJob.jobStatus[4]
  ttblJob.jobStatus[5]
  ttblJob.jobStatus[6]
  ttblJob.jobStatus[7]
  ttblJob.jobStatus[8]
  ttblJob.jobStatus[9]
  ttblJob.jobStatus[10]
  ttblJob.jobStatus[11]
  ttblJob.jobStatus[12]
  ttblJob.jobStatus[13]
  ttblJob.jobStatus[14]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 149.6 BY 21.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     jobPhrase AT ROW 1.1 COL 4 COLON-ALIGNED
     resources AT ROW 1.1 COL 27.8 HELP
          "Select Resource"
     btnUpdatesPending AT ROW 1.1 COL 73
     customCheckoffValue AT ROW 1.1 COL 105 HELP
          "Select to Apply Checkoff to Whole Job vs. Each Resource"
     browseJob AT ROW 2.19 COL 1
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
/* BROWSE-TAB browseJob customCheckoffValue F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       browseJob:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 3
       browseJob:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR TOGGLE-BOX customCheckoffValue IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN jobPhrase IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
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
  IF CAN-DO('job,jobSequence',{&BROWSE-NAME}:CURRENT-COLUMN:NAME) THEN
  DO:
    columnLabel = {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
    RUN reopenBrowse.
  END.
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME customCheckoffValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customCheckoffValue sObject
ON VALUE-CHANGED OF customCheckoffValue IN FRAME F-Main /* Apply Custom Value Checkoff to Whole Job */
DO:
  ASSIGN {&SELF-NAME}
    customCheckoff = {&SELF-NAME}.
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
  columnLabel = IF {&SELF-NAME} BEGINS '<Select' THEN 'job' ELSE 'jobSequence'.
  RUN reopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
    FIND ttblJob EXCLUSIVE-LOCK WHERE ttblJob.job EQ lvJob
                                  AND ttblJob.resource EQ lvResource NO-ERROR.
    IF NOT AVAILABLE ttblJob THEN NEXT.
    ASSIGN
      ttblJob.startDate = fromDate
      ttblJob.endDate = toDate.
    DO i = 1 TO EXTENT(lvStatus):
      ttblJob.jobStatus[i] = lvStatus[i].
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
  customCheckoffValue:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(customCheckoff).
  RUN getResources.
  RUN getSBStatus.
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
  
  IF VALID-HANDLE(adm-broker-hdl) THEN
  DO:
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
  OUTPUT TO VALUE('{&updates}\' + ID + '\updatesPending.dat') APPEND.
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

