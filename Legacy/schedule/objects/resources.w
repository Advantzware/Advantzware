&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: resources.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 6.22.2004

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

{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE ID AS CHARACTER NO-UNDO {{&includes}/initID.i}.
DEFINE VARIABLE boardType AS CHARACTER NO-UNDO.
DEFINE VARIABLE containerHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE scenario AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttblAvail NO-UNDO
  FIELD resource AS CHARACTER LABEL 'Resource' FORMAT 'X(20)'
    INDEX ttblAvail IS PRIMARY UNIQUE resource.

DEFINE TEMP-TABLE ttblList NO-UNDO
  FIELD resource AS CHARACTER LABEL 'Resource' FORMAT 'X(20)'
    INDEX ttblList IS PRIMARY UNIQUE resource.

DEFINE TEMP-TABLE priorityList NO-UNDO
  FIELD resource AS CHARACTER LABEL 'Resource' FORMAT 'X(20)'
  FIELD priority AS INTEGER LABEL 'Priority' FORMAT 'z,zz9'
  INDEX priority IS PRIMARY priority
  INDEX priorityList IS UNIQUE resource.

DEFINE BUFFER bPriority FOR priorityList.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME resourceFrame
&Scoped-define BROWSE-NAME availResources

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttblAvail priorityList ttblList

/* Definitions for BROWSE availResources                                */
&Scoped-define FIELDS-IN-QUERY-availResources ttblAvail.resource   
&Scoped-define ENABLED-FIELDS-IN-QUERY-availResources   
&Scoped-define SELF-NAME availResources
&Scoped-define QUERY-STRING-availResources FOR EACH ttblAvail
&Scoped-define OPEN-QUERY-availResources OPEN QUERY {&SELF-NAME} FOR EACH ttblAvail.
&Scoped-define TABLES-IN-QUERY-availResources ttblAvail
&Scoped-define FIRST-TABLE-IN-QUERY-availResources ttblAvail


/* Definitions for BROWSE listPriority                                  */
&Scoped-define FIELDS-IN-QUERY-listPriority priorityList.resource priorityList.priority   
&Scoped-define ENABLED-FIELDS-IN-QUERY-listPriority   
&Scoped-define SELF-NAME listPriority
&Scoped-define QUERY-STRING-listPriority FOR EACH priorityList
&Scoped-define OPEN-QUERY-listPriority OPEN QUERY {&SELF-NAME} FOR EACH priorityList.
&Scoped-define TABLES-IN-QUERY-listPriority priorityList
&Scoped-define FIRST-TABLE-IN-QUERY-listPriority priorityList


/* Definitions for BROWSE listResources                                 */
&Scoped-define FIELDS-IN-QUERY-listResources ttblList.resource   
&Scoped-define ENABLED-FIELDS-IN-QUERY-listResources   
&Scoped-define SELF-NAME listResources
&Scoped-define QUERY-STRING-listResources FOR EACH ttblList
&Scoped-define OPEN-QUERY-listResources OPEN QUERY {&SELF-NAME} FOR EACH ttblList.
&Scoped-define TABLES-IN-QUERY-listResources ttblList
&Scoped-define FIRST-TABLE-IN-QUERY-listResources ttblList


/* Definitions for FRAME resourceFrame                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 RECT-4 availResources ~
listResources btnSelectAvail btnClearAvail btnSelectList btnClearList ~
btnAdd btnRemove resourceUse btnSave btnRestore btnSelectList-2 ~
listPriority btnClearList-2 btnAdd-2 btnRemove-2 btnUp btnDown 
&Scoped-Define DISPLAYED-OBJECTS resourceUse 

/* Custom List Definitions                                              */
/* resourceObjects,List-2,List-3,List-4,List-5,List-6                   */
&Scoped-define resourceObjects btnSelectAvail btnClearAvail btnSelectList ~
btnClearList btnAdd btnRemove resourceUse btnSave btnRestore ~
btnSelectList-2 btnClearList-2 btnAdd-2 btnRemove-2 btnUp btnDown 
&Scoped-define List-2 RECT-2 RECT-4 availResources listResources ~
listPriority 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD nextPriority sObject 
FUNCTION nextPriority RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     LABEL ">> &Add >>" 
     SIZE 16 BY 1.14 TOOLTIP "Move Selected Resources".

DEFINE BUTTON btnAdd-2 
     LABEL ">> &Add >>" 
     SIZE 16 BY 1.14 TOOLTIP "Move Selected Resources".

DEFINE BUTTON btnClearAvail 
     LABEL "<< Clear All" 
     SIZE 16 BY 1.14 TOOLTIP "Clear All Downtime Selections".

DEFINE BUTTON btnClearList 
     LABEL "Clear All >>" 
     SIZE 16 BY 1.14 TOOLTIP "Clear All Downtime Selections".

DEFINE BUTTON btnClearList-2 
     LABEL "Clear All >>" 
     SIZE 16 BY 1.14 TOOLTIP "Clear All Downtime Selections".

DEFINE BUTTON btnDown 
     LABEL "Move &Down" 
     SIZE 16 BY 1.14 TOOLTIP "Remove Selected Resources".

DEFINE BUTTON btnRemove 
     LABEL "<< &Remove <<" 
     SIZE 16 BY 1.14 TOOLTIP "Remove Selected Resources".

DEFINE BUTTON btnRemove-2 
     LABEL "<< &Remove <<" 
     SIZE 16 BY 1.14 TOOLTIP "Remove Selected Resources".

DEFINE BUTTON btnRestore 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "Restore" 
     SIZE 7 BY 1.67 TOOLTIP "Restore to Last Save".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "&Save" 
     SIZE 7 BY 1.67 TOOLTIP "Save ALL Changes".

DEFINE BUTTON btnSelectAvail 
     LABEL "<< Select All" 
     SIZE 16 BY 1.14 TOOLTIP "Select All Downtime Records".

DEFINE BUTTON btnSelectList 
     LABEL "Select All >>" 
     SIZE 16 BY 1.14 TOOLTIP "Select All Downtime Records".

DEFINE BUTTON btnSelectList-2 
     LABEL "Select All >>" 
     SIZE 16 BY 1.14 TOOLTIP "Select All Downtime Records".

DEFINE BUTTON btnUp 
     LABEL "Move &Up" 
     SIZE 16 BY 1.14 TOOLTIP "Move Selected Resources".

DEFINE VARIABLE resourceUse AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "&Include", "Include",
"Include w/o &Jobs", "Without",
"&Exclude", "Exclude",
"&None", ""
     SIZE 22 BY 3.57
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 18 BY 15.95
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 92 BY 25.71
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 24 BY 25.24
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY availResources FOR 
      ttblAvail SCROLLING.

DEFINE QUERY listPriority FOR 
      priorityList SCROLLING.

DEFINE QUERY listResources FOR 
      ttblList SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE availResources
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS availResources sObject _FREEFORM
  QUERY availResources DISPLAY
      ttblAvail.resource
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 32 BY 25.33
         BGCOLOR 8 
         TITLE BGCOLOR 8 "Available Resources" ROW-HEIGHT-CHARS .62 EXPANDABLE.

DEFINE BROWSE listPriority
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS listPriority sObject _FREEFORM
  QUERY listPriority DISPLAY
      priorityList.resource
      priorityList.priority
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 32 BY 8.57
         BGCOLOR 8 
         TITLE BGCOLOR 8 "Priority List" ROW-HEIGHT-CHARS .62 EXPANDABLE.

DEFINE BROWSE listResources
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS listResources sObject _FREEFORM
  QUERY listResources DISPLAY
      ttblList.resource
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 32 BY 16.43
         BGCOLOR 8 
         TITLE BGCOLOR 8 "Resource List" ROW-HEIGHT-CHARS .62 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME resourceFrame
     availResources AT ROW 1.24 COL 2
     listResources AT ROW 1.24 COL 60
     btnSelectAvail AT ROW 1.71 COL 39
     btnClearAvail AT ROW 3.14 COL 39
     btnSelectList AT ROW 5.05 COL 39
     btnClearList AT ROW 6.48 COL 39
     btnAdd AT ROW 8.38 COL 39
     btnRemove AT ROW 9.81 COL 39
     resourceUse AT ROW 11.48 COL 36 NO-LABEL
     btnSave AT ROW 15.52 COL 39 HELP
          "Click to Save ALL Changes"
     btnRestore AT ROW 15.52 COL 48 HELP
          "Click to Cancel ALL Changes"
     btnSelectList-2 AT ROW 17.91 COL 39
     listPriority AT ROW 17.91 COL 60
     btnClearList-2 AT ROW 19.33 COL 39
     btnAdd-2 AT ROW 20.76 COL 39
     btnRemove-2 AT ROW 22.19 COL 39
     btnUp AT ROW 23.62 COL 39
     btnDown AT ROW 25.05 COL 39
     RECT-2 AT ROW 1.71 COL 35
     RECT-3 AT ROW 1 COL 1
     RECT-4 AT ROW 1.24 COL 35
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 .


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
         HEIGHT             = 25.91
         WIDTH              = 92.
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
/* SETTINGS FOR FRAME resourceFrame
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB availResources RECT-4 resourceFrame */
/* BROWSE-TAB listResources availResources resourceFrame */
/* BROWSE-TAB listPriority btnSelectList-2 resourceFrame */
ASSIGN 
       FRAME resourceFrame:SCROLLABLE       = FALSE
       FRAME resourceFrame:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE availResources IN FRAME resourceFrame
   2                                                                    */
/* SETTINGS FOR BUTTON btnAdd IN FRAME resourceFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnAdd-2 IN FRAME resourceFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnClearAvail IN FRAME resourceFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnClearList IN FRAME resourceFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnClearList-2 IN FRAME resourceFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnDown IN FRAME resourceFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnRemove IN FRAME resourceFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnRemove-2 IN FRAME resourceFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnRestore IN FRAME resourceFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnSave IN FRAME resourceFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnSelectAvail IN FRAME resourceFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnSelectList IN FRAME resourceFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnSelectList-2 IN FRAME resourceFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnUp IN FRAME resourceFrame
   1                                                                    */
/* SETTINGS FOR BROWSE listPriority IN FRAME resourceFrame
   2                                                                    */
/* SETTINGS FOR BROWSE listResources IN FRAME resourceFrame
   2                                                                    */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME resourceFrame
   2                                                                    */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME resourceFrame
   2                                                                    */
/* SETTINGS FOR RADIO-SET resourceUse IN FRAME resourceFrame
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE availResources
/* Query rebuild information for BROWSE availResources
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttblAvail.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE availResources */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE listPriority
/* Query rebuild information for BROWSE listPriority
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH priorityList.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE listPriority */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE listResources
/* Query rebuild information for BROWSE listResources
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttblList.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE listResources */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME resourceFrame
/* Query rebuild information for FRAME resourceFrame
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME resourceFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME availResources
&Scoped-define SELF-NAME availResources
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL availResources sObject
ON DEFAULT-ACTION OF availResources IN FRAME resourceFrame /* Available Resources */
DO:
  APPLY 'CHOOSE' TO btnAdd.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd sObject
ON CHOOSE OF btnAdd IN FRAME resourceFrame /* >> Add >> */
DO:
  RUN addResources.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd-2 sObject
ON CHOOSE OF btnAdd-2 IN FRAME resourceFrame /* >> Add >> */
DO:
  RUN addPriority.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearAvail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearAvail sObject
ON CHOOSE OF btnClearAvail IN FRAME resourceFrame /* << Clear All */
DO:
  IF availResources:NUM-SELECTED-ROWS NE 0 THEN
  ldummy = availResources:DESELECT-ROWS().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearList sObject
ON CHOOSE OF btnClearList IN FRAME resourceFrame /* Clear All >> */
DO:
  IF listResources:NUM-SELECTED-ROWS NE 0 THEN
  ldummy = listResources:DESELECT-ROWS().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearList-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearList-2 sObject
ON CHOOSE OF btnClearList-2 IN FRAME resourceFrame /* Clear All >> */
DO:
  IF listPriority:NUM-SELECTED-ROWS NE 0 THEN
  ldummy = listPriority:DESELECT-ROWS().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDown sObject
ON CHOOSE OF btnDown IN FRAME resourceFrame /* Move Down */
DO:
  IF NOT AVAILABLE priorityList OR
     priorityList.priority EQ nextPriority() - 1 THEN RETURN NO-APPLY.
  RUN movePriority (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove sObject
ON CHOOSE OF btnRemove IN FRAME resourceFrame /* << Remove << */
DO:
  RUN removeResources.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove-2 sObject
ON CHOOSE OF btnRemove-2 IN FRAME resourceFrame /* << Remove << */
DO:
  RUN removePriority.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRestore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestore sObject
ON CHOOSE OF btnRestore IN FRAME resourceFrame /* Restore */
DO:
  RUN getResources.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave sObject
ON CHOOSE OF btnSave IN FRAME resourceFrame /* Save */
DO:
  RUN saveResources.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectAvail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectAvail sObject
ON CHOOSE OF btnSelectAvail IN FRAME resourceFrame /* << Select All */
DO:
  IF CAN-FIND(FIRST ttblAvail WHERE ttblAvail.resource EQ ttblAvail.resource) THEN
  ldummy = availResources:SELECT-ALL().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectList sObject
ON CHOOSE OF btnSelectList IN FRAME resourceFrame /* Select All >> */
DO:
  IF CAN-FIND(FIRST ttblList) THEN
  ldummy = listResources:SELECT-ALL().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectList-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectList-2 sObject
ON CHOOSE OF btnSelectList-2 IN FRAME resourceFrame /* Select All >> */
DO:
  IF CAN-FIND(FIRST priorityList) THEN
  ldummy = listPriority:SELECT-ALL().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUp sObject
ON CHOOSE OF btnUp IN FRAME resourceFrame /* Move Up */
DO:
  IF NOT AVAILABLE priorityList OR
     priorityList.priority EQ 1 THEN RETURN NO-APPLY.
  RUN movePriority (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME listPriority
&Scoped-define SELF-NAME listPriority
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL listPriority sObject
ON DEFAULT-ACTION OF listPriority IN FRAME resourceFrame /* Priority List */
DO:
  APPLY 'CHOOSE' TO btnRemove-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME listResources
&Scoped-define SELF-NAME listResources
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL listResources sObject
ON DEFAULT-ACTION OF listResources IN FRAME resourceFrame /* Resource List */
DO:
  APPLY 'CHOOSE' TO btnRemove.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME resourceUse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL resourceUse sObject
ON VALUE-CHANGED OF resourceUse IN FRAME resourceFrame
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME availResources
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK sObject 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addPriority sObject 
PROCEDURE addPriority :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF availResources:NUM-SELECTED-ROWS NE 0 THEN
    DO i = 1 TO availResources:NUM-SELECTED-ROWS:
      ldummy = availResources:FETCH-SELECTED-ROW(i).
      RUN addPriorityList (ttblAvail.resource).
    END.
    IF listResources:NUM-SELECTED-ROWS NE 0 THEN
    DO i = 1 TO listResources:NUM-SELECTED-ROWS:
      ldummy = listResources:FETCH-SELECTED-ROW(i).
      RUN addPriorityList (ttblList.resource).
    END.
    RUN openQueries.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addPriorityList sObject 
PROCEDURE addPriorityList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.

  IF CAN-FIND(FIRST priorityList
     WHERE priorityList.resource EQ ipResource) THEN RETURN.
  CREATE priorityList.
  ASSIGN
    priorityList.priority = nextPriority()
    priorityList.resource = ipResource.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addResources sObject 
PROCEDURE addResources :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF availResources:NUM-SELECTED-ROWS EQ 0 THEN RETURN.
    DO i = 1 TO availResources:NUM-SELECTED-ROWS:
      ldummy = availResources:FETCH-SELECTED-ROW(i).
      IF CAN-FIND(FIRST ttblList
         WHERE ttblList.resource EQ ttblAvail.resource) THEN NEXT.
      CREATE ttblList.
      ttblList.resource = ttblAvail.resource.
      /* DELETE ttblAvail. */
    END.
    RUN openQueries.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE containerHandle sObject 
PROCEDURE containerHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipHandle AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER ipBoardType AS CHARACTER NO-UNDO.

  ASSIGN
    containerHandle = ipHandle
    boardType = ipBoardType.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  HIDE FRAME resourceFrame.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
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
  DEFINE VARIABLE resourceName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE priorityValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE useResource AS CHARACTER NO-UNDO.

  EMPTY TEMP-TABLE ttblAvail.
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/resources.dat')) NO-ECHO.
  REPEAT WITH FRAME {&FRAME-NAME}:
    IMPORT ^ resourceName.
    IF CAN-FIND(ttblAvail WHERE ttblAvail.resource EQ resourceName) THEN NEXT.
    CREATE ttblAvail.
    ttblAvail.resource = resourceName.
  END.
  EMPTY TEMP-TABLE ttblList.
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/resourceList.dat')) NO-ECHO.
  IMPORT useResource.
  REPEAT WITH FRAME {&FRAME-NAME}:
    IMPORT resourceName.
    CREATE ttblList.
    ttblList.resource = resourceName.
    /*
    FIND ttblAvail EXCLUSIVE-LOCK
         WHERE ttblAvail.resource EQ ttblList.resource NO-ERROR.
    IF AVAILABLE ttblAvail THEN
    DELETE ttblAvail.
    */
  END.
  EMPTY TEMP-TABLE priorityList.
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/priorityList.dat')) NO-ECHO.
  REPEAT WITH FRAME {&FRAME-NAME}:
    IMPORT resourceName priorityValue.
    CREATE priorityList.
    ASSIGN
      priorityList.resource = resourceName
      priorityList.priority = priorityValue.
  END.
  INPUT CLOSE.
  ASSIGN
    resourceUse:SCREEN-VALUE IN FRAME {&FRAME-NAME} = useResource
    resourceUse.
  {&OPEN-QUERY-availResources}
  RUN openQueries.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ID sObject 
PROCEDURE ID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/id.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initResources sObject 
PROCEDURE initResources :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN ID.
  RUN getResources.
  IF boardType NE '{&Board}' THEN
  DISABLE {&resourceObjects} WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE movePriority sObject 
PROCEDURE movePriority :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipMove AS INTEGER NO-UNDO.

  DEFINE VARIABLE lvRowID AS ROWID NO-UNDO.

  FIND bPriority EXCLUSIVE-LOCK
       WHERE bPriority.priority EQ priorityList.priority + ipMove.
  ASSIGN
    bPriority.priority = priorityList.priority
    priorityList.priority = priorityList.priority + ipMove
    lvRowID = ROWID(priorityList).
  {&OPEN-QUERY-listPriority}
  REPOSITION listPriority TO ROWID lvRowID.
  APPLY 'ENTRY' TO BROWSE listPriority.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueries sObject 
PROCEDURE openQueries :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {&OPEN-QUERY-listResources}
  {&OPEN-QUERY-listPriority}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE removePriority sObject 
PROCEDURE removePriority :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF listPriority:NUM-SELECTED-ROWS EQ 0 THEN RETURN.
    DO i = 1 TO listPriority:NUM-SELECTED-ROWS:
      ldummy = listPriority:FETCH-SELECTED-ROW(i).
      DELETE priorityList.
    END.
    i = 0.
    FOR EACH priorityList EXCLUSIVE-LOCK:
      ASSIGN
        i = i + 1
        priorityList.priority = i.
    END.
    RUN openQueries.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE removeResources sObject 
PROCEDURE removeResources :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF listResources:NUM-SELECTED-ROWS EQ 0 THEN RETURN.
    DO i = 1 TO listResources:NUM-SELECTED-ROWS:
      ldummy = listResources:FETCH-SELECTED-ROW(i).
      DELETE ttblList.
    END.
    RUN openQueries.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveResources sObject 
PROCEDURE saveResources :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OUTPUT TO VALUE(SEARCH('{&data}/' + ID + '/resourceList.dat')) NO-ECHO.
  EXPORT resourceUse.
  FOR EACH ttblList NO-LOCK:
    EXPORT ttblList.resource.
  END.
  OUTPUT TO VALUE(SEARCH('{&data}/' + ID + '/priorityList.dat')) NO-ECHO.
  FOR EACH priorityList NO-LOCK:
    EXPORT priorityList.
  END.
  OUTPUT CLOSE.
  MESSAGE 'Resource List Saved.' SKIP(1)
    'Scheduler' boardType 'must be closed and' SKIP
    'reopened for changes to take effect.' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION nextPriority sObject 
FUNCTION nextPriority RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND LAST bPriority NO-LOCK USE-INDEX priority NO-ERROR.
  RETURN (IF AVAILABLE bPriority THEN bPriority.priority ELSE 0) + 1.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

