&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: AOA/aoaBatchParam.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author:  Ron Stark
  Created: 12.5.2018

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE aoaCompany    AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaProgramID  AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaUserID     AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaTitle      AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaColumns    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE char-hdl      AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle       AS HANDLE    NO-UNDO.
DEFINE VARIABLE iHeightPixels AS INTEGER   NO-UNDO.
DEFINE VARIABLE iWidthPixels  AS INTEGER   NO-UNDO.

DEFINE VARIABLE h_aoaParam    AS HANDLE    NO-UNDO.

DEFINE TEMP-TABLE ttUserPrint NO-UNDO LIKE user-print
    FIELD userPrintRowID AS ROWID EXTENT 2
    .
DEFINE TEMP-TABLE ttParamValue NO-UNDO
    FIELD paramOrder AS INTEGER   LABEL "Index"       FORMAT ">>9"
    FIELD batch-seq  AS INTEGER
    FIELD prgmName   AS CHARACTER 
    FIELD paramLabel AS CHARACTER LABEL "Param Label" FORMAT "x(31)"
    FIELD paramValue AS CHARACTER LABEL "Param Value" FORMAT "x(30)"
    FIELD paramName  AS CHARACTER LABEL "Param Name"  FORMAT "x(20)"
        INDEX paramOrder IS PRIMARY
            paramOrder
            .
DEFINE BUFFER bUserPrint      FOR user-print.
DEFINE BUFFER jasperUserPrint FOR user-print.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME browseParamValue

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttParamValue ttUserPrint

/* Definitions for BROWSE browseParamValue                              */
&Scoped-define FIELDS-IN-QUERY-browseParamValue ttParamValue.paramLabel ttParamValue.paramValue ttParamValue.paramName ttParamValue.paramOrder   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseParamValue   
&Scoped-define SELF-NAME browseParamValue
&Scoped-define QUERY-STRING-browseParamValue FOR EACH ttParamValue      WHERE ttParamValue.batch-seq EQ ttUserPrint.batch-seq      AND ttParamValue.prgmName EQ ttUserPrint.prgmName
&Scoped-define OPEN-QUERY-browseParamValue OPEN QUERY {&SELF-NAME} FOR EACH ttParamValue      WHERE ttParamValue.batch-seq EQ ttUserPrint.batch-seq      AND ttParamValue.prgmName EQ ttUserPrint.prgmName.
&Scoped-define TABLES-IN-QUERY-browseParamValue ttParamValue
&Scoped-define FIRST-TABLE-IN-QUERY-browseParamValue ttParamValue


/* Definitions for BROWSE browseUserPrint                               */
&Scoped-define FIELDS-IN-QUERY-browseUserPrint ttUserPrint.batch-seq ttUserPrint.prog-title ttUserPrint.last-date STRING(ttUserPrint.last-time,"hh:mm:ss am")   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseUserPrint   
&Scoped-define SELF-NAME browseUserPrint
&Scoped-define QUERY-STRING-browseUserPrint FOR EACH ttUserPrint
&Scoped-define OPEN-QUERY-browseUserPrint OPEN QUERY {&SELF-NAME} FOR EACH ttUserPrint.
&Scoped-define TABLES-IN-QUERY-browseUserPrint ttUserPrint
&Scoped-define FIRST-TABLE-IN-QUERY-browseUserPrint ttUserPrint


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-browseParamValue}~
    ~{&OPEN-QUERY-browseUserPrint}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS browseUserPrint browseParamValue btnAdd ~
btnSave btnDelete btnApply 

/* Custom List Definitions                                              */
/* List-1,List-2,batchObjects,batchShowHide,List-5,List-6               */
&Scoped-define batchObjects browseUserPrint browseParamValue btnAdd btnSave ~
btnDelete btnApply 
&Scoped-define batchShowHide browseUserPrint browseParamValue btnAdd ~
btnSave btnDelete btnApply 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "AOA/images/navigate_plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "&Add" 
     SIZE 4.4 BY 1 TOOLTIP "Add New Task ID".

DEFINE BUTTON btnApply 
     IMAGE-UP FILE "AOA/images/aoaapply.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "&Apply" 
     SIZE 4.4 BY 1 TOOLTIP "Apply Task Values to Parameter Values".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "AOA/images/navigate_cross.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "&Delete" 
     SIZE 4.4 BY 1 TOOLTIP "Delete Task ID".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "AOA\images\navigate_check.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "&Save" 
     SIZE 4.4 BY 1 TOOLTIP "Save Parameter Values to Task ID".

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 21 BY 1.43
     BGCOLOR 15 FGCOLOR 1 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseParamValue FOR 
      ttParamValue SCROLLING.

DEFINE QUERY browseUserPrint FOR 
      ttUserPrint SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseParamValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseParamValue sObject _FREEFORM
  QUERY browseParamValue DISPLAY
      ttParamValue.paramLabel
ttParamValue.paramValue
ttParamValue.paramName
ttParamValue.paramOrder
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 67 BY 7.14
         TITLE "Task Parameter Values".

DEFINE BROWSE browseUserPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseUserPrint sObject _FREEFORM
  QUERY browseUserPrint DISPLAY
      ttUserPrint.batch-seq LABEL "Task ID"
ttUserPrint.prog-title
ttUserPrint.last-date LABEL "Date"
STRING(ttUserPrint.last-time,"hh:mm:ss am") LABEL "Time" FORMAT "x(12)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80 BY 5.48
         TITLE "Tasks".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     browseUserPrint AT ROW 1 COL 1 WIDGET-ID 500
     browseParamValue AT ROW 1 COL 82 WIDGET-ID 600
     btnAdd AT ROW 6.95 COL 61 HELP
          "Add New Task ID" WIDGET-ID 46
     btnSave AT ROW 6.95 COL 66 HELP
          "Save Parameter Values to Task ID" WIDGET-ID 18
     btnDelete AT ROW 6.95 COL 71 HELP
          "Delete Task ID" WIDGET-ID 4
     btnApply AT ROW 6.95 COL 76 HELP
          "Apply Task Values to Parameter Values" WIDGET-ID 16
     RECT-1 AT ROW 6.71 COL 60 WIDGET-ID 50
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
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
         HEIGHT             = 7.29
         WIDTH              = 148.4.
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
/* BROWSE-TAB browseUserPrint 1 F-Main */
/* BROWSE-TAB browseParamValue RECT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE browseParamValue IN FRAME F-Main
   3 4                                                                  */
/* SETTINGS FOR BROWSE browseUserPrint IN FRAME F-Main
   3 4                                                                  */
ASSIGN 
       browseUserPrint:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 1.

/* SETTINGS FOR BUTTON btnAdd IN FRAME F-Main
   3 4                                                                  */
/* SETTINGS FOR BUTTON btnApply IN FRAME F-Main
   3 4                                                                  */
/* SETTINGS FOR BUTTON btnDelete IN FRAME F-Main
   3 4                                                                  */
/* SETTINGS FOR BUTTON btnSave IN FRAME F-Main
   3 4                                                                  */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseParamValue
/* Query rebuild information for BROWSE browseParamValue
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttParamValue
     WHERE ttParamValue.batch-seq EQ ttUserPrint.batch-seq
     AND ttParamValue.prgmName EQ ttUserPrint.prgmName.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE browseParamValue */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseUserPrint
/* Query rebuild information for BROWSE browseUserPrint
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttUserPrint.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE browseUserPrint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME browseUserPrint
&Scoped-define SELF-NAME browseUserPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseUserPrint sObject
ON DEFAULT-ACTION OF browseUserPrint IN FRAME F-Main /* Tasks */
DO:
    APPLY "CHOOSE":U TO btnApply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseUserPrint sObject
ON VALUE-CHANGED OF browseUserPrint IN FRAME F-Main /* Tasks */
DO:
    {&OPEN-QUERY-browseParamValue}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd sObject
ON CHOOSE OF btnAdd IN FRAME F-Main /* Add */
DO:
    RUN pAddTask.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnApply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnApply sObject
ON CHOOSE OF btnApply IN FRAME F-Main /* Apply */
DO:
    IF AVAILABLE ttUserPrint THEN
    RUN pApplyTask.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete sObject
ON CHOOSE OF btnDelete IN FRAME F-Main /* Delete */
DO:
    IF AVAILABLE ttUserPrint THEN
    RUN pDeleteTask.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave sObject
ON CHOOSE OF btnSave IN FRAME F-Main /* Save */
DO:
    IF AVAILABLE ttUserPrint THEN
    RUN pSaveTask.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseParamValue
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize sObject 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
      ENABLE {&batchObjects}.
  END. /* with frame */
  {methods/run_link.i "CONTAINER" "pGetContainerSize" "(OUTPUT iHeightPixels, OUTPUT iWidthPixels)"}
  RUN pSetWinSize.
  {methods/run_link.i "CONTAINER" "pGetCompany" "(OUTPUT aoaCompany)"}
  {methods/run_link.i "CONTAINER" "pGetProgramID" "(OUTPUT aoaProgramID)"}
  {methods/run_link.i "CONTAINER" "pGetUserID" "(OUTPUT aoaUserID)"}
  {methods/run_link.i "CONTAINER" "pGetTitle" "(OUTPUT aoaTitle)"}
  {methods/run_link.i "CONTAINER" "pGetlColumns" "(OUTPUT aoaColumns)"}
  RUN pGetUserPrintTask.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddTask sObject 
PROCEDURE pAddTask :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTitle  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTaskID AS INTEGER   NO-UNDO.
    DEFINE VARIABLE rRowID  AS ROWID     NO-UNDO EXTENT 2.
    
    FOR EACH user-print NO-LOCK
        WHERE user-print.company EQ aoaCompany
           BY user-print.batch-seq DESCENDING
        :
        iTaskID = user-print.batch-seq.
        LEAVE.
    END. /* each user-print */
    iTaskID = iTaskID + 1.

    {methods/run_link.i "CONTAINER" "pSaveUserPrint" "(YES)"}
    {methods/run_link.i "CONTAINER" "pSaveJasperUserPrint" "(YES)"}
    {methods/run_link.i "CONTAINER" "pGetUserPrintRowID" "(OUTPUT rRowID[1])"}
    {methods/run_link.i "CONTAINER" "pGetJasperUserPrintRowID" "(OUTPUT rRowID[2])"}

    cTitle = aoaTitle.
    UPDATE cTitle FORMAT "x(30)" LABEL "Task Title"
        WITH FRAME fTitle CENTERED ROW 10 SIDE-LABELS OVERLAY.
    HIDE FRAME fTitle.
    
    DO TRANSACTION:
        FIND FIRST user-print EXCLUSIVE-LOCK
             WHERE ROWID(user-print) EQ rRowID[1].
        ASSIGN
            user-print.batch-seq  = iTaskID
            user-print.prog-title = cTitle
            user-print.frequency  = ""
            .
    END. /* do transaction */
    IF AVAILABLE user-print THEN
    FIND CURRENT user-print NO-LOCK.
    
    DO TRANSACTION:
        FIND FIRST jasperUserPrint EXCLUSIVE-LOCK
             WHERE ROWID(jasperUserPrint) EQ rRowID[2].
        ASSIGN
            jasperUserPrint.batch-seq  = user-print.batch-seq
            jasperUserPrint.prog-title = user-print.prog-title
            jasperUserPrint.frequency  = user-print.frequency
            .
    END. /* do transaction */
    IF AVAILABLE jasperUserPrint THEN
    FIND CURRENT jasperUserPrint NO-LOCK.

    RUN pGetUserPrintTask.

    MESSAGE
        "Parameters created for ..." SKIP(1)
        "Company:" aoaCompany "- Task ID:" iTaskID
        VIEW-AS ALERT-BOX TITLE "Add Task Record".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pApplyTask sObject 
PROCEDURE pApplyTask :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx    AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx    AS INTEGER NO-UNDO.
    DEFINE VARIABLE rRowID AS ROWID   NO-UNDO EXTENT 2.
    
    IF ttUserPrint.batch-seq NE 0 THEN DO:
        {methods/run_link.i "CONTAINER" "pGetUserPrintRowID" "(OUTPUT rRowID[1])"}
        {methods/run_link.i "CONTAINER" "pGetJasperUserPrintRowID" "(OUTPUT rRowID[2])"}
        DO jdx = 1 TO 2:
            FIND FIRST bUserPrint NO-LOCK
                 WHERE ROWID(bUserPrint) EQ ttUserPrint.userPrintRowID[jdx].
            DO TRANSACTION:
                FIND FIRST user-print EXCLUSIVE-LOCK
                     WHERE ROWID(user-print) EQ rRowID[jdx].
                ASSIGN
                    user-print.field-name  = ""
                    user-print.field-label = ""
                    user-print.field-value = ""
                    .
                DO idx = 1 TO EXTENT(bUserPrint.field-name):
                    IF bUserPrint.field-name[idx] EQ "" THEN LEAVE.
                    ASSIGN
                        user-print.field-name[idx]  = bUserPrint.field-name[idx]
                        user-print.field-label[idx] = bUserPrint.field-label[idx]
                        user-print.field-value[idx] = bUserPrint.field-value[idx]
                        .
                END. /* do idx */
                FIND CURRENT user-print NO-LOCK.
            END. /* do trans */
        END. /* do jdx */
        {methods/run_link.i "CONTAINER" "pGetUserPrint" "(?)"}
        {methods/run_link.i "CONTAINER" "pRefreshColumnsPage"}
    END. /* if batch-seq ne 0 */
    {methods/run_link.i "CONTAINER" "pSelectPage" "(1)"}
    RUN pGetUserPrintTask.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteTask sObject 
PROCEDURE pDeleteTask :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF ttUserPrint.batch-seq NE 0 THEN DO:
        MESSAGE "Delete Task" ttUserPrint.batch-seq "?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            TITLE "Delete Task Record"
            UPDATE deleteTask AS LOGICAL
            .
        IF deleteTask THEN DO TRANSACTION:
            FOR EACH user-print EXCLUSIVE-LOCK
                WHERE user-print.program-id EQ ttUserPrint.program-id
                  AND user-print.user-id    EQ ttUserPrint.user-id
                  AND user-print.batch-seq  EQ ttUserPrint.batch-seq
                  AND user-print.batch      EQ ttUserPrint.batch
                :
                DELETE user-print.
            END. /* each user-print */
        END. /* if delete */
        IF deleteTask THEN
        RUN pGetUserPrintTask.
    END. /* if batch-seq ne 0 */
    ELSE
    MESSAGE 
        "User Default Task cannot be Deleted!"
    VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUserPrintTask sObject 
PROCEDURE pGetUserPrintTask :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttUserPrint.
    EMPTY TEMP-TABLE ttParamValue.

    FOR EACH user-print NO-LOCK
        WHERE user-print.company    EQ aoaCompany
          AND user-print.program-id EQ aoaProgramID
          AND user-print.user-id    EQ aoaUserID
          /*
          AND user-print.batch      EQ "Batch"
          AND user-print.batch-seq  GT 0
          */
          AND user-print.prgmName   EQ "",
        FIRST jasperUserPrint NO-LOCK
        WHERE jasperUserPrint.company    EQ user-print.company
          AND jasperUserPrint.program-id EQ user-print.program-id
          AND jasperUserPrint.user-id    EQ user-print.user-id
          AND jasperUserPrint.batch      EQ user-print.batch
          AND jasperUserPrint.batch-seq  EQ user-print.batch-seq
          AND jasperUserPrint.prgmName   EQ "Jasper"
        :
        CREATE ttUserPrint.
        BUFFER-COPY user-print TO ttUserPrint.
        ttUserPrint.userPrintRowID[1] = ROWID(user-print).
        ttUserPrint.userPrintRowID[2] = ROWID(jasperUserPrint).
        DO idx = 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            CREATE ttParamValue.
            ASSIGN
                ttParamValue.paramOrder = idx
                ttParamValue.batch-seq  = user-print.batch-seq
                ttParamValue.prgmName   = user-print.prgmName
                ttParamValue.paramLabel = IF user-print.field-label[idx] NE ? THEN user-print.field-label[idx]
                                          ELSE "[ " + user-print.field-name[idx] + " ]"
                ttParamValue.paramValue = user-print.field-value[idx]
                ttParamValue.paramName  = user-print.field-name[idx]
                .
        END. /* do idx */
    END. /* each user-print */

    {&OPEN-QUERY-browseUserPrint}
    
    APPLY "VALUE-CHANGED":U TO BROWSE browseUserPrint.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveTask sObject 
PROCEDURE pSaveTask :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx    AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx    AS INTEGER NO-UNDO.
    DEFINE VARIABLE rRowID AS ROWID   NO-UNDO EXTENT 2.

    {methods/run_link.i "CONTAINER" "pSaveUserPrint" "(NO)"}
    {methods/run_link.i "CONTAINER" "pSaveJasperUserPrint" "(NO)"}
    {methods/run_link.i "CONTAINER" "pGetUserPrintRowID" "(OUTPUT rRowID[1])"}
    {methods/run_link.i "CONTAINER" "pGetJasperUserPrintRowID" "(OUTPUT rRowID[2])"}
    DO jdx = 1 TO 2:
        FIND FIRST bUserPrint NO-LOCK
             WHERE ROWID(bUserPrint) EQ rRowID[jdx].
        DO TRANSACTION:
            FIND FIRST user-print EXCLUSIVE-LOCK
                 WHERE ROWID(user-print) EQ ttUserPrint.userPrintRowID[jdx].
            ASSIGN
                user-print.field-name  = ""
                user-print.field-label = ""
                user-print.field-value = ""
                .
            DO idx = 1 TO EXTENT(bUserPrint.field-name):
                IF bUserPrint.field-name[idx] EQ "" THEN LEAVE.
                FIND FIRST ttParamValue
                     WHERE ttParamValue.paramOrder EQ idx
                       AND ttParamValue.batch-seq  EQ user-print.batch-seq.
                ASSIGN
                    ttParamValue.paramValue     = bUserPrint.field-value[idx]
                    user-print.field-name[idx]  = bUserPrint.field-name[idx]
                    user-print.field-label[idx] = bUserPrint.field-label[idx]
                    user-print.field-value[idx] = bUserPrint.field-value[idx]
                    .
            END. /* do idx */
        END. /* do trans */
        FIND CURRENT user-print NO-LOCK.
    END. /* do jdx */
    BROWSE browseParamValue:REFRESH() NO-ERROR.
    MESSAGE "Task ID" ttUserPrint.batch-seq "Saved"
        VIEW-AS ALERT-BOX
        TITLE "Task Record Saved"
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetWinSize sObject 
PROCEDURE pSetWinSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF iHeightPixels LT 347 THEN
    iHeightPixels = 347.
    IF iWidthPixels  LT 746 THEN
    iWidthPixels  = 746.
    ASSIGN
        FRAME {&FRAME-NAME}:HEIGHT-PIXELS     = iHeightPixels
        FRAME {&FRAME-NAME}:WIDTH-PIXELS      = iWidthPixels
        RECT-1:Y                              = iHeightPixels
                                              - RECT-1:HEIGHT-PIXELS - 4
        btnAdd:Y                              = RECT-1:Y + 5
        btnSave:Y                             = RECT-1:Y + 5
        btnDelete:Y                           = RECT-1:Y + 5
        btnApply:Y                            = RECT-1:Y + 5
        BROWSE browseParamValue:HEIGHT-PIXELS = iHeightPixels - 2
        BROWSE browseParamValue:X             = iWidthPixels
                                              - BROWSE browseParamValue:WIDTH-PIXELS - 2
        BROWSE browseUserPrint:WIDTH-PIXELS   = BROWSE browseParamValue:X - 2
        BROWSE browseUserPrint:HEIGHT-PIXELS  = iHeightPixels - 37
        .
    IF BROWSE browseUserPrint:WIDTH-PIXELS GT 407 THEN
    ASSIGN
        BROWSE browseUserPrint:WIDTH-PIXELS = 407
        BROWSE browseParamValue:X = BROWSE browseUserPrint:WIDTH-PIXELS + 2
        .
    ASSIGN
        RECT-1:X    = BROWSE browseUserPrint:WIDTH-PIXELS - 108
        btnAdd:X    = RECT-1:X + 5
        btnSave:X   = RECT-1:X + 30
        btnDelete:X = RECT-1:X + 55
        btnApply:X  = RECT-1:X + 80
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

