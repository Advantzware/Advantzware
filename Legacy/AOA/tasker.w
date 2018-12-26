&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
          audit            PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: tasker.w

  Description: Tasks Monitor

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 12.15.2018

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

&Scoped-define program-id tasker

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cRun            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTasker         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTaskLog        AS CHARACTER NO-UNDO.
DEFINE VARIABLE dttDateTime     AS DATETIME  NO-UNDO.
DEFINE VARIABLE dttOpenDateTime AS DATETIME  NO-UNDO.
DEFINE VARIABLE lFound          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lJasperStarter  AS LOGICAL   NO-UNDO.

lJasperStarter = INDEX(OS-GETENV("Path"),"jasperstarter") NE 0.
IF lJasperStarter EQ NO THEN DO:
    MESSAGE 
      "Jasper Starter is NOT installed, please contact" SKIP
      "your System Administrator for assistance."
    VIEW-AS ALERT-BOX WARNING.
    RETURN.
END. /* if jasperstarter */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME AuditBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES AuditHdr taskEmail Task

/* Definitions for BROWSE AuditBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-AuditBrowse AuditHdr.AuditDateTime ~
AuditHdr.AuditTable fTAskLog(AuditHdr.AuditID) @ cTaskLog 
&Scoped-define ENABLED-FIELDS-IN-QUERY-AuditBrowse 
&Scoped-define QUERY-STRING-AuditBrowse FOR EACH AuditHdr ~
      WHERE AuditHdr.AuditDB EQ "ASI" AND ~
AuditHdr.AuditType EQ "TASK" AND ~
AuditHdr.AuditDateTime GE dttOpenDateTime NO-LOCK ~
    BY AuditHdr.AuditDateTime DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-AuditBrowse OPEN QUERY AuditBrowse FOR EACH AuditHdr ~
      WHERE AuditHdr.AuditDB EQ "ASI" AND ~
AuditHdr.AuditType EQ "TASK" AND ~
AuditHdr.AuditDateTime GE dttOpenDateTime NO-LOCK ~
    BY AuditHdr.AuditDateTime DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-AuditBrowse AuditHdr
&Scoped-define FIRST-TABLE-IN-QUERY-AuditBrowse AuditHdr


/* Definitions for BROWSE EmailBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-EmailBrowse taskEmail.subject ~
taskEmail.recipients 
&Scoped-define ENABLED-FIELDS-IN-QUERY-EmailBrowse 
&Scoped-define QUERY-STRING-EmailBrowse FOR EACH taskEmail NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-EmailBrowse OPEN QUERY EmailBrowse FOR EACH taskEmail NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-EmailBrowse taskEmail
&Scoped-define FIRST-TABLE-IN-QUERY-EmailBrowse taskEmail


/* Definitions for BROWSE TaskBrowse                                    */
&Scoped-define FIELDS-IN-QUERY-TaskBrowse Task.runNow Task.taskName ~
Task.nextDate Task.cNextTime Task.lastDate Task.cLastTime Task.isRunning ~
Task.taskID Task.programID Task.user-id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-TaskBrowse 
&Scoped-define QUERY-STRING-TaskBrowse FOR EACH Task ~
      WHERE Task.scheduled EQ YES OR Task.runNow EQ YES NO-LOCK ~
    BY Task.runNow DESCENDING ~
       BY Task.nextDate ~
        BY Task.nextTime INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-TaskBrowse OPEN QUERY TaskBrowse FOR EACH Task ~
      WHERE Task.scheduled EQ YES OR Task.runNow EQ YES NO-LOCK ~
    BY Task.runNow DESCENDING ~
       BY Task.nextDate ~
        BY Task.nextTime INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-TaskBrowse Task
&Scoped-define FIRST-TABLE-IN-QUERY-TaskBrowse Task


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-AuditBrowse}~
    ~{&OPEN-QUERY-EmailBrowse}~
    ~{&OPEN-QUERY-TaskBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TaskBrowse EmailBrowse AuditBrowse 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fTaskLog C-Win 
FUNCTION fTaskLog RETURNS CHARACTER
  (ipiAuditID AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY AuditBrowse FOR 
      AuditHdr SCROLLING.

DEFINE QUERY EmailBrowse FOR 
      taskEmail SCROLLING.

DEFINE QUERY TaskBrowse FOR 
      Task SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE AuditBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS AuditBrowse C-Win _STRUCTURED
  QUERY AuditBrowse NO-LOCK DISPLAY
      AuditHdr.AuditDateTime COLUMN-LABEL "Date Time" FORMAT "99/99/9999 HH:MM:SS.SSS":U
      AuditHdr.AuditTable COLUMN-LABEL "Type" FORMAT "x(16)":U
      fTAskLog(AuditHdr.AuditID) @ cTaskLog COLUMN-LABEL "Values" FORMAT "x(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 14.29
         TITLE "Task Logging" ROW-HEIGHT-CHARS .76.

DEFINE BROWSE EmailBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS EmailBrowse C-Win _STRUCTURED
  QUERY EmailBrowse NO-LOCK DISPLAY
      taskEmail.subject FORMAT "x(40)":U
      taskEmail.recipients FORMAT "x(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 39 BY 14.29
         TITLE "Pending Emails" ROW-HEIGHT-CHARS .76.

DEFINE BROWSE TaskBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS TaskBrowse C-Win _STRUCTURED
  QUERY TaskBrowse NO-LOCK DISPLAY
      Task.runNow FORMAT "yes/no":U VIEW-AS TOGGLE-BOX
      Task.taskName FORMAT "x(24)":U
      Task.nextDate FORMAT "99/99/9999":U
      Task.cNextTime FORMAT "99:99":U
      Task.lastDate FORMAT "99/99/9999":U
      Task.cLastTime FORMAT "99:99":U
      Task.isRunning FORMAT "yes/no":U VIEW-AS TOGGLE-BOX
      Task.taskID FORMAT "->,>>>,>>9":U
      Task.programID FORMAT "x(20)":U
      Task.user-id FORMAT "x(10)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 121 BY 14.29
         TITLE "Scheduled Tasks" ROW-HEIGHT-CHARS .76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     TaskBrowse AT ROW 1 COL 1 WIDGET-ID 200
     EmailBrowse AT ROW 1 COL 122 WIDGET-ID 300
     AuditBrowse AT ROW 15.29 COL 1 WIDGET-ID 400
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "AOA Tasker"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB TaskBrowse 1 DEFAULT-FRAME */
/* BROWSE-TAB EmailBrowse TaskBrowse DEFAULT-FRAME */
/* BROWSE-TAB AuditBrowse EmailBrowse DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE AuditBrowse
/* Query rebuild information for BROWSE AuditBrowse
     _TblList          = "Audit.AuditHdr"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Audit.AuditHdr.AuditDateTime|no"
     _Where[1]         = "AuditHdr.AuditDB EQ ""ASI"" AND
AuditHdr.AuditType EQ ""TASK"" AND
AuditHdr.AuditDateTime GE dttOpenDateTime"
     _FldNameList[1]   > Audit.AuditHdr.AuditDateTime
"AuditHdr.AuditDateTime" "Date Time" ? "datetime" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Audit.AuditHdr.AuditTable
"AuditHdr.AuditTable" "Type" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"fTAskLog(AuditHdr.AuditID) @ cTaskLog" "Values" "x(256)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE AuditBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE EmailBrowse
/* Query rebuild information for BROWSE EmailBrowse
     _TblList          = "ASI.taskEmail"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = ASI.taskEmail.subject
     _FldNameList[2]   = ASI.taskEmail.recipients
     _Query            is OPENED
*/  /* BROWSE EmailBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE TaskBrowse
/* Query rebuild information for BROWSE TaskBrowse
     _TblList          = "ASI.Task"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "ASI.Task.runNow|no,ASI.Task.nextDate|yes,ASI.Task.nextTime|yes"
     _Where[1]         = "Task.scheduled EQ YES OR Task.runNow EQ YES"
     _FldNameList[1]   > ASI.Task.runNow
"Task.runNow" ? ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[2]   = ASI.Task.taskName
     _FldNameList[3]   = ASI.Task.nextDate
     _FldNameList[4]   = ASI.Task.cNextTime
     _FldNameList[5]   = ASI.Task.lastDate
     _FldNameList[6]   = ASI.Task.cLastTime
     _FldNameList[7]   > ASI.Task.isRunning
"Task.isRunning" ? ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[8]   = ASI.Task.taskID
     _FldNameList[9]   = ASI.Task.programID
     _FldNameList[10]   = ASI.Task.user-id
     _Query            is OPENED
*/  /* BROWSE TaskBrowse */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1
       COLUMN          = 1
       HEIGHT          = 4.76
       WIDTH           = 20
       WIDGET-ID       = 2
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(TaskBrowse:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* AOA Tasker */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* AOA Tasker */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN pSaveSettings.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* AOA Tasker */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
    {&WINDOW-NAME}:TITLE = "AOA Tasker - Scanning Tasks".
    RUN pTasks.
    {&WINDOW-NAME}:TITLE = "AOA Tasker - Scanning Emails".
    RUN pTaskEmails.
    {&WINDOW-NAME}:TITLE = "AOA Tasker - Idle".
    {&OPEN-QUERY-AuditBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME AuditBrowse
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN pSetTaskerNK1.
  RUN pRunCommand (OUTPUT cRun).
  dttOpenDateTime = NOW.
  RUN pGetSettings.
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "tasker.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "tasker.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  RUN control_load.
  ENABLE TaskBrowse EmailBrowse AuditBrowse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings C-Win 
PROCEDURE pGetSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    
    FIND FIRST user-print NO-LOCK
         WHERE user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF AVAILABLE user-print THEN DO:
        DO idx = 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            CASE user-print.field-name[idx]:
                WHEN "WindowColumn" THEN
                {&WINDOW-NAME}:COLUMN = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowRow" THEN
                {&WINDOW-NAME}:ROW = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowWidth" THEN
                ASSIGN
                    {&WINDOW-NAME}:WIDTH = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH = {&WINDOW-NAME}:WIDTH
                    .
                WHEN "WindowHeight" THEN
                ASSIGN
                    {&WINDOW-NAME}:HEIGHT = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
                    .
            END CASE.
        END. /* do idx */
    END. /* if avail */
    RUN pWinReSize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunCommand C-Win 
PROCEDURE pRunCommand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcRun AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cDLC   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParam AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lSkip  AS LOGICAL   NO-UNDO.
    
    GET-KEY-VALUE SECTION 'STARTUP'
        KEY 'DLC'
        VALUE cDLC.
    DO idx = 1 TO NUM-ENTRIES(SESSION:STARTUP-PARAMETERS):
        cParam = ENTRY(idx,SESSION:STARTUP-PARAMETERS).
        IF cParam BEGINS "-p " THEN NEXT.
        IF cParam BEGINS "-debugalert" THEN NEXT.
        IF lSkip EQ NO THEN
        opcRun = opcRun + cParam + " ".
        IF cParam BEGINS "-pf" THEN lSkip = YES.
        IF cParam BEGINS "(end .pf)" THEN lSkip = NO.
    END. /* do idx */
    opcRun = cDLC + "\bin\prowin " + opcRun
           + "-p &1 -param &2"
           .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveSettings C-Win 
PROCEDURE pSaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.program-id = "{&program-id}"
            user-print.user-id    = USERID("ASI")
            .
    END. /* not avail */
    ASSIGN
        user-print.field-name  = ""
        user-print.field-value = ""
        user-print.field-label = ""
        .
    ASSIGN
        idx = idx + 1
        user-print.field-name[idx]  = "WindowColumn"
        user-print.field-label[idx] = "WindowColumn"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:COLUMN)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowRow"
        user-print.field-label[idx] = "WindowRow"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:ROW)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowWidth"
        user-print.field-label[idx] = "WindowWidth"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:WIDTH)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowHeight"
        user-print.field-label[idx] = "WindowHeight"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:HEIGHT)
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetTaskerNK1 C-Win 
PROCEDURE pSetTaskerNK1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Task NO-LOCK
        BREAK BY Task.company
        :
        IF FIRST-OF(Task.company) THEN DO:
            RUN sys/ref/nk1look.p (
                Task.company,"Tasker","L",NO,NO,"","",
                OUTPUT cTasker,OUTPUT lFound
                ).
            IF lJasperStarter AND cTasker EQ "no" THEN DO TRANSACTION:
                FIND FIRST sys-ctrl EXCLUSIVE-LOCK
                     WHERE sys-ctrl.company EQ Task.company
                       AND sys-ctrl.name    EQ "Tasker"
                     NO-ERROR.
                IF AVAILABLE sys-ctrl THEN
                sys-ctrl.log-fld = YES.
            END. /* if jasper and tasker no */
        END. /* if first-of */
    END. /* each task */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTaskEmails C-Win 
PROCEDURE pTaskEmails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dttDateTime AS DATETIME NO-UNDO.
    DEFINE VARIABLE iDay        AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iMonth      AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iTime       AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iYear       AS INTEGER  NO-UNDO.
    DEFINE VARIABLE lRefresh    AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE rRowID      AS ROWID    NO-UNDO.
    
    DEFINE BUFFER bTaskEmail   FOR TaskEmail.
    DEFINE BUFFER bCueCardText FOR cueCardText.

    FOR EACH bTaskEmail:
        ASSIGN
            iYear       = INTEGER(SUBSTR(bTaskEmail.rec_key,1,4))
            iMonth      = INTEGER(SUBSTR(bTaskEmail.rec_key,5,2))
            iDay        = INTEGER(SUBSTR(bTaskEmail.rec_key,7,2))
            iTime       = INTEGER(SUBSTR(bTaskEmail.rec_key,9,5))
            dttDateTime = DATETIME(DATE(iMonth,iDay,iYear) + 1,iTime)
            .
        IF bTaskEmail.mustExist EQ NO OR
           SEARCH(bTaskEmail.attachment) NE ? THEN DO:
            IF bTaskEmail.recipients EQ "Cue Card Message" THEN DO:
                FIND LAST cueCardText NO-LOCK
                     WHERE cueCardText.cueID     EQ 0
                       AND cueCardText.cueTextID EQ 0
                       AND cueCardText.cueType   EQ "Message"
                     NO-ERROR.
                IF AVAILABLE cueCardText THEN DO:
                    CREATE bCueCardText.
                    BUFFER-COPY cueCardText EXCEPT rec_key TO bCueCardText
                        ASSIGN
                            bCueCardText.cueText     = "Submitted Run Now Request is Available"
                                                     + CHR(10) + CHR(10) + "File: "
                                                     + bTaskEmail.attachment
                            bCueCardText.isActive    = YES
                            bCueCardText.cueOrder    = cueCardText.cueOrder + 1
                            bCueCardText.createdDate = TODAY
                            bCueCardText.createdTime = TIME
                            bCueCardText.createdFor  = bTaskEmail.user-id
                            .
                END. /* if avail */
            END. /* if cue card message */
            ELSE
            OS-COMMAND NO-WAIT VALUE(
                SUBSTITUTE(
                    cRun,
                    "AOA\TaskEmail.p",    "~"" +
                    bTaskEmail.subject    + "+" +
                    bTaskEmail.body       + "+" +
                    bTaskEmail.attachment + "+" +
                    bTaskEmail.recipients + "+" +
                    bTaskEmail.rec_key    + "~""
                    )
                ).
            DELETE bTaskEmail.
            lRefresh = YES.
        END. /* if search */
        ELSE IF dttDateTime LT NOW THEN DO:
            DELETE bTaskEmail.
            lRefresh = YES.
        END. /* else if */
    END. /* each bTaskEmail */
    IF lRefresh THEN
    {&OPEN-QUERY-EmailBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTasks C-Win 
PROCEDURE pTasks :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE rRowID AS ROWID NO-UNDO.

    DEFINE BUFFER bTask FOR Task.

    {&OPEN-QUERY-TaskBrowse}
    GET FIRST TaskBrowse.
    DO WHILE AVAILABLE Task:
        ASSIGN
            dttDateTime = DATETIME(Task.nextDate, Task.nextTime * 1000)
            rRowID      = ROWID(Task)
            .
        IF Task.isRunning EQ NO AND
          (Task.runNow    EQ YES OR
         ((Task.startDate EQ ?   OR Task.startDate LE TODAY) AND
          (Task.endDate   EQ ?   OR Task.endDate   GE TODAY) AND
           dttDateTime    LE NOW)) THEN DO:
            REPOSITION TaskBrowse TO ROWID rRowID.
            DO TRANSACTION:
                FIND FIRST bTask EXCLUSIVE-LOCK
                     WHERE ROWID(bTask) EQ ROWID(Task).
                bTask.isRunning = YES.
                RELEASE bTask.
            END. /* do trans */
            OS-COMMAND NO-WAIT VALUE(
                    SUBSTITUTE(
                        cRun,
                        "AOA\runTask.p",
                        STRING(ROWID(Task))
                        )
                    ).
            PAUSE 2 NO-MESSAGE.
        END.
        GET NEXT TaskBrowse.
    END. /* do while */
    {&OPEN-QUERY-EmailBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize C-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    
    SESSION:SET-WAIT-STATE("General").
    DO WITH FRAME {&FRAME-NAME}:
        HIDE BROWSE TaskBrowse.
        HIDE BROWSE EmailBrowse.
        HIDE BROWSE AuditBrowse.
        HIDE FRAME {&FRAME-NAME}.
        IF {&WINDOW-NAME}:HEIGHT LT 28.57 THEN
        {&WINDOW-NAME}:HEIGHT = 28.57.
        IF {&WINDOW-NAME}:WIDTH  LT 160   THEN
        {&WINDOW-NAME}:WIDTH  = 160.
        ASSIGN
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH  = {&WINDOW-NAME}:WIDTH
            BROWSE AuditBrowse:ROW     = 1
            BROWSE TaskBrowse:HEIGHT   = FRAME {&FRAME-NAME}:HEIGHT / 2
            BROWSE EmailBrowse:HEIGHT  = BROWSE TaskBrowse:HEIGHT
            BROWSE AuditBrowse:HEIGHT  = FRAME {&FRAME-NAME}:HEIGHT
                                       - BROWSE TaskBrowse:HEIGHT - .001
            BROWSE EmailBrowse:WIDTH   = FRAME {&FRAME-NAME}:WIDTH
                                       - BROWSE EmailBrowse:COL + 1
            BROWSE AuditBrowse:WIDTH   = FRAME {&FRAME-NAME}:WIDTH - .001
            BROWSE AuditBrowse:ROW     = BROWSE TaskBrowse:HEIGHT + 1
            .
        VIEW FRAME {&FRAME-NAME}.
        VIEW BROWSE TaskBrowse.
        VIEW BROWSE EmailBrowse.
        VIEW BROWSE AuditBrowse.
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fTaskLog C-Win 
FUNCTION fTaskLog RETURNS CHARACTER
  (ipiAuditID AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cAuditLog AS CHARACTER NO-UNDO.
    
    FOR EACH AuditDtl NO-LOCK
        WHERE AuditDtl.AuditID EQ ipiAuditID
        :
        cAuditLog = cAuditLog + AuditDtl.AuditBeforeValue + " | ".
    END. /* each auditdtl */
    cAuditLog = TRIM(cAuditLog," | ").
    
    RETURN cAuditLog.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

