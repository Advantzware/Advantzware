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

  Created: 12.15.2018 (updated 2.9.2021)

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

&Scoped-define program-id tasker.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{AOA/includes/taskerDefs.i}

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
AuditHdr.AuditTable fTaskLog(AuditHdr.AuditID) @ cTaskLog 
&Scoped-define ENABLED-FIELDS-IN-QUERY-AuditBrowse 
&Scoped-define QUERY-STRING-AuditBrowse FOR EACH AuditHdr ~
      WHERE AuditHdr.AuditType EQ "TASK" AND ~
AuditHdr.AuditDateTime GE dttOpenDateTime NO-LOCK ~
    BY AuditHdr.AuditDateTime DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-AuditBrowse OPEN QUERY AuditBrowse FOR EACH AuditHdr ~
      WHERE AuditHdr.AuditType EQ "TASK" AND ~
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
Task.subjectID Task.paramValueID Task.taskID Task.user-id Task.runSync ~
Task.taskStart Task.expired 
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
&Scoped-Define ENABLED-OBJECTS showLogging TaskBrowse btnClearIsRunning ~
btnClearPendingEmails EmailBrowse AuditBrowse 
&Scoped-Define DISPLAYED-OBJECTS showLogging 

/* Custom List Definitions                                              */
/* taskObjects,List-2,List-3,List-4,List-5,List-6                       */
&Scoped-define taskObjects showLogging btnClearIsRunning ~
btnClearPendingEmails 

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
DEFINE BUTTON btnClearIsRunning 
     LABEL "Clear Running Flag" 
     SIZE 21 BY .91 TOOLTIP "Click to Clear Is Running".

DEFINE BUTTON btnClearPendingEmails 
     LABEL "Clear" 
     SIZE 8 BY .91 TOOLTIP "Click to Clear Pending Emails".

DEFINE VARIABLE showLogging AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Show Logging", yes,
"Hide Logging", no
     SIZE 34 BY .91 TOOLTIP "Show/Hide Logging Panel" NO-UNDO.

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
      fTaskLog(AuditHdr.AuditID) @ cTaskLog COLUMN-LABEL "Values" FORMAT "x(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 205 BY 14.29
         TITLE "Task Logging".

DEFINE BROWSE EmailBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS EmailBrowse C-Win _STRUCTURED
  QUERY EmailBrowse NO-LOCK DISPLAY
      taskEmail.subject FORMAT "x(40)":U
      taskEmail.recipients FORMAT "x(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 43 BY 14.29
         TITLE "Pending Emails".

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
      Task.subjectID FORMAT ">,>>>,>>9":U
      Task.paramValueID COLUMN-LABEL "Sched ID" FORMAT "->,>>>,>>9":U
      Task.taskID FORMAT "->,>>>,>>9":U
      Task.user-id FORMAT "x(10)":U
      Task.runSync FORMAT "yes/no":U
      Task.taskStart FORMAT "99/99/9999 HH:MM:SS.SSS":U
      Task.expired FORMAT "yes/no":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 162 BY 14.29
         TITLE "Tasks".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     showLogging AT ROW 1 COL 1 HELP
          "Show/Hide Logging" NO-LABEL WIDGET-ID 4
     TaskBrowse AT ROW 1 COL 1 WIDGET-ID 200
     btnClearIsRunning AT ROW 1 COL 98 HELP
          "Click to Clear Is Running" WIDGET-ID 8
     btnClearPendingEmails AT ROW 1 COL 163 HELP
          "Click to Clear Pending Emails" WIDGET-ID 10
     EmailBrowse AT ROW 1 COL 163 WIDGET-ID 300
     AuditBrowse AT ROW 15.29 COL 1 WIDGET-ID 400
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 205.2 BY 28.57
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
         TITLE              = "Task Monitor"
         HEIGHT             = 28.57
         WIDTH              = 205.2
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics/32x32/jss_icon_32.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics/32x32/jss_icon_32.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB TaskBrowse showLogging DEFAULT-FRAME */
/* BROWSE-TAB EmailBrowse btnClearPendingEmails DEFAULT-FRAME */
/* BROWSE-TAB AuditBrowse EmailBrowse DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnClearIsRunning IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnClearPendingEmails IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR RADIO-SET showLogging IN FRAME DEFAULT-FRAME
   1                                                                    */
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
     _Where[1]         = "AuditHdr.AuditType EQ ""TASK"" AND
AuditHdr.AuditDateTime GE dttOpenDateTime"
     _FldNameList[1]   > Audit.AuditHdr.AuditDateTime
"AuditHdr.AuditDateTime" "Date Time" ? "datetime" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Audit.AuditHdr.AuditTable
"AuditHdr.AuditTable" "Type" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"fTaskLog(AuditHdr.AuditID) @ cTaskLog" "Values" "x(256)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
     _FldNameList[8]   = ASI.Task.subjectID
     _FldNameList[9]   > ASI.Task.paramValueID
"Task.paramValueID" "Sched ID" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   = ASI.Task.taskID
     _FldNameList[11]   = ASI.Task.user-id
     _FldNameList[12]   = ASI.Task.runSync
     _FldNameList[13]   = ASI.Task.taskStart
     _FldNameList[14]   = ASI.Task.expired
     _Query            is OPENED
*/  /* BROWSE TaskBrowse */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.95
       COLUMN          = 1
       HEIGHT          = 4.76
       WIDTH           = 20
       WIDGET-ID       = 2
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(EmailBrowse:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Task Monitor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Task Monitor */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN pSaveSettings.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Task Monitor */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearIsRunning
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearIsRunning C-Win
ON CHOOSE OF btnClearIsRunning IN FRAME DEFAULT-FRAME /* Clear Running Flag */
DO:
    RUN pClearRunNow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearPendingEmails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearPendingEmails C-Win
ON CHOOSE OF btnClearPendingEmails IN FRAME DEFAULT-FRAME /* Clear */
DO:
    RUN pClearPendingEmails.
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
    DISABLE {&taskObjects} WITH FRAME {&FRAME-NAME}.
    {&WINDOW-NAME}:TITLE = "AOA Tasker - Scanning Tasks".
    RUN pTasks.
    {&WINDOW-NAME}:TITLE = "AOA Tasker - Scanning Emails".
    RUN pTaskEmails.
    IF showLogging THEN DO:
        {&WINDOW-NAME}:TITLE = "AOA Tasker - Refresh".
        {&OPEN-QUERY-AuditBrowse}
    END.
    {&WINDOW-NAME}:TITLE = "AOA Tasker - Update Last Executed".
    RUN pLastExecuted.
    ASSIGN
        {&WINDOW-NAME}:TITLE = "AOA Tasker - Idle"
        lUpdated             = NO
        .
    ENABLE {&taskObjects} WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME showLogging
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL showLogging C-Win
ON VALUE-CHANGED OF showLogging IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    RUN pWinReSize.
    {&OPEN-QUERY-AuditBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME AuditBrowse
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

&Scoped-define sdBrowseName TaskBrowse
{methods/template/brwcustom2.i 1}
&Scoped-define sdBrowseName EmailBrowse
{methods/template/brwcustom2.i 2}
&Scoped-define sdBrowseName AuditBrowse
{methods/template/brwcustom2.i 3}

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
  RUN pGetTaskTimeLimit.
  RUN pRunCommand (OUTPUT cRun).
  dttOpenDateTime = NOW.
  RUN enable_UI.
  RUN pGetSettings.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{AOA/includes/taskerProcs.i}

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
  DISPLAY showLogging 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE showLogging TaskBrowse btnClearIsRunning btnClearPendingEmails 
         EmailBrowse AuditBrowse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClearPendingEmails C-Win 
PROCEDURE pClearPendingEmails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bTaskEmail FOR TaskEmail.
    
    FOR EACH bTaskEmail:
        DELETE bTaskEmail.
    END. /* each btaskemail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClearRunNow C-Win 
PROCEDURE pClearRunNow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bTask FOR Task.
    
    FOR EACH bTask EXCLUSIVE-LOCK:
        ASSIGN
            bTask.isRunning = NO
            bTask.taskStart = ?
            bTask.expired   = NO
            .
    END. /* each btask */

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
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DEFINE BUFFER user-print FOR user-print.

    RUN pTrackAudit ("Started").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveSettings C-Win 
PROCEDURE pSaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iAuditID AS INTEGER NO-UNDO.
    DEFINE VARIABLE idx      AS INTEGER NO-UNDO.
    
    DEFINE BUFFER user-print FOR user-print.

    RUN pTrackAudit ("Stopped").

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
    FIND CURRENT user-print NO-LOCK.

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
        IF {&WINDOW-NAME}:WIDTH  LT 205.2 THEN
        {&WINDOW-NAME}:WIDTH  = 205.2.
        ASSIGN
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH  = {&WINDOW-NAME}:WIDTH
            BROWSE AuditBrowse:ROW     = 1
            .
        IF showLogging THEN
        ASSIGN
            BROWSE TaskBrowse:HEIGHT   = FRAME {&FRAME-NAME}:HEIGHT / 2
            BROWSE EmailBrowse:HEIGHT  = BROWSE TaskBrowse:HEIGHT
            BROWSE AuditBrowse:HEIGHT  = FRAME {&FRAME-NAME}:HEIGHT
                                       - BROWSE TaskBrowse:HEIGHT - .001
            BROWSE EmailBrowse:WIDTH   = FRAME {&FRAME-NAME}:WIDTH
                                       - BROWSE EmailBrowse:COL + 1
            BROWSE AuditBrowse:WIDTH   = FRAME {&FRAME-NAME}:WIDTH - .001
            BROWSE AuditBrowse:ROW     = BROWSE TaskBrowse:HEIGHT + 1
            .
        ELSE
        ASSIGN
            BROWSE TaskBrowse:HEIGHT   = FRAME {&FRAME-NAME}:HEIGHT
            BROWSE EmailBrowse:HEIGHT  = BROWSE TaskBrowse:HEIGHT
            BROWSE EmailBrowse:WIDTH   = FRAME {&FRAME-NAME}:WIDTH
                                       - BROWSE EmailBrowse:COL + 1
            .
        VIEW FRAME {&FRAME-NAME}.
        VIEW BROWSE TaskBrowse.
        VIEW BROWSE EmailBrowse.
        IF showLogging THEN
        VIEW BROWSE AuditBrowse.
        showLogging:MOVE-TO-TOP().
        btnClearIsRunning:MOVE-TO-TOP().
        btnClearPendingEmails:MOVE-TO-TOP().
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

