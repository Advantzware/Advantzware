&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
          audit            PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aoaSched.w

  Description: AOA Task Scheduler

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 10.31.2018 (Halloween)

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

&Scoped-define programID aoaTasks.
&Scoped-define dayOfWeek ~
Task.dayOfWeek1 ~
Task.dayOfWeek2 ~
Task.dayOfWeek3 ~
Task.dayOfWeek4 ~
Task.dayOfWeek5 ~
Task.dayOfWeek6 ~
Task.dayOfWeek7
&Scoped-define dayOfMonth Task.dayOfMonth Task.lastOfMonth
&Scoped-define prompts schedule/objects/prompts
&Scoped-define calendarObjects btnCalendar-1 btnCalendar-2 startDateOption endDateOption

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{methods/defines/sortByDefs.i}

DEFINE VARIABLE iUserPrintOffSet   AS INTEGER   NO-UNDO INITIAL 5.
DEFINE VARIABLE iUserSecurityLevel AS INTEGER   NO-UNDO INITIAL 9999.
DEFINE VARIABLE cMode              AS CHARACTER NO-UNDO.
DEFINE VARIABLE lContinue          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSortMove          AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE hContainer         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hAppSrvBin         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hJasper            AS HANDLE    NO-UNDO.
DEFINE VARIABLE cSuperProcedures   AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx                AS INTEGER   NO-UNDO.
DEFINE VARIABLE hHandle            AS HANDLE    NO-UNDO.
DEFINE VARIABLE cModule            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProgramID         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserID            AS CHARACTER NO-UNDO.

DEFINE BUFFER bUserPrint FOR user-print.

cSuperProcedures = SESSION:SUPER-PROCEDURE.
DO idx = 1 TO NUM-ENTRIES(cSuperProcedures):
    hHandle = WIDGET-HANDLE(ENTRY(idx,cSuperProcedures)).
    IF INDEX(hHandle:NAME,"aoaBin") NE 0 THEN
    hAppSrvBin = hHandle.
    IF INDEX(hHandle:NAME,"aoaJasper") NE 0 THEN
    hJasper = hHandle.
END. /* do idx */

IF NOT VALID-HANDLE(hAppSrvBin) THEN DO:
    RUN AOA\appServer\aoaBin.p PERSISTENT SET hAppSrvBin.
    SESSION:ADD-SUPER-PROCEDURE (hAppSrvBin).
END. /* if valid-handle */

IF NOT VALID-HANDLE(hJasper) THEN DO:
    RUN AOA\aoaJasper.p PERSISTENT SET hJasper.
    SESSION:ADD-SUPER-PROCEDURE (hJasper).
END. /* if valid-handle */

/* function fDateOptions */
{AOA/includes/fDateOptions.i}
/* function fDateOptionValue */
{AOA/includes/fDateOptionValue.i}

{methods/lockWindowUpdate.i}

SESSION:SET-WAIT-STATE("").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME auditBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES AuditHdr AuditDtl Task

/* Definitions for BROWSE auditBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-auditBrowse AuditHdr.AuditDateTime ~
AuditHdr.AuditTable AuditHdr.AuditUser AuditDtl.AuditField ~
AuditDtl.AuditBeforeValue AuditDtl.AuditAfterValue 
&Scoped-define ENABLED-FIELDS-IN-QUERY-auditBrowse 
&Scoped-define QUERY-STRING-auditBrowse FOR EACH AuditHdr ~
      WHERE AuditHdr.AuditKey EQ Task.rec_key ~
AND AuditHdr.AuditType EQ "Task" NO-LOCK, ~
      EACH AuditDtl OF AuditHdr NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-auditBrowse OPEN QUERY auditBrowse FOR EACH AuditHdr ~
      WHERE AuditHdr.AuditKey EQ Task.rec_key ~
AND AuditHdr.AuditType EQ "Task" NO-LOCK, ~
      EACH AuditDtl OF AuditHdr NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-auditBrowse AuditHdr AuditDtl
&Scoped-define FIRST-TABLE-IN-QUERY-auditBrowse AuditHdr
&Scoped-define SECOND-TABLE-IN-QUERY-auditBrowse AuditDtl


/* Definitions for BROWSE taskBrowse                                    */
&Scoped-define FIELDS-IN-QUERY-taskBrowse Task.scheduled Task.taskName fPrgmTitle(Task.programID) Task.frequency Task.cTaskTime Task.cFromTime Task.cToTime Task.dayOfWeek1 Task.dayOfWeek2 Task.dayOfWeek3 Task.dayOfWeek4 Task.dayOfWeek5 Task.dayOfWeek6 Task.dayOfWeek7 Task.lastOfMonth Task.taskFormat Task.nextDate Task.cNextTime Task.lastDate Task.cLastTime Task.startDate Task.endDate Task.taskID Task.module Task.programID Task.user-id Task.securityLevel Task.recipients   
&Scoped-define ENABLED-FIELDS-IN-QUERY-taskBrowse   
&Scoped-define SELF-NAME taskBrowse
&Scoped-define QUERY-STRING-taskBrowse FOR EACH Task WHERE Task.company EQ g_company   AND Task.module BEGINS cModule   AND Task.programID BEGINS cProgramID   AND Task.user-id BEGINS cUserID   AND Task.securityLevel LE iUserSecurityLevel   AND Task.allData MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-taskBrowse OPEN QUERY {&SELF-NAME} FOR EACH Task WHERE Task.company EQ g_company   AND Task.module BEGINS cModule   AND Task.programID BEGINS cProgramID   AND Task.user-id BEGINS cUserID   AND Task.securityLevel LE iUserSecurityLevel   AND Task.allData MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-taskBrowse Task
&Scoped-define FIRST-TABLE-IN-QUERY-taskBrowse Task


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-taskBrowse}

/* Definitions for FRAME viewFrame                                      */
&Scoped-define FIELDS-IN-QUERY-viewFrame Task.taskName Task.taskID ~
Task.user-id Task.securityLevel Task.scheduled Task.programID Task.module ~
Task.frequency Task.cTaskTime Task.cFromTime Task.cToTime Task.dayOfWeek1 ~
Task.dayOfWeek2 Task.dayOfWeek3 Task.dayOfWeek4 Task.dayOfWeek5 ~
Task.dayOfWeek6 Task.dayOfWeek7 Task.dayOfMonth[1] Task.dayOfMonth[2] ~
Task.dayOfMonth[3] Task.dayOfMonth[4] Task.dayOfMonth[5] Task.dayOfMonth[6] ~
Task.dayOfMonth[7] Task.dayOfMonth[8] Task.dayOfMonth[9] ~
Task.dayOfMonth[10] Task.dayOfMonth[11] Task.dayOfMonth[12] ~
Task.dayOfMonth[13] Task.dayOfMonth[14] Task.dayOfMonth[15] ~
Task.dayOfMonth[16] Task.dayOfMonth[17] Task.dayOfMonth[18] ~
Task.dayOfMonth[19] Task.dayOfMonth[20] Task.dayOfMonth[21] ~
Task.dayOfMonth[22] Task.dayOfMonth[23] Task.dayOfMonth[24] ~
Task.dayOfMonth[25] Task.dayOfMonth[26] Task.dayOfMonth[27] ~
Task.dayOfMonth[28] Task.runNow Task.dayOfMonth[29] Task.dayOfMonth[30] ~
Task.dayOfMonth[31] Task.lastOfMonth Task.startDate Task.endDate ~
Task.taskFormat Task.nextDate Task.cNextTime Task.lastDate Task.cLastTime ~
Task.recipients 
&Scoped-define QUERY-STRING-viewFrame FOR EACH Task SHARE-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-viewFrame OPEN QUERY viewFrame FOR EACH Task SHARE-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-viewFrame Task
&Scoped-define FIRST-TABLE-IN-QUERY-viewFrame Task


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnRun btnSortMove btnView showTasks ~
searchBar taskBrowse btnRestoreDefaults 
&Scoped-Define DISPLAYED-OBJECTS showTasks searchBar 

/* Custom List Definitions                                              */
/* transPanel,transInit,transUpdate,displayFields,enabledFields,timeRange */
&Scoped-define transPanel btnAddEmail btnRunNow btnClose btnFirst btnLast ~
btnNext btnPrev btnAdd btnCancel btnCopy btnDelete btnReset btnUpdate 
&Scoped-define transInit btnRunNow btnClose btnFirst btnLast btnNext ~
btnPrev btnAdd btnCopy btnDelete btnUpdate 
&Scoped-define transUpdate btnAddEmail btnCancel btnReset btnUpdate 
&Scoped-define displayFields Task.taskName Task.taskID Task.user-id ~
Task.securityLevel Task.scheduled Task.programID Task.module Task.frequency ~
Task.cTaskTime Task.cFromTime Task.cToTime Task.dayOfWeek1 Task.dayOfWeek2 ~
Task.dayOfWeek3 Task.dayOfWeek4 Task.dayOfWeek5 Task.dayOfWeek6 ~
Task.dayOfWeek7 Task.dayOfMonth[1] Task.dayOfMonth[2] Task.dayOfMonth[3] ~
Task.dayOfMonth[4] Task.dayOfMonth[5] Task.dayOfMonth[6] Task.dayOfMonth[7] ~
Task.dayOfMonth[8] Task.dayOfMonth[9] Task.dayOfMonth[10] ~
Task.dayOfMonth[11] Task.dayOfMonth[12] Task.dayOfMonth[13] ~
Task.dayOfMonth[14] Task.dayOfMonth[15] Task.dayOfMonth[16] ~
Task.dayOfMonth[17] Task.dayOfMonth[18] Task.dayOfMonth[19] ~
Task.dayOfMonth[20] Task.dayOfMonth[21] Task.dayOfMonth[22] ~
Task.dayOfMonth[23] Task.dayOfMonth[24] Task.dayOfMonth[25] ~
Task.dayOfMonth[26] Task.dayOfMonth[27] Task.dayOfMonth[28] Task.runNow ~
Task.dayOfMonth[29] Task.dayOfMonth[30] Task.dayOfMonth[31] ~
Task.lastOfMonth Task.startDate Task.endDate Task.taskFormat Task.nextDate ~
Task.cNextTime Task.lastDate Task.cLastTime Task.recipients 
&Scoped-define enabledFields Task.taskName Task.taskID Task.securityLevel ~
Task.scheduled Task.frequency Task.cTaskTime Task.cFromTime Task.cToTime ~
Task.dayOfWeek1 Task.dayOfWeek2 Task.dayOfWeek3 Task.dayOfWeek4 ~
Task.dayOfWeek5 Task.dayOfWeek6 Task.dayOfWeek7 Task.dayOfMonth[1] ~
Task.dayOfMonth[2] Task.dayOfMonth[3] Task.dayOfMonth[4] Task.dayOfMonth[5] ~
Task.dayOfMonth[6] Task.dayOfMonth[7] Task.dayOfMonth[8] Task.dayOfMonth[9] ~
Task.dayOfMonth[10] Task.dayOfMonth[11] Task.dayOfMonth[12] ~
Task.dayOfMonth[13] Task.dayOfMonth[14] Task.dayOfMonth[15] ~
Task.dayOfMonth[16] Task.dayOfMonth[17] Task.dayOfMonth[18] ~
Task.dayOfMonth[19] Task.dayOfMonth[20] Task.dayOfMonth[21] ~
Task.dayOfMonth[22] Task.dayOfMonth[23] Task.dayOfMonth[24] ~
Task.dayOfMonth[25] Task.dayOfMonth[26] Task.dayOfMonth[27] ~
Task.dayOfMonth[28] Task.dayOfMonth[29] Task.dayOfMonth[30] ~
Task.dayOfMonth[31] Task.lastOfMonth Task.startDate Task.endDate ~
Task.taskFormat Task.recipients 
&Scoped-define timeRange Task.cFromTime Task.cToTime 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAllData C-Win 
FUNCTION fAllData RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fConvertTime C-Win 
FUNCTION fConvertTime RETURNS INTEGER
  (ipcTime AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fPrgmTitle C-Win 
FUNCTION fPrgmTitle RETURNS CHARACTER
  (ipcProgramID AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fValidTime C-Win 
FUNCTION fValidTime RETURNS LOGICAL
  (iphTime AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fWindowTitle C-Win 
FUNCTION fWindowTitle RETURNS CHARACTER
  (ipcTitle     AS CHARACTER,
   ipcModule    AS CHARACTER,
   ipcProgramID AS CHARACTER,
   ipcUserID    AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnRestoreDefaults 
     IMAGE-UP FILE "Graphics/16x16/rename.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Defaults" 
     SIZE 4 BY .95 TOOLTIP "Restore Defaults".

DEFINE BUTTON btnRun 
     IMAGE-UP FILE "Graphics/16x16/media_play.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Run" 
     SIZE 4 BY .95 TOOLTIP "Run Now".

DEFINE BUTTON btnSortMove 
     IMAGE-UP FILE "Graphics/16x16/sort_up_down2.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Sort/Move" 
     SIZE 4 BY .95 TOOLTIP "Toggle Sort/Move Columns".

DEFINE BUTTON btnView 
     IMAGE-UP FILE "Graphics/16x16/window_dialog.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "View" 
     SIZE 4 BY .95 TOOLTIP "Viewer".

DEFINE VARIABLE searchBar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 99 BY 1 TOOLTIP "Search Bar" NO-UNDO.

DEFINE VARIABLE showTasks AS LOGICAL INITIAL yes 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Show Tasks", Yes,
"Show History", No
     SIZE 32 BY .91 NO-UNDO.

DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "Graphics/32x32/navigate_plus.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_plus_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Add" 
     SIZE 8 BY 1.91 TOOLTIP "Add".

DEFINE BUTTON btnAddEmail 
     IMAGE-UP FILE "AOA/images/navigate_plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Email" 
     SIZE 4.4 BY 1.05 TOOLTIP "Add Recipents".

DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCancel 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_cross_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel".

DEFINE BUTTON btnClose 
     IMAGE-UP FILE "Graphics/16x16/delete.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Close" 
     SIZE 4.2 BY 1 TOOLTIP "Close".

DEFINE BUTTON btnCopy 
     IMAGE-UP FILE "Graphics/32x32/element_copy.ico":U
     IMAGE-INSENSITIVE FILE "Graphics\32x32\form_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Copy" 
     SIZE 8 BY 1.91 TOOLTIP "Copy".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "Graphics/32x32/navigate_minus.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_minus_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 8 BY 1.91 TOOLTIP "Delete".

DEFINE BUTTON btnFirst 
     IMAGE-UP FILE "Graphics/32x32/navigate_beginning.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_beginning_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "First" 
     SIZE 8 BY 1.91 TOOLTIP "First".

DEFINE BUTTON btnLast 
     IMAGE-UP FILE "Graphics/32x32/navigate_end.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_end_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Last" 
     SIZE 8 BY 1.91 TOOLTIP "Last".

DEFINE BUTTON btnNext 
     IMAGE-UP FILE "Graphics/32x32/navigate_right.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_right_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Next" 
     SIZE 8 BY 1.91 TOOLTIP "Next".

DEFINE BUTTON btnPrev 
     IMAGE-UP FILE "Graphics/32x32/navigate_left.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_left_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Previous" 
     SIZE 8 BY 1.91 TOOLTIP "Previous".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/undo_32.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/undo_32_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE BUTTON btnRunNow 
     IMAGE-UP FILE "Graphics/32x32/media_play.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/media_play_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Run Now" 
     SIZE 8 BY 1.91 TOOLTIP "Run Now".

DEFINE BUTTON btnUpdate 
     IMAGE-UP FILE "Graphics/32x32/pencil.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/pencil_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Update" 
     SIZE 8 BY 1.91 TOOLTIP "Update/Save".

DEFINE VARIABLE endDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE startDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE cPrgmTitle AS CHARACTER FORMAT "X(256)":U 
     LABEL "Title" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE navPanel
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 34 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 48 BY 1.43.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 112 BY 1.19.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 71 BY 5.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 64 BY 1.43.

DEFINE RECTANGLE runNow-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 9.8 BY 2.38.

DEFINE RECTANGLE transPanel
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 50 BY 2.38
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY auditBrowse FOR 
      AuditHdr, 
      AuditDtl SCROLLING.

DEFINE QUERY taskBrowse FOR 
      Task SCROLLING.

DEFINE QUERY viewFrame FOR 
      Task SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE auditBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS auditBrowse C-Win _STRUCTURED
  QUERY auditBrowse NO-LOCK DISPLAY
      AuditHdr.AuditDateTime FORMAT "99/99/9999 HH:MM:SS.SSS":U
      AuditHdr.AuditTable FORMAT "x(16)":U WIDTH 12.2
      AuditHdr.AuditUser FORMAT "x(16)":U WIDTH 13.2
      AuditDtl.AuditField FORMAT "x(16)":U WIDTH 13.2
      AuditDtl.AuditBeforeValue FORMAT "x(70)":U WIDTH 71.2
      AuditDtl.AuditAfterValue FORMAT "x(16)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 4.2
         TITLE "History".

DEFINE BROWSE taskBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS taskBrowse C-Win _FREEFORM
  QUERY taskBrowse DISPLAY
      Task.scheduled LABEL-BGCOLOR 14 VIEW-AS TOGGLE-BOX
Task.taskName LABEL-BGCOLOR 14
fPrgmTitle(Task.programID) FORMAT "x(40)" LABEL "Title"
Task.frequency LABEL-BGCOLOR 14
Task.cTaskTime LABEL-BGCOLOR 14
Task.cFromTime LABEL-BGCOLOR 14
Task.cToTime LABEL-BGCOLOR 14
Task.dayOfWeek1 VIEW-AS TOGGLE-BOX
Task.dayOfWeek2 VIEW-AS TOGGLE-BOX
Task.dayOfWeek3 VIEW-AS TOGGLE-BOX
Task.dayOfWeek4 VIEW-AS TOGGLE-BOX
Task.dayOfWeek5 VIEW-AS TOGGLE-BOX
Task.dayOfWeek6 VIEW-AS TOGGLE-BOX
Task.dayOfWeek7 VIEW-AS TOGGLE-BOX
Task.lastOfMonth VIEW-AS TOGGLE-BOX
Task.taskFormat LABEL-BGCOLOR 14
Task.nextDate LABEL-BGCOLOR 14
Task.cNextTime LABEL-BGCOLOR 14
Task.lastDate LABEL-BGCOLOR 14
Task.cLastTime LABEL-BGCOLOR 14
Task.startDate LABEL-BGCOLOR 14
Task.endDate LABEL-BGCOLOR 14
Task.taskID LABEL-BGCOLOR 14
Task.module LABEL-BGCOLOR 14
Task.programID LABEL-BGCOLOR 14
Task.user-id LABEL-BGCOLOR 14
Task.securityLevel LABEL-BGCOLOR 14
Task.recipients
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 5.24
         TITLE "Tasks" ROW-HEIGHT-CHARS .76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnRun AT ROW 1 COL 11 HELP
          "Run Now" WIDGET-ID 44
     btnSortMove AT ROW 1 COL 6 HELP
          "Toggle Sort/Move Columns" WIDGET-ID 48
     btnView AT ROW 1 COL 16 HELP
          "Viewer" WIDGET-ID 46
     showTasks AT ROW 1 COL 21 NO-LABEL WIDGET-ID 52
     searchBar AT ROW 1 COL 60 COLON-ALIGNED HELP
          "Search" WIDGET-ID 6
     taskBrowse AT ROW 1.95 COL 1 WIDGET-ID 200
     auditBrowse AT ROW 25.29 COL 1 WIDGET-ID 500
     btnRestoreDefaults AT ROW 1 COL 1 HELP
          "Restore Defaults" WIDGET-ID 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME viewFrame
     btnAddEmail AT ROW 17.91 COL 8 HELP
          "Add Recipents" WIDGET-ID 636
     Task.taskName AT ROW 1.24 COL 12 COLON-ALIGNED WIDGET-ID 480
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
          BGCOLOR 15 
     Task.taskID AT ROW 1.24 COL 48 COLON-ALIGNED WIDGET-ID 504
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     Task.user-id AT ROW 1.24 COL 77 COLON-ALIGNED WIDGET-ID 516
          VIEW-AS FILL-IN 
          SIZE 16.2 BY 1
          BGCOLOR 15 
     Task.securityLevel AT ROW 1.24 COL 110 COLON-ALIGNED WIDGET-ID 630
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 
     Task.scheduled AT ROW 2.43 COL 14 WIDGET-ID 482
          VIEW-AS TOGGLE-BOX
          SIZE 13 BY 1
     Task.programID AT ROW 2.43 COL 48 COLON-ALIGNED WIDGET-ID 512
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 15 
     cPrgmTitle AT ROW 2.43 COL 77 COLON-ALIGNED WIDGET-ID 616
     Task.module AT ROW 2.43 COL 128 COLON-ALIGNED WIDGET-ID 648
          VIEW-AS FILL-IN 
          SIZE 10.2 BY 1
          BGCOLOR 15 
     Task.frequency AT ROW 3.62 COL 14 NO-LABEL WIDGET-ID 604
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Every ------", "Every":U,
"Daily ------", "Daily":U,
"Weekly -----", "Weekly":U,
"Monthly ----", "Monthly":U
          SIZE 13 BY 4.52
     Task.cTaskTime AT ROW 4.33 COL 32 COLON-ALIGNED WIDGET-ID 598
          LABEL "Time"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 
     Task.cFromTime AT ROW 4.33 COL 49 COLON-ALIGNED WIDGET-ID 626
          LABEL "From"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 
     Task.cToTime AT ROW 4.33 COL 63 COLON-ALIGNED WIDGET-ID 628
          LABEL "To"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 
     Task.dayOfWeek1 AT ROW 6 COL 28 WIDGET-ID 488
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY .81
     Task.dayOfWeek2 AT ROW 6 COL 44 WIDGET-ID 490
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY .81
     Task.dayOfWeek3 AT ROW 6 COL 60 WIDGET-ID 492
          VIEW-AS TOGGLE-BOX
          SIZE 12 BY .81
     Task.dayOfWeek4 AT ROW 6 COL 76 WIDGET-ID 494
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY .81
     Task.dayOfWeek5 AT ROW 6 COL 94 WIDGET-ID 496
          VIEW-AS TOGGLE-BOX
          SIZE 13.2 BY .81
     Task.dayOfWeek6 AT ROW 6 COL 110 WIDGET-ID 498
          VIEW-AS TOGGLE-BOX
          SIZE 10 BY .81
     Task.dayOfWeek7 AT ROW 6 COL 124 WIDGET-ID 500
          VIEW-AS TOGGLE-BOX
          SIZE 12 BY .81
     Task.dayOfMonth[1] AT ROW 7.43 COL 28 WIDGET-ID 524
          LABEL "1"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[2] AT ROW 7.43 COL 36 WIDGET-ID 526
          LABEL "2"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[3] AT ROW 7.43 COL 44 WIDGET-ID 528
          LABEL "3"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[4] AT ROW 7.43 COL 52 WIDGET-ID 530
          LABEL "4"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[5] AT ROW 7.43 COL 60 WIDGET-ID 532
          LABEL "5"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 21 ROW 7.43
         SIZE 140 BY 22.14
         FGCOLOR 1  WIDGET-ID 400.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME viewFrame
     Task.dayOfMonth[6] AT ROW 7.43 COL 68 WIDGET-ID 534
          LABEL "6"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[7] AT ROW 7.43 COL 76 WIDGET-ID 536
          LABEL "7"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[8] AT ROW 8.38 COL 28 WIDGET-ID 538
          LABEL "8"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[9] AT ROW 8.38 COL 36 WIDGET-ID 540
          LABEL "9"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[10] AT ROW 8.38 COL 44 WIDGET-ID 542
          LABEL "10"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[11] AT ROW 8.38 COL 52 WIDGET-ID 544
          LABEL "11"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[12] AT ROW 8.38 COL 60 WIDGET-ID 546
          LABEL "12"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[13] AT ROW 8.38 COL 68 WIDGET-ID 548
          LABEL "13"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[14] AT ROW 8.38 COL 76 WIDGET-ID 550
          LABEL "14"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[15] AT ROW 9.33 COL 28 WIDGET-ID 556
          LABEL "15"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[16] AT ROW 9.33 COL 36 WIDGET-ID 562
          LABEL "16"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[17] AT ROW 9.33 COL 44 WIDGET-ID 564
          LABEL "17"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[18] AT ROW 9.33 COL 52 WIDGET-ID 552
          LABEL "18"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[19] AT ROW 9.33 COL 60 WIDGET-ID 554
          LABEL "19"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[20] AT ROW 9.33 COL 68 WIDGET-ID 558
          LABEL "20"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[21] AT ROW 9.33 COL 76 WIDGET-ID 560
          LABEL "21"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[22] AT ROW 10.29 COL 28 WIDGET-ID 570
          LABEL "22"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[23] AT ROW 10.29 COL 36 WIDGET-ID 576
          LABEL "23"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[24] AT ROW 10.29 COL 44 WIDGET-ID 578
          LABEL "24"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[25] AT ROW 10.29 COL 52 WIDGET-ID 566
          LABEL "25"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[26] AT ROW 10.29 COL 60 WIDGET-ID 568
          LABEL "26"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[27] AT ROW 10.29 COL 68 WIDGET-ID 572
          LABEL "27"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[28] AT ROW 10.29 COL 76 WIDGET-ID 574
          LABEL "28"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 21 ROW 7.43
         SIZE 140 BY 22.14
         FGCOLOR 1  WIDGET-ID 400.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME viewFrame
     Task.runNow AT ROW 11.24 COL 14 WIDGET-ID 652
          VIEW-AS TOGGLE-BOX
          SIZE 12 BY .81
     Task.dayOfMonth[29] AT ROW 11.24 COL 28 WIDGET-ID 584
          LABEL "29"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[30] AT ROW 11.24 COL 36 WIDGET-ID 580
          LABEL "30"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.dayOfMonth[31] AT ROW 11.24 COL 44 WIDGET-ID 582
          LABEL "31"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     Task.lastOfMonth AT ROW 11.24 COL 76 WIDGET-ID 586
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .81
     Task.startDate AT ROW 12.43 COL 12 COLON-ALIGNED WIDGET-ID 514
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     btnCalendar-1 AT ROW 12.43 COL 30 WIDGET-ID 76
     startDateOption AT ROW 12.43 COL 33 COLON-ALIGNED HELP
          "Select Start Receipt Date Option" NO-LABEL WIDGET-ID 74
     Task.endDate AT ROW 13.62 COL 12 COLON-ALIGNED WIDGET-ID 506
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     btnCalendar-2 AT ROW 13.62 COL 30 WIDGET-ID 78
     endDateOption AT ROW 13.62 COL 33 COLON-ALIGNED HELP
          "Select End Receipt Date Option" NO-LABEL WIDGET-ID 70
     Task.taskFormat AT ROW 13.62 COL 81 NO-LABEL WIDGET-ID 608
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "CSV", "CSV":U,
"XLS", "XLS":U,
"DocX", "DOCX":U,
"PDF", "PDF":U,
"HTML", "HTML":U
          SIZE 51 BY 1
     Task.nextDate AT ROW 14.81 COL 12 COLON-ALIGNED WIDGET-ID 510
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     Task.cNextTime AT ROW 14.81 COL 36 COLON-ALIGNED WIDGET-ID 596
          LABEL "Time"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 
     Task.lastDate AT ROW 16 COL 12 COLON-ALIGNED WIDGET-ID 508
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     Task.cLastTime AT ROW 16 COL 36 COLON-ALIGNED WIDGET-ID 594
          LABEL "Time"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 
     Task.recipients AT ROW 17.19 COL 14 NO-LABEL WIDGET-ID 600
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 125 BY 2.14
          BGCOLOR 15 
     btnRunNow AT ROW 19.81 COL 80 HELP
          "Run Now" WIDGET-ID 634
     btnClose AT ROW 1 COL 136 HELP
          "Close" WIDGET-ID 72
     btnFirst AT ROW 19.81 COL 106 HELP
          "First" WIDGET-ID 274
     btnLast AT ROW 19.86 COL 130 HELP
          "Last" WIDGET-ID 68
     btnNext AT ROW 19.81 COL 122 HELP
          "Next" WIDGET-ID 276
     btnPrev AT ROW 19.81 COL 114 HELP
          "Previous" WIDGET-ID 278
     btnAdd AT ROW 19.86 COL 23 HELP
          "Add" WIDGET-ID 20
     btnCancel AT ROW 19.86 COL 55 HELP
          "Cancel" WIDGET-ID 28
     btnCopy AT ROW 19.86 COL 31 HELP
          "Copy" WIDGET-ID 24
     btnDelete AT ROW 19.86 COL 39 HELP
          "Delete" WIDGET-ID 26
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 21 ROW 7.43
         SIZE 140 BY 22.14
         FGCOLOR 1  WIDGET-ID 400.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME viewFrame
     btnReset AT ROW 19.86 COL 47 HELP
          "Reset" WIDGET-ID 22
     btnUpdate AT ROW 19.86 COL 15 HELP
          "Update/Save" WIDGET-ID 18
     "Frequency:" VIEW-AS TEXT
          SIZE 11 BY 1 AT ROW 3.62 COL 2 WIDGET-ID 618
     "Recipients:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 17.19 COL 3 WIDGET-ID 602
     "Format:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 13.62 COL 72 WIDGET-ID 614
     transPanel AT ROW 19.57 COL 14 WIDGET-ID 16
     navPanel AT ROW 19.57 COL 105 WIDGET-ID 280
     RECT-2 AT ROW 5.76 COL 27 WIDGET-ID 620
     RECT-3 AT ROW 7.19 COL 27 WIDGET-ID 622
     RECT-4 AT ROW 13.38 COL 69 WIDGET-ID 624
     runNow-2 AT ROW 19.57 COL 79 WIDGET-ID 632
     RECT-1 AT ROW 4.1 COL 27 WIDGET-ID 638
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 21 ROW 7.43
         SIZE 140 BY 22.14
         FGCOLOR 1 
         TITLE "View" WIDGET-ID 400.


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
         TITLE              = "AOA Tasks"
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
/* REPARENT FRAME */
ASSIGN FRAME viewFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME viewFrame:MOVE-AFTER-TAB-ITEM (taskBrowse:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME viewFrame:MOVE-BEFORE-TAB-ITEM (auditBrowse:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB taskBrowse searchBar DEFAULT-FRAME */
/* BROWSE-TAB auditBrowse viewFrame DEFAULT-FRAME */
/* SETTINGS FOR BROWSE auditBrowse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       auditBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

ASSIGN 
       taskBrowse:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 2
       taskBrowse:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       taskBrowse:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE
       taskBrowse:ROW-RESIZABLE IN FRAME DEFAULT-FRAME          = TRUE.

/* SETTINGS FOR FRAME viewFrame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME viewFrame:HIDDEN           = TRUE
       FRAME viewFrame:MOVABLE          = TRUE.

/* SETTINGS FOR BUTTON btnAdd IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnAddEmail IN FRAME viewFrame
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnCancel IN FRAME viewFrame
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON btnClose IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnCopy IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnDelete IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnFirst IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnLast IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnNext IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnPrev IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnReset IN FRAME viewFrame
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON btnRunNow IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnUpdate IN FRAME viewFrame
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN Task.cFromTime IN FRAME viewFrame
   NO-ENABLE 4 5 6 EXP-LABEL                                            */
/* SETTINGS FOR FILL-IN Task.cLastTime IN FRAME viewFrame
   NO-ENABLE 4 EXP-LABEL                                                */
ASSIGN 
       Task.cLastTime:READ-ONLY IN FRAME viewFrame        = TRUE.

/* SETTINGS FOR FILL-IN Task.cNextTime IN FRAME viewFrame
   NO-ENABLE 4 EXP-LABEL                                                */
ASSIGN 
       Task.cNextTime:READ-ONLY IN FRAME viewFrame        = TRUE.

/* SETTINGS FOR FILL-IN cPrgmTitle IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Task.cTaskTime IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN Task.cToTime IN FRAME viewFrame
   NO-ENABLE 4 5 6 EXP-LABEL                                            */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[10] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[11] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[12] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[13] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[14] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[15] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[16] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[17] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[18] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[19] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[1] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[20] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[21] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[22] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[23] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[24] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[25] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[26] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[27] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[28] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[29] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[2] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[30] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[31] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[3] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[4] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[5] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[6] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[7] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[8] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfMonth[9] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfWeek1 IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfWeek2 IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfWeek3 IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfWeek4 IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfWeek5 IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfWeek6 IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR TOGGLE-BOX Task.dayOfWeek7 IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN Task.endDate IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR COMBO-BOX endDateOption IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Task.frequency IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN Task.lastDate IN FRAME viewFrame
   NO-ENABLE 4                                                          */
ASSIGN 
       Task.lastDate:READ-ONLY IN FRAME viewFrame        = TRUE.

/* SETTINGS FOR TOGGLE-BOX Task.lastOfMonth IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN Task.module IN FRAME viewFrame
   NO-ENABLE 4                                                          */
/* SETTINGS FOR RECTANGLE navPanel IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Task.nextDate IN FRAME viewFrame
   NO-ENABLE 4                                                          */
ASSIGN 
       Task.nextDate:READ-ONLY IN FRAME viewFrame        = TRUE.

/* SETTINGS FOR FILL-IN Task.programID IN FRAME viewFrame
   NO-ENABLE 4                                                          */
ASSIGN 
       Task.programID:READ-ONLY IN FRAME viewFrame        = TRUE.

/* SETTINGS FOR EDITOR Task.recipients IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Task.runNow IN FRAME viewFrame
   NO-ENABLE 4                                                          */
/* SETTINGS FOR RECTANGLE runNow-2 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Task.scheduled IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN Task.securityLevel IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN Task.startDate IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR COMBO-BOX startDateOption IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Task.taskFormat IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN Task.taskID IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN Task.taskName IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR RECTANGLE transPanel IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Task.user-id IN FRAME viewFrame
   NO-ENABLE 4                                                          */
ASSIGN 
       Task.user-id:READ-ONLY IN FRAME viewFrame        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE auditBrowse
/* Query rebuild information for BROWSE auditBrowse
     _TblList          = "Audit.AuditHdr,Audit.AuditDtl OF Audit.AuditHdr"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "AuditHdr.AuditKey EQ Task.rec_key
AND AuditHdr.AuditType EQ ""Task"""
     _FldNameList[1]   = Audit.AuditHdr.AuditDateTime
     _FldNameList[2]   > Audit.AuditHdr.AuditTable
"AuditHdr.AuditTable" ? ? "character" ? ? ? ? ? ? no ? no no "12.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Audit.AuditHdr.AuditUser
"AuditHdr.AuditUser" ? ? "character" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Audit.AuditDtl.AuditField
"AuditDtl.AuditField" ? ? "character" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Audit.AuditDtl.AuditBeforeValue
"AuditDtl.AuditBeforeValue" ? "x(70)" "character" ? ? ? ? ? ? no ? no no "71.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = Audit.AuditDtl.AuditAfterValue
     _Query            is NOT OPENED
*/  /* BROWSE auditBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE taskBrowse
/* Query rebuild information for BROWSE taskBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Task
WHERE Task.company EQ g_company
  AND Task.module BEGINS cModule
  AND Task.programID BEGINS cProgramID
  AND Task.user-id BEGINS cUserID
  AND Task.securityLevel LE iUserSecurityLevel
  AND Task.allData MATCHES "*" + searchBar + "*"
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE taskBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME viewFrame
/* Query rebuild information for FRAME viewFrame
     _TblList          = "ASI.Task"
     _Options          = "SHARE-LOCK SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* FRAME viewFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* AOA Tasks */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* AOA Tasks */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN pSaveSettings (USERID("ASI")).
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* AOA Tasks */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME viewFrame /* Add */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddEmail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddEmail C-Win
ON CHOOSE OF btnAddEmail IN FRAME viewFrame /* Email */
DO:
    DEFINE VARIABLE cRecipients AS CHARACTER NO-UNDO.
    
    cRecipients = Task.recipients:SCREEN-VALUE.
    RUN AOA/aoaRecipients.w (INPUT-OUTPUT cRecipients).
    Task.recipients:SCREEN-VALUE = cRecipients.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 C-Win
ON CHOOSE OF btnCalendar-1 IN FRAME viewFrame
DO:
  {methods/btnCalendar.i Task.startDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 C-Win
ON CHOOSE OF btnCalendar-2 IN FRAME viewFrame
DO:
  {methods/btnCalendar.i Task.endDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME viewFrame /* Cancel */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME viewFrame /* Close */
DO:
    SELF:MOVE-TO-BOTTOM().
    HIDE FRAME viewFrame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy C-Win
ON CHOOSE OF btnCopy IN FRAME viewFrame /* Copy */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME viewFrame /* Delete */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst C-Win
ON CHOOSE OF btnFirst IN FRAME viewFrame /* First */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast C-Win
ON CHOOSE OF btnLast IN FRAME viewFrame /* Last */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext C-Win
ON CHOOSE OF btnNext IN FRAME viewFrame /* Next */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrev C-Win
ON CHOOSE OF btnPrev IN FRAME viewFrame /* Previous */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME viewFrame /* Reset */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnRestoreDefaults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestoreDefaults C-Win
ON CHOOSE OF btnRestoreDefaults IN FRAME DEFAULT-FRAME /* Defaults */
DO:
    RUN pGetSettings ("_default").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRun
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRun C-Win
ON CHOOSE OF btnRun IN FRAME DEFAULT-FRAME /* Run */
DO:
    RUN pRunNow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnRunNow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRunNow C-Win
ON CHOOSE OF btnRunNow IN FRAME viewFrame /* Run Now */
DO:
    RUN pRunNow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnSortMove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSortMove C-Win
ON CHOOSE OF btnSortMove IN FRAME DEFAULT-FRAME /* Sort/Move */
DO:
    ASSIGN
        BROWSE {&BROWSE-NAME}:COLUMN-MOVABLE = lSortMove
        lSortMove = NOT lSortMove
        .
    SELF:LOAD-IMAGE("Graphics/16x16/"
        + IF lSortMove THEN "sort_up_down2.gif"
          ELSE "left_right_arrows.gif")
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate C-Win
ON CHOOSE OF btnUpdate IN FRAME viewFrame /* Update */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView C-Win
ON CHOOSE OF btnView IN FRAME DEFAULT-FRAME /* View */
DO:
    IF FRAME viewFrame:HIDDEN THEN DO:
    VIEW FRAME viewFrame.
    RUN pDisplay.
    END. /* if hidden */
    ELSE
    APPLY "CHOOSE":U TO btnClose.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME Task.cFromTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Task.cFromTime C-Win
ON LEAVE OF Task.cFromTime IN FRAME viewFrame /* From */
DO:
    IF NOT fValidTime(SELF) THEN DO:
        APPLY "ENTRY":U TO SELF.
        RETURN NO-APPLY.
    END. /* if not validtime */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Task.cLastTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Task.cLastTime C-Win
ON ENTRY OF Task.cLastTime IN FRAME viewFrame /* Time */
DO:
    RUN pReadOnlyField (SELF).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Task.cNextTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Task.cNextTime C-Win
ON ENTRY OF Task.cNextTime IN FRAME viewFrame /* Time */
DO:
    RUN pReadOnlyField (SELF).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Task.cTaskTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Task.cTaskTime C-Win
ON LEAVE OF Task.cTaskTime IN FRAME viewFrame /* Time */
DO:
    IF NOT fValidTime(SELF) THEN DO:
        APPLY "ENTRY":U TO SELF.
        RETURN NO-APPLY.
    END. /* if not validtime */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Task.cToTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Task.cToTime C-Win
ON LEAVE OF Task.cToTime IN FRAME viewFrame /* To */
DO:
    IF NOT fValidTime(SELF) THEN DO:
        APPLY "ENTRY":U TO SELF.
        RETURN NO-APPLY.
    END. /* if not validtime */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Task.endDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Task.endDate C-Win
ON HELP OF Task.endDate IN FRAME viewFrame /* End Date */
DO:
    {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME endDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL endDateOption C-Win
ON VALUE-CHANGED OF endDateOption IN FRAME viewFrame
DO:
    {AOA/includes/tDateOption.i &dateObject=Task.endDate &btnCalendar=2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Task.frequency
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Task.frequency C-Win
ON VALUE-CHANGED OF Task.frequency IN FRAME viewFrame /* Freguency */
DO:
    DO WITH FRAME viewFrame:
        CASE SELF:SCREEN-VALUE:
            WHEN "Every" THEN DO:
                ENABLE {&timeRange}.
                DISABLE
                    {&dayOfWeek}
                    {&dayOfMonth}
                    .
            END.
            WHEN "Daily" THEN
            DISABLE
                {&timeRange}
                {&dayOfWeek}
                {&dayOfMonth}
                .
            WHEN "Weekly" THEN DO:
                ENABLE {&dayOfWeek}.
                DISABLE
                    {&timeRange}
                    {&dayOfMonth}
                    .
            END.
            WHEN "Monthly" THEN DO:
                DISABLE
                    {&timeRange}
                    {&dayOfWeek}
                    .
                ENABLE {&dayOfMonth}.
            END.
        END CASE.
    END. /* frame viewframe */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Task.lastDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Task.lastDate C-Win
ON ENTRY OF Task.lastDate IN FRAME viewFrame /* Last Date */
DO:
    RUN pReadOnlyField (SELF).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Task.nextDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Task.nextDate C-Win
ON ENTRY OF Task.nextDate IN FRAME viewFrame /* Next Date */
DO:
    RUN pReadOnlyField (SELF).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Task.programID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Task.programID C-Win
ON ENTRY OF Task.programID IN FRAME viewFrame /* Program ID */
DO:
    RUN pReadOnlyField (SELF).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME searchBar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchBar C-Win
ON VALUE-CHANGED OF searchBar IN FRAME DEFAULT-FRAME /* Search */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME showTasks
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL showTasks C-Win
ON VALUE-CHANGED OF showTasks IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
        {&SELF-NAME}
        btnRestoreDefaults:SENSITIVE = {&SELF-NAME}
        btnSortMove:SENSITIVE        = {&SELF-NAME}
        btnRun:SENSITIVE             = {&SELF-NAME}
        btnView:SENSITIVE            = {&SELF-NAME}
        BROWSE taskBrowse:SENSITIVE  = {&SELF-NAME}
        BROWSE auditBrowse:HIDDEN    = {&SELF-NAME}
        .
    IF BROWSE auditBrowse:HIDDEN EQ NO THEN DO:
        {&OPEN-QUERY-auditBrowse}
        auditBrowse:MOVE-TO-TOP().
    END. /* if not hidden */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME Task.startDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Task.startDate C-Win
ON HELP OF Task.startDate IN FRAME viewFrame /* Start Date */
DO:
    {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME startDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL startDateOption C-Win
ON VALUE-CHANGED OF startDateOption IN FRAME viewFrame
DO:
    {AOA/includes/tDateOption.i &dateObject=Task.startDate &btnCalendar=1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME taskBrowse
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME taskBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL taskBrowse C-Win
ON DEFAULT-ACTION OF taskBrowse IN FRAME DEFAULT-FRAME /* Tasks */
DO:
    APPLY "CHOOSE":U TO btnView.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL taskBrowse C-Win
ON START-SEARCH OF taskBrowse IN FRAME DEFAULT-FRAME /* Tasks */
DO:
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL taskBrowse C-Win
ON VALUE-CHANGED OF taskBrowse IN FRAME DEFAULT-FRAME /* Tasks */
DO:
    RUN pDisplay.
    IF BROWSE auditBrowse:HIDDEN EQ NO THEN
    {&OPEN-QUERY-auditBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME Task.taskID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Task.taskID C-Win
ON LEAVE OF Task.taskID IN FRAME viewFrame /* Task ID */
DO:
    IF NOT Task.runNow THEN
    DO WITH FRAME viewFrame:
        ASSIGN
            Task.user-id:SCREEN-VALUE   = ""
            Task.programID:SCREEN-VALUE = ""
            cPrgmTitle:SCREEN-VALUE     = ""
            Task.module:SCREEN-VALUE    = ""
            .
        FIND FIRST bUserPrint NO-LOCK
             WHERE bUserPrint.company   EQ g_company
               AND bUserPrint.batch-seq EQ INTEGER(SELF:SCREEN-VALUE)
               AND bUserPrint.batch     EQ "Batch"
               AND bUserPrint.prgmName  EQ "Jasper"
             NO-ERROR.
        IF AVAILABLE bUserPrint THEN DO:
            ASSIGN
                Task.user-id:SCREEN-VALUE   = bUserPrint.user-id
                Task.programID:SCREEN-VALUE = bUserPrint.program-id
                .
            FIND FIRST prgrms NO-LOCK
                 WHERE prgrms.prgmname EQ bUserPrint.program-id
                 NO-ERROR.
            IF AVAILABLE prgrms THEN
            ASSIGN
                cPrgmTitle:SCREEN-VALUE  = prgrms.prgTitle
                Task.module:SCREEN-VALUE = SUBSTR(prgrms.mnemonic,1,2)
                .
        END. /* if avail */
        ELSE DO:
            MESSAGE
                "Invalid Task ID..."
            VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END. /* else */
    END. /* with frame */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Task.user-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Task.user-id C-Win
ON ENTRY OF Task.user-id IN FRAME viewFrame /* User ID */
DO:
    RUN pReadOnlyField (SELF).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME auditBrowse
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

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
RUN util/CheckModule.p ("ASI","Jasper", YES, OUTPUT lContinue).
&ELSE
lContinue = YES.
&ENDIF

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF lContinue THEN DO:
      hContainer = THIS-PROCEDURE.
      RUN spGetTaskFilter (OUTPUT cModule, OUTPUT cProgramID, OUTPUT cUserID).
      {&WINDOW-NAME}:TITLE = fWindowTitle({&WINDOW-NAME}:TITLE, cModule, cProgramID, cUserID).
      FIND FIRST users NO-LOCK
           WHERE users.user_id EQ USERID("ASI")
           NO-ERROR.
      IF AVAILABLE users THEN
      iUserSecurityLevel = users.securityLevel.
      RUN enable_UI.
      DYNAMIC-FUNCTION('fDateOptions',startDateOption:HANDLE).
      DYNAMIC-FUNCTION('fDateOptions',endDateOption:HANDLE).
      RUN pGetSettings (USERID("ASI")).
  END. /* if continue */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{AOA/includes/pCalcNextRun.i}

{methods/sortByProc.i "pByEndDate" "Task.endDate"}
{methods/sortByProc.i "pByFrequency" "Task.frequency"}
{methods/sortByProc.i "pByFromTime" "Task.cFromTime"}
{methods/sortByProc.i "pByLastDate" "Task.lastDate"}
{methods/sortByProc.i "pByLastTime" "Task.cLastTime"}
{methods/sortByProc.i "pByModule" "Task.module"}
{methods/sortByProc.i "pByNextDate" "Task.nextDate"}
{methods/sortByProc.i "pByNextTime" "Task.cNextTime"}
{methods/sortByProc.i "pByProgramID" "Task.programID"}
{methods/sortByProc.i "pBySchedule" "Task.scheduled"}
{methods/sortByProc.i "pBySecurityLevel" "Task.securityLevel"}
{methods/sortByProc.i "pByStartDate" "Task.startDate"}
{methods/sortByProc.i "pByTaskFormat" "Task.taskFormat"}
{methods/sortByProc.i "pByTaskID" "Task.taskID"}
{methods/sortByProc.i "pByTaskName" "Task.taskName"}
{methods/sortByProc.i "pByTaskTime" "Task.cTaskTime"}
{methods/sortByProc.i "pByTotTime" "Task.cToTime"}
{methods/sortByProc.i "pByUserID" "Task.user-id"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY showTasks searchBar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnRun btnSortMove btnView showTasks searchBar taskBrowse 
         btnRestoreDefaults 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY cPrgmTitle startDateOption endDateOption 
      WITH FRAME viewFrame IN WINDOW C-Win.
  IF AVAILABLE Task THEN 
    DISPLAY Task.taskName Task.taskID Task.user-id Task.securityLevel 
          Task.scheduled Task.programID Task.module Task.frequency 
          Task.cTaskTime Task.cFromTime Task.cToTime Task.dayOfWeek1 
          Task.dayOfWeek2 Task.dayOfWeek3 Task.dayOfWeek4 Task.dayOfWeek5 
          Task.dayOfWeek6 Task.dayOfWeek7 Task.dayOfMonth[1] Task.dayOfMonth[2] 
          Task.dayOfMonth[3] Task.dayOfMonth[4] Task.dayOfMonth[5] 
          Task.dayOfMonth[6] Task.dayOfMonth[7] Task.dayOfMonth[8] 
          Task.dayOfMonth[9] Task.dayOfMonth[10] Task.dayOfMonth[11] 
          Task.dayOfMonth[12] Task.dayOfMonth[13] Task.dayOfMonth[14] 
          Task.dayOfMonth[15] Task.dayOfMonth[16] Task.dayOfMonth[17] 
          Task.dayOfMonth[18] Task.dayOfMonth[19] Task.dayOfMonth[20] 
          Task.dayOfMonth[21] Task.dayOfMonth[22] Task.dayOfMonth[23] 
          Task.dayOfMonth[24] Task.dayOfMonth[25] Task.dayOfMonth[26] 
          Task.dayOfMonth[27] Task.dayOfMonth[28] Task.runNow 
          Task.dayOfMonth[29] Task.dayOfMonth[30] Task.dayOfMonth[31] 
          Task.lastOfMonth Task.startDate Task.endDate Task.taskFormat 
          Task.nextDate Task.cNextTime Task.lastDate Task.cLastTime 
          Task.recipients 
      WITH FRAME viewFrame IN WINDOW C-Win.
  ENABLE btnRunNow btnClose btnFirst btnLast btnNext btnPrev btnAdd btnCopy 
         btnDelete btnUpdate 
      WITH FRAME viewFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-viewFrame}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssign C-Win 
PROCEDURE pAssign :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO TRANSACTION WITH FRAME viewFrame:
        FIND CURRENT Task EXCLUSIVE-LOCK.
        ASSIGN
            {&enabledFields}
            Task.user-id
            Task.programID
            Task.module
            Task.taskTime = fConvertTime(Task.cTaskTime)
            Task.fromTime = fConvertTime(Task.cFromTime)
            Task.toTime   = fConvertTime(Task.cToTime)
            Task.nextTime = fConvertTime(Task.cNextTime)
            Task.lastTime = fConvertTime(Task.cLastTime)
            Task.allData  = fAllData()
            .
        CASE Task.frequency:
            WHEN "Every" OR WHEN "Daily" THEN DO:
                ASSIGN
                    Task.dayOfWeek1  = NO
                    Task.dayOfWeek2  = NO
                    Task.dayOfWeek3  = NO
                    Task.dayOfWeek4  = NO
                    Task.dayOfWeek5  = NO
                    Task.dayOfWeek6  = NO
                    Task.dayOfWeek7  = NO
                    Task.dayOfWeek   = NO
                    Task.dayOfMonth  = NO
                    Task.lastOfMonth = NO
                    .
                IF Task.frequency EQ "Daily" THEN
                ASSIGN
                    Task.cFromTime = "0000"
                    Task.fromTime  = 0
                    Task.cToTime   = "0000"
                    Task.toTime    = 0
                    .
            END.
            WHEN "Weekly" THEN
            ASSIGN
                Task.cFromTime    = "0000"
                Task.fromTime     = 0
                Task.cToTime      = "0000"
                Task.toTime       = 0
                Task.dayOfWeek[1] = Task.dayOfWeek1
                Task.dayOfWeek[2] = Task.dayOfWeek2
                Task.dayOfWeek[3] = Task.dayOfWeek3
                Task.dayOfWeek[4] = Task.dayOfWeek4
                Task.dayOfWeek[5] = Task.dayOfWeek5
                Task.dayOfWeek[6] = Task.dayOfWeek6
                Task.dayOfWeek[7] = Task.dayOfWeek7
                Task.dayOfMonth   = NO
                Task.lastOfMonth  = NO
                .
            WHEN "Monthly" THEN
            ASSIGN
                Task.cFromTime  = "0000"
                Task.fromTime   = 0
                Task.cToTime    = "0000"
                Task.toTime     = 0
                Task.dayOfWeek1 = NO
                Task.dayOfWeek2 = NO
                Task.dayOfWeek3 = NO
                Task.dayOfWeek4 = NO
                Task.dayOfWeek5 = NO
                Task.dayOfWeek6 = NO
                Task.dayOfWeek7 = NO
                Task.dayOfWeek  = NO
                .
        END CASE.
        FIND CURRENT Task NO-LOCK.
    END. /* with frame */
    RUN pCalcNextRun (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClearView C-Win 
PROCEDURE pClearView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    
    DO WITH FRAME viewFrame:
        ASSIGN
            hWidget = FRAME viewFrame:HANDLE
            hWidget = hWidget:FIRST-CHILD
            hWidget = hWidget:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hWidget):
            IF hWidget:TYPE NE "BUTTON" AND
               hWidget:SELECTABLE EQ NO AND 
               hWidget:SENSITIVE THEN
            hWidget:SCREEN-VALUE = if hWidget:TYPE EQ "TOGGLE-BOX" THEN "NO" ELSE "".
            hWidget = hWidget:NEXT-SIBLING.
        END. /* do while */
        ASSIGN
            Task.securityLevel:SCREEN-VALUE = STRING(iUserSecurityLevel)
            Task.module:SCREEN-VALUE        = ""
            Task.user-id:SCREEN-VALUE       = ""
            Task.programID:SCREEN-VALUE     = ""
            cPrgmTitle:SCREEN-VALUE         = ""
            Task.nextDate:SCREEN-VALUE      = ""
            Task.lastDate:SCREEN-VALUE      = ""
            Task.cNextTime:SCREEN-VALUE     = "0000"
            Task.cLastTime:SCREEN-VALUE     = "0000"
            Task.cTaskTime:SCREEN-VALUE     = "0000"
            Task.cFromTime:SCREEN-VALUE     = "0000"
            Task.cToTime:SCREEN-VALUE       = "0000"
            .
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCRUD C-Win 
PROCEDURE pCRUD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphMode AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.
    DEFINE VARIABLE rRowID    AS ROWID   NO-UNDO.

    DO WITH FRAME viewFrame:
        CASE iphMode:LABEL:
            WHEN "Add" OR WHEN "Copy" OR WHEN "Update" THEN DO:
                DISABLE {&transPanel}.
                BROWSE {&BROWSE-NAME}:SENSITIVE = NO.
                ENABLE {&transUpdate} {&enabledFields} {&calendarObjects}.
                btnUpdate:LOAD-IMAGE("Graphics\32x32\Save_As.ico").
                IF iphMode:LABEL EQ "Add" THEN DO:
                    RUN pClearView.
                    DISABLE btnReset.
                END. /* add */
                ASSIGN
                    FRAME viewFrame:TITLE = iphMode:LABEL
                    btnUpdate:LABEL       = "Save"
                    .
                IF INTEGER(Task.taskID:SCREEN-VALUE) NE 0 THEN
                APPLY "LEAVE":U TO Task.taskID.
                APPLY "VALUE-CHANGED":U TO Task.frequency.
            END. /* add copy update */
            WHEN "Cancel" OR WHEN "Save" THEN DO:
                IF iphMode:LABEL EQ "Save" THEN DO:
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO TRANSACTION:
                        CREATE Task.
                        ASSIGN
                            Task.company = g_company
                            rRowID       = ROWID(Task)
                            .
                    END. /* if add/copy */
                    RUN pAssign.
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO:
                        RUN pReopenBrowse.
                        REPOSITION {&BROWSE-NAME} TO ROWID rRowID.
                    END. /* if add/copy */
                    ELSE
                    BROWSE {&BROWSE-NAME}:REFRESH().
                END. /* save */
                DISABLE {&transPanel} {&enabledFields} {&calendarObjects}.
                ENABLE {&transInit}.
                BROWSE {&BROWSE-NAME}:SENSITIVE = YES.
                btnUpdate:LOAD-IMAGE("Graphics\32x32\Pencil.ico").
                ASSIGN
                    FRAME viewFrame:TITLE           = "View"
                    btnUpdate:LABEL                 = "Update"
                    BROWSE {&BROWSE-NAME}:SENSITIVE = YES
                    .
                APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.
            END. /* cancel save */
            WHEN "Delete" THEN DO:
                IF AVAILABLE Task THEN DO:
                    MESSAGE
                        "Delete Currently Selected Record?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    UPDATE lContinue.
                    IF lContinue THEN DO TRANSACTION:
                        cMode = iphMode:LABEL.
                        FIND CURRENT Task EXCLUSIVE-LOCK.
                        DELETE Task.
                        BROWSE {&BROWSE-NAME}:DELETE-CURRENT-ROW().
                    END. /* if lcontinue */
                    IF AVAILABLE Task THEN
                    BROWSE {&BROWSE-NAME}:REFRESH().
                    RUN pDisplay.
                END. /* if avail */
            END. /* delete */
            WHEN "Reset" THEN DO:
                RUN pDisplay.
                DISABLE {&transPanel}.
                ENABLE {&transUpdate}.
                APPLY "VALUE-CHANGED":U TO Task.frequency.
            END. /* reset */
        END CASE. /* ipcmode:label */
        IF Task.taskName:SENSITIVE THEN
        APPLY "ENTRY":U TO Task.taskName.
        ELSE
        APPLY "ENTRY":U TO BROWSE {&BROWSE-NAME}.
        /* save the mode for when logic returns to this procedure */
        cMode = iphMode:LABEL.
    END. /* do frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplay C-Win 
PROCEDURE pDisplay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF AVAILABLE Task THEN
    DO WITH FRAME viewFrame:
        DISPLAY {&displayFields}.
        ENABLE {&transInit}.
        APPLY "LEAVE":U TO Task.taskID.
    END. /* if avail */
    ELSE DO WITH FRAME viewFrame:
        RUN pClearView.
        DISABLE {&transPanel}.
        ENABLE btnAdd btnClose.
    END. /* else */

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
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE kdx     AS INTEGER NO-UNDO.
    
    IF NOT CAN-FIND(FIRST user-print
                    WHERE user-print.company    EQ g_company
                      AND user-print.program-id EQ "{&programID}"
                      AND user-print.user-id    EQ "_default") THEN
    RUN pSaveSettings ("_default").
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "{&programID}"
           AND user-print.user-id    EQ ipcUserID
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
                WHEN "BrowseRowHeight" THEN
                BROWSE taskBrowse:ROW-HEIGHT = DECIMAL(user-print.field-value[idx]).
            END CASE.
        END. /* do idx */
        DO idx = iUserPrintOffSet + 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            /* set browse column width, hidden & order */
            DO kdx = 1 TO BROWSE taskBrowse:NUM-COLUMNS:
                IF user-print.field-name[idx] EQ BROWSE taskBrowse:GET-BROWSE-COLUMN(kdx):NAME THEN DO:
                    ASSIGN
                        jdx = idx - iUserPrintOffSet
                        hColumn = BROWSE taskBrowse:GET-BROWSE-COLUMN(jdx)
                        hColumn:WIDTH = DECIMAL(user-print.field-value[idx])
                        .
                    BROWSE taskBrowse:MOVE-COLUMN(kdx,jdx).
                END. /* if name */
            END. /* do kdx */
        END. /* do idx */
    END. /* if avail */
    RUN pWinReSize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNavPanel C-Win 
PROCEDURE pNavPanel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphNavPanel AS HANDLE NO-UNDO.
    
    CASE iphNavPanel:LABEL:
        WHEN "First" THEN
        APPLY "HOME":U TO BROWSE {&BROWSE-NAME}.
        WHEN "Previous" THEN
        BROWSE {&BROWSE-NAME}:SELECT-PREV-ROW().
        WHEN "Next" THEN
        BROWSE {&BROWSE-NAME}:SELECT-NEXT-ROW().
        WHEN "Last" THEN
        APPLY "END":U TO BROWSE {&BROWSE-NAME}.
    END CASE.
    IF AVAILABLE Task THEN
    APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReadOnlyField C-Win 
PROCEDURE pReadOnlyField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphField AS HANDLE NO-UNDO.
    
    IF KEYLABEL(LASTKEY) EQ "SHIFT-TAB" THEN
    APPLY "SHIFT-TAB":U TO iphField.
    ELSE
    APPLY "TAB":U TO iphField.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse C-Win 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CASE cColumnLabel:
        WHEN "endDate" THEN
        RUN pByEndDate.
        WHEN "frequency" THEN
        RUN pByFrequency.
        WHEN "cFromTime" THEN
        RUN pByFromTime.
        WHEN "lastDate" THEN
        RUN pByLastDate.
        WHEN "cLastTime" THEN
        RUN pByLastTime.
        WHEN "module" THEN
        RUN pByModule.
        WHEN "nextDate" THEN
        RUN pByNextDate.
        WHEN "cNextTime" THEN
        RUN pByNextTime.
        WHEN "programID" THEN
        RUN pByProgramID.
        WHEN "scheduled" THEN
        RUN pByScheduled.
        WHEN "securityLevel" THEN
        RUN pBySecurityLevel.
        WHEN "startDate" THEN
        RUN pByStartDate.
        WHEN "taskFormat" THEN
        RUN pByTaskFormat.
        WHEN "taskID" THEN
        RUN pByTaskID.
        WHEN "taskName" THEN
        RUN pByTaskName.
        WHEN "cTaskTime" THEN
        RUN pByTaskTime.
        WHEN "cToTime" THEN
        RUN pByTotTime.
        WHEN "user-id" THEN
        RUN pByUserID.
        OTHERWISE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunNow C-Win 
PROCEDURE pRunNow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF AVAILABLE Task THEN DO:
        DO TRANSACTION:
            FIND CURRENT Task EXCLUSIVE-LOCK.
            ASSIGN
                Task.runNow:SCREEN-VALUE IN FRAME viewFrame = "YES"
                Task.runNow
                .
            FIND CURRENT Task NO-LOCK.
        END. /* do trans */
        MESSAGE
            "Task ~"" + Task.taskName + "~" has been submitted."
        VIEW-AS ALERT-BOX TITLE "Run Now".
    END. /* if avail */

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
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx     AS INTEGER NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "{&programID}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company    = g_company
            user-print.program-id = "{&programID}"
            user-print.user-id    = ipcUserID
            user-print.last-date  = TODAY
            user-print.last-time  = TIME
            .
    END. /* not avail */
    ASSIGN
        user-print.next-date   = TODAY
        user-print.next-time   = TIME
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
        idx = idx + 1
        user-print.field-name[idx]  = "BrowseRowHeight"
        user-print.field-label[idx] = "BrowseRowHeight"
        user-print.field-value[idx] = STRING(BROWSE taskBrowse:ROW-HEIGHT)
        .
    /* save browse column order and width */
    DO jdx = 1 TO BROWSE taskBrowse:NUM-COLUMNS:
        ASSIGN
            idx = idx + 1
            hColumn = BROWSE taskBrowse:GET-BROWSE-COLUMN(jdx)
            user-print.field-label[idx] = "BrowseColumn"
            user-print.field-name[idx]  = hColumn:NAME
            user-print.field-value[idx] = STRING(MAX(hColumn:WIDTH, .2 /*BROWSE taskBrowse:MIN-COLUMN-WIDTH-CHARS*/ ))
            .
    END. /* do jdx */

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
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).

    DO WITH FRAME {&FRAME-NAME}:
        HIDE FRAME viewFrame.
        HIDE FRAME {&FRAME-NAME}.
        IF {&WINDOW-NAME}:HEIGHT LT 28.57 THEN
        {&WINDOW-NAME}:HEIGHT = 28.57.
        IF {&WINDOW-NAME}:WIDTH  LT 160   THEN
        {&WINDOW-NAME}:WIDTH  = 160.
        ASSIGN
            /* view frame */
            FRAME viewFrame:COL = 1
            FRAME viewFrame:ROW = 1
            /* default frame */
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH  = {&WINDOW-NAME}:WIDTH
            /* search bar */
            searchBar:WIDTH = FRAME {&FRAME-NAME}:WIDTH
                            - searchBar:COL + 1
            /* browse frame */
            BROWSE taskBrowse:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT
                                     - BROWSE taskBrowse:ROW + 1
            BROWSE taskBrowse:WIDTH  = FRAME {&FRAME-NAME}:WIDTH
                                     - BROWSE taskBrowse:COL + 1
            /* audit browse */
            BROWSE auditBrowse:COL    = BROWSE taskBrowse:COL
            BROWSE auditBrowse:ROW    = BROWSE taskBrowse:ROW
            BROWSE auditBrowse:HEIGHT = BROWSE taskBrowse:HEIGHT
            /* view frame */
            FRAME viewFrame:COL = FRAME {&FRAME-NAME}:WIDTH
                                - FRAME viewFrame:WIDTH  + 1
            FRAME viewFrame:ROW = FRAME {&FRAME-NAME}:HEIGHT
                                - FRAME viewFrame:HEIGHT + 1
            .
        VIEW FRAME {&FRAME-NAME}.
    END. /* do with */

    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAllData C-Win 
FUNCTION fAllData RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    RETURN
        Task.taskName + "|" +
        STRING(Task.taskID) + "|" +
        Task.user-id + "|" +
        Task.module + "|" +
        Task.programID + "|" +
        Task.frequency + "|" +
        Task.taskFormat + "|" +
        Task.recipients + "|" +
        fPrgmTitle(Task.programID) + "|" +
        Task.access + "|" +
        Task.lastUser
        .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fConvertTime C-Win 
FUNCTION fConvertTime RETURNS INTEGER
  (ipcTime AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iTime AS INTEGER NO-UNDO.
    
    ASSIGN
        ipcTime = REPLACE(ipcTime,":","")
        iTime   = INTEGER(SUBSTR(ipcTime,1,2)) * 3600
                + INTEGER(SUBSTR(ipcTime,3,2)) * 60
                .
    RETURN iTime.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fPrgmTitle C-Win 
FUNCTION fPrgmTitle RETURNS CHARACTER
  (ipcProgramID AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    FIND FIRST prgrms NO-LOCK
         WHERE prgrms.prgmname EQ ipcProgramID
         NO-ERROR.
    RETURN IF AVAILABLE prgrms THEN prgrms.prgtitle ELSE "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fValidTime C-Win 
FUNCTION fValidTime RETURNS LOGICAL
  (iphTime AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    IF SUBSTR(iphTime:SCREEN-VALUE,1,1) EQ "" THEN SUBSTR(iphTime:SCREEN-VALUE,1,1) = "0".
    IF SUBSTR(iphTime:SCREEN-VALUE,2,1) EQ "" THEN SUBSTR(iphTime:SCREEN-VALUE,2,1) = "0".
    IF SUBSTR(iphTime:SCREEN-VALUE,4,1) EQ "" THEN SUBSTR(iphTime:SCREEN-VALUE,4,1) = "0".
    IF SUBSTR(iphTime:SCREEN-VALUE,5,1) EQ "" THEN SUBSTR(iphTime:SCREEN-VALUE,5,1) = "0".
    IF fConvertTime(iphTime:SCREEN-VALUE) GT 86400 THEN DO:
        MESSAGE
            "Invalid Time Entry, Please Try Again."
        VIEW-AS ALERT-BOX ERROR.
        RETURN FALSE.
    END.
    ELSE
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fWindowTitle C-Win 
FUNCTION fWindowTitle RETURNS CHARACTER
  (ipcTitle     AS CHARACTER,
   ipcModule    AS CHARACTER,
   ipcProgramID AS CHARACTER,
   ipcUserID    AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTitle AS CHARACTER NO-UNDO.
    
    IF ipcModule    NE "" OR
       ipcProgramID NE "" OR
       ipcUserID    NE "" THEN DO:
        cTitle = " - Filter [ ".
        IF ipcModule    NE "" THEN
        cTitle = cTitle + "Module: "     + cModule      + " ".
        IF ipcProgramID NE "" THEN
        cTitle = cTitle + "Program ID: " + ipcProgramID + " ".
        IF ipcUserID    NE "" THEN
        cTitle = cTitle + "User ID: "    + ipcUserID    + " ".
        cTitle = cTitle + "]".
    END.
    cTitle = ipcTitle + cTitle.
    RETURN cTitle.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

