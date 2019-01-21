&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttTask NO-UNDO LIKE ttTask.



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

&Scoped-define program-id aoaSched.
&Scoped-define dayOfWeek ~
ttTask.dayOfWeek1 ~
ttTask.dayOfWeek2 ~
ttTask.dayOfWeek3 ~
ttTask.dayOfWeek4 ~
ttTask.dayOfWeek5 ~
ttTask.dayOfWeek6 ~
ttTask.dayOfWeek7
&Scoped-define dayOfMonth ttTask.dayOfMonth ttTask.lastOfMonth
&Scoped-define prompts schedule/objects/prompts
&Scoped-define calendarObjects btnCalendar-1 btnCalendar-2 startDateOption endDateOption

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/prgsecur.i}
{methods/defines/sortByDefs.i}

DEFINE VARIABLE iUserPrintOffSet AS INTEGER   NO-UNDO INITIAL 5.
DEFINE VARIABLE cMode            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lContinue        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hContainer       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hAppSrvBin       AS HANDLE    NO-UNDO.

RUN AOA\appServer\aoaBin.p PERSISTENT SET hAppSrvBin.
SESSION:ADD-SUPER-PROCEDURE (hAppSrvBin).

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
&Scoped-define BROWSE-NAME taskBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttTask

/* Definitions for BROWSE taskBrowse                                    */
&Scoped-define FIELDS-IN-QUERY-taskBrowse ttTask.scheduled ttTask.taskName fPrgmTitle(ttTask.program-id) ttTask.frequency ttTask.cTaskTime ttTask.cFromTime ttTask.cToTime ttTask.dayOfWeek1 ttTask.dayOfWeek2 ttTask.dayOfWeek3 ttTask.dayOfWeek4 ttTask.dayOfWeek5 ttTask.dayOfWeek6 ttTask.dayOfWeek7 ttTask.lastOfMonth ttTask.taskFormat ttTask.nextDate ttTask.cNextTime ttTask.lastDate ttTask.cLastTime ttTask.startDate ttTask.endDate ttTask.taskID ttTask.program-id ttTask.user-id ttTask.securityLevel ttTask.recipients   
&Scoped-define ENABLED-FIELDS-IN-QUERY-taskBrowse   
&Scoped-define SELF-NAME taskBrowse
&Scoped-define QUERY-STRING-taskBrowse FOR EACH ttTask WHERE ttTask.company EQ g_company   AND ttTask.allData MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-taskBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttTask WHERE ttTask.company EQ g_company   AND ttTask.allData MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-taskBrowse ttTask
&Scoped-define FIRST-TABLE-IN-QUERY-taskBrowse ttTask


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-taskBrowse}

/* Definitions for FRAME viewFrame                                      */
&Scoped-define FIELDS-IN-QUERY-viewFrame ttTask.scheduled ttTask.taskName ~
ttTask.securityLevel ttTask.taskID ttTask.user-id ttTask.program-id ~
ttTask.frequency ttTask.cTaskTime ttTask.cFromTime ttTask.cToTime ~
ttTask.dayOfWeek1 ttTask.dayOfWeek2 ttTask.dayOfWeek3 ttTask.dayOfWeek4 ~
ttTask.dayOfWeek5 ttTask.dayOfWeek6 ttTask.dayOfWeek7 ttTask.dayOfMonth[1] ~
ttTask.dayOfMonth[2] ttTask.dayOfMonth[3] ttTask.dayOfMonth[4] ~
ttTask.dayOfMonth[5] ttTask.dayOfMonth[6] ttTask.dayOfMonth[7] ~
ttTask.dayOfMonth[8] ttTask.dayOfMonth[9] ttTask.dayOfMonth[10] ~
ttTask.dayOfMonth[11] ttTask.dayOfMonth[12] ttTask.dayOfMonth[13] ~
ttTask.dayOfMonth[14] ttTask.dayOfMonth[15] ttTask.dayOfMonth[16] ~
ttTask.dayOfMonth[17] ttTask.dayOfMonth[18] ttTask.dayOfMonth[19] ~
ttTask.dayOfMonth[20] ttTask.dayOfMonth[21] ttTask.dayOfMonth[22] ~
ttTask.dayOfMonth[23] ttTask.dayOfMonth[24] ttTask.dayOfMonth[25] ~
ttTask.dayOfMonth[26] ttTask.dayOfMonth[27] ttTask.dayOfMonth[28] ~
ttTask.dayOfMonth[29] ttTask.dayOfMonth[30] ttTask.dayOfMonth[31] ~
ttTask.lastOfMonth ttTask.startDate ttTask.endDate ttTask.taskFormat ~
ttTask.nextDate ttTask.cNextTime ttTask.lastDate ttTask.cLastTime ~
ttTask.recipients 
&Scoped-define QUERY-STRING-viewFrame FOR EACH ttTask SHARE-LOCK
&Scoped-define OPEN-QUERY-viewFrame OPEN QUERY viewFrame FOR EACH ttTask SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-viewFrame ttTask
&Scoped-define FIRST-TABLE-IN-QUERY-viewFrame ttTask


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnView searchBar taskBrowse btnRun ~
btnRestoreDefaults 
&Scoped-Define DISPLAYED-OBJECTS searchBar 

/* Custom List Definitions                                              */
/* transPanel,transInit,transUpdate,displayFields,enabledFields,timeRange */
&Scoped-define transPanel btnAddEmail btnRunNow btnClose btnFirst btnLast ~
btnNext btnPrev btnAdd btnCancel btnCopy btnDelete btnReset btnUpdate 
&Scoped-define transInit btnRunNow btnClose btnFirst btnLast btnNext ~
btnPrev btnAdd btnCopy btnDelete btnUpdate 
&Scoped-define transUpdate btnAddEmail btnCancel btnReset btnUpdate 
&Scoped-define displayFields ttTask.scheduled ttTask.taskName ~
ttTask.securityLevel ttTask.taskID ttTask.user-id ttTask.program-id ~
ttTask.frequency ttTask.cTaskTime ttTask.cFromTime ttTask.cToTime ~
ttTask.dayOfWeek1 ttTask.dayOfWeek2 ttTask.dayOfWeek3 ttTask.dayOfWeek4 ~
ttTask.dayOfWeek5 ttTask.dayOfWeek6 ttTask.dayOfWeek7 ttTask.dayOfMonth[1] ~
ttTask.dayOfMonth[2] ttTask.dayOfMonth[3] ttTask.dayOfMonth[4] ~
ttTask.dayOfMonth[5] ttTask.dayOfMonth[6] ttTask.dayOfMonth[7] ~
ttTask.dayOfMonth[8] ttTask.dayOfMonth[9] ttTask.dayOfMonth[10] ~
ttTask.dayOfMonth[11] ttTask.dayOfMonth[12] ttTask.dayOfMonth[13] ~
ttTask.dayOfMonth[14] ttTask.dayOfMonth[15] ttTask.dayOfMonth[16] ~
ttTask.dayOfMonth[17] ttTask.dayOfMonth[18] ttTask.dayOfMonth[19] ~
ttTask.dayOfMonth[20] ttTask.dayOfMonth[21] ttTask.dayOfMonth[22] ~
ttTask.dayOfMonth[23] ttTask.dayOfMonth[24] ttTask.dayOfMonth[25] ~
ttTask.dayOfMonth[26] ttTask.dayOfMonth[27] ttTask.dayOfMonth[28] ~
ttTask.dayOfMonth[29] ttTask.dayOfMonth[30] ttTask.dayOfMonth[31] ~
ttTask.lastOfMonth ttTask.startDate ttTask.endDate ttTask.taskFormat ~
ttTask.nextDate ttTask.cNextTime ttTask.lastDate ttTask.cLastTime ~
ttTask.recipients 
&Scoped-define enabledFields ttTask.scheduled ttTask.taskName ~
ttTask.securityLevel ttTask.taskID ttTask.user-id ttTask.program-id ~
ttTask.frequency ttTask.cTaskTime ttTask.cFromTime ttTask.cToTime ~
ttTask.dayOfWeek1 ttTask.dayOfWeek2 ttTask.dayOfWeek3 ttTask.dayOfWeek4 ~
ttTask.dayOfWeek5 ttTask.dayOfWeek6 ttTask.dayOfWeek7 ttTask.dayOfMonth[1] ~
ttTask.dayOfMonth[2] ttTask.dayOfMonth[3] ttTask.dayOfMonth[4] ~
ttTask.dayOfMonth[5] ttTask.dayOfMonth[6] ttTask.dayOfMonth[7] ~
ttTask.dayOfMonth[8] ttTask.dayOfMonth[9] ttTask.dayOfMonth[10] ~
ttTask.dayOfMonth[11] ttTask.dayOfMonth[12] ttTask.dayOfMonth[13] ~
ttTask.dayOfMonth[14] ttTask.dayOfMonth[15] ttTask.dayOfMonth[16] ~
ttTask.dayOfMonth[17] ttTask.dayOfMonth[18] ttTask.dayOfMonth[19] ~
ttTask.dayOfMonth[20] ttTask.dayOfMonth[21] ttTask.dayOfMonth[22] ~
ttTask.dayOfMonth[23] ttTask.dayOfMonth[24] ttTask.dayOfMonth[25] ~
ttTask.dayOfMonth[26] ttTask.dayOfMonth[27] ttTask.dayOfMonth[28] ~
ttTask.dayOfMonth[29] ttTask.dayOfMonth[30] ttTask.dayOfMonth[31] ~
ttTask.lastOfMonth ttTask.startDate ttTask.endDate ttTask.taskFormat ~
ttTask.recipients 
&Scoped-define timeRange ttTask.cFromTime ttTask.cToTime 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fTimeConvert C-Win 
FUNCTION fTimeConvert RETURNS CHARACTER
  (ipiTime AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fValidTime C-Win 
FUNCTION fValidTime RETURNS LOGICAL
  (iphTime AS HANDLE)  FORWARD.

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

DEFINE BUTTON btnView 
     IMAGE-UP FILE "Graphics/16x16/window_dialog.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "View" 
     SIZE 4 BY .95 TOOLTIP "Viewer".

DEFINE VARIABLE searchBar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 137 BY 1 TOOLTIP "Search Bar" NO-UNDO.

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
     SIZE 39 BY 1
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

DEFINE RECTANGLE runNow
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 9.8 BY 2.38.

DEFINE RECTANGLE transPanel
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 50 BY 2.38
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY taskBrowse FOR 
      ttTask SCROLLING.

DEFINE QUERY viewFrame FOR 
      ttTask SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE taskBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS taskBrowse C-Win _FREEFORM
  QUERY taskBrowse DISPLAY
      ttTask.scheduled LABEL-BGCOLOR 14 VIEW-AS TOGGLE-BOX
ttTask.taskName LABEL-BGCOLOR 14
fPrgmTitle(ttTask.program-id) FORMAT "x(40)" LABEL "Title"
ttTask.frequency LABEL-BGCOLOR 14
ttTask.cTaskTime LABEL-BGCOLOR 14
ttTask.cFromTime LABEL-BGCOLOR 14
ttTask.cToTime LABEL-BGCOLOR 14
ttTask.dayOfWeek1  VIEW-AS TOGGLE-BOX
ttTask.dayOfWeek2  VIEW-AS TOGGLE-BOX
ttTask.dayOfWeek3  VIEW-AS TOGGLE-BOX
ttTask.dayOfWeek4  VIEW-AS TOGGLE-BOX
ttTask.dayOfWeek5  VIEW-AS TOGGLE-BOX
ttTask.dayOfWeek6  VIEW-AS TOGGLE-BOX
ttTask.dayOfWeek7  VIEW-AS TOGGLE-BOX
ttTask.lastOfMonth VIEW-AS TOGGLE-BOX
ttTask.taskFormat LABEL-BGCOLOR 14
ttTask.nextDate LABEL-BGCOLOR 14
ttTask.cNextTime LABEL-BGCOLOR 14
ttTask.lastDate LABEL-BGCOLOR 14
ttTask.cLastTime LABEL-BGCOLOR 14
ttTask.startDate LABEL-BGCOLOR 14
ttTask.endDate LABEL-BGCOLOR 14
ttTask.taskID LABEL-BGCOLOR 14
ttTask.program-id LABEL-BGCOLOR 14
ttTask.user-id LABEL-BGCOLOR 14
ttTask.securityLevel
ttTask.recipients
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 5.48
         TITLE "Tasks".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnView AT ROW 1 COL 11 HELP
          "Viewer" WIDGET-ID 46
     searchBar AT ROW 1 COL 22 COLON-ALIGNED HELP
          "Search" WIDGET-ID 6
     taskBrowse AT ROW 1.95 COL 1 WIDGET-ID 200
     btnRun AT ROW 1 COL 6 HELP
          "Run Now" WIDGET-ID 44
     btnRestoreDefaults AT ROW 1 COL 1 HELP
          "Restore Defaults" WIDGET-ID 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME viewFrame
     btnAddEmail AT ROW 18.14 COL 8 HELP
          "Add Recipents" WIDGET-ID 636
     ttTask.scheduled AT ROW 1.24 COL 14 WIDGET-ID 482
          VIEW-AS TOGGLE-BOX
          SIZE 13 BY 1
     ttTask.taskName AT ROW 1.24 COL 38 COLON-ALIGNED WIDGET-ID 480
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
          BGCOLOR 15 
     ttTask.securityLevel AT ROW 1.24 COL 98 COLON-ALIGNED WIDGET-ID 630
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
          BGCOLOR 15 
     ttTask.taskID AT ROW 2.43 COL 12 COLON-ALIGNED WIDGET-ID 504
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     ttTask.user-id AT ROW 2.43 COL 38 COLON-ALIGNED WIDGET-ID 516
          VIEW-AS FILL-IN 
          SIZE 16.2 BY 1
          BGCOLOR 15 
     ttTask.program-id AT ROW 2.43 COL 69 COLON-ALIGNED WIDGET-ID 512
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 15 
     cPrgmTitle AT ROW 2.43 COL 98 COLON-ALIGNED WIDGET-ID 616
     ttTask.frequency AT ROW 3.62 COL 14.6 NO-LABEL WIDGET-ID 604
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Every", "Every":U,
"Daily", "Daily":U,
"Weekly", "Weekly":U,
"Monthly", "Monthly":U
          SIZE 11 BY 4.76
     ttTask.cTaskTime AT ROW 4.33 COL 38 COLON-ALIGNED WIDGET-ID 598
          LABEL "Time"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 
     ttTask.cFromTime AT ROW 4.33 COL 55 COLON-ALIGNED WIDGET-ID 626
          LABEL "From"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 
     ttTask.cToTime AT ROW 4.33 COL 69 COLON-ALIGNED WIDGET-ID 628
          LABEL "To"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 
     ttTask.dayOfWeek1 AT ROW 6.24 COL 28 WIDGET-ID 488
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY .81
     ttTask.dayOfWeek2 AT ROW 6.24 COL 44 WIDGET-ID 490
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY .81
     ttTask.dayOfWeek3 AT ROW 6.24 COL 60 WIDGET-ID 492
          VIEW-AS TOGGLE-BOX
          SIZE 12 BY .81
     ttTask.dayOfWeek4 AT ROW 6.24 COL 76 WIDGET-ID 494
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY .81
     ttTask.dayOfWeek5 AT ROW 6.24 COL 94 WIDGET-ID 496
          VIEW-AS TOGGLE-BOX
          SIZE 13.2 BY .81
     ttTask.dayOfWeek6 AT ROW 6.24 COL 110 WIDGET-ID 498
          VIEW-AS TOGGLE-BOX
          SIZE 10 BY .81
     ttTask.dayOfWeek7 AT ROW 6.24 COL 124 WIDGET-ID 500
          VIEW-AS TOGGLE-BOX
          SIZE 12 BY .81
     ttTask.dayOfMonth[1] AT ROW 7.67 COL 28 WIDGET-ID 524
          LABEL "1"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[2] AT ROW 7.67 COL 36 WIDGET-ID 526
          LABEL "2"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[3] AT ROW 7.67 COL 44 WIDGET-ID 528
          LABEL "3"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[4] AT ROW 7.67 COL 52 WIDGET-ID 530
          LABEL "4"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[5] AT ROW 7.67 COL 60 WIDGET-ID 532
          LABEL "5"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[6] AT ROW 7.67 COL 68 WIDGET-ID 534
          LABEL "6"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 21 ROW 7.67
         SIZE 140 BY 21.91
         FGCOLOR 1  WIDGET-ID 400.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME viewFrame
     ttTask.dayOfMonth[7] AT ROW 7.67 COL 76 WIDGET-ID 536
          LABEL "7"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[8] AT ROW 8.62 COL 28 WIDGET-ID 538
          LABEL "8"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[9] AT ROW 8.62 COL 36 WIDGET-ID 540
          LABEL "9"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[10] AT ROW 8.62 COL 44 WIDGET-ID 542
          LABEL "10"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[11] AT ROW 8.62 COL 52 WIDGET-ID 544
          LABEL "11"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[12] AT ROW 8.62 COL 60 WIDGET-ID 546
          LABEL "12"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[13] AT ROW 8.62 COL 68 WIDGET-ID 548
          LABEL "13"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[14] AT ROW 8.62 COL 76 WIDGET-ID 550
          LABEL "14"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[15] AT ROW 9.57 COL 28 WIDGET-ID 556
          LABEL "15"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     btnRunNow AT ROW 19.57 COL 80 HELP
          "Run Now" WIDGET-ID 634
     ttTask.dayOfMonth[16] AT ROW 9.57 COL 36 WIDGET-ID 562
          LABEL "16"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[17] AT ROW 9.57 COL 44 WIDGET-ID 564
          LABEL "17"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[18] AT ROW 9.57 COL 52 WIDGET-ID 552
          LABEL "18"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[19] AT ROW 9.57 COL 60 WIDGET-ID 554
          LABEL "19"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[20] AT ROW 9.57 COL 68 WIDGET-ID 558
          LABEL "20"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[21] AT ROW 9.57 COL 76 WIDGET-ID 560
          LABEL "21"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[22] AT ROW 10.52 COL 28 WIDGET-ID 570
          LABEL "22"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[23] AT ROW 10.52 COL 36 WIDGET-ID 576
          LABEL "23"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[24] AT ROW 10.52 COL 44 WIDGET-ID 578
          LABEL "24"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[25] AT ROW 10.52 COL 52 WIDGET-ID 566
          LABEL "25"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[26] AT ROW 10.52 COL 60 WIDGET-ID 568
          LABEL "26"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[27] AT ROW 10.52 COL 68 WIDGET-ID 572
          LABEL "27"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[28] AT ROW 10.52 COL 76 WIDGET-ID 574
          LABEL "28"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 21 ROW 7.67
         SIZE 140 BY 21.91
         FGCOLOR 1  WIDGET-ID 400.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME viewFrame
     ttTask.dayOfMonth[29] AT ROW 11.48 COL 28 WIDGET-ID 584
          LABEL "29"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[30] AT ROW 11.48 COL 36 WIDGET-ID 580
          LABEL "30"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.dayOfMonth[31] AT ROW 11.48 COL 44 WIDGET-ID 582
          LABEL "31"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     ttTask.lastOfMonth AT ROW 11.48 COL 76 WIDGET-ID 586
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .81
     ttTask.startDate AT ROW 12.67 COL 12 COLON-ALIGNED WIDGET-ID 514
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     btnCalendar-1 AT ROW 12.67 COL 30 WIDGET-ID 76
     startDateOption AT ROW 12.67 COL 33 COLON-ALIGNED HELP
          "Select Start Receipt Date Option" NO-LABEL WIDGET-ID 74
     ttTask.endDate AT ROW 13.86 COL 12 COLON-ALIGNED WIDGET-ID 506
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     btnCalendar-2 AT ROW 13.86 COL 30 WIDGET-ID 78
     endDateOption AT ROW 13.86 COL 33 COLON-ALIGNED HELP
          "Select End Receipt Date Option" NO-LABEL WIDGET-ID 70
     ttTask.taskFormat AT ROW 14.57 COL 80 NO-LABEL WIDGET-ID 608
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "CSV", "CSV":U,
"XLS", "XLS":U,
"DOCX", "DOCX":U,
"PDF", "PDF":U,
"HTML", "HTML":U
          SIZE 52 BY 1
     ttTask.nextDate AT ROW 15.05 COL 12 COLON-ALIGNED WIDGET-ID 510
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     ttTask.cNextTime AT ROW 15.05 COL 35 COLON-ALIGNED WIDGET-ID 596
          LABEL "Time"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 
     ttTask.lastDate AT ROW 16.24 COL 12 COLON-ALIGNED WIDGET-ID 508
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     ttTask.cLastTime AT ROW 16.24 COL 35 COLON-ALIGNED WIDGET-ID 594
          LABEL "Time"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 
     ttTask.recipients AT ROW 17.43 COL 14 NO-LABEL WIDGET-ID 600
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 125 BY 1.67
          BGCOLOR 15 
     btnClose AT ROW 1 COL 136 HELP
          "Close" WIDGET-ID 72
     btnFirst AT ROW 19.57 COL 106 HELP
          "First" WIDGET-ID 274
     btnLast AT ROW 19.62 COL 130 HELP
          "Last" WIDGET-ID 68
     btnNext AT ROW 19.57 COL 122 HELP
          "Next" WIDGET-ID 276
     btnPrev AT ROW 19.57 COL 114 HELP
          "Previous" WIDGET-ID 278
     btnAdd AT ROW 19.62 COL 23 HELP
          "Add" WIDGET-ID 20
     btnCancel AT ROW 19.62 COL 55 HELP
          "Cancel" WIDGET-ID 28
     btnCopy AT ROW 19.62 COL 31 HELP
          "Copy" WIDGET-ID 24
     btnDelete AT ROW 19.62 COL 39 HELP
          "Delete" WIDGET-ID 26
     btnReset AT ROW 19.62 COL 47 HELP
          "Reset" WIDGET-ID 22
     btnUpdate AT ROW 19.62 COL 15 HELP
          "Update/Save" WIDGET-ID 18
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 21 ROW 7.67
         SIZE 140 BY 21.91
         FGCOLOR 1  WIDGET-ID 400.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME viewFrame
     "Frequency:" VIEW-AS TEXT
          SIZE 11 BY 1 AT ROW 3.62 COL 3 WIDGET-ID 618
     "Recipients:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 17.43 COL 3 WIDGET-ID 602
     "Format:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 14.57 COL 71 WIDGET-ID 614
     transPanel AT ROW 19.33 COL 14 WIDGET-ID 16
     navPanel AT ROW 19.33 COL 105 WIDGET-ID 280
     RECT-2 AT ROW 6 COL 27 WIDGET-ID 620
     RECT-3 AT ROW 7.43 COL 27 WIDGET-ID 622
     RECT-4 AT ROW 14.33 COL 69 WIDGET-ID 624
     runNow AT ROW 19.33 COL 79 WIDGET-ID 632
     RECT-1 AT ROW 4.1 COL 33 WIDGET-ID 638
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 21 ROW 7.67
         SIZE 140 BY 21.91
         FGCOLOR 1 
         TITLE "View" WIDGET-ID 400.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttTask T "?" NO-UNDO temp-db ttTask
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "AOA Scheduler"
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
/* END-ASSIGN-TABS */.

/* BROWSE-TAB taskBrowse searchBar DEFAULT-FRAME */
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
/* SETTINGS FOR FILL-IN ttTask.cFromTime IN FRAME viewFrame
   NO-ENABLE 4 5 6 EXP-LABEL                                            */
/* SETTINGS FOR FILL-IN ttTask.cLastTime IN FRAME viewFrame
   NO-ENABLE 4 EXP-LABEL                                                */
ASSIGN 
       ttTask.cLastTime:READ-ONLY IN FRAME viewFrame        = TRUE.

/* SETTINGS FOR FILL-IN ttTask.cNextTime IN FRAME viewFrame
   NO-ENABLE 4 EXP-LABEL                                                */
ASSIGN 
       ttTask.cNextTime:READ-ONLY IN FRAME viewFrame        = TRUE.

/* SETTINGS FOR FILL-IN cPrgmTitle IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttTask.cTaskTime IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN ttTask.cToTime IN FRAME viewFrame
   NO-ENABLE 4 5 6 EXP-LABEL                                            */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[10] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[11] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[12] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[13] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[14] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[15] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[16] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[17] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[18] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[19] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[1] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[20] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[21] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[22] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[23] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[24] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[25] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[26] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[27] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[28] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[29] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[2] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[30] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[31] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[3] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[4] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[5] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[6] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[7] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[8] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfMonth[9] IN FRAME viewFrame
   NO-ENABLE 4 5 EXP-LABEL                                              */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfWeek1 IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfWeek2 IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfWeek3 IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfWeek4 IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfWeek5 IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfWeek6 IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR TOGGLE-BOX ttTask.dayOfWeek7 IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN ttTask.endDate IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR COMBO-BOX endDateOption IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET ttTask.frequency IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN ttTask.lastDate IN FRAME viewFrame
   NO-ENABLE 4                                                          */
ASSIGN 
       ttTask.lastDate:READ-ONLY IN FRAME viewFrame        = TRUE.

/* SETTINGS FOR TOGGLE-BOX ttTask.lastOfMonth IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR RECTANGLE navPanel IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttTask.nextDate IN FRAME viewFrame
   NO-ENABLE 4                                                          */
ASSIGN 
       ttTask.nextDate:READ-ONLY IN FRAME viewFrame        = TRUE.

/* SETTINGS FOR FILL-IN ttTask.program-id IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
ASSIGN 
       ttTask.program-id:READ-ONLY IN FRAME viewFrame        = TRUE.

/* SETTINGS FOR EDITOR ttTask.recipients IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE runNow IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX ttTask.scheduled IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN ttTask.securityLevel IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN ttTask.startDate IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR COMBO-BOX startDateOption IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET ttTask.taskFormat IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN ttTask.taskID IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN ttTask.taskName IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR RECTANGLE transPanel IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttTask.user-id IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
ASSIGN 
       ttTask.user-id:READ-ONLY IN FRAME viewFrame        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE taskBrowse
/* Query rebuild information for BROWSE taskBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTask
WHERE ttTask.company EQ g_company
  AND ttTask.allData MATCHES "*" + searchBar + "*"
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE taskBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME viewFrame
/* Query rebuild information for FRAME viewFrame
     _TblList          = "Temp-Tables.ttTask"
     _Query            is NOT OPENED
*/  /* FRAME viewFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* AOA Scheduler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* AOA Scheduler */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN pSaveSettings (USERID("ASI")).
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* AOA Scheduler */
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
    
    cRecipients = ttTask.recipients:SCREEN-VALUE.
    RUN AOA/aoaRecipients.w (INPUT-OUTPUT cRecipients).
    ttTask.recipients:SCREEN-VALUE = cRecipients.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 C-Win
ON CHOOSE OF btnCalendar-1 IN FRAME viewFrame
DO:
  {methods/btnCalendar.i ttTask.startDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 C-Win
ON CHOOSE OF btnCalendar-2 IN FRAME viewFrame
DO:
  {methods/btnCalendar.i ttTask.endDate}
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
    APPLY "CHOOSE":U TO btnRun IN FRAME DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
&Scoped-define SELF-NAME ttTask.cFromTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttTask.cFromTime C-Win
ON LEAVE OF ttTask.cFromTime IN FRAME viewFrame /* From */
DO:
    IF NOT fValidTime(SELF) THEN DO:
        APPLY "ENTRY":U TO SELF.
        RETURN NO-APPLY.
    END. /* if not validtime */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttTask.cLastTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttTask.cLastTime C-Win
ON ENTRY OF ttTask.cLastTime IN FRAME viewFrame /* Time */
DO:
    RUN pReadOnlyField (SELF).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttTask.cNextTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttTask.cNextTime C-Win
ON ENTRY OF ttTask.cNextTime IN FRAME viewFrame /* Time */
DO:
    RUN pReadOnlyField (SELF).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttTask.cTaskTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttTask.cTaskTime C-Win
ON LEAVE OF ttTask.cTaskTime IN FRAME viewFrame /* Time */
DO:
    IF NOT fValidTime(SELF) THEN DO:
        APPLY "ENTRY":U TO SELF.
        RETURN NO-APPLY.
    END. /* if not validtime */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttTask.cToTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttTask.cToTime C-Win
ON LEAVE OF ttTask.cToTime IN FRAME viewFrame /* To */
DO:
    IF NOT fValidTime(SELF) THEN DO:
        APPLY "ENTRY":U TO SELF.
        RETURN NO-APPLY.
    END. /* if not validtime */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttTask.endDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttTask.endDate C-Win
ON HELP OF ttTask.endDate IN FRAME viewFrame /* End Date */
DO:
    {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME endDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL endDateOption C-Win
ON VALUE-CHANGED OF endDateOption IN FRAME viewFrame
DO:
    {AOA/includes/tDateOption.i &dateObject=ttTask.endDate &btnCalendar=2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttTask.frequency
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttTask.frequency C-Win
ON VALUE-CHANGED OF ttTask.frequency IN FRAME viewFrame
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


&Scoped-define SELF-NAME ttTask.lastDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttTask.lastDate C-Win
ON ENTRY OF ttTask.lastDate IN FRAME viewFrame /* Last Date */
DO:
    RUN pReadOnlyField (SELF).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttTask.nextDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttTask.nextDate C-Win
ON ENTRY OF ttTask.nextDate IN FRAME viewFrame /* Next Date */
DO:
    RUN pReadOnlyField (SELF).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttTask.program-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttTask.program-id C-Win
ON ENTRY OF ttTask.program-id IN FRAME viewFrame /* Program ID */
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


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME ttTask.startDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttTask.startDate C-Win
ON HELP OF ttTask.startDate IN FRAME viewFrame /* Start Date */
DO:
    {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME startDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL startDateOption C-Win
ON VALUE-CHANGED OF startDateOption IN FRAME viewFrame
DO:
    {AOA/includes/tDateOption.i &dateObject=ttTask.startDate &btnCalendar=1}
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME ttTask.taskID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttTask.taskID C-Win
ON LEAVE OF ttTask.taskID IN FRAME viewFrame /* Task ID */
DO:
    cPrgmTitle:SCREEN-VALUE IN FRAME viewFrame =
        fPrgmTitle(ttTask.program-id:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttTask.user-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttTask.user-id C-Win
ON ENTRY OF ttTask.user-id IN FRAME viewFrame /* User ID */
DO:
    RUN pReadOnlyField (SELF).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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

DEFINE VARIABLE idx AS INTEGER NO-UNDO.
DO idx = 1 to 2:
    CREATE ttTask.
    ASSIGN
        ttTask.scheduled = idx EQ 1
        ttTask.company = "001"
        ttTask.taskName = IF idx EQ 1 THEN "TR3 - Every" ELSE "OR5 - Every"
        ttTask.program-id = IF idx EQ 1 THEN "r-mchtrn." ELSE "r-booked."
        ttTask.user-id = "NoSweat"
        ttTask.taskID = 28
        ttTask.frequency = "Every"
        ttTask.cTaskTime = "0100"
        ttTask.taskTime = 3600
        ttTask.cFromTime = "0800"
        ttTask.fromTime = 28800
        ttTask.ctoTime = "1700"
        ttTask.toTime = 61200
        ttTask.dayOfWeek1 = NO
        ttTask.dayOfWeek2 = NO
        ttTask.dayOfWeek3 = NO
        ttTask.dayOfWeek4 = NO
        ttTask.dayOfWeek5 = NO
        ttTask.dayOfWeek6 = NO
        ttTask.dayOfWeek7 = NO
        ttTask.taskFormat = "CSV"
        ttTask.recipients = "ron.stark@advantzware.com,ron@thestarkgroup.com,ronstark@hotmail.com"
        ttTask.allData = fAllData()
        .
    CREATE ttTask.
    ASSIGN
        ttTask.scheduled = YES
        ttTask.company = "001"
        ttTask.taskName = IF idx EQ 1 THEN "TR3 - Daily" ELSE "OR5 - Daily"
        ttTask.program-id = IF idx EQ 1 THEN "r-mchtrn." ELSE "r-booked."
        ttTask.user-id = "NoSweat"
        ttTask.taskID = 28
        ttTask.frequency = "Daily"
        ttTask.cTaskTime = "1700"
        ttTask.taskTime = 61200
        ttTask.dayOfWeek1 = NO
        ttTask.dayOfWeek2 = NO
        ttTask.dayOfWeek3 = NO
        ttTask.dayOfWeek4 = NO
        ttTask.dayOfWeek5 = NO
        ttTask.dayOfWeek6 = NO
        ttTask.dayOfWeek7 = NO
        ttTask.taskFormat = "CSV"
        ttTask.recipients = "ron.stark@advantzware.com,ron@thestarkgroup.com,ronstark@hotmail.com"
        ttTask.allData = fAllData()
        .
    CREATE ttTask.
    ASSIGN
        ttTask.scheduled = YES
        ttTask.company = "001"
        ttTask.taskName = IF idx EQ 1 THEN "TR3 - Weekly" ELSE "OR5 - Weekly"
        ttTask.program-id = IF idx EQ 1 THEN "r-mchtrn." ELSE "r-booked."
        ttTask.user-id = "NoSweat"
        ttTask.taskID = 28
        ttTask.frequency = "Weekly"
        ttTask.taskTime = 0
        ttTask.dayOfWeek1 = NO
        ttTask.dayOfWeek2 = YES
        ttTask.dayOfWeek3 = YES
        ttTask.dayOfWeek4 = YES
        ttTask.dayOfWeek5 = YES
        ttTask.dayOfWeek6 = YES
        ttTask.dayOfWeek7 = NO
        ttTask.taskFormat = "PDF"
        ttTask.recipients = "ron.stark@advantzware.com,ron@thestarkgroup.com"
        ttTask.allData = fAllData()
        .
    CREATE ttTask.
    ASSIGN
        ttTask.scheduled = NO
        ttTask.company = "001"
        ttTask.taskName = IF idx EQ 1 THEN "TR3 - Monthly" ELSE "OR5 - Monthly"
        ttTask.program-id = IF idx EQ 1 THEN "r-mchtrn." ELSE "r-booked."
        ttTask.user-id = "NoSweat"
        ttTask.taskID = 28
        ttTask.frequency = "Monthly"
        ttTask.taskTime = 0
        ttTask.dayOfWeek1 = NO
        ttTask.dayOfWeek2 = NO
        ttTask.dayOfWeek3 = NO
        ttTask.dayOfWeek4 = NO
        ttTask.dayOfWeek5 = NO
        ttTask.dayOfWeek6 = NO
        ttTask.dayOfWeek7 = NO
        ttTask.lastOfMonth = YES
        ttTask.taskFormat = "XLS"
        ttTask.recipients = "ron.stark@advantzware.com,ron@thestarkgroup.com"
        ttTask.allData = fAllData()
        .
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF lContinue THEN DO:
      hContainer = THIS-PROCEDURE.
      RUN enable_UI.
      DYNAMIC-FUNCTION('fDateOptions',startDateOption:HANDLE).
      DYNAMIC-FUNCTION('fDateOptions',endDateOption:HANDLE).
      RUN pGetSettings (USERID("ASI")).
  END. /* if continue */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{methods/sortByProc.i "pBySchedule" "ttTask.scheduled"}
{methods/sortByProc.i "pByTaskName" "ttTask.taskName"}
{methods/sortByProc.i "pByFrequency" "ttTask.frequency"}
{methods/sortByProc.i "pByTaskTime" "ttTask.cTaskTime"}
{methods/sortByProc.i "pByFromTime" "ttTask.cFromTime"}
{methods/sortByProc.i "pByTotTime" "ttTask.cToTime"}
{methods/sortByProc.i "pByTaskFormat" "ttTask.taskFormat"}
{methods/sortByProc.i "pByNextDate" "ttTask.nextDate"}
{methods/sortByProc.i "pByNextTime" "ttTask.cNextTime"}
{methods/sortByProc.i "pByLastDate" "ttTask.lastDate"}
{methods/sortByProc.i "pByLastTime" "ttTask.cLastTime"}
{methods/sortByProc.i "pByStartDate" "ttTask.startDate"}
{methods/sortByProc.i "pByEndDate" "ttTask.endDate"}
{methods/sortByProc.i "pByTaskID" "ttTask.taskID"}
{methods/sortByProc.i "pByProgramID" "ttTask.program-id"}
{methods/sortByProc.i "pByUserID" "ttTask.user-id"}

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
  DISPLAY searchBar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnView searchBar taskBrowse btnRun btnRestoreDefaults 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY cPrgmTitle startDateOption endDateOption 
      WITH FRAME viewFrame IN WINDOW C-Win.
  IF AVAILABLE ttTask THEN 
    DISPLAY ttTask.scheduled ttTask.taskName ttTask.securityLevel ttTask.taskID 
          ttTask.user-id ttTask.program-id ttTask.frequency ttTask.cTaskTime 
          ttTask.cFromTime ttTask.cToTime ttTask.dayOfWeek1 ttTask.dayOfWeek2 
          ttTask.dayOfWeek3 ttTask.dayOfWeek4 ttTask.dayOfWeek5 
          ttTask.dayOfWeek6 ttTask.dayOfWeek7 ttTask.dayOfMonth[1] 
          ttTask.dayOfMonth[2] ttTask.dayOfMonth[3] ttTask.dayOfMonth[4] 
          ttTask.dayOfMonth[5] ttTask.dayOfMonth[6] ttTask.dayOfMonth[7] 
          ttTask.dayOfMonth[8] ttTask.dayOfMonth[9] ttTask.dayOfMonth[10] 
          ttTask.dayOfMonth[11] ttTask.dayOfMonth[12] ttTask.dayOfMonth[13] 
          ttTask.dayOfMonth[14] ttTask.dayOfMonth[15] ttTask.dayOfMonth[16] 
          ttTask.dayOfMonth[17] ttTask.dayOfMonth[18] ttTask.dayOfMonth[19] 
          ttTask.dayOfMonth[20] ttTask.dayOfMonth[21] ttTask.dayOfMonth[22] 
          ttTask.dayOfMonth[23] ttTask.dayOfMonth[24] ttTask.dayOfMonth[25] 
          ttTask.dayOfMonth[26] ttTask.dayOfMonth[27] ttTask.dayOfMonth[28] 
          ttTask.dayOfMonth[29] ttTask.dayOfMonth[30] ttTask.dayOfMonth[31] 
          ttTask.lastOfMonth ttTask.startDate ttTask.endDate ttTask.taskFormat 
          ttTask.nextDate ttTask.cNextTime ttTask.lastDate ttTask.cLastTime 
          ttTask.recipients 
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
        FIND CURRENT ttTask EXCLUSIVE-LOCK.
        ASSIGN
            {&enabledFields}
            ttTask.taskTime = fConvertTime(ttTask.cTaskTime)
            ttTask.fromTime = fConvertTime(ttTask.cFromTime)
            ttTask.toTime   = fConvertTime(ttTask.cToTime)
            ttTask.nextTime = fConvertTime(ttTask.cNextTime)
            ttTask.lastTime = fConvertTime(ttTask.cLastTime)
            ttTask.allData  = fAllData()
            .
        CASE ttTask.frequency:
            WHEN "Daily" THEN
            ASSIGN
                ttTask.dayOfWeek1  = NO
                ttTask.dayOfWeek2  = NO
                ttTask.dayOfWeek3  = NO
                ttTask.dayOfWeek4  = NO
                ttTask.dayOfWeek5  = NO
                ttTask.dayOfWeek6  = NO
                ttTask.dayOfWeek7  = NO
                ttTask.dayOfWeek   = NO
                ttTask.dayOfMonth  = NO
                ttTask.lastOfMonth = NO
                .
            WHEN "Weekly" THEN
            ASSIGN
                ttTask.dayOfWeek[1] = ttTask.dayOfWeek1
                ttTask.dayOfWeek[2] = ttTask.dayOfWeek2
                ttTask.dayOfWeek[3] = ttTask.dayOfWeek3
                ttTask.dayOfWeek[4] = ttTask.dayOfWeek4
                ttTask.dayOfWeek[5] = ttTask.dayOfWeek5
                ttTask.dayOfWeek[6] = ttTask.dayOfWeek6
                ttTask.dayOfWeek[7] = ttTask.dayOfWeek7
                ttTask.dayOfMonth   = NO
                ttTask.lastOfMonth  = NO
                .
            WHEN "Monthly" THEN
            ASSIGN
                ttTask.dayOfWeek1 = NO
                ttTask.dayOfWeek2 = NO
                ttTask.dayOfWeek3 = NO
                ttTask.dayOfWeek4 = NO
                ttTask.dayOfWeek5 = NO
                ttTask.dayOfWeek6 = NO
                ttTask.dayOfWeek7 = NO
                ttTask.dayOfWeek  = NO
                .
        END CASE.
        FIND CURRENT ttTask NO-LOCK.
    END. /* with frame */

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
                ENABLE {&transUpdate} {&enabledFields} {&calendarObjects}.
                btnUpdate:LOAD-IMAGE("Graphics\32x32\Save_As.ico").
                IF iphMode:LABEL EQ "Add" THEN DO:
                    RUN pClearView.
                    DISABLE btnReset.
                END. /* add */
                IF iphMode:LABEL EQ "Add" OR iphMode:LABEL EQ "Copy" THEN DO:
                END. /* if add or copy */
                ASSIGN
                    FRAME viewFrame:TITLE = iphMode:LABEL
                    btnUpdate:LABEL       = "Save"
                    .
                APPLY "LEAVE":U TO ttTask.taskID.
                APPLY "VALUE-CHANGED":U TO ttTask.frequency.
            END. /* add copy update */
            WHEN "Cancel" OR WHEN "Save" THEN DO:
                IF iphMode:LABEL EQ "Save" THEN DO:
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO:
                        IF cMode EQ "Copy" THEN DO:
                        END. /* if copy */
                        ELSE DO: /* add */
                        END. /* else add */
                    END. /* if add/copy */
                    RUN pAssign.
                    BROWSE {&BROWSE-NAME}:REFRESH().
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO:
                    END. /* if add */
                END. /* save */
                DISABLE {&transPanel} {&enabledFields} {&calendarObjects}.
                ENABLE {&transInit}.
                btnUpdate:LOAD-IMAGE("Graphics\32x32\Pencil.ico").
                ASSIGN
                    FRAME viewFrame:TITLE           = "View"
                    btnUpdate:LABEL                 = "Update"
                    BROWSE {&BROWSE-NAME}:SENSITIVE = YES
                    .
                APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.
            END. /* cancel save */
            WHEN "Delete" THEN DO:
                IF AVAILABLE ttTask THEN DO:
                    MESSAGE
                        "Delete Currently Selected Record?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    UPDATE lContinue.
                    IF lContinue THEN DO:
                        cMode = iphMode:LABEL.
                        BROWSE {&BROWSE-NAME}:DELETE-CURRENT-ROW().
                    END. /* if lcontinue */
                    IF AVAILABLE ttTask THEN
                    BROWSE {&BROWSE-NAME}:REFRESH().
                END. /* if avail */
            END. /* delete */
            WHEN "Reset" THEN DO:
                RUN pDisplay.
                APPLY "VALUE-CHANGED":U TO ttTask.frequency.
            END. /* reset */
        END CASE. /* ipcmode:label */
        IF ttTask.scheduled:SENSITIVE THEN
        APPLY "ENTRY":U TO ttTask.scheduled.
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
    IF AVAILABLE ttTask THEN
    DO WITH FRAME viewFrame:
        DISPLAY {&displayFields}.
        ENABLE {&transInit}.
        APPLY "LEAVE":U TO ttTask.taskID.
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
                      AND user-print.program-id EQ "{&program-id}"
                      AND user-print.user-id    EQ "_default") THEN
    RUN pSaveSettings ("_default").
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "{&program-id}"
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
                ASSIGN
                    BROWSE {&BROWSE-NAME}:ROW-HEIGHT = DECIMAL(user-print.field-value[idx])
                    .
            END CASE.
        END. /* do idx */
        DO idx = iUserPrintOffSet + 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            /* set browse column width, hidden & order */
            DO kdx = 1 TO BROWSE {&BROWSE-NAME}:NUM-COLUMNS:
                IF user-print.field-name[idx] EQ BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(kdx):NAME THEN DO:
                    ASSIGN
                        jdx = idx - iUserPrintOffSet
                        hColumn = BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(jdx)
                        hColumn:WIDTH = DECIMAL(user-print.field-value[idx])
                        .
                    BROWSE {&BROWSE-NAME}:MOVE-COLUMN(kdx,jdx).
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
    IF AVAILABLE ttTask THEN
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
        WHEN "scheduled" THEN
        RUN pBySchedule.
        WHEN "taskName" THEN
        RUN pByTaskName.
        WHEN "frequency" THEN
        RUN pByFrequency.
        WHEN "cTaskTime" THEN
        RUN pByTaskTime.
        WHEN "cFromTime" THEN
        RUN pByFromTime.
        WHEN "cToTime" THEN
        RUN pByTotTime.
        WHEN "taskFormat" THEN
        RUN pByTaskFormat.
        WHEN "nextDate" THEN
        RUN pByNextDate.
        WHEN "cNextTime" THEN
        RUN pByNextTime.
        WHEN "lastDate" THEN
        RUN pByLastDate.
        WHEN "cLastTime" THEN
        RUN pByLastTime.
        WHEN "startDate" THEN
        RUN pByStartDate.
        WHEN "endDate" THEN
        RUN pByEndDate.
        WHEN "taskID" THEN
        RUN pByTaskID.
        WHEN "program-id" THEN
        RUN pByProgramID.
        WHEN "user-id" THEN
        RUN pByUserID.
    END CASE.

END PROCEDURE.

/*
fPrgmTitle(WHEN "program-id) FORMAT "x(40)" LABEL "Title"
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunNow C-Win 
PROCEDURE pRunNow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF AVAILABLE ttTask THEN DO:
        ttTask.runNow = YES.
        MESSAGE
            "Task ~"" + ttTask.taskName + "~" has been submitted."
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
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company    = g_company
            user-print.program-id = "{&program-id}"
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
        user-print.field-value[idx] = STRING(BROWSE {&BROWSE-NAME}:ROW-HEIGHT)
        .
    /* save browse column order and width */
    DO jdx = 1 TO BROWSE {&BROWSE-NAME}:NUM-COLUMNS:
        ASSIGN
            idx = idx + 1
            hColumn = BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(jdx)
            user-print.field-label[idx] = "BrowseColumn"
            user-print.field-name[idx]  = hColumn:NAME
            user-print.field-value[idx] = STRING(MAX(hColumn:WIDTH, .2 /*BROWSE {&BROWSE-NAME}:MIN-COLUMN-WIDTH-CHARS*/ ))
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
            /* default frame */
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH  = {&WINDOW-NAME}:WIDTH
            /* search bar */
            searchBar:WIDTH = FRAME {&FRAME-NAME}:WIDTH
                            - searchBar:COL + 1
            /* browse frame */
            BROWSE {&BROWSE-NAME}:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT
                                         - BROWSE {&BROWSE-NAME}:ROW + 1
            BROWSE {&BROWSE-NAME}:WIDTH  = FRAME {&FRAME-NAME}:WIDTH
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
        ttTask.taskName + "|" +
        STRING(ttTask.taskID) + "|" +
        ttTask.user-id + "|" +
        ttTask.program-id + "|" +
        ttTask.frequency + "|" +
        ttTask.taskFormat + "|" +
        ttTask.recipients + "|" +
        fPrgmTitle(ttTask.program-id)
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
        iTime = INTEGER(SUBSTR(ipcTime,1,2)) * 3600
              + INTEGER(SUBSTR(ipcTime,4,2)) * 60
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fTimeConvert C-Win 
FUNCTION fTimeConvert RETURNS CHARACTER
  (ipiTime AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTime AS CHARACTER NO-UNDO.
    
    ASSIGN
        cTime = STRING(ipiTime,"HH:MM")
        cTime = SUBSTR(cTime,1,2) + SUBSTR(cTime,4,2)
        .
    RETURN cTime.

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

