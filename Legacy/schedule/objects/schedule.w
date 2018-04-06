&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: schedule.w

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:  <none>

  Output Parameters: <none>

  History: Ron Stark - 5.28.2001
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

DEFINE VARIABLE progressINI AS CHARACTER NO-UNDO.

IF SEARCH('nosweat.r') EQ ? THEN DO:
  GET-KEY-VALUE SECTION 'STARTUP'
    KEY 'DLC'
    VALUE progressINI.
  progressINI = progressINI + '\bin\progress'.
  LOAD progressINI.
  USE progressINI.
END.

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{system/sysconst.i}
{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{{&includes}/filterVars.i NEW}
{{&viewers}/includes/sharedVars.i NEW}

DEFINE VARIABLE accumLastSave AS INTEGER NO-UNDO.
DEFINE VARIABLE asOfTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE autoMonitor AS LOGICAL NO-UNDO.
DEFINE VARIABLE autoMonitorImage AS INTEGER NO-UNDO.
DEFINE VARIABLE boardObject AS CHARACTER NO-UNDO INITIAL '{&objects}/board{&Board}.w'.
DEFINE VARIABLE boardSize AS CHARACTER NO-UNDO.
DEFINE VARIABLE closeBoard AS LOGICAL NO-UNDO INITIAL YES.
DEFINE VARIABLE dontShowStartup AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE ID AS CHARACTER NO-UNDO {{&includes}/initID.i}.
DEFINE VARIABLE inUseMsg AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE lvLoginID AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE popup AS HANDLE NO-UNDO EXTENT 13.
DEFINE VARIABLE reSized AS LOGICAL NO-UNDO.
DEFINE VARIABLE turnOff AS LOGICAL NO-UNDO.
DEFINE VARIABLE updatesPending AS CHARACTER NO-UNDO.
DEFINE VARIABLE useSequence AS LOGICAL NO-UNDO INITIAL YES.
DEFINE VARIABLE winTitle AS CHARACTER NO-UNDO.

SESSION:SET-WAIT-STATE('').
IF LDBNAME(1) NE ? THEN
SESSION:TIME-SOURCE = LDBNAME(1).

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
{{&includes}/lockWindowUpdate.i}
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME schedulerFrame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnAutoMonitor btnColumns btnSequencer ~
btnMoveResource btnPrint btnReLoad btnPackResource btnPackBoard ~
btnBringForward btnAbout btnHelp 
&Scoped-Define DISPLAYED-OBJECTS packText lastSave 

/* Custom List Definitions                                              */
/* boardButtons,buttonList,autoMonitorObjects,List-4,List-5,List-6      */
&Scoped-define boardButtons btnUpdates btnColumns btnSequencer ~
btnMoveResource btnPrint btnReLoad btnDataCollection btnPackResource ~
btnPackBoard btnBringForward packText 
&Scoped-define buttonList btnUpdates btnColumns btnSequencer ~
btnMoveResource btnPrint btnReLoad btnDataCollection btnPackResource ~
btnPackBoard btnBringForward btnCapacityView btnLegend btnAbout btnHelp ~
packText 
&Scoped-define autoMonitorObjects btnUpdates btnColumns btnSequencer ~
btnMoveResource btnPrint btnReLoad btnDataCollection btnPackResource ~
btnPackBoard btnBringForward 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD updatesPending W-Win 
FUNCTION updatesPending RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Pack 
       MENU-ITEM m_btnPackResource LABEL "Selected Resource/Job"
       MENU-ITEM m_btnPackBoard LABEL "All Jobs"      .

DEFINE SUB-MENU m_Board_Date 
       MENU-ITEM m_btnPrevDate  LABEL "Previous Date" 
       MENU-ITEM m_btnNextDate  LABEL "Next Date"     
       MENU-ITEM m_btnCalendar  LABEL "Popup Calendar"
       MENU-ITEM m_btnTimeLine  LABEL "Current Date"  .

DEFINE SUB-MENU m_Board_Interval 
       MENU-ITEM m_btnPrevInterval LABEL "Previous Interval"
       MENU-ITEM m_btnNextInterval LABEL "Next Interval" .

DEFINE SUB-MENU m_Scenario 
       MENU-ITEM m_btnSave      LABEL "Save"          
       MENU-ITEM m_btnReset     LABEL "Reset" .

DEFINE MENU POPUP-MENU-W-Win 
       MENU-ITEM m_Board        LABEL "Board"         
       MENU-ITEM m_Configuration LABEL "Configuration" 
       MENU-ITEM m_Downtime     LABEL "Downtime"      
       MENU-ITEM m_Resources    LABEL "Resources"     
       RULE
       MENU-ITEM m_btnUpdates   LABEL "Updates Pending"
       MENU-ITEM m_btnColumns   LABEL "Browse Columns Order/Report Layouts"
       MENU-ITEM m_btnBringForward LABEL "Bring Past Jobs Forward"
       MENU-ITEM m_btnSequencer LABEL "Sequencer"     
       MENU-ITEM m_btnMoveResource LABEL "Move Resource" 
       MENU-ITEM m_btnPrint     LABEL "Print"         
       MENU-ITEM m_btnReLoad    LABEL "Reload Board"  
       RULE
       SUB-MENU  m_Pack         LABEL "Pack"          
       MENU-ITEM m_btnUseSequence LABEL "(Don't) Use Job Sequence"
       RULE
       MENU-ITEM m_btnCapacityView LABEL "Capacity View" 
       MENU-ITEM m_btnLegend    LABEL "View Color Legend"
       RULE
       SUB-MENU  m_Board_Date   LABEL "Board Date"    
       SUB-MENU  m_Board_Interval LABEL "Board Interval"
       RULE
       SUB-MENU  m_Scenario     LABEL "Scenario"      
       RULE
       MENU-ITEM m_btnJobSeqScan LABEL "Job Sequence Scan" .        
       RULE
       MENU-ITEM m_btnPending   LABEL "Pending by Job"
       MENU-ITEM m_btnPendingJobs LABEL "Pending by Resource"
       RULE
       MENU-ITEM m_btnComplete  LABEL "Complete Job"  
       MENU-ITEM m_btnJobNotes  LABEL "Job Notes"     
       RULE
       MENU-ITEM m_btnDatePrompt LABEL "Date Prompt On/Off"
       MENU-ITEM m_btnShowDowntime LABEL "Show/Hide Downtime"
       MENU-ITEM m_btnDetail    LABEL "Detail Windows On/Off"
       MENU-ITEM m_btnFlashlight LABEL "Flashlight On/Off"
       MENU-ITEM m_btnJobBrowse LABEL "Job Browse"    
       RULE
       MENU-ITEM m_btnHelp      LABEL "Help"          
       MENU-ITEM m_btnAbout     LABEL "About (Support Contact Info)"
       RULE
       MENU-ITEM m_Exit         LABEL "E&xit"         .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_board AS HANDLE NO-UNDO.
DEFINE VARIABLE h_config AS HANDLE NO-UNDO.
DEFINE VARIABLE h_downtime AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_resources AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAbout 
     IMAGE-UP FILE "schedule/images/info.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "About (Support Contact Information)".

DEFINE BUTTON btnAutoMonitor 
     IMAGE-UP FILE "schedule/images/media_play.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Turn Auto Monitor ON".

DEFINE BUTTON btnBringForward 
     IMAGE-UP FILE "schedule/images/bringforward.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Bring Past Jobs Forward"
     FONT 6.

DEFINE BUTTON btnCapacityView 
     IMAGE-UP FILE "schedule/images/capacityview.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Access Capacity View".

DEFINE BUTTON btnColumns 
     IMAGE-UP FILE "schedule/images/grids.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Browser Column Order and Report Layouts".

DEFINE BUTTON btnDataCollection 
     IMAGE-UP FILE "schedule/images/datacollection.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Import Data Collection".

DEFINE BUTTON btnHelp 
     IMAGE-UP FILE "schedule/images/help.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Help (User Manual)".

DEFINE BUTTON btnLegend 
     IMAGE-UP FILE "schedule/images/legend.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Color Legend".

DEFINE BUTTON btnMoveResource 
     IMAGE-UP FILE "schedule/images/moveresource.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Move Resource".

DEFINE BUTTON btnPackBoard 
     IMAGE-UP FILE "schedule/images/entireboard.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Pack All Jobs"
     FONT 6.

DEFINE BUTTON btnPackResource 
     LABEL "?" 
     SIZE 16 BY 1.1 TOOLTIP "Select Resource or Job to Enable"
     FONT 6.

DEFINE BUTTON btnPrint 
     IMAGE-UP FILE "schedule/images/print.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Print".

DEFINE BUTTON btnReLoad 
     IMAGE-UP FILE "schedule/images/refresh.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Reload Board".

DEFINE BUTTON btnSequencer 
     IMAGE-UP FILE "schedule/images/setseq.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Sequencer".

DEFINE BUTTON btnUpdates 
     IMAGE-UP FILE "schedule/images/viewtable.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Updates Pending".

DEFINE VARIABLE lastSave AS CHARACTER FORMAT "X(256)":U INITIAL "99:99:99" 
      VIEW-AS TEXT 
     SIZE 9 BY 1
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE packText AS CHARACTER FORMAT "X(256)":U INITIAL "Pack:" 
      VIEW-AS TEXT 
     SIZE 6 BY 1.1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME schedulerFrame
     btnAutoMonitor AT ROW 1 COL 68 HELP
          "Click to Turn Auto Monitor On/Off" WIDGET-ID 2
     btnUpdates AT ROW 1 COL 73 HELP
          "Click to Process Updates Pending"
     btnColumns AT ROW 1 COL 78 HELP
          "Click to Access Browser Column Order and Report Layouts"
     btnSequencer AT ROW 1 COL 83 HELP
          "Click to Access Sequencer"
     btnMoveResource AT ROW 1 COL 88 HELP
          "Click to Access Move Resource"
     btnPrint AT ROW 1 COL 93 HELP
          "Click to Access Print Utility"
     btnReLoad AT ROW 1 COL 98 HELP
          "Click to Reload Board Jobs from Source"
     btnDataCollection AT ROW 1 COL 103 HELP
          "Click to Import Data Collection"
     btnPackResource AT ROW 1 COL 115 HELP
          "Click to Pack Select Job"
     btnPackBoard AT ROW 1 COL 131 HELP
          "Click to Pack All Jobs"
     btnBringForward AT ROW 1.05 COL 136 HELP
          "Click to Bring Past Jobs Forward"
     btnCapacityView AT ROW 1.05 COL 141 HELP
          "Click to Access Capacity View"
     btnLegend AT ROW 1.05 COL 146 HELP
          "Click to Display Color Legend"
     btnAbout AT ROW 1.05 COL 151 HELP
          "Click to Access About Information"
     btnHelp AT ROW 1.05 COL 156 HELP
          "Click to Access Help"
     packText AT ROW 1 COL 107 COLON-ALIGNED NO-LABEL
     lastSave AT ROW 1.1 COL 57 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 160.2 BY 28.57
         DEFAULT-BUTTON btnPackResource.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Scheduler"
         HEIGHT             = 28.57
         WIDTH              = 160.2
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         POPUP-MENU         = MENU POPUP-MENU-W-Win:HANDLE
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:POPUP-MENU = MENU POPUP-MENU-W-Win:HANDLE.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("schedule/images/scheduler.ico":U) THEN
    MESSAGE "Unable to load icon: schedule/images/scheduler.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME schedulerFrame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME schedulerFrame:HEIGHT           = 28.57
       FRAME schedulerFrame:WIDTH            = 160.2.

/* SETTINGS FOR BUTTON btnAbout IN FRAME schedulerFrame
   2                                                                    */
ASSIGN 
       btnAbout:PRIVATE-DATA IN FRAME schedulerFrame     = 
                "boardObject".

ASSIGN 
       btnAutoMonitor:PRIVATE-DATA IN FRAME schedulerFrame     = 
                "Turn Auto Monitor".

/* SETTINGS FOR BUTTON btnBringForward IN FRAME schedulerFrame
   1 2 3                                                                */
/* SETTINGS FOR BUTTON btnCapacityView IN FRAME schedulerFrame
   NO-ENABLE 2                                                          */
ASSIGN 
       btnCapacityView:HIDDEN IN FRAME schedulerFrame           = TRUE
       btnCapacityView:PRIVATE-DATA IN FRAME schedulerFrame     = 
                "boardObject".

/* SETTINGS FOR BUTTON btnColumns IN FRAME schedulerFrame
   1 2 3                                                                */
/* SETTINGS FOR BUTTON btnDataCollection IN FRAME schedulerFrame
   NO-ENABLE 1 2 3                                                      */
ASSIGN 
       btnDataCollection:HIDDEN IN FRAME schedulerFrame           = TRUE.

/* SETTINGS FOR BUTTON btnHelp IN FRAME schedulerFrame
   2                                                                    */
ASSIGN 
       btnHelp:PRIVATE-DATA IN FRAME schedulerFrame     = 
                "boardObject".

/* SETTINGS FOR BUTTON btnLegend IN FRAME schedulerFrame
   NO-ENABLE 2                                                          */
ASSIGN 
       btnLegend:PRIVATE-DATA IN FRAME schedulerFrame     = 
                "boardObject".

/* SETTINGS FOR BUTTON btnMoveResource IN FRAME schedulerFrame
   1 2 3                                                                */
/* SETTINGS FOR BUTTON btnPackBoard IN FRAME schedulerFrame
   1 2 3                                                                */
/* SETTINGS FOR BUTTON btnPackResource IN FRAME schedulerFrame
   1 2 3                                                                */
/* SETTINGS FOR BUTTON btnPrint IN FRAME schedulerFrame
   1 2 3                                                                */
/* SETTINGS FOR BUTTON btnReLoad IN FRAME schedulerFrame
   1 2 3                                                                */
/* SETTINGS FOR BUTTON btnSequencer IN FRAME schedulerFrame
   1 2 3                                                                */
/* SETTINGS FOR BUTTON btnUpdates IN FRAME schedulerFrame
   NO-ENABLE 1 2 3                                                      */
/* SETTINGS FOR FILL-IN lastSave IN FRAME schedulerFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN packText IN FRAME schedulerFrame
   NO-ENABLE 1 2                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

/* OCX BINARY:FILENAME is: schedule\objects\schedule.wrx */

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME schedulerFrame:HANDLE
       ROW             = 1.95
       COLUMN          = 2
       HEIGHT          = 4.76
       WIDTH           = 20
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME schedulerFrame:HANDLE
       ROW             = 1.95
       COLUMN          = 22
       HEIGHT          = 4.76
       WIDTH           = 20
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(btnHelp:HANDLE IN FRAME schedulerFrame).
      CtrlFrame-2:MOVE-AFTER(CtrlFrame).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Scheduler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Scheduler */
DO:
  {{&includes}/{&Board}/closeBoard.i}
  RUN closePopups.
  IF VALID-HANDLE(pHandle) THEN
  DELETE PROCEDURE pHandle.
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  IF SEARCH('nosweat.r') EQ ? THEN
  UNLOAD progressINI.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-RESIZED OF W-Win /* Scheduler */
DO:
  RUN winReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAbout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAbout W-Win
ON CHOOSE OF btnAbout IN FRAME schedulerFrame
DO:
  RUN {&prompts}/about.w ('{&Board}',ID,THIS-PROCEDURE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAutoMonitor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAutoMonitor W-Win
ON CHOOSE OF btnAutoMonitor IN FRAME schedulerFrame
DO:
  {{&includes}/{&Board}/btnAutoMonitor.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBringForward
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBringForward W-Win
ON CHOOSE OF btnBringForward IN FRAME schedulerFrame
DO:
  {{&includes}/{&Board}/bringForward.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCapacityView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCapacityView W-Win
ON CHOOSE OF btnCapacityView IN FRAME schedulerFrame
DO:
  RUN closePopups.
  RUN capacityView in h_board.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnColumns W-Win
ON CHOOSE OF btnColumns IN FRAME schedulerFrame
DO:
  RUN dynColumns IN h_board.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDataCollection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDataCollection W-Win
ON CHOOSE OF btnDataCollection IN FRAME schedulerFrame
DO:
  MESSAGE
    'Import Job Data Collection?'
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE asiDC AS LOGICAL.
  IF asiDC THEN
  RUN asiDC IN h_board.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHelp W-Win
ON CHOOSE OF btnHelp IN FRAME schedulerFrame
DO:
  IF SEARCH("{&startDir}\sbProDoc.docx") NE ? THEN
  OS-COMMAND NO-WAIT VALUE(SEARCH("{&startDir}\sbProDoc.docx")).
  ELSE IF SEARCH("{&startDir}\sbProDoc.doc") NE ? THEN
  OS-COMMAND NO-WAIT VALUE(SEARCH("{&startDir}\sbProDoc.doc")).
  ELSE MESSAGE "Schedule Board Documentation Not Available" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLegend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLegend W-Win
ON CHOOSE OF btnLegend IN FRAME schedulerFrame
DO:
  RUN colorGrid IN h_Board.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveResource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveResource W-Win
ON CHOOSE OF btnMoveResource IN FRAME schedulerFrame
DO:
  RUN moveResource IN h_board.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPackBoard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPackBoard W-Win
ON CHOOSE OF btnPackBoard IN FRAME schedulerFrame
DO:
  {{&includes}/{&Board}/btnPackBoard.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPackResource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPackResource W-Win
ON CHOOSE OF btnPackResource IN FRAME schedulerFrame /* ? */
DO:
  {{&includes}/{&Board}/btnPackResource.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint W-Win
ON CHOOSE OF btnPrint IN FRAME schedulerFrame
DO:
  RUN print IN h_board ('ALL').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReLoad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReLoad W-Win
ON CHOOSE OF btnReLoad IN FRAME schedulerFrame
DO:
  MESSAGE 'Reload Board Jobs from Source?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO UPDATE reloadBoard AS LOGICAL.
  IF reloadBoard THEN DO:
    MESSAGE 'Before Reload, Save Current Board?' VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE saveBoard AS LOGICAL.
    IF saveBoard THEN
    RUN saveBoard IN h_board.
    RUN pReload.
    MESSAGE 'Reload Complete!' VIEW-AS ALERT-BOX.
  END. /* if reloadboard */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSequencer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSequencer W-Win
ON CHOOSE OF btnSequencer IN FRAME schedulerFrame
DO:
  RUN closePopups.
  RUN jobSequencer IN h_board.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUpdates
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdates W-Win
ON CHOOSE OF btnUpdates IN FRAME schedulerFrame
DO:
  RUN updatesPending IN h_board.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame W-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/timeLine.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-2 W-Win OCX.Tick
PROCEDURE CtrlFrame-2.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
    RUN jobMoving IN h_board.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Board W-Win
ON CHOOSE OF MENU-ITEM m_Board /* Board */
DO:
  RUN select-page IN THIS-PROCEDURE (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnAbout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnAbout W-Win
ON CHOOSE OF MENU-ITEM m_btnAbout /* About (Support Contact Info) */
DO:
  APPLY 'CHOOSE' TO btnAbout IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnBringForward
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnBringForward W-Win
ON CHOOSE OF MENU-ITEM m_btnBringForward /* Bring Past Jobs Forward */
DO:
  APPLY 'CHOOSE' TO btnBringForward IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnCalendar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnCalendar W-Win
ON CHOOSE OF MENU-ITEM m_btnCalendar /* Popup Calendar */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnCapacityView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnCapacityView W-Win
ON CHOOSE OF MENU-ITEM m_btnCapacityView /* Capacity View */
DO:
  APPLY 'CHOOSE' TO btnCapacityView IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnColumns W-Win
ON CHOOSE OF MENU-ITEM m_btnColumns /* Browse Columns Order/Report Layouts */
DO:
  APPLY 'CHOOSE' TO btnColumns IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnComplete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnComplete W-Win
ON CHOOSE OF MENU-ITEM m_btnComplete /* Complete Job */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnDatePrompt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnDatePrompt W-Win
ON CHOOSE OF MENU-ITEM m_btnDatePrompt /* Date Prompt On/Off */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnDetail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnDetail W-Win
ON CHOOSE OF MENU-ITEM m_btnDetail /* Detail Windows On/Off */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnFlashlight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnFlashlight W-Win
ON CHOOSE OF MENU-ITEM m_btnFlashlight /* Flashlight On/Off */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnHelp W-Win
ON CHOOSE OF MENU-ITEM m_btnHelp /* Help */
DO:
  APPLY 'CHOOSE' TO btnHelp IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnJobBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnJobBrowse W-Win
ON CHOOSE OF MENU-ITEM m_btnJobBrowse /* Job Browse */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnJobNotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnJobNotes W-Win
ON CHOOSE OF MENU-ITEM m_btnJobNotes /* Job Notes */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnLegend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnLegend W-Win
ON CHOOSE OF MENU-ITEM m_btnLegend /* View Color Legend */
DO:
  APPLY 'CHOOSE' TO btnLegend IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnMoveResource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnMoveResource W-Win
ON CHOOSE OF MENU-ITEM m_btnMoveResource /* Move Resource */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnNextDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnNextDate W-Win
ON CHOOSE OF MENU-ITEM m_btnNextDate /* Next Date */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnNextInterval
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnNextInterval W-Win
ON CHOOSE OF MENU-ITEM m_btnNextInterval /* Next Interval */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnPackBoard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnPackBoard W-Win
ON CHOOSE OF MENU-ITEM m_btnPackBoard /* All Jobs */
DO:
  APPLY 'CHOOSE' TO btnPackBoard IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnPackResource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnPackResource W-Win
ON CHOOSE OF MENU-ITEM m_btnPackResource /* Selected Resource/Job */
DO:
  APPLY 'CHOOSE' TO btnPackResource IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnPending
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnPending W-Win
ON CHOOSE OF MENU-ITEM m_btnPending /* Pending by Job */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnPendingJobs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnPendingJobs W-Win
ON CHOOSE OF MENU-ITEM m_btnPendingJobs /* Pending by Resource */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnPrevDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnPrevDate W-Win
ON CHOOSE OF MENU-ITEM m_btnPrevDate /* Previous Date */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnPrevInterval
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnPrevInterval W-Win
ON CHOOSE OF MENU-ITEM m_btnPrevInterval /* Previous Interval */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnPrint W-Win
ON CHOOSE OF MENU-ITEM m_btnPrint /* Print */
DO:
  APPLY 'CHOOSE' TO btnPrint IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnReLoad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnReLoad W-Win
ON CHOOSE OF MENU-ITEM m_btnReLoad /* Reload Board */
DO:
  APPLY 'CHOOSE' TO btnReLoad IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnJobSeqScan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnJobSeqScan W-Win
ON CHOOSE OF MENU-ITEM m_btnJobSeqScan /* Job Sequence Scan */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnReset W-Win
ON CHOOSE OF MENU-ITEM m_btnReset /* Reset */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnSave W-Win
ON CHOOSE OF MENU-ITEM m_btnSave /* Save */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnSequencer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnSequencer W-Win
ON CHOOSE OF MENU-ITEM m_btnSequencer /* Sequencer */
DO:
  APPLY 'CHOOSE' TO btnSequencer IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnShowDowntime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnShowDowntime W-Win
ON CHOOSE OF MENU-ITEM m_btnShowDowntime /* Show/Hide Downtime */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnTimeLine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnTimeLine W-Win
ON CHOOSE OF MENU-ITEM m_btnTimeLine /* Current Date */
DO:
  RUN menuItem IN h_board (SELF:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnUpdates
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnUpdates W-Win
ON CHOOSE OF MENU-ITEM m_btnUpdates /* Updates Pending */
DO:
  APPLY 'CHOOSE' TO btnUpdates IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_btnUseSequence
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_btnUseSequence W-Win
ON CHOOSE OF MENU-ITEM m_btnUseSequence /* (Don't) Use Job Sequence */
DO:
/*  APPLY 'CHOOSE' TO btnUseSequence IN FRAME {&FRAME-NAME}.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Configuration
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Configuration W-Win
ON CHOOSE OF MENU-ITEM m_Configuration /* Configuration */
DO:
  RUN select-page IN THIS-PROCEDURE (2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Downtime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Downtime W-Win
ON CHOOSE OF MENU-ITEM m_Downtime /* Downtime */
DO:
  RUN select-page IN THIS-PROCEDURE (3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Exit W-Win
ON CHOOSE OF MENU-ITEM m_Exit /* Exit */
DO:
  APPLY 'WINDOW-CLOSE' TO {&WINDOW-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Resources
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Resources W-Win
ON CHOOSE OF MENU-ITEM m_Resources /* Resources */
DO:
  RUN select-page IN THIS-PROCEDURE (4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

RUN {&prompts}/ID.w (OUTPUT ID).
IF ID EQ '' THEN RETURN.

&IF '{&Board}' EQ 'Pro' &THEN
RUN {&prompts}/inUse.w (ID).
&ENDIF

{{&includes}/startUp.i}

RUN {&prompts}/scheduleID.p PERSISTENT SET pHandle.
RUN setValues IN pHandle (ID,'Actual').
INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/startUp.dat')) NO-ECHO.
IMPORT dontShowStartup boardSize.
INPUT CLOSE.
IF NOT dontShowStartup THEN
RUN {&prompts}/startUp.w.

{{&viewers}/includes/winTitle.i}
winTitle = {&WINDOW-NAME}:TITLE.

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME schedulerFrame:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Board|Config Opts|Downtime|Resources' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 28.48 , 160.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             btnUpdates:HANDLE IN FRAME schedulerFrame , 'BEFORE':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
           &IF DEFINED(UIB_is_Running) ne 0 &THEN
             INPUT  'schedule/objects/board.w':U ,
           &ELSE
             INPUT boardObject ,
           &ENDIF
             INPUT  FRAME schedulerFrame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_board ).
       RUN set-position IN h_board ( 2.43 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 26.76 , 157.40 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_board ,
             btnHelp:HANDLE IN FRAME schedulerFrame , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'schedule/objects/config.w':U ,
             INPUT  FRAME schedulerFrame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_config ).
       RUN set-position IN h_config ( 2.43 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 26.48 , 152.40 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_config ,
             btnHelp:HANDLE IN FRAME schedulerFrame , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'schedule/objects/downtime.w':U ,
             INPUT  FRAME schedulerFrame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_downtime ).
       RUN set-position IN h_downtime ( 2.43 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 26.76 , 157.40 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_downtime ,
             btnHelp:HANDLE IN FRAME schedulerFrame , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'schedule/objects/resources.w':U ,
             INPUT  FRAME schedulerFrame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_resources ).
       RUN set-position IN h_resources ( 2.91 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 25.71 , 92.00 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_resources ,
             btnHelp:HANDLE IN FRAME schedulerFrame , 'AFTER':U ).
    END. /* Page 4 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asiCommaList W-Win 
PROCEDURE asiCommaList :
/*------------------------------------------------------------------------------
  Purpose:     comma delimited list of values gotten from startUp.p
  Parameters:  output requested value
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipValue AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opValue AS CHARACTER NO-UNDO.

  DEFINE VARIABLE listValues AS CHARACTER NO-UNDO.

  ASSIGN
    listValues = 'Company,Location'
    opValue = ENTRY(LOOKUP(ipValue,listValues),commaList)
    .
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkPopup W-Win 
PROCEDURE checkPopup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opRunning AS LOGICAL NO-UNDO.

  opRunning = VALID-HANDLE(popup[ipIdx]).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE closePopups W-Win 
PROCEDURE closePopups :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = 1 TO EXTENT(popup):
    IF VALID-HANDLE(popup[i]) THEN
    DELETE PROCEDURE popup[i].
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load W-Win  _CONTROL-LOAD
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

OCXFile = SEARCH( "schedule\objects\schedule.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
  .
  RUN DISPATCH IN THIS-PROCEDURE("initialize-controls":U) NO-ERROR.
END.
ELSE MESSAGE "schedule\objects\schedule.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayLastSave W-Win 
PROCEDURE displayLastSave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipLastSave AS INTEGER NO-UNDO.

  lastSave:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INTEGER(ipLastSave),'HH:MM:SS').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY packText lastSave 
      WITH FRAME schedulerFrame IN WINDOW W-Win.
  ENABLE btnAutoMonitor btnColumns btnSequencer btnMoveResource btnPrint 
         btnReLoad btnPackResource btnPackBoard btnBringForward btnAbout 
         btnHelp 
      WITH FRAME schedulerFrame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-schedulerFrame}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hoganPopup W-Win 
PROCEDURE hoganPopup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipEnable AS LOGICAL NO-UNDO.

  chCtrlFrame-2:PSTimer:Enabled = ipEnable.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadConfiguration W-Win 
PROCEDURE loadConfiguration :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN select-page IN THIS-PROCEDURE (1).
  RUN loadConfiguration IN h_board.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadDowntime W-Win 
PROCEDURE loadDowntime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN select-page IN THIS-PROCEDURE (1).
  RUN loadDowntime IN h_board.
  RUN loadBoard IN h_board (YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage AS INTEGER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {{&includes}/{&Board}/localChangePage.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE configFrameW AS DECIMAL NO-UNDO.
  DEFINE VARIABLE configFrameH AS DECIMAL NO-UNDO.
  DEFINE VARIABLE configW AS DECIMAL NO-UNDO.
  DEFINE VARIABLE configH AS DECIMAL NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE startHeight AS INTEGER NO-UNDO.
  DEFINE VARIABLE startWidth AS INTEGER NO-UNDO.
  DEFINE VARIABLE version AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
  IMPORT version.
  INPUT CLOSE.
  IF version LT "{&version1}" THEN DO:
    MESSAGE
        "Schedule Board Configuration Release" version "is Outdated." SKIP 
        "Auto Correct to Release {&version}?" SKIP(1)
        "Warning: Auto Correct will not preserve any previous Configuration Values!!!"
    VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE "Invalid SB Release"
    UPDATE lContinue.
    IF lContinue THEN
    OS-COPY VALUE(SEARCH('{&data}/_config.dat')) VALUE(SEARCH('{&data}/' + ID + '/config.dat')).
    ELSE DO:
        APPLY 'CLOSE':U TO THIS-PROCEDURE.
        RETURN.
    END.
  END.
&IF DEFINED(UIB_is_Running) EQ 0 &THEN
  IF NOT continue THEN RETURN.
&ENDIF

  IF boardSize EQ 'Maximize' THEN
  ASSIGN
    {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS = 8000
    {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = 8000
    {&WINDOW-NAME}:WINDOW-STATE = 1.
  ELSE DO:
    IF boardSize EQ 'Minimum' THEN
    ASSIGN
      startWidth = 750
      startHeight = 504.
    ELSE
    ASSIGN
      startWidth = INTEGER(SUBSTR(boardSize,1,INDEX(boardSize,'x') - 1))
      startHeight = INTEGER(SUBSTR(boardSize,INDEX(boardSize,'x') + 1)).
    ASSIGN
      {&WINDOW-NAME}:X = 0
      {&WINDOW-NAME}:Y = 0
      {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS = startwidth
      {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = startHeight
      {&WINDOW-NAME}:WIDTH-PIXELS = startwidth
      {&WINDOW-NAME}:HEIGHT-PIXELS = startHeight.
  END.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF
  RUN select-page IN THIS-PROCEDURE (4).
  RUN containerHandle IN h_resources (THIS-PROCEDURE:HANDLE,'{&Board}').
  RUN select-page IN THIS-PROCEDURE (3).
  RUN containerHandle IN h_downtime (THIS-PROCEDURE:HANDLE,'{&Board}').
  RUN select-page IN THIS-PROCEDURE (2).
  RUN containerHandle IN h_config (THIS-PROCEDURE:HANDLE,'{&Board}').
  RUN select-page IN THIS-PROCEDURE (1).
  RUN containerHandle IN h_board (THIS-PROCEDURE:HANDLE,h_downtime:HANDLE).
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (0,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = FALSE.
&ENDIF
  RUN winReSize.
  RUN getSize IN h_config (OUTPUT configFrameH,OUTPUT configFrameW).
  ASSIGN
    configH = (FRAME {&FRAME-NAME}:HEIGHT-CHARS - configFrameH) / 2 + 2
    configW = (FRAME {&FRAME-NAME}:WIDTH-CHARS - configFrameW) / 2 + 1.
  IF configH + configFrameH LE FRAME {&FRAME-NAME}:HEIGHT-CHARS AND
     configW + configFrameW LE FRAME {&FRAME-NAME}:WIDTH-CHARS THEN
  RUN set-position IN h_config (configH,configW) NO-ERROR.
  IF '{&Board}' NE 'Pro' THEN
  DO WITH FRAME {&FRAME-NAME}:
    HIDE btnAutoMonitor {&boardButtons} lastSave NO-PAUSE.
    RUN setMenuItems (NO).
  END.
  ELSE
  ASSIGN
    i = ETIME(YES)
    btnDataCollection:HIDDEN = NOT CONNECTED('emptrack')
    btnDataCollection:SENSITIVE = CONNECTED('emptrack').
  &IF '{&Board}' EQ 'Basic' &THEN
  HIDE btnCapacityView NO-PAUSE.
  &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveBoardObjects W-Win 
PROCEDURE moveBoardObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipX AS INTEGER NO-UNDO.

  DEFINE VARIABLE pWidget AS WIDGET-HANDLE NO-UNDO.

  ASSIGN
    pWidget = FRAME {&FRAME-NAME}:HANDLE
    pWidget = pWidget:FIRST-CHILD
    pWidget = pWidget:FIRST-CHILD.
  DO WHILE VALID-HANDLE(pWidget):
    IF pWidget:PRIVATE-DATA EQ 'boardObject' THEN
    pWidget:X = pWidget:X + ipX.
    pWidget = pWidget:NEXT-SIBLING.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mxpCommaList W-Win 
PROCEDURE mxpCommaList :
/*------------------------------------------------------------------------------
  Purpose:     comma delimited list of values gotten from startUp.p
  Parameters:  output requested value
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipValue AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opValue AS CHARACTER NO-UNDO.

  DEFINE VARIABLE listValues AS CHARACTER NO-UNDO.

  ASSIGN
    listValues = 'loginControl'
    opValue = ENTRY(LOOKUP(listValues,ipValue),commaList).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE objectName W-Win 
PROCEDURE objectName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/{&Board}/objectName.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFromPending W-Win 
PROCEDURE pFromPending :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN pFromPending IN h_board.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFromPendingByDueDate W-Win 
PROCEDURE pFromPendingByDueDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN pFromPendingByDueDate IN h_board.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReload W-Win 
PROCEDURE pReload :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN getLoginID IN h_board (OUTPUT lvLoginID).
    OS-DELETE VALUE(SEARCH('{&data}/' + ID + '/inUse.' + lvLoginID + '.dat')).
    RUN loadBoard IN h_board (YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveBoard W-Win 
PROCEDURE saveBoard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE saveBoard AS LOGICAL NO-UNDO.

  MESSAGE 'Because Changes to Downtime Require the Board' SKIP
          'to Reload, Do You Wish to Save the Current Board?'
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE saveBoard.
  IF saveBoard THEN
  RUN saveBoard IN h_board.
  RUN getLoginID IN h_board (OUTPUT lvLoginID).
  OS-DELETE VALUE(SEARCH('{&data}/' + ID + '/inUse.' + lvLoginID + '.dat')).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAutoMonitorImage W-Win 
PROCEDURE setAutoMonitorImage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    btnAutoMonitor:LOAD-IMAGE('{&images}/sign_forbidden.gif') IN FRAME {&FRAME-NAME}.
    autoMonitorImage = 0.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setMenuItems W-Win 
PROCEDURE setMenuItems :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipSetting AS LOGICAL NO-UNDO.

  ASSIGN
    MENU-ITEM m_btnBringForward:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnColumns:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnComplete:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnDatePrompt:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnDetail:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnJobBrowse:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnJobNotes:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnFlashLight:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnMoveResource:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnPending:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnPendingJobs:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnPrint:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnReLoad:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnSequencer:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnShowDowntime:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnUpdates:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    MENU-ITEM m_btnUseSequence:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    SUB-MENU m_Pack:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting
    SUB-MENU m_Scenario:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = ipSetting.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPopup W-Win 
PROCEDURE setPopup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER idx AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipHandle AS HANDLE NO-UNDO.

  popup[idx] = ipHandle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE winReSize W-Win 
PROCEDURE winReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE offSet AS INTEGER NO-UNDO.

  offSet = IF {&WINDOW-NAME}:HEIGHT-PIXELS GT 600 THEN 30 ELSE 0.
  IF {&WINDOW-NAME}:HEIGHT-PIXELS LT 600 THEN {&WINDOW-NAME}:HEIGHT-PIXELS = 600.
  IF {&WINDOW-NAME}:WIDTH-PIXELS LT 800 THEN {&WINDOW-NAME}:WIDTH-PIXELS = 800.
  ASSIGN
    {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - offSet
    {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS.
  IF reSized THEN RETURN.
  reSized = YES.
  RUN moveBoardObjects (FRAME {&FRAME-NAME}:WIDTH-PIXELS - 800).
  RUN set-size IN h_folder (FRAME {&FRAME-NAME}:HEIGHT-CHARS - .19,FRAME {&FRAME-NAME}:WIDTH-CHARS).
  RUN setSize IN h_board (FRAME {&FRAME-NAME}:HEIGHT-PIXELS,FRAME {&FRAME-NAME}:WIDTH-PIXELS).
  RUN setSize IN h_downtime (FRAME {&FRAME-NAME}:HEIGHT-PIXELS,FRAME {&FRAME-NAME}:WIDTH-PIXELS).
  RUN initResources IN h_resources.
  ENABLE btnCapacityView btnLegend WITH FRAME {&FRAME-NAME}.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION updatesPending W-Win 
FUNCTION updatesPending RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  &IF '{&Board}' EQ 'Pro' &THEN
  IF SEARCH('{&updates}\' + ID + '\updatesPending.dat') NE ? THEN
  DO:
    ASSIGN
      updatesPending = IF updatesPending NE '' THEN '' ELSE ' (Updates Pending)'
      MENU-ITEM m_btnUpdates:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = YES.
    ENABLE btnUpdates WITH FRAME {&FRAME-NAME}.
  END.
  ELSE
  DO:
    ASSIGN
      updatesPending = ''
      MENU-ITEM m_btnUpdates:SENSITIVE IN MENU POPUP-MENU-{&WINDOW-NAME} = NO.
    DISABLE btnUpdates WITH FRAME {&FRAME-NAME}.
  END.
  &ENDIF
  RETURN updatesPending.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

