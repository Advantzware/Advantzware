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

{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{{&includes}/filterVars.i NEW}
{{&viewers}/includes/sharedVars.i NEW}

DEFINE VARIABLE accumLastSave AS INTEGER NO-UNDO.
DEFINE VARIABLE asOfTime AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE updatesPending AS CHARACTER NO-UNDO.
DEFINE VARIABLE useSequence AS LOGICAL NO-UNDO INITIAL YES.
DEFINE VARIABLE winTitle AS CHARACTER NO-UNDO.

SESSION:SET-WAIT-STATE('').
IF LDBNAME(1) NE ? THEN
SESSION:TIME-SOURCE = LDBNAME(1).

{{&includes}/lockWindowUpdate.i}

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
&Scoped-Define ENABLED-OBJECTS btnColumns btnSequencer btnMoveResource ~
btnPrint btnReLoad btnPackResource btnPackBoard btnUseSequence ~
btnBringForward btnAbout btnHelp 
&Scoped-Define DISPLAYED-OBJECTS packText lastSave 

/* Custom List Definitions                                              */
/* boardButtons,buttonList,List-3,List-4,List-5,List-6                  */
&Scoped-define boardButtons btnUpdates btnColumns btnSequencer ~
btnMoveResource btnPrint btnReLoad btnDataCollection btnPackResource ~
btnPackBoard btnUseSequence btnBringForward packText 
&Scoped-define buttonList btnUpdates btnColumns btnSequencer ~
btnMoveResource btnPrint btnReLoad btnDataCollection btnPackResource ~
btnPackBoard btnUseSequence btnBringForward btnCapacityView btnLegend ~
btnAbout btnHelp packText 

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
       MENU-ITEM m_btnRemove    LABEL "Remove"        
       MENU-ITEM m_btnReset     LABEL "Reset"         .

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

DEFINE BUTTON btnUseSequence 
     IMAGE-UP FILE "schedule/images/usesequenceon.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Use Job Sequence Values when Packing"
     FONT 6.

DEFINE VARIABLE lastSave AS CHARACTER FORMAT "X(256)":U INITIAL "99:99:99" 
      VIEW-AS TEXT 
     SIZE 9 BY 1
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE packText AS CHARACTER FORMAT "X(256)":U INITIAL "Pack:" 
      VIEW-AS TEXT 
     SIZE 6 BY 1.1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME schedulerFrame
     btnUpdates AT ROW 1.05 COL 68 HELP
          "Click to Process Updates Pending"
     btnColumns AT ROW 1.05 COL 73 HELP
          "Click to Access Browser Column Order and Report Layouts"
     btnSequencer AT ROW 1.05 COL 78 HELP
          "Click to Access Sequencer"
     btnMoveResource AT ROW 1.05 COL 83 HELP
          "Click to Access Move Resource"
     btnPrint AT ROW 1.05 COL 88 HELP
          "Click to Access Print Utility"
     btnReLoad AT ROW 1.05 COL 93 HELP
          "Click to Reload Board Jobs from Source"
     btnDataCollection AT ROW 1.05 COL 98 HELP
          "Click to Import Data Collection"
     btnPackResource AT ROW 1.05 COL 110 HELP
          "Click to Pack Select Job"
     btnPackBoard AT ROW 1.05 COL 126 HELP
          "Click to Pack All Jobs"
     btnUseSequence AT ROW 1.05 COL 131 HELP
          "Set Use Job Sequence Values when Packing On/Off"
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
     packText AT ROW 1 COL 102 COLON-ALIGNED NO-LABEL
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

&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
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

/* SETTINGS FOR BUTTON btnBringForward IN FRAME schedulerFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnCapacityView IN FRAME schedulerFrame
   NO-ENABLE 2                                                          */
ASSIGN 
       btnCapacityView:HIDDEN IN FRAME schedulerFrame           = TRUE
       btnCapacityView:PRIVATE-DATA IN FRAME schedulerFrame     = 
                "boardObject".

/* SETTINGS FOR BUTTON btnColumns IN FRAME schedulerFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnDataCollection IN FRAME schedulerFrame
   NO-ENABLE 1 2                                                        */
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
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnPackBoard IN FRAME schedulerFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnPackResource IN FRAME schedulerFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnPrint IN FRAME schedulerFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnReLoad IN FRAME schedulerFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnSequencer IN FRAME schedulerFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnUpdates IN FRAME schedulerFrame
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR BUTTON btnUseSequence IN FRAME schedulerFrame
   1 2                                                                  */
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

