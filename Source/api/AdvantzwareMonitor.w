&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: api/AdvantzwareMonitor.w

  Description:Monitors Advantzware Resources

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:Vishnu Priya

  Created:29/08/19

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
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
DEFINE SHARED VARIABLE cIniLoc AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdAdvantzwareMonitorProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE cColumnHandles            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDLCDir                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAPIIPAddress             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAPIPort                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAdminServerPort          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAppServerPort            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAppServerName            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNameServerName           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNameServerPort           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cServerHostName           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEmailConfigDesc          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResourceName             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResourceType             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cModeList                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMachineIPAddress         AS CHARACTER NO-UNDO.

DEF TEMP-TABLE ttIniFile
    FIELD iPos      AS INTEGER
    FIELD cRaw      AS CHARACTER
    FIELD cVarName  AS CHARACTER
    FIELD cVarValue AS CHARACTER
    INDEX idxPos IS PRIMARY UNIQUE iPos.

/* Fetches API required element from Advantzware.ini */
RUN pFetchAPIElements.

RUN api/AdvantzwareMonitorProcs.p PERSISTENT SET hdAdvantzwareMonitorProcs.

RUN OS_GetIPAddress (
    OUTPUT cMachineIPAddress
    ).

/* Procedure to initialize the configuration variables required for monitoring */
RUN AdvantzwareMonitor_Initialize IN hdAdvantzwareMonitorProcs (
    INPUT cDLCDir,
    INPUT g_company,
    INPUT cAdminServerPort,
    INPUT cAppServerName,
    INPUT cAppServerPort,
    INPUT cNameServerName,
    INPUT cNameServerPort,
    INPUT cAPIIPAddress,
    INPUT cAPIPort
    ).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-10

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES serverResource

/* Definitions for BROWSE BROWSE-10                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-10 fGetResourceType() @ cResourceType ~
fGetResourceName() @ cResourceName serverResource.isActive ~
serverResource.port serverResource.resourceStatus ~
serverResource.statusRemarks serverResource.notified ~
serverResource.refreshFrequency fGetConfigDescription() @ cEmailConfigDesc 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-10 
&Scoped-define QUERY-STRING-BROWSE-10 FOR EACH serverResource NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-10 OPEN QUERY BROWSE-10 FOR EACH serverResource NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-10 serverResource
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-10 serverResource


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-10}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-29 btRefresh btexit BROWSE-10 fiProcess 
&Scoped-Define DISPLAYED-OBJECTS fiProcess 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetConfigDescription C-Win 
FUNCTION fGetConfigDescription RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetResourceName C-Win 
FUNCTION fGetResourceName RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetResourceType C-Win 
FUNCTION fGetResourceType RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btexit 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "" 
     SIZE 11 BY 2.62 TOOLTIP "Exit Window".

DEFINE BUTTON btRefresh 
     IMAGE-UP FILE "Graphics/32x32/refresh.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/refresh_disabled.ico":U
     LABEL "Restart" 
     SIZE 11 BY 2.62 TOOLTIP "Refresh Services Status".

DEFINE BUTTON btStart 
     IMAGE-UP FILE "Graphics/32x32/media_play.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/media_play_disabled.ico":U
     LABEL "Start" 
     SIZE 11 BY 2.62 TOOLTIP "Start Service"
     FONT 6.

DEFINE BUTTON btStop 
     IMAGE-UP FILE "Graphics/32x32/close.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/delete_disabled.ico":U
     LABEL "Stop" 
     SIZE 11 BY 2.62 TOOLTIP "Stop Service"
     FONT 6.

DEFINE VARIABLE fiProcess AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 28.2 BY 1.05
     FGCOLOR 9 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 211 BY 24.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-10 FOR 
      serverResource SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-10 C-Win _STRUCTURED
  QUERY BROWSE-10 NO-LOCK DISPLAY
      fGetResourceType() @ cResourceType COLUMN-LABEL "Resource Type" FORMAT "X(32)":U
            WIDTH 20.4
      fGetResourceName() @ cResourceName COLUMN-LABEL "Name" FORMAT "x(32)":U
            WIDTH 26
      serverResource.isActive FORMAT "Yes/No":U WIDTH 11.2
      serverResource.port COLUMN-LABEL "Listening on port" FORMAT "x(20)":U
            WIDTH 19.2
      serverResource.resourceStatus COLUMN-LABEL "Status" FORMAT "x(20)":U
            WIDTH 11.2
      serverResource.statusRemarks COLUMN-LABEL "Remarks" FORMAT "x(80)":U
            WIDTH 55.2
      serverResource.notified COLUMN-LABEL "Notified" FORMAT "Yes/No":U
            WIDTH 9.2
      serverResource.refreshFrequency COLUMN-LABEL "Refresh Freq(s)" FORMAT ">>>>>>9":U
            WIDTH 18.2
      fGetConfigDescription() @ cEmailConfigDesc COLUMN-LABEL "Email Config" FORMAT "x(24)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 209 BY 23.43
         FONT 5 ROW-HEIGHT-CHARS 1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btStart AT ROW 1.67 COL 5 WIDGET-ID 6
     btStop AT ROW 1.67 COL 22 WIDGET-ID 8
     btRefresh AT ROW 1.67 COL 186 WIDGET-ID 28
     btexit AT ROW 1.67 COL 203.4 WIDGET-ID 12
     BROWSE-10 AT ROW 5.67 COL 5 WIDGET-ID 200
     fiProcess AT ROW 3.1 COL 154 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     "Advantzware Service Resource Monitor" VIEW-AS TEXT
          SIZE 46.8 BY .62 AT ROW 4.71 COL 8.2 WIDGET-ID 36
          FONT 5
     RECT-29 AT ROW 5.05 COL 4 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 216.2 BY 28.57
         BGCOLOR 15 FONT 6 WIDGET-ID 100.


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
         TITLE              = "Advantzware Service Resource Monitor"
         HEIGHT             = 28.57
         WIDTH              = 216.2
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 216.2
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 216.2
         MAX-BUTTON         = no
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
/* BROWSE-TAB BROWSE-10 btexit DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btStart IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btStop IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiProcess:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-10
/* Query rebuild information for BROWSE BROWSE-10
     _TblList          = "ASI.serverResource"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"fGetResourceType() @ cResourceType" "Resource Type" "X(32)" ? ? ? ? ? ? ? no ? no no "20.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"fGetResourceName() @ cResourceName" "Name" "x(32)" ? ? ? ? ? ? ? no ? no no "26" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.serverResource.isActive
"serverResource.isActive" ? "Yes/No" "logical" ? ? ? ? ? ? no ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.serverResource.port
"serverResource.port" "Listening on port" "x(20)" "character" ? ? ? ? ? ? no ? no no "19.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.serverResource.resourceStatus
"serverResource.resourceStatus" "Status" ? "character" ? ? ? ? ? ? no ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.serverResource.statusRemarks
"serverResource.statusRemarks" "Remarks" "x(80)" "character" ? ? ? ? ? ? no ? no no "55.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.serverResource.notified
"serverResource.notified" "Notified" "Yes/No" "logical" ? ? ? ? ? ? no ? no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.serverResource.refreshFrequency
"serverResource.refreshFrequency" "Refresh Freq(s)" ? "integer" ? ? ? ? ? ? no ? no no "18.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"fGetConfigDescription() @ cEmailConfigDesc" "Email Config" "x(24)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-10 */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.38
       COLUMN          = 92.6
       HEIGHT          = 2.62
       WIDTH           = 11
       WIDGET-ID       = 38
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Advantzware Service Resource Monitor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Advantzware Service Resource Monitor */
DO:
    IF VALID-HANDLE(hdAdvantzwareMonitorProcs) THEN
        DELETE PROCEDURE hdAdvantzwareMonitorProcs.

    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE" TO THIS-PROCEDURE.    
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-10
&Scoped-define SELF-NAME BROWSE-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-10 C-Win
ON ROW-DISPLAY OF BROWSE-10 IN FRAME DEFAULT-FRAME
DO:
   DEFINE VARIABLE iColor AS INTEGER NO-UNDO.
    
   iColor = IF AVAILABLE serverResource AND serverResource.resourceStatus MATCHES "*Running" THEN
                 10 /* Green */
             ELSE
                 12. /* Red */
        
   RUN pUpdateColor (
       INPUT iColor
       ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-10 C-Win
ON VALUE-CHANGED OF BROWSE-10 IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
        btStart:SENSITIVE = AVAILABLE serverResource AND 
                            serverResource.resourceStatus EQ "Stopped" AND
                            (serverResource.resourceType  NE "Node" OR
                            (serverResource.resourceType EQ "Node" AND
                             cMachineIPAddress           EQ cAPIIPAddress))
        btStop:SENSITIVE  = AVAILABLE serverResource  AND 
                            serverResource.resourceStatus EQ "Running" AND
                            serverResource.resourceType NE "AdminServer" AND
                            serverResource.resourceType NE "ASI" AND
                            (serverResource.resourceType NE "Node" OR
                            (serverResource.resourceType EQ "Node" AND
                            cMachineIPAddress           EQ cAPIIPAddress))
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btexit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btexit C-Win
ON CHOOSE OF btexit IN FRAME DEFAULT-FRAME
DO:
    IF VALID-HANDLE(hdAdvantzwareMonitorProcs) THEN
        DELETE PROCEDURE hdAdvantzwareMonitorProcs.
    
    APPLY "CLOSE" TO THIS-PROCEDURE.
    
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRefresh C-Win
ON CHOOSE OF btRefresh IN FRAME DEFAULT-FRAME /* Restart */
DO: 
    RUN pInit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btStart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btStart C-Win
ON CHOOSE OF btStart IN FRAME DEFAULT-FRAME /* Start */
DO:

    DEFINE VARIABLE cStartService AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCommand      AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE lSilent       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lNoWait       AS LOGICAL   NO-UNDO.
             
    DEFINE VARIABLE iCurrBrowseRow AS INTEGER NO-UNDO.
  
    SESSION:SET-WAIT-STATE("GENERAL").

    IF AVAILABLE serverResource THEN DO:

        IF SEARCH(serverResource.startService) NE ? THEN DO:
            cCommand = SEARCH(serverResource.startService).

            IF serverResource.resourceType EQ "ASI" THEN DO:
                RUN pGetASIMonitorStartCommand (
                    INPUT  serverResource.name,
                    INPUT  SEARCH(serverResource.startService),
                    OUTPUT cCommand,
                    OUTPUT lSuccess,
                    OUTPUT cMessage
                    ) NO-ERROR.
                IF NOT lSuccess THEN DO:
                    MESSAGE cMessage
                        VIEW-AS ALERT-BOX ERROR.
                    RETURN NO-APPLY.
                END.
            END.

            ASSIGN
                lSilent = TRUE
                lNoWait = FALSE
                .
            
            /* Set Silent and NO-WAIT options to false for starting Node, as command 
               may prompt for additional details in console */ 
            IF serverResource.resourceType EQ "Node" THEN
                ASSIGN
                    lSilent = TRUE 
                    lNoWait = TRUE
                    .
            /* Set Silent to false for starting ASI Monitors, as command 
               may suppress any visual objects in windows opened */
            ELSE IF serverResource.resourceType EQ "ASI" THEN
                ASSIGN
                    lSilent = FALSE
                    lNoWait = TRUE
                    .
            /* Re-starts AdminServer,AppServer, Node, ASI Monitors and NameServer */                                                     
            RUN OS_RunCommand (
                INPUT  cCommand,             /* Command string to run */
                INPUT  "",                   /* File name to write the command output */
                INPUT  lSilent,              /* Run with SILENT option */
                INPUT  lNoWait,              /* Run with NO-WAIT option */
                OUTPUT lSuccess,
                OUTPUT cMessage
                ) NO-ERROR.
            IF ERROR-STATUS:ERROR OR NOT lSuccess THEN
                RETURN.         
        END. 
        ELSE DO:
            cErrorMessage = IF serverResource.resourceType EQ "Node" OR serverResource.resourceType EQ "AdminServer" THEN
                                "Start script [" + serverResource.startService + "] for " + serverResource.resourceType + " is not found"
                            ELSE
                                "Start script [" + serverResource.startService + "] for " + serverResource.resourceType + " [" + serverResource.name + "] is not found"
                            .
            MESSAGE cErrorMessage VIEW-AS ALERT-BOX ERROR
            TITLE "Error".
        END.
    END.
    IF serverResource.resourceType EQ "Node" THEN 
        PAUSE 3 NO-MESSAGE.
        
    SESSION:SET-WAIT-STATE("").
    ASSIGN
        iCurrBrowseRow = BROWSE {&BROWSE-NAME}:FOCUSED-ROW.
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
    BROWSE {&BROWSE-NAME}:SELECT-ROW(iCurrBrowseRow).
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
    APPLY 'choose' TO btRefresh.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btStop C-Win
ON CHOOSE OF btStop IN FRAME DEFAULT-FRAME /* Stop */
DO:
    DEFINE VARIABLE cStopService   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cErrorMessage  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCurrBrowseRow AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCommand       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSilent        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lNoWait        AS LOGICAL   NO-UNDO.

    SESSION:SET-WAIT-STATE("GENERAL").
  
    IF AVAILABLE serverResource THEN DO:
        IF SEARCH(serverResource.stopService) NE ? THEN DO:
            cCommand = SEARCH(serverResource.stopService).
            /* Stops AdminServer, AppServer, NodeServer and NameServer */

            ASSIGN
                lSilent = TRUE
                lNoWait = FALSE
                .

            /* Set Silent and NO-WAIT options to false for stopping Node, as command 
               may prompt for additional details in console */             
            IF serverResource.resourceType EQ "Node" THEN
                ASSIGN
                    lSilent = FALSE
                    lNoWait = FALSE
                    .
            
            RUN OS_RunCommand (
                INPUT  cCommand,             /* Command string to run */
                INPUT  "",                   /* File name to write the command output */
                INPUT  lSilent,              /* Run with SILENT option */
                INPUT  lNoWait,              /* Run with NO-WAIT option */
                OUTPUT lSuccess,
                OUTPUT cMessage
                ) NO-ERROR.
            IF ERROR-STATUS:ERROR OR NOT lSuccess THEN
                RETURN. 
        END.
        ELSE DO:
            cErrorMessage = IF serverResource.resourceType EQ "Node" THEN
                           "Stop script [" + serverResource.stopService + "] for " + serverResource.resourceType + " is not found"
                       ELSE
                           "Stop script [" + serverResource.stopService + "] for " + serverResource.resourceType + " [" + serverResource.name + "] is not found"
                       .
            MESSAGE cErrorMessage VIEW-AS ALERT-BOX ERROR
            TITLE "Error".
        END.
    END.
    
    SESSION:SET-WAIT-STATE("").
    ASSIGN
        iCurrBrowseRow = BROWSE {&BROWSE-NAME}:FOCUSED-ROW.
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
    BROWSE {&BROWSE-NAME}:SELECT-ROW(iCurrBrowseRow).
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
    apply 'choose' to btRefresh.
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

    APPLY 'CHOOSE' TO btRefresh IN FRAME DEFAULT-FRAME.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   FIND FIRST company NO-LOCK 
        WHERE company.company EQ g_company
        NO-ERROR .
   IF AVAILABLE company THEN
   {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                        + " - {&awversion}" + " - " 
                        + STRING(company.name) + " - " + g_loc. 
  RUN enable_UI.
  RUN pInit.
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

OCXFile = SEARCH( "AdvantzwareMonitor.wrx":U ).
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
ELSE MESSAGE "AdvantzwareMonitor.wrx":U SKIP(1)
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
  DISPLAY fiProcess 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-29 btRefresh btexit BROWSE-10 fiProcess 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFetchAPIElements C-Win 
PROCEDURE pFetchAPIElements :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VARIABLE cIniLine    AS CHARACTER NO-UNDO.
    DEF VARIABLE cIniVarList AS CHARACTER NO-UNDO.
    DEF VARIABLE iLine       AS INTEGER   NO-UNDO.
    
    /* Create the temp-table and load var names */
    cIniVarList = "# Setup Variables,DLCDir,hostname,
                   # API Elements,apiIPAddress,apiPort,adminPort,nameServerName,nameServerPort,appServerName,appServerPort,
                   # ASI Login Items,modeList".
    
    EMPTY TEMP-TABLE ttIniFile.
    
    DO iLine = 1 TO NUM-ENTRIES(cIniVarList):
        CREATE ttIniFile.
        ASSIGN
            ttIniFile.iPos     = iLine
            ttIniFile.cVarName = ENTRY(iLine,cIniVarList).
    END.
    
    INPUT FROM VALUE(SEARCH(cIniLoc)).
    REPEAT:
        IMPORT UNFORMATTED cIniLine.
        IF cIniLine BEGINS "#" THEN DO:
            FIND ttIniFile WHERE 
                ttIniFile.cVarName = cIniLine
                NO-ERROR.
            IF AVAILABLE ttIniFile THEN ASSIGN
                ttIniFile.cRaw = cIniLine.
        END.
        ELSE DO:
            FIND ttIniFile WHERE 
                ttIniFile.cVarName = ENTRY(1,cIniLine,"=")
                NO-ERROR.
            IF AVAILABLE ttIniFile THEN 
                ASSIGN
                    ttIniFile.cRaw      = cIniLine
                    ttIniFile.cVarValue = ENTRY(2,cIniLine,"=")
                    .
        END.            
    END.
    INPUT CLOSE.
     
    FOR EACH ttIniFile:
        IF ttIniFile.cVarName EQ "DLCDir" THEN
            cDLCDir = ttIniFile.cVarValue.
        IF ttIniFile.cVarName EQ "adminPort" THEN
            cAdminServerPort = ttIniFile.cVarValue.
        IF ttIniFile.cVarName EQ "AppServerName" THEN
            cAppServerName = ttIniFile.cVarValue.
        IF ttIniFile.cVarName EQ "AppServerPort" THEN
            cAppServerPort = ttIniFile.cVarValue.
        IF ttIniFile.cVarName EQ "NameServerName" THEN
            cNameServerName = ttIniFile.cVarValue.
        IF ttIniFile.cVarName EQ "NameServerPort" THEN
            cNameServerPort = ttIniFile.cVarValue.
        IF ttIniFile.cVarName EQ "apiIPAddress" THEN
            cAPIIPAddress = ttIniFile.cVarValue.
        IF ttIniFile.cVarName EQ "apiPort" THEN
            cAPIPort = ttIniFile.cVarValue.
        IF ttIniFile.cVarName EQ "hostname" THEN
            cServerHostName = ttIniFile.cVarValue.
        IF ttIniFile.cVarName EQ "modeList" THEN
            cModeList = ttIniFile.cVarValue.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit C-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCurrBrowseRow AS INTEGER NO-UNDO.
    DEFINE VARIABLE lIsServer      AS LOGICAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN pValidateClientServer (
        OUTPUT lIsServer
        ).

    IF NOT lIsServer THEN
        RETURN.
        
    RUN pStoreHandles.
    
    ASSIGN
        fiProcess:SCREEN-VALUE = "Refreshing..."
        btRefresh:SENSITIVE    = FALSE
        .
    
    SESSION:SET-WAIT-STATE("GENERAL").

    STATUS DEFAULT "Refreshing Service Resources status. Please wait..." IN WINDOW THIS-PROCEDURE:CURRENT-WINDOW.
    
    RUN AdvantzwareMonitor_UpdateResourceStatus IN hdAdvantzwareMonitorProcs.

    STATUS DEFAULT "Last Refresh Time: " + STRING(TIME, "hh:mm:ss") IN WINDOW THIS-PROCEDURE:CURRENT-WINDOW.
    SESSION:SET-WAIT-STATE("").
    
    ASSIGN
        iCurrBrowseRow = BROWSE {&BROWSE-NAME}:FOCUSED-ROW
        .

    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
    BROWSE {&BROWSE-NAME}:SELECT-ROW(iCurrBrowseRow).
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}. 
    
    ASSIGN
        fiProcess:SCREEN-VALUE = ""
        btRefresh:SENSITIVE    = TRUE
        .
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pStartASIMonitor C-Win
PROCEDURE pGetASIMonitorStartCommand PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to generate ASI start service command 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcResourceName AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCommand      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCommand      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess      AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cCommandParamUserID      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCommandParamPassword    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCommandParamEnvironment AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCommandParamResource    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCommandParamDatabase    AS CHARACTER NO-UNDO.
    
    ASSIGN
        cCommandParamUserID      = "monitor"
        cCommandParamPassword    = "monitor"
        cCommandParamDatabase    = PDBNAME(LDBNAME(1))    /* Fetch physical DB name for "ASI" */ 
        cCommandParamEnvironment = IF NUM-ENTRIES(ipcCommand, "\") GE 3 THEN   /* Fetch environment from batch file name */ 
                                       ENTRY(3, ipcCommand, "\")
                                   ELSE
                                       ""
        .
    
    IF cCommandParamEnvironment EQ "" THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Unable to fetch the environment"
            .
        RETURN.    
    END.
    
    CASE ipcResourceName:
        WHEN "cXML" THEN
            cCommandParamResource = "cXML Monitor".
        WHEN "fgXML" THEN
            cCommandParamResource = "FG XML Monitor".
        WHEN "RelXML" THEN
            cCommandParamResource = "Rel XML Monitor".
        WHEN "RFID" THEN
            cCommandParamResource = "RFID Monitor".
        WHEN "jobXML" THEN
            cCommandParamResource = "Esko Monitor".
        WHEN "Schedule" THEN
            cCommandParamResource = "Schedule Monitor".
        OTHERWISE
            "".
    END.
    
    IF LOOKUP(cCommandParamResource, cModeList) EQ 0 THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Mode '" + cCommandParamResource + "' is not available in available mode list"
            .
        RETURN.        
    END.    
    
    /* Command to execute should have the following arguments
       Command - <bat file to start ASI Monitors>.bat "<param1>,<param2>,<param3>,<param4>,<param5>"
       param1 - ASI User ID
       param2 - Password for the param1 user id
       param3 - Environment to run the ASI Monitor (e.g. Devel, Prod)
       param4 - ASI Monitor Mode name. **Important** Replace any spaces in Mode name with 
                underscore "_", as Progress cannot accept spaces in arguments
       param5 - Database to connect. (e.g. asiDevel, asiCust1) */    
    opcCommand = ipcCommand + ' "' 
               + cCommandParamUserID + ","
               + cCommandParamPassword + ","
               + cCommandParamEnvironment + ","
               + REPLACE(cCommandParamResource," ","_") + ","
               + cCommandParamDatabase
               + '"'.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .               
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pStoreHandles C-Win 
PROCEDURE pStoreHandles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdCurrColHdl AS HANDLE NO-UNDO.
    
    hdCurrColHdl = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
    cColumnHandles = "".
    DO WHILE VALID-HANDLE(hdCurrColHdl):
        ASSIGN 
            cColumnHandles = IF cColumnHandles <> "":U THEN
                                 cColumnHandles + ",":U + STRING(hdCurrColHdl)
                             ELSE
                                 STRING(hdCurrColHdl)
            hdCurrColHdl   = hdCurrColHdl:NEXT-COLUMN
            .
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pupdateColor C-Win 
PROCEDURE pupdateColor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiColor AS INTEGER NO-UNDO.

    DEFINE VARIABLE iNumCols AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCounter AS INTEGER NO-UNDO.
    DEFINE VARIABLE hdCurCol AS HANDLE  NO-UNDO.

    iNumCols = NUM-ENTRIES(cColumnHandles).
 
    DO iCounter = 1 TO iNumCols:
        ASSIGN 
            hdCurCol         = WIDGET-HANDLE(ENTRY(iCounter,cColumnHandles))
            hdCurCol:BGCOLOR = ipiColor.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidateClientServer C-Win 
PROCEDURE pValidateClientServer :
/*------------------------------------------------------------------------------
  Purpose: Validates if the API Monitor is run from a work station
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplIsServer AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cClientHostName AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    cClientHostName = OS-GETENV("COMPUTERNAME").

    IF cClientHostName NE cServerHostName THEN DO:
        MESSAGE "Advantzware Monitor should not be run from workstation"
            VIEW-AS ALERT-BOX ERROR.
        
        ASSIGN
            btStart:SENSITIVE        = FALSE
            btStop:SENSITIVE         = FALSE
            btRefresh:SENSITIVE      = FALSE
            {&BROWSE-NAME}:SENSITIVE = FALSE
            CtrlFrame:SENSITIVE      = FALSE
            .
        RETURN.
    END.  
    
    oplIsServer = TRUE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetConfigDescription C-Win 
FUNCTION fGetConfigDescription RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Function to fetch the email config description
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cConfigDesc AS CHARACTER NO-UNDO.
    
    IF AVAILABLE serverResource THEN DO:
        FIND FIRST emailConfig NO-LOCK
             WHERE emailConfig.configID EQ serverResource.configID
             NO-ERROR.
        IF AVAILABLE emailConfig THEN
            cConfigDesc = emailConfig.description.
    END.
    
    RETURN cConfigDesc.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetResourceName C-Win 
FUNCTION fGetResourceName RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Function to fetch resource name
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cResourceName AS CHARACTER NO-UNDO.
    
    IF AVAILABLE serverResource THEN
        cResourceName = IF serverResource.resourceType EQ "ASI" THEN
                            serverResource.name + " Monitor"
                        ELSE
                            serverResource.name.
    
    RETURN cResourceName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetResourceType C-Win 
FUNCTION fGetResourceType RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Function to fetch resource type
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cResourceType AS CHARACTER NO-UNDO.
    
    IF AVAILABLE serverResource THEN DO:        
        IF serverResource.resourceType EQ "NameServer" THEN
            cResourceType = "-->" + serverResource.resourceType.
        ELSE IF serverResource.resourceType EQ "AppServer" THEN
            cResourceType = "---->" + serverResource.resourceType.
        ELSE
            cResourceType = serverResource.resourceType.
    END.
    
    RETURN cResourceType.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

