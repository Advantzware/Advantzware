&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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
DEF VAR cUserList AS CHAR NO-UNDO.
DEF VAR ictr AS INT NO-UNDO.
DEF VAR jCtr AS INT NO-UNDO.
DEF VAR lMonitor AS LOG NO-UNDO.
DEF VAR hqRecKey AS HANDLE NO-UNDO.
DEF VAR hbRecKey AS HANDLE NO-UNDO.
DEF VAR hbhRecKey1 AS HANDLE NO-UNDO.
DEF VAR hbhRecKey2 AS HANDLE NO-UNDO.
DEF VAR cName AS CHAR NO-UNDO.
DEF VAR cKeyString AS CHAR NO-UNDO.
DEF VAR iLockCountSel AS INT NO-UNDO.
DEF VAR iLockCountAll AS INT NO-UNDO.
DEF VAR iTransCountSel AS INT NO-UNDO.
DEF VAR iTransCountAll AS INT NO-UNDO.
DEF VAR hQuery AS HANDLE NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hBufField AS HANDLE NO-UNDO EXTENT 17.
DEF VAR hBrowse AS HANDLE NO-UNDO.
DEF VAR cExportFile AS CHAR NO-UNDO FORMAT "x(60)".
DEF VAR cRecKey AS CHAR NO-UNDO.
DEF VAR lShowInstructions AS LOG NO-UNDO INITIAL TRUE.
DEF VAR hThisUser AS HANDLE NO-UNDO.
DEF VAR cThisUser AS CHAR NO-UNDO.
DEF VAR lExclusive AS LOG NO-UNDO.
DEF VAR cMyUser AS CHAR NO-UNDO.
DEF VAR iMyLevel AS INT NO-UNDO.
DEF VAR lTraceOn AS LOG NO-UNDO.


{src/adm2/widgetprto.i}

DEF TEMP-TABLE ttLocks
    FIELD ttfFlags AS CHAR LABEL "Type"
    FIELD ttfLock-userid AS CHAR LABEL "User ID" FORMAT "x(12)"
    FIELD ttfLock-name AS CHAR LABEL "Name" FORMAT "x(25)"
    FIELD ttfLock-Tty AS CHAR LABEL "Terminal" FORMAT "x(13)"
    FIELD ttfTable AS CHAR LABEL "Table" FORMAT "x(12)"
    FIELD ttfDispTime AS CHAR LABEL "Duration" FORMAT "x(10)"
    FIELD ttfFields AS CHAR  LABEL "Fields" FORMAT "x(32)"
    FIELD ttfValues AS CHAR LABEL "Values" FORMAT "x(32)"

    FIELD ttfUserID AS CHAR LABEL "User ID"
    FIELD ttfLock-Id AS INT64 LABEL "Lock ID"
    FIELD ttfLock-Usr AS INT LABEL "Lock User#"
    FIELD ttfLock-Table AS INT LABEL "Table#"
    FIELD ttfLock-RecId AS RECID LABEL "Recid" FORMAT ">>>>>>>>>>>9"
    FIELD ttfLockTime AS DATETIME-TZ LABEL "Lock Time"
    FIELD ttfDuration AS INT LABEL "Duration"
    FIELD ttfLock-Type AS CHAR LABEL "Lk Type"
    .
DEF VAR cLabels AS CHAR INITIAL 
    "Type,User ID,Name,Terminal,Table,Duration,Fields,Values,User ID,Lock ID,Lock User#,Table#,Recid,Lock Time,Duration,Lk Type".
    
DEF TEMP-TABLE ttLocks2
    LIKE ttLocks.
    
DEF TEMP-TABLE ttLockUsers
    FIELD ttfUserID AS CHAR 
    FIELD ttfLockCount AS INT
    FIELD ttfExclusive AS INT 
    FIELD ttfShared AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rLocks rTrans rStatus bStartStop bRefresh ~
bGetFile bExport bInstructions bExit fiLK fiTX fiST eInstructions fiUserID ~
fiDuration fiTable tbExclusive fiRefresh fiExportFile tbAutoRun 
&Scoped-Define DISPLAYED-OBJECTS fiLK fiTX fiST fiLockCount fiTxnCount ~
fiStatus fiInstructions eInstructions fiUserID fiDuration fiTable ~
tbExclusive fiRefresh fiExportFile tbAutoRun 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-fMain 
       MENU-ITEM m_Show_User_Stack_Trace LABEL "Show User Stack Trace".


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "Exit" 
     SIZE 8 BY 1.67 TOOLTIP "Exit"
     FONT 6.

DEFINE BUTTON bExport 
     IMAGE-UP FILE "Graphics/32x32/export.ico":U
     LABEL "Export" 
     SIZE 8 BY 1.67 TOOLTIP "Export to a CSV file"
     FONT 6.

DEFINE BUTTON bGetFile 
     IMAGE-UP FILE "Graphics/32x32/elements_tree.ico":U
     LABEL "" 
     SIZE 8 BY 1.67 TOOLTIP "Select a file for export".

DEFINE BUTTON bInstructions 
     IMAGE-UP FILE "Graphics/32x32/book_open.ico":U
     LABEL "" 
     SIZE 8 BY 1.67 TOOLTIP "Show/Hide Instructions".

DEFINE BUTTON bRefresh 
     IMAGE-UP FILE "Graphics/32x32/refresh.ico":U
     LABEL "" 
     SIZE 8 BY 1.67 TOOLTIP "Refresh the list"
     FONT 6.

DEFINE BUTTON bStartStop 
     IMAGE-UP FILE "Graphics/32x32/calendar_clock.ico":U
     LABEL "Stop Monitor" 
     SIZE 8 BY 1.67 TOOLTIP "Start/Stop Monitor"
     FONT 6.

DEFINE VARIABLE eInstructions AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 154 BY 7.14 NO-UNDO.

DEFINE VARIABLE fiDuration AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Duration GT (secs)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE fiExportFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "Export File Location" 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE fiInstructions AS CHARACTER FORMAT "X(256)":U INITIAL "Instructions:" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE fiLK AS CHARACTER FORMAT "X(256)":U INITIAL "LKS" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81 TOOLTIP "Locks"
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiLockCount AS INTEGER FORMAT ">>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .81 NO-UNDO.

DEFINE VARIABLE fiRefresh AS INTEGER FORMAT ">>>>9":U INITIAL 60 
     LABEL "Refresh Interval (secs)" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE fiST AS CHARACTER FORMAT "X(256)":U INITIAL "STS" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81 TOOLTIP "Status"
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiStatus AS CHARACTER FORMAT "X(256)":U INITIAL "Stopped" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .81 TOOLTIP "Status" NO-UNDO.

DEFINE VARIABLE fiTable AS CHARACTER FORMAT "X(256)":U 
     LABEL "TableName Matches" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiTX AS CHARACTER FORMAT "X(256)":U INITIAL "TXN" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81 TOOLTIP "Transactions"
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiTxnCount AS INTEGER FORMAT ">>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .81 TOOLTIP "Transactions" NO-UNDO.

DEFINE VARIABLE fiUserID AS CHARACTER FORMAT "X(256)":U 
     LABEL "User ID Matches" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE rLocks
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 8 BY 1.67 TOOLTIP "Locks"
     BGCOLOR 10 .

DEFINE RECTANGLE rStatus
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 8 BY 1.67 TOOLTIP "Status"
     BGCOLOR 12 .

DEFINE RECTANGLE rTrans
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 8 BY 1.67 TOOLTIP "Transactions"
     BGCOLOR 10 .

DEFINE VARIABLE tbAutoRun AS LOGICAL INITIAL yes 
     LABEL "AutoRun" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.2 BY 1 TOOLTIP "Autorun  Excel when exported" NO-UNDO.

DEFINE VARIABLE tbExclusive AS LOGICAL INITIAL no 
     LABEL "Exclusive locks only" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     bStartStop AT ROW 1.48 COL 104
     bRefresh AT ROW 1.48 COL 113
     bGetFile AT ROW 1.48 COL 122
     bExport AT ROW 1.48 COL 131
     bInstructions AT ROW 1.48 COL 140
     bExit AT ROW 1.48 COL 149
     fiLK AT ROW 1.95 COL 64 COLON-ALIGNED NO-LABEL
     fiTX AT ROW 1.95 COL 73 COLON-ALIGNED NO-LABEL
     fiST AT ROW 1.95 COL 82 COLON-ALIGNED NO-LABEL
     fiLockCount AT ROW 3.14 COL 72 RIGHT-ALIGNED NO-LABEL
     fiTxnCount AT ROW 3.14 COL 72 COLON-ALIGNED NO-LABEL
     fiStatus AT ROW 3.14 COL 81 COLON-ALIGNED NO-LABEL
     fiInstructions AT ROW 3.57 COL 2 COLON-ALIGNED NO-LABEL
     eInstructions AT ROW 4.33 COL 4 NO-LABEL NO-TAB-STOP 
     fiUserID AT ROW 11.95 COL 20 COLON-ALIGNED
     fiDuration AT ROW 11.95 COL 59 COLON-ALIGNED
     fiTable AT ROW 11.95 COL 93 COLON-ALIGNED
     tbExclusive AT ROW 11.95 COL 117
     fiRefresh AT ROW 30.05 COL 27 COLON-ALIGNED
     fiExportFile AT ROW 30.05 COL 63 COLON-ALIGNED
     tbAutoRun AT ROW 30.05 COL 142
     rLocks AT ROW 1.48 COL 65
     rTrans AT ROW 1.48 COL 74
     rStatus AT ROW 1.48 COL 83.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159.8 BY 30.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Lock Monitor"
         HEIGHT             = 30.95
         WIDTH              = 159.8
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 159.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 159.8
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
ASSIGN 
       FRAME fMain:POPUP-MENU       = MENU POPUP-MENU-fMain:HANDLE.

ASSIGN 
       eInstructions:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiInstructions IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiInstructions:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiLockCount IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       fiLockCount:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiStatus IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiStatus:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiTxnCount IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiTxnCount:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 1.71
       COLUMN          = 5
       HEIGHT          = 1.43
       WIDTH           = 7
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(bExit:HANDLE IN FRAME fMain).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Lock Monitor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Lock Monitor */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bExport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bExport wWin
ON CHOOSE OF bExport IN FRAME fMain /* Export */
DO:
    DEF VARIABLE cLineOut AS CHAR.
    OUTPUT TO VALUE(cExportFile).
    PUT UNFORMATTED cLabels + CHR(10).
    FOR EACH ttLocks2:
        EXPORT DELIMITER "," ttLocks2.
    END.
    OUTPUT CLOSE.
    IF tbAutoRun:CHECKED IN FRAME {&frame-name} THEN DO:
        OS-COMMAND SILENT VALUE (cExportFile).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bGetFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bGetFile wWin
ON CHOOSE OF bGetFile IN FRAME fMain
DO:
    SYSTEM-DIALOG GET-FILE cExportFile
        TITLE "Lock Monitor Export File"
        FILTERS "CSV Files (*.csv)" "*.csv"
        CREATE-TEST-FILE
        USE-FILENAME
        DEFAULT-EXTENSION ".csv"
        INITIAL-DIR "C:\tmp".  
    ASSIGN 
        fiExportFile:SCREEN-VALUE = cExportFile.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bInstructions
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bInstructions wWin
ON CHOOSE OF bInstructions IN FRAME fMain
DO:
    DEF VAR hLabel1 AS WIDGET-HANDLE NO-UNDO.
    DEF VAR hLabel2 AS WIDGET-HANDLE NO-UNDO.
    DEF VAR hLabel3 AS WIDGET-HANDLE NO-UNDO.
    DEF VAR hLabel4 AS WIDGET-HANDLE NO-UNDO.

    ASSIGN 
        hLabel1 = fiUserID:SIDE-LABEL-HANDLE  
        hLabel2 = fiDuration:SIDE-LABEL-HANDLE 
        hLabel3 = fiTable:SIDE-LABEL-HANDLE
        .
         
    ASSIGN 
        lShowInstructions = NOT lShowInstructions.
    IF NOT lShowInstructions THEN ASSIGN
        hLabel1:ROW = 4.36
        hLabel2:ROW = 4.36
        hLabel3:ROW = 4.36
        fiInstructions:VISIBLE = FALSE 
        eInstructions:VISIBLE = FALSE
        fiUserID:ROW = 4.36
        fiDuration:ROW = 4.36
        fiTable:ROW = 4.36
        tbExclusive:ROW = 4.36
        .
    ELSE ASSIGN
        hLabel1:ROW = 11.95
        hLabel2:ROW = 11.95
        hLabel3:ROW = 11.95
        fiUserID:ROW = 11.95
        fiDuration:ROW = 11.95
        fiTable:ROW = 11.95
        tbExclusive:ROW = 11.95 
        fiInstructions:VISIBLE = TRUE  
        eInstructions:VISIBLE = TRUE
        . 
        
    RUN pRefresh.
            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bRefresh wWin
ON CHOOSE OF bRefresh IN FRAME fMain
DO:
        RUN pRefresh.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bStartStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bStartStop wWin
ON CHOOSE OF bStartStop IN FRAME fMain /* Stop Monitor */
DO:
    IF lMonitor THEN ASSIGN 
        lMonitor = FALSE 
        fiStatus:SCREEN-VALUE = "Stopped"
        rStatus:BGCOLOR = 12
        fiRefresh:SENSITIVE = TRUE.
    ELSE ASSIGN 
        lMonitor = TRUE 
        fiStatus:SCREEN-VALUE = "Running"
        rStatus:BGCOLOR = 10
        fiRefresh:SENSITIVE = FALSE.
    IF lMonitor THEN 
        RUN pRunMonitor. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
PROCESS EVENTS.
IF lMonitor THEN 
    RUN pRefresh.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRefresh wWin
ON VALUE-CHANGED OF fiRefresh IN FRAME fMain /* Refresh Interval (secs) */
DO:
        chCtrlFrame:PSTimer:Interval = (INTEGER(SELF:SCREEN-VALUE) * 1000).  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Show_User_Stack_Trace
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Show_User_Stack_Trace wWin
ON CHOOSE OF MENU-ITEM m_Show_User_Stack_Trace /* Show User Stack Trace */
DO:
    IF lTraceOn THEN 
        RUN util/dStackTrace.w (cThisUser).
    ELSE MESSAGE 
        "This function is not available for this user." SKIP 
        "To correct, turn on the Track Usage option in" SKIP 
        "User Maintenance (NU3)."
        VIEW-AS ALERT-BOX ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbExclusive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbExclusive wWin
ON VALUE-CHANGED OF tbExclusive IN FRAME fMain /* Exclusive locks only */
DO:
    RUN pRefresh.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWin  _CONTROL-LOAD
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

OCXFile = SEARCH( "wLockMonitor.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wLockMonitor.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY fiLK fiTX fiST fiLockCount fiTxnCount fiStatus fiInstructions 
          eInstructions fiUserID fiDuration fiTable tbExclusive fiRefresh 
          fiExportFile tbAutoRun 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE rLocks rTrans rStatus bStartStop bRefresh bGetFile bExport 
         bInstructions bExit fiLK fiTX fiST eInstructions fiUserID fiDuration 
         fiTable tbExclusive fiRefresh fiExportFile tbAutoRun 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  RUN SUPER.

    /* Non-admin users can only see their own locks */
    ASSIGN 
        cMyUser = USERID(LDBNAME(1)).
    IF cMyUser NE "" THEN DO:
        FIND users NO-LOCK WHERE 
            users.user_id EQ cMyUser
            NO-ERROR.
        IF NOT AVAIL users THEN APPLY 'choose' TO bExit IN FRAME {&frame-name}.
        ASSIGN 
            iMyLevel = INTEGER(users.securityLevel).
        IF iMyLevel LT 900 THEN ASSIGN 
                fiUserID:SCREEN-VALUE IN FRAME {&frame-name} = cMyUser
                fiUserID:SENSITIVE = FALSE.       
    END.
  
    ASSIGN 
        fiExportFile:SCREEN-VALUE IN FRAME {&frame-name} = 
            "c:\tmp\lockmonitor-" + 
            STRING(YEAR(TODAY),"9999") + 
            STRING(MONTH(TODAY),"99") + 
            STRING(DAY(TODAY),"99") + "-" +
            SUBSTRING(STRING(TIME,"HH:MM:SS"),1,2) + 
            SUBSTRING(STRING(TIME,"HH:MM:SS"),4,2) + 
            SUBSTRING(STRING(TIME,"HH:MM:SS"),7,2) + ".csv"
        cExportFile = fiExportFile:SCREEN-VALUE
        eInstructions:SCREEN-VALUE = 
            "This function displays a list of all record locks currently held by your system's users.  Each line represents " +
            "a separate record being locked.  The visible columns include:" + CHR(10) +
            "'Type' - the level of the lock.  These vary in importance:" + CHR(10) +
            "    'EXCL' - other users may not access this record at all until the lock is released" + CHR(10) +
            "    'SHRD' - other users may view this record but not update it until the lock is released" + CHR(10) +
            "    Locks can normally be released by a SAVE action, a CANCEL action, or closing a function." + CHR(10) +
            "'UserID/Name/Terminal' - the user holding the lock and the workstation where the lock was originated" + CHR(10) +
            "'Table/Duration' - the table containing the locked record and the amount of time the lock has been in place" + CHR(10) +
            "    (Note: the duration will be the lesser of the total lock time or the time since you started this program)"  + CHR(10) +
            "'Fields/Values' - information describing the specific record held by this lock" + CHR(10) +
            "You can EXPORT the contents of this list to a CSV file by entering a file name in the box below and pressing the Export button.". 
    
    APPLY 'value-changed' TO fiRefresh.
    APPLY 'entry' TO bRefresh.            
    APPLY 'choose' TO bRefresh.            

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetLockData wWin 
PROCEDURE pGetLockData :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR hIdxFld AS HANDLE EXTENT 50 NO-UNDO.
    DEF VAR cFieldString AS CHAR NO-UNDO.
    DEF VAR iIdxCt AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    DEF VAR kCtr AS INT NO-UNDO.
    DEF VAR hTestName AS HANDLE NO-UNDO.
    DEF VAR cTestName AS CHAR NO-UNDO.
    DEF VAR cDisp AS CHAR NO-UNDO.
    DEF VAR cUserMatch AS CHAR NO-UNDO.
    DEF VAR cTableMatch AS CHAR NO-UNDO.
    
    ASSIGN
        cUserMatch = IF fiUserID:SCREEN-VALUE IN FRAME {&frame-name} EQ "" THEN "*" ELSE fiUserID:SCREEN-VALUE + "*"
        cTableMatch = IF fiTable:SCREEN-VALUE IN FRAME {&frame-name} EQ "" THEN "*" ELSE fiTable:SCREEN-VALUE + "*".
    
    ASSIGN 
        iLockCountAll = 0
        iLockCountSel = 0
        jCtr = 0.

    EMPTY TEMP-TABLE ttLocks.
    
    FOR EACH _lock WHERE
        _lock._lock-usr <> ? AND 
        _lock._lock-recid <> ? NO-LOCK:
        ASSIGN 
            iIdxCt = 0
            cFieldString = ""
            cKeyString = "".     
        FIND FIRST _file WHERE
            _file._file-number = _lock._lock-table
            USE-INDEX _file-number
            NO-LOCK NO-ERROR.
        IF _file._file-name BEGINS "_" 
        OR _file._file-name EQ "users" THEN NEXT.
        FIND _user NO-LOCK WHERE
            _user._userid EQ _lock._lock-name
            NO-ERROR.
        IF AVAIL _file THEN FIND FIRST _index OF _file WHERE 
            _index._unique EQ TRUE  
            NO-LOCK NO-ERROR.
        IF NOT AVAIL _index THEN FIND FIRST _index OF _file WHERE 
                RECID(_index) = _file._prime-index
                NO-LOCK NO-ERROR.
        IF AVAIL _index THEN DO:
            CREATE BUFFER hbRecKey FOR TABLE _file._file-name.
            CREATE QUERY hqRecKey.
            hqRecKey:SET-BUFFERS(hbRecKey).
            hqRecKey:QUERY-PREPARE("preselect each " + _file._file-name + " no-lock where recid(" + _file._file-name + 
                ") = " + STRING(_lock._Lock-RecID)).
            hqRecKey:QUERY-OPEN.
            IF hqRecKey:NUM-RESULTS NE 0 THEN DO:
                hqRecKey:GET-FIRST().
                FOR EACH _index-field OF _index NO-LOCK:
                    FIND FIRST _field OF _index-field NO-LOCK NO-ERROR.
                    IF AVAIL _field 
                    AND _field._extent LT 2 THEN DO:
                        ASSIGN
                            iIdxCt = iIdxCt + 1.
                        checkname:
                        DO jCtr = 1 TO hbRecKey:NUM-FIELDS:
                            ASSIGN 
                                hTestName = hbRecKey:BUFFER-FIELD(jCtr)
                                cTestName = hTestName:NAME.
                            IF cTestName EQ "rec_key" THEN ASSIGN 
                                cRecKey = hTestName:BUFFER-VALUE().
                            IF hTestName:EXTENT GT 1 THEN NEXT.
                            IF cTestName EQ _field._field-name THEN DO:
                                ASSIGN 
                                    hIdxFld[iIdxCt] = hbRecKey:BUFFER-FIELD(jCtr) 
                                    cFieldString = cFieldString + hIdxFld[iIdxCt]:NAME  + "|" 
                                    cKeyString = cKeyString + 
                                                    (IF hIdxFld[iIdxCt]:BUFFER-VALUE EQ "" THEN "' '" ELSE STRING(hIdxFld[iIdxCt]:BUFFER-VALUE)) + "|" 
                                    NO-ERROR.
                                LEAVE checkname.
                            END.
                        END.
                    END.
                END.    
                ASSIGN 
                    cFieldString = TRIM(cFieldString,"|")
                    cKeyString = TRIM(cKeyString,"|").     
            END.                

            CREATE ttLocks.
            ASSIGN
                ttLocks.ttfLock-Id = _lock._Lock-ID
                ttLocks.ttfLock-Usr = _lock._Lock-Usr
                ttLocks.ttfLock-userid = IF AVAIL _user THEN _user._userid ELSE "" 
                ttLocks.ttfLock-Name = _lock._Lock-Name
                ttLocks.ttfLock-Type = _lock._Lock-Type
                ttLocks.ttfLock-Table = _lock._Lock-Table
                ttLocks.ttfLock-RecID = _lock._Lock-RecID
                ttLocks.ttfLock-Tty = _lock._Lock-Tty
                ttLocks.ttfFlags = IF INDEX(_lock._lock-flags,"X") > 0 THEN "EXCL" ELSE
                                   IF INDEX(_lock._lock-flags,"S") > 0 THEN "SHRD" ELSE
                                   IF INDEX(_lock._lock-flags,"U") > 0 THEN "UPGR" ELSE
                                   IF INDEX(_lock._lock-flags,"L") > 0 THEN "LIMB" ELSE
                                   IF INDEX(_lock._lock-flags,"Q") > 0 THEN "QUED" ELSE
                                   ""
                ttLocks.ttfTable = _file._file-name
                ttLocks.ttfLockTime = NOW
                ttLocks.ttfDuration = 0
                ttLocks.ttfDispTime = "00:00:00"
                ttLocks.ttfUserID = _lock._lock-name
                ttLocks.ttfFields = cFieldString
                ttLocks.ttfValues = cKeyString
                .
            DELETE OBJECT hqRecKey.
            DELETE OBJECT hbRecKey.            
        END.
    END.
    
    FOR EACH ttLocks:
        FIND FIRST ttLocks2 WHERE
            ttLocks2.ttfLock-ID EQ ttLocks.ttfLock-ID
            NO-ERROR.
        IF NOT AVAIL ttLocks2 THEN 
        DO:
            CREATE ttLocks2.
            BUFFER-COPY ttLocks TO ttLocks2.
        END.
        ELSE 
        DO:
            ASSIGN
                ttLocks2.ttfDuration = (NOW - ttLocks2.ttfLockTime) / 1000
                ttLocks2.ttfDispTime = STRING(ttLocks2.ttfDuration,"HH:MM:SS").
        END.
    END.
    FOR EACH ttLocks2 WHERE NOT CAN-FIND (FIRST ttLocks WHERE
        ttLocks.ttfLock-ID = ttLocks2.ttfLock-ID):
        DELETE ttLocks2.
    END.

    ASSIGN 
        lExclusive = FALSE.
    FOR EACH ttLocks2 WHERE 
        ttLocks2.ttfLock-userid MATCHES cUserMatch AND 
        ttLocks2.ttfTable MATCHES cTableMatch AND
        ttLocks2.ttfDuration GE INTEGER(fiDuration:SCREEN-VALUE) AND 
        ttLocks2.ttfFlags = "EXCL":
        ASSIGN 
            lExclusive = TRUE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetTransData wWin 
PROCEDURE pGetTransData :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    ASSIGN 
        iCtr = 0
        jCtr = 0
        iTransCountAll = 0
        iTransCountSel = 0.
    
    FOR EACH _trans NO-LOCK WHERE 
        _trans._trans-usrnum NE ?:
        FIND FIRST _user NO-LOCK WHERE 
            _user._user_number = _trans._Trans-Usrnum
            NO-ERROR.
        IF NOT AVAIL _user THEN NEXT.
        ASSIGN 
            iTransCountAll = iTransCountAll + 1
            iTransCountSel = IF CAN-DO(cUserList,_user._userid) THEN iTransCountSel + 1 ELSE iTransCountSel.
    END.
    ASSIGN 
        fiTxnCount:SCREEN-VALUE IN FRAME {&frame-name} = STRING(iTransCountAll)
        rTrans:BGCOLOR = IF iTransCountAll NE 0 THEN 12 ELSE 10.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUser wWin 
PROCEDURE pGetUser :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    hThisUser = hBrowse:GET-BROWSE-COLUMN (2).
    cThisUser = hThisUser:SCREEN-VALUE.
    
    FIND FIRST users NO-LOCK WHERE 
        users.user_id = cThisUser
        NO-ERROR.
    IF AVAIL users 
    AND users.track_usage THEN ASSIGN 
        lTraceOn = TRUE.
    ELSE ASSIGN lTraceOn = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRefresh wWin 
PROCEDURE pRefresh :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR cUserMatch AS CHAR NO-UNDO.
    DEF VAR cTableMatch AS CHAR NO-UNDO.
    DEF VAR iOldStatusColor AS INT NO-UNDO.
    DEF VAR iLockColor AS INT NO-UNDO.
    DEF VAR cFlagMatch AS CHAR NO-UNDO.
    
    ASSIGN
        cFlagMatch = IF tbExclusive:CHECKED IN FRAME {&frame-name} THEN "EXCL" ELSE "*"
        lExclusive = FALSE 
        iOldStatusColor = rStatus:BGCOLOR IN FRAME {&frame-name} 
        rStatus:BGCOLOR = 14 
        cUserMatch = IF fiUserID:SCREEN-VALUE IN FRAME {&frame-name} EQ "" THEN "*" ELSE fiUserID:SCREEN-VALUE + "*"
        cTableMatch = IF fiTable:SCREEN-VALUE IN FRAME {&frame-name} EQ "" THEN "*" ELSE fiTable:SCREEN-VALUE + "*".
    
    RUN pRunMonitor.
    
    IF VALID-HANDLE(hBuffer) THEN DELETE OBJECT hBuffer.
    IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery.
    IF VALID-HANDLE(hBrowse) THEN DELETE OBJECT hBrowse.
    DO iCtr = 1 TO 17:
        IF VALID-HANDLE(hBufField[iCtr]) THEN DELETE OBJECT hBufField[iCtr].
    END.    
    hBuffer = TEMP-TABLE ttLocks2:HANDLE.
    CREATE QUERY hQuery.
    hQuery:ADD-BUFFER (BUFFER ttLocks2:HANDLE).

    hQuery:QUERY-PREPARE ("FOR EACH ttLocks2 WHERE " +
                           "ttLocks2.ttfLock-userid MATCHES '" +
                            STRING(cUserMatch) + 
                           "' AND ttLocks2.ttfTable MATCHES '" + 
                            STRING(cTableMatch) +
                           "' AND ttLocks2.ttfDuration GE " +
                           fiDuration:SCREEN-VALUE +
                           " AND CAN-DO('" +
                            STRING(cFlagMatch) +                           
                           "',ttLocks2.ttfFlags)" +
                           " BY ttLocks2.ttfDuration DESCENDING"
                          ).
    hQuery:QUERY-OPEN().
    
    CREATE BROWSE hBrowse
        ASSIGN 
            FRAME = FRAME {&frame-name}:HANDLE 
            QUERY = hQuery:HANDLE 
            ROW-MARKERS = FALSE
            ROW = IF lShowInstructions THEN 13.47 ELSE 5.79
            COLUMN = 5.00
            WIDTH = 154.00
            HEIGHT = IF lShowInstructions THEN 16.03 ELSE 23.78
            VISIBLE = TRUE 
            READ-ONLY = TRUE 
            SENSITIVE = TRUE
            SEPARATORS = TRUE
        TRIGGERS:
            ON VALUE-CHANGED PERSISTENT RUN pGetUser.
        END TRIGGERS.

    hBrowse:ADD-COLUMNS-FROM (BUFFER ttLocks2:HANDLE).
    hBrowse:NUM-LOCKED-COLUMNS = 2.

    APPLY 'value-changed' TO hBrowse.

    ASSIGN
        iLockColor = IF NOT lExclusive THEN 14 ELSE 12
        iLockColor = IF hQuery:NUM-RESULTS > 0 THEN iLockColor ELSE 10
        rStatus:BGCOLOR = iOldStatusColor
        fiLockCount:SCREEN-VALUE = STRING(hQuery:NUM-RESULTS)
        rLocks:BGCOLOR = iLockColor. 
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunMonitor wWin 
PROCEDURE pRunMonitor :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN pGetTransData.
    RUN pGetLockData.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

