&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
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
DEF VAR lMonitorRunning AS LOG NO-UNDO.
DEF VAR iLockCountSel AS INT NO-UNDO.
DEF VAR iLockCountAll AS INT NO-UNDO.
DEF VAR iTransCountSel AS INT NO-UNDO.
DEF VAR iTransCountAll AS INT NO-UNDO.

{src/adm2/widgetprto.i}

DEF TEMP-TABLE ttLocks
    FIELD ttfLock-Id AS INT64
    FIELD ttfLock-Usr AS INT
    FIELD ttfLock-userid AS CHAR
    FIELD ttfLock-name AS CHAR
    FIELD ttfLock-Type AS CHAR
    FIELD ttfLock-Table AS INT
    FIELD ttfLock-RecId AS RECID
    FIELD ttfLock-Tty AS CHAR
    FIELD ttfUserID AS CHAR
    FIELD ttfRecKey AS CHAR
    FIELD ttfTable AS CHAR
    FIELD ttfLockTime AS DATETIME-TZ
    FIELD ttfDuration AS INT
    FIELD ttfDispTime AS CHAR
    FIELD ttfFields AS CHAR 
    FIELD ttfValues AS CHAR 
    FIELD ttfFlags AS CHAR 
    .
    
DEF TEMP-TABLE ttLocks2
    LIKE ttLocks.

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
&Scoped-define BROWSE-NAME brUsers

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES users

/* Definitions for BROWSE brUsers                                       */
&Scoped-define FIELDS-IN-QUERY-brUsers users.user_id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brUsers 
&Scoped-define QUERY-STRING-brUsers FOR EACH users NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brUsers OPEN QUERY brUsers FOR EACH users NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brUsers users
&Scoped-define FIRST-TABLE-IN-QUERY-brUsers users


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-brUsers}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rTrans RECT-4 rLocks fiRefresh bStartStop ~
bExit fiUserID brUsers fiTransSel fiTransAll fiLocksSel fiLocksAll ~
slLockList 
&Scoped-Define DISPLAYED-OBJECTS fiLUserID fiRefresh fiUserID fiBlank ~
fiLTxns fiTransSel fiTransAll fiLLocks fiLocksSel fiLocksAll slLockList 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bExit AUTO-END-KEY 
     IMAGE-UP FILE "N:/Environments/Devel/Resources/Graphics/32x32/door_exit.ico":U
     LABEL "Exit" 
     SIZE 8 BY 2.14 TOOLTIP "Exit".

DEFINE BUTTON bStartStop 
     LABEL "Stop Monitor" 
     SIZE 27 BY 2.14
     FONT 6.

DEFINE VARIABLE fiBlank AS CHARACTER FORMAT "X(256)":U INITIAL "(leave blank for all)" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiLLocks AS CHARACTER FORMAT "X(256)":U INITIAL "Record Locks:" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE fiLocksAll AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "All Users" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fiLocksSel AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Selected User" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fiLTxns AS CHARACTER FORMAT "X(256)":U INITIAL "Uncommitted Transactions:" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE fiLUserID AS CHARACTER FORMAT "X(256)":U INITIAL "User ID:" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiRefresh AS INTEGER FORMAT ">>>9":U INITIAL 5 
     LABEL "Refresh Interval (secs)" 
     VIEW-AS FILL-IN 
     SIZE 7.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiTransAll AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "All Users" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fiTransSel AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Selected User" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fiUserID AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 128 BY 2.62.

DEFINE RECTANGLE rLocks
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 15 BY 2.14
     BGCOLOR 10 .

DEFINE RECTANGLE rTrans
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 15 BY 2.14
     BGCOLOR 10 .

DEFINE VARIABLE slLockList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     SIZE 124 BY 18.33
     FONT 2 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brUsers FOR 
      users SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brUsers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brUsers wWin _STRUCTURED
  QUERY brUsers NO-LOCK DISPLAY
      users.user_id FORMAT "X(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 24 BY 25 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiLUserID AT ROW 1.48 COL 2.4 NO-LABEL NO-TAB-STOP 
     fiRefresh AT ROW 1.48 COL 56 COLON-ALIGNED
     bStartStop AT ROW 1.48 COL 120
     bExit AT ROW 1.48 COL 149
     fiUserID AT ROW 2.67 COL 1 COLON-ALIGNED NO-LABEL
     fiBlank AT ROW 2.67 COL 22 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     brUsers AT ROW 3.86 COL 3
     fiLTxns AT ROW 4.1 COL 32 NO-LABEL NO-TAB-STOP 
     fiTransSel AT ROW 5.29 COL 51 COLON-ALIGNED NO-TAB-STOP 
     fiTransAll AT ROW 5.29 COL 85 COLON-ALIGNED NO-TAB-STOP 
     fiLLocks AT ROW 7.19 COL 32 NO-LABEL NO-TAB-STOP 
     fiLocksSel AT ROW 8.38 COL 51 COLON-ALIGNED NO-TAB-STOP 
     fiLocksAll AT ROW 8.38 COL 85 COLON-ALIGNED NO-TAB-STOP 
     slLockList AT ROW 10.52 COL 33 NO-LABEL
     rTrans AT ROW 4.1 COL 120
     RECT-4 AT ROW 3.86 COL 29
     rLocks AT ROW 7.19 COL 120
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159.8 BY 28.52.


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
         TITLE              = "Transaction and Lock Monitor"
         HEIGHT             = 28.52
         WIDTH              = 159.8
         MAX-HEIGHT         = 30.48
         MAX-WIDTH          = 159.8
         VIRTUAL-HEIGHT     = 30.48
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
/* BROWSE-TAB brUsers fiBlank fMain */
/* SETTINGS FOR FILL-IN fiBlank IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiBlank:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiLLocks IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiLLocks:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fiLocksAll:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fiLocksSel:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiLTxns IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiLTxns:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiLUserID IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiLUserID:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fiTransAll:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fiTransSel:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brUsers
/* Query rebuild information for BROWSE brUsers
     _TblList          = "asi.users"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = asi.users.user_id
     _Query            is OPENED
*/  /* BROWSE brUsers */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 1.48
       COLUMN          = 68
       HEIGHT          = 1.43
       WIDTH           = 7
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(fiRefresh:HANDLE IN FRAME fMain).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Transaction and Lock Monitor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Transaction and Lock Monitor */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brUsers
&Scoped-define SELF-NAME brUsers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brUsers wWin
ON VALUE-CHANGED OF brUsers IN FRAME fMain
DO:
        ASSIGN 
            fiUserID:SCREEN-VALUE IN FRAME fMain = users.user_id
            cUserList = users.user_id.
        IF NOT lMonitorRunning THEN 
            RUN pRunMonitor.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bStartStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bStartStop wWin
ON CHOOSE OF bStartStop IN FRAME fMain /* Stop Monitor */
DO:
    IF SELF:LABEL = "Stop Monitor" THEN ASSIGN 
        lMonitor = FALSE 
        SELF:LABEL = "Start Monitor"
        fiRefresh:SENSITIVE = TRUE.
    ELSE ASSIGN 
        lMonitor = TRUE 
        SELF:LABEL = "Stop Monitor"
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
    RUN pRunMonitor.

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


&Scoped-define SELF-NAME fiUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUserID wWin
ON VALUE-CHANGED OF fiUserID IN FRAME fMain
DO:
    OPEN QUERY brUsers FOR EACH users NO-LOCK WHERE users.user_id MATCHES SELF:SCREEN-VALUE + "*".
    ASSIGN 
        cUserList = "".
    IF QUERY brUsers:NUM-RESULTS GT 0 THEN DO:
        QUERY brUsers:GET-FIRST.
        /* BROWSE brUsers:REFRESH(). */
        DO ictr = 1 TO QUERY brUsers:NUM-RESULTS:
            ASSIGN 
                cUserList = cUserList + users.user_id + ",".
            QUERY brUsers:GET-NEXT.
        END.
        ASSIGN 
            cUserList = TRIM(cUserList,",").
    END.
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

OCXFile = SEARCH( "wLockTransMonitor.wrx":U ).
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
ELSE MESSAGE "wLockTransMonitor.wrx":U SKIP(1)
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
  DISPLAY fiLUserID fiRefresh fiUserID fiBlank fiLTxns fiTransSel fiTransAll 
          fiLLocks fiLocksSel fiLocksAll slLockList 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE rTrans RECT-4 rLocks fiRefresh bStartStop bExit fiUserID brUsers 
         fiTransSel fiTransAll fiLocksSel fiLocksAll slLockList 
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

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN 
        lMonitor = TRUE.
    IF USERID("asi") NE ? 
        AND USERID("asi") NE "" THEN 
    DO:
        FIND users NO-LOCK WHERE 
            users.user_id EQ USERID("ASI")
            NO-ERROR.
        IF AVAIL users 
            AND users.securityLevel LT 900 THEN 
        DO:
            ASSIGN 
                fiUserID:SCREEN-VALUE IN FRAME FMain = USERID("asi")
                cUserList = USERID("asi")
                fiUserID:SENSITIVE = FALSE 
                BROWSE brUsers:VISIBLE = FALSE.
        END.
        ELSE 
        DO:
            ASSIGN 
                fiUserID:SCREEN-VALUE = USERID("asi").
            APPLY 'value-changed' TO fiUserID.
            APPLY 'choose' TO bStartStop.
        END.
    END.
    ELSE 
    DO:
        ASSIGN 
            fiUserID:SCREEN-VALUE = "".
        APPLY 'value-changed' TO fiUserID.
        APPLY 'choose' TO bStartStop.
    END.

    IF lMonitor THEN 
        RUN pRunMonitor.

    ASSIGN 
        fiTransSel:SCREEN-VALUE = STRING(iTransCountSel)
        fiTransAll:SCREEN-VALUE = STRING(iTransCountAll)
        fiLocksSel:SCREEN-VALUE = STRING(iLockCountSel)
        fiLocksAll:SCREEN-VALUE = STRING(iLockCountAll).
        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipGetLockData wWin 
PROCEDURE ipGetLockData :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR hIdxFld AS HANDLE EXTENT 50 NO-UNDO.
    DEF VAR cFieldString AS CHAR NO-UNDO.
    DEF VAR iIdxCt AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    DEF VAR hTestName AS HANDLE NO-UNDO.
    DEF VAR cTestName AS CHAR NO-UNDO.
    DEF VAR cDisp AS CHAR NO-UNDO.
    
    ASSIGN 
        iLockCountAll = 0
        iLockCountSel = 0
        jCtr = 0.
    FOR EACH _lock WHERE
        _lock._lock-usr <> ? AND 
        _lock._lock-recid <> ? NO-LOCK:
        FIND FIRST _file WHERE
            _file._file-number = _lock._lock-table
            USE-INDEX _file-number
            NO-LOCK NO-ERROR.
        IF _file._file-name BEGINS "_" 
            OR _file._file-name EQ "users" THEN NEXT.
        ASSIGN 
            jCtr = jCtr + 1
            iLockCountAll = iLockCountAll + 1
            iLockCountSel = IF CAN-DO(cUserList,_lock._lock-name) THEN iLockCountSel + 1 ELSE iLockCountSel
            .
    END.
    IF jCtr EQ 0 THEN DO:
        ASSIGN 
            slLockList:LIST-ITEMS IN FRAME fMain = ""
            rLocks:BGCOLOR = 10
            cDisp = FILL(" ",124)
            SUBSTRING(cDisp,1,50) = "No locked records found.".
        slLockList:ADD-LAST(cDisp).
        RETURN.
    END.
    ELSE ASSIGN 
        rLocks:BGCOLOR IN FRAME FMain = 12
        fiLocksAll:SCREEN-VALUE = STRING(iLockCountAll)
        fiLocksSel:SCREEN-VALUE = STRING(iLockCountSel).

    EMPTY TEMP-TABLE ttLocks.
    
    FOR EACH _lock WHERE
        _lock._lock-usr <> ? AND 
        _lock._lock-recid <> ? NO-LOCK:
        ASSIGN 
            iIdxCt = 0.
        FIND FIRST _file WHERE
            _file._file-number = _lock._lock-table
            USE-INDEX _file-number
            NO-LOCK NO-ERROR.
        IF _file._file-name BEGINS "_" 
        OR _file._file-name EQ "users" THEN NEXT.
        FIND _user NO-LOCK WHERE
            _user._userid EQ _lock._lock-name
            NO-ERROR.
        IF NOT CAN-DO(cUserList,_user._userid) THEN NEXT.
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
                hqRecKey:GET-FIRST.
                FOR EACH _index-field OF _index NO-LOCK:
                    FIND FIRST _field OF _index-field NO-LOCK NO-ERROR.
                    IF AVAIL _field THEN DO:
                        ASSIGN
                            iIdxCt = iIdxCt + 1.
                        checkname:
                        DO jCtr = 1 TO hbRecKey:NUM-FIELDS:
                            ASSIGN 
                                hTestName = hbRecKey:BUFFER-FIELD(jCtr)
                                cTestName = hTestName:NAME.
                            IF cTestName EQ _field._field-name THEN DO:
                                ASSIGN 
                                    hIdxFld[iIdxCt] = hbRecKey:BUFFER-FIELD(jCtr) 
                                    cFieldString = cFieldString + hIdxFld[iIdxCt]:LABEL + "|" 
                                    cKeyString = cKeyString + hIdxFld[iIdxCt]:BUFFER-VALUE + "|" 
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
        slLockList:LIST-ITEMS IN FRAME fMain = ""
        cDisp = "Duration Type User ID     Table          Where".
    slLockList:ADD-LAST(cDisp).
    ASSIGN 
        cDisp = FILL("-",124).
    slLockList:ADD-LAST(cDisp).
        
    FOR EACH ttLocks2:
        ASSIGN 
            cDisp = FILL(" ",124)
            SUBSTRING(cDisp,1,8) = ttLocks2.ttfDispTime 
            SUBSTRING(cDisp,10,4) = ttLocks2.ttfFlags
            SUBSTRING(cDisp,15,11) = ttLocks2.ttfLock-Userid
            SUBSTRING(cDisp,27,14) = SUBSTRING(ttLocks2.ttfTable,1,20)
            SUBSTRING(cDisp,42,50) = "FieldName = Value"
            .
        slLockList:ADD-LAST(cDisp).
        DO iCtr = 1 TO NUM-ENTRIES(ttLocks2.ttfFields,"|"):
            ASSIGN 
                cDisp = FILL(" ",124)
                SUBSTRING(cDisp,42,50) = ENTRY(iCtr,ttLocks2.ttfFields,"|") + " = " + ENTRY(iCtr,ttLocks2.ttfValues,"|").
            slLockList:ADD-LAST(cDisp).
        END.
    END.
    IF NOT CAN-FIND(FIRST ttLocks2) THEN 
    DO:
        ASSIGN 
            cDisp = FILL(" ",124)
            SUBSTRING(cDisp,1,50) = "No locked records found.".
        slLockList:ADD-LAST(cDisp).
    END.
                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipGetTransData wWin 
PROCEDURE ipGetTransData :
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
        rTrans:BGCOLOR IN FRAME FMain = IF iTransCountSel GT 0 THEN 12 ELSE 10
        fiTransAll:SCREEN-VALUE = STRING(iTransCountAll)
        fiTransSel:SCREEN-VALUE = STRING(iTransCountSel).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunMonitor wWin 
PROCEDURE pRunMonitor :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipGetTransData.
    RUN ipGetLockData.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

