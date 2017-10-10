&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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
&SCOPED-DEFINE IN IN FRAME DEFAULT-FRAME
&SCOPED-DEFINE SV SCREEN-VALUE {&IN}
&SCOPED-DEFINE loginProcedure nosweat/login.w
&SCOPED-DEFINE checkUserRecord YES
&SCOPED-DEFINE connectDatabases YES
&SCOPED-DEFINE runAsiLoad YES
&SCOPED-DEFINE createSingleUserPFs YES
&SCOPED-DEFINE execProgram mainmenu.    
&SCOPED-DEFINE checkExpiredLicense YES
&GLOBAL-DEFINE checkUserCount YES

DEF STREAM usrStream.

DEFINE NEW GLOBAL SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_track_usage AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_header_line AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_groups AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE init_menu AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_developer AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_version AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_rec_key AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_pageno AS INTEGER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g_mainmenu AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE g-sharpshooter  AS LOG NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cIniLoc AS CHAR NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cUsrLoc AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_sysdate AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE g_period AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_init AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE g_batch AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE g_batch-rowid AS rowid NO-UNDO.
DEFINE NEW SHARED VARIABLE miscflds_reckey AS CHARACTER.
DEFINE NEW SHARED VARIABLE table_reckey AS CHARACTER.
DEFINE NEW SHARED VARIABLE persistent-handle AS HANDLE.
DEFINE NEW SHARED VARIABLE ListLogic-Handle AS HANDLE.
DEFINE NEW SHARED VARIABLE igsSessionID AS INTEGER.
DEFINE NEW SHARED VARIABLE quit_login AS LOGICAL NO-UNDO.
DEFINE VARIABLE run-proc AS CHARACTER.
DEFINE VARIABLE hsignature AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE phandle AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE is-running AS LOGICAL NO-UNDO.
DEFINE VARIABLE help-page AS INTEGER NO-UNDO.
DEFINE VARIABLE m_id AS CHAR NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE lExit AS LOGICAL NO-UNDO.
DEFINE VARIABLE tslogin-log AS LOGICAL NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE cTsLogin AS CHARACTER NO-UNDO.

DEF VAR cIniFileLoc AS CHAR NO-UNDO.
DEF VAR cUsrFileLoc AS CHAR NO-UNDO.
DEF VAR cVarName AS CHAR EXTENT 100 NO-UNDO.
DEF VAR cVarValue AS CHAR EXTENT 100 NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR iNumUsers AS INT NO-UNDO.
DEF VAR cIniLine AS CHAR NO-UNDO.
DEF VAR cUsrLine AS CHAR NO-UNDO.
DEF VAR lFoundIni AS LOG NO-UNDO.
DEF VAR lFoundUsr AS LOG NO-UNDO.
DEF VAR lCorrupt AS LOG NO-UNDO.
DEF VAR lSysError AS LOG NO-UNDO.
DEF VAR lMakeBackup AS LOG NO-UNDO.
DEF VAR cDbList AS CHAR NO-UNDO.
DEF VAR cDbDirList AS CHAR NO-UNDO.
DEF VAR cDbPortList AS CHAR NO-UNDO.
DEF VAR origPropath AS CHAR NO-UNDO.
DEF VAR connectStatement AS CHAR NO-UNDO.
DEF VAR cRunPgm AS CHAR NO-UNDO.
DEF VAR tslogin-cha AS CHAR NO-UNDO.
DEF VAR hPreRun AS HANDLE.
DEF VAR xDbDir AS CHAR NO-UNDO.
DEF VAR cUsrList AS CHAR NO-UNDO.
DEF VAR lUpdUsr AS LOG NO-UNDO.

/* Server/Host variables */
DEF VAR csitename AS CHAR NO-UNDO.
DEF VAR chostname AS CHAR NO-UNDO.
DEF VAR cdrive AS CHAR NO-UNDO.
DEF VAR ctopDir AS CHAR NO-UNDO.
DEF VAR cmapDir AS CHAR NO-UNDO.
/* Directory Structure */
/* Top Level */
DEF VAR cadminDir AS CHAR NO-UNDO.
DEF VAR cbackupDir AS CHAR NO-UNDO.
DEF VAR ccustDir AS CHAR NO-UNDO.
DEF VAR cdbDir AS CHAR NO-UNDO.
DEF VAR cenvDir AS CHAR NO-UNDO.
DEF VAR cinstallDir AS CHAR NO-UNDO.
DEF VAR cupdateDir AS CHAR NO-UNDO.
/* Admin Level */
DEF VAR cdbAdmin AS CHAR NO-UNDO.
DEF VAR cenvAdmin AS CHAR NO-UNDO.
/* Backup Level */
DEF VAR cdbBackup AS CHAR NO-UNDO.
DEF VAR cpgmBackup AS CHAR NO-UNDO.
DEF VAR cresBackup AS CHAR NO-UNDO.
/* Database Level */
DEF VAR cdbDataDir AS CHAR NO-UNDO.
DEF VAR cdbProdDir AS CHAR NO-UNDO.
DEF VAR cdbShipDir AS CHAR NO-UNDO.
DEF VAR cdbStructDir AS CHAR NO-UNDO.
DEF VAR cdbTestDir AS CHAR NO-UNDO.
/* Environments Level */
DEF VAR cenvProdDir AS CHAR NO-UNDO.
DEF VAR cenvTestDir AS CHAR NO-UNDO.
DEF VAR cenvCustDir AS CHAR NO-UNDO.
DEF VAR cenvOverDir AS CHAR NO-UNDO.
DEF VAR cenvPgmDir AS CHAR NO-UNDO.
DEF VAR cenvResDir AS CHAR NO-UNDO.
DEF VAR cenvSchDir AS CHAR NO-UNDO.
DEF VAR cenvUMenuDir AS CHAR NO-UNDO.
DEF VAR cenvUserDir AS CHAR NO-UNDO.
/* Updates Level */
DEF VAR czipDir AS CHAR NO-UNDO.
DEF VAR cdataUpdDir AS CHAR NO-UNDO.
DEF VAR cstrUpdDir AS CHAR NO-UNDO.
DEF VAR coverDir AS CHAR NO-UNDO.
DEF VAR cprogramsDir AS CHAR NO-UNDO.
DEF VAR cresourceDir AS CHAR NO-UNDO.
/* DBMS variables */
DEF VAR cDLC11 AS CHAR NO-UNDO.
DEF VAR cproVersion AS CHAR NO-UNDO.
/* Database variables */
DEF VAR cprodDbName AS CHAR NO-UNDO.
DEF VAR cprodDbPort AS CHAR NO-UNDO.
DEF VAR cprodDbStFile AS CHAR NO-UNDO.
DEF VAR cshipDBName AS CHAR NO-UNDO.
DEF VAR cshipDBPort AS CHAR NO-UNDO.
DEF VAR cshipDbStFile AS CHAR NO-UNDO.
DEF VAR ctestDBName AS CHAR NO-UNDO.
DEF VAR ctestDbPort AS CHAR NO-UNDO.
DEF VAR ctestDbStFile AS CHAR NO-UNDO.
DEF VAR cdfFilename AS CHAR NO-UNDO.
/* Misc variables */
DEF VAR cdeltaFilename AS CHAR NO-UNDO.
DEF VAR cadminport AS CHAR NO-UNDO.
DEF VAR cmakeBackup AS CHAR NO-UNDO.
/* Login variables */
DEF VAR cenvList AS CHAR NO-UNDO.
DEF VAR cmodeList AS CHAR NO-UNDO.
DEF VAR cpgmList AS CHAR NO-UNDO.
DEF VAR cXenvList AS CHAR NO-UNDO.
DEF VAR cXmodeList AS CHAR NO-UNDO.
DEF VAR cXdbList AS CHAR NO-UNDO.
DEF VAR cXdbPortList AS CHAR NO-UNDO.
DEF VAR cXportList AS CHAR NO-UNDO.

DEF TEMP-TABLE ttUsers
    FIELD ttfPdbname AS CHAR
    FIELD ttfUserID AS CHAR
    FIELD ttfUserAlias AS CHAR
    FIELD ttfEnvList AS CHAR
    FIELD ttfDbList AS CHAR
    FIELD ttfModeList AS CHAR.

PROCEDURE SetCurrentDirectoryW EXTERNAL "KERNEL32.DLL":
   DEFINE INPUT  PARAMETER chrCurDir AS CHARACTER.
   DEFINE RETURN PARAMETER iResult AS LONG.
END PROCEDURE.

PROCEDURE GetLastError EXTERNAL "kernel32.dll":
    DEFINE RETURN PARAMETER iReturnValue AS LONG.
END.

/* Pre-visualization tasks */
ASSIGN
    g_lookup-var = ""
    g_init = yes
    g_sysdate = TODAY
    g_version = "2.1A-8.2A"
    origPropath = PROPATH.
        
/* Find the .ini file containing variables and values */
RUN ipGetIniFile.
IF NOT lFoundIni THEN DO:
    MESSAGE
        "Unable to locate an 'advantzware.ini' file." SKIP
        "Please contact Advantzware Support. Exiting..."
        VIEW-AS ALERT-BOX ERROR.
    QUIT.
END.
ELSE DO:
    /* Read the .ini file and set the name/value pair. */
    RUN ipReadIniFile.
    IF lCorrupt THEN DO:
        MESSAGE
            "The configuration file 'advantzware.ini'" SKIP
            "is incomplete or corrupt. Exiting..."
            VIEW-AS ALERT-BOX ERROR.
        QUIT.
    END.
END.

/* Find the .usr file containing user-level settings */
RUN ipGetUsrFile.
IF NOT lFoundUsr THEN DO:
    MESSAGE
        "Unable to locate an 'advantzware.usr' file." SKIP
        "Please contact Advantzware Support. Exiting..."
        VIEW-AS ALERT-BOX ERROR.
    QUIT.
END.
ELSE DO:
    RUN ipReadUsrFile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-2 fiUserID fiPassword cbMode ~
cbEnvironment cbDatabase Btn_Cancel Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS fiUserID fiPassword cbMode cbEnvironment ~
cbDatabase 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Login" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cbDatabase AS CHARACTER FORMAT "X(256)":U 
     LABEL "Database" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE cbEnvironment AS CHARACTER FORMAT "X(256)":U 
     LABEL "Environment" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "Live","Test" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE cbMode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mode" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "Standard","Touchscreen","Sharpshooter","Monitor" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE fiPassword AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE fiUserID AS CHARACTER FORMAT "X(256)":U 
     LABEL "User ID" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-2
     SIZE 40 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiUserID AT ROW 8.38 COL 15 COLON-ALIGNED WIDGET-ID 4
     fiPassword AT ROW 9.81 COL 15 COLON-ALIGNED WIDGET-ID 6 PASSWORD-FIELD 
     cbMode AT ROW 11.24 COL 15 COLON-ALIGNED WIDGET-ID 10
     cbEnvironment AT ROW 12.67 COL 15 COLON-ALIGNED WIDGET-ID 8
     cbDatabase AT ROW 14.1 COL 15 COLON-ALIGNED WIDGET-ID 18
     Btn_Cancel AT ROW 15.76 COL 3 WIDGET-ID 22
     Btn_OK AT ROW 15.76 COL 27 WIDGET-ID 26
     IMAGE-2 AT ROW 1.48 COL 4 WIDGET-ID 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 46.8 BY 17.19
         CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
         TITLE              = "Login"
         HEIGHT             = 16.86
         WIDTH              = 45.6
         MAX-HEIGHT         = 21.48
         MAX-WIDTH          = 83.2
         VIRTUAL-HEIGHT     = 21.48
         VIRTUAL-WIDTH      = 83.2
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = yes
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
ASSIGN 
       cbDatabase:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Login */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Login */
DO:
  /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    QUIT.
    /* RETURN NO-APPLY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    IF VALID-HANDLE(hPreRun) THEN 
        RUN epDisconnectDB IN hPreRun.
    APPLY 'window-close' TO C-Win.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* Login */
OR RETURN OF fiPassword
OR RETURN OF cbEnvironment
OR RETURN OF cbDatabase
OR RETURN OF cbMode
DO:
    DEF VAR lUserOK AS LOG NO-UNDO.
    DEF VAR lError AS LOG NO-UNDO.
    DEF VAR cMessage AS CHAR NO-UNDO.
    DEF VAR cCmdString AS CHAR NO-UNDO.

/*

    APPLY 'value-changed' TO cbDatabase.
    APPLY 'value-changed' TO cbEnvironment.
    APPLY 'value-changed' TO cbMode.
*/    
    
    IF connectStatement <> "" THEN DO:
        IF VALID-HANDLE(hPreRun) THEN DO:
            RUN epDisconnectDB IN hPreRun.
            RUN epConnectDB IN hPreRun (connectStatement,
                                        ttUsers.ttfUserID,
                                        fiPassword:{&SV},
                                        OUTPUT lError).
        END.
        ELSE DO:
            RUN ipDisconnectDB IN THIS-PROCEDURE.
            RUN ipConnectDB in THIS-PROCEDURE (connectStatement,
                                               ttUsers.ttfUserID,
                                               fiPassword:{&SV},
                                               OUTPUT lError).
            IF CONNECTED(LDBNAME(1)) THEN
                RUN preRun.p PERSISTENT SET hPreRun.
            ELSE DO:
                MESSAGE 
                    "Unable to connect to that database with the" SKIP
                    "credentials supplied.  Please try again."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
        END.
    END.
    ASSIGN
        lUserOK = SETUSERID(ttUsers.ttfUserID,fiPassword:{&SV},LDBNAME(1)).
    IF lUserOK = TRUE THEN DO:
        RUN ipPreRun.
        ASSIGN
            c-Win:VISIBLE = FALSE.
        IF NOT cbMode:{&SV} = "Monitor Users" THEN DO:
            /* Set current dir */
            RUN ipSetCurrentDir (cMapDir + "\" + cEnvDir + "\" + cbEnvironment:{&SV} + "\"). 

            RUN VALUE(cRunPgm).
        END.
        ELSE DO:
            ASSIGN 
                cCmdString = cDLC11 + "\bin\proshut.bat" + " -db " +
                             cDrive + "\" + cTopDir + "\" + cDbDir + "\" + xDbDir + "\" + PDBNAME(1).
            OS-COMMAND VALUE(cCmdString).
        END.
    END.
    ELSE MESSAGE
        "Unable to login with that User ID and Password."
        VIEW-AS ALERT-BOX ERROR.
    quit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbDatabase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbDatabase C-Win
ON VALUE-CHANGED OF cbDatabase IN FRAME DEFAULT-FRAME /* Database */
DO:
    RUN ipChangeDatabase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbEnvironment
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbEnvironment C-Win
ON VALUE-CHANGED OF cbEnvironment IN FRAME DEFAULT-FRAME /* Environment */
DO:
    RUN ipChangeEnvironment.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbMode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbMode C-Win
ON VALUE-CHANGED OF cbMode IN FRAME DEFAULT-FRAME /* Mode */
DO:
    RUN ipChangeMode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUserID C-Win
ON LEAVE OF fiUserID IN FRAME DEFAULT-FRAME /* User ID */
DO:
    FIND FIRST ttUsers NO-LOCK WHERE
        ttUsers.ttfUserID = SELF:{&SV} AND
        ttUsers.ttfPdbName = cbDatabase:{&SV}
        NO-ERROR.
    IF NOT AVAIL ttUsers THEN DO:
        FIND FIRST ttUsers NO-LOCK WHERE
            ttUsers.ttfUserAlias = SELF:{&SV}
            NO-ERROR.
    END.
    IF NOT AVAIL ttUsers THEN DO:
        MESSAGE
            "Unable to locate this user in the advantzware.usr file." SKIP
            "Please contact your system administrator for assistance."
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        ASSIGN
            cbEnvironment:LIST-ITEMS = IF ttUsers.ttfEnvList <> "" THEN ttUsers.ttfEnvList ELSE cEnvList
            cbDatabase:LIST-ITEMS = IF ttUsers.ttfDbList <> "" THEN ttUsers.ttfDbList ELSE cDbList
            cbMode:LIST-ITEMS = IF ttUsers.ttfModeList <> "" THEN ttUsers.ttfModeList ELSE cModeList
            cbEnvironment:SCREEN-VALUE = ENTRY(1,cbEnvironment:LIST-ITEMS)
            cbDatabase:SCREEN-VALUE = ENTRY(1,cbDatabase:LIST-ITEMS)
            cbMode:SCREEN-VALUE = ENTRY(1,cbMode:LIST-ITEMS).
        APPLY 'value-changed' TO cbEnvironment.
        APPLY 'value-changed' to cbDatabase.
        APPLY 'value-changed' TO cbMode.
        IF ttUsers.ttfUserId = "Admin" 
        OR ttUsers.ttfUserId = "ASI"  THEN
            ENABLE cbDatabase WITH FRAME {&FRAME-NAME}.

    END.
        
END.

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

    IMAGE-2:LOAD-IMAGE("asilogosm.jpg").
    ASSIGN
        cbEnvironment:LIST-ITEMS = TRIM(cEnvList,",")
        cbMode:LIST-ITEMS = TRIM(cModeList,",")
        cbDatabase:LIST-ITEMS = TRIM(cdbList,",").
        
    RUN enable_UI.
/*    RUN no-top-bann (C-Win:HWND, YES, 0,0). */
    
    ASSIGN
        cbDatabase:SCREEN-VALUE = ENTRY(1,cbDatabase:LIST-ITEMS)
        cbEnvironment:SCREEN-VALUE = ENTRY(1,cbEnvironment:LIST-ITEMS)
        cbMode:SCREEN-VALUE = ENTRY(1,cbMode:LIST-ITEMS)
        fiUserID:SCREEN-VALUE = OS-GETENV("USERNAME").
        
    APPLY 'value-changed' TO cbEnvironment.
    APPLY 'value-changed' TO cbMode.
    APPLY 'entry' TO fiUserID.
    

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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
  DISPLAY fiUserID fiPassword cbMode cbEnvironment cbDatabase 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE IMAGE-2 fiUserID fiPassword cbMode cbEnvironment cbDatabase Btn_Cancel 
         Btn_OK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChangeDatabase C-Win 
PROCEDURE ipChangeDatabase :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR iLookup AS INT NO-UNDO.
    DEF VAR xdbName AS CHAR NO-UNDO.
    DEF VAR xdbPort AS CHAR NO-UNDO.
    ASSIGN
        connectStatement = ""
        xdbName = cbDatabase:{&SV}
        iLookup = LOOKUP(xdbName,cDbList)
        xDbDir = ENTRY(iLookup,cDbDirList)
        xdbPort = ENTRY(iLookup,cDbPortList).
    IF xdbName <> ""
    AND xdbPort <> "" THEN ASSIGN
        connectStatement = "-db " + xdbName + 
                           " -H " + chostName +
                           " -S " + xdbPort +
                           " -N tcp -ld ASI".
    ELSE DO:
        MESSAGE
            "There is a problem with your database connection list." SKIP
            "Pleasse contact Advantzware support for assistance."
            VIEW-AS ALERT-BOX.
        APPLY 'choose' to btn_Cancel.
        RETURN NO-APPLY.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChangeEnvironment C-Win 
PROCEDURE ipChangeEnvironment :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cTop AS CHAR NO-UNDO.
    DEF VAR preProPath AS CHAR NO-UNDO.
    DEF VAR iLookup AS INT NO-UNDO.
    DEF VAR iCtr AS INT NO-UNDO.
    
    CASE cbEnvironment:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
        WHEN "Prod" THEN DO:
            DO iCtr = 1 TO NUM-ENTRIES(cbDatabase:LIST-ITEMS):
                IF INDEX(ENTRY(iCtr,cbDatabase:LIST-ITEMS),"Prod") <> 0 THEN DO:
                    ASSIGN
                        cbDatabase:SCREEN-VALUE = ENTRY(iCtr,cbDatabase:LIST-ITEMS).
                    LEAVE.
                END.
            END.
        END.
        WHEN "Test" THEN DO:
            DO iCtr = 1 TO NUM-ENTRIES(cbDatabase:LIST-ITEMS):
                IF INDEX(ENTRY(iCtr,cbDatabase:LIST-ITEMS),"Test") <> 0 THEN DO:
                    ASSIGN
                        cbDatabase:SCREEN-VALUE = ENTRY(iCtr,cbDatabase:LIST-ITEMS).
                    LEAVE.
                END.
            END.
        END.
        WHEN "16.6.0" THEN DO:
            DO iCtr = 1 TO NUM-ENTRIES(cbDatabase:LIST-ITEMS):
                IF INDEX(ENTRY(iCtr,cbDatabase:LIST-ITEMS),"166") <> 0 THEN DO:
                    ASSIGN
                        cbDatabase:SCREEN-VALUE = ENTRY(iCtr,cbDatabase:LIST-ITEMS).
                    LEAVE.
                END.
            END.
        END.
        WHEN "16.5.8" OR 
        WHEN "16.5.4" OR 
        WHEN "16.5.0" THEN
        DO:
            DO iCtr = 1 TO NUM-ENTRIES(cbDatabase:LIST-ITEMS):
                IF INDEX(ENTRY(iCtr,cbDatabase:LIST-ITEMS),"165") <> 0 THEN DO:
                    ASSIGN
                        cbDatabase:SCREEN-VALUE = ENTRY(iCtr,cbDatabase:LIST-ITEMS).
                    LEAVE.
                END.
            END.
        END.
    END CASE.
    APPLY 'value-changed' to cbDatabase.

    ASSIGN
        iLookup = LOOKUP(cbEnvironment:{&SV},cEnvList)
        cTop = cMapDir + "\" + cEnvDir + "\" + cbEnvironment:{&SV} + "\"
        preProPath = cMapDir + "\" + cEnvDir + "\" + cbEnvironment:{&SV} + "," +
                     cTop + cenvCustDir + "," +
                     cTop + cenvOverDir + "," +
                     cTop + cenvPgmDir + "," +
                     cTop + cenvResDir + "," +
                     cTop + cenvPgmDir + "\Addon" + "," +
                     cTop + cenvResDir + "\Addon" + ","
        PROPATH = preProPath + origPropath.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChangeMode C-Win 
PROCEDURE ipChangeMode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cModeItem AS CHAR NO-UNDO.
    DEF VAR cPgmItem AS CHAR NO-UNDO.
    DEF VAR iIndex AS INT NO-UNDO.
    
    ASSIGN
        cModeItem = cbMode:{&SV}
        iIndex = LOOKUP(cModeItem,cModeList)
        cPgmItem = ENTRY(iIndex,cPgmList)
        cRunPgm = cPgmItem.
    IF cModeItem EQ "Sharpshooter" OR cModeItem EQ "Addon" THEN
      g-sharpshooter = YES.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConnectDb C-Win 
PROCEDURE ipConnectDb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER cStatement AS CHAR NO-UNDO.
    DEF INPUT PARAMETER cUser AS CHAR NO-UNDO.
    DEF INPUT PARAMETER cPassword AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER lError AS LOG NO-UNDO.

    IF cPassword <> "" THEN

    CONNECT VALUE(cStatement + 
                  " -U " + cUser + 
                  " -P " + cPassword + 
                  " -ct 2") NO-ERROR.
    ELSE
            CONNECT VALUE(cStatement + 
                  " -U " + cUser + 
                  " -ct 2") NO-ERROR.
    IF LDBNAME(1) = ? THEN DO:
        cStatement = REPLACE(cStatement,"-H " + cHostName,"-H LOCALHOST").
        CONNECT VALUE(cStatement + 
                      " -U " + cUser + 
                      " -P " + cPassword) NO-ERROR.
    END.
    IF CONNECTED(LDBNAME(1))
    AND LDBNAME(1) = "ASI" THEN DO:
        CREATE ALIAS nosweat FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS emptrack FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS jobs FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS rfq FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS asihelp FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS asihlp FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS asinos FOR DATABASE VALUE(LDBNAME(1)).
    END.

    ASSIGN
        lError = NOT CONNECTED(LDBNAME(1)).
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDisconnectDB C-Win 
PROCEDURE ipDisconnectDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF CONNECTED(LDBNAME(1)) THEN
        DISCONNECT VALUE(LDBNAME(1)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipGetIniFile C-Win 
PROCEDURE ipGetIniFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
        lFoundIni = FALSE
        cIniFileLoc = "advantzware.ini".
    IF SEARCH(cIniFileLoc) = ? THEN DO WHILE SEARCH(cIniFileLoc) = ?:
        ASSIGN 
            iCtr = iCtr + 1
            cIniFileLoc = "..\" + cIniFileLoc.
        IF iCtr > 4 THEN LEAVE.
    END.
    IF NOT SEARCH(cIniFileLoc) = ? THEN ASSIGN
        file-info:file-name = SEARCH(cIniFileLoc)
        cIniLoc = file-info:full-pathname
        lFoundIni = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipGetUsrFile C-Win 
PROCEDURE ipGetUsrFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
        lFoundUsr = FALSE
        cUsrFileLoc = "advantzware.usr".
    IF SEARCH(cUsrFileLoc) = ? THEN DO WHILE SEARCH(cUsrFileLoc) = ?:
        ASSIGN 
            iCtr = iCtr + 1
            cUsrFileLoc = "..\" + cUsrFileLoc.
        IF iCtr > 4 THEN LEAVE.
    END.
    IF NOT SEARCH(cUsrFileLoc) = ? THEN ASSIGN
        cUsrFileLoc = SEARCH(cUsrFileLoc)
        file-info:file-name = SEARCH(cUsrFileLoc)
        cUsrLoc = file-info:full-pathname
        lFoundUsr = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipPreRun C-Win 
PROCEDURE ipPreRun :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR lOK AS LOG NO-UNDO.

    IF NOT VALID-HANDLE(persistent-handle) THEN
        RUN nosweat/persist.p PERSISTENT SET persistent-handle.
    IF NOT VALID-HANDLE(listlogic-handle) THEN
        RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

    IF cbMode:{&SV} = "Touchscreen" THEN 
        RUN epTouchLogin in hPreRun (OUTPUT tslogin-log).

    RUN epUserRecordCheck IN hPreRun (OUTPUT lOK, OUTPUT g_track_usage).
    IF NOT lOK THEN DO:
    QUIT.
    END.
    
    RUN epUpdateUsrFile IN hPreRun (OUTPUT cUsrList).
    RUN ipUpdUsrFile IN THIS-PROCEDURE (cUsrList).
    RUN epGetUserGroups IN hPreRun (OUTPUT g_groups).
    
    IF fiUserID:{&SV} = "ASI" THEN RUN asiload.p.

    RUN epCheckExpiration IN hPreRun (OUTPUT lOK).
    IF NOT lOK THEN DO:
    QUIT.
    END.
    
    RUN epGetDeveloperList IN hPreRun (OUTPUT g_developer).

    RUN epGetUsercomp IN hPreRun (OUTPUT g_company, OUTPUT g_loc).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipReadIniFile C-Win 
PROCEDURE ipReadIniFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN 
        lCorrupt = FALSE
        iCtr = 1.
    INPUT FROM VALUE(SEARCH(cIniFileLoc)).
    REPEAT:
        IMPORT UNFORMATTED cIniLine.
        IF INDEX(cIniLine,"=") = 0 THEN NEXT.
        ASSIGN
            cVarName[iCtr] = ENTRY(1,cIniLine,"=")
            cVarValue[iCtr] = ENTRY(2,cIniLine,"=")
            iCtr = iCtr + 1.
    END.
    INPUT CLOSE.

    DO iCtr = 1 TO 100:
        IF cVarName[iCtr] = "" THEN LEAVE.
        CASE cVarName[iCtr]:
            WHEN "sitename" THEN ASSIGN csitename = cVarValue[iCtr]. 
            WHEN "hostname" THEN ASSIGN chostname = cVarValue[iCtr]. 
            WHEN "drive" THEN ASSIGN cdrive = cVarValue[iCtr]. 
            WHEN "topDir" THEN ASSIGN ctopDir = cVarValue[iCtr]. 
            WHEN "mapDir" THEN ASSIGN cmapDir = cVarValue[iCtr]. 
            
            WHEN "adminDir" THEN ASSIGN cAdminDir = cVarValue[iCtr].
            WHEN "backupDir" THEN ASSIGN cbackupDir = cVarValue[iCtr]. 
            WHEN "custDir" THEN ASSIGN ccustDir = cVarValue[iCtr]. 
            WHEN "dbDir" THEN ASSIGN cdbDir = cVarValue[iCtr]. 
            WHEN "envDir" THEN ASSIGN cenvDir = cVarValue[iCtr]. 
            WHEN "installDir" THEN ASSIGN cinstallDir = cVarValue[iCtr]. 
            WHEN "updateDir" THEN ASSIGN cupdateDir = cVarValue[iCtr]. 
            
            WHEN "dbAdmin" THEN ASSIGN cdbAdmin = cVarValue[iCtr]. 
            WHEN "envAdmin" THEN ASSIGN cenvAdmin = cVarValue[iCtr]. 
            
            WHEN "dbBackup" THEN ASSIGN cdbBackup = cVarValue[iCtr]. 
            WHEN "pgmBackup" THEN ASSIGN cpgmBackup = cVarValue[iCtr]. 
            WHEN "resBackup" THEN ASSIGN cresBackup = cVarValue[iCtr]. 

            WHEN "dbDataDir" THEN ASSIGN cdbDataDir = cVarValue[iCtr]. 
            WHEN "dbProdDir" THEN ASSIGN cdbProdDir = cVarValue[iCtr]. 
            WHEN "dbShipDir" THEN ASSIGN cdbShipDir = cVarValue[iCtr]. 
            WHEN "dbStructDir" THEN ASSIGN cdbStructDir = cVarValue[iCtr]. 
            WHEN "dbTestDir" THEN ASSIGN cdbTestDir = cVarValue[iCtr]. 

            WHEN "envProdDir" THEN ASSIGN cenvProdDir = cVarValue[iCtr]. 
            WHEN "envTestDir" THEN ASSIGN cenvTestDir = cVarValue[iCtr]. 
            WHEN "envCustDir" THEN ASSIGN cenvCustDir = cVarValue[iCtr]. 
            WHEN "envOverDir" THEN ASSIGN cenvOverDir = cVarValue[iCtr]. 
            WHEN "envPgmDir" THEN ASSIGN cenvPgmDir = cVarValue[iCtr]. 
            WHEN "envResDir" THEN ASSIGN cenvResDir = cVarValue[iCtr]. 
            WHEN "envSchDir" THEN ASSIGN cenvSchDir = cVarValue[iCtr]. 
            WHEN "envUMenuDir" THEN ASSIGN cenvUMenuDir = cVarValue[iCtr]. 
            WHEN "envUserDir" THEN ASSIGN cenvUserDir = cVarValue[iCtr]. 

            WHEN "zipDir" THEN ASSIGN czipDir = cVarValue[iCtr]. 
            WHEN "dataUpdDir" THEN ASSIGN cdataUpdDir = cVarValue[iCtr]. 
            WHEN "strUpdDir" THEN ASSIGN cstrUpdDir = cVarValue[iCtr]. 
            WHEN "overDir" THEN ASSIGN coverDir = cVarValue[iCtr]. 
            WHEN "programsDir" THEN ASSIGN cprogramsDir = cVarValue[iCtr]. 
            WHEN "resourceDir" THEN ASSIGN cresourceDir = cVarValue[iCtr]. 

            WHEN "DLC11" THEN ASSIGN cDLC11 = cVarValue[iCtr]. 
            WHEN "proVersion" THEN ASSIGN cproVersion = cVarValue[iCtr]. 

            WHEN "prodDbName" THEN ASSIGN cprodDbName = cVarValue[iCtr]. 
            WHEN "prodDbPort" THEN ASSIGN cprodDbPort = cVarValue[iCtr]. 
            WHEN "prodDbStFile" THEN ASSIGN cprodDbStFile = cVarValue[iCtr]. 
            WHEN "shipDBName" THEN ASSIGN cshipDBName = cVarValue[iCtr]. 
            WHEN "shipDBPort" THEN ASSIGN cshipDBPort = cVarValue[iCtr]. 
            WHEN "shipDbStFile" THEN ASSIGN cshipDbStFile = cVarValue[iCtr]. 
            WHEN "testDBName" THEN ASSIGN ctestDBName = cVarValue[iCtr]. 
            WHEN "testDbPort" THEN ASSIGN ctestDbPort = cVarValue[iCtr]. 
            WHEN "testDbStFile" THEN ASSIGN ctestDbStFile = cVarValue[iCtr]. 
            WHEN "dfFilename" THEN ASSIGN cdfFilename = cVarValue[iCtr]. 

            WHEN "deltaFilename" THEN ASSIGN cdeltaFilename = cVarValue[iCtr]. 
            WHEN "adminport" THEN ASSIGN cadminport = cVarValue[iCtr]. 
            WHEN "makeBackup" THEN ASSIGN cmakeBackup = cVarValue[iCtr]. 

            WHEN "envList" THEN ASSIGN cenvList = cVarValue[iCtr]. 
            WHEN "modeList" THEN ASSIGN cmodeList = cVarValue[iCtr]. 
            WHEN "pgmList" THEN ASSIGN cpgmList = cVarValue[iCtr]. 
            WHEN "dbList" THEN ASSIGN cdbList = cVarValue[iCtr]. 
            WHEN "dbDirList" THEN ASSIGN cDbDirList = cVarValue[iCtr]. 
            WHEN "dbPortList" THEN ASSIGN cDbPortList = cVarValue[iCtr]. 
            
        END CASE.
    END.
    IF csitename = ""
    OR chostname = ""
    OR cdrive = ""
    OR ctopDir = ""
    OR cmapDir = ""
    OR cadminDir = ""
    OR cbackupDir = ""
    OR ccustDir = ""
    OR cdbDir = ""
    OR cenvDir = ""
    OR cinstallDir = ""
    OR cupdateDir = ""
    OR cdbAdmin = ""
    OR cenvAdmin = ""
    OR cdbBackup = ""
    OR cpgmBackup = ""
    OR cresBackup = ""
    OR cdbDataDir = ""
    OR cdbProdDir = ""
    OR cdbShipDir = ""
    OR cdbStructDir = ""
    OR cdbTestDir = ""
    OR cenvProdDir = ""
    OR cenvTestDir = ""
    OR cenvCustDir = ""
    OR cenvOverDir = ""
    OR cenvPgmDir = ""
    OR cenvResDir = ""
    OR cenvSchDir = ""
    OR cenvUMenuDir = ""
    OR cenvUserDir = ""
    OR czipDir = ""
    OR cdataUpdDir = ""
    OR cstrUpdDir = ""
    OR coverDir = ""
    OR cprogramsDir = ""
    OR cresourceDir = ""
    OR cDLC11 = ""
    OR cproVersion = ""
    OR cprodDbName = ""
    OR cprodDbPort = ""
    OR cprodDbStFile = ""
    OR cshipDBName = ""
    OR cshipDBPort = ""
    OR cshipDbStFile = ""
    OR ctestDBName = ""
    OR ctestDbPort = ""
    OR ctestDbStFile = ""
    OR cdfFilename = ""
    OR cdeltaFilename = ""
    OR cadminport = ""
    OR cmakeBackup = ""
    OR cenvList = ""
    OR cmodeList = "" 
    OR cdbList = ""
    OR cDbDirList = ""
    OR cDbPortList = "" THEN DO:
        ASSIGN
            lCorrupt = TRUE.
        RETURN.
    END.

    ASSIGN
        lmakeBackup = IF cMakeBackup = "Y" THEN TRUE ELSE FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipReadUsrFile C-Win 
PROCEDURE ipReadUsrFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN 
        lCorrupt = FALSE
        iCtr = 1.
    INPUT FROM VALUE(SEARCH(cUsrFileLoc)).
    REPEAT:
        IMPORT UNFORMATTED cUsrLine.
        IF INDEX(cUsrLine,"|") = 0 THEN NEXT.
        CREATE ttUsers.
        ASSIGN
            ttUsers.ttfPdbname = ENTRY(1,cUsrLine,"|")
            ttUsers.ttfUserAlias = ENTRY(2,cUsrLine,"|")
            ttUsers.ttfUserID = ENTRY(3,cUsrLine,"|")
            ttUsers.ttfEnvList = ENTRY(4,cUsrLine,"|")
            ttUsers.ttfDbList = ENTRY(5,cUsrLine,"|")
            ttUsers.ttfModeList = ENTRY(6,cUsrLine,"|")
            iCtr = iCtr + 1.
    END.
    INPUT CLOSE.
    ASSIGN
        iNumUsers = iCtr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetCurrentDir C-Win 
PROCEDURE ipSetCurrentDir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipTgtDir AS CHAR NO-UNDO.
    DEF VAR iResult AS INT NO-UNDO.
    DEF VAR iReturnValue AS INT NO-UNDO.
    
    RUN SetCurrentDirectoryW (INPUT CODEPAGE-CONVERT(ipTgtDir, "UTF-16"), 
                              OUTPUT iResult).

    IF iResult = 0 THEN DO:
        RUN GetLastError (output iReturnValue).
        MESSAGE 
            "Unable to set working directory." SKIP
            "Error code:" iReturnValue 
            VIEW-AS ALERT-BOX.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpdUsrFile C-Win 
PROCEDURE ipUpdUsrFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcUserList AS CHAR NO-UNDO.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR cOutString AS CHAR.
    
    DO iCtr = 1 TO NUM-ENTRIES(ipcUserList):
        FIND FIRST ttUsers WHERE
            ttUsers.ttfPdbname = PDBNAME(1) AND
            ttUsers.ttfUserID = ENTRY(iCtr,ipcUserList)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL ttUsers THEN DO:
            CREATE ttUsers.
            ASSIGN
                lUpdUsr = TRUE
                ttUsers.ttfPdbname = PDBNAME(1)
                ttUsers.ttfUserID = ENTRY(iCtr,ipcUserList)
                ttUsers.ttfUserAlias = ENTRY(iCtr,ipcUserList)
                .
        END.
    END.
    FOR EACH ttUsers WHERE
        ttUsers.ttfPdbname = PDBNAME(1):
        IF LOOKUP(ttUsers.ttfUserID,ipcUserList) = 0 THEN DO:
            DELETE ttUsers.
        END.
    END.
    IF lUpdUsr = TRUE THEN DO:
        OUTPUT STREAM usrStream TO VALUE(cUsrFileLoc).
        FOR EACH ttUsers:
            ASSIGN cOutString = 
                ttUsers.ttfPdbname + "|" +
                ttUsers.ttfUserAlias + "|" + 
                ttUsers.ttfUserID + "|" + 
                ttUsers.ttfEnvList + "|" +
                ttUsers.ttfDbList + "|" +
                ttUsers.ttfModeList.
            PUT STREAM usrStream UNFORMATTED cOutString + CHR(10).
        END.
        OUTPUT STREAM usrStream CLOSE.
    END.
                
    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

