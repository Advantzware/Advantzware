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
DEFINE NEW GLOBAL SHARED VARIABLE persistent-handle AS HANDLE.
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
DEFINE VARIABLE origDirectoryName AS CHARACTER NO-UNDO FORMAT "X(256)".
DEFINE VARIABLE intBufferSize    AS INTEGER   NO-UNDO INITIAL 256.
DEFINE VARIABLE intResult        AS INTEGER   NO-UNDO.
DEFINE VARIABLE ptrToString      AS MEMPTR    NO-UNDO.

DEF VAR cVarName AS CHAR EXTENT 100 NO-UNDO.
DEF VAR cVarValue AS CHAR EXTENT 100 NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR jCtr AS INT NO-UNDO.
DEF VAR iNumUsers AS INT NO-UNDO.
DEF VAR cIniLine AS CHAR NO-UNDO.
DEF VAR cUsrLine AS CHAR NO-UNDO.
DEF VAR lConnectAudit AS LOG NO-UNDO.
DEF VAR lFoundIni AS LOG NO-UNDO.
DEF VAR lFoundUsr AS LOG NO-UNDO.
DEF VAR lCorrupt AS LOG NO-UNDO.
DEF VAR lSysError AS LOG NO-UNDO.
DEF VAR lMakeBackup AS LOG NO-UNDO.
DEF VAR origPropath AS CHAR NO-UNDO.
DEF VAR connectStatement AS CHAR NO-UNDO.
DEF VAR cRunPgm AS CHAR NO-UNDO.
DEF VAR tslogin-cha AS CHAR NO-UNDO.
DEF VAR hPreRun AS HANDLE.
DEF VAR xDbDir AS CHAR NO-UNDO.
DEF VAR cUsrList AS CHAR NO-UNDO.
DEF VAR lUpdUsr AS LOG NO-UNDO.
DEF VAR iTries AS INT NO-UNDO.
DEF VAR iLockoutTries AS INT NO-UNDO.
DEF VAR cIniVarList AS CHAR NO-UNDO.
DEF VAR cPatchNo AS CHAR NO-UNDO.

/* Ensure that these lists always match, 'c' is always the prefix */
ASSIGN cIniVarList = 
    "# Setup Variables,siteName,hostname,drive,dbDrive,topDir,mapDir,DLCDir,currVer,verDate,connectAudit,makeBackup,lockoutTries," +
    "# Filestructure Variables,adminDir,backupDir,dbDir,deskDir,docDir,envDir,installDir,updatesDir," +
    "# Admin subdirs,dbAdmin,envAdmin," +
    "# Backup subdirs,dbBackup,pgmBackup,resBackup," +
    "# Database subdirs,dbAuditDir,dbDataDir,dbProdDir,dbShipDir,dbStructDir,dbTestDir," +
    "# Documentation subdirs,docMiscDocuments,docReleaseNotes,docUserManual," +
    "# Environment subdirs,envProdDir,envTestDir," +
    "# Environment inner structure,envAddonDir,envCustFiles,envCustomerDir,envOverrideDir,envPoDir,envProgramsDir,envResourceDir,envScheduleDir,envTemplateDir,envUserMenuDir,envUsersDir," + 
    "# Install subdirs,instAOA,instBackup,instDBMS,instEsko,instFileUtils,instLocalPrint,instRemAccess," +
    "# Updates subdirs,updAdminDir,updCompressDir,updDataDir,updDataUpdateDir,updDeskDir,updMenuDir,updProgramDir,updRelNotesDir,updSqlDir,updStructureDir," +
    "# ASI Login Items,modeList,envList,dbList," +
    "# ASI Login Items Support,pgmList,dbDirList,dbPortList,audDirList,audDbList,audPortList,envVerList,dbVerList," +
    "# Basic DB Elements,audDbName,audDbPort,audDbStFile,prodDbName,prodDbPort,prodDbStFile,shipDbName,shipDbPort,shipDbStFile,testDbName,testDbPort,testDbStFile," +
    "# Misc Elements,adminPort,dfFileName,deltaFileName".


/* # Setup Variables */
DEF VAR cSitename AS CHAR INITIAL "ASI" NO-UNDO.
DEF VAR cHostname AS CHAR INITIAL "HOSTNAME" NO-UNDO.
DEF VAR cDrive AS CHAR INITIAL "C:" NO-UNDO.
DEF VAR cDbDrive AS CHAR INITIAL "C:" NO-UNDO.
DEF VAR cTopDir AS CHAR INITIAL "asigui" NO-UNDO.
DEF VAR cMapDir AS CHAR INITIAL "N:" NO-UNDO.
DEF VAR cDLCDir AS CHAR INITIAL "C:\Progress\OE116" NO-UNDO.
DEF VAR cCurrVer AS CHAR INITIAL "10.6.0" NO-UNDO.
DEF VAR cVerDate AS CHAR INITIAL "10/1/17" NO-UNDO.
DEF VAR cConnectAudit AS CHAR INITIAL "NO" NO-UNDO.
DEF VAR cMakeBackup AS CHAR INITIAL "NO" NO-UNDO.
DEF VAR cLockoutTries AS CHAR INITIAL "4" NO-UNDO.
/* # Filestructure Variables */
DEF VAR cAdminDir AS CHAR INITIAL "Admin" NO-UNDO.
DEF VAR cBackupDir AS CHAR INITIAL "Backups" NO-UNDO.
DEF VAR cDbDir AS CHAR INITIAL "Databases" NO-UNDO.
DEF VAR cDeskDir AS CHAR INITIAL "Desktop" NO-UNDO.
DEF VAR cDocDir AS CHAR INITIAL "Documentation" NO-UNDO.
DEF VAR cEnvDir AS CHAR INITIAL "Environments" NO-UNDO.
DEF VAR cInstallDir AS CHAR INITIAL "Install" NO-UNDO.
DEF VAR cUpdatesDir AS CHAR INITIAL "Updates" NO-UNDO.
/* # Admin subdirs */
DEF VAR cDbAdmin AS CHAR INITIAL "DbAdmin" NO-UNDO.
DEF VAR cEnvAdmin AS CHAR INITIAL "EnvAdmin" NO-UNDO.
/* # Backup subdirs */
DEF VAR cDbBackup AS CHAR INITIAL "Databases" NO-UNDO.
DEF VAR cPgmBackup AS CHAR INITIAL "Programs" NO-UNDO.
DEF VAR cResBackup AS CHAR INITIAL "Resources" NO-UNDO.
/* # Database subdirs */
DEF VAR cDbAuditDir AS CHAR INITIAL "Audit" NO-UNDO.
DEF VAR cDbDataDir AS CHAR INITIAL "Data" NO-UNDO.
DEF VAR cDbProdDir AS CHAR INITIAL "Prod" NO-UNDO.
DEF VAR cDbShipDir AS CHAR INITIAL "Ship" NO-UNDO.
DEF VAR cDbStructDir AS CHAR INITIAL "Structure" NO-UNDO.
DEF VAR cDbTestDir AS CHAR INITIAL "Test" NO-UNDO.
/* # Documentation subdirs */
DEF VAR cDocMiscDocuments AS CHAR INITIAL "MiscDocuments" NO-UNDO.
DEF VAR cDocReleaseNotes AS CHAR INITIAL "ReleaseNotes" NO-UNDO.
DEF VAR cDocUserManual AS CHAR INITIAL "UserManual" NO-UNDO.
/* # Environment subdirs */
DEF VAR cEnvProdDir AS CHAR INITIAL "Prod" NO-UNDO.
DEF VAR cEnvTestDir AS CHAR INITIAL "Test" NO-UNDO.
/* # Environment inner structure */
DEF VAR cEnvAddonDir AS CHAR INITIAL "Addon" NO-UNDO.
DEF VAR cEnvCustFiles AS CHAR INITIAL "CustFiles" NO-UNDO.
DEF VAR cEnvCustomerDir AS CHAR INITIAL "Customer" NO-UNDO.
DEF VAR cEnvOverrideDir AS CHAR INITIAL "Override" NO-UNDO.
DEF VAR cEnvPODir AS CHAR INITIAL "PO" NO-UNDO.
DEF VAR cEnvProgramsDir AS CHAR INITIAL "Programs" NO-UNDO.
DEF VAR cEnvResourceDir AS CHAR INITIAL "Resources" NO-UNDO.
DEF VAR cEnvScheduleDir AS CHAR INITIAL "Schedule" NO-UNDO.
DEF VAR cEnvTemplateDir AS CHAR INITIAL "Schedule" NO-UNDO.
DEF VAR cEnvUserMenuDir AS CHAR INITIAL "Usermenu" NO-UNDO.
DEF VAR cEnvUsersDir AS CHAR INITIAL "Users" NO-UNDO.
/* # Install subdirs */
DEF VAR cInstAOA AS CHAR INITIAL "AOAInstall" NO-UNDO.
DEF VAR cInstBackup AS CHAR INITIAL "BackupInstall" NO-UNDO.
DEF VAR cInstDBMS AS CHAR INITIAL "DBMSInstall" NO-UNDO.
DEF VAR cInstEsko AS CHAR INITIAL "EskoInstall" NO-UNDO.
DEF VAR cInstFileUtils AS CHAR INITIAL "FileUtilities" NO-UNDO.
DEF VAR cInstLocalPrint AS CHAR INITIAL "LocalPrintInstall" NO-UNDO.
DEF VAR cInstRemAccess AS CHAR INITIAL "RemoteAccessInstall" NO-UNDO.
/* # Updates subdirs */
DEF VAR cUpdAdminDir AS CHAR INITIAL "Admin" NO-UNDO.
DEF VAR cUpdCompressDir AS CHAR INITIAL "Compress" NO-UNDO.
DEF VAR cUpdDataDir AS CHAR INITIAL "DataFiles" NO-UNDO.
DEF VAR cUpdDataUpdateDir AS CHAR INITIAL "DataFiles" NO-UNDO.
DEF VAR cUpdDeskDir AS CHAR INITIAL "Desktop" NO-UNDO.
DEF VAR cUpdMenuDir AS CHAR INITIAL "MenuFiles" NO-UNDO.
DEF VAR cUpdProgramDir AS CHAR INITIAL "ProgramFiles" NO-UNDO.
DEF VAR cUpdRelNotesDir AS CHAR INITIAL "ReleaseNotes" NO-UNDO.
DEF VAR cUpdSQLDir AS CHAR INITIAL "SQLAccess" NO-UNDO.
DEF VAR cUpdStructureDir AS CHAR INITIAL "StructureUpdate" NO-UNDO.
/* #ASI Login Items */
DEF VAR cModeList AS CHAR INITIAL "Advantzware,Addon,CaseLabel,Schedule Monitor,Editor,Esko Monitor,FG XML Monitor,Loadtags,Monitor Users,Rel XML Monitor,RFID Monitor,RM Loadtag,Sharpshooter,Touchscreen" NO-UNDO.
DEF VAR cEnvList AS CHAR INITIAL "Prod" NO-UNDO.
DEF VAR cDbList AS CHAR INITIAL "asiProd" NO-UNDO.
/* #ASI Login Items Support */
DEF VAR cPgmList AS CHAR INITIAL "system/mainmenu.w,system/addmain.w,oerep/r-casetg.w,custom/asiSchW.w,_edit.p,jobxml\monitor.w,fgXml\monitor.w,oerep/r-loadtg.w,proshut.bat,relxml\monitor.w,rfid\monitor.w,rmrep/rmloadtg.w,sshoot/sshoot.w,touch/touchscr.w" NO-UNDO.
DEF VAR cDbDirList AS CHAR INITIAL "Prod" NO-UNDO.
DEF VAR cDbPortList AS CHAR INITIAL "2826" NO-UNDO.
DEF VAR cAudDirList AS CHAR INITIAL "Audit" NO-UNDO.
DEF VAR cAudDBList AS CHAR INITIAL "audProd" NO-UNDO.
DEF VAR cAudPortList AS CHAR INITIAL "2836" NO-UNDO.
DEF VAR cEnvVerList AS CHAR INITIAL "16.7.0" NO-UNDO.
DEF VAR cDbVerList AS CHAR INITIAL "16.7.0" NO-UNDO.
/* # Basic DB Elements */
DEF VAR cAudDbName AS CHAR INITIAL "audProd" NO-UNDO.
DEF VAR cAudDbPort AS CHAR INITIAL "2836" NO-UNDO.
DEF VAR cAudDbStFile AS CHAR INITIAL "audit.st" NO-UNDO.
DEF VAR cProdDbName AS CHAR INITIAL "asiProd" NO-UNDO.
DEF VAR cProdDbPort AS CHAR INITIAL "2826" NO-UNDO.
DEF VAR cProdDbStFile AS CHAR INITIAL "asiProd.st" NO-UNDO.
DEF VAR cShipDbName AS CHAR INITIAL "asiShip" NO-UNDO.
DEF VAR cShipDbPort AS CHAR INITIAL "2825" NO-UNDO.
DEF VAR cShipDbStFile AS CHAR INITIAL "asiShip.st" NO-UNDO.
DEF VAR cTestDbName AS CHAR INITIAL "asiTest" NO-UNDO.
DEF VAR cTestDbPort AS CHAR INITIAL "2827" NO-UNDO.
DEF VAR cTestDbStFile AS CHAR INITIAL "asiTest.st" NO-UNDO.
/* # Misc Elements */
DEF VAR cAdminPort AS CHAR INITIAL "20942.st" NO-UNDO.
DEF VAR cDfFileName AS CHAR INITIAL "asi167.df" NO-UNDO.
DEF VAR cDeltaFileName AS CHAR INITIAL "asi166167.df" NO-UNDO.

/* END advantzware.ini Variables */

DEF TEMP-TABLE ttIniFile
    FIELD iPos AS INT
    FIELD cRaw AS CHAR
    FIELD cVarName AS CHAR
    FIELD cVarValue AS CHAR
    INDEX idxPos IS PRIMARY UNIQUE iPos.

DEF TEMP-TABLE ttUsers
    FIELD ttfPdbname AS CHAR
    FIELD ttfUserID AS CHAR
    FIELD ttfUserAlias AS CHAR
    FIELD ttfEnvList AS CHAR
    FIELD ttfDbList AS CHAR
    FIELD ttfModeList AS CHAR.

PROCEDURE GetCurrentDirectoryA EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT        PARAMETER intBufferSize AS LONG.
    DEFINE INPUT-OUTPUT PARAMETER ptrToString   AS MEMPTR.
    DEFINE RETURN       PARAMETER intResult     AS SHORT.
END PROCEDURE.

PROCEDURE SetCurrentDirectoryA EXTERNAL "KERNEL32.DLL":
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

IF origDirectoryName = "" THEN DO:
    SET-SIZE(ptrToString) = 256.
    RUN GetCurrentDirectoryA (INPUT        intBufferSize,
                              INPUT-OUTPUT ptrToString,
                              OUTPUT       intResult).
    ASSIGN origDirectoryName = GET-STRING(ptrToString,1).    
END.
ELSE DO:
    RUN ipSetCurrentDir (origDirectoryName). 
END.
        
/* Find the .ini file containing variables and values */
RUN ipCreateTTIniFile.
RUN ipFindIniFile.
RUN ipFindUsrFile.

/*
ASSIGN
    FILE-INFO:FILE-NAME = cIniLoc
    cIniLoc = FILE-INFO:FULL-PATHNAME.
ASSIGN
    FILE-INFO:FILE-NAME = cUsrLoc
    cUsrLoc = FILE-INFO:FULL-PATHNAME.
*/


IF cIniLoc EQ "" THEN DO:
    MESSAGE
        "Unable to locate an 'advantzware.ini' file." SKIP
        "Please contact Advantzware Support. Exiting..."
        VIEW-AS ALERT-BOX ERROR.
    QUIT.
END.
ELSE DO:
    RUN ipReadIniFile.
END.

/* Find the .usr file containing user-level settings */
RUN ipFindUsrFile.
IF cUsrLoc EQ "" THEN DO:
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
     Btn_Cancel AT ROW 15.76 COL 6 WIDGET-ID 22
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

            IF CONNECTED(LDBNAME(1)) THEN DO:
                IF INDEX(PDBNAME(1),"165") <> 0 THEN
                    RUN preRun165.p PERSISTENT SET hPreRun.
                ELSE IF INDEX(PDBNAME(1),"166") <> 0 
                OR INDEX(PDBNAME(1),"PremTest") <> 0 THEN
                    RUN preRun166.p PERSISTENT SET hPreRun.
                ELSE IF INDEX(PDBNAME(1),"167") <> 0 THEN
                    RUN preRun167.p PERSISTENT SET hPreRun.
                ELSE
                    RUN preRun.p PERSISTENT SET hPreRun.
            END.
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

            RUN ipSetCurrentDir (cMapDir + "\" + cEnvDir + "\" + cbEnvironment:{&SV}). 

            IF INDEX(cRunPgm,"mainmenu") <> 0
            AND SEARCH("system/mainmenu2.r") NE ? THEN ASSIGN
                cRunPgm = "system/mainmenu2.w".

            RUN VALUE(cRunPgm).
        END.
        ELSE DO:
            ASSIGN 
                cCmdString = cDLCDir + "\bin\proshut.bat" + " -db " +
                             cDrive + "\" + cTopDir + "\" + cDbDir + "\" + xDbDir + "\" + PDBNAME(1).
            OS-COMMAND VALUE(cCmdString).
        END.
    END.
    ELSE MESSAGE
        "Unable to login with that User ID and Password."
        VIEW-AS ALERT-BOX ERROR.
    QUIT.
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
        WHEN "16.7.0" OR
        WHEN "16.7.4" OR
        WHEN "16.7.8" THEN DO: 
            DO iCtr = 1 TO NUM-ENTRIES(cbDatabase:LIST-ITEMS):
                IF INDEX(ENTRY(iCtr,cbDatabase:LIST-ITEMS),"167") <> 0 THEN DO:
                    ASSIGN
                        cbDatabase:SCREEN-VALUE = ENTRY(iCtr,cbDatabase:LIST-ITEMS).
                    LEAVE.
                END.
            END.
        END.
        WHEN "16.6.0" OR
        WHEN "16.6.4" OR
        WHEN "16.6.8" THEN DO: 
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
        WHEN "16.5.0" THEN DO:
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
                     cTop + cEnvCustomerDir + "," +
                     cTop + cEnvOverrideDir + "," +
                     cTop + cEnvProgramsDir + "," +
                     cTop + cEnvCustomerDir + "\Addon," +
                     cTop + cEnvOverrideDir + "\Addon," +
                     cTop + cEnvProgramsDir + "\Addon" + "," +
                     cTop + cEnvCustFiles + "," +
                     cTop + cEnvResourceDir + "," +
                     cTop + cEnvResourceDir + "\Addon" + "," +
                     cMapDir + "\" + cAdminDir + "\" + cEnvAdmin + ",".
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

    DEF VAR iLookup AS INT NO-UNDO.
    DEF VAR xdbName AS CHAR NO-UNDO.
    DEF VAR xdbPort AS CHAR NO-UNDO.

    IF cPassword <> "" THEN
        CONNECT VALUE(cStatement + 
                  " -U " + cUser + 
                  " -P '" + cPassword + 
                  "' -ct 2") NO-ERROR.
    ELSE
            CONNECT VALUE(cStatement + 
                  " -U " + cUser + 
                  " -ct 2") NO-ERROR.
    IF LDBNAME(1) = ? THEN DO:
        cStatement = REPLACE(cStatement,"-H " + cHostName,"-H LOCALHOST").
        CONNECT VALUE(cStatement + 
                      " -U " + cUser + 
                      " -P '" + cPassword + "'") NO-ERROR.
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
    ELSE DO:
        ASSIGN
            iTries = iTries + 1.
        IF iLockoutTries > 0 AND iTries > iLockoutTries THEN DO:
            MESSAGE
                "You have exceeded the allowed login attempts." SKIP
                "Exiting..."
                VIEW-AS ALERT-BOX ERROR.
            QUIT.
        END.
    END.

    ASSIGN
        lConnectAudit = IF INDEX(cConnectAudit,"Y") NE 0 OR INDEX(cConnectAudit,"T") NE 0 THEN TRUE ELSE FALSE
        lError = NOT CONNECTED(LDBNAME(1)).
    IF lError THEN
        RETURN.

    IF CONNECTED(LDBNAME(1))
    AND lConnectAudit THEN DO:
    
    ASSIGN
        xdbName = cbDatabase:{&SV}
        iLookup = LOOKUP(cbEnvironment:{&SV},cEnvList)
        xDbName = ""
        xDbName = ENTRY(iLookup,cAudDbList)
        xdbPort = ENTRY(iLookup,cAudPortList)
        connectStatement = "".
    
        IF INDEX(PDBNAME(1),"165") <> 0 
        OR INDEX(PDBNAME(1),"ship") <> 0 THEN ASSIGN
            connectStatement = "".
        ELSE IF xDbName NE "" THEN ASSIGN
            connectStatement = "-db " + xDbName + 
                               " -H " + chostName +
                               " -S " + xdbPort + 
                               " -N tcp -ld AUDIT".

        IF connectStatement NE "" THEN DO:
            CONNECT VALUE(connectStatement).
            IF NOT CONNECTED(LDBNAME(2)) THEN DO:
                MESSAGE
                    "The Audit database failed to connect.  Please" SKIP
                    "contact your system administrator for assistance."
                    VIEW-AS ALERT-BOX ERROR.
                QUIT.
            END.
        END.
    END.

        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateTTIniFile C-Win 
PROCEDURE ipCreateTTIniFile :
/*------------------------------------------------------------------------------
  Purpose:     Builds initial ttIniFile with sequences correct for output
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttIniFile.
    DO i = 1 to NUM-ENTRIES(cIniVarList):
        CREATE ttIniFile.
        ASSIGN
            ttIniFile.iPos = i
            ttIniFile.cVarName = ENTRY(i,cIniVarList).
    END.
            
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
    IF CONNECTED(LDBNAME(2)) THEN
        DISCONNECT VALUE(LDBNAME(2)).

    IF CONNECTED(LDBNAME(1)) THEN
        DISCONNECT VALUE(LDBNAME(1)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipExpandVarNames C-Win 
PROCEDURE ipExpandVarNames :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Modify variables for ease of use */
    ASSIGN
        cAdminDir = cMapDir + "\" + cAdminDir
        cBackupDir = cMapDir + "\" + cBackupDir
        cDBDir = cMapDir + "\" + cDbDir
        cDocDir = cMapDir + "\" + cDocDir
        cDeskDir = cMapDir + "\" + cDeskDir
        cEnvDir = cMapDir + "\" + cEnvDir
        cInstallDir = cMapDir + "\" + cInstallDir
        cUpdatesDir = cMapDir + "\" + cUpdatesDir

        cDbAdmin = cAdminDir + "\" + cDbAdmin
        cEnvAdmin = cAdminDir + "\" + cEnvAdmin
        cDbBackup = cBackupDir + "\" + cDbBackup
        cPgmBackup = cBackupDir + "\" + cPgmBackup
        cResBackup = cBackupDir + "\" + cResBackup
        cDbAuditDir = cDbDir + "\" + cDbAuditDir
        cDbDataDir = cDbDir + "\" + cDbDataDir
        cDbProdDir = cDbDir + "\" + cDbProdDir
        cDbShipDir = cDbDir + "\" + cDbShipDir
        cDbStructDir = cDbDir + "\" + cDbStructDir
        cDbTestDir = cDbDir + "\" + cDbTestDir
        cEnvProdDir = cEnvDir + "\" + cEnvProdDir
        cEnvTestDir = cEnvDir + "\" + cEnvTestDir
        cUpdAdminDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdAdminDir
        cUpdCompressDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdCompressDir
        cUpdDataDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdDataDir
        cUpdDeskDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdDeskDir
        cUpdMenuDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdMenuDir
        cUpdProgramDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdProgramDir
        cUpdRelNotesDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdRelNotesDir
        cUpdStructureDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdStructureDir
        lmakeBackup = IF INDEX(cMakeBackup,"Y") NE 0 OR INDEX(cMakeBackup,"T") NE 0 THEN TRUE ELSE FALSE
        cLockoutTries = SUBSTRING(cLockoutTries,1,1)
        iLockoutTries = IF cLockoutTries NE "" 
                        AND ASC(cLockoutTries) GE 48
                        AND ASC(cLockoutTries) LE 57 THEN INT(cLockoutTries) ELSE 0
        .
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFindIniFile C-Win 
PROCEDURE ipFindIniFile :
/*------------------------------------------------------------------------------
  Purpose:     Find the advantzware.ini file
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Start guessing where the file might be */
    DO:
        IF SEARCH(cIniLoc) <> ? THEN DO:
            ASSIGN
                cIniLoc = SEARCH(cIniLoc).
            LEAVE.
        END.
        ELSE ASSIGN
            cIniLoc = "..\advantzware.ini".
        IF SEARCH(cIniLoc) <> ? THEN DO:
            ASSIGN
                cIniLoc = SEARCH(cIniLoc).
            LEAVE.
        END.
        ASSIGN
            cIniLoc = "N:\Admin\advantzware.ini".
        IF SEARCH(cIniLoc) <> ? THEN DO:
            ASSIGN
                cIniLoc = SEARCH(cIniLoc).
            LEAVE.
        END.
        ASSIGN
            cIniLoc = "P:\Admin\advantzware.ini".
        IF SEARCH(cIniLoc) <> ? THEN DO:
            ASSIGN
                cIniLoc = SEARCH(cIniLoc).
            LEAVE.
        END.
        ASSIGN
            cIniLoc = "C:\ASIGUI\Admin\advantzware.ini.".
        IF SEARCH(cIniLoc) <> ? THEN DO:
            ASSIGN
                cIniLoc = SEARCH(cIniLoc).
            LEAVE.
        END.
        ASSIGN
            cIniLoc = "C:\ASI\Admin\advantzware.ini".
        IF SEARCH(cIniLoc) <> ? THEN DO:
            ASSIGN
                cIniLoc = SEARCH(cIniLoc).
            LEAVE.
        END.
        ASSIGN
            cIniLoc = "D:\ASIGUI\Admin\advantzware.ini.".
        IF SEARCH(cIniLoc) <> ? THEN DO:
            ASSIGN
                cIniLoc = SEARCH(cIniLoc).
            LEAVE.
        END.
        ASSIGN
            cIniLoc = "D:\ASI\Admin\advantzware.ini".
        IF SEARCH(cIniLoc) <> ? THEN DO:
            ASSIGN
                cIniLoc = SEARCH(cIniLoc).
            LEAVE.
        END.
        ASSIGN
            cIniLoc = "E:\ASIGUI\Admin\advantzware.ini.".
        IF SEARCH(cIniLoc) <> ? THEN DO:
            ASSIGN
                cIniLoc = SEARCH(cIniLoc).
            LEAVE.
        END.
        ASSIGN
            cIniLoc = "E:\ASI\Admin\advantzware.ini".
        IF SEARCH(cIniLoc) <> ? THEN DO:
            ASSIGN
                cIniLoc = SEARCH(cIniLoc).
            LEAVE.
        END.
        ASSIGN
            cIniLoc = "F:\ASIGUI\Admin\advantzware.ini.".
        IF SEARCH(cIniLoc) <> ? THEN DO:
            ASSIGN
                cIniLoc = SEARCH(cIniLoc).
            LEAVE.
        END.
        ASSIGN
            cIniLoc = "F:\ASI\Admin\advantzware.ini".
        IF SEARCH(cIniLoc) <> ? THEN DO:
            ASSIGN
                cIniLoc = SEARCH(cIniLoc).
            LEAVE.
        END.
        ASSIGN
            cIniLoc = "".
    END.
    
    IF cIniLoc EQ "" THEN
        RUN ipStatus ("Cannot locate .ini file. Autocreating...").
    

           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFindUsrFile C-Win 
PROCEDURE ipFindUsrFile :
/*------------------------------------------------------------------------------
  Purpose:     Find the advantzware.usr file
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Start guessing where the file might be */
    DO:
        ASSIGN
            cUsrLoc = "n:\admin\advantzware.usr".
        IF SEARCH(cUsrLoc) <> ? THEN DO:
            ASSIGN
                cUsrLoc = SEARCH(cUsrLoc).
            LEAVE.
        END.
        ELSE ASSIGN
            cUsrLoc = "..\advantzware.usr".
        IF SEARCH(cUsrLoc) <> ? THEN DO:
            ASSIGN
                cUsrLoc = SEARCH(cUsrLoc).
            LEAVE.
        END.
        ASSIGN
            cUsrLoc = "N:\Admin\advantzware.usr".
        IF SEARCH(cUsrLoc) <> ? THEN DO:
            ASSIGN
                cUsrLoc = SEARCH(cUsrLoc).
            LEAVE.
        END.
        ASSIGN
            cUsrLoc = "P:\Admin\advantzware.usr".
        IF SEARCH(cUsrLoc) <> ? THEN DO:
            ASSIGN
                cUsrLoc = SEARCH(cUsrLoc).
            LEAVE.
        END.
        ASSIGN
            cUsrLoc = "C:\ASIGUI\Admin\advantzware.usr.".
        IF SEARCH(cUsrLoc) <> ? THEN DO:
            ASSIGN
                cUsrLoc = SEARCH(cUsrLoc).
            LEAVE.
        END.
        ASSIGN
            cUsrLoc = "C:\ASI\Admin\advantzware.usr".
        IF SEARCH(cUsrLoc) <> ? THEN DO:
            ASSIGN
                cUsrLoc = SEARCH(cUsrLoc).
            LEAVE.
        END.
        ASSIGN
            cUsrLoc = "D:\ASIGUI\Admin\advantzware.usr.".
        IF SEARCH(cUsrLoc) <> ? THEN DO:
            ASSIGN
                cUsrLoc = SEARCH(cUsrLoc).
            LEAVE.
        END.
        ASSIGN
            cUsrLoc = "D:\ASI\Admin\advantzware.usr".
        IF SEARCH(cUsrLoc) <> ? THEN DO:
            ASSIGN
                cUsrLoc = SEARCH(cUsrLoc).
            LEAVE.
        END.
        ASSIGN
            cUsrLoc = "E:\ASIGUI\Admin\advantzware.usr.".
        IF SEARCH(cUsrLoc) <> ? THEN DO:
            ASSIGN
                cUsrLoc = SEARCH(cUsrLoc).
            LEAVE.
        END.
        ASSIGN
            cUsrLoc = "E:\ASI\Admin\advantzware.usr".
        IF SEARCH(cUsrLoc) <> ? THEN DO:
            ASSIGN
                cUsrLoc = SEARCH(cUsrLoc).
            LEAVE.
        END.
        ASSIGN
            cUsrLoc = "F:\ASIGUI\Admin\advantzware.usr.".
        IF SEARCH(cUsrLoc) <> ? THEN DO:
            ASSIGN
                cUsrLoc = SEARCH(cUsrLoc).
            LEAVE.
        END.
        ASSIGN
            cUsrLoc = "F:\ASI\Admin\advantzware.usr".
        IF SEARCH(cUsrLoc) <> ? THEN DO:
            ASSIGN
                cUsrLoc = SEARCH(cUsrLoc).
            LEAVE.
        END.
        ASSIGN
            cUsrLoc = "".
    END.
    
    IF cUsrLoc EQ "" THEN
        RUN ipStatus ("Cannot locate .ini file. Aborting...").
    
    
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
    DEF VAR lOK AS LOG INITIAL TRUE NO-UNDO.
    DEF VAR lExit AS LOG INITIAL TRUE NO-UNDO.

    IF INDEX(PDBNAME(1),"165") EQ 0 
    AND USERID(LDBNAME(1)) NE "asi" THEN DO:
        RUN epCheckPwdExpire IN hPreRun (INPUT-OUTPUT lOK).
        IF NOT lOK THEN QUIT.
        RUN epCheckUserLocked IN hPreRun (INPUT-OUTPUT lOK).
        IF NOT lOK THEN QUIT.
    END.
    IF NOT VALID-HANDLE(persistent-handle) THEN
        RUN nosweat/persist.p PERSISTENT SET persistent-handle.
    IF NOT VALID-HANDLE(listlogic-handle) THEN
        RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

    IF cbMode:{&SV} NE "Monitor Users" 
    AND cbMode:{&SV} NE "Editor" THEN DO:
        RUN epUserLogin IN hPreRun (OUTPUT lExit).
        IF lExit THEN QUIT.
    END.

    IF cbMode:{&SV} = "Touchscreen" THEN 
        RUN epTouchLogin in hPreRun (OUTPUT tslogin-log).

    RUN epUserRecordCheck IN hPreRun (OUTPUT lOK, OUTPUT g_track_usage).
    IF NOT lOK THEN QUIT.

    RUN epUpdateUsrFile IN hPreRun (OUTPUT cUsrList).

    RUN ipUpdUsrFile IN THIS-PROCEDURE (cUsrList).

    RUN epGetUserGroups IN hPreRun (OUTPUT g_groups).

    IF INDEX(PDBNAME(1),"166") EQ 0 THEN RUN epSetUpEDI IN hPreRun.

    IF fiUserID:{&SV} = "ASI" THEN RUN asiload.p.

    RUN epCheckExpiration IN hPreRun (OUTPUT lOK).
    IF NOT lOK THEN QUIT.

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
    INPUT FROM VALUE(SEARCH(cIniLoc)).
    REPEAT:
        IMPORT UNFORMATTED cIniLine.
        IF cIniLine BEGINS "#" THEN DO:
            FIND ttIniFile WHERE 
                ttIniFile.cVarName = cIniLine
                NO-ERROR.
            IF AVAIL ttIniFile THEN ASSIGN
                ttIniFile.cRaw = cIniLine.
        END.
        ELSE DO:
            FIND ttIniFile WHERE 
                ttIniFile.cVarName = ENTRY(1,cIniLine,"=")
                NO-ERROR.
            IF AVAIL ttIniFile THEN ASSIGN
                ttIniFile.cRaw = cIniLine
                ttIniFile.cVarValue = ENTRY(2,cIniLine,"=").
        END.            
    END.
    INPUT CLOSE.
    
    /* This lets the advantzware.ini file "heal" itself when changes made to program but not local */
    /* Used in installer, not used in login
    FOR EACH ttIniFile WHERE
        NOT ttIniFile.cVarName BEGINS "#" AND
        NOT ttIniFile.cVarName EQ "" AND
        ttIniFile.cVarValue = "":
        DISP 
            ttIniFile.cVarName LABEL "Name" FORMAT "x(32)" WITH WIDTH 90 
            FRAME dGetValue VIEW-AS DIALOG-BOX THREE-D CENTERED 
            1 COLUMN SIDE-LABELS TITLE "Need .INI file value".
        UPDATE 
            ttIniFile.cVarValue LABEL "Value" FORMAT "x(60)"
            WITH FRAME dGetValue.
    END. 
    */

    FOR EACH ttIniFile:
        CASE ttIniFile.cVarName:
            WHEN "siteName" THEN ASSIGN cSiteName = ttIniFile.cVarValue.
            WHEN "hostname" THEN ASSIGN cHostname = ttIniFile.cVarValue.
            WHEN "drive" THEN ASSIGN cDrive = ttIniFile.cVarValue.
            WHEN "dbDrive" THEN ASSIGN cDbDrive = ttIniFile.cVarValue.
            WHEN "topDir" THEN ASSIGN cTopDir = ttIniFile.cVarValue.
            WHEN "mapDir" THEN ASSIGN cMapDir = ttIniFile.cVarValue.
            WHEN "DLCDir" THEN ASSIGN cDLCDir = ttIniFile.cVarValue.
            WHEN "currVer" THEN ASSIGN cCurrVer = ttIniFile.cVarValue.
            WHEN "verDate" THEN ASSIGN cVerDate = ttIniFile.cVarValue.
            WHEN "connectAudit" THEN ASSIGN cConnectAudit = ttIniFile.cVarValue.
            WHEN "makeBackup" THEN ASSIGN cMakeBackup = ttIniFile.cVarValue.
            WHEN "lockoutTries" THEN ASSIGN cLockoutTries = ttIniFile.cVarValue.
            WHEN "adminDir" THEN ASSIGN cAdminDir = ttIniFile.cVarValue.
            WHEN "backupDir" THEN ASSIGN cBackupDir = ttIniFile.cVarValue.
            WHEN "dbDir" THEN ASSIGN cDbDir = ttIniFile.cVarValue.
            WHEN "deskDir" THEN ASSIGN cDeskDir = ttIniFile.cVarValue.
            WHEN "docDir" THEN ASSIGN cDocDir = ttIniFile.cVarValue.
            WHEN "envDir" THEN ASSIGN cEnvDir = ttIniFile.cVarValue.
            WHEN "installDir" THEN ASSIGN cInstallDir = ttIniFile.cVarValue.
            WHEN "updatesDir" THEN ASSIGN cUpdatesDir = ttIniFile.cVarValue.
            WHEN "dbAdmin" THEN ASSIGN cDbAdmin = ttIniFile.cVarValue.
            WHEN "envAdmin" THEN ASSIGN cEnvAdmin = ttIniFile.cVarValue.
            WHEN "dbBackup" THEN ASSIGN cDbBackup = ttIniFile.cVarValue.
            WHEN "pgmBackup" THEN ASSIGN cPgmBackup = ttIniFile.cVarValue.
            WHEN "resBackup" THEN ASSIGN cResBackup = ttIniFile.cVarValue.
            WHEN "dbAuditDir" THEN ASSIGN cDbAuditDir = ttIniFile.cVarValue.
            WHEN "dbDataDir" THEN ASSIGN cDbDataDir = ttIniFile.cVarValue.
            WHEN "dbProdDir" THEN ASSIGN cDbProdDir = ttIniFile.cVarValue.
            WHEN "dbShipDir" THEN ASSIGN cDbShipDir = ttIniFile.cVarValue.
            WHEN "dbStructDir" THEN ASSIGN cDbStructDir = ttIniFile.cVarValue.
            WHEN "dbTestDir" THEN ASSIGN cDbTestDir = ttIniFile.cVarValue.
            WHEN "docMiscDocuments" THEN ASSIGN cDocMiscDocuments = ttIniFile.cVarValue.
            WHEN "docReleaseNotes" THEN ASSIGN cDocReleaseNotes = ttIniFile.cVarValue.
            WHEN "docUserManual" THEN ASSIGN cDocUserManual = ttIniFile.cVarValue.
            WHEN "envProdDir" THEN ASSIGN cEnvProdDir = ttIniFile.cVarValue.
            WHEN "envTestDir" THEN ASSIGN cEnvTestDir = ttIniFile.cVarValue.
            WHEN "envAddonDir" THEN ASSIGN cEnvAddonDir = ttIniFile.cVarValue.
            WHEN "envCustFiles" THEN ASSIGN cEnvCustFiles = ttIniFile.cVarValue.
            WHEN "envCustomerDir" THEN ASSIGN cEnvCustomerDir = ttIniFile.cVarValue.
            WHEN "envOverrideDir" THEN ASSIGN cEnvOverrideDir = ttIniFile.cVarValue.
            WHEN "envPoDir" THEN ASSIGN cEnvPoDir = ttIniFile.cVarValue.
            WHEN "envProgramsDir" THEN ASSIGN cEnvProgramsDir = ttIniFile.cVarValue.
            WHEN "envResourceDir" THEN ASSIGN cEnvResourceDir = ttIniFile.cVarValue.
            WHEN "envScheduleDir" THEN ASSIGN cEnvScheduleDir = ttIniFile.cVarValue.
            WHEN "envTemplateDir" THEN ASSIGN cEnvTemplateDir = ttIniFile.cVarValue.
            WHEN "envUserMenuDir" THEN ASSIGN cEnvUserMenuDir = ttIniFile.cVarValue.
            WHEN "envUsersDir" THEN ASSIGN cEnvUsersDir = ttIniFile.cVarValue.
            WHEN "instAOA" THEN ASSIGN cInstAOA = ttIniFile.cVarValue.
            WHEN "instBackup" THEN ASSIGN cInstBackup = ttIniFile.cVarValue.
            WHEN "instDBMS" THEN ASSIGN cInstDBMS = ttIniFile.cVarValue.
            WHEN "instEsko" THEN ASSIGN cInstEsko = ttIniFile.cVarValue.
            WHEN "instFileUtils" THEN ASSIGN cInstFileUtils = ttIniFile.cVarValue.
            WHEN "instLocalPrint" THEN ASSIGN cInstLocalPrint = ttIniFile.cVarValue.
            WHEN "instRemAccess" THEN ASSIGN cInstRemAccess = ttIniFile.cVarValue.
            WHEN "updAdminDir" THEN ASSIGN cUpdAdminDir = ttIniFile.cVarValue.
            WHEN "updCompressDir" THEN ASSIGN cUpdCompressDir = ttIniFile.cVarValue.
            WHEN "updDataDir" THEN ASSIGN cUpdDataDir = ttIniFile.cVarValue.
            WHEN "updDataUpdateDir" THEN ASSIGN cUpdDataUpdateDir = ttIniFile.cVarValue.
            WHEN "updDeskDir" THEN ASSIGN cUpdDeskDir = ttIniFile.cVarValue.
            WHEN "updMenuDir" THEN ASSIGN cUpdMenuDir = ttIniFile.cVarValue.
            WHEN "updProgramDir" THEN ASSIGN cUpdProgramDir = ttIniFile.cVarValue.
            WHEN "updRelNotesDir" THEN ASSIGN cUpdRelNotesDir = ttIniFile.cVarValue.
            WHEN "updSqlDir" THEN ASSIGN cUpdSqlDir = ttIniFile.cVarValue.
            WHEN "updStructureDir" THEN ASSIGN cUpdStructureDir = ttIniFile.cVarValue.
            WHEN "modeList" THEN ASSIGN cModeList = ttIniFile.cVarValue.
            WHEN "envList" THEN ASSIGN cEnvList = ttIniFile.cVarValue.
            WHEN "dbList" THEN ASSIGN cDbList = ttIniFile.cVarValue.
            WHEN "pgmList" THEN ASSIGN cPgmList = ttIniFile.cVarValue.
            WHEN "dbDirList" THEN ASSIGN cDbDirList = ttIniFile.cVarValue.
            WHEN "dbPortList" THEN ASSIGN cDbPortList = ttIniFile.cVarValue.
            WHEN "audDirList" THEN ASSIGN cAudDirList = ttIniFile.cVarValue.
            WHEN "audDbList" THEN ASSIGN cAudDbList = ttIniFile.cVarValue.
            WHEN "audPortList" THEN ASSIGN cAudPortList = ttIniFile.cVarValue.
            WHEN "envVerList" THEN ASSIGN cEnvVerList = ttIniFile.cVarValue.
            WHEN "dbVerList" THEN ASSIGN cDbVerList = ttIniFile.cVarValue.
            WHEN "prodDbName" THEN ASSIGN cProdDbName = ttIniFile.cVarValue.
            WHEN "prodDbPort" THEN ASSIGN cProdDbPort = ttIniFile.cVarValue.
            WHEN "prodDbStFile" THEN ASSIGN cProdDbStFile = ttIniFile.cVarValue.
            WHEN "shipDbName" THEN ASSIGN cShipDbName = ttIniFile.cVarValue.
            WHEN "shipDbPort" THEN ASSIGN cShipDbPort = ttIniFile.cVarValue.
            WHEN "shipDbStFile" THEN ASSIGN cShipDbStFile = ttIniFile.cVarValue.
            WHEN "testDbName" THEN ASSIGN cTestDbName = ttIniFile.cVarValue.
            WHEN "testDbPort" THEN ASSIGN cTestDbPort = ttIniFile.cVarValue.
            WHEN "testDbStFile" THEN ASSIGN cTestDbStFile = ttIniFile.cVarValue.
            WHEN "adminPort" THEN ASSIGN cAdminPort = ttIniFile.cVarValue.
            WHEN "dfFileName" THEN ASSIGN cDfFileName = ttIniFile.cVarValue.
            WHEN "deltaFileName" THEN ASSIGN cDeltaFileName = ttIniFile.cVarValue.
        END CASE.
    END.

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
    INPUT FROM VALUE(SEARCH(cUsrLoc)).
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
    
    RUN SetCurrentDirectoryA (ipTgtDir, OUTPUT iResult).

    IF iResult NE 1 THEN DO:
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
        OUTPUT STREAM usrStream TO VALUE(cUsrLoc).
        FOR EACH ttUsers BY ttUsers.ttfPdbname by ttUsers.ttfUserID:
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

