&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File: asiInstaller.w
  Description: utility to install ASI patches and releases
  Input Parameters:  <none>
  Output Parameters: <none>
  Author: MYT
  Created: 10/1/2017 and highly modified/adapted over next several months
  Change History:
    12/19/2017 - MYT -  updated to handle 16.6.9 data fixes
                        added Clean Before Install to suppress deletion
                        of existing programs/resources directories prior
                        to install of new (lets customers stay live)
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
DEF INPUT PARAMETER ipcName AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcPort AS CHAR NO-UNDO. 
DEF INPUT PARAMETER ipcDir AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipiCurrDbVer AS INT NO-UNDO.
DEF INPUT PARAMETER ipiPatchDbVer AS INT NO-UNDO.
DEF INPUT PARAMETER ipiCurrAudVer AS INT NO-UNDO.
DEF INPUT PARAMETER ipiPatchAudVer AS INT NO-UNDO.
DEF INPUT PARAMETER ipiLevel AS INT NO-UNDO.

DEF OUTPUT PARAMETER oplSuccess AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */
&SCOPED-DEFINE SV SCREEN-VALUE IN FRAME DEFAULT-FRAME

DEF STREAM s1.

DEF TEMP-TABLE ttIniFile
    FIELD iPos AS INT
    FIELD cRaw AS CHAR
    FIELD cVarName AS CHAR
    FIELD cVarValue AS CHAR
    INDEX idxPos IS PRIMARY UNIQUE iPos.

DEF STREAM outStream.
DEF STREAM logStream.
DEF STREAM iniStream.

DEF VAR cCurrDir AS CHAR NO-UNDO.
DEF VAR cPortNo AS CHAR NO-UNDO.
DEF VAR ptrToString      AS MEMPTR    NO-UNDO.
DEF VAR intBufferSize    AS INTEGER   NO-UNDO INITIAL 256.
DEF VAR intResult        AS INTEGER   NO-UNDO.
DEF VAR iDbCtr AS INT NO-UNDO.
DEF VAR delCtr AS INT NO-UNDO.
DEF VAR dupCtr AS INT NO-UNDO.
DEF VAR cIniVarList AS CHAR NO-UNDO.
DEF VAR cTemp AS CHAR NO-UNDO.
DEF VAR cMsgStr AS CHAR FORMAT "x(80)" EXTENT 100 NO-UNDO.
DEF VAR timestring AS CHAR NO-UNDO.
DEF VAR iUserCount AS INT NO-UNDO.
DEF VAR iMsgCtr AS INT NO-UNDO.
DEF VAR iExtra AS INT NO-UNDO.
DEF VAR ll-ans AS LOG NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR v1 AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v2 AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR cBadDirList AS CHAR NO-UNDO.
DEF VAR cPatchNo AS CHAR FORMAT "x(8)" NO-UNDO.
DEF VAR cMapDrive AS CHAR FORMAT "x(2)" NO-UNDO.
DEF VAR cVarName AS CHAR EXTENT 100 NO-UNDO.
DEF VAR cVarValue AS CHAR EXTENT 100 NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR jCtr AS INT NO-UNDO.
DEF VAR iCurrVerExt AS INT NO-UNDO.
DEF VAR iNumberChecked AS INT NO-UNDO.
DEF VAR iLastIni AS INT NO-UNDO.
DEF VAR iNumUsers AS INT NO-UNDO.
DEF VAR iListEntry AS INT NO-UNDO.
DEF VAR cIniLine AS CHAR NO-UNDO.
DEF VAR cIniLoc AS CHAR NO-UNDO.
DEF VAR cUsrLine AS CHAR NO-UNDO.
DEF VAR lConnectAudit AS LOG NO-UNDO.
DEF VAR lFoundIni AS LOG NO-UNDO.
DEF VAR lFoundUsr AS LOG NO-UNDO.
DEF VAR lCorrupt AS LOG NO-UNDO.
DEF VAR lSuccess AS LOG NO-UNDO.
DEF VAR lSysError AS LOG NO-UNDO.
DEF VAR lMakeBackup AS LOG NO-UNDO.
DEF VAR lValidDB AS LOG NO-UNDO.
DEF VAR origPropath AS CHAR NO-UNDO.
DEF VAR connectStatement AS CHAR NO-UNDO.
DEF VAR cRunPgm AS CHAR NO-UNDO.
DEF VAR tslogin-cha AS CHAR NO-UNDO.
DEF VAR hPreRun AS HANDLE.
DEF VAR xDbDir AS CHAR NO-UNDO.
DEF VAR cUsrList AS CHAR NO-UNDO.
DEF VAR lUpdUsr AS LOG NO-UNDO.
DEF VAR iLockoutTries AS INT NO-UNDO.
DEF VAR lNeedUsercontrol AS LOG NO-UNDO.
DEF VAR cTestDir AS CHAR NO-UNDO.
DEF VAR lAllOK AS LOG NO-UNDO.
DEF VAR cUserID AS CHAR FORMAT "x(8)" LABEL "User ID" NO-UNDO.
DEF VAR cPassword AS CHAR FORMAT "x(24)" LABEL "Password" NO-UNDO.
DEF VAR cThisPatch AS CHAR NO-UNDO.
DEF VAR cfrom AS CHAR.
DEF VAR cTo AS CHAR.
DEF VAR iDBCurrVer AS INT NO-UNDO.
DEF VAR iDBTgtVer AS INT NO-UNDO.
DEF VAR iInstance AS INT NO-UNDO.
DEF VAR cEnvVer AS CHAR NO-UNDO.
DEF VAR iFromDelta AS INT NO-UNDO.
DEF VAR iToDelta AS INT NO-UNDO.
DEF VAR cDeltaFile AS CHAR NO-UNDO.
DEF VAR wDbList AS CHAR NO-UNDO.
DEF VAR wDbVerList AS CHAR NO-UNDO.
DEF VAR wAudDbList AS CHAR NO-UNDO.
DEF VAR wAudPortList AS CHAR NO-UNDO.
DEF VAR wAudDirList AS CHAR NO-UNDO.


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
DEF VAR cEnvVerList AS CHAR INITIAL "16.7.20" NO-UNDO.
DEF VAR cDbVerList AS CHAR INITIAL "16.7" NO-UNDO.
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
DEF VAR cAdminPort AS CHAR INITIAL "20942" NO-UNDO.
DEF VAR cDfFileName AS CHAR INITIAL "asi168.df" NO-UNDO.
DEF VAR cDeltaFileName AS CHAR INITIAL "asi167168.df" NO-UNDO.

/* END advantzware.ini Variables */

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


PROCEDURE ShellExecuteA EXTERNAL "shell32" :
    define input parameter hwnd as long.
    define input parameter lpOperation as char.
    define input parameter lpFile as char.
    define input parameter lpParameters as char.
    define input parameter lpDirectory as char.
    define input parameter nShowCmd as long.
    define return parameter hInstance as long.
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
&Scoped-Define ENABLED-OBJECTS bProcess eStatus 
&Scoped-Define DISPLAYED-OBJECTS fiSiteName fiHostname fiDbName fiDbDir ~
fiPortNo fiFromVer fiToVer fiDeltaFilename eStatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fIntVer C-Win 
FUNCTION fIntVer RETURNS INTEGER
  ( INPUT cVerString AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bProcess AUTO-END-KEY 
     LABEL "No User Action Required" 
     SIZE 46 BY 1.43
     FONT 6.

DEFINE VARIABLE eStatus AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 75 BY 3.81 NO-UNDO.

DEFINE VARIABLE fiDbDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "in directory" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE fiDbName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Upgrading Database" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiDeltaFilename AS CHARACTER FORMAT "X(256)":U 
     LABEL "using delta file" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromVer AS CHARACTER FORMAT "X(256)":U 
     LABEL "from ASI version" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiHostname AS CHARACTER FORMAT "X(256)":U 
     LABEL "Server Name" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fiPortNo AS CHARACTER FORMAT "X(256)":U 
     LABEL "running on port" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiSiteName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Site Name" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fiToVer AS CHARACTER FORMAT "X(256)":U 
     LABEL "to version" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiSiteName AT ROW 1.24 COL 19 COLON-ALIGNED WIDGET-ID 68
     fiHostname AT ROW 2.19 COL 19 COLON-ALIGNED WIDGET-ID 36
     fiDbName AT ROW 3.86 COL 29 COLON-ALIGNED
     fiDbDir AT ROW 4.81 COL 29 COLON-ALIGNED
     fiPortNo AT ROW 5.76 COL 29 COLON-ALIGNED
     fiFromVer AT ROW 6.71 COL 29 COLON-ALIGNED
     fiToVer AT ROW 7.67 COL 29 COLON-ALIGNED WIDGET-ID 46
     fiDeltaFilename AT ROW 8.62 COL 29 COLON-ALIGNED WIDGET-ID 50
     bProcess AT ROW 10.29 COL 15 WIDGET-ID 404
     eStatus AT ROW 12.43 COL 3 NO-LABEL WIDGET-ID 52
     "Status:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.71 COL 3 WIDGET-ID 54
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.8 BY 15.91
         DEFAULT-BUTTON bProcess WIDGET-ID 100.


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
         TITLE              = "Advantzware Update - Upgrade Database"
         HEIGHT             = 15.48
         WIDTH              = 79.8
         MAX-HEIGHT         = 39.29
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 39.29
         VIRTUAL-WIDTH      = 320
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
/* SETTINGS FOR FILL-IN fiDbDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDbName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDeltaFilename IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFromVer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiHostname IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPortNo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSiteName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiToVer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Advantzware Update - Upgrade Database */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
    IF USERID(LDBNAME(1)) EQ "" THEN QUIT.
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Advantzware Update - Upgrade Database */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bProcess C-Win
ON CHOOSE OF bProcess IN FRAME DEFAULT-FRAME /* No User Action Required */
DO:
    RUN ipProcessRequest IN THIS-PROCEDURE.  
    APPLY 'close' TO THIS-PROCEDURE.
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
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE
   ON END-KEY UNDO MAIN-BLOCK, LEAVE:
    RUN enable_UI.

    IF ipiLevel LT 5 THEN DO:
        MESSAGE
            "You do not have sufficient permissions to run this" SKIP
            "procedure.  Please contact your System Administrator."
            VIEW-AS ALERT-BOX ERROR.
        QUIT.
    END.
    
    /* Get the initial directory for later resetting */
    SET-SIZE(ptrToString) = 256.
    RUN GetCurrentDirectoryA (INPUT        intBufferSize,
                              INPUT-OUTPUT ptrToString,
                              OUTPUT       intResult).
    ASSIGN 
        cCurrDir = GET-STRING(ptrToString,1).    
    
    RUN ipCreateTTiniFile.
    RUN ipFindIniFile.
    IF cIniLoc NE "" THEN DO:
        ASSIGN
            FILE-INFO:FILE-NAME = cIniLoc
            cIniLoc = FILE-INFO:FULL-PATHNAME.
        RUN ipReadIniFile.
    END.
    RUN ipExpandVarNames.
    RUN ipSetDispVars.

    ASSIGN 
        fiHostName:{&SV} = cHostName
        fiDbName:{&SV} = ipcName
        fiPortNo:{&SV} = ipcPort
        fiDbDir:{&SV} = ipcDir
        fiFromVer:{&SV} = STRING(ipiCurrDbVer,"99999999")
        fiToVer:{&SV} = STRING(ipiPatchDbVer,"99999999")
        cDeltaFile = "asi" + STRING(ipiCurrDbVer,"99999999") + "_" + STRING(ipiPatchDbVer,"99999999") + ".df"
        fiDeltaFileName:{&SV} = cDeltaFile.

    IF ipiLevel LT 10 THEN APPLY 'choose' TO bProcess.
    ELSE DO:
        ASSIGN 
            bProcess:LABEL = "Start Update".
        APPLY 'entry' TO bProcess.
    END.
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

RETURN.

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
  DISPLAY fiSiteName fiHostname fiDbName fiDbDir fiPortNo fiFromVer fiToVer 
          fiDeltaFilename eStatus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE bProcess eStatus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipBackupDBs C-Win 
PROCEDURE ipBackupDBs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cCmdLine AS CHAR NO-UNDO.
    DEF VAR cLocItem AS CHAR NO-UNDO.
    DEF VAR cLocDir AS CHAR NO-UNDO.
    DEF VAR cLocName AS CHAR NO-UNDO.
    DEF VAR cLocPort AS CHAR NO-UNDO.
    DEF VAR cBackupName AS CHAR NO-UNDO.
    DEF VAR cLockFile AS CHAR NO-UNDO.  
    DEF VAR cPrefix  AS CHAR NO-UNDO.
    DEF VAR cThisDir AS CHAR NO-UNDO.
      
    ASSIGN 
        cPrefix = SUBSTRING(fiDbName:{&SV},1,3).
        
    IF cPrefix = "asi" THEN ASSIGN 
        iListEntry = LOOKUP(fiDbName:{&SV},cDbList)
        cThisDir = ENTRY(iListEntry,cDbDirList).
    ELSE ASSIGN 
        iListEntry = LOOKUP(fiDbName:{&SV},cAudDbList)
        cThisDir = ENTRY(iListEntry,cAudDirList).
    
    ASSIGN
        cLocDir  = fiDbDir:{&SV}
        cLocName = fiDbName:{&SV}
        cLocPort = fiPortNo:{&SV}
        cBackupName = cDbBackup + "\" + cLocName + "_" +
                   STRING(YEAR(TODAY)) +
                   STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   STRING(TIME) + ".bak" 
        cCmdLine = cDLCDir + "\bin\probkup online " + 
                   cDBDrive + "\" + 
                   cTopDir + "\" + 
                   cDbDir + "\" +
                   cLocDir + "\" +
                   cLocName + " " + 
                   cBackupName
        cLockFile = cDbDrive + "\" +
                   cTopDir + "\" + cDbDir + "\" + 
                   cThisDir + "\" +
                   fiDBName:{&SV} + ".lk".
    .

    IF SEARCH(cLockFile) EQ ? THEN DO:
        MESSAGE 
            "The " + fiDbName:{&SV} + " database is not currently running." SKIP 
            "This means that it will not be possible to back up the data-" SKIP 
            "base, or to upgrade it with this program.  You should exit" SKIP 
            "this program now, and make sure that the databases are" SKIP 
            "running before you attempt to upgrade the system again."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            lSuccess = FALSE.
        RETURN.
    END.
    
    RUN ipStatus ("  Backing Up database " + fiDbName:{&SV}).
    
    OS-COMMAND SILENT VALUE(cCmdLine).
    
    IF SEARCH(cBackupName) NE ? THEN DO:
        ASSIGN
            lSuccess = TRUE.
        RUN ipStatus ("    Backup successful").
    END.
    ELSE DO:
        ASSIGN
            lSuccess = FALSE.
        RUN ipStatus ("    Backup FAILED").
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateAudit C-Win 
PROCEDURE ipCreateAudit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcDBIdent AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipiListIndex AS INT NO-UNDO.
    
    DEF VAR cCmdLine AS CHAR NO-UNDO.
    DEF VAR cLocItem AS CHAR NO-UNDO.
    DEF VAR cLocDir AS CHAR NO-UNDO.
    DEF VAR cLocName AS CHAR NO-UNDO.
    DEF VAR cLocPort AS CHAR NO-UNDO.
    DEF VAR cStructureST AS CHAR NO-UNDO.    
    DEF VAR cAuditDF AS CHAR NO-UNDO.
    DEF VAR cStatement AS CHAR NO-UNDO.
    DEF VAR cCmd AS CHAR NO-UNDO.
    DEF VAR cAudName AS CHAR NO-UNDO.
    DEF VAR cAudPort AS CHAR NO-UNDO.
    DEF VAR cStartString AS CHAR NO-UNDO.
        
    RUN ipStatus ("Creating Audit database").
    
    RUN ipSetCurrentDir (cDbDrive + "\" + cTopDir + "\databases\audit"). 
    ASSIGN
        cLocItem = ipcDbIdent
        cLocDir  = ENTRY(1,cLocItem,"-")
        cLocName = ENTRY(2,cLocItem,"-")
        cLocPort = ENTRY(3,cLocItem,"-")
        cStructureST = cDrive + "\" + cTopDir + "\" +
                   cUpdatesDir + "\PATCH" + fiToVer:{&SV} + "\" +
                   "StructureUpdate\STFiles\audit.st"
        cAuditDf = cDrive + "\" + cTopDir + "\" +
                   cUpdatesDir + "\PATCH" + fiToVer:{&SV} + "\" +
                   "StructureUpdate\DFFiles\asiAudit.df"
        cAudName = "aud" + SUBSTRING(cLocName,4,8)
        cAudPort = STRING(INT(cLocPort) + 10,"9999").

    IF cDbDrive EQ cDrive THEN ASSIGN
        cCmdLine = cDlcDir + '\bin\prostrct create ' + 
                   cAudName + ' ' + 
                   cStructureST.
    ELSE ASSIGN
        cCmdLine = cDlcDir + '\bin\prostrct create ' + 
                   cAudName + ' ' + 
                   cStructureST.

    OS-COMMAND SILENT VALUE(cCmdLine).

    
    IF cDbDrive EQ cDrive THEN ASSIGN
        cCmdLine = cDlcDir + "\bin\procopy " + 
                   cDlcDir + "\empty4 " + 
                   cAudName.
    ELSE ASSIGN
        cCmdLine = cDlcDir + "\bin\procopy " + 
                   cDlcDir + "\empty4 " + 
                   cAudName.

    RUN ipStatus ("  Copying metaschema data...").
    OS-COMMAND SILENT VALUE(cCmdLine).

        ASSIGN
            /* Single user connect statment */
            cStatement = "-db " + 
                         cAudName +
                         " -1 -ld " + cAudName.
        /* Connect to the database single user */
        CONNECT VALUE(cStatement).
        CREATE ALIAS DICTDB FOR DATABASE VALUE(cAudName).
        /* Load the delta */
        RUN ipStatus ("  Loading schema...").
        RUN prodict/load_df.p (cAuditDf). 
                
        /* Disconnect it */
        RUN ipStatus ("  Disconnecting.").
        DISCONNECT VALUE(cAudName).
        
    /* Create/Write a txt file that can be loaded into conmgr.properties */
    RUN ipStatus ("  Building conmgr file.").
    OUTPUT TO c:\tmp\conmgrdelta.txt.
    PUT UNFORMATTED "[configuration." + cAudName + ".defaultconfiguration]" + CHR(10).
    PUT UNFORMATTED "    afterimageprocess=false" + CHR(10).
    PUT UNFORMATTED "    asynchronouspagewriters=1" + CHR(10).
    PUT UNFORMATTED "    beforeimageprocess=true" + CHR(10).
    PUT UNFORMATTED "    blocksindatabasebuffers=32768" + CHR(10).
    PUT UNFORMATTED "    database=" + cAudName + CHR(10).
    PUT UNFORMATTED "    displayname=defaultConfiguration" + CHR(10).
    PUT UNFORMATTED "    locktableentries=96000" + CHR(10).
    PUT UNFORMATTED "    monitored=true" + CHR(10).
    PUT UNFORMATTED "    otherargs=" + CHR(10).
    PUT UNFORMATTED "    servergroups=" + cAudName + ".defaultconfiguration.defaultservergroup" + CHR(10).
    PUT UNFORMATTED "    watchdogprocess=true" + CHR(10).
    PUT UNFORMATTED "" + CHR(10).
    PUT UNFORMATTED "[database." + cAudName + "]" + CHR(10).
    PUT UNFORMATTED "    autostart=true" + CHR(10).
    PUT UNFORMATTED "    configurations=" + cAudName + ".defaultconfiguration" + CHR(10).
    PUT UNFORMATTED "    databasename=" + cDbDrive + "\" + cTopDir + "\Databases\Audit\" + cAudName + CHR(10).
    PUT UNFORMATTED "    defaultconfiguration=" + cAudName + ".defaultconfiguration" + CHR(10).
    PUT UNFORMATTED "    displayname=" + cAudName + CHR(10).
    PUT UNFORMATTED "    monitorlicensed=true" + CHR(10).
    PUT UNFORMATTED "" + CHR(10).
    PUT UNFORMATTED "[servergroup." + cAudName + ".defaultconfiguration.defaultservergroup]" + CHR(10).
    PUT UNFORMATTED "    configuration=" + cAudName + ".defaultconfiguration" + CHR(10).
    PUT UNFORMATTED "    displayname=defaultServerGroup" + CHR(10).
    PUT UNFORMATTED "    port=" + cAudPort + CHR(10).
    PUT UNFORMATTED "    type=both" + CHR(10).
    OUTPUT CLOSE.
    
    ASSIGN
        cCmdLine     = cDlcDir + "\bin\mergeprop -type database -action create -delta " + 
                       "c:\tmp\conmgrdelta.txt -silent"
        cStartString = cDlcDir + "\bin\dbman" + 
                       " -host " + cHostName + 
                       " -port " + cPortNo + 
                       " -database " + cAudName + 
                       " -start".

    RUN ipStatus ("  Merging conmgr info.").
    OS-COMMAND SILENT VALUE(cCmdLine).
    RUN ipStatus ("  Waiting for connection manager.").
    PAUSE 10 NO-MESSAGE.
    
    RUN ipStatus ("  Serving " + cAudName).

    OS-COMMAND VALUE(cStartString).
    pause 10 no-message.
    
    /*    
    MESSAGE
        "You should add this database to the system with the OE Explorer" SKIP
        "tool now.  The database name is '" + cAudName + "' and it is located in" SKIP
        "the '" + cDbAuditDir + "' directory.  It should have port number '" + cAudPort "'." SKIP
        "YOU SHOULD WRITE THESE VALUES DOWN NOW!" SKIP
        "NAME: " + cAudName SKIP
        "LOCATION: " + cDbAuditDir SKIP
        "PORT: " + cAudPort SKIP
        "Would you like me to start this tool for you?"
        view-as alert-box question buttons yes-no update lStart AS LOG.
    
    IF lStart THEN DO:
        ASSIGN 
            cCmd = cDLCDir + "oemgmt\fathom.url".
        RUN ShellExecuteA IN THIS-PROCEDURE
            (0,
            "open",
            ccmd,
            "",
            "",
            8,
            output iInstance).
         MESSAGE
            "Press OK when you have completed the OE Explorer operation."
            VIEW-AS ALERT-BOX.
    END.
    ELSE DO:
        MESSAGE
            "You should create the OE Explorer data as soon as possible. For now," skip
            "I will generate the needed information to continue the update."
            VIEW-AS ALERT-BOX INFO.
            
        cCmdLine = cDlcDir + "\bin\proserve " + 
                   "-db " + cDbAuditDir + "\" + cAudName + 
                   " -H " + cHostName +
                   " -S " + cAudPort +
                   " -ld audit -L 96000 -B 32768".
        RUN ipStatus ("  Starting audit with PROSERVE").
        OS-COMMAND SILENT VALUE(cCmdLine).

        RUN ipStatus("Need OE Explorer info added").
    END.
    */
    
    ASSIGN
        ENTRY(iListEntry,cAudDbList) = cAudName
        ENTRY(iListEntry,cAudPortList) = cAudPort.
    RUN ipSetCurrentDir (cCurrDir). 
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipExpandVarNames C-Win 
PROCEDURE ipExpandVarNames :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Modify variables for ease of use */
    /* Modify variables for ease of use */
    ASSIGN
        cMapDir = cDrive + "\" + cTopDir
        cAdminDir = cMapDir + "\" + cAdminDir
        cBackupDir = cMapDir + "\" + cBackupDir
        cDBDir = cDbDrive + "\" + cTopDir + "\" + cDbDir 
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
        cUpdAdminDir = cUpdatesDir + "\" + cUpdAdminDir
        cUpdCompressDir = cUpdatesDir + "\" + cUpdCompressDir
        cUpdDataDir = cUpdatesDir + "\" + cUpdDataDir
        cUpdDeskDir = cUpdatesDir + "\" + cUpdDeskDir
        cUpdMenuDir = cUpdatesDir + "\" + cUpdMenuDir
        cUpdProgramDir = cUpdatesDir + "\" + cUpdProgramDir
        cUpdRelNotesDir = cUpdatesDir + "\" + cUpdRelNotesDir
        cUpdStructureDir = cUpdatesDir + "\" + cUpdStructureDir
        lmakeBackup = IF INDEX(cMakeBackup,"Y") NE 0 OR INDEX(cMakeBackup,"T") NE 0 THEN TRUE ELSE FALSE
        lConnectAudit = IF INDEX(cConnectAudit,"Y") NE 0 OR INDEX(cConnectAudit,"T") NE 0 THEN TRUE ELSE FALSE
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
        ASSIGN
            cIniLoc = "advantzware.ini".
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
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipProcessRequest C-Win 
PROCEDURE ipProcessRequest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR iLookup AS INT NO-UNDO.
    
    RUN ipStatus ("Beginning Database Schema Update").
    /* Process "regular" database (asixxxx.db) */
    RUN ipBackupDBs.
    IF NOT lSuccess THEN 
    DO:
        ASSIGN 
            oplSuccess = FALSE.
        RUN ipStatus ("  Upgrade failed in ipBackupDBs for database " + fiDbName:{&SV}).
        RETURN.
    END.
    
    RUN ipUpgradeDBs.
    IF NOT lSuccess THEN 
    DO:
        ASSIGN 
            oplSuccess = FALSE.
        RUN ipStatus ("  Upgrade failed in ipUpgradeDBs for database " + fiDbName:{&SV}).
        RETURN.
    END.
    
    /* Process "audit" database (audxxxx.db) */
    ASSIGN 
        iLookup = LOOKUP(fiDbName:{&SV},cDbList)
        fiDbName:{&SV} = REPLACE(fiDbName:{&SV},"asi","aud")
        fiDbDir:{&SV} = "audit"
        fiPortNo:{&SV} = ENTRY(iLookup,cAudPortList). 
    
    RUN ipBackupDBs.
    IF NOT lSuccess THEN 
    DO:
        ASSIGN 
            oplSuccess = FALSE.
        RUN ipStatus ("  Upgrade failed in ipBackupDBs for database " + fiDbName:{&SV}).
        RETURN.
    END.
    
    RUN ipUpgradeDBs.
    IF NOT lSuccess THEN 
    DO:
        ASSIGN 
            oplSuccess = FALSE.
        RUN ipStatus ("  Upgrade failed in ipUpgradeDBs for database " + fiDbName:{&SV}).
        RETURN.
    END.
    
    RUN ipStatus ("Database Schema Update Complete").
    RUN ipWriteIniFile.

    ASSIGN
        fiFromVer:{&SV} = fiToVer:{&SV}
        oplSuccess = lSuccess.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipReadAdminSvcProps C-Win 
PROCEDURE ipReadAdminSvcProps :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR cLine AS CHAR NO-UNDO.
    
    RUN ipStatus ("  Reading admin service props file " + fiDbName:{&SV}).

    INPUT FROM VALUE (cDLCDir + "\properties\AdminServerPlugins.properties").
    REPEAT:
        IMPORT cLine.
        ASSIGN 
            cLine = TRIM(cLine).
        IF ENTRY(1,cLine,"=") EQ "port" THEN DO:
            ASSIGN 
                cPortNo = ENTRY(2,cLine,"=").
            LEAVE.
        END.
    END.
    INPUT CLOSE.
        

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
            WHEN "dbList" THEN ASSIGN 
                cDbList = ttIniFile.cVarValue
                wDbList = ttIniFile.cVarValue.
            WHEN "pgmList" THEN ASSIGN cPgmList = ttIniFile.cVarValue.
            WHEN "dbDirList" THEN ASSIGN cDbDirList = ttIniFile.cVarValue.
            WHEN "dbPortList" THEN ASSIGN cDbPortList = ttIniFile.cVarValue.
            WHEN "audDirList" THEN ASSIGN 
                cAudDirList = ttIniFile.cVarValue
                wAudDirList = ttIniFile.cVarValue.
            WHEN "audDbList" THEN ASSIGN 
                cAudDbList = ttIniFile.cVarValue
                wAudDbList = ttIniFile.cVarValue.
            WHEN "audPortList" THEN ASSIGN 
                cAudPortList = ttIniFile.cVarValue
                wAudPortList = ttIniFile.cVarValue.
            WHEN "envVerList" THEN ASSIGN cEnvVerList = ttIniFile.cVarValue.
            WHEN "dbVerList" THEN ASSIGN 
                cDbVerList = ttIniFile.cVarValue
                wDbVerList = ttIniFile.cVarValue.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetDispVars C-Win 
PROCEDURE ipSetDispVars :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO:
        FOR EACH ttIniFile:
            CASE ttIniFile.cVarName:
                WHEN "siteName" THEN ASSIGN fiSiteName:{&SV} = ttIniFile.cVarValue.
                WHEN "hostname" THEN ASSIGN cHostName = ttIniFile.cVarValue.
            END CASE.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipStatus C-Win 
PROCEDURE ipStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcStatus AS CHAR NO-UNDO.
    DEF VAR cLogFile AS CHAR NO-UNDO.
                
                
    IF INDEX(ipcStatus,"duplicate") EQ 0 THEN DO:
        ASSIGN
            eStatus:{&SV}       = eStatus:{&SV} + ipcStatus + CHR(10)
            eStatus:CURSOR-LINE = eStatus:NUM-LINES.
        ASSIGN
            cLogFile = cEnvAdmin + "\UpdateLog.txt"
            iMsgCtr = iMsgCtr + 1
            cMsgStr[iMsgCtr] = ipcStatus.
        OUTPUT STREAM logStream TO VALUE(cLogFile) APPEND.
        PUT STREAM logStream
            STRING(TODAY,"99/99/99") AT 1
            STRING(TIME,"HH:MM:SS") AT 12
            cMsgStr[iMsgCtr] FORMAT "x(160)" AT 25
            SKIP.
        OUTPUT STREAM logStream CLOSE.
    END.
        
    PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpgradeDBs C-Win 
PROCEDURE ipUpgradeDBs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cStopString AS CHAR NO-UNDO.
    DEF VAR cStartString AS CHAR NO-UNDO.
    DEF VAR cStatement AS CHAR NO-UNDO.
    DEF VAR cDeltaDf AS CHAR NO-UNDO.
    DEF VAR cLockFile AS CHAR NO-UNDO.
    DEF VAR cNewList AS CHAR NO-UNDO.
    DEF VAR cNewSel AS CHAR NO-UNDO.
    DEF VAR cThisEntry AS CHAR NO-UNDO.
    DEF VAR cReplEntry AS CHAR NO-UNDO.
    DEF VAR cDelta AS CHAR NO-UNDO.
    DEF VAR cPrefix AS CHAR NO-UNDO.
    DEF VAR cThisDb AS CHAR NO-UNDO. 
    DEF VAR cThisDir AS CHAR NO-UNDO.
    DEF VAR iWaitCount AS INT NO-UNDO.
    DEF VAR lAlternate AS LOG NO-UNDO.
    DEF VAR cFullDelta AS CHAR NO-UNDO.
    DEF VAR cThisPort AS CHAR NO-UNDO.

    RUN ipStatus ("  Upgrading database " + fiDbName:{&SV}).

    ASSIGN 
        cPrefix = SUBSTRING(fiDbName:{&SV},1,3)
        iDbCtr = iDbCtr + 1
        iWaitCount = 0
        cDelta = REPLACE(cDeltaFile,"asi",cPrefix)
        cFullDelta = REPLACE(cUpdStructureDir,"PATCH","PATCH" + fiToVer:{&SV}) + "\DFFiles\" + cDelta.
        
    IF cPrefix = "asi" THEN ASSIGN 
        iListEntry = LOOKUP(fiDbName:{&SV},cDbList)
        cThisDir = ENTRY(iListEntry,cDbDirList)
        cThisPort = ENTRY(iListEntry,cDbPortList).
    ELSE ASSIGN 
        iListEntry = LOOKUP(fiDbName:{&SV},cAudDbList)
        cThisDir = ENTRY(iListEntry,cAudDirList)
        cThisPort = ENTRY(iListEntry,cAudPortList).

    IF iToDelta LE iFromDelta THEN DO:
        RUN ipStatus ("    Database is already upgraded.  Cancelling.").
        ASSIGN 
            lSuccess = FALSE.
        RETURN.
    END.
    
    IF SEARCH(cFullDelta) EQ ? THEN DO:
        IF cPrefix EQ "asi" THEN DO:
            RUN ipStatus ("    Unable to locate delta file " + cDelta + ".  Cancelling.").
            RUN ipStatus ("      " + cFullDelta).
            ASSIGN 
                lSuccess = FALSE.
            RETURN.
        END.
        ELSE DO:
            RUN ipStatus ("    Unable to locate AUDIT delta file " + cDelta + ".  Continuing.").
            RETURN.
        END.
    END.
            
    ASSIGN
        /* This results in a value similar to 'asi167_168.df' */
        cDeltaDf = cDelta
        /* This fully qualifies the path name to the delta */
        cDeltaDf = cDrive + "\" + cTopDir + "\" +
                   cUpdatesDir + "\Patch" + fiToVer:{&SV} + "\" +
                   "StructureUpdate\DFFiles\" + cDeltaDf
        cStopString = cDlcDir + "\bin\dbman" + 
                     " -host " + cHostName + 
                     " -port " + cPortNo + 
                     " -database " + fiDbName:{&SV} + 
                     " -stop"
        cStartString = cDlcDir + "\bin\dbman" + 
                     " -host " + cHostName + 
                     " -port " + cPortNo + 
                     " -database " + fiDBName:{&SV} + 
                     " -start"
        /* Single user connect statment */
        cStatement = "-db " + cDbDrive + "\" +
                     cTopDir + "\" + cDbDir + "\" + 
                     cThisDir + "\" +
                     fiDBName:{&SV} + 
                     " -1 -ld updDB" + STRING(iDbCtr)
        cLockFile = cDbDrive + "\" +
                     cTopDir + "\" + cDbDir + "\" + 
                     cThisDir + "\" +
                     fiDBName:{&SV} + ".lk".
    
    /* Unserve the database */
    RUN ipStatus ("    Stopping database service").
    OS-COMMAND SILENT VALUE(cStopString).
    /* May have to wait for DB to shut down */
    WaitBlock:
    DO WHILE SEARCH(cLockFile) NE ?:
        RUN ipStatus ("    Waiting for removal of lock file").
        ASSIGN 
            iWaitCount = iWaitCount + 1.
        PAUSE 5 NO-MESSAGE.
        IF iWaitCount EQ 3 THEN DO:
            RUN ipStatus ("    Normal shutdown failed.  Trying alternate method").
            ASSIGN 
                lAlternate = TRUE.
            LEAVE waitblock.
        END.
    END.
    
    /* Database didn't shut down normally, try with proshut */
    IF lAlternate THEN DO:
        ASSIGN 
            cStopString = cDlcDir + "\bin\_mprshut -by -db " + 
                          cDbDrive + "\" +
                          cTopDir + "\" + cDbDir + "\" + 
                          cThisDir + "\" +
                          fiDBName:{&SV}
            cStartString = cDlcDir + "\bin\_mprosrv -db " + 
                           cDbDrive + "\" +
                           cTopDir + "\" + cDbDir + "\" + 
                           cThisDir + "\" +
                           fiDBName:{&SV} +
                           " -H " + cHostName +
                           " -S " + cThisPort +
                           " -N tcp"
                          .
                          
        OS-COMMAND SILENT VALUE(cStopString).
        Waitblock2:
        DO WHILE SEARCH(cLockFile) NE ?:
            RUN ipStatus ("    Waiting for removal of lock file").
            ASSIGN 
                iWaitCount = iWaitCount + 1.
            PAUSE 5 NO-MESSAGE.
            IF iWaitCount EQ 3 THEN 
            DO:
                RUN ipStatus ("    Alternate shutdown failed.  Cancelling.").
                LEAVE waitblock2.
            END.
        END.
    END.
    IF SEARCH(cLockFile) NE ? THEN DO:
        RUN ipStatus (    "Unable to shut down server for " + fiDbName:{&SV} + ".  Cancelling.").
        ASSIGN 
            lSuccess = FALSE.
        RETURN.
    END.
        
    /* Connect to the database single user */
    RUN ipStatus ("    Connecting single-user mode").
    CONNECT VALUE(cStatement).
    RUN ipStatus ("    Creating DICTDB alias").
    CREATE ALIAS DICTDB FOR DATABASE VALUE("updDB" + STRING(iDbCtr)).
    
    /* Load the delta */
    RUN ipStatus ("    Loading delta " + STRING(cDeltaDf)).
    RUN prodict/load_df.p (cFullDelta). 
    
    /* Disconnect it */
    RUN ipStatus ("    Disconnecting").
    DISCONNECT VALUE("updDB" + STRING(iDbCtr)).
    
    /* Re-Serve it */
    RUN ipStatus ("    Serving " + fiDbName:{&SV}).
    
    OS-COMMAND SILENT VALUE(cStartString).
    Waitblock3:
    DO WHILE SEARCH(cLockFile) EQ ?:
        RUN ipStatus ("    Waiting for restore of lock file").
        ASSIGN 
            iWaitCount = iWaitCount + 1.
        PAUSE 5 NO-MESSAGE.
        IF iWaitCount EQ 3 THEN 
        DO:
            RUN ipStatus ("    DB restart failed.  Cancelling.").
            LEAVE waitblock3.
        END.
    END.
    IF SEARCH(cLockFile) EQ ? THEN DO:
        MESSAGE 
            "Unable to restart the " + fiDBName:{&SV} + " database after upgrade." SKIP 
            "This may be a symptom of several problems.  You should contact" SKIP 
            "Advantzware support before continuing the update process."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            lSuccess = FALSE.
        RETURN.
    END.
        
    IF cPrefix EQ "asi" THEN DO: 
        ASSIGN 
            ENTRY(iListEntry,wDbVerList) = STRING(iToDelta / 10,"99.9").
    END.

    ASSIGN
        lSuccess = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipWriteIniFile C-Win 
PROCEDURE ipWriteIniFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cThisElement AS CHAR NO-UNDO.

    RUN ipStatus ("Writing advantzware.ini file...").

    FIND ttIniFile WHERE ttIniFile.cVarName = "dbVerList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = wDbVerList.
    FIND ttIniFile WHERE ttIniFile.cVarName = "audDirList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = wAudDirList.
    FIND ttIniFile WHERE ttIniFile.cVarName = "audPortList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = wAudPortList.

    OUTPUT TO VALUE(cIniLoc).
    FOR EACH ttIniFile BY ttIniFile.iPos:
        IF ttIniFile.cVarName BEGINS "#" THEN
            PUT UNFORMATTED ttIniFile.cVarName + CHR(10).
        ELSE IF ttIniFile.cVarName NE "" THEN
            PUT UNFORMATTED ttIniFile.cVarName + "=" + ttIniFile.cVarValue + CHR(10).
        ELSE NEXT.
    END.
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fIntVer C-Win 
FUNCTION fIntVer RETURNS INTEGER
  ( INPUT cVerString AS CHAR ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR cStrVal AS CHAR EXTENT 4 NO-UNDO.
    DEF VAR iIntVal AS INT  EXTENT 4 NO-UNDO.
    DEF VAR iIntVer AS INT  NO-UNDO.
    ASSIGN
        cStrVal[1] = ENTRY(1,cVerString,".")
        cStrVal[2] = ENTRY(2,cVerString,".")
        cStrVal[3] = ENTRY(3,cVerString,".")
        cStrVal[4] = IF NUM-ENTRIES(cVerString,".") GT 3 THEN ENTRY(4,cVerString,".") ELSE "0"
        iIntVal[1] = INT(cStrVal[1])
        iIntVal[2] = INT(cStrVal[2])
        iIntVal[3] = IF INT(cStrVal[3]) LT 10 THEN INT(cStrVal[3]) * 10 ELSE INT(cStrVal[3])
        iIntVal[4] = INT(cStrVal[4])
        iIntVer    = (iIntVal[1] * 1000000) + (iIntVal[2] * 10000) + (iIntVal[3] * 100) + iIntVal[4]
        NO-ERROR.
    
    RETURN iIntVer.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

