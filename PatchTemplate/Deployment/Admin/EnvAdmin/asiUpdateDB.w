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
DEF INPUT PARAMETER ipcVer AS CHAR NO-UNDO.
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
DEF VAR cEnvVerList AS CHAR INITIAL "16.7.4" NO-UNDO.
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
DEF VAR cDfFileName AS CHAR INITIAL "asi167.df" NO-UNDO.
DEF VAR cDeltaFileName AS CHAR INITIAL "asi166167.df" NO-UNDO.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 slDatabases tbUpgradeDBs ~
tbBackupDBs bProcess 
&Scoped-Define DISPLAYED-OBJECTS fiCurrVer fiVerDate fiNewVer fiBackupDir ~
fiDbBackup fiSiteName fiHostname fiPgmBackup fiResBackup fiDrive fiTopDir ~
fiMapDir fiDbDir fiDlcDir fiDBDrive fiDbAuditDir fiDbDataDir ~
fiDeltaFilename fiDbProdDir fiDbShipDir fiDbStructDir fiDbTestDir ~
slDatabases tbComp-17 tbUpgradeDBs fiUpdatesDir tbComp-10 tbBackupDBs ~
fiPatchDir 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bProcess AUTO-END-KEY 
     LABEL "Start  Update" 
     SIZE 21 BY 2.43
     FONT 6.

DEFINE VARIABLE fiBackupDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiCurrVer AS CHARACTER FORMAT "X(256)":U INITIAL "16.6.0" 
     LABEL "Current Version" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiDbAuditDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiDbBackup AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiDbDataDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiDbDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiDBDrive AS CHARACTER FORMAT "X(256)":U 
     LABEL "Database Drive" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fiDbProdDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiDbShipDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiDbStructDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiDbTestDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiDeltaFilename AS CHARACTER FORMAT "X(256)":U INITIAL "asi166_167.df" 
     LABEL "Delta Filename" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiDlcDir AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\PROGRESS~\OE116" 
     LABEL "DBMS Directory" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE fiDrive AS CHARACTER FORMAT "X(256)":U INITIAL "C:" 
     LABEL "Physical Drive" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fiHostname AS CHARACTER FORMAT "X(256)":U INITIAL "DEMO" 
     LABEL "Server Name" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE fiMapDir AS CHARACTER FORMAT "X(256)":U INITIAL "N:" 
     LABEL "Mapped Drive" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fiNewVer AS CHARACTER FORMAT "X(256)":U INITIAL "16.7.4" 
     LABEL "New Version" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiPatchDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiPgmBackup AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiResBackup AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiSiteName AS CHARACTER FORMAT "X(256)":U INITIAL "DEMO" 
     LABEL "Site Name" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiTopDir AS CHARACTER FORMAT "X(256)":U INITIAL "ASIGUI" 
     LABEL "ASI Directory" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiUpdatesDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiVerDate AS DATE FORMAT "99/99/99":U INITIAL 10/01/17 
     LABEL "Installed On" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 7.14.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 5.71.

DEFINE VARIABLE slDatabases AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 43 BY 4.29 NO-UNDO.

DEFINE VARIABLE tbBackupDBs AS LOGICAL INITIAL no 
     LABEL "Backup Databases" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-10 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-17 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpgradeDBs AS LOGICAL INITIAL no 
     LABEL "Upgrade Databases" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiCurrVer AT ROW 2.43 COL 21 COLON-ALIGNED WIDGET-ID 66
     fiVerDate AT ROW 2.43 COL 51 COLON-ALIGNED WIDGET-ID 56
     fiNewVer AT ROW 2.43 COL 84 COLON-ALIGNED WIDGET-ID 46
     fiBackupDir AT ROW 2.43 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 502
     fiDbBackup AT ROW 3.14 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 506
     fiSiteName AT ROW 3.62 COL 25 COLON-ALIGNED WIDGET-ID 68
     fiHostname AT ROW 3.62 COL 69 COLON-ALIGNED WIDGET-ID 36
     fiPgmBackup AT ROW 3.86 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 520
     fiResBackup AT ROW 4.57 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 522
     fiDrive AT ROW 4.81 COL 25 COLON-ALIGNED WIDGET-ID 64
     fiTopDir AT ROW 4.81 COL 45 COLON-ALIGNED WIDGET-ID 62
     fiMapDir AT ROW 4.81 COL 89 COLON-ALIGNED WIDGET-ID 42
     fiDbDir AT ROW 5.29 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 510
     fiDlcDir AT ROW 6 COL 25 COLON-ALIGNED WIDGET-ID 44
     fiDBDrive AT ROW 6 COL 89 COLON-ALIGNED WIDGET-ID 478
     fiDbAuditDir AT ROW 6 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 504
     fiDbDataDir AT ROW 6.71 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 508
     fiDeltaFilename AT ROW 7.19 COL 25 COLON-ALIGNED WIDGET-ID 50
     fiDbProdDir AT ROW 7.43 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 512
     fiDbShipDir AT ROW 8.14 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 514
     fiDbStructDir AT ROW 8.86 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 516
     fiDbTestDir AT ROW 9.57 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 518
     slDatabases AT ROW 10.05 COL 11 NO-LABEL WIDGET-ID 484
     tbComp-17 AT ROW 10.05 COL 57 WIDGET-ID 496
     tbUpgradeDBs AT ROW 10.05 COL 63 WIDGET-ID 494
     fiUpdatesDir AT ROW 10.29 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 548
     tbComp-10 AT ROW 11 COL 57 WIDGET-ID 428
     tbBackupDBs AT ROW 11 COL 63 WIDGET-ID 384
     fiPatchDir AT ROW 11 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 546
     bProcess AT ROW 12.43 COL 112 WIDGET-ID 404
     " Databases" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 9.1 COL 9 WIDGET-ID 482
          FONT 6
     " General Variables" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 1.48 COL 8 WIDGET-ID 356
          FONT 6
     " Your Directory Structure" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 1.48 COL 107 WIDGET-ID 558
          FONT 6
     RECT-2 AT ROW 1.71 COL 5 WIDGET-ID 358
     RECT-3 AT ROW 9.33 COL 5 WIDGET-ID 362
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 166.2 BY 32.57
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
         TITLE              = "ASI Database Upgrade Processor"
         HEIGHT             = 14.33
         WIDTH              = 136.4
         MAX-HEIGHT         = 34.29
         MAX-WIDTH          = 180.6
         VIRTUAL-HEIGHT     = 34.29
         VIRTUAL-WIDTH      = 180.6
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
/* SETTINGS FOR FILL-IN fiBackupDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCurrVer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDbAuditDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDbBackup IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDbDataDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDbDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDBDrive IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDbProdDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDbShipDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDbStructDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDbTestDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDeltaFilename IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDlcDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDrive IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiHostname IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiMapDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiNewVer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPatchDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPgmBackup IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiResBackup IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSiteName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTopDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiUpdatesDir IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiVerDate IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-10 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-17 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* ASI Database Upgrade Processor */
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
ON WINDOW-CLOSE OF C-Win /* ASI Database Upgrade Processor */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON LEAVE OF FRAME DEFAULT-FRAME
ANYWHERE DO:
    RUN ipUpdateTTIniFile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bProcess C-Win
ON CHOOSE OF bProcess IN FRAME DEFAULT-FRAME /* Start  Update */
DO:
    RUN ipStatus ("Beginning Patch Application").
    ASSIGN
        SELF:LABEL = "Processing..."
        SELF:SENSITIVE = FALSE.

    RUN ipStatus ("Connecting selected database").
    
    IF tbBackupDBs:CHECKED IN FRAME {&FRAME-NAME} 
    AND tbBackupDBs:SENSITIVE THEN DO:
        DO iCtr = 1 TO NUM-ENTRIES(slDatabases:{&SV}):
            RUN ipBackupDBs (ENTRY(iCtr,slDatabases:{&SV})).
            ASSIGN
                tbComp-10:CHECKED = TRUE.
        END.        
        
    END.
    
    IF tbUpgradeDbs:CHECKED THEN DO:
        RUN ipUpgradeDBs.
        ASSIGN
            tbComp-17:CHECKED = TRUE.
    END.
    
    
    RUN ipStatus ("Database Schema Update Complete").
    RUN ipWriteIniFile.

    ASSIGN
        SELF:LABEL = "Start Update"
        SELF:SENSITIVE = TRUE
        fiCurrVer:{&SV} = fiNewVer:{&SV}
        fiVerDate:{&SV} = STRING(TODAY,"99/99/99").
        
    APPLY 'leave' to fiCurrVer.
    
    STATUS INPUT.

    IF NOT lSuccess THEN MESSAGE
        "There were no operations performed for this run."
        VIEW-AS ALERT-BOX.
        
    ASSIGN
        oplSuccess = lSuccess.
                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbUpgradeDBs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbUpgradeDBs C-Win
ON VALUE-CHANGED OF tbUpgradeDBs IN FRAME DEFAULT-FRAME /* Upgrade Databases */
DO:
    IF SELF:CHECKED THEN ASSIGN
        tbBackupDBs:CHECKED = TRUE.
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
    
    RUN ipCreateTTiniFile.
    RUN ipFindIniFile.
    IF cIniLoc NE "" THEN 
        RUN ipReadIniFile.
    RUN ipExpandVarNames.
    RUN ipSetDispVars.

    DO i = 1 TO NUM-ENTRIES(PROGRAM-NAME(1),"\"):
        IF ENTRY(i,PROGRAM-NAME(1),"\") BEGINS "PATCH" THEN ASSIGN
            cThisPatch = ENTRY(i,PROGRAM-NAME(1),"\").
    END.
    ASSIGN
        cThisPatch = SUBSTRING(cThisPatch,6).
    IF cThisPatch NE "" 
    AND cThisPatch NE ? THEN DO:
        ASSIGN
            fiNewVer:{&SV} = cThisPatch.
        APPLY 'leave' TO fiNewVer.
    END.
    ASSIGN
        tbUpgradeDBs:CHECKED = TRUE.
    APPLY 'value-changed' TO tbUpgradeDBs.
    
    APPLY 'entry' TO bProcess.
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
    
IF KEYFUNCTION(LASTKEY) = "END-ERROR" THEN QUIT.

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
  DISPLAY fiCurrVer fiVerDate fiNewVer fiBackupDir fiDbBackup fiSiteName 
          fiHostname fiPgmBackup fiResBackup fiDrive fiTopDir fiMapDir fiDbDir 
          fiDlcDir fiDBDrive fiDbAuditDir fiDbDataDir fiDeltaFilename 
          fiDbProdDir fiDbShipDir fiDbStructDir fiDbTestDir slDatabases 
          tbComp-17 tbUpgradeDBs fiUpdatesDir tbComp-10 tbBackupDBs fiPatchDir 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-2 RECT-3 slDatabases tbUpgradeDBs tbBackupDBs bProcess 
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
    DEF INPUT PARAMETER ipcDBIdent AS CHAR NO-UNDO.
    DEF VAR cCmdLine AS CHAR NO-UNDO.
    DEF VAR cLocItem AS CHAR NO-UNDO.
    DEF VAR cLocDir AS CHAR NO-UNDO.
    DEF VAR cLocName AS CHAR NO-UNDO.
    DEF VAR cLocPort AS CHAR NO-UNDO.
    
    RUN ipStatus ("Backing Up Selected Databases").
    
        ASSIGN
            cLocItem = ipcDbIdent
            cLocDir  = ENTRY(1,cLocItem,"-")
            cLocName = ENTRY(2,cLocItem,"-")
            cLocPort = ENTRY(3,cLocItem,"-").
        IF fiDbDrive:{&SV} EQ fiDrive:{&SV} THEN ASSIGN
            cCmdLine = fiDlcDir:{&SV} + "\bin\probkup online " + 
                       fiDrive:{&SV} + "\" + 
                       fiTopDir:{&SV} + "\" + 
                       fiDbDir:{&SV} + "\" +
                       cLocDir + "\" +
                       cLocName + " " + 
                       cDbBackup + "\" + cLocName + 
                       STRING(YEAR(TODAY)) +
                       STRING(MONTH(TODAY),"99") +
                       STRING(DAY(TODAY),"99") + ".bak".
        ELSE ASSIGN
            cCmdLine = fiDlcDir:{&SV} + "\bin\probkup online " + 
                       fiDBDrive:{&SV} + "\" + 
                       fiDbDir:{&SV} + "\" +
                       cLocDir + "\" +
                       cLocName + " " + 
                       cDbBackup + "\" + cLocName + 
                       STRING(YEAR(TODAY)) +
                       STRING(MONTH(TODAY),"99") +
                       STRING(DAY(TODAY),"99") + ".bak".
    
        RUN ipStatus ("  Backing Up " + ENTRY(2,ipcDbIdent,"-")).
        RUN ipStatus ("  using command " + cCmdLine).
        OS-COMMAND SILENT VALUE(cCmdLine).
        
        IF SEARCH(cDbBackup + "\" + cLocName + 
                  STRING(YEAR(TODAY)) +
                  STRING(MONTH(TODAY),"99") +
                  STRING(DAY(TODAY),"99") + ".bak") NE ? THEN ASSIGN
            lSuccess = TRUE.

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
    
    RUN ipStatus ("Creating associatated AUDIT database").
    
    ASSIGN
        cLocItem = ipcDbIdent
        cLocDir  = ENTRY(1,cLocItem,"-")
        cLocName = ENTRY(2,cLocItem,"-")
        cLocPort = ENTRY(3,cLocItem,"-")
        cStructureST = fiDrive:{&SV} + "\" + fiTopDir:{&SV} + "\" +
                   fiUpdatesDir:{&SV} + "\" + fiPatchDir:{&SV} + "\" +
                   "StructureUpdate\STFiles\audit.st"
        cAuditDf = fiDrive:{&SV} + "\" + fiTopDir:{&SV} + "\" +
                   fiUpdatesDir:{&SV} + "\" + fiPatchDir:{&SV} + "\" +
                   "StructureUpdate\DFFiles\asiAudit.df".

    IF fiDbDrive:{&SV} EQ fiDrive:{&SV} THEN ASSIGN
        cCmdLine = fiDlcDir:{&SV} + '\bin\prostrct create ' + 
                   cDbAuditDir + '\aud' +
                   SUBSTRING(cLocName,1,8) + ' "' + 
                   cStructureST + '"'.
    ELSE ASSIGN
        cCmdLine = fiDlcDir:{&SV} + '\bin\prostrct create ' + 
                   cDbAuditDir + '\aud' +
                   SUBSTRING(cLocName,1,8) + ' "' + 
                   cStructureST + '"'.
 
    RUN ipSetCurrentDir (cDbAuditDir). 
    RUN ipStatus ("  Creating audit db aud" + SUBSTRING(ENTRY(2,ipcDbIdent,"-"),1,8)).
    OS-COMMAND SILENT VALUE(cCmdLine).
    
    IF fiDbDrive:{&SV} EQ fiDrive:{&SV} THEN ASSIGN
        cCmdLine = fiDlcDir:{&SV} + "\bin\procopy " + 
                   fiDlcDir:{&SV} + "\empty4 " + 
                   cDbAuditDir + "\aud" +
                   SUBSTRING(cLocName,1,8).
    ELSE ASSIGN
        cCmdLine = fiDlcDir:{&SV} + "\bin\procopy " + 
                   fiDlcDir:{&SV} + "\empty4 " + 
                   cDbAuditDir + "\aud" +
                   SUBSTRING(cLocName,1,8).

    RUN ipStatus ("  Copying metaschema data...").
    OS-COMMAND SILENT VALUE(cCmdLine).

        ASSIGN
            /* Single user connect statment */
            cStatement = "-db " + 
                         cDbAuditDir + "\aud" +
                         SUBSTRING(cLocName,1,8) +
                         " -1 -ld audDB" + STRING(iCtr).
        /* Connect to the database single user */
        CONNECT VALUE(cStatement).
        CREATE ALIAS DICTDB FOR DATABASE VALUE("audDB" + STRING(iCtr)).
        /* Load the delta */
        RUN ipStatus ("  Loading schema...").
        RUN prodict/load_df.p (cAuditDf). 
                
        /* Disconnect it */
        RUN ipStatus ("  Disconnecting.").
        DISCONNECT VALUE("audDB" + STRING(iCtr)).
        
    MESSAGE
        "You must manually add this database to the system with OE Explorer"
        view-as alert-box.
        
    
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
    RUN ipStatus ("Creating ttIniFile...").

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
    RUN ipStatus ("Expanding variable names...").


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
        lConnectAudit = IF INDEX(cConnectAudit,"Y") NE 0 OR INDEX(cConnectAudit,"T") NE 0 THEN TRUE ELSE FALSE
        cLockoutTries = SUBSTRING(cLockoutTries,1,1)
        iLockoutTries = IF cLockoutTries NE "" 
                        AND ASC(cLockoutTries) GE 48
                        AND ASC(cLockoutTries) LE 57 THEN INT(cLockoutTries) ELSE 0
        cPatchNo = fiNewVer:{&SV}
        iDbCurrVer = (INTEGER(ENTRY(1,fiCurrVer:{&SV},".")) * 10) +
                     INTEGER(ENTRY(2,fiCurrVer:{&SV},"."))
        iDbTgtVer = (INTEGER(ENTRY(1,fiNewVer:{&SV},".")) * 10) +
                     INTEGER(ENTRY(2,fiNewVer:{&SV},"."))
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
    RUN ipStatus ("Locating advantzware.ini file...").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipReadIniFile C-Win 
PROCEDURE ipReadIniFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Reading advantzware.ini file...").

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
    
    RUN ipStatus ("Setting current directory to " + ipTgtDir).

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
    RUN ipStatus ("Setting display variables...").

    DO:
        ASSIGN
            slDatabases:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
        
        FOR EACH ttIniFile:
            CASE ttIniFile.cVarName:
                WHEN "siteName" THEN ASSIGN fiSiteName:{&SV} = ttIniFile.cVarValue.
                WHEN "hostname" THEN ASSIGN fiHostname:{&SV} = ttIniFile.cVarValue.
                WHEN "drive" THEN ASSIGN fiDrive:{&SV} = ttIniFile.cVarValue.
                WHEN "dbDrive" THEN ASSIGN fiDbDrive:{&SV} = ttIniFile.cVarValue.
                WHEN "topDir" THEN ASSIGN fiTopDir:{&SV} = ttIniFile.cVarValue.
                WHEN "mapDir" THEN ASSIGN fiMapDir:{&SV} = ttIniFile.cVarValue.
                WHEN "DLCDir" THEN ASSIGN fiDLCDir:{&SV} = ttIniFile.cVarValue.
                WHEN "currVer" THEN ASSIGN fiCurrVer:{&SV} = ttIniFile.cVarValue.
                WHEN "verDate" THEN ASSIGN fiVerDate:{&SV} = ttIniFile.cVarValue.
                WHEN "connectAudit" THEN.
                WHEN "makeBackup" THEN.
                WHEN "backupDir" THEN ASSIGN fiBackupDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbDir" THEN ASSIGN fiDbDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updatesDir" THEN ASSIGN fiUpdatesDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbBackup" THEN ASSIGN fiDbBackup:{&SV} = ttIniFile.cVarValue.
                WHEN "pgmBackup" THEN ASSIGN fiPgmBackup:{&SV} = ttIniFile.cVarValue.
                WHEN "resBackup" THEN ASSIGN fiResBackup:{&SV} = ttIniFile.cVarValue.
                WHEN "dbAuditDir" THEN ASSIGN fiDbAuditDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbDataDir" THEN ASSIGN fiDbDataDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbProdDir" THEN ASSIGN fiDbProdDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbShipDir" THEN ASSIGN fiDbShipDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbStructDir" THEN ASSIGN fiDbStructDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbTestDir" THEN ASSIGN fiDbTestDir:{&SV} = ttIniFile.cVarValue.
                WHEN "deltaFileName" THEN ASSIGN fiDeltaFileName:{&SV} = ttIniFile.cVarValue.
                
                WHEN "dbList" THEN DO iCtr = 1 TO NUM-ENTRIES(ttIniFile.cVarValue):
                    slDatabases:ADD-LAST(ENTRY(iCtr,cDbDirList) + "-" + ENTRY(iCtr,cDbList) + "-" + 
                                         ENTRY(iCtr,cDbPortList) + "-" + ENTRY(iCtr,cDbVerList)).
                END. 
                

            END CASE.
        END.
    END.

    slDatabases:SCREEN-VALUE = ipcDir + "-" + ipcName + "-" + ipcPort + "-" + ipcVer.
    APPLY 'value-changed' TO slDatabases.
    
    ASSIGN
        fiPatchDir:{&SV} = "PATCH" + fiNewVer:{&SV}.
    
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
                
    STATUS INPUT ipcStatus + "...".

    IF cUpdatesDir <> "Updates" THEN DO:
        IF INDEX(ipcStatus,"duplicate") EQ 0 THEN DO:
            ASSIGN
                cLogFile = cUpdatesDir + "\" + "Patch" + fiNewVer:{&SV} + "\installLog.txt"
                iMsgCtr = iMsgCtr + 1
                cMsgStr[iMsgCtr] = ipcStatus + "...".
            OUTPUT STREAM logStream TO VALUE(cLogFile) APPEND.
            PUT STREAM logStream
                STRING(TODAY,"99/99/99") AT 1
                STRING(TIME,"HH:MM:SS") AT 12
                cMsgStr[iMsgCtr] FORMAT "x(160)" AT 25
                SKIP.
            OUTPUT STREAM logStream CLOSE.
        END.
    END.
        
    PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpdateTTIniFile C-Win 
PROCEDURE ipUpdateTTIniFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ttIniFile:
        CASE ttIniFile.cVarName:
            WHEN "siteName" THEN ASSIGN ttIniFile.cVarValue = fiSiteName:{&SV}.
            WHEN "hostname" THEN ASSIGN ttIniFile.cVarValue = fiHostName:{&SV}.
            WHEN "drive" THEN ASSIGN ttIniFile.cVarValue = fiDrive:{&SV}.
            WHEN "topDir" THEN ASSIGN ttIniFile.cVarValue = fiTopDir:{&SV}.
            WHEN "mapDir" THEN ASSIGN ttIniFile.cVarValue = fiMapDir:{&SV}.
            WHEN "currVer" THEN ASSIGN ttIniFile.cVarValue = fiCurrVer:{&SV}.
            WHEN "verDate" THEN ASSIGN ttIniFile.cVarValue = fiVerDate:{&SV}.
            WHEN "DLCDir" THEN ASSIGN ttIniFile.cVarValue = fiDLCDir:{&SV}.
            WHEN "backupDir" THEN ASSIGN ttIniFile.cVarValue = fiBackupDir:{&SV}.
            WHEN "dbDir" THEN ASSIGN ttIniFile.cVarValue = fiDbDir:{&SV}.
            WHEN "updateDir" THEN ASSIGN ttIniFile.cVarValue = fiUpdatesDir:{&SV}.
            WHEN "dbBackup" THEN ASSIGN ttIniFile.cVarValue = fiDbBackup:{&SV}.
            WHEN "pgmBackup" THEN ASSIGN ttIniFile.cVarValue = fiPgmBackup:{&SV}.
            WHEN "resBackup" THEN ASSIGN ttIniFile.cVarValue = fiResBackup:{&SV}.
            WHEN "dbDataDir" THEN ASSIGN ttIniFile.cVarValue = fiDbDataDir:{&SV}.
            WHEN "dbProdDir" THEN ASSIGN ttIniFile.cVarValue = fiDbProdDir:{&SV}.
            WHEN "dbShipDir" THEN ASSIGN ttIniFile.cVarValue = fiDbShipDir:{&SV}.
            WHEN "dbStructDir" THEN ASSIGN ttIniFile.cVarValue = fiDbStructDir:{&SV}.
            WHEN "dbTestDir" THEN ASSIGN ttIniFile.cVarValue = fiDbTestDir:{&SV}.
            WHEN "deltaFileName" THEN ASSIGN ttIniFile.cVarValue = fiDeltaFileName:{&SV}.
        END CASE.
    END.
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

    DO iCtr = 1 TO NUM-ENTRIES(slDatabases:{&SV}):
        ASSIGN
            cThisEntry = ENTRY(iCtr,slDatabases:{&SV})
            iListEntry = LOOKUP(cThisEntry,slDatabases:LIST-ITEMS).          

        IF DECIMAL(ENTRY(4,ENTRY(iCtr,slDatabases:{&SV}),"-")) * 10 LT iDbTgtVer THEN DO:
            /*
            MESSAGE 
                "Database " + ENTRY(2,ENTRY(iCtr,slDatabases:{&SV}),"-") + " is currently at version " +
                STRING(DECIMAL(ENTRY(4,ENTRY(iCtr,slDatabases:{&SV}),"-"))) + "." SKIP
                "You are about to upgrade it to version " + STRING(iDbTgtVer / 10) + "." SKIP
                "Is this correct?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lUpdNow AS LOG.
            IF lUpdNow THEN */
            DO:
                RUN ipBackupDBs (ENTRY(iCtr,slDatabases:{&SV})).
                RUN ipStatus ("Upgrading " + ENTRY(2,ENTRY(iCtr,slDatabases:{&SV}),"-")).
                ASSIGN
                    /* This results in a value similar to 'asi166_167.df' */
                    cDeltaDf = "asi" + 
                               STRING(DECIMAL(ENTRY(4,ENTRY(iCtr,slDatabases:{&SV}),"-")) * 10) +
                               "_" + STRING(iDbTgtVer) + ".df"
                    /* This fully qualifies the path name to the delta */
                    cDeltaDf = fiDrive:{&SV} + "\" + fiTopDir:{&SV} + "\" +
                               fiUpdatesDir:{&SV} + "\" + fiPatchDir:{&SV} + "\" +
                               "StructureUpdate\DFFiles\" + cDeltaDf
                    cStopString = fiDlcDir:{&SV} + "\bin\dbman" + 
                                 " -host " + fiHostName:{&SV} + 
                                 " -port " + cAdminPort + 
                                 " -database " + ENTRY(2,ENTRY(iCtr,slDatabases:{&SV}),"-") + 
                                 " -stop"
                    cStartString = fiDlcDir:{&SV} + "\bin\dbman" + 
                                 " -host " + fiHostName:{&SV} + 
                                 " -port " + cAdminPort + 
                                 " -database " + ENTRY(2,ENTRY(iCtr,slDatabases:{&SV}),"-") + 
                                 " -start"
                    /* Single user connect statment */
                    cStatement = "-db " + fiDbDrive:{&SV} + "\" +
                                 fiTopDir:{&SV} + "\" + fiDbDir:{&SV} + "\" + 
                                 ENTRY(1,ENTRY(iCtr,slDatabases:{&SV}),"-") + "\" +
                                 ENTRY(2,ENTRY(iCtr,slDatabases:{&SV}),"-") + 
                                 " -1 -ld updDB" + STRING(iCtr)
                    cLockFile = fiDbDrive:{&SV} + "\" +
                                 fiTopDir:{&SV} + "\" + fiDbDir:{&SV} + "\" + 
                                 ENTRY(1,ENTRY(iCtr,slDatabases:{&SV}),"-") + "\" +
                                 ENTRY(2,ENTRY(iCtr,slDatabases:{&SV}),"-") + ".lk".
                
                /* Unserve the database */
                RUN ipStatus ("  Stopping database service using string " + cStopString).
                OS-COMMAND SILENT VALUE(cStopString).
                /* May have to wait for DB to shut down */
                DO WHILE SEARCH(cLockFile) NE ?:
                    RUN ipStatus ("  Waiting for removal of lock file " + cLockFile).
                    PAUSE 2 NO-MESSAGE.
                END.
                /* Connect to the database single user */
                RUN ipStatus ("  Connecting single-user using string " + cStatement).
                CONNECT VALUE(cStatement).
                RUN ipStatus ("  Creating DICTDB alias for updDB " + STRING(iCtr)).
                CREATE ALIAS DICTDB FOR DATABASE VALUE("updDB" + STRING(iCtr)).
                /* Load the delta */
                RUN ipStatus ("  Loading delta " + STRING(cDeltaDf)).
                RUN prodict/load_df.p (cDeltaDf). 
                
                /* Disconnect it */
                RUN ipStatus ("  Disconnecting " + ENTRY(2,ENTRY(iCtr,slDatabases:{&SV}),"-")).
                DISCONNECT VALUE("updDB" + STRING(iCtr)).
                
                /* Re-Serve it */
                RUN ipStatus ("  Serving " + ENTRY(2,ENTRY(iCtr,slDatabases:{&SV}),"-")).
                OS-COMMAND SILENT VALUE(cStartString).
                ASSIGN
                    lSuccess = TRUE
                    cThisEntry = ENTRY(iCtr,slDatabases:{&SV})
                    cReplEntry = REPLACE(cThisEntry,
                                     ("-" + STRING(DECIMAL(ENTRY(4,ENTRY(iCtr,slDatabases:{&SV}),"-")))),
                                     "-" + STRING(DECIMAL(iDbTgtVer / 10)))
                    cNewList = REPLACE(slDatabases:LIST-ITEMS,cThisEntry,cReplEntry)
                    cNewSel = REPLACE(slDatabases:{&SV},cThisEntry,cReplEntry)
                    slDatabases:list-items = cNewList
                    slDatabases:{&SV} = cNewSel
                    cNewList = ""
                    cNewSel = "".
            END.
        END.
        ELSE MESSAGE
            "Database " + ENTRY(2,ENTRY(iCtr,slDatabases:{&SV}),"-") + " is already at the current" SKIP
            "version level.  It should not be updated again"
            VIEW-AS ALERT-BOX INFO.

        IF ENTRY(iListEntry,cAudDbList) = "x" 
        OR ENTRY(iListEntry,cAudDbList) = "x" THEN DO:
            MESSAGE
                "There is no audit database associated with database " + ENTRY(2,ENTRY(iCtr,slDatabases:{&SV}),"-") + "." SKIP
                "Would you like to create one now?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lCreate AS LOG.
            IF lCreate THEN
                RUN ipCreateAudit (cThisEntry, iListEntry).
            ASSIGN
                ENTRY(iListEntry,cAudDbList) = "aud" + SUBSTRING(ENTRY(2,cThisEntry,"-"),1,8)
                ENTRY(iListEntry,cAudDirList) = "Audit"
                ENTRY(iListEntry,cAudPortList) = string(integer(cAdminPort) + 10,"9999").
        END.
                    
    END.

    ASSIGN
        tbComp-17:CHECKED = TRUE.

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

    RUN ipStatus ("Writing modified advantzware.ini file...").

    FIND ttIniFile WHERE ttIniFile.cVarName = "dbDirList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = "".
    FIND ttIniFile WHERE ttIniFile.cVarName = "dbList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = "".
    FIND ttIniFile WHERE ttIniFile.cVarName = "dbPortList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = "".
    FIND ttIniFile WHERE ttIniFile.cVarName = "dbVerList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = "".
    DO iCtr = 1 TO NUM-ENTRIES(slDatabases:LIST-ITEMS IN FRAME {&FRAME-NAME}):
        ASSIGN
            cThisElement = ENTRY(iCtr,slDatabases:LIST-ITEMS).
        FIND ttIniFile WHERE ttIniFile.cVarName = "dbDirList" NO-ERROR.
        ASSIGN ttIniFile.cVarValue = ttIniFile.cVarValue + ENTRY(1,cThisElement,"-") + ",".
        FIND ttIniFile WHERE ttIniFile.cVarName = "dbList" NO-ERROR.
        ASSIGN ttIniFile.cVarValue = ttIniFile.cVarValue + ENTRY(2,cThisElement,"-") + ",".
        FIND ttIniFile WHERE ttIniFile.cVarName = "dbPortList" NO-ERROR.
        ASSIGN ttIniFile.cVarValue = ttIniFile.cVarValue + ENTRY(3,cThisElement,"-") + ",".
        FIND ttIniFile WHERE ttIniFile.cVarName = "dbVerList" NO-ERROR.
        ASSIGN ttIniFile.cVarValue = ttIniFile.cVarValue + ENTRY(4,cThisElement,"-") + ",".
    END.
    FIND ttIniFile WHERE ttIniFile.cVarName = "dbDirList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = TRIM(ttIniFile.cVarValue,",").
    FIND ttIniFile WHERE ttIniFile.cVarName = "dbList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = TRIM(ttIniFile.cVarValue,",").
    FIND ttIniFile WHERE ttIniFile.cVarName = "dbPortList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = TRIM(ttIniFile.cVarValue,",").
    FIND ttIniFile WHERE ttIniFile.cVarName = "dbVerList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = TRIM(ttIniFile.cVarValue,",").

    FIND ttIniFile WHERE ttIniFile.cVarName = "audDbList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = cAudDbList.
    FIND ttIniFile WHERE ttIniFile.cVarName = "audDirList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = cAudDirList.
    FIND ttIniFile WHERE ttIniFile.cVarName = "audPortList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = cAudPortList.

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

