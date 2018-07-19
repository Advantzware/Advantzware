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
/*
/* FOR TEST PURPOSES ONLY */
DEF VAR ipcName AS CHAR NO-UNDO.
DEF VAR ipcPort AS CHAR NO-UNDO.
DEF VAR ipcDir AS CHAR NO-UNDO.
DEF VAR ipcVer AS CHAR NO-UNDO.
DEF VAR ipiLevel AS INT NO-UNDO.
DEF VAR oplSuccess AS LOG NO-UNDO.
ASSIGN
    ipcName = "asiTest167"
    ipcPort = "2856"
    ipcDir = "Test"
    ipcVer = "16.7"
    ipiLevel = 10.
*/
/* Local Variable Definitions ---                                       */
&SCOPED-DEFINE SV SCREEN-VALUE IN FRAME DEFAULT-FRAME

DEF STREAM s1.

DEF TEMP-TABLE ttAuditTbl LIKE AuditTbl.
DEF TEMP-TABLE ttPrgms LIKE prgrms.
DEF TEMP-TABLE ttPrgmxref LIKE prgmxref.
DEF TEMP-TABLE ttEmailcod LIKE emailcod.
DEF TEMP-TABLE ttNotes LIKE notes.
DEF TEMP-TABLE ttModule LIKE module.
DEF TEMP-TABLE ttLookups LIKE lookups.
DEF TEMP-TABLE ttReftable LIKE reftable.
DEF TEMP-TABLE ttSysCtrl LIKE sys-ctrl.
DEF TEMP-TABLE ttSysCtrlShipto LIKE sys-ctrl-shipto.
DEF TEMP-TABLE tempUser NO-UNDO LIKE _User.
DEF TEMP-TABLE ttIniFile
    FIELD iPos AS INT
    FIELD cRaw AS CHAR
    FIELD cVarName AS CHAR
    FIELD cVarValue AS CHAR
    INDEX idxPos IS PRIMARY UNIQUE iPos.
DEF TEMP-TABLE ttUsers
    FIELD ttfuserid AS CHAR
    FIELD ttfdbname AS CHAR
    FIELD ttfalias AS CHAR
    FIELD ttfenvlist AS CHAR
    FIELD ttfdblist AS CHAR
    FIELD ttfmodelist AS CHAR.
 

DEF BUFFER bnotes FOR notes.
DEF BUFFER bf-usercomp FOR usercomp.
DEF BUFFER bf-module FOR MODULE.

DEF STREAM outStream.
DEF STREAM logStream.
DEF STREAM iniStream.

DEF VAR cBadDirList AS CHAR NO-UNDO.
DEF VAR cfrom AS CHAR.
DEF VAR cIniLine AS CHAR NO-UNDO.
DEF VAR cIniLoc AS CHAR NO-UNDO.
DEF VAR cIniVarList AS CHAR NO-UNDO.
DEF VAR cMapDrive AS CHAR FORMAT "x(2)" NO-UNDO.
DEF VAR cMsgStr AS CHAR FORMAT "x(80)" EXTENT 100 NO-UNDO.
DEF VAR connectStatement AS CHAR NO-UNDO.
DEF VAR cPassword AS CHAR FORMAT "x(24)" LABEL "Password" NO-UNDO.
DEF VAR cPatchNo AS CHAR FORMAT "x(8)" NO-UNDO.
DEF VAR cRunPgm AS CHAR NO-UNDO.
DEF VAR cTemp AS CHAR NO-UNDO.
DEF VAR cTestDir AS CHAR NO-UNDO.
DEF VAR cThisPatch AS CHAR NO-UNDO.
DEF VAR cTo AS CHAR.
DEF VAR cUserID AS CHAR FORMAT "x(8)" LABEL "User ID" NO-UNDO.
DEF VAR cUsrLine AS CHAR NO-UNDO.
DEF VAR cUsrList AS CHAR NO-UNDO.
DEF VAR cVarName AS CHAR EXTENT 100 NO-UNDO.
DEF VAR cVarValue AS CHAR EXTENT 100 NO-UNDO.
DEF VAR delCtr AS INT NO-UNDO.
DEF VAR dupCtr AS INT NO-UNDO.
DEF VAR hPreRun AS HANDLE.
DEF VAR i AS INT NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR iCurrVerExt AS INT NO-UNDO.
DEF VAR iDBCurrVer AS INT NO-UNDO.
DEF VAR iDBTgtVer AS INT NO-UNDO.
DEF VAR iExtra AS INT NO-UNDO.
DEF VAR iLastIni AS INT NO-UNDO.
DEF VAR iLockoutTries AS INT NO-UNDO.
DEF VAR iMsgCtr AS INT NO-UNDO.
DEF VAR iNumberChecked AS INT NO-UNDO.
DEF VAR iNumUsers AS INT NO-UNDO.
DEF VAR iUserCount AS INT NO-UNDO.
DEF VAR jCtr AS INT NO-UNDO.
DEF VAR lAllOK AS LOG NO-UNDO.
DEF VAR lAutorun AS LOG NO-UNDO.
DEF VAR lConnectAudit AS LOG NO-UNDO.
DEF VAR lCorrupt AS LOG NO-UNDO.
DEF VAR lFoundIni AS LOG NO-UNDO.
DEF VAR lFoundUsr AS LOG NO-UNDO.
DEF VAR ll-ans AS LOG NO-UNDO.
DEF VAR lMakeBackup AS LOG NO-UNDO.
DEF VAR lNeedUsercontrol AS LOG NO-UNDO.
DEF VAR lSuccess AS LOG NO-UNDO.
DEF VAR lSysError AS LOG NO-UNDO.
DEF VAR lUpdUsr AS LOG NO-UNDO.
DEF VAR lValidDB AS LOG NO-UNDO.
DEF VAR origPropath AS CHAR NO-UNDO.
DEF VAR timestring AS CHAR NO-UNDO.
DEF VAR tslogin-cha AS CHAR NO-UNDO.
DEF VAR v1 AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v2 AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v3 LIKE lookups.frame_field NO-UNDO.
DEF VAR v4 LIKE lookups.prgmname NO-UNDO.
DEF VAR v5 LIKE lookups.rec_key NO-UNDO.
DEF VAR xDbDir AS CHAR NO-UNDO.

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
DEF VAR cEnvVerList AS CHAR INITIAL "16.7.12" NO-UNDO.
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
DEF VAR cAdminPort AS CHAR INITIAL "20942.st" NO-UNDO.
DEF VAR cDfFileName AS CHAR INITIAL "asi167.df" NO-UNDO.
DEF VAR cDeltaFileName AS CHAR INITIAL "asi166167.df" NO-UNDO.

/* END advantzware.ini Variables */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 fiCurrVer ~
fiVerDate fiNewVer fiAdminDir fiDbAdmin fiSiteName fiHostname fiEnvAdmin ~
fiBackupDir fiDrive fiTopDir fiMapDir fiDbBackup fiDlcDir fiDBDrive ~
fiPgmBackup fiResBackup fiDfFilename fiDeltaFilename fiDbDir fiDbAuditDir ~
fiLockoutTries fiLicensedUsers fiDbDataDir fiDbProdDir fiDbShipDir ~
slEnvironments slDatabases fiDbStructDir fiDbTestDir fiDeskDir fiDocDir ~
fiEnvDir fiEnvProdDir fiEnvAddonDir fiEnvCustFiles fiEnvCustomerDir ~
fiEnvOverrideDir tbBackupDBs tbRunDataFix fiEnvPoDir fiEnvProgramsDir ~
tbUserControl tbDelDupeNotes fiEnvResourceDir tbUserCleanup tbUpdateNK1s ~
fiEnvScheduleDir tbDelBadData fiEnvTemplateDir fiEnvUserMenuDir ~
tbUpdateMaster tbReftableConv fiEnvUsersDir fiInstallDir fiUpdatesDir ~
tbLoadMenus fiPatchDir fiUpdAdminDir fiUpdCompressDir fiUpdDataDir ~
tbRelNotes fiUpdDataUpdateDir fiUpdDeskDir bProcess fiUpdMenuDir ~
tbInstallFiles fiUpdProgramDir tbUpdateIni fiUpdRelNotesDir fiUpdSqlDir ~
fiUpdStructureDir 
&Scoped-Define DISPLAYED-OBJECTS fiCurrVer fiVerDate fiNewVer fiAdminDir ~
fiDbAdmin fiSiteName fiHostname fiEnvAdmin fiBackupDir fiDrive fiTopDir ~
fiMapDir fiDbBackup fiDlcDir fiDBDrive fiPgmBackup fiResBackup fiDfFilename ~
fiDeltaFilename fiDbDir fiDbAuditDir fiLockoutTries fiLicensedUsers ~
fiDbDataDir fiDbProdDir fiDbShipDir slEnvironments slDatabases ~
fiDbStructDir fiDbTestDir fiDeskDir fiDocDir fiEnvDir fiEnvProdDir ~
fiEnvAddonDir fiEnvCustFiles fiEnvCustomerDir fiEnvOverrideDir tbComp-10 ~
tbBackupDBs tbComp-7 tbRunDataFix fiEnvPoDir fiEnvProgramsDir tbComp-1 ~
tbUserControl tbComp-13 tbDelDupeNotes fiEnvResourceDir tbComp-2 ~
tbUserCleanup tbComp-14 tbUpdateNK1s fiEnvScheduleDir tbComp-4 tbDelBadData ~
tbComp-15 tbUpdateFileLocs fiEnvTemplateDir fiEnvUserMenuDir tbComp-5 ~
tbUpdateMaster tbComp-17 tbReftableConv fiEnvUsersDir fiInstallDir ~
fiUpdatesDir tbComp-6 tbLoadMenus fiPatchDir fiUpdAdminDir tbComp-3 ~
tbBuildDirs fiUpdCompressDir tbComp-8 tbUpdateStartup fiUpdDataDir tbComp-9 ~
tbRelNotes fiUpdDataUpdateDir fiUpdDeskDir tbComp-11 tbBackupFiles ~
fiUpdMenuDir tbComp-12 tbInstallFiles fiUpdProgramDir tbComp-16 tbUpdateIni ~
fiUpdRelNotesDir fiUpdSqlDir fiUpdStructureDir 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD intVer C-Win 
FUNCTION intVer RETURNS INTEGER
  ( INPUT cVerString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bProcess 
     LABEL "Start  Update" 
     SIZE 21 BY 2.43
     FONT 6.

DEFINE VARIABLE fiAdminDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiBackupDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiCurrVer AS CHARACTER FORMAT "X(256)":U INITIAL "16.6.0" 
     LABEL "Current Version" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiDbAdmin AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

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

DEFINE VARIABLE fiDeltaFilename AS CHARACTER FORMAT "X(256)":U INITIAL "asi165166.df" 
     LABEL "Delta Filename" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiDeskDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiDfFilename AS CHARACTER FORMAT "X(256)":U INITIAL "asi166.df" 
     LABEL "DF Filename" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiDlcDir AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\PROGRESS~\OE116" 
     LABEL "DBMS Directory" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE fiDocDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiDrive AS CHARACTER FORMAT "X(256)":U INITIAL "C:" 
     LABEL "Physical Drive" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fiEnvAddonDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiEnvAdmin AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiEnvCustFiles AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiEnvCustomerDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiEnvDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiEnvOverrideDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiEnvPoDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiEnvProdDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiEnvProgramsDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiEnvResourceDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiEnvScheduleDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiEnvTemplateDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiEnvUserMenuDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiEnvUsersDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiHostname AS CHARACTER FORMAT "X(256)":U INITIAL "DEMO" 
     LABEL "Server Name" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE fiInstallDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiLicensedUsers AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Licensed Users" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE fiLockoutTries AS CHARACTER FORMAT "X(256)":U INITIAL "4" 
     LABEL "Lockout Tries" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fiMapDir AS CHARACTER FORMAT "X(256)":U INITIAL "N:" 
     LABEL "Mapped Drive" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fiNewVer AS CHARACTER FORMAT "X(256)":U INITIAL "16.7.12" 
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

DEFINE VARIABLE fiUpdAdminDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiUpdatesDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiUpdCompressDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiUpdDataDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiUpdDataUpdateDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiUpdDeskDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiUpdMenuDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiUpdProgramDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiUpdRelNotesDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiUpdSqlDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiUpdStructureDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiVerDate AS DATE FORMAT "99/99/99":U INITIAL 10/01/17 
     LABEL "Installed On" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 57 BY 31.43.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 8.1.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 5.24.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100 BY 17.14.

DEFINE VARIABLE slDatabases AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 43 BY 3.57 NO-UNDO.

DEFINE VARIABLE slEnvironments AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 36 BY 3.57 NO-UNDO.

DEFINE VARIABLE tbBackupDBs AS LOGICAL INITIAL no 
     LABEL "Backup Databases" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbBackupFiles AS LOGICAL INITIAL no 
     LABEL "Backup System Files" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbBuildDirs AS LOGICAL INITIAL no 
     LABEL "Build/Update Directories" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-1 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-10 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-11 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-12 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-13 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-14 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-15 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-16 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-17 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-2 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-3 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-4 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-5 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-6 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-7 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-8 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbComp-9 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tbDelBadData AS LOGICAL INITIAL no 
     LABEL "Remove deprecated records" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbDelDupeNotes AS LOGICAL INITIAL no 
     LABEL "Delete duplicate notes" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbInstallFiles AS LOGICAL INITIAL no 
     LABEL "Install new System Files" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbLoadMenus AS LOGICAL INITIAL no 
     LABEL "Load new Menu files" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbReftableConv AS LOGICAL INITIAL no 
     LABEL "Convert Reftable elements" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbRelNotes AS LOGICAL INITIAL no 
     LABEL "Copy Release Notes" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbRunDataFix AS LOGICAL INITIAL no 
     LABEL "Run Data Fix programs" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateFileLocs AS LOGICAL INITIAL no 
     LABEL "Update hardcoded file locations" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateIni AS LOGICAL INITIAL no 
     LABEL "Update advantzware.ini file" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateMaster AS LOGICAL INITIAL no 
     LABEL "Update Master records" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateNK1s AS LOGICAL INITIAL no 
     LABEL "Update NK1 records" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateStartup AS LOGICAL INITIAL no 
     LABEL "Create/Update Startup files" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbUserCleanup AS LOGICAL INITIAL no 
     LABEL "Cleanup user files" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbUserControl AS LOGICAL INITIAL no 
     LABEL "Create/Update User Control" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiCurrVer AT ROW 2.43 COL 21 COLON-ALIGNED WIDGET-ID 66
     fiVerDate AT ROW 2.43 COL 51 COLON-ALIGNED WIDGET-ID 56
     fiNewVer AT ROW 2.43 COL 84 COLON-ALIGNED WIDGET-ID 46
     fiAdminDir AT ROW 2.43 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     fiDbAdmin AT ROW 3.14 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     fiSiteName AT ROW 3.62 COL 25 COLON-ALIGNED WIDGET-ID 68
     fiHostname AT ROW 3.62 COL 69 COLON-ALIGNED WIDGET-ID 36
     fiEnvAdmin AT ROW 3.86 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     fiBackupDir AT ROW 4.57 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     fiDrive AT ROW 4.81 COL 25 COLON-ALIGNED WIDGET-ID 64
     fiTopDir AT ROW 4.81 COL 45 COLON-ALIGNED WIDGET-ID 62
     fiMapDir AT ROW 4.81 COL 89 COLON-ALIGNED WIDGET-ID 42
     fiDbBackup AT ROW 5.29 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     fiDlcDir AT ROW 6 COL 25 COLON-ALIGNED WIDGET-ID 44
     fiDBDrive AT ROW 6 COL 89 COLON-ALIGNED WIDGET-ID 478
     fiPgmBackup AT ROW 6 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     fiResBackup AT ROW 6.71 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     fiDfFilename AT ROW 7.19 COL 25 COLON-ALIGNED WIDGET-ID 52
     fiDeltaFilename AT ROW 7.19 COL 69 COLON-ALIGNED WIDGET-ID 50
     fiDbDir AT ROW 7.43 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     fiDbAuditDir AT ROW 8.14 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 486
     fiLockoutTries AT ROW 8.38 COL 25 COLON-ALIGNED WIDGET-ID 34
     fiLicensedUsers AT ROW 8.38 COL 49 COLON-ALIGNED WIDGET-ID 440
     fiDbDataDir AT ROW 8.86 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 126
     fiDbProdDir AT ROW 9.57 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 124
     fiDbShipDir AT ROW 10.29 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 122
     slEnvironments AT ROW 10.76 COL 10 NO-LABEL WIDGET-ID 480
     slDatabases AT ROW 10.76 COL 58 NO-LABEL WIDGET-ID 484
     fiDbStructDir AT ROW 11 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 120
     fiDbTestDir AT ROW 11.71 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 118
     fiDeskDir AT ROW 12.43 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     fiDocDir AT ROW 13.14 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     fiEnvDir AT ROW 13.86 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     fiEnvProdDir AT ROW 14.57 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     fiEnvAddonDir AT ROW 15.29 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 116
     fiEnvCustFiles AT ROW 16 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 114
     fiEnvCustomerDir AT ROW 16.71 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 112
     fiEnvOverrideDir AT ROW 17.43 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 110
     tbComp-10 AT ROW 18.14 COL 15 WIDGET-ID 428
     tbBackupDBs AT ROW 18.14 COL 21 WIDGET-ID 384
     tbComp-7 AT ROW 18.14 COL 61 WIDGET-ID 414
     tbRunDataFix AT ROW 18.14 COL 67 WIDGET-ID 400
     fiEnvPoDir AT ROW 18.14 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     fiEnvProgramsDir AT ROW 18.86 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 136
     tbComp-1 AT ROW 19.1 COL 15 WIDGET-ID 406
     tbUserControl AT ROW 19.1 COL 21 WIDGET-ID 370
     tbComp-13 AT ROW 19.1 COL 61 WIDGET-ID 434
     tbDelDupeNotes AT ROW 19.1 COL 67 WIDGET-ID 390
     fiEnvResourceDir AT ROW 19.57 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 134
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 166.2 BY 32.57 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     tbComp-2 AT ROW 20.05 COL 15 WIDGET-ID 408
     tbUserCleanup AT ROW 20.05 COL 21 WIDGET-ID 368
     tbComp-14 AT ROW 20.05 COL 61 WIDGET-ID 452
     tbUpdateNK1s AT ROW 20.05 COL 67 WIDGET-ID 396
     fiEnvScheduleDir AT ROW 20.29 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 132
     tbComp-4 AT ROW 21 COL 15 WIDGET-ID 412
     tbDelBadData AT ROW 21 COL 21 WIDGET-ID 374
     tbComp-15 AT ROW 21 COL 61 WIDGET-ID 430
     tbUpdateFileLocs AT ROW 21 COL 67 WIDGET-ID 398
     fiEnvTemplateDir AT ROW 21 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 286
     fiEnvUserMenuDir AT ROW 21.71 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 130
     tbComp-5 AT ROW 21.95 COL 15 WIDGET-ID 418
     tbUpdateMaster AT ROW 21.95 COL 21 WIDGET-ID 376
     tbComp-17 AT ROW 21.95 COL 61 WIDGET-ID 502
     tbReftableConv AT ROW 21.95 COL 67 WIDGET-ID 504
     fiEnvUsersDir AT ROW 22.43 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 128
     fiInstallDir AT ROW 23.14 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 142
     fiUpdatesDir AT ROW 23.86 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 144
     tbComp-6 AT ROW 24.57 COL 15 WIDGET-ID 420
     tbLoadMenus AT ROW 24.57 COL 21 WIDGET-ID 378
     fiPatchDir AT ROW 24.57 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 290
     fiUpdAdminDir AT ROW 25.29 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 152
     tbComp-3 AT ROW 25.52 COL 15 WIDGET-ID 410
     tbBuildDirs AT ROW 25.52 COL 21 WIDGET-ID 372
     fiUpdCompressDir AT ROW 26 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 150
     tbComp-8 AT ROW 26.48 COL 15 WIDGET-ID 416
     tbUpdateStartup AT ROW 26.48 COL 21 WIDGET-ID 380
     fiUpdDataDir AT ROW 26.71 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 148
     tbComp-9 AT ROW 27.43 COL 15 WIDGET-ID 426
     tbRelNotes AT ROW 27.43 COL 21 WIDGET-ID 382
     fiUpdDataUpdateDir AT ROW 27.43 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 154
     fiUpdDeskDir AT ROW 28.14 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 146
     tbComp-11 AT ROW 28.38 COL 15 WIDGET-ID 422
     tbBackupFiles AT ROW 28.38 COL 21 WIDGET-ID 386
     bProcess AT ROW 28.62 COL 73 WIDGET-ID 404
     fiUpdMenuDir AT ROW 28.86 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 298
     tbComp-12 AT ROW 29.33 COL 15 WIDGET-ID 424
     tbInstallFiles AT ROW 29.33 COL 21 WIDGET-ID 388
     fiUpdProgramDir AT ROW 29.57 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 162
     tbComp-16 AT ROW 30.29 COL 15 WIDGET-ID 432
     tbUpdateIni AT ROW 30.29 COL 21 WIDGET-ID 450
     fiUpdRelNotesDir AT ROW 30.29 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 160
     fiUpdSqlDir AT ROW 31 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 158
     fiUpdStructureDir AT ROW 31.71 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 164
     "<EnvName>" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 14.57 COL 145 WIDGET-ID 242
     "Programs" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 6 COL 145 WIDGET-ID 280
     "PO" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 18.14 COL 147 WIDGET-ID 202
     "Select one or more to upgrade." VIEW-AS TEXT
          SIZE 32 BY .62 AT ROW 14.57 COL 10 WIDGET-ID 490
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 166.2 BY 32.57 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     " General Variables" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 1.48 COL 8 WIDGET-ID 356
          FONT 6
     "StructureUpdate" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 31.71 COL 148 WIDGET-ID 296
     " (Defaults)" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.48 COL 144 WIDGET-ID 300
          FONT 6
     "Database tasks - will be performed once for each DATABASE selected above" VIEW-AS TEXT
          SIZE 86 BY .62 AT ROW 16.95 COL 11 WIDGET-ID 498
     "Environments" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 13.86 COL 141 WIDGET-ID 240
     "ReleaseNotes" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 30.29 COL 148 WIDGET-ID 182
     "Test" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 11.71 COL 145 WIDGET-ID 244
     "Updates" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 23.86 COL 141 WIDGET-ID 194
     "Desktop" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 12.43 COL 141 WIDGET-ID 246
     "Schedule" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 20.29 COL 147 WIDGET-ID 208
     " Environments" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 10.05 COL 8 WIDGET-ID 360
          FONT 6
     "Structure" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 11 COL 145 WIDGET-ID 254
     "Backups" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 4.57 COL 141 WIDGET-ID 278
     "EnvAdmin" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 3.86 COL 145 WIDGET-ID 270
     "Resources" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 6.71 COL 145 WIDGET-ID 266
     "MenuFiles" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 28.86 COL 148 WIDGET-ID 184
     "CustFiles" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 16 COL 147 WIDGET-ID 210
     "Customer" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 16.71 COL 147 WIDGET-ID 212
     "Select ONE and ONLY ONE to upgrade~\back up." VIEW-AS TEXT
          SIZE 49 BY .62 AT ROW 14.57 COL 55 WIDGET-ID 492
     "Resources" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 19.57 COL 147 WIDGET-ID 206
     "Database" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 5.29 COL 145 WIDGET-ID 264
     " Your Directory Structure" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 1.48 COL 111 WIDGET-ID 140
          FONT 6
     "Install" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 23.14 COL 141 WIDGET-ID 178
     "Compress" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 26 COL 148 WIDGET-ID 186
     "Admin" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 2.19 COL 141 WIDGET-ID 172
     "Admin" VIEW-AS TEXT
          SIZE 7 BY .76 AT ROW 2.19 COL 141 WIDGET-ID 170
     "Template" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 21 COL 147 WIDGET-ID 288
     "DbAdmin" VIEW-AS TEXT
          SIZE 10 BY .76 AT ROW 2.91 COL 145 WIDGET-ID 284
     "DataFiles" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 26.71 COL 148 WIDGET-ID 188
     "ProgramFiles" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 29.57 COL 148 WIDGET-ID 180
     "DataUpdate" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 27.43 COL 148 WIDGET-ID 190
     "Desktop" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 28.14 COL 148 WIDGET-ID 192
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 166.2 BY 32.57 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     "Prod" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 9.57 COL 145 WIDGET-ID 250
     "Override" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 17.43 COL 147 WIDGET-ID 214
     "Data" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 8.86 COL 145 WIDGET-ID 256
     "Documentation" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 13.14 COL 141 WIDGET-ID 248
     "Audit" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 8.14 COL 145 WIDGET-ID 488
     " Databases" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 10.05 COL 56 WIDGET-ID 482
          FONT 6
     "Ship" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 10.29 COL 145 WIDGET-ID 252
     "Admin" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 25.29 COL 148 WIDGET-ID 198
     "Users" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 22.43 COL 147 WIDGET-ID 218
     "SQLAccess" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 31 COL 148 WIDGET-ID 292
     "UserMenu" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 21.71 COL 147 WIDGET-ID 200
     "Databases" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 7.43 COL 141 WIDGET-ID 282
     "Addon" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 15.29 COL 147 WIDGET-ID 216
     " Patch Processing" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 15.76 COL 8 WIDGET-ID 456
          FONT 6
     "Admin" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 2.19 COL 141 WIDGET-ID 174
     "Programs" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 18.86 COL 147 WIDGET-ID 204
     "Patch<n>" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 24.57 COL 145 WIDGET-ID 196
     "Environment tasks - will be performed once for each ENVIRONMENT selected above" VIEW-AS TEXT
          SIZE 86 BY .62 AT ROW 23.38 COL 11 WIDGET-ID 500
     RECT-1 AT ROW 1.71 COL 109 WIDGET-ID 354
     RECT-2 AT ROW 1.71 COL 5 WIDGET-ID 358
     RECT-3 AT ROW 10.29 COL 5 WIDGET-ID 362
     RECT-4 AT ROW 16 COL 5 WIDGET-ID 454
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 166.2 BY 32.57 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "ASI Install/Update Processor"
         HEIGHT             = 32.38
         WIDTH              = 166.2
         MAX-HEIGHT         = 34.29
         MAX-WIDTH          = 166.2
         VIRTUAL-HEIGHT     = 34.29
         VIRTUAL-WIDTH      = 166.2
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
/* SETTINGS FOR TOGGLE-BOX tbBackupFiles IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbBuildDirs IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-10 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-11 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-12 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-13 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-14 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-15 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-16 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-17 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-6 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-7 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-8 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbComp-9 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbUpdateFileLocs IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbUpdateStartup IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* ASI Install/Update Processor */
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
ON WINDOW-CLOSE OF C-Win /* ASI Install/Update Processor */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
    QUIT.
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
    RUN ipProcessAll.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiNewVer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiNewVer C-Win
ON LEAVE OF fiNewVer IN FRAME DEFAULT-FRAME /* New Version */
DO:
    ASSIGN
        fiPatchDir:{&SV} = "PATCH" + fiNewVer:{&SV}
        cUpdProgramDir = fiPatchDir:{&SV}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slEnvironments
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slEnvironments C-Win
ON VALUE-CHANGED OF slEnvironments IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
        fiCurrVer:{&SV} = ENTRY(2,SELF:{&SV},"-").
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

    RUN ipCreateTTiniFile.
    RUN ipFindIniFile.
    IF cIniLoc NE "" THEN 
        RUN ipReadIniFile.
    RUN ipExpandVarNames.
    RUN ipSetDispVars.

    RUN ipValidateDB (OUTPUT lValidDB).
    IF NOT lValidDB THEN QUIT.

    IF ipiLevel GT 10 THEN ASSIGN
        fiLicensedUsers:SENSITIVE = TRUE
        tbUserControl:SENSITIVE = TRUE.

    FIND FIRST usercontrol NO-LOCK NO-ERROR.
    IF AVAIL usercontrol THEN ASSIGN
        fiLicensedUsers:{&SV} = STRING(usercontrol.numLicensedUsers)
        lNeedUsercontrol = FALSE.
    ELSE ASSIGN
        lNeedUsercontrol = TRUE.
        
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
    
    APPLY 'value-changed' TO slEnvironments.
    
    ASSIGN
        tbBackupDBs:CHECKED = TRUE
        tbUserControl:CHECKED = TRUE
        tbUserCleanup:CHECKED = TRUE
        tbDelBadData:CHECKED = TRUE
        tbUpdateMaster:CHECKED = TRUE
        tbLoadMenus:CHECKED = TRUE
        tbRunDataFix:CHECKED = TRUE
        tbDelDupeNotes:CHECKED = TRUE
        tbUpdateNK1s:CHECKED = TRUE
        tbUpdateFileLocs:CHECKED = TRUE
        tbRefTableConv:CHECKED = TRUE
        tbBuildDirs:CHECKED = FALSE
        tbUpdateStartup:CHECKED = FALSE
        tbRelNotes:CHECKED = TRUE
        tbBackupFiles:CHECKED = FALSE
        tbInstallFiles:CHECKED = TRUE
        tbUpdateINI:CHECKED = TRUE
        .
        
    IF NUM-ENTRIES(slEnvironments:LIST-ITEMS) EQ 1 
    AND NUM-ENTRIES(slDatabases:LIST-ITEMS) = 1
    AND ipiLevel LT 10 THEN DO:
        ASSIGN
            lAutorun = TRUE.
        DISABLE ALL EXCEPT bProcess WITH FRAME {&FRAME-NAME}.
        APPLY 'choose' to bProcess.
    END.
    
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
  DISPLAY fiCurrVer fiVerDate fiNewVer fiAdminDir fiDbAdmin fiSiteName 
          fiHostname fiEnvAdmin fiBackupDir fiDrive fiTopDir fiMapDir fiDbBackup 
          fiDlcDir fiDBDrive fiPgmBackup fiResBackup fiDfFilename 
          fiDeltaFilename fiDbDir fiDbAuditDir fiLockoutTries fiLicensedUsers 
          fiDbDataDir fiDbProdDir fiDbShipDir slEnvironments slDatabases 
          fiDbStructDir fiDbTestDir fiDeskDir fiDocDir fiEnvDir fiEnvProdDir 
          fiEnvAddonDir fiEnvCustFiles fiEnvCustomerDir fiEnvOverrideDir 
          tbComp-10 tbBackupDBs tbComp-7 tbRunDataFix fiEnvPoDir 
          fiEnvProgramsDir tbComp-1 tbUserControl tbComp-13 tbDelDupeNotes 
          fiEnvResourceDir tbComp-2 tbUserCleanup tbComp-14 tbUpdateNK1s 
          fiEnvScheduleDir tbComp-4 tbDelBadData tbComp-15 tbUpdateFileLocs 
          fiEnvTemplateDir fiEnvUserMenuDir tbComp-5 tbUpdateMaster tbComp-17 
          tbReftableConv fiEnvUsersDir fiInstallDir fiUpdatesDir tbComp-6 
          tbLoadMenus fiPatchDir fiUpdAdminDir tbComp-3 tbBuildDirs 
          fiUpdCompressDir tbComp-8 tbUpdateStartup fiUpdDataDir tbComp-9 
          tbRelNotes fiUpdDataUpdateDir fiUpdDeskDir tbComp-11 tbBackupFiles 
          fiUpdMenuDir tbComp-12 tbInstallFiles fiUpdProgramDir tbComp-16 
          tbUpdateIni fiUpdRelNotesDir fiUpdSqlDir fiUpdStructureDir 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 RECT-3 RECT-4 fiCurrVer fiVerDate fiNewVer fiAdminDir 
         fiDbAdmin fiSiteName fiHostname fiEnvAdmin fiBackupDir fiDrive 
         fiTopDir fiMapDir fiDbBackup fiDlcDir fiDBDrive fiPgmBackup 
         fiResBackup fiDfFilename fiDeltaFilename fiDbDir fiDbAuditDir 
         fiLockoutTries fiLicensedUsers fiDbDataDir fiDbProdDir fiDbShipDir 
         slEnvironments slDatabases fiDbStructDir fiDbTestDir fiDeskDir 
         fiDocDir fiEnvDir fiEnvProdDir fiEnvAddonDir fiEnvCustFiles 
         fiEnvCustomerDir fiEnvOverrideDir tbBackupDBs tbRunDataFix fiEnvPoDir 
         fiEnvProgramsDir tbUserControl tbDelDupeNotes fiEnvResourceDir 
         tbUserCleanup tbUpdateNK1s fiEnvScheduleDir tbDelBadData 
         fiEnvTemplateDir fiEnvUserMenuDir tbUpdateMaster tbReftableConv 
         fiEnvUsersDir fiInstallDir fiUpdatesDir tbLoadMenus fiPatchDir 
         fiUpdAdminDir fiUpdCompressDir fiUpdDataDir tbRelNotes 
         fiUpdDataUpdateDir fiUpdDeskDir bProcess fiUpdMenuDir tbInstallFiles 
         fiUpdProgramDir tbUpdateIni fiUpdRelNotesDir fiUpdSqlDir 
         fiUpdStructureDir 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipAddSuppUserRecords C-Win 
PROCEDURE ipAddSuppUserRecords :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Adding Supplemental User Records").

    DEF INPUT PARAMETER ipcUserID AS CHAR NO-UNDO.
    DEF VAR lv-default-comp AS CHAR NO-UNDO.
    DEF VAR lv-default-loc AS CHAR NO-UNDO.
    DEF VAR cCurrentDir AS CHAR NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF usercomp.
    DISABLE TRIGGERS FOR LOAD OF usr.

    /* Add default usercomp records for new user */
    FIND FIRST bf-usercomp NO-LOCK WHERE 
        bf-usercomp.USER_id = "ASI" AND
        bf-usercomp.company_default EQ TRUE
        NO-ERROR.
    IF NOT AVAIL bf-usercomp THEN FIND FIRST bf-usercomp NO-LOCK WHERE 
        bf-usercomp.company_default EQ TRUE
        NO-ERROR.
    
    ASSIGN 
        lv-default-comp = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001".
    FIND FIRST usercomp NO-LOCK WHERE 
        usercomp.USER_id = ipcUserID AND 
        usercomp.company = lv-default-comp AND
        usercomp.loc = ""
        NO-ERROR.
    IF NOT AVAIL usercomp THEN DO:
        CREATE usercomp.
        ASSIGN 
            usercomp.user_id = ipcUserID
            usercomp.company = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001"
            usercomp.loc = ""
            usercomp.company_default = YES.
    END.

    FIND FIRST bf-usercomp NO-LOCK WHERE
        bf-usercomp.user_id = "ASI" AND
        bf-usercomp.loc_default EQ TRUE
        NO-ERROR.
    IF NOT AVAIL bf-usercomp THEN FIND FIRST bf-usercomp NO-LOCK WHERE
        bf-usercomp.loc_default 
        NO-ERROR.
    ASSIGN 
        lv-default-loc = IF AVAIL bf-usercomp THEN bf-usercomp.loc ELSE "MAIN".
    FIND FIRST usercomp NO-LOCK WHERE 
        usercomp.user_id = ipcUserID AND 
        usercomp.company = lv-default-comp AND
        usercomp.loc = lv-default-loc 
        NO-ERROR.
    IF NOT AVAIL usercomp THEN DO:
        CREATE usercomp.
        ASSIGN 
            usercomp.user_id = ipcUserID
            usercomp.company = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001"
            usercomp.loc = IF AVAIL bf-usercomp THEN bf-usercomp.loc ELSE "MAIN"
            usercomp.loc_DEFAULT = YES.
    END.
    
    FIND FIRST usr WHERE 
        usr.uid EQ ipcUserID 
        NO-ERROR.
    IF NOT AVAIL usr THEN DO:
        CREATE usr.
        ASSIGN
            usr.uid = ipcUserID
            usr.usr-lang = "English"
            usr.last-chg = today.
    END.
    ELSE DO:
        IF usr.usr-lang = "EN" THEN ASSIGN
            usr.usr-lang = "English".
    END.

    /* Ensure folder available for custom menus */
    ASSIGN
        cCurrentDir = fiDrive:{&SV} + "\" + 
                      fiTopDir:{&SV} + "\" +
                      fiEnvDir:{&SV} + "\" +
                      ENTRY(1,slEnvironments:{&SV},"-") + "\" +
                      "UserMenu\" + ipcUserID.
    OS-CREATE-DIR VALUE(cCurrentDir).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipArchiveFiles C-Win 
PROCEDURE ipArchiveFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Archiving ASI System Files").

    DEF VAR cCmdZip AS CHAR NO-UNDO.
    DEF VAR lProdExists AS LOG NO-UNDO.
    DEF VAR lProdOverExists AS LOG NO-UNDO.
    
    FILE-INFO:FILE-NAME = cEnvProdDir.
    ASSIGN
        lProdExists = FILE-INFO:FULL-PATHNAME NE ?.
        
    IF SEARCH(cUpdProgramDir + "\programs.7z") NE ? THEN DO:
        RUN ipStatus ("Archiving old program files").

        /* Default is to pull from Env\Prod, but if not found, will also search root drive and Rcode */
        IF SEARCH(cEnvProdDir + "\Programs\nosweat.r") NE ? THEN DO:
            ASSIGN
                cCmdZip = cUpdCompressDir + "\7z.exe a " + 
                          cPgmBackup + "\Programs" + 
                          STRING(YEAR(TODAY)) +
                          STRING(MONTH(TODAY),"99") +
                          STRING(DAY(TODAY),"99") + ".7z " +
                          cEnvProdDir + "\Programs\*".
            OS-COMMAND SILENT VALUE(cCmdZip).
        END.    
        ELSE IF SEARCH(cMapDir + "\Programs\nosweat.r") NE ? THEN DO:
            ASSIGN
                cCmdZip = cUpdCompressDir + "\7z.exe a " + 
                          cPgmBackup + "\Programs" + 
                          STRING(YEAR(TODAY)) +
                          STRING(MONTH(TODAY),"99") +
                          STRING(DAY(TODAY),"99") + ".7z " +
                          cMapDir + "\Programs\*".
            OS-COMMAND SILENT VALUE(cCmdZip).
        END.
        ELSE IF SEARCH(cMapDir + "\Rcode\Programs\nosweat.r") NE ? THEN DO:
            ASSIGN
                cCmdZip = cUpdCompressDir + "\7z.exe a " + 
                          cPgmBackup + "\Programs" + 
                          STRING(YEAR(TODAY),"99") +
                          STRING(MONTH(TODAY),"99") +
                          STRING(DAY(TODAY)) + ".7z " +
                          cMapDir + "\Rcode\Programs\*".
            OS-COMMAND SILENT VALUE(cCmdZip).
        END.
    END.

    IF SEARCH(cUpdProgramDir + "\resources.7z") NE ? THEN DO:
        RUN ipStatus ("Archiving old resource files").

        IF SEARCH(cEnvProdDir + "\Resources\quoter.exe") NE ? THEN DO:
            ASSIGN
                cCmdZip = cUpdCompressDir + "\7z.exe a " + 
                          cResBackup + "\Resources" + 
                          STRING(YEAR(TODAY)) +
                          STRING(MONTH(TODAY),"99") +
                          STRING(DAY(TODAY),"99") + ".7z " +
                          cEnvProdDir + "\Resources\*".
            OS-COMMAND SILENT VALUE(cCmdZip).
        END.
        ELSE IF SEARCH(cMapDir + "\Resources\quoter.exe") NE ? THEN DO:
            ASSIGN
                cCmdZip = cUpdCompressDir + "\7z.exe a " + 
                          cResBackup + "\Resources" + 
                          STRING(YEAR(TODAY)) +
                          STRING(MONTH(TODAY),"99") +
                          STRING(DAY(TODAY),"99") + ".7z " +
                          cMapDir + "\Resources\*".
            OS-COMMAND SILENT VALUE(cCmdZip).
        END.
        ELSE IF SEARCH(cMapDir + "\Rcode\Resources\quoter.exe") NE ? THEN DO:
            ASSIGN
                cCmdZip = cUpdCompressDir + "\7z.exe a " + 
                          cResBackup + "\Resources" + 
                          STRING(YEAR(TODAY)) +
                          STRING(MONTH(TODAY),"99") +
                          STRING(DAY(TODAY),"99") + ".7z " +
                          cMapDir + "\Rcode\Resources\*".
            OS-COMMAND SILENT VALUE(cCmdZip).
        END.
    END.

    FILE-INFO:FILE-NAME = cEnvProdDir + "\Override".
    ASSIGN
        lProdOverExists = FILE-INFO:FULL-PATHNAME NE ?.
    IF lProdOverExists THEN DO:
        RUN ipStatus ("Archiving old hotfix files").
        ASSIGN
            cCmdZip = cUpdCompressDir + "\7z.exe a " + 
                      cPgmBackup + "\Override" + 
                      STRING(YEAR(TODAY)) +
                      STRING(MONTH(TODAY),"99") +
                      STRING(DAY(TODAY),"99") + ".7z " +
                      cEnvProdDir + "\Override\*".
        OS-COMMAND SILENT VALUE(cCmdZip).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipBackupDataFiles C-Win 
PROCEDURE ipBackupDataFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Backing up data files").
    
&SCOPED-DEFINE cFile AuditTbl
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}" + ".bak") NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
        CREATE ttAuditTbl.
        BUFFER-COPY {&cFile} TO ttAuditTbl.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile sys-ctrl
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}" + ".bak") NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
        CREATE ttSysCtrl.
        BUFFER-COPY {&cFile} TO ttSysCtrl.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile sys-ctrl-shipto
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}" + ".bak") NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
        CREATE ttSysCtrlShipto.
        BUFFER-COPY {&cFile} TO ttSysCtrlShipto.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile emailcod
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}" + ".bak") NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile lookups
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}" + ".bak") NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile module
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}" + ".bak") NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile prgmxref
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}" + ".bak") NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile prgrms
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}" + ".bak") NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

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
    RUN ipStatus ("Backing Up Selected Databases").

    DEF VAR cCmdLine AS CHAR NO-UNDO.
    DEF VAR cLocItem AS CHAR NO-UNDO.
    DEF VAR cLocDir AS CHAR NO-UNDO.
    DEF VAR cLocName AS CHAR NO-UNDO.
    DEF VAR cLocPort AS CHAR NO-UNDO.
    
    DO iCtr = 1 TO NUM-ENTRIES(slDatabases:{&SV}):
        ASSIGN
            cLocItem = ENTRY(iCtr,slDatabases:{&SV})
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
    
        RUN ipStatus ("  Backing Up " + ENTRY(iCtr,slDatabases:{&SV})).
        OS-COMMAND SILENT VALUE(cCmdLine).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipBuildCustFilesTree C-Win 
PROCEDURE ipBuildCustFilesTree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Creating CustFiles Structure").

    DEF VAR cCmdLine1 AS CHAR NO-UNDO.
    DEF VAR cCmdLine2 AS CHAR NO-UNDO.
    DEF VAR cCmdLine3 AS CHAR NO-UNDO.
    DEF VAR cFileType AS CHAR NO-UNDO.
    DEF VAR lProdExists AS LOG NO-UNDO.
    DEF VAR lTestExists AS LOG NO-UNDO.
    DEF VAR lProdFilesExist AS LOG NO-UNDO.
    DEF VAR lTestFilesExist AS LOG NO-UNDO.
    DEF VAR cTestDir AS CHAR NO-UNDO.
    DEF VAR cBaseDir AS CHAR NO-UNDO.
    DEF VAR iNumPasses AS INT NO-UNDO.
    DEF VAR cCmdLine AS CHAR NO-UNDO.
    
    FILE-INFO:FILE-NAME = cEnvProdDir.
    ASSIGN
        lProdExists = FILE-INFO:FULL-PATHNAME NE ?.
    FILE-INFO:FILE-NAME = cEnvTestDir.
    ASSIGN
        lTestExists = FILE-INFO:FULL-PATHNAME NE ?
        cCmdLine1 = cUpdCompressDir + "\7z.exe x ".
    FILE-INFO:FILE-NAME = cEnvProdDir + "\CustFiles\Logs".
    ASSIGN
        lProdFilesExist = FILE-INFO:FULL-PATHNAME NE ?.
    FILE-INFO:FILE-NAME = cEnvTestDir + "\CustFiles\Logs".
    ASSIGN
        lTestFilesExist = FILE-INFO:FULL-PATHNAME NE ?.
    
    IF lProdFilesExist
    AND lTestFilesExist THEN RETURN.
    
    IF NOT lProdFilesExist
    AND SEARCH(cUpdatesDir + "\" + "Patch" + cPatchNo + "\Deployment\CustFiles.7z") NE ? THEN DO:
        ASSIGN
            cCmdLine2 = cCmdLine1 + cUpdatesDir + "\" + "Patch" + cPatchNo + "\Deployment\CustFiles..7z -y -o".
        IF lProdExists THEN DO:
            OS-CREATE-DIR VALUE(cEnvProdDir + "\CustFiles").
            ASSIGN
                cCmdLine3 = cCmdLine2 + cEnvProdDir + "\CustFiles".
            OS-COMMAND SILENT VALUE(cCmdLine3).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipBuildDirs C-Win 
PROCEDURE ipBuildDirs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Building Required Directories").

    DEF VAR cBadDir AS CHAR NO-UNDO.
    DEF VAR lStructOK AS LOG NO-UNDO.
    
    RUN ipTestStructure (OUTPUT lStructOK).
    
    IF cBadDirList NE "" THEN DO i = 1 TO NUM-ENTRIES(cBadDirList):
        ASSIGN
            cBadDir = ENTRY(i,cBadDirList).
        OS-CREATE-DIR VALUE(cBadDir).
    END.
    
    RUN ipBuildCustFilesTree.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCheckPayMaster C-Win 
PROCEDURE ipCheckPayMaster :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Verifying payment-type records").

    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPaper AS LOGICAL NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF payment-type.

    IF NOT CAN-FIND(FIRST payment-type WHERE 
                    payment-type.company EQ ipcCompany AND 
                    payment-type.type EQ ipcType) THEN DO:
        CREATE payment-type.
        ASSIGN
            payment-type.company    = company.company
            payment-type.type       = ipcType
            payment-type.dscr       = ipcDescription
            payment-type.paperCheck = iplPaper
            .
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCleanBadUserData C-Win 
PROCEDURE ipCleanBadUserData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Clean Bad User Data").

DISABLE TRIGGERS FOR LOAD OF usr.
DISABLE TRIGGERS FOR LOAD OF user-print.
DISABLE TRIGGERS FOR LOAD OF usercomp.
DISABLE TRIGGERS FOR LOAD OF usercust.
DISABLE TRIGGERS FOR LOAD OF userEula.
DISABLE TRIGGERS FOR LOAD OF userlog.
DISABLE TRIGGERS FOR LOAD OF usersman.
DISABLE TRIGGERS FOR LOAD OF uservend.
DISABLE TRIGGERS FOR LOAD OF usr-grp.
DISABLE TRIGGERS FOR LOAD OF usr-menu.
DISABLE TRIGGERS FOR LOAD OF usrx.
DISABLE TRIGGERS FOR LOAD OF reftable.

    RUN ipStatus ("  Removing Old User Records").

/* Clean up remnant records for any deleted users */
&SCOPED-DEF cFileName usr
&SCOPED-DEF cFieldName uid
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName user-print
&SCOPED-DEF cFieldName user-id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        IF {&cFileName}.{&cFieldName} = "" THEN NEXT.
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName usercomp
&SCOPED-DEF cFieldName user_id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        IF {&cFileName}.{&cFieldName} = "" THEN NEXT.
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName usercust
&SCOPED-DEF cFieldName user_id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName userEula
&SCOPED-DEF cFieldName user_id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        IF {&cFileName}.{&cFieldName} = "" THEN NEXT.
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName userLog
&SCOPED-DEF cFieldName user_id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName usersman
&SCOPED-DEF cFieldName user_id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName userVend
&SCOPED-DEF cFieldName user_id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName usr-grp
&SCOPED-DEF cFieldName uid
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        IF {&cFileName}.{&cFieldName} = "" THEN NEXT.
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName usr-menu
&SCOPED-DEF cFieldName user_id
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        IF {&cFileName}.{&cFieldName} = "" THEN NEXT.
        DELETE {&cFileName}.
    END. 

&SCOPED-DEF cFileName usrx
&SCOPED-DEF cFieldName uid
    FOR EACH {&cFileName} EXCLUSIVE WHERE NOT CAN-FIND (users WHERE users.user_id EQ {&cFileName}.{&cFieldName}):
        DELETE {&cFileName}.
    END. 

DISABLE TRIGGERS FOR LOAD OF reftable.

    FOR EACH reftable EXCLUSIVE WHERE reftable.reftable EQ "users.user-docs":
        IF NOT CAN-FIND(users WHERE 
                        users.user_id EQ reftable.company) THEN
            DELETE reftable.
    END.

    FOR EACH reftable EXCLUSIVE WHERE reftable.reftable EQ "users.phone-no":
        IF NOT CAN-FIND(users WHERE 
                        users.user_id EQ reftable.company) THEN
            DELETE reftable.
    END.

    FOR EACH reftable EXCLUSIVE WHERE reftable.reftable EQ "users.fax-no":
        IF NOT CAN-FIND(users WHERE 
                        users.user_id EQ reftable.company) THEN
            DELETE reftable.
    END.

    FOR EACH reftable EXCLUSIVE WHERE reftable.reftable EQ "users.phone-cnty":
        IF NOT CAN-FIND(users WHERE 
                        users.user_id EQ reftable.company) THEN
            DELETE reftable.
    END.

    FOR EACH reftable EXCLUSIVE WHERE reftable.reftable EQ "users.fax-cnty":
        IF NOT CAN-FIND(users WHERE 
                        users.user_id EQ reftable.company) THEN
            DELETE reftable.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConfirmAdminUser C-Win 
PROCEDURE ipConfirmAdminUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Validating ADMIN user entries").

    DISABLE TRIGGERS FOR LOAD OF users.

    FIND users EXCLUSIVE WHERE 
        users.user_id = "admin"
        NO-ERROR.
    ASSIGN
        users.developer = false
        users.isActive = true
        users.isLocked = false
        users.securityLevel = IF users.securityLevel LT 900 THEN 900 ELSE users.securityLevel
        users.updateDate = today
        users.updateTime = time
        users.updateUser = USERID(LDBNAME(1))
        users.userType = "Administrator"
        users.user_language = IF users.user_language = "" THEN "English" ELSE users.user_language
        users.user_name = IF users.user_name = "" THEN "Local System Admin" ELSE users.user_name
        users.use_colors = false
        users.use_ctrl_keys = false
        users.use_fonts = false
        users.widget_bgc = 0
        users.widget_fgc = 0
        users.widget_font = 0
        . 
    
    RUN ipSetAdminPwd IN THIS-PROCEDURE.
    RUN ipAddSuppUserRecords IN THIS-PROCEDURE (INPUT users.user_id).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConfirmASIUser C-Win 
PROCEDURE ipConfirmASIUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Validating ASI User Entries").

    DISABLE TRIGGERS FOR LOAD OF users.

    FIND users EXCLUSIVE WHERE 
        users.user_id = "asi"
        NO-ERROR.
    ASSIGN
        users.developer = false
        users.fax = ""
        users.fax-cnty = ""
        users.image_filename = "asiHelp@advantzware.com"
        users.isActive = true
        users.isLocked = false
        users.phone = "2153697800"
        users.phone-cnty = "1"
        users.securityLevel = 1000
        users.showOnAck = false
        users.showOnBol = false
        users.showOnInv = false
        users.showOnPO = false
        users.showOnQuote = false
        users.track_usage = true
        users.updateDate = today
        users.updateTime = time
        users.updateUser = "asi"
        users.userType = "Administrator"
        users.user_language = "English"
        users.user_name = "Advantzware System Admin"
        users.user_program[1] = ""
        users.user_program[2] = ""
        users.user_program[3] = ""
        users.use_colors = false
        users.use_ctrl_keys = false
        users.use_fonts = false
        users.widget_bgc = 0
        users.widget_fgc = 0
        users.widget_font = 0
        . 
    
    RUN ipSetAsiPwd IN THIS-PROCEDURE.
    RUN ipAddSuppUserRecords IN THIS-PROCEDURE (INPUT users.user_id).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConvertModule C-Win 
PROCEDURE ipConvertModule :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Converting module " + cFrom + " to " + cTo).

    FIND FIRST module NO-LOCK WHERE 
        module.module = cfrom 
        NO-ERROR.
    IF NOT AVAIL module THEN RETURN.
  
    FIND FIRST bf-module EXCLUSIVE WHERE 
        ROWID(bf-module) = ROWID(module) NO-ERROR.
    IF AVAIL bf-module THEN ASSIGN
        bf-module.module = cTo.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConvertUsrFile C-Win 
PROCEDURE ipConvertUsrFile :
/*---------------------------------------------------------------------------*/
/*  File:           admin\envadmin\convusr.p                                 */
/*  Copyright:      (c)2018 Advanced Software Services, Inc.All rights rsrvd */
/*  Description:    Convert old-style advantzware.usr file to new style      */
/*                                                                           */
/*  Included files:     none                                                 */
/*                                                                           */
/*  External RUN/CALL:  none                                                 */
/*                                                                           */
/*  External files:     READ admin/advantzware.usr                           */
/*                      COPY admin/advantzware.usr.old                       */
/*                      WRITE admin/advantzware.usr                          */
/*                                                                           */
/*  Revision history:   MM/DD/YY    INIT    TKT     Description              */
/*                      06/14/18    MYT     xxxxx   Original Version         */
/*                      06/15/18    MYT             Moved to upgrade process */
/*---------------------------------------------------------------------------*/

    RUN ipStatus ("Converting advantzware.usr file...").

DEF VAR cLine AS CHAR NO-UNDO.
DEF VAR cOutline AS CHAR NO-UNDO.
DEF VAR cuserlist AS CHAR NO-UNDO.
DEF VAR cenvlist AS CHAR NO-UNDO.
DEF VAR cdblist AS CHAR NO-UNDO.
DEF VAR calias AS CHAR NO-UNDO.
DEF VAR cenv AS CHAR NO-UNDO.
DEF VAR cdb AS CHAR NO-UNDO.
DEF VAR cmode AS CHAR NO-UNDO.
DEF VAR i AS INT NO-UNDO. 
DEF VAR j AS INT NO-UNDO. 
DEF VAR k AS INT NO-UNDO. 
DEF VAR ctr AS INT NO-UNDO.
DEF VAR lContinue AS LOG INITIAL TRUE NO-UNDO.

INPUT FROM VALUE(cAdminDir + "\advantzware.usr").
REPEAT:
    IMPORT UNFORMATTED cLine.
    IF ENTRY(2,cLine,"|") EQ "*" THEN DO:
        ASSIGN
            lContinue = FALSE.
        RETURN.
    END.
    
    CREATE ttUsers.
    ASSIGN
        ttfUserid = ENTRY(3,cLine,"|")
        ttfdbname = ENTRY(1,cLine,"|")
        ttfalias = ENTRY(2,cLine,"|")
        ttfenvlist = ENTRY(4,cLine,"|")
        ttfdblist = ENTRY(5,cLine,"|")
        ttfmodelist = ENTRY(6,cLine,"|").
END.
INPUT CLOSE.

IF lContinue EQ FALSE THEN RETURN.

OS-COPY VALUE(cAdminDir + "\advantzware.usr") VALUE(cAdminDir + "\advantzware.usr.old").

FOR EACH ttUsers:
    IF INDEX(cuserlist,ttfuserid) EQ 0 THEN ASSIGN
        cuserlist = cuserlist + ttfuserid + ",".
    DO i = 1 TO NUM-ENTRIES(ttfenvlist):
        IF INDEX(cenvlist,ENTRY(i,ttfenvlist)) EQ 0 THEN ASSIGN
            cenvlist = cenvlist + ENTRY(i,ttfenvlist) + ",".
    END.
    IF INDEX(cdblist,ttfDbName) EQ 0 THEN ASSIGN
        cdblist = cdblist + ttfDbName + ",".
END.

ASSIGN
    cuserlist = TRIM(cuserlist,",")
    cdblist = TRIM(cdblist,",")
    cenvlist = TRIM(cenvlist,",").

OUTPUT TO VALUE(cAdminDir + "\advantzware.usr").
DO i = 1 TO NUM-ENTRIES(cuserlist):
    FIND FIRST ttusers WHERE
        ttfuserid = ENTRY(i,cuserlist) AND
        ttfalias NE ""
        NO-ERROR.
    ASSIGN
        calias = IF AVAIL ttusers THEN ttfalias ELSE "".

    FIND FIRST ttusers WHERE
        ttfuserid = ENTRY(i,cuserlist) AND
        ttfenvlist NE ""
        NO-ERROR.
    ASSIGN
        cenv = IF AVAIL ttusers THEN ttfenvlist ELSE "".

    FIND FIRST ttusers WHERE
        ttfuserid = ENTRY(i,cuserlist) AND
        ttfdblist NE ""
        NO-ERROR.
    ASSIGN
        cdb = IF AVAIL ttusers THEN ttfdblist ELSE "".

    FIND FIRST ttusers WHERE
        ttfuserid = ENTRY(i,cuserlist) AND
        ttfmodelist NE ""
        NO-ERROR.
    ASSIGN
        cmode = IF AVAIL ttusers THEN ttfmodelist ELSE "".

    ASSIGN
        cOutline = ENTRY(i,cuserlist) + "|*|" +
                   calias + "|" +
                   cenv + "|" +
                   cdb + "|".
    PUT UNFORMATTED coutline + CHR(10).
END.
    
DO j = 1 TO NUM-ENTRIES(cdblist):
    ASSIGN
        cdb = ENTRY(j,cdblist).
    DO i = 1 TO NUM-ENTRIES(cuserlist):
        FIND FIRST ttusers WHERE
            ttfuserid = ENTRY(i,cuserlist) AND
            ttfmodelist NE ""
            NO-ERROR.
        ASSIGN
            cmode = IF AVAIL ttusers THEN ttfmodelist ELSE ""
            cOutline = ENTRY(i,cuserlist) + "|" +
                       cdb + "|" +
                       "" + "|" +
                       "" + "|" +
                       "" + "|" +
                       cmode.
        PUT unformatted coutline + CHR(10).
    END.
END.
OUTPUT CLOSE.  
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipConvQtyPerSet C-Win 
PROCEDURE ipConvQtyPerSet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Converting QtyPerSet records...").

    DEF VAR cOrigPropath AS CHAR NO-UNDO.
    DEF VAR cNewPropath AS CHAR NO-UNDO.
    DEF VAR cThisElement AS CHAR NO-UNDO.
    DEF VAR dQtyPerSet AS DECIMAL NO-UNDO.
    DEF VAR iCount AS INTEGER NO-UNDO.
    DEF VAR iCountProcessed AS INTEGER NO-UNDO.
    DEF VAR iCountInitialized AS INTEGER NO-UNDO.
    DEF VAR iCountSets AS INTEGER NO-UNDO.
    DEF VAR iCountFGSets AS INTEGER NO-UNDO.
    DEF VAR iCountFGSetsProcessed AS INTEGER NO-UNDO.
    DEF VAR iCountFGSetsInitialized AS INTEGER NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF eb.
    DISABLE TRIGGERS FOR LOAD OF fg-set.
    
    FOR EACH company NO-LOCK, 
        EACH est NO-LOCK WHERE 
        est.company EQ company.company,
        EACH eb OF est EXCLUSIVE-LOCK:
        ASSIGN
            iCount = iCount + 1.
        CASE est.est-type:
            WHEN 5 OR WHEN 6 THEN DO:
                IF eb.quantityPerSet EQ 0 THEN DO: 
                    ASSIGN 
                        iCountProcessed = iCountProcessed + 1 
                        dQtyPerSet = eb.yld-qty
                        .
                    IF dQtyPerSet LT 0 THEN dQtyPerSet = -1 / dQtyPerSet.
                    IF dQtyPerSet EQ 0 THEN dQtyPerSet = 1.
                    eb.quantityPerSet = dQtyPerSet.   
                END.
            END.
  
            /*Folding carton uses %-cust - out of scope for ticket 25146*/     
/*          WHEN 1 OR WHEN 2 THEN      */
/*                dQtyPerSet = eb.cust-%.*/
/*                                       */
        END CASE.
        IF eb.quantityPerSet EQ 0 THEN ASSIGN 
            eb.quantityPerSet = 1
            iCountInitialized = iCountInitialized + 1.
    END.

    FOR EACH company NO-LOCK, 
        EACH fg-set EXCLUSIVE-LOCK WHERE 
            fg-set.company EQ company.company:
        ASSIGN
            iCountFGSets = iCountFGSets + 1.
        IF fg-set.qtyPerSet EQ 0 AND fg-set.part-qty NE 0 THEN ASSIGN 
            iCountFGSetsProcessed = iCountFGSetsProcessed + 1 
            fg-set.qtyPerSet = fg-set.part-qty.
        IF fg-set.qtyPerSet EQ 0 THEN ASSIGN 
            iCountFGSetsInitialized = iCountFGSetsInitialized + 1 
            fg-set.qtyPerSet = 1.
    END.     

    RUN ipStatus ("   Total Estimates: "  + STRING(iCount)).
    RUN ipStatus ("   Converted from .yld-qty: " + STRING(iCountProcessed)).
    RUN ipStatus ("   Initialized to 1: " + STRING(iCountInitialized)).
    RUN ipStatus ("   Total Sets: " + STRING(iCountFGSets)).
    RUN ipStatus ("   Sets Converted from .part-qty to .qtyPerSet: " + STRING(iCountFGSetsProcessed)).
    RUN ipStatus ("   Sets Initialized to 1: " + STRING(iCountFGSetsInitialized )).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCopyDirs C-Win 
PROCEDURE ipCopyDirs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcDir AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcTgtDir AS CHAR NO-UNDO.
    DEF VAR cFileStream AS CHAR NO-UNDO.

    RUN ipStatus ("Copying " + ipcDir + " to " + ipcTgtDir).

    INPUT FROM OS-DIR (ipcDir).

    REPEAT:
        IMPORT cFileStream.
        FILE-INFO:FILE-NAME = ipcDir + "\" + cFileStream.
        IF SUBSTRING(FILE-INFO:FILE-NAME,LENGTH(FILE-INFO:FILE-NAME),1) EQ "." THEN DO:
            NEXT.
        END.
        ELSE IF FILE-INFO:FILE-TYPE BEGINS "F" THEN DO:
            OS-COPY VALUE(FILE-INFO:FILE-NAME) VALUE(ipcTgtDir).
        END.
        ELSE DO:
            OS-CREATE-DIR VALUE(ipcTgtDir + "\" + cFileStream).
            RUN ipCopyDirs IN THIS-PROCEDURE (FILE-INFO:FILE-NAME,ipcTgtDir + "\" + cFileStream).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCopyRelNotes C-Win 
PROCEDURE ipCopyRelNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Copying Release Notes").

    DEF VAR cCommandLine AS CHAR NO-UNDO.
    
    FILE-INFO:FILE-NAME = cDocDir.
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN
        OS-CREATE-DIR VALUE(cDocDir).
    OS-CREATE-DIR VALUE(cDocDir + "\ReleaseNotes").

    ASSIGN
        cCommandLine = "COPY " + cUpdRelNotesDir + "\*.* " + cDocDir + "\ReleaseNotes".
    OS-COMMAND SILENT VALUE(cCommandLine).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCopyStartup C-Win 
PROCEDURE ipCopyStartup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR lAdminExists AS LOG NO-UNDO.
    DEF VAR lDesktopExists AS LOG NO-UNDO.
    
    RUN ipStatus ("Installing Startup Files").

    ASSIGN
        FILE-INFO:FILE-NAME = cDeskDir
        lDesktopExists = FILE-INFO:FULL-PATHNAME NE ?
        FILE-INFO:FILE-NAME = cAdminDir
        lAdminExists = FILE-INFO:FULL-PATHNAME NE ?.
    
    IF NOT lDesktopExists 
    OR SEARCH(cDeskDir + "\advantzware.ico") = ? THEN
        OS-COPY VALUE(cUpdDeskDir) VALUE(cDeskDir).

    OS-COMMAND SILENT VALUE("COPY " + cUpdAdminDir + " " + cAdminDir).
    IF SEARCH(cAdminDir + "\advantzware.usr") = ? THEN
        OS-COMMAND SILENT VALUE("COPY " + cUpdDataDir + "\advantzware.usr " + cAdminDir). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateAdminUser C-Win 
PROCEDURE ipCreateAdminUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Creating ADMIN User").

    DISABLE TRIGGERS FOR LOAD OF users.
    
    CREATE users.
    ASSIGN
        users.user_id = "ADMIN"
        users.createDate = today
        users.createTime = time
        users.createUser = USERID(LDBNAME(1)).
        
    RUN ipConfirmAdminUser IN THIS-PROCEDURE.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateASIUser C-Win 
PROCEDURE ipCreateASIUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Creating ASI User").

    DISABLE TRIGGERS FOR LOAD OF users.

    CREATE users.
    ASSIGN
        users.user_id = "asi"
        users.createDate = today
        users.createTime = time
        users.createUser = USERID(LDBNAME(1)).
        
    RUN ipConfirmAsiUser IN THIS-PROCEDURE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix C-Win 
PROCEDURE ipDataFix :
/*------------------------------------------------------------------------------
  Purpose:     Master Procedure for Data Fix Processes
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cThisEntry AS CHAR NO-UNDO.
    DEF VAR cTgtEnv AS CHAR NO-UNDO.

    RUN ipStatus ("Starting Data Fixes...").

    ASSIGN 
        cThisEntry = ENTRY(1,slEnvironments:{&SV})
        cThisEntry = ENTRY(2,cThisEntry,"-").

    IF intVer(cThisEntry) LT 160001 THEN
        RUN ipDataFix160001.
    IF intVer(cThisEntry) LT 160104 THEN
        RUN ipDataFix160104.
    IF intVer(cThisEntry) LT 160200 THEN
        RUN ipDataFix160200.
    IF intVer(cThisEntry) LT 160600 THEN
        RUN ipDataFixConfig.
    IF intVer(cThisEntry) LT 160609 THEN
        RUN ipDataFix160609.
    IF intVer(cThisEntry) LT 160700 THEN 
        RUN ipDataFix160700.
    IF intVer(cThisEntry) LT 160704 THEN
        RUN ipDataFix160704.
    IF intVer(cThisEntry) LT 160708 THEN
        RUN ipDataFix160708.
    IF intVer(cThisEntry) LT 160712 THEN
        RUN ipDataFix160712.

    RUN ipStatus ("Completed Data Fixes...").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160001 C-Win 
PROCEDURE ipDataFix160001 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bf-reftable FOR reftable.
    DEF VAR cnt AS INT NO-UNDO.
    DISABLE TRIGGERS FOR LOAD OF bf-reftable.

    RUN ipStatus ("  Data Fix 160001...").

    OUTPUT STREAM s1 TO reftable-phone-save.d APPEND.

    FOR EACH emailcod NO-LOCK:
        FOR EACH reftable NO-LOCK WHERE 
            reftable.reftable = "" AND 
            reftable.code = emailcod.emailcod:
            FIND FIRST phone NO-LOCK WHERE 
                RECID(phone) = INT(reftable.rec_key)
                NO-ERROR.
            IF AVAIL phone THEN DO:
                EXPORT STREAM s1 reftable.
                FIND bf-reftable EXCLUSIVE WHERE 
                    ROWID(bf-reftable) EQ ROWID(reftable)
                    NO-ERROR.
                IF AVAIL bf-reftable THEN ASSIGN
                    bf-reftable.rec_key = STRING(phone.rec_key).
                ASSIGN 
                    cnt = cnt + 1.
            END. /* If matching phone record found */
        END. /* each reftable */
    END. /* each emailcod */

    OUTPUT STREAM s1 CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160104 C-Win 
PROCEDURE ipDataFix160104 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cPrimComp AS CHAR NO-UNDO.
    DEF VAR iNextNum AS INT NO-UNDO.
    DEF VAR li-nxt-r-no AS INT NO-UNDO.
    DEF BUFFER bf-rctd FOR rm-rctd.
 
    RUN ipStatus ("  Data Fix 160104...").

    FOR EACH company NO-LOCK:
        FIND LAST po-ord NO-LOCK WHERE 
            po-ord.company EQ company.company 
            NO-ERROR.
        IF AVAIL po-ord THEN
            DYNAMIC-CURRENT-VALUE("po_Seq" + company.spare-char-1, "ASI") = po-ord.po-no + 1.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160200 C-Win 
PROCEDURE ipDataFix160200 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF vend.
    
    RUN ipStatus ("  Data Fix 160200...").

    FOR EACH company NO-LOCK:
        /*Set Payment Type Defaults*/
        RUN ipCheckPayMaster (company.company,"Check","Paper Check",YES).
        RUN ipCheckPayMaster (company.company,"Bill Pay","Online Bill Pay",NO).
        RUN ipCheckPayMaster (company.company,"Credit Card","Credit Card",NO).
        RUN ipCheckPayMaster (company.company,"ACH","ACH Electronic Transfer",NO).
        
        /*Convert spare-int values to payment-type*/
        FOR EACH vend EXCLUSIVE WHERE 
            vend.company EQ company.company:
            IF vend.spare-int-1 = 1 THEN ASSIGN 
                vend.payment-type = "ACH".
            ELSE IF vend.spare-int-2 = 1 THEN ASSIGN 
                vend.payment-type = "Bill Pay".
            ELSE ASSIGN 
                vend.payment-type = "Check".
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160609 C-Win 
PROCEDURE ipDataFix160609 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 160609...").

    RUN ipInvRnoSeq.
    RUN ipRelRnoSeq.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160700 C-Win 
PROCEDURE ipDataFix160700 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 160700...").

    /* 24948 - Must ALWAYS be set to "BOL" (true) */
    DISABLE TRIGGERS FOR LOAD OF oe-ctrl.
    FOR EACH oe-ctrl:
        ASSIGN
            oe-ctrl.u-inv = TRUE.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160704 C-Win 
PROCEDURE ipDataFix160704 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF job-code.
    DISABLE TRIGGERS FOR LOAD OF oe-rel.
       
    RUN ipStatus ("  Data Fix 160704...").

    /* Ensure jobCode sequence is GT 100 */
    IF CURRENT-VALUE(jobCodeDMIseq) LT 100 THEN ASSIGN
        CURRENT-VALUE(jobCodeDMIseq) = CURRENT-VALUE(jobCodeDMIseq) + 100.
        
    /* If any empty job-codes, assign dmiID */
    FOR EACH job-code WHERE 
        job-code.dmiID EQ 0:
        ASSIGN
            job-code.dmiID = NEXT-VALUE(jobCodeDMIseq).
    END. 
    
    /* If job-codes had been built with ID LT 100, fix them */
    IF CAN-FIND(FIRST job-code WHERE job-code.dmiID LT 100) THEN FOR EACH job-code EXCLUSIVE:
        ASSIGN
            job-code.dmiID = job-code.dmiID + 100.
    END.
    
    RUN ipConvQtyPerSet.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160708 C-Win 
PROCEDURE ipDataFix160708 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RUN ipStatus ("  Data Fix 160708...").

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160712 C-Win 
PROCEDURE ipDataFix160712 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF oe-rel.

    RUN ipStatus ("  Data Fix 160712...").

    RUN ipConvertUsrFile.
    RUN ipTurnOffUserColors.
    RUN ipFixPoEdiDirs.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFixConfig C-Win 
PROCEDURE ipDataFixConfig :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH config EXCLUSIVE:
        ASSIGN
            config.audit_dir = ".\CustFiles\Logs\AuditFiles"
            config.logs_dir = ".\CustFiles\Logs"
            config.spool_dir = ".\CustFiles\Logs\Spool".
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDelBadData C-Win 
PROCEDURE ipDelBadData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Removing Deprecated File Entries").

    DISABLE TRIGGERS FOR LOAD OF parmfile.
    DISABLE TRIGGERS FOR LOAD OF userlog.
    DISABLE TRIGGERS FOR LOAD OF mfdata.
    
    /* Remove parmfile database connections */
    FOR EACH parmfile:
        DELETE parmfile.
    END.

    /* Remove bad UDF entries */
    FOR EACH mfdata:
        IF INDEX(mfdata.mfgroup_data,"General Class") <> 0 THEN
            DELETE mfdata.
    END.

    /* Remove userlogs for proper user control counting */
    IF lNeedUsercontrol THEN FOR EACH userlog:
        DELETE userlog.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDelDupeNotes C-Win 
PROCEDURE ipDelDupeNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Don't need to do this for modern installs */
    IF cCurrVer GE "16.6" THEN DO:
        RUN ipStatus ("Version doesn't require notes de-dupe").
        RETURN.
    END.
        
    DISABLE TRIGGERS FOR LOAD OF notes.
    
    RUN ipStatus ("Deleting duplicate notes").

    ASSIGN
        delCtr = 0
        dupCtr = 0.
        
    FOR EACH notes NO-LOCK:
        ASSIGN jCtr = jCtr + 1.
    END.
    
    FOR EACH notes NO-LOCK:
        ASSIGN
            iCtr = iCtr + 1.
    
        IF ictr MODULO 1000 EQ 0 THEN 
            RUN ipStatus ("Deleting duplicate notes - Reviewing " + STRING(iCtr,">>>>>>>9") + " of " + STRING(jCtr,">>>>>>>9") + " records.").
    
        /* Remove exact duplicates */
        FIND bnotes WHERE
            ROWID(bnotes) <> ROWID(notes) AND
            bnotes.note_code = notes.note_code AND
            bnotes.note_date = notes.note_date AND
            bnotes.note_form_no = notes.note_form_no AND
            bnotes.note_group = notes.note_group AND
            bnotes.note_text = notes.note_text AND
            bnotes.note_time = notes.note_time AND
            bnotes.note_title = notes.note_title AND
            bnotes.note_type = notes.note_type AND
            bnotes.rec_key = notes.rec_key AND
            bnotes.user_id = notes.user_id
            EXCLUSIVE NO-ERROR.
        IF AVAIL bnotes THEN DO:
            ASSIGN 
                delCtr = delCtr + 1.
            DELETE bnotes.
        END.
        
        /* Write possible duplicates to file */
        FIND bnotes WHERE
            ROWID(bnotes) <> ROWID(notes) AND
            bnotes.note_code = notes.note_code AND
            bnotes.note_date = notes.note_date AND
            bnotes.note_form_no = notes.note_form_no AND
            bnotes.note_group = notes.note_group AND
            bnotes.note_time = notes.note_time AND
            bnotes.note_title = notes.note_title AND
            bnotes.note_type = notes.note_type AND
            bnotes.rec_key = notes.rec_key AND
            bnotes.user_id = notes.user_id
            EXCLUSIVE NO-ERROR.
        IF AVAIL bnotes THEN DO:
            ASSIGN 
                dupCtr = dupCtr + 1.
            IF dupCtr = 1 THEN DO:
                ASSIGN
                    FILE-INFO:FILE-NAME = "c:\tmp\."
                    cTemp = IF FILE-INFO:FULL-PATHNAME <> ? THEN "c:\tmp" ELSE "".
                IF cTemp = "" THEN DO:
                    ASSIGN
                        FILE-INFO:FILE-NAME = "c:\temp\."
                        cTemp = IF FILE-INFO:FULL-PATHNAME <> ? THEN "c:\temp" ELSE "".
                END.
                IF cTemp = "" THEN ASSIGN
                    cTemp = OS-GETENV("TEMP").
                OUTPUT STREAM outstream TO VALUE(cTemp + "\dupNotes.txt").
                PUT STREAM outstream UNFORMATTED
                    "List of possible duplicate notes detected on " + STRING(TODAY,"99/99/99") + CHR(10) + 
                    "-----------------------------------------------------------------------------------------" + CHR(10) + 
                    "Note Code Group     Type      Form      Date      Time      UserID       Text Begins     " + CHR(10) +
                    "-----------------------------------------------------------------------------------------".
            END.
            PUT STREAM outstream   
                bnotes.note_code                    AT 1
                bnotes.note_group                   AT 11
                bnotes.note_type                    AT 21
                STRING(bnotes.note_form_no)         AT 31
                STRING(bnotes.note_date,"99/99/99") AT 41
                STRING(bnotes.note_time,"HH:MM:SS") AT 51
                bnotes.USER_id                      AT 61
                bnotes.note_text FORMAT "x(30)"     AT 74.
        END.
    END.
    
    IF dupCtr > 0 
    OR delCtr > 0 THEN DO:
        OUTPUT STREAM outstream CLOSE.
    END.

    /* Deprecated - do not use
    /* Add additional field data - from WK populateNotesFields */
    FOR EACH notes EXCLUSIVE WHERE
        notes.createDate = ?:
        ASSIGN 
            notes.updateDate   = notes.note_date  
            notes.updateTime   = notes.note_time 
            notes.updateUser   = notes.user_id
            notes.createDate   = notes.note_date  
            notes.createTime   = notes.note_time 
            notes.createUser   = notes.user_id         
            .
    END.
    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipExpandFiles C-Win 
PROCEDURE ipExpandFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cCmdLine1 AS CHAR NO-UNDO.
    DEF VAR cCmdLine2 AS CHAR NO-UNDO.
    DEF VAR cCmdLine3 AS CHAR NO-UNDO.
    DEF VAR cFileType AS CHAR NO-UNDO.
    DEF VAR lProdExists AS LOG NO-UNDO.
    DEF VAR lTestExists AS LOG NO-UNDO.
    DEF VAR cTgtEnv AS CHAR NO-UNDO.
    DEF VAR cThisEntry AS CHAR NO-UNDO.
    DEF VAR cNewEntry AS CHAR NO-UNDO.
    DEF VAR cListItems AS CHAR NO-UNDO.
    DEF VAR cScreenValue AS CHAR NO-UNDO.
    
    RUN ipStatus ("Installing New ASI System Files").

    DO iCtr = 1 TO NUM-ENTRIES(slEnvironments:{&SV}):
        ASSIGN 
            cThisEntry = ENTRY(iCtr,slEnvironments:{&SV})
            cTgtEnv = fiMapDir:{&SV} + "\" + fiEnvDir:{&SV} + "\" + ENTRY(1,cThisEntry,"-").

        IF iCtr = 1 THEN DO:
            RUN ipStatus ("  Expanding files...").

            ASSIGN
                cCmdLine1 = cUpdCompressDir + "\7z.exe x " + cUpdProgramDir + "\Override.7z -y -o" + cUpdProgramDir + "\Override"
                cCmdLine2 = cUpdCompressDir + "\7z.exe x " + cUpdProgramDir + "\Programs.7z -y -o" + cUpdProgramDir + "\Programs"
                cCmdLine3 = cUpdCompressDir + "\7z.exe x " + cUpdProgramDir + "\Resources.7z -y -o" + cUpdProgramDir + "\Resources".
            IF SEARCH(cUpdProgramDir + "\Override.7z") NE ? THEN DO:
                OS-COMMAND SILENT VALUE(cCmdLine1).
            END.
            OS-COMMAND SILENT VALUE(cCmdLine2).
            OS-COMMAND SILENT VALUE(cCmdLine3).
        END.
        IF iCtr LT NUM-ENTRIES(slEnvironments:{&SV}) THEN DO:
            RUN ipStatus ("  Copying 'N' files to " + cThisEntry + "...").

            /* COPY the new files from Patch to Environment */
            ASSIGN
                cCmdLine1 = "XCOPY /E " + cUpdProgramDir + "\Override " + cTgtEnv + "\OverrideN"
                cCmdLine2 = "XCOPY /E " + cUpdProgramDir + "\Programs " + cTgtEnv + "\ProgramsN"
                cCmdLine3 = "XCOPY /E " + cUpdProgramDir + "\Resources " + cTgtEnv + "\ResourcesN".

            IF SEARCH(cUpdProgramDir + "\Override.7z") NE ? THEN DO:
                OS-CREATE-DIR VALUE(cTgtEnv + "\OverrideN").
                OS-COMMAND SILENT VALUE(cCmdLine1).
            END.
            OS-CREATE-DIR VALUE(cTgtEnv + "\ProgramsN").
            OS-COMMAND SILENT VALUE(cCmdLine2).
            OS-CREATE-DIR VALUE(cTgtEnv + "\ResourcesN").
            OS-COMMAND SILENT VALUE(cCmdLine3).

            /* Rename the old files to "O", then new files from "N", then delete "O" */
            RUN ipStatus ("  Renaming old files in " + cThisEntry + "...").
            OS-RENAME VALUE(cTgtEnv + "\Override") VALUE(cTgtEnv + "\OverrideO").
            OS-RENAME VALUE(cTgtEnv + "\Programs") VALUE(cTgtEnv + "\ProgramsO").
            OS-RENAME VALUE(cTgtEnv + "\Resources") VALUE(cTgtEnv + "\ResourcesO").

            RUN ipStatus ("  Renaming new files in " + cThisEntry + "...").
            OS-RENAME VALUE(cTgtEnv + "\OverrideN") VALUE(cTgtEnv + "\Override").
            OS-RENAME VALUE(cTgtEnv + "\ProgramsN") VALUE(cTgtEnv + "\Programs").
            OS-RENAME VALUE(cTgtEnv + "\ResourcesN") VALUE(cTgtEnv + "\Resources").

            RUN ipStatus ("  Deleting old files in " + cThisEntry + "...").
            OS-DELETE VALUE(cTgtEnv + "\OverrideO") RECURSIVE.
            OS-DELETE VALUE(cTgtEnv + "\ProgramsO") RECURSIVE.
            OS-DELETE VALUE(cTgtEnv + "\ResourcesO") RECURSIVE.
        END.
        ELSE DO:
            ASSIGN 
                cThisEntry = ENTRY(iCtr,slEnvironments:{&SV})
                cTgtEnv = fiMapDir:{&SV} + "\" + fiEnvDir:{&SV} + "\" + ENTRY(1,cThisEntry,"-").
            /* Skip the copy part, just MOVE the files  */
            RUN ipStatus ("  Renaming old files in " + cThisEntry + "...").
            OS-RENAME VALUE(cUpdProgramDir + "\Override") VALUE(cTgtEnv + "\OverrideN").
            OS-RENAME VALUE(cUpdProgramDir + "\Programs") VALUE(cTgtEnv + "\ProgramsN").
            OS-RENAME VALUE(cUpdProgramDir + "\Resources") VALUE(cTgtEnv + "\ResourcesN").

            /* Rename the old files to "O", then new files from "N", then delete "O" */
            RUN ipStatus ("  Renaming old files in " + cThisEntry + "...").
            OS-RENAME VALUE(cTgtEnv + "\Override") VALUE(cTgtEnv + "\OverrideO").
            OS-RENAME VALUE(cTgtEnv + "\Programs") VALUE(cTgtEnv + "\ProgramsO").
            OS-RENAME VALUE(cTgtEnv + "\Resources") VALUE(cTgtEnv + "\ResourcesO").

            RUN ipStatus ("  Renaming new files in " + cThisEntry + "...").
            OS-RENAME VALUE(cTgtEnv + "\OverrideN") VALUE(cTgtEnv + "\Override").
            OS-RENAME VALUE(cTgtEnv + "\ProgramsN") VALUE(cTgtEnv + "\Programs").
            OS-RENAME VALUE(cTgtEnv + "\ResourcesN") VALUE(cTgtEnv + "\Resources").

            RUN ipStatus ("  Deleting old files in " + cThisEntry + "...").
            OS-DELETE VALUE(cTgtEnv + "\OverrideO") RECURSIVE.
            OS-DELETE VALUE(cTgtEnv + "\ProgramsO") RECURSIVE.
            OS-DELETE VALUE(cTgtEnv + "\ResourcesO") RECURSIVE.
        END.
        ASSIGN
            cListItems = slEnvironments:LIST-ITEMS
            cScreenValue = slEnvironments:SCREEN-VALUE
            cNewEntry = ENTRY(1,cThisEntry,"-") + "-" + fiNewVer:{&SV}
            cListItems = REPLACE(cListItems,cThisEntry,cNewEntry)
            cScreenValue = REPLACE(cScreenValue,cThisEntry,cNewEntry)
            slEnvironments:LIST-ITEMS = cListItems
            slEnvironments:SCREEN-VALUE = cScreenValue.
            
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
    ASSIGN
        cPatchNo = fiNewVer:{&SV}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixPoEdiDirs C-Win 
PROCEDURE ipFixPoEdiDirs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cTestLoc AS CHAR NO-UNDO.
    
    RUN ipStatus(" Fix file locations for PO EDI").
    
    /* The correct target for this dir is <env>\CustFiles\EDIfiles\PO */
    /* Is it already correct? */
    ASSIGN
        cTestLoc = cEnvDir + "\" + ENTRY(1,slEnvironments:{&SV},"-") + "\CustFiles\EDIfiles\POs\poexport.dat".
    IF SEARCH(cTestLoc) NE ? THEN
        RETURN.        
        
    /* Is it in /Customers folder? */
    ASSIGN
        cTestLoc = cEnvDir + "\" + ENTRY(1,slEnvironments:{&SV},"-") + "\Customer\PO\poexport.dat".
    IF SEARCH(cTestLoc) NE ? THEN DO:
        RUN ipCopyDirs (cEnvDir + "\" + ENTRY(1,slEnvironments:{&SV},"-") + "\Customer\PO",
                        cEnvDir + "\" + ENTRY(1,slEnvironments:{&SV},"-") + "\CustFiles\EDIfiles\POs").
        RETURN.
    END.
    
    /* Is it in /PO? */
    ASSIGN
        cTestLoc = cEnvDir + "\" + ENTRY(1,slEnvironments:{&SV},"-") + "\PO\poexport.dat".
    IF SEARCH(cTestLoc) NE ? THEN 
        RUN ipCopyDirs (cEnvDir + "\" + ENTRY(1,slEnvironments:{&SV},"-") + "\PO",
                        cEnvDir + "\" + ENTRY(1,slEnvironments:{&SV},"-") + "\CustFiles\EDIfiles\POs").
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixUsers C-Win 
PROCEDURE ipFixUsers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Validating Required Users").

    IF NOT CAN-FIND (FIRST users WHERE
                     users.user_id = "asi") THEN
        RUN ipCreateAsiUser IN THIS-PROCEDURE.
    ELSE 
        RUN ipConfirmAsiUser IN THIS-PROCEDURE.

    IF NOT CAN-FIND (FIRST users WHERE
                     users.user_id = "admin") THEN
        RUN ipCreateAdminUser IN THIS-PROCEDURE.
    ELSE 
        RUN ipConfirmAdminUser IN THIS-PROCEDURE.

    RUN ipLoadNewUserData IN THIS-PROCEDURE.
    RUN ipCleanBadUserData IN THIS-PROCEDURE.

    ASSIGN
        tbComp-2:CHECKED IN FRAME {&FRAME-NAME} = TRUE.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipInvRnoSeq C-Win 
PROCEDURE ipInvRnoSeq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*  File:           util\InvRnoSeq.p                                         */
/*  Copyright:      (c)2017 Advanced Software Services, Inc.All rights rsrvd */
/*  Description:    Utility to assign inv_r_no_seq                           */
/*                  values from raw data. Apply to versions 16.6.8 and below.*/
/*                  (non-destructive if applied twice)                       */
/*                                                                           */
/*  Included files:     none                                                 */
/*  External RUN/CALL:  none                                                 */
/*  External files:     READ inv-head                                        */                   
/*                      READ company                                         */
/*                                                                           */
/*  Revision history:   MM/DD/YY    INIT    TKT     Description              */
/*                      12/01/17    wfk     24853   Original Version         */
/*---------------------------------------------------------------------------*/

    DEF VAR iCurrVal AS INT NO-UNDO.
    DEF VAR iLastDataValue AS INT NO-UNDO.
    DEF VAR iTries AS INT NO-UNDO.
    DEF VAR cCompSuffix AS CHAR NO-UNDO.
    
    RUN ipStatus ("    Data Fix InvRnoSeq...").

    /* Remove orphaned inv-line records */
    DISABLE TRIGGERS FOR LOAD OF inv-line.
    OUTPUT TO C:\tmp\OrphanInvLineRecs.d APPEND.
    FOR EACH inv-line WHERE
        NOT CAN-FIND (FIRST inv-head OF inv-line):
        EXPORT inv-line.
        DELETE inv-line.
    END.
    OUTPUT CLOSE.
    
    /* Create inv_r_no_seq from last inv-head by r-no */
    ASSIGN
        iTries = 0
        iCurrVal = 0
        CURRENT-VALUE(inv_r_no_seq) = 0.
    INVHEAD_RNO:
    DO WHILE iCurrVal EQ 0:
    
        FIND LAST inv-head NO-LOCK
          USE-INDEX r-no 
          NO-ERROR.
        
        IF AVAIL inv-head THEN DO:
            ASSIGN
                iLastDataValue = IF AVAIL inv-head THEN inv-head.r-no ELSE 0.
            
            /* If the record is in ambiguous state or otherwise returns 0, keep trying */
            IF iLastDataValue EQ 0 THEN DO WHILE iLastDataValue EQ 0:
                PAUSE 1 before-hide.
                FIND LAST inv-head NO-LOCK
                  USE-INDEX r-no 
                  NO-ERROR.
                ASSIGN
                    iLastDataValue = IF AVAIL inv-head THEN inv-head.r-no ELSE 0
                    iTries = iTries + 1.
                IF iTries GT 60 THEN DO: /* Try for 1 minute, then quit */
                    MESSAGE
                        "Unable to set sequence value for inv-head.r-no" SKIP
                        "Please contact Advantzware Support for assistance."
                        VIEW-AS ALERT-BOX ERROR.
                    LEAVE INVHEAD_RNO.
                END.
            END.
        
            ASSIGN
                CURRENT-VALUE(inv_r_no_seq) = iLastDataValue
                iCurrVal = CURRENT-VALUE(inv_r_no_seq).       
        END.  
        ELSE DO:
            LEAVE INVHEAD_RNO.
        END.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadAuditRecs C-Win 
PROCEDURE ipLoadAuditRecs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Loading AuditTbl Records").

    DISABLE TRIGGERS FOR LOAD OF audittbl.
    
    INPUT FROM VALUE(cUpdDataDir + "\audittbl.d") NO-ECHO.
    REPEAT:
        CREATE AuditTbl.
        IMPORT AuditTbl.
    END.
    INPUT CLOSE.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadEmailCodes C-Win 
PROCEDURE ipLoadEmailCodes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Loading Email codes").

    DISABLE TRIGGERS FOR LOAD OF emailcod.
    
    INPUT FROM VALUE(cUpdDataDir + "\emailcod.d") NO-ECHO.
    REPEAT:
        CREATE ttEmailcod.
        IMPORT 
            ttEmailcod.
        FIND FIRST emailcod EXCLUSIVE WHERE 
            emailcod.emailcod EQ ttEmailcod.emailcod
            NO-ERROR.
        IF NOT AVAIL emailcod THEN DO:
            CREATE emailcod.
            BUFFER-COPY ttEmailcod TO emailcod.
        END.
    END.
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE ttEmailcod.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadLookups C-Win 
PROCEDURE ipLoadLookups :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Loading Lookups").

    DISABLE TRIGGERS FOR LOAD OF lookups.
    
    FOR EACH lookups:
        DELETE lookups.
    END.
    
    INPUT FROM VALUE(cUpdDataDir + "\lookups.d") NO-ECHO.
    REPEAT:
        CREATE lookups.
        IMPORT lookups.
    END.
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE ttLookups.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadMenus C-Win 
PROCEDURE ipLoadMenus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcDir AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcTgtDir AS CHAR NO-UNDO.
    DEF VAR cFileStream AS CHAR NO-UNDO.
    DEF VAR cThisEntry AS CHAR NO-UNDO.
    DEF VAR cTgtEnv AS CHAR NO-UNDO.

    RUN ipStatus ("Loading New Menus").

    
    DO iCtr = 1 TO NUM-ENTRIES(slEnvironments:{&SV}):
        ASSIGN 
            cThisEntry = ENTRY(iCtr,slEnvironments:{&SV})
            cTgtEnv = fiMapDir:{&SV} + "\" + fiEnvDir:{&SV} + "\" + ENTRY(1,cThisEntry,"-").

        INPUT FROM OS-DIR (ipcDir).

        REPEAT:
            IMPORT cFileStream.
            FILE-INFO:FILE-NAME = ipcDir + "\" + cFileStream.
            IF SUBSTRING(FILE-INFO:FILE-NAME,LENGTH(FILE-INFO:FILE-NAME),1) EQ "." THEN DO:
                NEXT.
            END.
            ELSE IF FILE-INFO:FILE-TYPE BEGINS "F" THEN DO:
                OS-COPY VALUE(FILE-INFO:FILE-NAME) VALUE(cTgtEnv).
            END.
            ELSE DO:
                OS-CREATE-DIR VALUE(cTgtEnv + "\" + cFileStream).
                RUN ipLoadMenus IN THIS-PROCEDURE (FILE-INFO:FILE-NAME,cTgtEnv + "\Addon").
            END.
        END.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadModules C-Win 
PROCEDURE ipLoadModules :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Loading Module Records").

    DISABLE TRIGGERS FOR LOAD OF module.
    
    INPUT FROM VALUE(cUpdDataDir + "\module.d") NO-ECHO.
    REPEAT:
        CREATE ttModule.
        IMPORT ttModule.
        IF CAN-FIND(module WHERE 
                    module.db-name EQ ttModule.db-name AND
                    module.module EQ ttModule.module) THEN DO:
            DELETE ttModule.
            NEXT.
        END.
        ELSE DO:
            CREATE module.
            BUFFER-COPY ttModule TO module.
            DELETE ttModule.
        END.
    END.
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE ttmodule.

/* From Wade's convertModule.p in ticket 23532 */
cfrom = "m2." . cTo = "outboundProcess." . RUN ipConvertModule.

cfrom = "m31." . cTo = "eddoc." . RUN ipConvertModule.
cfrom = "m33." . cTo = "edivtran." . RUN ipConvertModule.
cfrom = "m34." . cTo = "wedshtr." . RUN ipConvertModule.
cfrom = "m36." . cTo = "edcat." . RUN ipConvertModule.

cfrom = "m41." . cTo = "edmast." . RUN ipConvertModule.
cfrom = "m42." . cTo = "edcode." . RUN ipConvertModule.
cfrom = "m43." . cTo = "edShipTo." . RUN ipConvertModule.
cfrom = "m44." . cTo = "edICXRef." . RUN ipConvertModule.
cfrom = "m45." . cTo = "edshipvia." . RUN ipConvertModule.
cfrom = "m46." . cTo = "edco." . RUN ipConvertModule.
cfrom = "m47." . cTo = "edSetID." . RUN ipConvertModule.
cfrom = "m48." . cTo = "edPartnerGrp." . RUN ipConvertModule.
cfrom = "m49." . cTo = "ediPartnerSegment." . RUN ipConvertModule.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadNewUserData C-Win 
PROCEDURE ipLoadNewUserData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF users.
    DISABLE TRIGGERS FOR LOAD OF usr.
    DISABLE TRIGGERS FOR LOAD OF reftable.

    RUN ipStatus ("Updating User Table Fields").

    /* Add/convert data for new users table fields */
    FOR EACH users EXCLUSIVE:
        IF users.userType = "" OR users.userType = ? THEN DO:
            CASE users.user_id:
                WHEN "ASI" OR
                WHEN "Administrator" OR
                WHEN "Admin"THEN ASSIGN users.userType = "Administrator".
                OTHERWISE ASSIGN users.userType = "Full User".
            END CASE.
        END.
        IF users.securityLevel = 0 THEN DO:
            CASE users.user_id:
                WHEN "ASI" THEN ASSIGN users.securityLevel = 1000.
                WHEN "Administrator" OR
                WHEN "Admin"THEN ASSIGN users.securityLevel = 900.
                OTHERWISE ASSIGN users.securityLevel = 100.
            END CASE.
        END.
        /* Ticket 30974 - disable colors/fonts */
        ASSIGN
            users.use_colors = FALSE
            users.use_fonts = FALSE.
            

        FOR EACH reftable EXCLUSIVE WHERE 
            reftable.reftable EQ "users.user-docs" AND
            reftable.company EQ users.user_id:
            ASSIGN
                users.showOnPO = IF users.showOnPO = TRUE OR reftable.val[1] = 1 THEN TRUE ELSE FALSE
                users.showOnBOL = IF users.showOnBOL = TRUE OR reftable.val[2] = 1 THEN TRUE ELSE FALSE
                users.showOnInv = IF users.showOnInv = TRUE OR reftable.val[3] = 1 THEN TRUE ELSE FALSE
                users.showOnAck = IF users.showOnAck = TRUE OR reftable.val[4] = 1 THEN TRUE ELSE FALSE
                users.showOnQuote = IF users.showOnQuote = TRUE OR reftable.val[5] = 1 THEN TRUE ELSE FALSE
                .
            DELETE reftable.
        END.
        FOR EACH reftable EXCLUSIVE WHERE
            reftable.reftable EQ "users.phone-no" AND
            reftable.company EQ users.user_id:
            ASSIGN
                users.phone = IF users.phone = "" AND reftable.CODE = "" THEN reftable.CODE ELSE users.phone
                .
            DELETE reftable.
        END.
        FOR EACH reftable EXCLUSIVE WHERE
            reftable.reftable EQ "users.fax-no" AND
            reftable.company EQ users.user_id:
            ASSIGN
                users.fax = IF users.fax = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.fax
                .
            DELETE reftable.
        END.
        FOR EACH reftable EXCLUSIVE WHERE
            reftable.reftable EQ "users.phone-cnty" AND
            reftable.company EQ users.user_id:
            ASSIGN
                users.phone-cnty = IF users.phone-cnty = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.phone-cnty
                .
            DELETE reftable.
        END.
        FOR EACH reftable EXCLUSIVE WHERE
            reftable.reftable EQ "users.fax-cnty" AND
            reftable.company EQ users.user_id:
            ASSIGN
                users.fax-cnty = IF users.fax-cnty = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.fax-cnty
                .
            DELETE reftable.
        END.
    END. /* each users */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadPrograms C-Win 
PROCEDURE ipLoadPrograms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Loading Program Master Records").

    DISABLE TRIGGERS FOR LOAD OF prgrms.
    
    INPUT FROM VALUE(cUpdDataDir + "\prgrms.d") NO-ECHO.
    REPEAT:
        CREATE ttPrgms.
        IMPORT 
            ttPrgms.prgmname
            ttPrgms.prgtitle
            ttPrgms.run_persistent
            ttPrgms.can_run
            ttPrgms.can_create
            ttPrgms.can_update
            ttPrgms.can_delete
            ttPrgms.dir_group
            ttPrgms.use_colors
            ttPrgms.use_fonts
            ttPrgms.widget_bgc
            ttPrgms.widget_fgc
            ttPrgms.widget_font
            ttPrgms.track_usage
            ttPrgms.popup
            ttPrgms.prgm_ver
            ttPrgms.menu_item
            ttPrgms.mfgroup 
            ttPrgms.rec_key.

        FIND FIRST prgrms EXCLUSIVE WHERE 
            prgrms.prgmname EQ ttPrgms.prgmname 
            NO-ERROR.
        IF NOT AVAIL prgrms THEN DO:
            CREATE prgrms.
            BUFFER-COPY ttPrgms TO prgrms
            ASSIGN 
                prgrms.can_run = '*'
                prgrms.can_create = '*'
                prgrms.can_update = '*'
                prgrms.can_delete = '*'.
        END.
        ELSE DO:
            ASSIGN 
                prgrms.prgtitle = ttPrgms.prgtitle
                prgrms.run_persistent = ttPrgms.RUN_persistent
                prgrms.dir_group = ttPrgms.DIR_group
                prgrms.use_colors = ttPrgms.USE_colors
                prgrms.use_fonts = ttPrgms.USE_fonts
                prgrms.track_usage = ttPrgms.track_usage
                prgrms.popup = ttPrgms.popup
                prgrms.prgm_ver = ttPrgms.prgm_ver
                prgrms.menu_item = ttPrgms.MENU_item
                prgrms.mfgroup = ttPrgms.mfgroup.
             DO i = 1 TO 13:
                ASSIGN 
                    prgrms.widget_bgc[i] = ttPrgms.WIDGET_bgc[i]
                    prgrms.widget_fgc[i] = ttPrgms.WIDGET_fgc[i]
                    prgrms.widget_font[i] = ttPrgms.WIDGET_font[i].
            END.
        END.
    END.
    INPUT CLOSE.
        
    /* Delete records no longer used */
    DISABLE TRIGGERS FOR LOAD OF prgrms.
    FOR EACH prgrms EXCLUSIVE WHERE 
        NOT CAN-FIND(FIRST ttPrgms WHERE ttPrgms.prgmname = prgrms.prgmname ):
        DELETE prgrms.
    END.
    
    EMPTY TEMP-TABLE ttPrgms.

    /* Fix "about." prgrms record description */
    FIND FIRST prgrms EXCLUSIVE-LOCK WHERE
        prgrms.prgmname EQ "about." 
        NO-ERROR.
    IF NOT AVAILABLE prgrms THEN DO:
        CREATE prgrms.
        ASSIGN
            prgrms.prgmname = "about."
            prgrms.dir_group = "nosweat"
            prgrms.run_persistent = YES
            prgrms.menu_item = YES
            .
    END.
    ASSIGN
        prgrms.prgtitle = "About". 
        
    /* Fix "audit." program master regardless of existing entry */
    FIND FIRST prgrms EXCLUSIVE-LOCK WHERE
        prgrms.prgmname EQ "audit." 
        NO-ERROR.
    IF NOT AVAILABLE prgrms THEN
        CREATE prgrms.
    ASSIGN
        prgrms.prgmname = "audit."
        prgrms.dir_group = "system"
        prgrms.run_persistent = YES
        prgrms.menu_item = YES
        .
        
    /* Ensure 'admin' user has same privileges as 'asi' */
    /* This is better handled with new security, but eliminates some access issues */
    /* See ticket 27968 */
    FOR EACH prgrms:
        IF CAN-DO(prgrms.can_run,"asi") 
        AND NOT CAN-DO(prgrms.can_run,"admin") THEN ASSIGN
            prgrms.can_run = prgrms.can_run + ",admin".
        IF CAN-DO(prgrms.can_create,"asi") 
        AND NOT CAN-DO(prgrms.can_create,"admin") THEN ASSIGN
            prgrms.can_create = prgrms.can_create + ",admin".
        IF CAN-DO(prgrms.can_update,"asi") 
        AND NOT CAN-DO(prgrms.can_update,"admin") THEN ASSIGN
            prgrms.can_update = prgrms.can_update + ",admin".
        IF CAN-DO(prgrms.can_delete,"asi") 
        AND NOT CAN-DO(prgrms.can_delete,"admin") THEN ASSIGN
            prgrms.can_delete = prgrms.can_delete + ",admin".
    END.
    
    /* Added usergrp test per BV request - same ticket */
    DISABLE TRIGGERS FOR LOAD OF usergrps.
    FOR EACH usergrps:
        IF CAN-DO(usergrps.users,"asi") 
        AND NOT CAN-DO(usergrps.users,"admin") THEN ASSIGN
            usergrps.users = usergrps.users + ",admin".
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadProgramXref C-Win 
PROCEDURE ipLoadProgramXref :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Loading Program Master Cross-References").

    DISABLE TRIGGERS FOR LOAD OF prgmxref.
    
    INPUT FROM VALUE(cUpdDataDir + "\prgmxref.d") NO-ECHO.
    REPEAT:
        CREATE ttPrgmxref.
        IMPORT 
            ttPrgmxref.table_name
            ttPrgmxref.prgmname
            ttPrgmxref.pageno.
        FIND FIRST prgmxref EXCLUSIVE WHERE 
            prgmxref.table_name EQ ttPrgmxref.table_name 
            NO-ERROR.
        IF NOT AVAIL prgmxref THEN DO:
            CREATE prgmxref.
            BUFFER-COPY ttPrgmxref TO prgmxref.
        END.
        ELSE DO:
            ASSIGN 
                prgmxref.prgmname = ttPrgmxref.prgmname
                prgmxref.pageno = ttPrgmxref.pageno.
        END.
    END.
    INPUT CLOSE.
        
    /* Delete records no longer used */
    FOR EACH prgmxref EXCLUSIVE WHERE
        NOT CAN-FIND(FIRST ttPrgmxref WHERE ttPrgmxref.table_name = prgmxref.table_name ):
        DELETE prgmxref.
    END.
    
    EMPTY TEMP-TABLE ttPrgmxref.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadUtilCodes C-Win 
PROCEDURE ipLoadUtilCodes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Loading New Utilities").

    DISABLE TRIGGERS FOR LOAD OF reftable.
    
    FOR EACH reftable WHERE 
        reftable.reftable EQ 'Utilities':
        DELETE reftable.
    END. 

    INPUT FROM VALUE(cUpdDataDir + "\reftable.d") NO-ECHO.
    REPEAT:
        CREATE ttReftable.
        IMPORT ttReftable.
        IF NOT ttRefTable.reftable EQ "Utilities" THEN DO:
            DELETE ttRefTable.
            NEXT.
        END.
        ELSE DO:
            CREATE reftable.
            BUFFER-COPY ttReftable TO reftable.
            DELETE ttRefTable.
        END.
    END. 
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE ttReftable.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadUtilNotes C-Win 
PROCEDURE ipLoadUtilNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Loading Utility Notes").

    DISABLE TRIGGERS FOR LOAD OF notes.
    
    FOR EACH reftable NO-LOCK WHERE 
        reftable.reftable EQ 'Utilities':
        FOR EACH notes EXCLUSIVE WHERE 
            notes.rec_key EQ reftable.rec_key:
            DELETE notes.
        END. 
    END. 
    
    INPUT FROM VALUE(cUpdDataDir + "\notes.d") NO-ECHO.
    REPEAT:
        CREATE ttNotes.
        IMPORT ttNotes.
        IF NOT CAN-FIND (FIRST reftable WHERE
                reftable.reftable EQ "Utilities" AND
                reftable.rec_key EQ ttNotes.rec_key) THEN DO:
            DELETE ttNotes.
            NEXT.
        END.
        ELSE DO:
            CREATE notes.
            BUFFER-COPY ttNotes TO notes.
            DELETE ttNotes.
        END.
    END. 
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE ttNotes.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipProcessAll C-Win 
PROCEDURE ipProcessAll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    RUN ipStatus ("Beginning Patch Application").
    ASSIGN
        SELF:LABEL = "Processing..."
        SELF:SENSITIVE = FALSE.

    IF tbBackupDBs:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipBackupDBs.
        ASSIGN
            lSuccess = TRUE
            tbComp-10:CHECKED = TRUE.
    END.
    
    ASSIGN
        slDatabases:{&SV} = ENTRY(1,slDatabases:LIST-ITEMS).
    DO iCtr = 1 TO NUM-ENTRIES(slDatabases:{&SV}):
        
        IF tbUserControl:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
            RUN ipUpdateUserControl.
            ASSIGN
                lSuccess = TRUE
                tbComp-1:CHECKED = TRUE.
        END.
        IF tbUserCleanup:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
            RUN ipFixUsers.
            ASSIGN
                lSuccess = TRUE
                tbComp-2:CHECKED = TRUE.
        END.
        IF tbDelBadData:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
            RUN ipDelBadData.
            ASSIGN
                lSuccess = TRUE
                tbComp-4:CHECKED = TRUE.
        END.
        IF tbUpdateMaster:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
            RUN ipUpdateMaster.
            ASSIGN
                lSuccess = TRUE
                tbComp-5:CHECKED = TRUE.
        END.
        IF tbRunDataFix:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
            RUN ipDataFix.
            ASSIGN
                lSuccess = TRUE
                tbComp-7:CHECKED = TRUE.
        END.
        IF tbDelDupeNotes:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
            RUN ipDelDupeNotes.
            ASSIGN
                lSuccess = TRUE
                tbComp-13:CHECKED = TRUE.
        END.
        IF tbUpdateNK1s:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
            RUN ipUpdateNK1s.
            /* RUN ipVerifyNK1Changes. */
            ASSIGN
                lSuccess = TRUE
                tbComp-14:CHECKED = TRUE.
        END.
        /*
        IF tbUpdateFileLocs:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
            RUN ipxxx.
            ASSIGN
                lSuccess = TRUE
                tbComp-15:CHECKED = TRUE.
        END.
        */
        
    END.        
    
    IF tbLoadMenus:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipLoadMenus (cUpdMenuDir,cEnvProdDir).
        ASSIGN
            lSuccess = TRUE
            tbComp-6:CHECKED = TRUE.
    END.

    IF tbBuildDirs:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipBuildDirs.
        ASSIGN
            lSuccess = TRUE
            tbComp-3:CHECKED = TRUE.
    END.

    IF tbUpdateStartup:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipCopyStartup.
        ASSIGN
            lSuccess = TRUE
            tbComp-8:CHECKED = TRUE.
    END.

    IF tbRelNotes:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipCopyRelNotes.
        ASSIGN
            lSuccess = TRUE
            tbComp-9:CHECKED = TRUE.
    END.

    IF tbBackupFiles:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipArchiveFiles.
        ASSIGN
            lSuccess = TRUE
            tbComp-11:CHECKED = TRUE.
    END.

    IF tbInstallFiles:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipExpandFiles.
        ASSIGN
            lSuccess = TRUE
            tbComp-12:CHECKED = TRUE.
    END.

    IF tbRefTableConv:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipRefTableConv.
        ASSIGN
            lSuccess = TRUE
            tbComp-17:CHECKED = TRUE.
    END.

    IF tbUpdateIni:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipUpdateTTIniFile.
        RUN ipWriteIniFile.
        ASSIGN
            lSuccess = TRUE
            tbComp-16:CHECKED = TRUE.
    END.
    
    RUN ipStatus ("Patch Application Complete").

    ASSIGN
        SELF:LABEL = "Start Update"
        SELF:SENSITIVE = TRUE
        fiCurrVer:{&SV} = fiNewVer:{&SV}
        fiVerDate:{&SV} = STRING(TODAY,"99/99/99").
        
    APPLY 'leave' to fiCurrVer.
    
    STATUS INPUT.

    IF lSuccess THEN MESSAGE
        "Congratulations!  Your upgrade to Advantzware Version " + fiNewVer:{&SV} + " is complete." SKIP
        "Please contact support@advantzware.com with any questions or issues."
        VIEW-AS ALERT-BOX.
    ELSE MESSAGE
        "No action was specified for this session."
        VIEW-AS ALERT-BOX.
        

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRefTableConv C-Win 
PROCEDURE ipRefTableConv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cOrigPropath AS CHAR NO-UNDO.
    DEF VAR cNewPropath AS CHAR NO-UNDO.
    DEF VAR cThisElement AS CHAR NO-UNDO.
    DISABLE TRIGGERS FOR LOAD OF reftable1.
    DISABLE TRIGGERS FOR LOAD OF oe-rel.
    
    IF ipiLevel LT 10 THEN DO:
        MESSAGE
            "WARNING - RefTable Conversion Time:" SKIP(1)
            "This operation can potentially take several hours to complete," SKIP
            "depending on several factors including the size and age of your" SKIP
            "database, the processing power and available resources of your" SKIP
            "server, and other considerations.  You may consider running this" SKIP
            "task separately from other upgrade choices, or you can run this" SKIP
            "from within the Advantzware system using a conversion utility." SKIP
            "Press 'Yes' to continue with the conversion, or 'No' to defer it."
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO update lContinue AS LOG.
            
        IF NOT lContinue THEN DO:
            RUN ipStatus ("Reftable conversion was deferred").
            RETURN.
        END.
    END.

    RUN ipStatus ("Converting Reftable records...").

    DO iCtr = 1 TO NUM-ENTRIES(slEnvironments:{&SV}):
        ASSIGN
            cThisElement = ENTRY(iCtr,slEnvironments:{&SV})
            cOrigPropath = PROPATH
            cNewPropath = fiMapDir:{&SV} + "\" + fiEnvDir:{&SV} + "\" +
                          ENTRY(1,cThisElement,"-") + "\Programs," + PROPATH
            PROPATH = cNewPropath.
        RUN ipStatus ("   ReftableConv for " + ENTRY(1,cThisElement,"-")).
        RUN 
            VALUE(SEARCH("RefTableConv.r")).
        ASSIGN
            PROPATH = cOrigPropath.
    END.

    /* Ticket 27898 */
    RUN ipStatus ("   Ticket 27898 oe-rel.s-code").
    FOR EACH reftable1 EXCLUSIVE WHERE
        reftable1.reftable EQ 'oe-rel.s-code' AND 
        reftable1.val[1] NE 1,
        FIRST oe-rel EXCLUSIVE WHERE
            oe-rel.r-no EQ integer(reftable1.company) AND 
            oe-rel.s-code NE reftable1.code:
        ASSIGN 
            reftable1.val[1] = 1
            oe-rel.s-code = reftable1.code.
    END.

    /* Ticket 32053 - oe-rel customer lot number */
    RUN ipStatus ("   Ticket 32053 oe-rel.lot-no").
    OUTPUT TO c:\tmp\reftable-oe-rel.txt.
    FOR EACH reftable1 EXCLUSIVE WHERE 
        reftable1.reftable = "oe-rel.lot-no" AND
        reftable1.spare-char-1 NE "1" AND
        reftable1.spare-char-2 NE "1" AND
        reftable1.spare-char-3 NE "1":
        FIND FIRST oe-rel EXCLUSIVE WHERE
            oe-rel.r-no = INT(reftable1.company)
            NO-ERROR.
        IF AVAILABLE oe-rel THEN DO: 
            IF oe-rel.lot-no EQ "" 
            AND reftable1.code NE "" THEN DO:
                EXPORT oe-rel.
                ASSIGN 
                    oe-rel.lot-no = reftable1.code
                    reftable1.spare-char-1 = "1".
            END.
            IF oe-rel.frt-pay EQ "" 
            AND reftable1.code2 NE "" THEN DO:
                EXPORT oe-rel.
                ASSIGN 
                    oe-rel.frt-pay = reftable1.code2
                    reftable1.spare-char-2 = "1".
            END.
            IF oe-rel.fob-code EQ "" 
            AND reftable1.dscr NE "" THEN DO:
                EXPORT oe-rel.
                ASSIGN 
                    oe-rel.fob-code = reftable1.dscr
                    reftable1.spare-char-3 = "1".
            END.
        END.   
    END.  /*FOR EACH reftable1*/  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRelRnoSeq C-Win 
PROCEDURE ipRelRnoSeq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*  File:           DeploymentFiles\DataFixPrograms\oeRelSeq.p               */
/*  Copyright:      (c)2017 Advanced Software Services, Inc.All rights rsrvd */
/*  Description:    Utility to assign oerel_rno_seq and oerel_release_seq    */
/*                  values from raw data. Apply to versions 16.6.8 and below.*/
/*                  (non-destructive if applied twice)                       */
/*                                                                           */
/*  Included files:     none                                                 */
/*  External RUN/CALL:  none                                                 */
/*  External files:     READ oe-rel                                          */
/*                      READ oe-relh                                         */
/*                      READ company                                         */
/*                                                                           */
/*  Revision history:   MM/DD/YY    INIT    TKT     Description              */
/*                      12/01/17    MYT     24853   Original Version         */
/*---------------------------------------------------------------------------*/

    DEF VAR iCurrVal AS INT NO-UNDO.
    DEF VAR iLastDataValue AS INT NO-UNDO.
    DEF VAR iTries AS INT NO-UNDO.
    DEF VAR cCompSuffix AS CHAR NO-UNDO.

    RUN ipStatus ("    Data Fix InvRnoSeq...").


    /* Create oerel_rno_seq from last oe-rel by r-no */
    ASSIGN
        iTries = 0
        iCurrVal = 0
        CURRENT-VALUE(oerel_rno_seq) = 0.

    OEREL_RNO:
    DO WHILE iCurrVal EQ 0:
        FIND FIRST oe-rel NO-LOCK 
            USE-INDEX seq-no 
            NO-ERROR.
        ASSIGN
            iLastDataValue = IF AVAIL oe-rel THEN oe-rel.r-no ELSE 0.
        /* If the record is in ambiguous state or otherwise returns 0, keep trying */
        IF iLastDataValue EQ 0 THEN DO WHILE iLastDataValue EQ 0:
            PAUSE 1 BEFORE-HIDE.
            FIND FIRST oe-rel NO-LOCK 
                USE-INDEX seq-no 
                NO-ERROR.
            ASSIGN
                iTries = iTries + 1
                iLastDataValue = IF AVAIL oe-rel THEN oe-rel.r-no ELSE 0.
            IF iTries GT 5 THEN DO: /* Try for 1 minute, then quit */
                MESSAGE
                    "Unable to set sequence value for oe-rel.r-no" SKIP
                    "Please contact Advantzware Support for assistance."
                    VIEW-AS ALERT-BOX ERROR.
                LEAVE OEREL_RNO.
            END.
        END.
        ASSIGN
            CURRENT-VALUE(oerel_rno_seq) = iLastDataValue
            iCurrVal = CURRENT-VALUE(oerel_rno_seq).
    END.    

    /* Create oerel_release_seq from last oe-relh by r-no (NOT by company) */
    ASSIGN
        iTries = 0
        iCurrVal = 0
        CURRENT-VALUE(oerel_release_seq) = 0.
    OEREL_REL_NOCO:
    DO WHILE iCurrVal EQ 0:
        FIND LAST oe-relh NO-LOCK 
            USE-INDEX r-no 
            NO-ERROR.
        ASSIGN
            iLastDataValue = IF AVAIL oe-relh THEN oe-relh.r-no ELSE 0.
        /* If the record is in ambiguous state or otherwise returns 0, keep trying */
        IF iLastDataValue EQ 0 THEN DO WHILE iLastDataValue EQ 0:
            PAUSE 1 BEFORE-HIDE.
            FIND LAST oe-relh NO-LOCK 
                USE-INDEX r-no 
                NO-ERROR.
            ASSIGN
                iTries = iTries + 1
                iLastDataValue = IF AVAIL oe-relh THEN oe-relh.r-no ELSE 0.
            IF iTries GT 5 THEN DO: /* Try for 1 minute, then quit */
                MESSAGE
                    "Unable to set sequence value for oe-relh.r-no" SKIP
                    "Please contact Advantzware Support for assistance."
                    VIEW-AS ALERT-BOX ERROR.
                LEAVE OEREL_REL_NOCO.
            END.
        END.
        ASSIGN
            CURRENT-VALUE(oerel_release_seq) = iLastDataValue
            iCurrVal = CURRENT-VALUE(oerel_release_seq).       
    END.    

    /* Create oerel_release_seq from last oe-relh by r-no BY COMPANY */
    FOR EACH company:
        IF company.spare-char-1 EQ "" 
        OR INT(company.spare-char-1) GT 10 THEN NEXT.
        ASSIGN
            iLastDataValue = 0
            cCompSuffix = company.spare-char-1
            iTries = 0
            iCurrVal = 0
            DYNAMIC-CURRENT-VALUE("oerel_release_seq" + cCompSuffix, "ASI") = 0.
        OEREL_REL_USINGCO:
        DO WHILE iCurrVal EQ 0:
            IF NOT CAN-FIND(FIRST oe-relh NO-LOCK WHERE 
                oe-relh.company EQ company.company) THEN
                LEAVE OEREL_REL_USINGCO.
            FIND LAST oe-relh NO-LOCK WHERE 
                oe-relh.company EQ company.company
                USE-INDEX release# 
                NO-ERROR.
            ASSIGN
                iLastDataValue = IF AVAIL oe-relh THEN oe-relh.release# ELSE 0.
            /* If the record is in ambiguous state or otherwise returns 0, keep trying */
            IF iLastDataValue EQ 0 THEN DO WHILE iLastDataValue EQ 0:
                PAUSE 1 BEFORE-HIDE.
                FIND LAST oe-relh NO-LOCK WHERE 
                    oe-relh.company EQ company.company
                    USE-INDEX release# 
                    NO-ERROR.
                ASSIGN
                    iTries = iTries + 1
                    iLastDataValue = IF AVAIL oe-relh THEN oe-relh.release# ELSE 0.
                IF iTries GT 2 THEN DO: /* Try for 1 minute, then quit */
                    MESSAGE
                        "Unable to set sequence value for oe-relh.r-no" SKIP
                        "Please contact Advantzware Support for assistance."
                        VIEW-AS ALERT-BOX ERROR.
                    LEAVE OEREL_REL_USINGCO.
                END.
            END.
            DYNAMIC-CURRENT-VALUE("oerel_release_seq" + cCompSuffix, "ASI") = iLastDataValue.
            ASSIGN
                iCurrVal = DYNAMIC-CURRENT-VALUE("oerel_release_seq" + cCompSuffix, "ASI").       
                
        END. 
    END.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetAdminPwd C-Win 
PROCEDURE ipSetAdminPwd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Setting ADMIN Password").

    FIND FIRST _User WHERE 
        _User._UserId = "admin" 
        EXCLUSIVE-LOCK NO-ERROR.

    IF NOT AVAIL (_User) THEN DO: 
        CREATE _User.
        ASSIGN
            _User._UserId = "ADMIN"
            _User._Password = ENCODE("admin").
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetAsiPwd C-Win 
PROCEDURE ipSetAsiPwd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        
    RUN ipStatus ("Setting ASI Password").

    FIND FIRST _User WHERE 
        _User._UserId = "asi" 
        EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL (_User) THEN DO:
        BUFFER-COPY _User EXCEPT _tenantID _User._Password TO tempUser.
        ASSIGN 
            tempUser._Password = "ifaOfSAcSdialAkd".
        DELETE _User.
        CREATE _User.
        BUFFER-COPY tempUser EXCEPT _tenantid TO _User.
    END.
    ELSE DO:
        CREATE _User.
        ASSIGN
            _User._UserId = "asi"
            _User._Password = "ifaOfSAcSdialAkd".
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
        ASSIGN
            slEnvironments:LIST-ITEMS IN FRAME {&FRAME-NAME}= ""
            slDatabases:LIST-ITEMS = "".
        
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
                WHEN "lockoutTries" THEN ASSIGN fiLockoutTries:{&SV} = ttIniFile.cVarValue.
                WHEN "adminDir" THEN ASSIGN fiAdminDir:{&SV} = ttIniFile.cVarValue.
                WHEN "backupDir" THEN ASSIGN fiBackupDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbDir" THEN ASSIGN fiDbDir:{&SV} = ttIniFile.cVarValue.
                WHEN "deskDir" THEN ASSIGN fiDeskDir:{&SV} = ttIniFile.cVarValue.
                WHEN "docDir" THEN ASSIGN fiDocDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envDir" THEN ASSIGN fiEnvDir:{&SV} = ttIniFile.cVarValue.
                WHEN "installDir" THEN ASSIGN fiInstallDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updatesDir" THEN ASSIGN fiUpdatesDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbAdmin" THEN ASSIGN fiDbAdmin:{&SV} = ttIniFile.cVarValue.
                WHEN "envAdmin" THEN ASSIGN fiEnvAdmin:{&SV} = ttIniFile.cVarValue.
                WHEN "dbBackup" THEN ASSIGN fiDbBackup:{&SV} = ttIniFile.cVarValue.
                WHEN "pgmBackup" THEN ASSIGN fiPgmBackup:{&SV} = ttIniFile.cVarValue.
                WHEN "resBackup" THEN ASSIGN fiResBackup:{&SV} = ttIniFile.cVarValue.
                WHEN "dbAuditDir" THEN ASSIGN fiDbAuditDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbDataDir" THEN ASSIGN fiDbDataDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbProdDir" THEN ASSIGN fiDbProdDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbShipDir" THEN ASSIGN fiDbShipDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbStructDir" THEN ASSIGN fiDbStructDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbTestDir" THEN ASSIGN fiDbTestDir:{&SV} = ttIniFile.cVarValue.
                WHEN "docMiscDocuments" THEN.
                WHEN "docReleaseNotes" THEN.
                WHEN "docUserManual" THEN.
                WHEN "envProdDir" THEN ASSIGN fiEnvProdDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envTestDir" THEN.
                WHEN "envAddonDir" THEN ASSIGN fiEnvAddonDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envCustFiles" THEN ASSIGN fiEnvCustFiles:{&SV} = ttIniFile.cVarValue.
                WHEN "envCustomerDir" THEN ASSIGN fiEnvCustomerDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envOverrideDir" THEN ASSIGN fiEnvOverrideDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envPoDir" THEN ASSIGN fiEnvPoDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envProgramsDir" THEN ASSIGN fiEnvProgramsDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envResourceDir" THEN ASSIGN fiEnvResourceDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envScheduleDir" THEN ASSIGN fiEnvScheduleDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envTemplateDir" THEN ASSIGN fiEnvTemplateDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envUserMenuDir" THEN ASSIGN fiEnvUserMenuDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envUsersDir" THEN ASSIGN fiEnvUsersDir:{&SV} = ttIniFile.cVarValue.
                WHEN "instAOA" THEN.
                WHEN "instBackup" THEN.
                WHEN "instDBMS" THEN.
                WHEN "instEsko" THEN.
                WHEN "instFileUtils" THEN.
                WHEN "instLocalPrint" THEN.
                WHEN "instRemAccess" THEN.
                WHEN "updAdminDir" THEN ASSIGN fiUpdAdminDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updCompressDir" THEN ASSIGN fiUpdCompressDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updDataDir" THEN ASSIGN fiUpdDataDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updDataUpdateDir" THEN ASSIGN fiUpdDataUpdateDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updDeskDir" THEN ASSIGN fiUpdDeskDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updMenuDir" THEN ASSIGN fiUpdMenuDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updProgramDir" THEN ASSIGN fiUpdProgramDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updRelNotesDir" THEN ASSIGN fiUpdRelNotesDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updSqlDir" THEN ASSIGN fiUpdSqlDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updStructureDir" THEN ASSIGN fiUpdStructureDir:{&SV} = ttIniFile.cVarValue.
                WHEN "prodDbName" THEN.
                WHEN "prodDbPort" THEN.
                WHEN "prodDbStFile" THEN.
                WHEN "shipDbName" THEN.
                WHEN "shipDbPort" THEN.
                WHEN "shipDbStFile" THEN.
                WHEN "testDbName" THEN.
                WHEN "testDbPort" THEN.
                WHEN "testDbStFile" THEN.
                WHEN "adminPort" THEN ASSIGN.
                WHEN "dfFileName" THEN ASSIGN fiDfFileName:{&SV} = ttIniFile.cVarValue.
                WHEN "deltaFileName" THEN ASSIGN fiDeltaFileName:{&SV} = ttIniFile.cVarValue.

                WHEN "envList" THEN DO iCtr = 1 TO NUM-ENTRIES(ttIniFile.cVarValue):
                    slEnvironments:ADD-LAST(ENTRY(iCtr,ttIniFile.cVarValue) + "-" + ENTRY(iCtr,cEnvVerList)).
                END.
                /*
                WHEN "dbList" THEN DO iCtr = 1 TO NUM-ENTRIES(ttIniFile.cVarValue):
                    slDatabases:ADD-LAST(ENTRY(iCtr,cDbDirList) + "-" + ENTRY(iCtr,cDbList) + "-" + 
                                         ENTRY(iCtr,cDbPortList) + "-" + ENTRY(iCtr,cDbVerList)).
                END. 
                */

                WHEN "modeList" THEN.
                WHEN "pgmList" THEN.
                WHEN "dbDirList" THEN.
                WHEN "dbPortList" THEN.
                WHEN "audDirList" THEN.
                WHEN "audDbList" THEN.
                WHEN "audPortList" THEN.

                
            END CASE.
        END.
    END.
    
    slDatabases:ADD-LAST(ipcDir + "-" + ipcName + "-" + ipcPort + "-" + ipcVer).
    APPLY 'value-changed' TO slDatabases.

    ASSIGN
        slDatabases:{&SV} = ENTRY(1,slDatabases:LIST-ITEMS)
        slEnvironments:{&SV} = ENTRY(1,slEnvironments:LIST-ITEMS)
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

    IF INDEX(ipcStatus,"duplicate") EQ 0 THEN DO:
        ASSIGN
            cLogFile = cUpdatesDir + "\" + "Patch" + fiNewVer:{&SV} + "\installLog.txt"
            iMsgCtr = iMsgCtr + 1
            cMsgStr[iMsgCtr] = ipcStatus + "...".
        
        OUTPUT STREAM logStream TO VALUE(cLogFile) APPEND.
        PUT STREAM logStream
            STRING(TODAY,"99/99/99") AT 1
            STRING(TIME,"HH:MM:SS") AT 12
            cMsgStr[iMsgCtr] FORMAT "x(60)" AT 25
            SKIP.
        OUTPUT STREAM logStream CLOSE.
    END.
    
    PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipTestLevel1 C-Win 
PROCEDURE ipTestLevel1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER lStructOK AS LOG NO-UNDO.

&SCOPED-DEFINE cField1 fiAdminDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField1}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiBackupDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField1}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiDbDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField1}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiDeskDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField1}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiDocDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField1}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiEnvDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField1}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiInstallDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField1}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiUpdatesDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField1}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipTestLevel2 C-Win 
PROCEDURE ipTestLevel2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER lStructOK AS LOG NO-UNDO.

&SCOPED-DEFINE cField1 fiAdminDir
&SCOPED-DEFINE cField2 fiDbAdmin
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField2}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiAdminDir
&SCOPED-DEFINE cField2 fiEnvAdmin
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField2}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.

&SCOPED-DEFINE cField1 fiBackupDir
&SCOPED-DEFINE cField2 fiDbBackup
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField2}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiBackupDir
&SCOPED-DEFINE cField2 fiPgmBackup
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField2}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiBackupDir
&SCOPED-DEFINE cField2 fiResBackup
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField2}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.

&SCOPED-DEFINE cField1 fiDbDir
&SCOPED-DEFINE cField2 fiDbDataDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV}
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField2}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiDbDir
&SCOPED-DEFINE cField2 fiDbProdDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField2}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiDbDir
&SCOPED-DEFINE cField2 fiDbShipDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField2}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiDbDir
&SCOPED-DEFINE cField2 fiDbStructDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField2}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiDbDir
&SCOPED-DEFINE cField2 fiDbTestDir
    /*
    IF tbAutoBuildTest:CHECKED IN FRAME {&FRAME-NAME} THEN ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField2}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
    */
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvProdDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField2}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvTestDir
    /*
    IF tbAutoBuildTest:CHECKED IN FRAME {&FRAME-NAME} THEN ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField2}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
    */
&SCOPED-DEFINE cField1 fiUpdatesDir
&SCOPED-DEFINE cField2 fiPatchDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField2}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipTestLevel3 C-Win 
PROCEDURE ipTestLevel3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER lStructOK AS LOG NO-UNDO.

&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvProdDir
&SCOPED-DEFINE cField3 fiEnvAddonDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvProdDir
&SCOPED-DEFINE cField3 fiEnvCustFiles
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvProdDir
&SCOPED-DEFINE cField3 fiEnvCustomerDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvProdDir
&SCOPED-DEFINE cField3 fiEnvOverrideDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvProdDir
&SCOPED-DEFINE cField3 fiEnvPoDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvProdDir
&SCOPED-DEFINE cField3 fiEnvProgramsDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvProdDir
&SCOPED-DEFINE cField3 fiEnvResourceDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvProdDir
&SCOPED-DEFINE cField3 fiEnvScheduleDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvProdDir
&SCOPED-DEFINE cField3 fiEnvTemplateDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvProdDir
&SCOPED-DEFINE cField3 fiEnvUsermenuDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvProdDir
&SCOPED-DEFINE cField3 fiEnvUsersDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
/*
    IF tbAutoBuildTest:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
    
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvTestDir
&SCOPED-DEFINE cField3 fiEnvAddonDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvTestDir
&SCOPED-DEFINE cField3 fiEnvCustFiles
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvTestDir
&SCOPED-DEFINE cField3 fiEnvCustomerDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvTestDir
&SCOPED-DEFINE cField3 fiEnvOverrideDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvTestDir
&SCOPED-DEFINE cField3 fiEnvPoDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvTestDir
&SCOPED-DEFINE cField3 fiEnvProgramsDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvTestDir
&SCOPED-DEFINE cField3 fiEnvResourceDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvTestDir
&SCOPED-DEFINE cField3 fiEnvScheduleDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvTestDir
&SCOPED-DEFINE cField3 fiEnvTemplateDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvTestDir
&SCOPED-DEFINE cField3 fiEnvUsermenuDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList.
&SCOPED-DEFINE cField1 fiEnvDir
&SCOPED-DEFINE cField2 fiEnvTestDir
&SCOPED-DEFINE cField3 fiEnvUsersDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList.
    END.
*/

&SCOPED-DEFINE cField1 fiUpdatesDir
&SCOPED-DEFINE cField2 fiPatchDir
&SCOPED-DEFINE cField3 fiUpdAdminDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiUpdatesDir
&SCOPED-DEFINE cField2 fiPatchDir
&SCOPED-DEFINE cField3 fiUpdCompressDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiUpdatesDir
&SCOPED-DEFINE cField2 fiPatchDir
&SCOPED-DEFINE cField3 fiUpdDataDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiUpdatesDir
&SCOPED-DEFINE cField2 fiPatchDir
&SCOPED-DEFINE cField3 fiUpdDataUpdateDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiUpdatesDir
&SCOPED-DEFINE cField2 fiPatchDir
&SCOPED-DEFINE cField3 fiUpdDeskDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiUpdatesDir
&SCOPED-DEFINE cField2 fiPatchDir
&SCOPED-DEFINE cField3 fiUpdMenuDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiUpdatesDir
&SCOPED-DEFINE cField2 fiPatchDir
&SCOPED-DEFINE cField3 fiUpdProgramDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiUpdatesDir
&SCOPED-DEFINE cField2 fiPatchDir
&SCOPED-DEFINE cField3 fiUpdRelNotesDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiUpdatesDir
&SCOPED-DEFINE cField2 fiPatchDir
&SCOPED-DEFINE cField3 fiUpdSqlDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.
&SCOPED-DEFINE cField1 fiUpdatesDir
&SCOPED-DEFINE cField2 fiPatchDir
&SCOPED-DEFINE cField3 fiUpdStructureDir
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\" + {&cField3}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField3}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipTestStructure C-Win 
PROCEDURE ipTestStructure :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER lStructOK AS LOG NO-UNDO.
    
    RUN ipStatus ("Testing Directory Structure").

    ASSIGN
        lStructOK = YES
        lAllOK = YES.
    
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?.
    IF NOT lStructOK THEN DO:
        MESSAGE
            "Drive " + cTestDir + " is not correctly mapped."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    RUN ipTestLevel1 (OUTPUT lStructOK).
    RUN ipTestLevel2 (OUTPUT lStructOK).
    RUN ipTestLevel3 (OUTPUT lStructOK).

    ASSIGN
        cBadDirList = TRIM(cBadDirList,",")
        lStructOK = lAllOK.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipTurnOffUserColors C-Win 
PROCEDURE ipTurnOffUserColors :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus(" Turn off user colors/fonts").
    
    DISABLE TRIGGERS FOR LOAD OF users.
    
    FOR EACH users:
        ASSIGN
            users.use_colors = FALSE
            users.use_fonts = FALSE.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpdateMaster C-Win 
PROCEDURE ipUpdateMaster :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Updating Master Records").

    RUN ipBackupDataFiles IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\prgrms.d") <> ? THEN
        RUN ipLoadPrograms IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\prgmxref.d") <> ? THEN
        RUN ipLoadProgramXref IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\lookups.d") <> ? THEN
        RUN ipLoadLookups IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\emailcod.d") <> ? THEN
        RUN ipLoadEmailCodes IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\reftable.d") <> ? THEN
        RUN ipLoadUtilCodes IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\notes.d") <> ? THEN
        RUN ipLoadUtilNotes IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\module.d") <> ? THEN
        RUN ipLoadModules IN THIS-PROCEDURE.

    IF NOT CAN-FIND(FIRST audit.audittbl) 
    AND SEARCH(cUpdDataDir + "\audittbl.d") <> ? THEN
        RUN ipLoadAuditRecs IN THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpdateNK1s C-Win 
PROCEDURE ipUpdateNK1s :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR lIsDir AS LOG NO-UNDO.
    DEF VAR cOrigValue AS CHAR NO-UNDO.
    DEF VAR cTgtDir AS CHAR NO-UNDO.
        
    RUN ipStatus ("Updating NK1 Records").
    
    DISABLE TRIGGERS FOR LOAD OF sys-ctrl.
    DISABLE TRIGGERS FOR LOAD OF sys-ctrl-shipto.
    
    /* Verify system help WSDL NK1 */
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.name EQ "AsiHelpServices"
        NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN
        sys-ctrl.char-fld = "-WSDL 'http:\\34.203.15.64/asihelpServices/helpmaintenance.asmx?WSDL'".
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.name EQ "AsiHelpService"
        NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN
        sys-ctrl.char-fld = "-WSDL 'http:\\34.203.15.64/asihelpServices/helpmaintenance.asmx?WSDL'".
    
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.name EQ "UpdateService"
        NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN
        sys-ctrl.char-fld = "-WSDL 'http:\\34.203.15.64/updatehelpServices/helpupdate.asmx?WSDL'".
    
    /* Reports - set LV = true */
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.name EQ "Reports"
        NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN
        sys-ctrl.log-fld = TRUE.

    /* RelType - set Default to "B" */
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.name EQ "RelType"
        NO-ERROR.
    IF AVAIL sys-ctrl 
    AND sys-ctrl.char-fld EQ "" THEN ASSIGN
        sys-ctrl.char-fld = "Bill and Ship".

        
    /* - future: update CustFile locations
    FOR EACH sys-ctrl WHERE
        INDEX(sys-ctrl.descrip,".") NE 0 OR
        INDEX(sys-ctrl.descrip,":") NE 0 OR
        INDEX(sys-ctrl.descrip,"\") NE 0 OR
        INDEX(sys-ctrl.descrip,"/") NE 0:
        ASSIGN
            FILE-INFO:FILE-NAME = sys-ctrl.descrip
            lIsDir = FILE-INFO:FULL-PATHNAME NE ?.
        IF lIsDir THEN DO:
            IF sys-ctrl.descrip BEGINS "C:\" THEN NEXT.
            IF INDEX(sys-ctrl.descrip,"CustFiles") NE 0 THEN NEXT.
            ASSIGN
                cOrigValue = sys-ctrl.descrip.
            DISP 
                sys-ctrl.name LABEL "NK1 Name" SKIP
                sys-ctrl.descrip LABEL "VALUE"
                WITH FRAME sysctl VIEW-AS DIALOG-BOX THREE-D SIDE-LABELS 1 DOWN.
            UPDATE sys-ctrl.descrip LABEL "Value"
                WITH FRAME sysctl.
            IF sys-ctrl.descrip NE cOrigValue THEN DO:
                ASSIGN
                    cTgtDir = REPLACE(sys-ctrl.descrip,".",cEnvProdDir).
                RUN ipCopyDirs IN THIS-PROCEDURE (cOrigValue, cTgtDir).
            END.
        END.
    END.
    */
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
    DEF VAR cThisElement AS CHAR NO-UNDO.
    DEF VAR cMinVer AS CHAR NO-UNDO INITIAL "99.99.99".
    
    FIND ttIniFile WHERE ttIniFile.cVarName = "envList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = "".
    FIND ttIniFile WHERE ttIniFile.cVarName = "envVerList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = "".
    DO iCtr = 1 TO NUM-ENTRIES(slEnvironments:LIST-ITEMS IN FRAME {&FRAME-NAME}):
        ASSIGN
            cThisElement = ENTRY(iCtr,slEnvironments:LIST-ITEMS).
        IF intVer(ENTRY(2,cThisElement,"-")) LE intVer(cMinVer) THEN ASSIGN
            cMinVer = ENTRY(2,cThisElement,"-").
        FIND ttIniFile WHERE ttIniFile.cVarName = "envList" NO-ERROR.
        ASSIGN ttIniFile.cVarValue = ttIniFile.cVarValue + ENTRY(1,cThisElement,"-") + ",".
        FIND ttIniFile WHERE ttIniFile.cVarName = "envVerList" NO-ERROR.
        ASSIGN ttIniFile.cVarValue = ttIniFile.cVarValue + ENTRY(2,cThisElement,"-") + ",".
    END.
    FIND ttIniFile WHERE ttIniFile.cVarName = "envList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = TRIM(ttIniFile.cVarValue,",").
    FIND ttIniFile WHERE ttIniFile.cVarName = "envVerList" NO-ERROR.
    ASSIGN ttIniFile.cVarValue = TRIM(ttIniFile.cVarValue,",").

    FOR EACH ttIniFile:
        CASE ttIniFile.cVarName:
            WHEN "siteName" THEN ASSIGN ttIniFile.cVarValue = fiSiteName:{&SV}.
            WHEN "hostname" THEN ASSIGN ttIniFile.cVarValue = fiHostName:{&SV}.
            WHEN "drive" THEN ASSIGN ttIniFile.cVarValue = fiDrive:{&SV}.
            WHEN "topDir" THEN ASSIGN ttIniFile.cVarValue = fiTopDir:{&SV}.
            WHEN "mapDir" THEN ASSIGN ttIniFile.cVarValue = fiMapDir:{&SV}.
            WHEN "currVer" THEN ASSIGN ttIniFile.cVarValue = cMinVer.
            WHEN "verDate" THEN ASSIGN ttIniFile.cVarValue = fiVerDate:{&SV}.
            WHEN "DLCDir" THEN ASSIGN ttIniFile.cVarValue = fiDLCDir:{&SV}.
            WHEN "adminDir" THEN ASSIGN ttIniFile.cVarValue = fiAdminDir:{&SV}.
            WHEN "backupDir" THEN ASSIGN ttIniFile.cVarValue = fiBackupDir:{&SV}.
            WHEN "dbDir" THEN ASSIGN ttIniFile.cVarValue = fiDbDir:{&SV}.
            WHEN "deskDir" THEN ASSIGN ttIniFile.cVarValue = fiDeskDir:{&SV}.
            WHEN "docDir" THEN ASSIGN ttIniFile.cVarValue = fiDocDir:{&SV}.
            WHEN "envDir" THEN ASSIGN ttIniFile.cVarValue = fiEnvDir:{&SV}.
            WHEN "installDir" THEN ASSIGN ttIniFile.cVarValue = fiInstallDir:{&SV}.
            WHEN "updateDir" THEN ASSIGN ttIniFile.cVarValue = fiUpdatesDir:{&SV}.
            WHEN "dbAdmin" THEN ASSIGN ttIniFile.cVarValue = fiDbAdmin:{&SV}.
            WHEN "envAdmin" THEN ASSIGN ttIniFile.cVarValue = fiEnvAdmin:{&SV}.
            WHEN "dbBackup" THEN ASSIGN ttIniFile.cVarValue = fiDbBackup:{&SV}.
            WHEN "pgmBackup" THEN ASSIGN ttIniFile.cVarValue = fiPgmBackup:{&SV}.
            WHEN "resBackup" THEN ASSIGN ttIniFile.cVarValue = fiResBackup:{&SV}.
            WHEN "dbDataDir" THEN ASSIGN ttIniFile.cVarValue = fiDbDataDir:{&SV}.
            WHEN "dbProdDir" THEN ASSIGN ttIniFile.cVarValue = fiDbProdDir:{&SV}.
            WHEN "dbShipDir" THEN ASSIGN ttIniFile.cVarValue = fiDbShipDir:{&SV}.
            WHEN "dbStructDir" THEN ASSIGN ttIniFile.cVarValue = fiDbStructDir:{&SV}.
            WHEN "dbTestDir" THEN ASSIGN ttIniFile.cVarValue = fiDbTestDir:{&SV}.
            WHEN "envProdDir" THEN ASSIGN ttIniFile.cVarValue = fiEnvProdDir:{&SV}.
            WHEN "envAddonDir" THEN ASSIGN ttIniFile.cVarValue = fiEnvAddonDir:{&SV}.
            WHEN "envCustFiles" THEN ASSIGN ttIniFile.cVarValue = fiEnvCustFiles:{&SV}.
            WHEN "envCustomerDir" THEN ASSIGN ttIniFile.cVarValue = fiEnvCustomerdir:{&SV}.
            WHEN "envOverride" THEN ASSIGN ttIniFile.cVarValue = fiEnvOverrideDir:{&SV}.
            WHEN "envPoDir" THEN ASSIGN ttIniFile.cVarValue = fiEnvPoDir:{&SV}.
            WHEN "envProgramsDir" THEN ASSIGN ttIniFile.cVarValue = fiEnvProgramsDir:{&SV}.
            WHEN "envResourceDir" THEN ASSIGN ttIniFile.cVarValue = fiEnvResourceDir:{&SV}.
            WHEN "envScheduleDir" THEN ASSIGN ttIniFile.cVarValue = fiEnvScheduleDir:{&SV}.
            WHEN "envTemplateDir" THEN ASSIGN ttIniFile.cVarValue = fiEnvTemplateDir:{&SV}.
            WHEN "envUserMenuDir" THEN ASSIGN ttIniFile.cVarValue = fiEnvUserMenuDir:{&SV}.
            WHEN "envUsersDir" THEN ASSIGN ttIniFile.cVarValue = fiEnvUsersDir:{&SV}.
            WHEN "updAdminDir" THEN ASSIGN ttIniFile.cVarValue = fiUpdAdminDir:{&SV}.
            WHEN "updCompressDir" THEN ASSIGN ttIniFile.cVarValue = fiUpdCompressDir:{&SV}.
            WHEN "updDataDir" THEN ASSIGN ttIniFile.cVarValue = fiUpdDataDir:{&SV}.
            WHEN "updDataUpdateDir" THEN ASSIGN ttIniFile.cVarValue = fiUpdDataUpdateDir:{&SV}.
            WHEN "updDeskDir" THEN ASSIGN ttIniFile.cVarValue = fiUpdDeskDir:{&SV}.
            WHEN "updMenuDir" THEN ASSIGN ttIniFile.cVarValue = fiUpdMenuDir:{&SV}.
            WHEN "updProgramDir" THEN ASSIGN ttIniFile.cVarValue = fiUpdProgramDir:{&SV}.
            WHEN "updRelNotesDir" THEN ASSIGN ttIniFile.cVarValue = fiUpdRelNotesDir:{&SV}.
            WHEN "updSqlDir" THEN ASSIGN ttIniFile.cVarValue = fiUpdSqlDir:{&SV}.
            WHEN "updStructureDir" THEN ASSIGN ttIniFile.cVarValue = fiUpdStructureDir:{&SV}.
            WHEN "dfFileName" THEN ASSIGN ttIniFile.cVarValue = fiDfFileName:{&SV}.
            WHEN "deltaFileName" THEN ASSIGN ttIniFile.cVarValue = fiDeltaFileName:{&SV}.
            WHEN "lockoutTries" THEN ASSIGN ttIniFile.cVarValue = fiLockoutTries:{&SV}.
        END CASE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipUpdateUserControl C-Win 
PROCEDURE ipUpdateUserControl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Create/Update usercontrol record").

    DISABLE TRIGGERS FOR LOAD OF usercontrol.
    
    ASSIGN
        iUserCount = INTEGER(fiLicensedUsers:{&SV}).
        
    /* Create/update usercontrol record */
    FIND FIRST usercontrol EXCLUSIVE NO-ERROR.
    IF NOT AVAIL usercontrol THEN
        CREATE usercontrol.
    IF usercontrol.numLicensedUsers = 0 
    OR usercontrol.numLicensedUsers = ? 
    OR usercontrol.rec_key = ? THEN DO:
        ASSIGN
            timeString = STRING(time,"HH:MM:SS")
            timeString = REPLACE(timeString,":","") + "00"
            iExtra = MAXIMUM(1,INTEGER(iUserCount / 10))
            usercontrol.numLicensedUsers = iUserCount
            usercontrol.maxAllowedUsers = iUserCount + iExtra
            usercontrol.maxSessionsPerUser = 2
            usercontrol.numUsersOverLimit = iExtra
            usercontrol.rec_key = STRING(year(today),"9999") +
                                 STRING(month(today),"99") +
                                 STRING(day(today),"99") +
                                 timeString.
        FIND FIRST rec_key WHERE
            rec_key.table_name = "usercontrol" AND
            rec_key.rec_key = usercontrol.rec_key
            NO-LOCK NO-ERROR.
        IF NOT AVAIL rec_key THEN DO:
            CREATE rec_key.
            ASSIGN
                rec_key.table_name = "usercontrol"
                rec_key.rec_key = usercontrol.rec_key.
        END.
    END.
    ELSE DO:
        ASSIGN
            usercontrol.numLicensedUsers = iUserCount.
    END.
    ASSIGN
        tbComp-1:CHECKED = TRUE.
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

    DO iCtr = 1 TO NUM-ENTRIES(slDatabases:{&SV}):
        IF DECIMAL(ENTRY(4,ENTRY(iCtr,slDatabases:{&SV}),"-")) * 10 LT iDbTgtVer THEN DO:
            MESSAGE 
                "Database " + ENTRY(2,ENTRY(iCtr,slDatabases:{&SV}),"-") + " is not at the current" SKIP
                "version level.  Do you want to update it now?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lUpdNow AS LOG.
            IF lUpdNow THEN DO:
                RUN ipStatus ("Upgrading Selected Databases").
                ASSIGN
                    /* This results in a value similar to 'asi166_167.df' */
                    cDeltaDf = "asi" + 
                               STRING(DECIMAL(ENTRY(4,ENTRY(iCtr,slDatabases:{&SV}),"-")) * 10) +
                               "_" + STRING(iDbTgtVer) + ".df"
                    /* This fully qualifies the path name to the delta */
                    cDeltaDf = fiDrive:{&SV} + "\" + fiTopDir:{&SV} + "\" +
                               fiUpdatesDir:{&SV} + "\" + fiPatchDir:{&SV} + "\" +
                               fiUpdStructureDir:{&SV} + "\DFFiles\" + cDeltaDf
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
                OS-COMMAND SILENT VALUE(cStopString).
                /* May have to wait for DB to shut down */
                DO WHILE SEARCH(cLockFile) NE ?:
                    PAUSE 3.
                END.
                /* Connect to the database single user */
                CONNECT VALUE(cStatement).
                CREATE ALIAS DICTDB FOR DATABASE VALUE("updDB" + STRING(iCtr)).
                /* Load the delta */
                RUN prodict/load_df.p (cDeltaDf). 
                
                /* Disconnect it */
                DISCONNECT VALUE("updDB" + STRING(iCtr)).
                /* Re-Serve it */
                OS-COMMAND SILENT VALUE(cStartString).
            END.
        END.
        ELSE MESSAGE
            "Database " + ENTRY(2,ENTRY(iCtr,slDatabases:{&SV}),"-") + " is already at the current" SKIP
            "version level.  It should not be updated again"
            VIEW-AS ALERT-BOX INFO.
            
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipValidateDB C-Win 
PROCEDURE ipValidateDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER oplValidDB AS LOG.
    
    ASSIGN
        oplValidDB = TRUE.
        
    IF NOT CAN-FIND(FIRST _file WHERE 
                    _file._file-name EQ "costheader") THEN DO:
        ASSIGN
            oplValidDb = FALSE.
        MESSAGE
            "The connected database has not been upgraded to version 16.7." SKIP
            "Please use the database update program provided in folder" SKIP
            cMapDrive + "\Desktop\DBUpdate to correct this issue, then run" SKIP
            "this program again."
            VIEW-AS ALERT-BOX.
        RETURN.
    END.
                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipVerifyNK1Changes C-Win 
PROCEDURE ipVerifyNK1Changes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Verifying NK1 Changes...").

    DEF VAR cLogFile AS CHAR NO-UNDO.
    DEF VAR cString AS CHAR NO-UNDO. 
    DEF VAR cChangeList AS CHAR NO-UNDO.
                   
    STATUS INPUT "Validating NK1 records...".

    ASSIGN
        cLogFile = cUpdatesDir + "\" + "Patch" + fiNewVer:{&SV} + "\installLog.txt"
        iMsgCtr = iMsgCtr + 1
        cMsgStr[iMsgCtr] = "Validating NK1 records...".
        
    DO:
        OUTPUT STREAM logStream TO VALUE(cLogFile) APPEND.
        PUT STREAM logStream
            STRING(TODAY,"99/99/99") AT 1
            STRING(TIME,"HH:MM:SS") AT 12
            cMsgStr[iMsgCtr] FORMAT "x(60)" AT 25
            SKIP.
        FOR EACH sys-ctrl NO-LOCK:
            FIND ttSysCtrl NO-LOCK WHERE
                ttSysCtrl.company = sys-ctrl.company AND
                ttSysCtrl.name = sys-ctrl.name
                NO-ERROR.
            IF NOT AVAIL ttSysCtrl THEN DO:
                ASSIGN
                    iMsgCtr = iMsgCtr + 1
                    cString = "ADDED NK1 - Company: " + sys-ctrl.company + " Name: " + sys-ctrl.name.
                PUT STREAM logStream
                    cString FORMAT "x(60)" AT 25.
            END.
            /* ELSE IF buffer-compare */
        END.    
        OUTPUT STREAM logStream CLOSE.
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION intVer C-Win 
FUNCTION intVer RETURNS INTEGER
  ( INPUT cVerString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR cStrVal AS CHAR EXTENT 3 NO-UNDO.
    DEF VAR iIntVal AS INT EXTENT 3 NO-UNDO.
    DEF VAR iIntVer AS INT NO-UNDO.
    ASSIGN
        cStrVal[1] = ENTRY(1,cVerString,".")
        cStrVal[2] = ENTRY(2,cVerString,".")
        cStrVal[3] = ENTRY(3,cVerString,".")
        iIntVal[1] = INT(cStrVal[1])
        iIntVal[2] = INT(cStrVal[2])
        iIntVal[3] = INT(cStrVal[3])
        iIntVer = (iIntVal[1] * 10000) + (iIntVal[2] * 100) + iIntVal[3]
        NO-ERROR.
    
  RETURN iIntVer.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

