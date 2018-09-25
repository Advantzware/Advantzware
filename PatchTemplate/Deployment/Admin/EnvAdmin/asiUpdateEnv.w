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
DEF INPUT PARAMETER ipcEnv AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcFromVer AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcToVer AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipiLevel AS INT NO-UNDO.
DEF INPUT PARAMETER iplNeedBackup AS LOG NO-UNDO.
DEF OUTPUT PARAMETER oplSuccess AS LOG NO-UNDO.

/*
/* FOR TEST PURPOSES ONLY */
DEF VAR ipcName AS CHAR NO-UNDO.
DEF VAR ipcPort AS CHAR NO-UNDO.
DEF VAR ipcDir AS CHAR NO-UNDO.
DEF VAR ipcVer AS CHAR NO-UNDO.
DEF VAR ipiLevel AS INT NO-UNDO.
DEF VAR iplNeedBackup AS LOG NO-UNDO.
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
DEF TEMP-TABLE ttTranslation LIKE translation.
DEF TEMP-TABLE ttUserLanguage LIKE userlanguage.
DEF TEMP-TABLE ttXuserMenu LIKE xuserMenu.
DEF TEMP-TABLE ttUtilities LIKE utilities.

DEFINE TEMP-TABLE ttUserMenu NO-UNDO
    FIELD prgmname AS CHARACTER
    INDEX prgmname IS PRIMARY
    prgmname
    .            
        

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
DEF VAR iListEntry AS INT NO-UNDO.
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
DEF VAR cEnvVerList AS CHAR INITIAL "16.7.16" NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS RECT-5 tbBackupDBs tbUserControl ~
tbUserCleanup tbDelBadData tbUpdateMaster tbRunDataFix tbDelDupeNotes ~
fiLicensedUsers tbUpdateNK1s bProcess tbReftableConv tbLoadMenus eStatus ~
tbRelNotes tbInstallFiles tbUpdateIni 
&Scoped-Define DISPLAYED-OBJECTS fiSiteName fiOptions fiHostname ~
tbBackupDBs tbUserControl fiEnvironment tbUserCleanup fiAsiDbName ~
fiAudDbName tbDelBadData fiAsiPortNo fiAudPortNo tbUpdateMaster fiFromVer ~
tbRunDataFix fiToVer tbDelDupeNotes fiLicensedUsers tbUpdateNK1s ~
tbUpdateFileLocs tbReftableConv tbLoadMenus eStatus tbRelNotes ~
tbBackupFiles tbInstallFiles tbUpdateIni fiLogFile 

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
     LABEL "Start Update" 
     SIZE 46 BY 1.43
     FONT 6.

DEFINE VARIABLE eStatus AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 75 BY 9.52 NO-UNDO.

DEFINE VARIABLE fiAsiDbName AS CHARACTER FORMAT "X(256)":U 
     LABEL "and databases (ASI)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiAsiPortNo AS CHARACTER FORMAT "X(256)":U 
     LABEL "running on ports" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiAudDbName AS CHARACTER FORMAT "X(256)":U 
     LABEL "(Audit)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiAudPortNo AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiEnvironment AS CHARACTER FORMAT "X(256)":U 
     LABEL "Upgrading environment" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromVer AS CHARACTER FORMAT "X(256)":U 
     LABEL "from ASI version" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiHostname AS CHARACTER FORMAT "X(256)":U INITIAL "DEMO" 
     LABEL "Server Name" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fiLicensedUsers AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Licensed User count" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE fiLogFile AS CHARACTER FORMAT "X(256)":U INITIAL "Log of actions will be stored in N:~\Admin~\EnvAdmin~\UpdateLog.txt" 
      VIEW-AS TEXT 
     SIZE 65 BY .62 NO-UNDO.

DEFINE VARIABLE fiOptions AS CHARACTER FORMAT "X(256)":U INITIAL "Options:" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiSiteName AS CHARACTER FORMAT "X(256)":U INITIAL "DEMO" 
     LABEL "Site Name" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fiToVer AS CHARACTER FORMAT "X(256)":U INITIAL "16.7.16" 
     LABEL "to version" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 16.19.

DEFINE VARIABLE tbBackupDBs AS LOGICAL INITIAL no 
     LABEL "Backup Databases" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tbBackupFiles AS LOGICAL INITIAL no 
     LABEL "Backup System Files" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

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
     fiSiteName AT ROW 1.24 COL 19 COLON-ALIGNED WIDGET-ID 68
     fiOptions AT ROW 1.24 COL 79 COLON-ALIGNED NO-LABEL
     fiHostname AT ROW 2.19 COL 19 COLON-ALIGNED WIDGET-ID 36
     tbBackupDBs AT ROW 2.43 COL 82 WIDGET-ID 384
     tbUserControl AT ROW 3.38 COL 82 WIDGET-ID 370
     fiEnvironment AT ROW 3.86 COL 29 COLON-ALIGNED
     tbUserCleanup AT ROW 4.33 COL 82 WIDGET-ID 368
     fiAsiDbName AT ROW 4.81 COL 29 COLON-ALIGNED
     fiAudDbName AT ROW 4.81 COL 56 COLON-ALIGNED
     tbDelBadData AT ROW 5.29 COL 82 WIDGET-ID 374
     fiAsiPortNo AT ROW 5.76 COL 29 COLON-ALIGNED
     fiAudPortNo AT ROW 5.76 COL 56 COLON-ALIGNED
     tbUpdateMaster AT ROW 6.24 COL 82 WIDGET-ID 376
     fiFromVer AT ROW 6.71 COL 29 COLON-ALIGNED
     tbRunDataFix AT ROW 7.19 COL 82 WIDGET-ID 400
     fiToVer AT ROW 7.67 COL 29 COLON-ALIGNED WIDGET-ID 46
     tbDelDupeNotes AT ROW 8.14 COL 82 WIDGET-ID 390
     fiLicensedUsers AT ROW 8.62 COL 29 COLON-ALIGNED WIDGET-ID 440
     tbUpdateNK1s AT ROW 9.1 COL 82 WIDGET-ID 396
     tbUpdateFileLocs AT ROW 10.05 COL 82 WIDGET-ID 398
     bProcess AT ROW 10.29 COL 15 WIDGET-ID 404
     tbReftableConv AT ROW 11 COL 82 WIDGET-ID 504
     tbLoadMenus AT ROW 11.95 COL 82 WIDGET-ID 378
     eStatus AT ROW 12.67 COL 2 NO-LABEL
     tbRelNotes AT ROW 12.91 COL 82 WIDGET-ID 382
     tbBackupFiles AT ROW 13.86 COL 82 WIDGET-ID 386
     tbInstallFiles AT ROW 14.81 COL 82 WIDGET-ID 388
     tbUpdateIni AT ROW 15.76 COL 82 WIDGET-ID 450
     fiLogFile AT ROW 22.43 COL 5 COLON-ALIGNED NO-LABEL
     "Status:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.95 COL 3 WIDGET-ID 54
     RECT-5 AT ROW 1.48 COL 79
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120 BY 22.67 WIDGET-ID 100.


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
         HEIGHT             = 22.67
         WIDTH              = 120
         MAX-HEIGHT         = 34.29
         MAX-WIDTH          = 166.2
         VIRTUAL-HEIGHT     = 34.29
         VIRTUAL-WIDTH      = 166.2
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
/* SETTINGS FOR FILL-IN fiAsiDbName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiAsiPortNo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiAudDbName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiAudPortNo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiEnvironment IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFromVer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiHostname IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiLogFile IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiOptions IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSiteName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiToVer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbBackupFiles IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbUpdateFileLocs IN FRAME DEFAULT-FRAME
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
    IF USERID(LDBNAME(1)) EQ "" THEN RETURN.
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* ASI Install/Update Processor */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
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
ON CHOOSE OF bProcess IN FRAME DEFAULT-FRAME /* Start Update */
DO:
    RUN ipProcessAll.
    IF CONNECTED(LDBNAME(2)) THEN
        DISCONNECT VALUE(LDBNAME(2)).
    IF CONNECTED(LDBNAME(1)) THEN
        DISCONNECT VALUE(LDBNAME(1)).
    APPLY "CLOSE":U TO THIS-PROCEDURE.
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

    ASSIGN 
        fiEnvironment:{&SV} = ipcEnv
        fiFromVer:{&SV}     = ipcFromVer
        fiToVer:{&SV}       = ipcToVer
        fiAsiDbName:{&SV}   = ipcName
        fiAsiPortNo:{&SV}   = ipcPort.

    RUN ipCreateTTiniFile.
    RUN ipFindIniFile.
    IF cIniLoc NE "" THEN 
        RUN ipReadIniFile.
    RUN ipExpandVarNames.
    RUN ipSetDispVars.
    
    ASSIGN 
        iListEntry          = LOOKUP(fiAsiDbName:{&SV},cDbList)
        fiAudDbName:{&SV}   = ENTRY(iListEntry,cAudDbList)
        fiAudPortNo:{&SV}   = ENTRY(iListEntry,cAudPortList).
    
    RUN ipValidateDB (OUTPUT lValidDB).
    IF NOT lValidDB THEN RETURN.

    IF ipiLevel GT 10 THEN ASSIGN
        fiLicensedUsers:SENSITIVE = TRUE
        tbUserControl:SENSITIVE = TRUE.

    FIND FIRST usercontrol NO-LOCK NO-ERROR.
    IF AVAIL usercontrol THEN ASSIGN
        fiLicensedUsers:{&SV} = STRING(usercontrol.numLicensedUsers)
        lNeedUsercontrol = FALSE.
    ELSE ASSIGN
        lNeedUsercontrol = TRUE.
        
    ASSIGN
        tbBackupDBs:CHECKED = iplNeedBackup
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
        tbRelNotes:CHECKED = TRUE
        tbBackupFiles:CHECKED = FALSE
        tbInstallFiles:CHECKED = TRUE
        tbUpdateINI:CHECKED = TRUE
        .
        
    IF ipiLevel LT 10 THEN DO:
        ASSIGN
            c-Win:WIDTH = 77
            bProcess:LABEL = "No User Action Required"
            lAutorun = TRUE.
        DISABLE ALL EXCEPT bProcess eStatus WITH FRAME {&FRAME-NAME}.
        APPLY 'choose' to bProcess.
    END.
    ELSE
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
  DISPLAY fiSiteName fiOptions fiHostname tbBackupDBs tbUserControl 
          fiEnvironment tbUserCleanup fiAsiDbName fiAudDbName tbDelBadData 
          fiAsiPortNo fiAudPortNo tbUpdateMaster fiFromVer tbRunDataFix fiToVer 
          tbDelDupeNotes fiLicensedUsers tbUpdateNK1s tbUpdateFileLocs 
          tbReftableConv tbLoadMenus eStatus tbRelNotes tbBackupFiles 
          tbInstallFiles tbUpdateIni fiLogFile 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-5 tbBackupDBs tbUserControl tbUserCleanup tbDelBadData 
         tbUpdateMaster tbRunDataFix tbDelDupeNotes fiLicensedUsers 
         tbUpdateNK1s bProcess tbReftableConv tbLoadMenus eStatus tbRelNotes 
         tbInstallFiles tbUpdateIni 
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
    RUN ipStatus ("  Adding Supplemental User Records").

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
        cCurrentDir = cDrive + "\" + 
                      cTopDir + "\" +
                      cEnvDir + "\" +
                      fiEnvironment:{&SV} + "\" +
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
    RUN ipStatus ("  Backing up data files").
    DISABLE TRIGGERS FOR DUMP OF sys-ctrl.
    DISABLE TRIGGERS FOR DUMP OF sys-ctrl-shipto.

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

&SCOPED-DEFINE cFile translation
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}" + ".bak") NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile userlanguage
    OUTPUT TO VALUE(cUpdDataDir + "\" + "{&cFile}" + ".bak") NO-ECHO.
    FOR EACH {&cFile}:
        EXPORT {&cFile}.
    END.
    OUTPUT CLOSE.

&SCOPED-DEFINE cFile xusermenu
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
    DEF VAR cCmdLine    AS CHAR NO-UNDO.
    DEF VAR cLocItem    AS CHAR NO-UNDO.
    DEF VAR cLocDir     AS CHAR NO-UNDO.
    DEF VAR cLocName    AS CHAR NO-UNDO.
    DEF VAR cLocPort    AS CHAR NO-UNDO.
    DEF VAR cBackupName AS CHAR NO-UNDO.
    DEF VAR cLockFile   AS CHAR NO-UNDO.  
    DEF VAR cPrefix     AS CHAR NO-UNDO.
    DEF VAR cThisDir    AS CHAR NO-UNDO.
    DEF VAR iCount AS INT NO-UNDO.
    DEF VAR cThisDB AS CHAR NO-UNDO.
    DEF VAR cThisPort AS CHAR NO-UNDO.
    
    ASSIGN 
        iCount = 1.
        
    DO iCount = 1 TO 2:
        IF iCount = 1 THEN ASSIGN 
            cThisDB = fiAsiDBName:{&SV}.
        ELSE ASSIGN 
            cThisDB = fiAudDBName:{&SV}.
      
        ASSIGN 
            cPrefix = SUBSTRING(cThisDB,1,3).
            
        IF cPrefix = "asi" THEN ASSIGN 
                iListEntry = LOOKUP(cThisDB,cDbList)
                cThisDir   = ENTRY(iListEntry,cDbDirList)
                cThisPort  = ENTRY(iListEntry,cDbPortList).
        ELSE ASSIGN 
                iListEntry = LOOKUP(cThisDB,cAudDbList)
                cThisDir   = ENTRY(iListEntry,cAudDirList)
                cThisPort  = ENTRY(iListEntry,cAudPortList).
        
        ASSIGN
            cLocDir     = cThisDir
            cLocName    = cThisDB
            cLocPort    = cThisDir
            cBackupName = cDbBackup + "\" + cLocName + "_" +
                       STRING(YEAR(TODAY)) +
                       STRING(MONTH(TODAY),"99") +
                       STRING(DAY(TODAY),"99") + "_" +
                       STRING(TIME) + ".bak" 
            cCmdLine    = cDLCDir + "\bin\probkup online " + 
                       cDBDrive + "\" + 
                       cTopDir + "\" + 
                       cDbDir + "\" +
                       cLocDir + "\" +
                       cLocName + " " + 
                       cBackupName
            cLockFile   = cDbDrive + "\" +
                       cTopDir + "\" + cDbDir + "\" + 
                       cThisDir + "\" +
                       cThisDB + ".lk".
        .
    
        IF SEARCH(cLockFile) EQ ? THEN 
        DO:
            MESSAGE 
                "The " + cThisDB + " database is not currently running." SKIP 
                "This means that it will not be possible to back up the data-" SKIP 
                "base, or to upgrade it with this program.  You should exit" SKIP 
                "this program now, and make sure that the databases are" SKIP 
                "running before you attempt to upgrade the system again."
                VIEW-AS ALERT-BOX ERROR.
            ASSIGN 
                lSuccess = FALSE.
            RETURN.
        END.
        
        RUN ipStatus ("  Backing Up database " + cThisDB).
        
        OS-COMMAND SILENT VALUE(cCmdLine).
        
        IF SEARCH(cBackupName) NE ? THEN 
        DO:
            ASSIGN
                lSuccess = TRUE.
            RUN ipStatus ("    Backup successful").
        END.
        ELSE 
        DO:
            ASSIGN
                lSuccess = FALSE.
            RUN ipStatus ("    Backup FAILED").
        END.
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
    RUN ipStatus ("  Validating ADMIN user entries").

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
    RUN ipStatus ("  Validating ASI User Entries").

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
    FIND FIRST module NO-LOCK WHERE 
        module.module = cfrom 
        NO-ERROR.
    IF NOT AVAIL module THEN RETURN.
  
    RUN ipStatus ("    Converting module " + cFrom + " to " + cTo).

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

    RUN ipStatus ("    Converting advantzware.usr file...").

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
    RUN ipStatus ("    Converting QtyPerSet records...").

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

    RUN ipStatus ("       Total Estimates: "  + STRING(iCount)).
    RUN ipStatus ("       Converted from .yld-qty: " + STRING(iCountProcessed)).
    RUN ipStatus ("       Initialized to 1: " + STRING(iCountInitialized)).
    RUN ipStatus ("       Total Sets: " + STRING(iCountFGSets)).
    RUN ipStatus ("       Sets Converted from .part-qty to .qtyPerSet: " + STRING(iCountFGSetsProcessed)).
    RUN ipStatus ("       Sets Initialized to 1: " + STRING(iCountFGSetsInitialized )).
    
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

    RUN ipStatus ("Starting Data Fixes - from version " + fiFromVer:{&SV}).

    ASSIGN 
        cThisEntry = fiFromVer:{&SV}.

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
    IF intVer(cThisEntry) LT 160800 THEN
        RUN ipDataFix160800.

    RUN ipStatus ("Completed Data Fixes").

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
    
    /* Ticket 32053 */
    FOR EACH reftable1 WHERE reftable1.reftable = "oe-rel.lot-no"
        NO-LOCK:
        DO TRANSACTION:    
            FIND FIRST oe-rel WHERE oe-rel.r-no = int(reftable1.company)
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-rel THEN    
            DO: 
                IF oe-rel.lot-no EQ "" THEN ASSIGN oe-rel.lot-no = reftable1.code.
                IF oe-rel.frt-pay EQ "" THEN ASSIGN oe-rel.frt-pay = reftable1.code2.
                IF oe-rel.fob-code EQ "" THEN ASSIGN oe-rel.fob-code = reftable1.dscr.
            END.   
            FIND CURRENT oe-rel NO-LOCK NO-ERROR.    
            RELEASE oe-rel. 
        END.  
    END.             
    
    RUN ipConvQtyPerSet.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDataFix160800 C-Win 
PROCEDURE ipDataFix160800 :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Data Fix 160800...").

    RUN ipRemoveUserAddon.
    RUN ipMoveUserMenusToDatabase.

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

    ASSIGN 
        cThisEntry = fiEnvironment:{&SV}
        cTgtEnv = cMapDir + "\" + cEnvDir + "\" + fiEnvironment:{&SV}.

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

    /* Skip the copy part, just MOVE the files  */
    RUN ipStatus ("  Moving expanded files from Patch to " + cThisEntry + " temp directory").
    OS-RENAME VALUE(cUpdProgramDir + "\Override") VALUE(cTgtEnv + "\OverrideN").
    OS-RENAME VALUE(cUpdProgramDir + "\Programs") VALUE(cTgtEnv + "\ProgramsN").
    OS-RENAME VALUE(cUpdProgramDir + "\Resources") VALUE(cTgtEnv + "\ResourcesN").

    /* Rename the old files to "O", then new files from "N", then delete "O" */
    RUN ipStatus ("  Renaming old " + cThisEntry + " directories").
    OS-RENAME VALUE(cTgtEnv + "\Override") VALUE(cTgtEnv + "\OverrideO").
    OS-RENAME VALUE(cTgtEnv + "\Programs") VALUE(cTgtEnv + "\ProgramsO").
    OS-RENAME VALUE(cTgtEnv + "\Resources") VALUE(cTgtEnv + "\ResourcesO").

    RUN ipStatus ("  Moving temp directories in " + cThisEntry + " to permanents").
    OS-RENAME VALUE(cTgtEnv + "\OverrideN") VALUE(cTgtEnv + "\Override").
    OS-RENAME VALUE(cTgtEnv + "\ProgramsN") VALUE(cTgtEnv + "\Programs").
    OS-RENAME VALUE(cTgtEnv + "\ResourcesN") VALUE(cTgtEnv + "\Resources").

    RUN ipStatus ("  Deleting old files from " + cThisEntry + " directories").
    OS-DELETE VALUE(cTgtEnv + "\OverrideO") RECURSIVE.
    OS-DELETE VALUE(cTgtEnv + "\ProgramsO") RECURSIVE.
    OS-DELETE VALUE(cTgtEnv + "\ResourcesO") RECURSIVE.

    RUN ipStatus ("Installation of new system files complete").
    
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
        cPatchNo = fiToVer:{&SV}
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
    
    RUN ipStatus("   Fix file locations for PO EDI").
    
    /* The correct target for this dir is <env>\CustFiles\EDIfiles\PO */
    /* Is it already correct? */
    ASSIGN
        cTestLoc = cEnvDir + "\" + fiEnvironment:{&SV} + "\CustFiles\EDIfiles\POs\poexport.dat".
    IF SEARCH(cTestLoc) NE ? THEN
        RETURN.        
        
    /* Is it in /Customers folder? */
    ASSIGN
        cTestLoc = cEnvDir + "\" + fiEnvironment:{&SV} + "\Customer\PO\poexport.dat".
    IF SEARCH(cTestLoc) NE ? THEN DO:
        RUN ipCopyDirs (cEnvDir + "\" + fiEnvironment:{&SV} + "\Customer\PO",
                        cEnvDir + "\" + fiEnvironment:{&SV} + "\CustFiles\EDIfiles\POs").
        RETURN.
    END.
    
    /* Is it in /PO? */
    ASSIGN
        cTestLoc = cEnvDir + "\" + fiEnvironment:{&SV} + "\PO\poexport.dat".
    IF SEARCH(cTestLoc) NE ? THEN 
        RUN ipCopyDirs (cEnvDir + "\" + fiEnvironment:{&SV} + "\PO",
                        cEnvDir + "\" + fiEnvironment:{&SV} + "\CustFiles\EDIfiles\POs").
    
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
    RUN ipStatus ("  Loading AuditTbl Records").

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
    RUN ipStatus ("  Loading Email codes").

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
    RUN ipStatus ("  Loading Lookups").

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

    
    ASSIGN 
        cTgtEnv = cEnvDir + "\" + fiEnvironment:{&SV}.

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
    RUN ipStatus ("  Loading Module Records").

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
    RUN ipStatus ("  Loading Program Master Records").

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

    /* 35628 - ensure additional field content is loaded */
    INPUT FROM VALUE(cUpdDataDir + "\prgrms.d") NO-ECHO.
    REPEAT:
        CREATE ttPrgms.
        IMPORT 
            ttPrgms.prgmname.
        FIND FIRST prgrms EXCLUSIVE WHERE 
            prgrms.prgmname EQ ttPrgms.prgmname 
            NO-ERROR.
        IF NOT AVAIL prgrms THEN DO:
            CREATE prgrms.
            BUFFER-COPY ttPrgms TO prgrms.
        END.
        ELSE DO:
            ASSIGN 
                prgrms.menuOrder = ttPrgms.menuOrder
                prgrms.menuLevel = ttPrgms.menuLevel
                prgrms.itemParent = ttPrgms.itemParent
                prgrms.mnemonic = ttPrgms.mnemonic
                prgrms.systemType = ttPrgms.systemType
                prgrms.menuImage = ttPrgms.menuImage
                prgrms.translation = ttPrgms.translation.
        END.
        DELETE ttPrgms.
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
    RUN ipStatus ("  Loading Program Master Cross-References").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadTranslation C-Win 
PROCEDURE ipLoadTranslation :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading Translation Records").

    DISABLE TRIGGERS FOR LOAD OF translation.
    
    INPUT FROM VALUE(cUpdDataDir + "\translation.d") NO-ECHO.
    REPEAT:
        CREATE ttTranslation.
        IMPORT ttTranslation.
        IF CAN-FIND(translation WHERE 
            translation.rec_key EQ ttTranslation.rec_key) THEN DO:
            DELETE ttTranslation.
            NEXT.
        END.
        ELSE DO:
            CREATE translation.
            BUFFER-COPY ttTranslation TO translation.
            DELETE ttTranslation.
        END.
    END.
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE ttTranslation.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadUserLanguage C-Win 
PROCEDURE ipLoadUserLanguage :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading UserLanguage Records").

    DISABLE TRIGGERS FOR LOAD OF UserLanguage.
    
    INPUT FROM VALUE(cUpdDataDir + "\UserLanguage.d") NO-ECHO.
    REPEAT:
        CREATE ttUserLanguage.
        IMPORT ttUserLanguage.
        IF CAN-FIND(UserLanguage WHERE 
            UserLanguage.rec_key EQ ttUserLanguage.rec_key) THEN DO:
            DELETE ttUserLanguage.
            NEXT.
        END.
        ELSE DO:
            CREATE UserLanguage.
            BUFFER-COPY ttUserLanguage TO UserLanguage.
            DELETE ttUserLanguage.
        END.
    END.
    
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE ttUserLanguage.

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
    RUN ipStatus ("  Loading New Utilities").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadUtilitiesTable C-Win 
PROCEDURE ipLoadUtilitiesTable :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading Utilities Records").

    DISABLE TRIGGERS FOR LOAD OF reftable.
    DISABLE TRIGGERS FOR LOAD OF notes.
    DISABLE TRIGGERS FOR LOAD OF Utilities.
    
    INPUT FROM VALUE(cUpdDataDir + "\Utilities.d") NO-ECHO.
    REPEAT:
        CREATE ttUtilities.
        IMPORT ttUtilities.
        IF CAN-FIND(Utilities WHERE 
            Utilities.programName EQ ttUtilities.programName) THEN 
        DO:
            DELETE ttUtilities.
            NEXT.
        END.
        ELSE 
        DO:
            CREATE Utilities.
            BUFFER-COPY ttUtilities TO Utilities.
            IF Utilities.programName = "module" THEN ASSIGN 
                utilities.securityLevel = 1000.
            ELSE ASSIGN 
                utilities.securityLevel = 900.
            
            DELETE ttUtilities.
        END.
    END.
    
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE ttUtilities.
    
    /* 25458 - Delete utilities reftables */
    FOR EACH reftable NO-LOCK WHERE 
        reftable.reftable EQ 'Utilities':
        FOR EACH notes EXCLUSIVE WHERE 
            notes.rec_key EQ reftable.rec_key:
            DELETE notes.
        END. 
    END. 
    FOR EACH reftable WHERE 
        reftable.reftable EQ 'Utilities':
        CREATE reftable1.
        BUFFER-COPY reftable TO reftable1.
        DELETE reftable.
    END. 


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
    RUN ipStatus ("  Loading Utility Notes").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadXuserMenu C-Win 
PROCEDURE ipLoadXuserMenu :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN ipStatus ("  Loading xusermenu Records").

    DISABLE TRIGGERS FOR LOAD OF xUserMenu.
    
    INPUT FROM VALUE(cUpdDataDir + "\xUserMenu.d") NO-ECHO.
    REPEAT:
        CREATE ttxUserMenu.
        IMPORT ttxUserMenu.
        IF ttxUserMenu.user_id NE "AddonUsr" THEN DO:
            DELETE ttxUserMenu.
            NEXT.
        END.
        IF CAN-FIND(xUserMenu WHERE 
            xUserMenu.rec_key EQ ttXuserMenu.rec_key) THEN DO:
            DELETE ttxUserMenu.
            NEXT.
        END.
        ELSE DO:
            CREATE xUserMenu.
            BUFFER-COPY ttxUserMenu TO xUserMenu.
            DELETE ttxUserMenu.
        END.
    END.
    
    INPUT CLOSE.
        
    EMPTY TEMP-TABLE ttxUserMenu.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipMoveUsermenusToDatabase C-Win 
PROCEDURE ipMoveUsermenusToDatabase :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cSearchDir AS CHARACTER NO-UNDO INITIAL ".\usermenu".
    DEFINE VARIABLE cUserDir   AS CHARACTER NO-UNDO FORMAT "X(60)".
    DEFINE VARIABLE cAttrList  AS CHARACTER NO-UNDO FORMAT "X(4)".
    DEFINE VARIABLE cListUsers AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cMenuList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrgmName  AS CHARACTER NO-UNDO.

    ASSIGN 
        cSearchDir =  cEnvDir + "/" + fiEnvironment:{&SV} + "/usermenu".
                
    /* get a list of usermenu folders */
    INPUT FROM OS-DIR(cSearchDir) NO-ECHO.
    REPEAT:
        SET cUserDir ^ cAttrList.
        IF cAttrList NE "d" THEN NEXT.
        IF CAN-DO(".,..",cUserDir) THEN NEXT.
        cListUsers = cListUsers + cUserDir + ",".
    END. /* repeat */
    INPUT CLOSE.
    cListUsers = TRIM(cListUsers,",").

    /* check each usermenu folder */
    DO idx = 1 TO NUM-ENTRIES(cListUsers):
        cMenuList = SEARCH(cSearchDir + "/" + ENTRY(idx,cListUsers) + "/menu.lst").
        /* if menu.lst does not exist */
        IF cMenuList EQ ? THEN NEXT.
        /* menu.lst exists, process it */
        EMPTY TEMP-TABLE ttUserMenu.
        INPUT FROM VALUE(cMenuList) NO-ECHO.
        REPEAT:
            IMPORT cPrgmName.
            CREATE ttUserMenu.
            ttUserMenu.prgmname = cPrgmName.
        END. /* repeat */
        INPUT CLOSE.
        /* look for each menu option in the user's custom menu.lst */
        FOR EACH prgrms NO-LOCK
            WHERE prgrms.menu_item EQ YES
            AND prgrms.menuOrder GT 0
            AND prgrms.menuLevel GT 0
            AND prgrms.mnemonic  NE ""
            :
            /* new additions, do not add to user's exceptions */
            IF CAN-DO("r-jcstdN.,translatn.,userLang.",prgrms.prgmname) THEN
                NEXT.
            /* if found, skip to next menu option */
            IF CAN-FIND(FIRST ttUserMenu
                WHERE ttUserMenu.prgmname EQ prgrms.prgmname) THEN
                NEXT.
            /* menu option not found in menu.lst, add as an exception */
            CREATE xUserMenu.                
            ASSIGN
                xUserMenu.user_id  = ENTRY(idx,cListUsers)
                xUserMenu.prgmname = prgrms.prgmname
                .
        END. /* each prgrms */
    END. /* do idx */


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
        SELF:LABEL = IF SELF:SENSITIVE THEN "Processing..." ELSE SELF:LABEL 
        SELF:SENSITIVE = FALSE
        lSuccess = TRUE.

    IF tbBackupDBs:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipBackupDBs.
    END.
    IF tbUserControl:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipUpdateUserControl.
    END.
    IF tbUserCleanup:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipFixUsers.
    END.
    IF tbDelBadData:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipDelBadData.
    END.
    IF tbUpdateMaster:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipUpdateMaster.
    END.
    IF tbRunDataFix:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipDataFix.
    END.
    IF tbDelDupeNotes:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipDelDupeNotes.
    END.
    IF tbUpdateNK1s:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipUpdateNK1s.
        /* RUN ipVerifyNK1Changes. */
    END.
    IF tbLoadMenus:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipLoadMenus (cUpdMenuDir,cEnvProdDir).
    END.
    IF tbRelNotes:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipCopyRelNotes.
    END.
    IF tbBackupFiles:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipArchiveFiles.
    END.
    IF tbInstallFiles:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipExpandFiles.
    END.
    IF tbRefTableConv:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipRefTableConv.
    END.
    IF tbUpdateIni:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipUpdateTTIniFile.
        RUN ipWriteIniFile.
    END.
    
    RUN ipStatus ("Patch Application Complete").

    ASSIGN
        SELF:LABEL = "Start Update"
        SELF:SENSITIVE = TRUE
        fiFromVer:{&SV} = fiToVer:{&SV}.
        
    IF lSuccess THEN MESSAGE
        "Congratulations!  Your upgrade to Advantzware Version " + fiToVer:{&SV} + " is complete." SKIP
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
    
    RUN ipStatus ("Converting Reftable records...").

    ASSIGN
        cOrigPropath = PROPATH
        cNewPropath  = cEnvDir + "\" + fiEnvironment:{&SV} + "\Programs," + PROPATH
        PROPATH = cNewPropath.
    RUN ipStatus ("   ReftableConv for " + fiEnvironment:{&SV}).
    RUN 
        VALUE(SEARCH("RefTableConv.r")).
    ASSIGN
        PROPATH = cOrigPropath.

    /* Ticket 25507 */
    RUN ipStatus ("   Ticket 25507 reftable = blank").
    FOR EACH reftable EXCLUSIVE WHERE
        reftable.reftable EQ "":
        CREATE reftable1.
        BUFFER-COPY reftable TO reftable1.
        DELETE 
            reftable.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRemoveUserAddon C-Win 
PROCEDURE ipRemoveUserAddon :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    RUN ipStatus ("    Removing addon mode from .usr file").

    DEF VAR cLine     AS CHAR NO-UNDO.
    DEF VAR cOutline  AS CHAR NO-UNDO.

    INPUT FROM VALUE(cAdminDir + "\advantzware.usr").
    REPEAT:
        IMPORT UNFORMATTED cLine.
        CREATE ttUsers.
        ASSIGN
            ttfUserid   = ENTRY(1,cLine,"|")
            ttfdbname   = ENTRY(2,cLine,"|")
            ttfalias    = ENTRY(3,cLine,"|")
            ttfenvlist  = ENTRY(4,cLine,"|")
            ttfdblist   = ENTRY(5,cLine,"|")
            ttfmodelist = ENTRY(6,cLine,"|").
    END.
    INPUT CLOSE.

    FOR EACH ttUsers:
        ASSIGN 
            ttfModeList = REPLACE(ttfModeList,"Addon","")
            ttfModeList = REPLACE(ttfModeList,",,",",").
    END.

    OUTPUT TO VALUE(cAdminDir + "\advantzware.usr").
    FOR EACH ttUsers:
        ASSIGN
            cOutline = ttfUserID + "|" +
                       ttfdbName + "|" +
                       ttfAlias  + "|" +
                       ttfEnvList + "|" +
                       ttfDbList + "|" +
                       ttfModeList.
        PUT UNFORMATTED cOutline + CHR(10).
    END.
    OUTPUT CLOSE.  


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
    RUN ipStatus ("  Setting ADMIN Password").

    FIND FIRST _User WHERE 
        _User._UserId = "admin" 
        EXCLUSIVE-LOCK NO-ERROR.

    IF NOT AVAIL (_User) THEN DO: 
        CREATE _User.
        ASSIGN
            _User._UserId = "ADMIN"
            _User._Password = ENCODE("admin").
    END.
    
    RELEASE _user.

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
        
    RUN ipStatus ("  Setting ASI Password").

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

    RELEASE _user.
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
                WHEN "hostname" THEN ASSIGN fiHostname:{&SV} = ttIniFile.cVarValue.
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
            cMsgStr[iMsgCtr] = "  " + ipcStatus.
        
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipTurnOffUserColors C-Win 
PROCEDURE ipTurnOffUserColors :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus("   Turn off user colors/fonts").
    
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
    /*
    IF SEARCH(cUpdDataDir + "\reftable.d") <> ? THEN
        RUN ipLoadUtilCodes IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\notes.d") <> ? THEN
        RUN ipLoadUtilNotes IN THIS-PROCEDURE.
    */
    IF SEARCH(cUpdDataDir + "\module.d") <> ? THEN
        RUN ipLoadModules IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\translation.d") <> ? THEN
        RUN ipLoadTranslation IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\userlanguage.d") <> ? THEN
        RUN ipLoadUserLanguage IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\xusermenu.d") <> ? THEN
        RUN ipLoadXuserMenu IN THIS-PROCEDURE.
    IF SEARCH(cUpdDataDir + "\utilities.d") <> ? THEN
        RUN ipLoadUtilitiesTable IN THIS-PROCEDURE.

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
    DISABLE TRIGGERS FOR DUMP OF sys-ctrl.
    DISABLE TRIGGERS FOR DUMP OF sys-ctrl-shipto.
    
    /* Verify system help WSDL NK1 */
    RUN ipStatus ("  Help Service entries").
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.name EQ "AsiHelpService"
        NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN
        sys-ctrl.char-fld = "-WSDL 'http:\\34.203.15.64/asihelpServices/helpmaintenance.asmx?WSDL'".
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.name EQ "AsiHelpService"
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
    RUN ipStatus ("  REPORTS - set log value to TRUE").
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.name EQ "Reports"
        NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN
        sys-ctrl.log-fld = TRUE.

    /* RelType - set Default to "B" */
    RUN ipStatus ("  RelType - if empty, set char value to Bill and Ship").
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.name EQ "RelType"
        NO-ERROR.
    IF AVAIL sys-ctrl 
    AND sys-ctrl.char-fld EQ "" THEN ASSIGN
            sys-ctrl.char-fld = "Bill and Ship".

    /* Zoho Support Button */
    RUN ipStatus ("  MenuLinkZoho").
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.name EQ "MenuLinkZoho"
        NO-ERROR.
    IF AVAIL sys-ctrl 
    AND sys-ctrl.descrip EQ "" THEN ASSIGN
        sys-ctrl.descrip = "https://desk.zoho.com/portal/advantzware/kb"
        sys-ctrl.char-fld = "Graphics\32x32\question.ico"
        sys-ctrl.log-fld = TRUE.
        
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

    ASSIGN 
        iListEntry = LOOKUP(fiEnvironment:{&SV},cEnvList).    

    FIND ttIniFile WHERE ttIniFile.cVarName = "envVerList" NO-ERROR.
    ASSIGN ENTRY(iListEntry,ttIniFile.cVarValue) = fiToVer:{&SV}.
    
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
    
    RELEASE usercontrol.
    
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
    
    CREATE ALIAS "DICTDB" FOR DATABASE asi.

    IF NOT CAN-FIND(FIRST _file WHERE 
                    _file._file-name EQ "xusermenu") THEN DO:
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
                   
    ASSIGN
        cLogFile = cUpdatesDir + "\" + "Patch" + fiToVer:{&SV} + "\installLog.txt"
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
        ELSE IF ttIniFile.cVarName NE "" THEN DO:
            IF ttIniFile.cVarName EQ "modeList" THEN ASSIGN 
                ttIniFile.cVarValue = REPLACE(ttIniFile.cVarValue,"Addon,","").
            IF ttIniFile.cVarName EQ "pgmList" THEN ASSIGN 
                ttIniFile.cVarValue = REPLACE(ttIniFile.cVarValue,"system/addmain.w,","")
                ttIniFile.cVarValue = REPLACE(ttIniFile.cVarValue,"system/addmain2.w,","")
                .
            PUT UNFORMATTED ttIniFile.cVarName + "=" + ttIniFile.cVarValue + CHR(10).
        END.
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

