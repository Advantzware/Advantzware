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
&SCOPED-DEFINE SV SCREEN-VALUE IN FRAME DEFAULT-FRAME

DEF TEMP-TABLE ttPrgms LIKE prgrms.
DEF TEMP-TABLE ttPrgmxref LIKE prgmxref.
DEF TEMP-TABLE ttEmailcod LIKE emailcod.
DEF TEMP-TABLE ttNotes LIKE notes.
DEF TEMP-TABLE ttModule LIKE module.
DEF TEMP-TABLE ttLookups LIKE lookups.
DEF TEMP-TABLE ttReftable LIKE reftable.
DEF TEMP-TABLE tempUser NO-UNDO LIKE _User.
DEF TEMP-TABLE ttIniFile
    FIELD iPos AS INT
    FIELD cRaw AS CHAR
    FIELD cVarName AS CHAR
    FIELD cVarValue AS CHAR
    INDEX idxPos IS PRIMARY UNIQUE iPos.

DEF BUFFER bnotes FOR notes.
DEF BUFFER bf-usercomp FOR usercomp.
DEF STREAM outStream.
DEF STREAM logStream.
DEF STREAM iniStream.

DEF VAR delCtr AS INT NO-UNDO.
DEF VAR dupCtr AS INT NO-UNDO.
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
DEF VAR v3 LIKE lookups.frame_field NO-UNDO.
DEF VAR v4 LIKE lookups.prgmname NO-UNDO.
DEF VAR v5 LIKE lookups.rec_key NO-UNDO.
DEF VAR cBadDirList AS CHAR NO-UNDO.
DEF VAR cPatchNo AS CHAR FORMAT "x(8)" NO-UNDO.
DEF VAR cMapDrive AS CHAR FORMAT "x(2)" NO-UNDO.
DEF VAR cVarName AS CHAR EXTENT 100 NO-UNDO.
DEF VAR cVarValue AS CHAR EXTENT 100 NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR jCtr AS INT NO-UNDO.
DEF VAR iLastIni AS INT NO-UNDO.
DEF VAR iNumUsers AS INT NO-UNDO.
DEF VAR cIniLine AS CHAR NO-UNDO.
DEF VAR cIniLoc AS CHAR NO-UNDO.
DEF VAR cUsrLine AS CHAR NO-UNDO.
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
DEF VAR iLockoutTries AS INT NO-UNDO.
DEF VAR lNeedUsercontrol AS LOG NO-UNDO.
DEF VAR cTestDir AS CHAR NO-UNDO.
DEF VAR lAllOK AS LOG NO-UNDO.

/* Advantzware.ini variables */
DEF VAR cAdminDir AS CHAR INITIAL "Admin" NO-UNDO.
DEF VAR cAdminport AS CHAR INITIAL "20952" NO-UNDO.
DEF VAR cBackupDir AS CHAR INITIAL "Backups" NO-UNDO.
DEF VAR cCurrVer AS CHAR INITIAL "10.6.0" NO-UNDO.
DEF VAR cDbAdmin AS CHAR INITIAL "DbAdmin" NO-UNDO.
DEF VAR cDbBackup AS CHAR INITIAL "Databases" NO-UNDO.
DEF VAR cDbDataDir AS CHAR INITIAL "Data" NO-UNDO.
DEF VAR cDbDir AS CHAR INITIAL "Databases" NO-UNDO.
DEF VAR cDbDirList AS CHAR INITIAL "Prod" NO-UNDO.
DEF VAR cDbList AS CHAR INITIAL "asiProd" NO-UNDO.
DEF VAR cDbPortList AS CHAR INITIAL "2826" NO-UNDO.
DEF VAR cDbProdDir AS CHAR INITIAL "Prod" NO-UNDO.
DEF VAR cDbShipDir AS CHAR INITIAL "Ship" NO-UNDO.
DEF VAR cDbStructDir AS CHAR INITIAL "Structure" NO-UNDO.
DEF VAR cDbTestDir AS CHAR INITIAL "Test" NO-UNDO.
DEF VAR cDeltaFilename AS CHAR INITIAL "asi165166.df" NO-UNDO.
DEF VAR cDeskDir AS CHAR INITIAL "Desktop" NO-UNDO.
DEF VAR cDfFilename AS CHAR INITIAL "asi166.df" NO-UNDO.
DEF VAR cDLCDir AS CHAR INITIAL "OE116" NO-UNDO.
DEF VAR cDocDir AS CHAR INITIAL "Documentation" NO-UNDO.
DEF VAR cDrive AS CHAR INITIAL "C" NO-UNDO.
DEF VAR cEnvAddonDir AS CHAR INITIAL "Addon" NO-UNDO.
DEF VAR cEnvAdmin AS CHAR INITIAL "EnvAdmin" NO-UNDO.
DEF VAR cEnvCustFiles AS CHAR INITIAL "CustFiles" NO-UNDO.
DEF VAR cEnvCustomerDir AS CHAR INITIAL "Customer" NO-UNDO.
DEF VAR cEnvDir AS CHAR INITIAL "Environments" NO-UNDO.
DEF VAR cEnvList AS CHAR INITIAL "Prod" NO-UNDO.
DEF VAR cEnvOverrideDir AS CHAR INITIAL "Override" NO-UNDO.
DEF VAR cEnvPODir AS CHAR INITIAL "PO" NO-UNDO.
DEF VAR cEnvProdDir AS CHAR INITIAL "Prod" NO-UNDO.
DEF VAR cEnvProgramsDir AS CHAR INITIAL "Programs" NO-UNDO.
DEF VAR cEnvResourceDir AS CHAR INITIAL "Resources" NO-UNDO.
DEF VAR cEnvTemplateDir AS CHAR INITIAL "Schedule" NO-UNDO.
DEF VAR cEnvScheduleDir AS CHAR INITIAL "Schedule" NO-UNDO.
DEF VAR cEnvTestDir AS CHAR INITIAL "Test" NO-UNDO.
DEF VAR cEnvUserMenuDir AS CHAR INITIAL "Usermenu" NO-UNDO.
DEF VAR cEnvUsersDir AS CHAR INITIAL "Users" NO-UNDO.
DEF VAR cHostname AS CHAR INITIAL "HOSTNAME" NO-UNDO.
DEF VAR cInstallDir AS CHAR INITIAL "Install" NO-UNDO.
DEF VAR cLockoutTries AS CHAR INITIAL "4" NO-UNDO.
DEF VAR cMakeBackup AS CHAR INITIAL "N" NO-UNDO.
DEF VAR cMapDir AS CHAR INITIAL "N" NO-UNDO.
DEF VAR cModeList AS CHAR INITIAL "Advantzware,Addon,CaseLabel,Schedule Monitor,Editor,Esko Monitor,FG XML Monitor,Loadtags,Monitor Users,Rel XML Monitor,RFID Monitor,RM Loadtag,Sharpshooter,Touchscreen" NO-UNDO.
DEF VAR cPgmBackup AS CHAR INITIAL "Programs" NO-UNDO.
DEF VAR cPgmList AS CHAR INITIAL "system/mainmenu.w,system/addmain.w,oerep/r-casetg.w,custom/asiSchW.w,_edit.p,jobxml\monitor.w,fgXml\monitor.w,oerep/r-loadtg.w,proshut.bat,relxml\monitor.w,rfid\monitor.w,rmrep/rmloadtg.w,sshoot/sshoot.w,touch/touchscr.w" NO-UNDO.
DEF VAR cProdDbName AS CHAR INITIAL "asiProd" NO-UNDO.
DEF VAR cProdDbPort AS CHAR INITIAL "2826" NO-UNDO.
DEF VAR cProdDbStFile AS CHAR INITIAL "asiProd.st" NO-UNDO.
DEF VAR cProVersion AS CHAR INITIAL "11" NO-UNDO.
DEF VAR cResBackup AS CHAR INITIAL "Resources" NO-UNDO.
DEF VAR cShipDBName AS CHAR INITIAL "asiShip" NO-UNDO.
DEF VAR cShipDBPort AS CHAR INITIAL "2825" NO-UNDO.
DEF VAR cShipDbStFile AS CHAR INITIAL "asiShip.st" NO-UNDO.
DEF VAR cSitename AS CHAR INITIAL "ASI" NO-UNDO.
DEF VAR cTestDBName AS CHAR INITIAL "asiTest" NO-UNDO.
DEF VAR cTestDbPort AS CHAR INITIAL "2827" NO-UNDO.
DEF VAR cTestDbStFile AS CHAR INITIAL "asiTest.st" NO-UNDO.
DEF VAR cTopDir AS CHAR INITIAL "asigui" NO-UNDO.
DEF VAR cUpdAdminDir AS CHAR INITIAL "Admin" NO-UNDO.
DEF VAR cUpdatesDir AS CHAR INITIAL "Updates" NO-UNDO.
DEF VAR cUpdCompressDir AS CHAR INITIAL "Compress" NO-UNDO.
DEF VAR cUpdDataDir AS CHAR INITIAL "DataFiles" NO-UNDO.
DEF VAR cUpdDataUpdateDir AS CHAR INITIAL "DataFiles" NO-UNDO.
DEF VAR cUpdDeskDir AS CHAR INITIAL "Desktop" NO-UNDO.
DEF VAR cUpdMenuDir AS CHAR INITIAL "MenuFiles" NO-UNDO.
DEF VAR cUpdProgramDir AS CHAR INITIAL "ProgramFiles" NO-UNDO.
DEF VAR cUpdRelNotesDir AS CHAR INITIAL "ReleaseNotes" NO-UNDO.
DEF VAR cUpdSQLDir AS CHAR INITIAL "SQLAccess" NO-UNDO.
DEF VAR cUpdStructureDir AS CHAR INITIAL "StructureUpdate" NO-UNDO.
DEF VAR cVerDate AS CHAR INITIAL "10/1/17" NO-UNDO.
DEF VAR cXdbList AS CHAR INITIAL "" NO-UNDO.
DEF VAR cXdbPortList AS CHAR INITIAL "" NO-UNDO.
DEF VAR cXenvList AS CHAR INITIAL "" NO-UNDO.
DEF VAR cXmodeList AS CHAR INITIAL "" NO-UNDO.
DEF VAR cXportList AS CHAR INITIAL "" NO-UNDO.
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
fiVerDate fiAdminDir fiDbAdmin fiSiteName fiHostname fiEnvAdmin fiBackupDir ~
fiDrive fiTopDir fiMapDir fiDbBackup fiDlcDir fiAdminPort fiPgmBackup ~
fiResBackup fiDfFilename fiDeltaFilename fiDbDir fiDbDataDir fiLockoutTries ~
fiLicensedUsers fiDbProdDir fiDbShipDir fiDbStructDir fiDbTestDir fiDeskDir ~
RADIO-SET-1 fiDbName-1 fiDbDir-1 fiDbPort-1 tbBackupProd fiDocDir fiEnvDir ~
fiDbName-2 fiDbDir-2 fiDbPort-2 tbBackupTest fiEnvProdDir fiDbName-3 ~
fiDbDir-3 fiDbPort-3 tbBackupAlt fiEnvAddonDir fiEnvCustFiles ~
fiEnvCustomerDir fiEnvOverrideDir tbInstallPatchComplete fiNewVer ~
bTestStructure fiEnvPoDir fiEnvProgramsDir tbUserControl fiEnvResourceDir ~
tbUserCleanup fiEnvScheduleDir tbBuildDirs fiEnvTemplateDir ~
fiEnvUserMenuDir tbDelBadData fiEnvUsersDir tbUpdateMaster fiEnvTestDir ~
tbLoadMenus fiInstallDir slDataFixPgmss fiUpdatesDir fiPatchDir ~
tbUpdateStartup fiUpdAdminDir tbRelNotes fiUpdCompressDir fiUpdDataDir ~
tbBackupDBs fiUpdDataUpdateDir tbBackupFiles fiUpdDeskDir tbInstallFiles ~
fiUpdMenuDir fiUpdProgramDir tbDelDupeNotes fiUpdRelNotesDir fiUpdSqlDir ~
bProcess fiUpdStructureDir 
&Scoped-Define DISPLAYED-OBJECTS fiCurrVer fiVerDate fiAdminDir fiDbAdmin ~
fiSiteName fiHostname fiEnvAdmin fiBackupDir fiDrive fiTopDir fiMapDir ~
fiDbBackup fiDlcDir fiAdminPort fiPgmBackup fiResBackup fiDfFilename ~
fiDeltaFilename fiDbDir fiDbDataDir fiLockoutTries fiLicensedUsers ~
fiDbProdDir fiDbShipDir fiDbStructDir fiDbTestDir fiDeskDir RADIO-SET-1 ~
fiDbName-1 fiDbDir-1 fiDbPort-1 tbBackupProd fiDocDir fiEnvDir fiDbName-2 ~
fiDbDir-2 fiDbPort-2 tbBackupTest fiEnvProdDir fiDbName-3 fiDbDir-3 ~
fiDbPort-3 tbBackupAlt fiEnvAddonDir fiEnvCustFiles fiEnvCustomerDir ~
fiEnvOverrideDir tbInstallPatchComplete fiNewVer fiEnvPoDir ~
fiEnvProgramsDir tbComp-1 tbUserControl fiEnvResourceDir tbComp-2 ~
tbUserCleanup fiEnvScheduleDir tbComp-3 tbBuildDirs fiEnvTemplateDir ~
fiEnvUserMenuDir tbComp-4 tbDelBadData fiEnvUsersDir tbComp-5 ~
tbUpdateMaster fiEnvTestDir tbComp-6 tbLoadMenus fiInstallDir ~
slDataFixPgmss fiUpdatesDir tbComp-7 tbRunDataFix fiPatchDir tbComp-8 ~
tbUpdateStartup fiUpdAdminDir tbComp-9 tbRelNotes fiUpdCompressDir ~
fiUpdDataDir tbComp-10 tbBackupDBs fiUpdDataUpdateDir tbComp-11 ~
tbBackupFiles fiUpdDeskDir tbComp-12 tbInstallFiles fiUpdMenuDir ~
fiUpdProgramDir tbComp-13 tbDelDupeNotes fiUpdRelNotesDir tbComp-14 ~
tbUpdateNK1s fiUpdSqlDir tbComp-15 tbUpdateFileLocs fiUpdStructureDir ~
tbComp-16 tbUpdateIni 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bFixStructure 
     LABEL "Fix Structure" 
     SIZE 21 BY 1.

DEFINE BUTTON bProcess 
     LABEL "Start  Update" 
     SIZE 21 BY 1.

DEFINE BUTTON bTestStructure 
     LABEL "Test Structure" 
     SIZE 21 BY 1.

DEFINE VARIABLE fiAdminDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiAdminPort AS CHARACTER FORMAT "X(256)":U 
     LABEL "Admin Port" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiBackupDir AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .76 NO-UNDO.

DEFINE VARIABLE fiCurrVer AS CHARACTER FORMAT "X(256)":U INITIAL "16.6.0" 
     LABEL "Current Version" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiDbAdmin AS CHARACTER FORMAT "X(256)":U 
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

DEFINE VARIABLE fiDbDir-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Prod" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiDbDir-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Test" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiDbDir-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Ship" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiDbName-1 AS CHARACTER FORMAT "X(256)":U INITIAL "asiProd" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE fiDbName-2 AS CHARACTER FORMAT "X(256)":U INITIAL "asiTest" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE fiDbName-3 AS CHARACTER FORMAT "X(256)":U INITIAL "asiShip" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE fiDbPort-1 AS CHARACTER FORMAT "X(256)":U INITIAL "2826" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiDbPort-2 AS CHARACTER FORMAT "X(256)":U INITIAL "2827" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiDbPort-3 AS CHARACTER FORMAT "X(256)":U INITIAL "2825" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

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

DEFINE VARIABLE fiEnvTestDir AS CHARACTER FORMAT "X(256)":U 
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

DEFINE VARIABLE fiNewVer AS CHARACTER FORMAT "X(256)":U INITIAL "16.6.4" 
     LABEL "New Version" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

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

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "", 1,
"", 2,
"", 3
     SIZE 4 BY 3.57 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 32.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 8.33.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 5.48.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 17.62.

DEFINE VARIABLE slDataFixPgmss AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 26 BY 3.81 NO-UNDO.

DEFINE VARIABLE tbBackupAlt AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE tbBackupDBs AS LOGICAL INITIAL no 
     LABEL "Backup Databases" 
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY .81 NO-UNDO.

DEFINE VARIABLE tbBackupFiles AS LOGICAL INITIAL no 
     LABEL "Backup System Files (programs, resources,env,rcode)" 
     VIEW-AS TOGGLE-BOX
     SIZE 57 BY .81 NO-UNDO.

DEFINE VARIABLE tbBackupProd AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE tbBackupTest AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE tbBuildDirs AS LOGICAL INITIAL no 
     LABEL "Build/Update Directory Structures" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 NO-UNDO.

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
     SIZE 54 BY .81 NO-UNDO.

DEFINE VARIABLE tbDelDupeNotes AS LOGICAL INITIAL no 
     LABEL "Delete duplicate notes records" 
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY .81 NO-UNDO.

DEFINE VARIABLE tbInstallFiles AS LOGICAL INITIAL no 
     LABEL "Install new system files (programs/resources)" 
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY .81 NO-UNDO.

DEFINE VARIABLE tbInstallPatchComplete AS LOGICAL INITIAL no 
     LABEL "Install Patch Complete" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tbLoadMenus AS LOGICAL INITIAL no 
     LABEL "Load new Menu files" 
     VIEW-AS TOGGLE-BOX
     SIZE 53 BY .81 NO-UNDO.

DEFINE VARIABLE tbRelNotes AS LOGICAL INITIAL no 
     LABEL "Copy Release Notes" 
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY .81 NO-UNDO.

DEFINE VARIABLE tbRunDataFix AS LOGICAL INITIAL no 
     LABEL "Run Data Fix programs" 
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateFileLocs AS LOGICAL INITIAL no 
     LABEL "Update hardcoded file locations" 
     VIEW-AS TOGGLE-BOX
     SIZE 53 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateIni AS LOGICAL INITIAL no 
     LABEL "Update advantzware.ini file" 
     VIEW-AS TOGGLE-BOX
     SIZE 53 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateMaster AS LOGICAL INITIAL no 
     LABEL "Update Master records (asiload.p)" 
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateNK1s AS LOGICAL INITIAL no 
     LABEL "Update NK1 records" 
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY .81 NO-UNDO.

DEFINE VARIABLE tbUpdateStartup AS LOGICAL INITIAL no 
     LABEL "Create/Update Startup files (Admin/Desktop)" 
     VIEW-AS TOGGLE-BOX
     SIZE 53 BY .81 NO-UNDO.

DEFINE VARIABLE tbUserCleanup AS LOGICAL INITIAL no 
     LABEL "Cleanup user files (asi user, admin user, bad user data)" 
     VIEW-AS TOGGLE-BOX
     SIZE 57 BY .81 NO-UNDO.

DEFINE VARIABLE tbUserControl AS LOGICAL INITIAL no 
     LABEL "Create/Update User Control Record" 
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiCurrVer AT ROW 2.43 COL 25 COLON-ALIGNED WIDGET-ID 66
     fiVerDate AT ROW 2.43 COL 55 COLON-ALIGNED WIDGET-ID 56
     fiAdminDir AT ROW 2.43 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     fiDbAdmin AT ROW 3.14 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     fiSiteName AT ROW 3.62 COL 25 COLON-ALIGNED WIDGET-ID 68
     fiHostname AT ROW 3.62 COL 65 COLON-ALIGNED WIDGET-ID 36
     fiEnvAdmin AT ROW 3.86 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     fiBackupDir AT ROW 4.57 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     fiDrive AT ROW 4.81 COL 25 COLON-ALIGNED WIDGET-ID 64
     fiTopDir AT ROW 4.81 COL 45 COLON-ALIGNED WIDGET-ID 62
     fiMapDir AT ROW 4.81 COL 85 COLON-ALIGNED WIDGET-ID 42
     fiDbBackup AT ROW 5.29 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     fiDlcDir AT ROW 6 COL 25 COLON-ALIGNED WIDGET-ID 44
     fiAdminPort AT ROW 6 COL 85 COLON-ALIGNED WIDGET-ID 54
     fiPgmBackup AT ROW 6 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     fiResBackup AT ROW 6.71 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     fiDfFilename AT ROW 7.19 COL 25 COLON-ALIGNED WIDGET-ID 52
     fiDeltaFilename AT ROW 7.19 COL 65 COLON-ALIGNED WIDGET-ID 50
     fiDbDir AT ROW 7.43 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     fiDbDataDir AT ROW 8.14 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 126
     fiLockoutTries AT ROW 8.38 COL 25 COLON-ALIGNED WIDGET-ID 34
     fiLicensedUsers AT ROW 8.38 COL 65 COLON-ALIGNED WIDGET-ID 440
     fiDbProdDir AT ROW 8.86 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 124
     fiDbShipDir AT ROW 9.57 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 122
     fiDbStructDir AT ROW 10.29 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 120
     fiDbTestDir AT ROW 11 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 118
     fiDeskDir AT ROW 11.71 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     RADIO-SET-1 AT ROW 12.19 COL 10 NO-LABEL WIDGET-ID 442
     fiDbName-1 AT ROW 12.19 COL 20 COLON-ALIGNED NO-LABEL WIDGET-ID 302
     fiDbDir-1 AT ROW 12.19 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 448
     fiDbPort-1 AT ROW 12.19 COL 64 COLON-ALIGNED NO-LABEL WIDGET-ID 306
     tbBackupProd AT ROW 12.19 COL 86 WIDGET-ID 338
     fiDocDir AT ROW 12.43 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     fiEnvDir AT ROW 13.14 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     fiDbName-2 AT ROW 13.38 COL 20 COLON-ALIGNED NO-LABEL WIDGET-ID 316
     fiDbDir-2 AT ROW 13.38 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 314
     fiDbPort-2 AT ROW 13.38 COL 64 COLON-ALIGNED NO-LABEL WIDGET-ID 318
     tbBackupTest AT ROW 13.38 COL 86 WIDGET-ID 340
     fiEnvProdDir AT ROW 13.86 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     fiDbName-3 AT ROW 14.57 COL 20 COLON-ALIGNED NO-LABEL WIDGET-ID 322
     fiDbDir-3 AT ROW 14.57 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 320
     fiDbPort-3 AT ROW 14.57 COL 64 COLON-ALIGNED NO-LABEL WIDGET-ID 324
     tbBackupAlt AT ROW 14.57 COL 86 WIDGET-ID 342
     fiEnvAddonDir AT ROW 14.57 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 116
     fiEnvCustFiles AT ROW 15.29 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 114
     fiEnvCustomerDir AT ROW 16 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 112
     fiEnvOverrideDir AT ROW 16.71 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 110
     tbInstallPatchComplete AT ROW 17.19 COL 11 WIDGET-ID 366
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 166.2 BY 33.33 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     fiNewVer AT ROW 17.19 COL 59 COLON-ALIGNED WIDGET-ID 46
     bTestStructure AT ROW 17.19 COL 81 WIDGET-ID 392
     fiEnvPoDir AT ROW 17.43 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     fiEnvProgramsDir AT ROW 18.14 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 136
     tbComp-1 AT ROW 18.38 COL 11 WIDGET-ID 406
     tbUserControl AT ROW 18.38 COL 17 WIDGET-ID 370
     bFixStructure AT ROW 18.38 COL 81 WIDGET-ID 394
     fiEnvResourceDir AT ROW 18.86 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 134
     tbComp-2 AT ROW 19.33 COL 11 WIDGET-ID 408
     tbUserCleanup AT ROW 19.33 COL 17 WIDGET-ID 368
     fiEnvScheduleDir AT ROW 19.57 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 132
     tbComp-3 AT ROW 20.29 COL 11 WIDGET-ID 410
     tbBuildDirs AT ROW 20.29 COL 17 WIDGET-ID 372
     fiEnvTemplateDir AT ROW 20.29 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 286
     fiEnvUserMenuDir AT ROW 21 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 130
     tbComp-4 AT ROW 21.24 COL 11 WIDGET-ID 412
     tbDelBadData AT ROW 21.24 COL 17 WIDGET-ID 374
     fiEnvUsersDir AT ROW 21.71 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 128
     tbComp-5 AT ROW 22.19 COL 11 WIDGET-ID 418
     tbUpdateMaster AT ROW 22.19 COL 17 WIDGET-ID 376
     fiEnvTestDir AT ROW 22.43 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 138
     tbComp-6 AT ROW 23.14 COL 11 WIDGET-ID 420
     tbLoadMenus AT ROW 23.14 COL 17 WIDGET-ID 378
     fiInstallDir AT ROW 23.14 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 142
     slDataFixPgmss AT ROW 23.86 COL 77 NO-LABEL WIDGET-ID 402
     fiUpdatesDir AT ROW 23.86 COL 109 COLON-ALIGNED NO-LABEL WIDGET-ID 144
     tbComp-7 AT ROW 24.1 COL 11 WIDGET-ID 414
     tbRunDataFix AT ROW 24.1 COL 17 WIDGET-ID 400
     fiPatchDir AT ROW 24.57 COL 113 COLON-ALIGNED NO-LABEL WIDGET-ID 290
     tbComp-8 AT ROW 25.05 COL 11 WIDGET-ID 416
     tbUpdateStartup AT ROW 25.05 COL 17 WIDGET-ID 380
     fiUpdAdminDir AT ROW 25.29 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 152
     tbComp-9 AT ROW 26 COL 11 WIDGET-ID 426
     tbRelNotes AT ROW 26 COL 17 WIDGET-ID 382
     fiUpdCompressDir AT ROW 26 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 150
     fiUpdDataDir AT ROW 26.71 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 148
     tbComp-10 AT ROW 26.95 COL 11 WIDGET-ID 428
     tbBackupDBs AT ROW 26.95 COL 17 WIDGET-ID 384
     fiUpdDataUpdateDir AT ROW 27.43 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 154
     tbComp-11 AT ROW 27.91 COL 11 WIDGET-ID 422
     tbBackupFiles AT ROW 27.91 COL 17 WIDGET-ID 386
     fiUpdDeskDir AT ROW 28.14 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 146
     tbComp-12 AT ROW 28.86 COL 11 WIDGET-ID 424
     tbInstallFiles AT ROW 28.86 COL 17 WIDGET-ID 388
     fiUpdMenuDir AT ROW 28.86 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 298
     fiUpdProgramDir AT ROW 29.57 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 162
     tbComp-13 AT ROW 29.81 COL 11 WIDGET-ID 434
     tbDelDupeNotes AT ROW 29.81 COL 17 WIDGET-ID 390
     fiUpdRelNotesDir AT ROW 30.29 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 160
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 166.2 BY 33.33 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     tbComp-14 AT ROW 30.76 COL 11 WIDGET-ID 452
     tbUpdateNK1s AT ROW 30.76 COL 17 WIDGET-ID 396
     fiUpdSqlDir AT ROW 31 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 158
     tbComp-15 AT ROW 31.71 COL 11 WIDGET-ID 430
     tbUpdateFileLocs AT ROW 31.71 COL 17 WIDGET-ID 398
     bProcess AT ROW 31.71 COL 81 WIDGET-ID 404
     fiUpdStructureDir AT ROW 31.71 COL 116 COLON-ALIGNED NO-LABEL WIDGET-ID 164
     tbComp-16 AT ROW 32.67 COL 11 WIDGET-ID 432
     tbUpdateIni AT ROW 32.67 COL 17 WIDGET-ID 450
     "Install" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 23.14 COL 141 WIDGET-ID 178
     "ReleaseNotes" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 30.29 COL 148 WIDGET-ID 182
     "MenuFiles" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 28.86 COL 148 WIDGET-ID 184
     "Compress" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 26 COL 148 WIDGET-ID 186
     "DataFiles" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 26.71 COL 148 WIDGET-ID 188
     "DataUpdate" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 27.43 COL 148 WIDGET-ID 190
     "Desktop" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 28.14 COL 148 WIDGET-ID 192
     "Prod" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 8.86 COL 145 WIDGET-ID 250
     " General Variables" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 1.48 COL 8 WIDGET-ID 356
          FONT 6
     "Admin" VIEW-AS TEXT
          SIZE 7 BY .76 AT ROW 2.19 COL 141 WIDGET-ID 170
     "Admin" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 2.19 COL 141 WIDGET-ID 172
     "Admin" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 2.19 COL 141 WIDGET-ID 174
     " Patch Processing" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 16.48 COL 8 WIDGET-ID 456
          FONT 6
     "Ship" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 9.57 COL 145 WIDGET-ID 252
     "Documentation" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 12.43 COL 141 WIDGET-ID 248
     "Desktop" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 11.71 COL 141 WIDGET-ID 246
     "CustFiles" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 15.29 COL 147 WIDGET-ID 210
     "Customer" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 16 COL 147 WIDGET-ID 212
     "Override" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 16.71 COL 147 WIDGET-ID 214
          FONT 6
     "Addon" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 14.57 COL 147 WIDGET-ID 216
     "Users" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 21.71 COL 147 WIDGET-ID 218
     "Test" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 22.43 COL 145 WIDGET-ID 220
     "Programs" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 6 COL 145 WIDGET-ID 280
     "Databases" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 7.43 COL 141 WIDGET-ID 282
     "DbAdmin" VIEW-AS TEXT
          SIZE 10 BY .76 AT ROW 2.91 COL 145 WIDGET-ID 284
     "Template" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 20.29 COL 147 WIDGET-ID 288
     "SQLAccess" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 31 COL 148 WIDGET-ID 292
     "StructureUpdate" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 31.71 COL 148 WIDGET-ID 296
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 166.2 BY 33.33 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     " (Defaults)" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.48 COL 144 WIDGET-ID 300
          FONT 6
     "Database Name" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.24 COL 22 WIDGET-ID 308
     "Environments" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 13.14 COL 141 WIDGET-ID 240
     "Prod" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 13.86 COL 145 WIDGET-ID 242
     "Test" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 11 COL 145 WIDGET-ID 244
     "Data" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 8.14 COL 145 WIDGET-ID 256
     " Directory Structure" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 1.48 COL 111 WIDGET-ID 140
          FONT 6
     "Backup before Install" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 11.24 COL 80 WIDGET-ID 364
     "Primary" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.24 COL 8 WIDGET-ID 446
     "Database" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 5.29 COL 145 WIDGET-ID 264
     "Resources" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 6.71 COL 145 WIDGET-ID 266
     "EnvAdmin" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 3.86 COL 145 WIDGET-ID 270
     "Backups" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 4.57 COL 141 WIDGET-ID 278
     "Structure" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 10.29 COL 145 WIDGET-ID 254
     " Database Variables" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 10.52 COL 8 WIDGET-ID 360
          FONT 6
     "(Alt)" VIEW-AS TEXT
          SIZE 6 BY .95 AT ROW 14.57 COL 15 WIDGET-ID 352
     "Test" VIEW-AS TEXT
          SIZE 6 BY .95 AT ROW 13.38 COL 15 WIDGET-ID 350
     "Prod" VIEW-AS TEXT
          SIZE 6 BY .95 AT ROW 12.19 COL 15 WIDGET-ID 348
     "Port" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 11.24 COL 66 WIDGET-ID 312
     "Directory" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 11.24 COL 49 WIDGET-ID 310
     "Schedule" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 19.57 COL 147 WIDGET-ID 208
     "Resources" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 18.86 COL 147 WIDGET-ID 206
          FONT 6
     "Programs" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 18.14 COL 147 WIDGET-ID 204
          FONT 6
     "PO" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 17.43 COL 147 WIDGET-ID 202
     "UserMenu" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 21 COL 147 WIDGET-ID 200
     "Admin" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 25.29 COL 148 WIDGET-ID 198
     "Patch<n>" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 24.57 COL 145 WIDGET-ID 196
     "Updates" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 23.86 COL 141 WIDGET-ID 194
     "ProgramFiles" VIEW-AS TEXT
          SIZE 16 BY .76 AT ROW 29.57 COL 148 WIDGET-ID 180
     RECT-1 AT ROW 1.71 COL 109 WIDGET-ID 354
     RECT-2 AT ROW 1.71 COL 5 WIDGET-ID 358
     RECT-3 AT ROW 10.76 COL 5 WIDGET-ID 362
     RECT-4 AT ROW 16.71 COL 5 WIDGET-ID 454
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 166.2 BY 33.33 WIDGET-ID 100.


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
         TITLE              = "ASI Install/Update Processor"
         HEIGHT             = 33.33
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
/* SETTINGS FOR BUTTON bFixStructure IN FRAME DEFAULT-FRAME
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
/* SETTINGS FOR TOGGLE-BOX tbRunDataFix IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbUpdateFileLocs IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbUpdateIni IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbUpdateNK1s IN FRAME DEFAULT-FRAME
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
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* ASI Install/Update Processor */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bFixStructure
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bFixStructure C-Win
ON CHOOSE OF bFixStructure IN FRAME DEFAULT-FRAME /* Fix Structure */
DO:
    RUN ipBuildDirs IN THIS-PROCEDURE.
    ASSIGN
        SELF:SENSITIVE = FALSE.
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
        
    IF tbUserControl:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipUpdateUserControl.
        ASSIGN
            tbComp-1:CHECKED = TRUE.
    END.
    IF tbUserCleanup:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipFixUsers.
        ASSIGN
            tbComp-2:CHECKED = TRUE.
    END.
    IF tbBuildDirs:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipBuildDirs.
        ASSIGN
            tbComp-3:CHECKED = TRUE.
    END.
    IF tbDelBadData:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipDelBadData.
        ASSIGN
            tbComp-4:CHECKED = TRUE.
    END.
    IF tbUpdateMaster:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipUpdateMaster.
        ASSIGN
            tbComp-5:CHECKED = TRUE.
    END.
    IF tbLoadMenus:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipLoadMenus (cUpdMenuDir,cEnvProdDir).
        ASSIGN
            tbComp-6:CHECKED = TRUE.
    END.
    IF tbRunDataFix:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipxxx.
        ASSIGN
            tbComp-7:CHECKED = TRUE.
    END.
    IF tbUpdateStartup:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipCopyStartup.
        ASSIGN
            tbComp-8:CHECKED = TRUE.
    END.
    IF tbRelNotes:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipCopyRelNotes.
        ASSIGN
            tbComp-9:CHECKED = TRUE.
    END.
    IF tbBackupDBs:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipBackupDBs.
        ASSIGN
            tbComp-10:CHECKED = TRUE.
    END.
    IF tbBackupFiles:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipArchiveFiles.
        ASSIGN
            tbComp-11:CHECKED = TRUE.
    END.
    IF tbInstallFiles:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipExpandFiles.
        ASSIGN
            tbComp-12:CHECKED = TRUE.
    END.
    IF tbDelDupeNotes:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipDelDupeNotes.
        ASSIGN
            tbComp-13:CHECKED = TRUE.
    END.
    IF tbUpdateNK1s:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipxxx.
        ASSIGN
            tbComp-14:CHECKED = TRUE.
    END.
    IF tbUpdateFileLocs:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipxxx.
        ASSIGN
            tbComp-15:CHECKED = TRUE.
    END.
    IF tbUpdateIni:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        RUN ipxxx.
        ASSIGN
            tbComp-16:CHECKED = TRUE.
    END.
    
    RUN ipStatus ("Patch Application Complete").
    ASSIGN
        SELF:LABEL = "Start Update"
        SELF:SENSITIVE = TRUE.
    STATUS INPUT.
    
    MESSAGE
        "Processing Complete."
        VIEW-AS ALERT-BOX.
        
    RETURN NO-APPLY.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bTestStructure
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bTestStructure C-Win
ON CHOOSE OF bTestStructure IN FRAME DEFAULT-FRAME /* Test Structure */
DO:
    DEF VAR lStructOK AS LOG NO-UNDO.
    RUN ipTestStructure (OUTPUT lStructOK).
    ASSIGN
        bFixStructure:SENSITIVE = NOT lStructOK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiNewVer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiNewVer C-Win
ON LEAVE OF fiNewVer IN FRAME DEFAULT-FRAME /* New Version */
DO:
    ASSIGN
        fiPatchDir:{&SV} = "PATCH" + fiNewVer:{&SV}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbInstallPatchComplete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbInstallPatchComplete C-Win
ON VALUE-CHANGED OF tbInstallPatchComplete IN FRAME DEFAULT-FRAME /* Install Patch Complete */
DO:
    ASSIGN
        tbUserControl:CHECKED = IF SELF:CHECKED AND tbUserControl:SENSITIVE THEN TRUE ELSE FALSE
        tbUserCleanup:CHECKED = SELF:CHECKED
        tbBuildDirs:CHECKED = SELF:CHECKED
        tbDelBadData:CHECKED = SELF:CHECKED
        tbUpdateMaster:CHECKED = SELF:CHECKED
        tbLoadMenus:CHECKED = SELF:CHECKED
        tbRunDataFix:CHECKED = IF SELF:CHECKED AND tbRunDataFix:SENSITIVE THEN TRUE ELSE FALSE
        tbUpdateStartup:CHECKED = SELF:CHECKED
        tbRelNotes:CHECKED = SELF:CHECKED
        tbBackupDBs:CHECKED = SELF:CHECKED
        tbBackupFiles:CHECKED = SELF:CHECKED
        tbInstallFiles:CHECKED = SELF:CHECKED
        tbDelDupeNotes:CHECKED = SELF:CHECKED
        tbUpdateNK1s:CHECKED = IF SELF:CHECKED AND tbUpdateNK1s:SENSITIVE THEN TRUE ELSE FALSE
        tbUpdateFileLocs:CHECKED = IF SELF:CHECKED AND tbUpdateFileLocs:SENSITIVE THEN TRUE ELSE FALSE
        tbUpdateIni:CHECKED = IF SELF:CHECKED AND tbUpdateIni:SENSITIVE THEN TRUE ELSE FALSE
        .

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
    RUN enable_UI.
    
    FIND FIRST usercontrol NO-LOCK NO-ERROR.
    IF AVAIL usercontrol THEN ASSIGN
        lNeedUsercontrol = FALSE.
    ELSE ASSIGN
        lNeedUsercontrol = TRUE.
        
    ASSIGN
        tbUserControl:SENSITIVE IN FRAME {&FRAME-NAME} = USERID(LDBNAME(1)) EQ "asi"
        fiLicensedUsers:SENSITIVE = tbUserControl:SENSITIVE.
    
    RUN ipCreateTTiniFile.
    RUN ipFindIniFile.

    IF cIniLoc EQ "" THEN DO:
        MESSAGE
            "Unable to locate an 'advantzware.ini' file." SKIP
            "Would you like to manually input its location?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lLocate AS LOG.
        IF lLocate THEN DO:
            UPDATE
                cIniLoc LABEL "Input location for advantzware.ini"
                WITH FRAME fIniLoc VIEW-AS DIALOG-BOX THREE-D 1 DOWN SIDE-LABELS.
        END.
        ELSE DO:
            MESSAGE
                "Would you like to create a new 'advantzware.ini' file?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lCreate AS LOG.
            IF NOT lCreate THEN DO:
                MESSAGE
                    "Exiting Installer..."
                    VIEW-AS ALERT-BOX INFO.
                RETURN.
            END.
        END.
    END.            
    IF cIniLoc NE "" THEN 
        RUN ipReadIniFile.
    RUN ipSetDispVars.
    RUN ipExpandVarNames.
    
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
  DISPLAY fiCurrVer fiVerDate fiAdminDir fiDbAdmin fiSiteName fiHostname 
          fiEnvAdmin fiBackupDir fiDrive fiTopDir fiMapDir fiDbBackup fiDlcDir 
          fiAdminPort fiPgmBackup fiResBackup fiDfFilename fiDeltaFilename 
          fiDbDir fiDbDataDir fiLockoutTries fiLicensedUsers fiDbProdDir 
          fiDbShipDir fiDbStructDir fiDbTestDir fiDeskDir RADIO-SET-1 fiDbName-1 
          fiDbDir-1 fiDbPort-1 tbBackupProd fiDocDir fiEnvDir fiDbName-2 
          fiDbDir-2 fiDbPort-2 tbBackupTest fiEnvProdDir fiDbName-3 fiDbDir-3 
          fiDbPort-3 tbBackupAlt fiEnvAddonDir fiEnvCustFiles fiEnvCustomerDir 
          fiEnvOverrideDir tbInstallPatchComplete fiNewVer fiEnvPoDir 
          fiEnvProgramsDir tbComp-1 tbUserControl fiEnvResourceDir tbComp-2 
          tbUserCleanup fiEnvScheduleDir tbComp-3 tbBuildDirs fiEnvTemplateDir 
          fiEnvUserMenuDir tbComp-4 tbDelBadData fiEnvUsersDir tbComp-5 
          tbUpdateMaster fiEnvTestDir tbComp-6 tbLoadMenus fiInstallDir 
          slDataFixPgmss fiUpdatesDir tbComp-7 tbRunDataFix fiPatchDir tbComp-8 
          tbUpdateStartup fiUpdAdminDir tbComp-9 tbRelNotes fiUpdCompressDir 
          fiUpdDataDir tbComp-10 tbBackupDBs fiUpdDataUpdateDir tbComp-11 
          tbBackupFiles fiUpdDeskDir tbComp-12 tbInstallFiles fiUpdMenuDir 
          fiUpdProgramDir tbComp-13 tbDelDupeNotes fiUpdRelNotesDir tbComp-14 
          tbUpdateNK1s fiUpdSqlDir tbComp-15 tbUpdateFileLocs fiUpdStructureDir 
          tbComp-16 tbUpdateIni 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 RECT-3 RECT-4 fiCurrVer fiVerDate fiAdminDir fiDbAdmin 
         fiSiteName fiHostname fiEnvAdmin fiBackupDir fiDrive fiTopDir fiMapDir 
         fiDbBackup fiDlcDir fiAdminPort fiPgmBackup fiResBackup fiDfFilename 
         fiDeltaFilename fiDbDir fiDbDataDir fiLockoutTries fiLicensedUsers 
         fiDbProdDir fiDbShipDir fiDbStructDir fiDbTestDir fiDeskDir 
         RADIO-SET-1 fiDbName-1 fiDbDir-1 fiDbPort-1 tbBackupProd fiDocDir 
         fiEnvDir fiDbName-2 fiDbDir-2 fiDbPort-2 tbBackupTest fiEnvProdDir 
         fiDbName-3 fiDbDir-3 fiDbPort-3 tbBackupAlt fiEnvAddonDir 
         fiEnvCustFiles fiEnvCustomerDir fiEnvOverrideDir 
         tbInstallPatchComplete fiNewVer bTestStructure fiEnvPoDir 
         fiEnvProgramsDir tbUserControl fiEnvResourceDir tbUserCleanup 
         fiEnvScheduleDir tbBuildDirs fiEnvTemplateDir fiEnvUserMenuDir 
         tbDelBadData fiEnvUsersDir tbUpdateMaster fiEnvTestDir tbLoadMenus 
         fiInstallDir slDataFixPgmss fiUpdatesDir fiPatchDir tbUpdateStartup 
         fiUpdAdminDir tbRelNotes fiUpdCompressDir fiUpdDataDir tbBackupDBs 
         fiUpdDataUpdateDir tbBackupFiles fiUpdDeskDir tbInstallFiles 
         fiUpdMenuDir fiUpdProgramDir tbDelDupeNotes fiUpdRelNotesDir 
         fiUpdSqlDir bProcess fiUpdStructureDir 
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
    DEF INPUT PARAMETER ipcUserID AS CHAR NO-UNDO.
    DEF VAR lv-default-comp AS CHAR NO-UNDO.
    DEF VAR lv-default-loc AS CHAR NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF usercomp.

    RUN ipStatus ("Adding Supplemental User Records").

    /* Add default usercomp records for new user */
    FIND FIRST bf-usercomp NO-LOCK WHERE 
        bf-usercomp.USER_id = "ASI" AND
        bf-usercomp.company_default 
        NO-ERROR.
    IF NOT AVAIL bf-usercomp THEN FIND FIRST bf-usercomp NO-LOCK WHERE 
        bf-usercomp.company_default 
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
        bf-usercomp.loc_default 
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
    DEF VAR cCmdZip AS CHAR NO-UNDO.
    DEF VAR lProdExists AS LOG NO-UNDO.
    
    RUN ipStatus ("Archiving ASI System Files").

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

    IF SEARCH(cUpdProgramDir + "\override.7z") NE ? THEN DO:
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
    DEF VAR cCmdLine AS CHAR NO-UNDO.
    
    RUN ipStatus ("Backing Up Selected Databases").
    
    IF tbBackupProd:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        ASSIGN
            cCmdLine = fiDlcDir:{&SV} + "\bin\probkup online " + 
                       fiDrive:{&SV} + "\" + 
                       fiTopDir:{&SV} + "\" +
                       fiDbDir:{&SV} + "\" +
                       fiDbDir-1:{&SV} + "\" +
                       fiDbName-1:{&SV} + " " + 
                       cDbBackup + "\" + fiDbName-1:{&SV} + 
                       STRING(YEAR(TODAY)) +
                       STRING(MONTH(TODAY),"99") +
                       STRING(DAY(TODAY),"99") + ".bak".
        OS-COMMAND SILENT VALUE(cCmdLine).
    END.
    IF tbBackupTest:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        ASSIGN
            cCmdLine = fiDlcDir:{&SV} + "\bin\probkup online " + 
                       fiDrive:{&SV} + "\" + 
                       fiTopDir:{&SV} + "\" +
                       fiDbDir:{&SV} + "\" +
                       fiDbDir-2:{&SV} + "\" +
                       fiDbName-2:{&SV} + " " + 
                       cDbBackup + "\" + fiDbName-2:{&SV} + 
                       STRING(YEAR(TODAY)) +
                       STRING(MONTH(TODAY),"99") +
                       STRING(DAY(TODAY),"99") + ".bak".
        OS-COMMAND SILENT VALUE(cCmdLine).
    END.
    IF tbBackupAlt:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        ASSIGN
            cCmdLine = fiDlcDir:{&SV} + "\bin\probkup online " + 
                       fiDrive:{&SV} + "\" + 
                       fiTopDir:{&SV} + "\" +
                       fiDbDir:{&SV} + "\" +
                       fiDbDir-3:{&SV} + "\" +
                       fiDbName-3:{&SV} + " " + 
                       cDbBackup + "\" + fiDbName-3:{&SV} + 
                       STRING(YEAR(TODAY)) +
                       STRING(MONTH(TODAY),"99") +
                       STRING(DAY(TODAY),"99") + ".bak".
        OS-COMMAND SILENT VALUE(cCmdLine).
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
    DEF VAR cBadDir AS CHAR NO-UNDO.
    
    RUN ipStatus ("Building Required Directories").

    IF cBadDirList NE "" THEN DO i = 1 TO NUM-ENTRIES(cBadDirList):
        ASSIGN
            cBadDir = ENTRY(i,cBadDirList).
        OS-CREATE-DIR VALUE(cBadDir).
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
DISABLE TRIGGERS FOR LOAD OF users.
DISABLE TRIGGERS FOR LOAD OF user-print.
DISABLE TRIGGERS FOR LOAD OF usercomp.
DISABLE TRIGGERS FOR LOAD OF userEula.
DISABLE TRIGGERS FOR LOAD OF userlog.
DISABLE TRIGGERS FOR LOAD OF usersman.
DISABLE TRIGGERS FOR LOAD OF uservend.
DISABLE TRIGGERS FOR LOAD OF usr.
DISABLE TRIGGERS FOR LOAD OF usr-grp.
DISABLE TRIGGERS FOR LOAD OF usr-menu.
DISABLE TRIGGERS FOR LOAD OF usrx.

    RUN ipStatus ("Removing Old User Records").

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
        users.developer = true
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
        users.developer = true
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCopyRelNotes C-Win 
PROCEDURE ipCopyRelNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipStatus ("Copying Release Notes").

    FILE-INFO:FILE-NAME = cDocDir.
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN
        OS-CREATE-DIR VALUE(cDocDir).
        
    OS-COPY VALUE(cUpdRelNotesDir) VALUE(cDocDir).
    
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

    OS-COPY VALUE(cUpdAdminDir) VALUE(cAdminDir).
    IF SEARCH(cAdminDir + "\advantzware.usr") = ? THEN
        OS-COPY VALUE(cUpdDataDir + "\advantzware.usr") VALUE(cAdminDir). 

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
    DO i = 1 to 100:
        CREATE ttIniFile.
        ASSIGN
            ttIniFile.iPos = i.
        CASE i - 1:
            WHEN 0 THEN ASSIGN ttIniFile.cVarName = "# Setup Variables".
            WHEN 1 THEN ASSIGN ttIniFile.cVarName = "siteName".
            WHEN 2 THEN ASSIGN ttIniFile.cVarName = "hostname".
            WHEN 3 THEN ASSIGN ttIniFile.cVarName = "drive".
            WHEN 4 THEN ASSIGN ttIniFile.cVarName = "topDir".
            WHEN 5 THEN ASSIGN ttIniFile.cVarName = "mapDir".
            WHEN 6 THEN ASSIGN ttIniFile.cVarName = "currVer".
            WHEN 7 THEN ASSIGN ttIniFile.cVarName = "verDate".
            WHEN 8 THEN ASSIGN ttIniFile.cVarName = "DLCDir".
            WHEN 9 THEN ASSIGN ttIniFile.cVarName = "# Filestructure Variables".
            WHEN 10 THEN ASSIGN ttIniFile.cVarName = "adminDir".
            WHEN 11 THEN ASSIGN ttIniFile.cVarName = "backupDir".
            WHEN 12 THEN ASSIGN ttIniFile.cVarName = "dbDir".
            WHEN 13 THEN ASSIGN ttIniFile.cVarName = "deskDir".
            WHEN 14 THEN ASSIGN ttIniFile.cVarName = "docDir".
            WHEN 15 THEN ASSIGN ttIniFile.cVarName = "envDir".
            WHEN 16 THEN ASSIGN ttIniFile.cVarName = "installDir".
            WHEN 17 THEN ASSIGN ttIniFile.cVarName = "updateDir".
            WHEN 18 THEN ASSIGN ttIniFile.cVarName = "# Admin subdirs".
            WHEN 19 THEN ASSIGN ttIniFile.cVarName = "dbAdmin".
            WHEN 20 THEN ASSIGN ttIniFile.cVarName = "envAdmin".
            WHEN 21 THEN ASSIGN ttIniFile.cVarName = "# Backup subdirs".
            WHEN 22 THEN ASSIGN ttIniFile.cVarName = "dbBackup".
            WHEN 23 THEN ASSIGN ttIniFile.cVarName = "pgmBackup".
            WHEN 24 THEN ASSIGN ttIniFile.cVarName = "resBackup".
            WHEN 25 THEN ASSIGN ttIniFile.cVarName = "# Database subdirs".
            WHEN 26 THEN ASSIGN ttIniFile.cVarName = "dbDataDir".
            WHEN 27 THEN ASSIGN ttIniFile.cVarName = "dbProdDir".
            WHEN 28 THEN ASSIGN ttIniFile.cVarName = "dbShipDir".
            WHEN 29 THEN ASSIGN ttIniFile.cVarName = "dbStructDir".
            WHEN 30 THEN ASSIGN ttIniFile.cVarName = "dbTestDir".
            WHEN 31 THEN ASSIGN ttIniFile.cVarName = "# Environment subdirs".
            WHEN 32 THEN ASSIGN ttIniFile.cVarName = "envProdDir".
            WHEN 33 THEN ASSIGN ttIniFile.cVarName = "envTestDir".
            WHEN 34 THEN ASSIGN ttIniFile.cVarName = "# Environment inner structure".
            WHEN 35 THEN ASSIGN ttIniFile.cVarName = "envAddonDir".
            WHEN 36 THEN ASSIGN ttIniFile.cVarName = "envCustFiles".
            WHEN 37 THEN ASSIGN ttIniFile.cVarName = "envCustomerDir".
            WHEN 38 THEN ASSIGN ttIniFile.cVarName = "envOverride".
            WHEN 39 THEN ASSIGN ttIniFile.cVarName = "envPoDir".
            WHEN 40 THEN ASSIGN ttIniFile.cVarName = "envProgramsDir".
            WHEN 41 THEN ASSIGN ttIniFile.cVarName = "envResourceDir".
            WHEN 42 THEN ASSIGN ttIniFile.cVarName = "envScheduleDir".
            WHEN 43 THEN ASSIGN ttIniFile.cVarName = "envTemplateDir".
            WHEN 44 THEN ASSIGN ttIniFile.cVarName = "envUserMenuDir".
            WHEN 45 THEN ASSIGN ttIniFile.cVarName = "envUsersDir".
            WHEN 46 THEN ASSIGN ttIniFile.cVarName = "# Updates sudirs".
            WHEN 47 THEN ASSIGN ttIniFile.cVarName = "updAdminDir".
            WHEN 48 THEN ASSIGN ttIniFile.cVarName = "updCompressDir".
            WHEN 49 THEN ASSIGN ttIniFile.cVarName = "updDataDir".
            WHEN 50 THEN ASSIGN ttIniFile.cVarName = "updDataUpdateDir".
            WHEN 51 THEN ASSIGN ttIniFile.cVarName = "updDeskDir".
            WHEN 52 THEN ASSIGN ttIniFile.cVarName = "updMenuDir".
            WHEN 53 THEN ASSIGN ttIniFile.cVarName = "updProgramDir".
            WHEN 54 THEN ASSIGN ttIniFile.cVarName = "updRelNotesDir".
            WHEN 55 THEN ASSIGN ttIniFile.cVarName = "updSqlDir".
            WHEN 56 THEN ASSIGN ttIniFile.cVarName = "updStructureDir".
            WHEN 57 THEN ASSIGN ttIniFile.cVarName = "# DB Connection Variables".
            WHEN 58 THEN ASSIGN ttIniFile.cVarName = "prodDbName".
            WHEN 59 THEN ASSIGN ttIniFile.cVarName = "prodDbPort".
            WHEN 60 THEN ASSIGN ttIniFile.cVarName = "prodDbStFile".
            WHEN 61 THEN ASSIGN ttIniFile.cVarName = "shipDbName".
            WHEN 62 THEN ASSIGN ttIniFile.cVarName = "shipDbPort".
            WHEN 63 THEN ASSIGN ttIniFile.cVarName = "shipDbStFile".
            WHEN 64 THEN ASSIGN ttIniFile.cVarName = "testDbName".
            WHEN 65 THEN ASSIGN ttIniFile.cVarName = "testDbPort".
            WHEN 66 THEN ASSIGN ttIniFile.cVarName = "testDbStFile".
            WHEN 67 THEN ASSIGN ttIniFile.cVarName = "# Connection Variables".
            WHEN 68 THEN ASSIGN ttIniFile.cVarName = "modeList".
            WHEN 69 THEN ASSIGN ttIniFile.cVarName = "pgmList".
            WHEN 70 THEN ASSIGN ttIniFile.cVarName = "envList".
            WHEN 71 THEN ASSIGN ttIniFile.cVarName = "dbDirList".
            WHEN 72 THEN ASSIGN ttIniFile.cVarName = "dbList".
            WHEN 73 THEN ASSIGN ttIniFile.cVarName = "dbPortList".
            WHEN 74 THEN ASSIGN ttIniFile.cVarName = "# Misc Variables".
            WHEN 75 THEN ASSIGN ttIniFile.cVarName = "adminPort".
            WHEN 76 THEN ASSIGN ttIniFile.cVarName = "dfFileName".
            WHEN 77 THEN ASSIGN ttIniFile.cVarName = "deltaFileName".
            WHEN 78 THEN ASSIGN ttIniFile.cVarName = "lockoutTries".
            WHEN 79 THEN ASSIGN ttIniFile.cVarName = "makeBackup".
        END CASE. 
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
        
        /* Add additional field data - from WK populateNotesFields */
        IF notes.createDate = ? THEN DO:
            FIND CURRENT notes EXCLUSIVE NO-ERROR.
            IF AVAIL notes THEN ASSIGN 
                notes.updateDate   = notes.note_date  
                notes.updateTime   = notes.note_time 
                notes.updateUser   = notes.user_id
                notes.createDate   = notes.note_date  
                notes.createTime   = notes.note_time 
                notes.createUser   = notes.user_id         
                .
            FIND CURRENT notes NO-LOCK.
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
    
    RUN ipStatus ("Installing New ASI System Files").

    FILE-INFO:FILE-NAME = cEnvProdDir.
    ASSIGN
        lProdExists = FILE-INFO:FULL-PATHNAME NE ?.
    FILE-INFO:FILE-NAME = cEnvTestDir.
    ASSIGN
        lTestExists = FILE-INFO:FULL-PATHNAME NE ?
        cCmdLine1 = cUpdCompressDir + "\7z.exe x ".
        
    IF SEARCH(cUpdProgramDir + "\programs.7z") NE ? THEN DO:
        RUN ipStatus ("Installing new program files").
    
        ASSIGN
            cFileType = "Programs"
            cCmdLine2 = cCmdLine1 + cUpdProgramDir + "\" + cFileType + ".7z -y -o".
        IF lProdExists THEN DO:
            OS-DELETE VALUE(cEnvProdDir + "\Programs") RECURSIVE.
            ASSIGN
                cCmdLine3 = cCmdLine2 + cEnvProdDir + "\" + cFileType.
            OS-COMMAND SILENT VALUE(cCmdLine3).
        END.
        IF lTestExists THEN DO:
            OS-DELETE VALUE(cEnvTestDir + "\Programs") RECURSIVE.
            ASSIGN
                cCmdLine3 = cCmdLine2 + cEnvTestDir + "\" + cFileType.
            OS-COMMAND SILENT VALUE(cCmdLine3).
        END.
    END.

    IF SEARCH(cUpdProgramDir + "\resources.7z") NE ? THEN DO:
        RUN ipStatus ("Installing new resource files").

        ASSIGN
            cFileType = "Resources"
            cCmdLine2 = cCmdLine1 + cUpdProgramDir + "\" + cFileType + ".7z -y -o".
        IF lProdExists THEN DO:
            OS-DELETE VALUE(cEnvProdDir + "\Resources") RECURSIVE.
            ASSIGN
                cCmdLine3 = cCmdLine2 + cEnvProdDir + "\" + cFileType.
            OS-COMMAND SILENT VALUE(cCmdLine3).
        END.
        IF lTestExists THEN DO:
            OS-DELETE VALUE(cEnvTestDir + "\Resources") RECURSIVE.
            ASSIGN
                cCmdLine3 = cCmdLine2 + cEnvTestDir + "\" + cFileType.
            OS-COMMAND SILENT VALUE(cCmdLine3).
        END.
    END.

    IF SEARCH(cUpdProgramDir + "\override.7z") NE ? THEN DO:
        RUN ipStatus ("Installing new hotfix files").

        ASSIGN
            cFileType = "Override"
            cCmdLine2 = cCmdLine1 + cUpdProgramDir + "\" + cFileType + ".7z -y -o".
        IF lProdExists THEN DO:
            OS-DELETE VALUE(cEnvProdDir + "\Override") RECURSIVE.
            ASSIGN
                cCmdLine3 = cCmdLine2 + cEnvProdDir + "\" + cFileType.
            OS-COMMAND SILENT VALUE(cCmdLine3).
        END.
        IF lTestExists THEN DO:
            OS-DELETE VALUE(cEnvTestDir + "\Override") RECURSIVE.
            ASSIGN
                cCmdLine3 = cCmdLine2 + cEnvTestDir + "\" + cFileType.
            OS-COMMAND SILENT VALUE(cCmdLine3).
        END.
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
        FOR EACH ttIniFile:
            CASE ttIniFile.cVarName:
                WHEN "siteName" THEN ASSIGN cSiteName = ttIniFile.cVarValue.
                WHEN "hostname" THEN ASSIGN cHostName = ttIniFile.cVarValue.
                WHEN "drive" THEN ASSIGN cDrive = ttIniFile.cVarValue.
                WHEN "topDir" THEN ASSIGN cTopDir = ttIniFile.cVarValue.
                WHEN "mapDir" THEN ASSIGN cMapDir = ttIniFile.cVarValue.
                WHEN "currVer" THEN ASSIGN cCurrVer = ttIniFile.cVarValue.
                WHEN "verDate" THEN ASSIGN cVerDate = ttIniFile.cVarValue.
                WHEN "DLCDir" THEN ASSIGN cDLCDir = ttIniFile.cVarValue.
                WHEN "adminDir" THEN ASSIGN cAdminDir = ttIniFile.cVarValue.
                WHEN "backupDir" THEN ASSIGN cBackupDir = ttIniFile.cVarValue.
                WHEN "dbDir" THEN ASSIGN cDbdir = ttIniFile.cVarValue.
                WHEN "deskDir" THEN ASSIGN cDeskDir = ttIniFile.cVarValue.
                WHEN "docDir" THEN ASSIGN cDocDir = ttIniFile.cVarValue.
                WHEN "envDir" THEN ASSIGN cEnvDir = ttIniFile.cVarValue.
                WHEN "installDir" THEN ASSIGN cInstallDir = ttIniFile.cVarValue.
                WHEN "updateDir" THEN ASSIGN cUpdatesDir = ttIniFile.cVarValue.
                WHEN "updatesDir" THEN ASSIGN cUpdatesDir = ttIniFile.cVarValue.
                WHEN "dbAdmin" THEN ASSIGN cDbAdmin = ttIniFile.cVarValue.
                WHEN "envAdmin" THEN ASSIGN cEnvAdmin = ttIniFile.cVarValue.
                WHEN "dbBackup" THEN ASSIGN cDbBackup = ttIniFile.cVarValue.
                WHEN "pgmBackup" THEN ASSIGN cPgmBackup = ttIniFile.cVarValue.
                WHEN "resBackup" THEN ASSIGN cResBackup = ttIniFile.cVarValue.
                WHEN "dbDataDir" THEN ASSIGN cDbDataDir = ttIniFile.cVarValue.
                WHEN "dbProdDir" THEN ASSIGN cDbProdDir = ttIniFile.cVarValue.
                WHEN "dbShipDir" THEN ASSIGN cDbShipDir = ttIniFile.cVarValue.
                WHEN "dbStructDir" THEN ASSIGN cDbStructDir = ttIniFile.cVarValue.
                WHEN "dbTestDir" THEN ASSIGN cDbTestDir = ttIniFile.cVarValue.
                WHEN "envProdDir" THEN ASSIGN cEnvProdDir = ttIniFile.cVarValue.
                WHEN "envTestDir" THEN ASSIGN cEnvTestDir = ttIniFile.cVarValue.
                WHEN "envAddonDir" THEN ASSIGN cEnvAddonDir = ttIniFile.cVarValue.
                WHEN "envCustFiles" THEN ASSIGN cEnvCustFiles = ttIniFile.cVarValue.
                WHEN "envCustomerDir" THEN ASSIGN cEnvCustomerDir = ttIniFile.cVarValue.
                WHEN "envOverride" THEN ASSIGN cEnvOverrideDir = ttIniFile.cVarValue.
                WHEN "envPoDir" THEN ASSIGN cEnvPoDir = ttIniFile.cVarValue.
                WHEN "envProgramsDir" THEN ASSIGN cEnvProgramsDir = ttIniFile.cVarValue.
                WHEN "envResourceDir" THEN ASSIGN cEnvResourceDir = ttIniFile.cVarValue.
                WHEN "envScheduleDir" THEN ASSIGN cEnvScheduleDir = ttIniFile.cVarValue.
                WHEN "envTemplateDir" THEN ASSIGN cEnvTemplateDir = ttIniFile.cVarValue.
                WHEN "envUserMenuDir" THEN ASSIGN cEnvUsermenuDir = ttIniFile.cVarValue.
                WHEN "envUsersDir" THEN ASSIGN cEnvUsersDir = ttIniFile.cVarValue.
                WHEN "updAdminDir" THEN ASSIGN cUpdAdminDir = ttIniFile.cVarValue.
                WHEN "updCompressDir" THEN ASSIGN cupdCompressDir = ttIniFile.cVarValue.
                WHEN "updDataDir" THEN ASSIGN cupdDataDir = ttIniFile.cVarValue.
                WHEN "updDataUpdateDir" THEN ASSIGN cUpdDataUpdateDir = ttIniFile.cVarValue.
                WHEN "updDeskDir" THEN ASSIGN cUpdDeskDir = ttIniFile.cVarValue.
                WHEN "updMenuDir" THEN ASSIGN cUpdMenuDir = ttIniFile.cVarValue.
                WHEN "updProgramDir" THEN ASSIGN cUpdProgramDir = ttIniFile.cVarValue.
                WHEN "updRelNotesDir" THEN ASSIGN cUpdRelNotesDir = ttIniFile.cVarValue.
                WHEN "updSqlDir" THEN ASSIGN cUpdSqlDir = ttIniFile.cVarValue.
                WHEN "updStructureDir" THEN ASSIGN cUpdStructureDir = ttIniFile.cVarValue.
                WHEN "adminPort" THEN ASSIGN cAdminPort = ttIniFile.cVarValue.
                WHEN "dfFileName" THEN ASSIGN cDfFileName = ttIniFile.cVarValue.
                WHEN "deltaFileName" THEN ASSIGN cDeltaFileName = ttIniFile.cVarValue.
                WHEN "lockoutTries" THEN ASSIGN cLockoutTries = ttIniFile.cVarValue.
            END CASE.
        END.
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
        cDbDataDir = cDbDir + "\" + cDbDataDir
        cDbProdDir = cDbDir + "\" + cDbProdDir
        cDbShipDir = cDbDir + "\" + cDbShipDir
        cDbStructDir = cDbDir + "\" + cDbStructDir
        cDbTestDir = cDbDir + "\" + cDbTestDir
        cEnvProdDir = cEnvDir + "\" + cEnvProdDir
        cEnvTestDir = cEnvDir + "\" + cEnvTestDir
        
        cPatchNo = fiNewVer:{&SV}
        cUpdDataDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdDataDir
        cUpdProgramDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdProgramDir
        cUpdAdminDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdAdminDir
        cUpdDeskDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdDeskDir
        cUpdMenuDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdMenuDir
        cUpdRelNotesDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdRelNotesDir
        cUpdStructureDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdStructureDir
        cUpdCompressDir = cUpdatesDir + "\" + "Patch" + cPatchNo + "\" + cUpdCompressDir
        lmakeBackup = IF cMakeBackup = "Y" THEN TRUE ELSE FALSE
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
            cIniLoc = "N:\Admin\advanceware.ini".
        IF SEARCH(cIniLoc) <> ? THEN DO:
            ASSIGN
                cIniLoc = SEARCH(cIniLoc).
            LEAVE.
        END.
        ASSIGN
            cIniLoc = "P:\Admin\avantzware.ini".
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
            cIniLoc = "".
    END.
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

    RUN ipStatus ("Loading New Menus").

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
            OS-CREATE-DIR VALUE(cEnvProdDir + "\" + cFileStream).
            RUN ipLoadMenus IN THIS-PROCEDURE (FILE-INFO:FILE-NAME,cEnvProdDir + "\Addon").
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

    FIND FIRST users NO-LOCK NO-ERROR.
    IF AVAIL users
    AND users.securityLevel NE 0 THEN RETURN.
    
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
        FIND FIRST usr EXCLUSIVE WHERE
            usr.uid EQ users.user_id
            NO-ERROR.
        IF NOT AVAIL usr THEN DO:
            CREATE usr.
            ASSIGN
                usr.uid = users.user_id
                usr.usr-lang = 'English'.
        END.
        ELSE DO:
            ASSIGN
                usr.usr-lang = IF usr.usr-lang = "" OR usr.usr-lang = "EN" THEN "English" ELSE usr.usr-lang.
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
        IMPORT ttReftable.
        IF NOT ttRefTable.reftable EQ "Utilities" THEN DO:
            DELETE ttReftable.
            NEXT.
        END.
        ELSE DO:
            CREATE reftable.
            BUFFER-COPY ttReftable TO reftable.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipReadIniFile C-Win 
PROCEDURE ipReadIniFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR iNextIni AS INT NO-UNDO.
    FOR EACH ttIniFile BY ttIniFile.iPos:
        IF ttIniFile.cVarName = "" THEN DO:
            ASSIGN
                iNextIni = ttIniFile.iPos.
            LEAVE.
        END.
    END.

    INPUT FROM VALUE(SEARCH(cIniLoc)).
    REPEAT:
        IMPORT UNFORMATTED cIniLine.
        IF NOT cIniLine BEGINS "#" THEN DO:
            FIND ttIniFile WHERE 
                ttIniFile.cVarName = ENTRY(1,cIniLine,"=")
                NO-ERROR.
            IF AVAIL ttIniFile THEN ASSIGN
                ttIniFile.cRaw = cIniLine
                ttIniFile.cVarValue = ENTRY(2,cIniLine,"=").
            ELSE DO:
                /* message ENTRY(1,cIniLine,"=") view-as alert-box.
                CREATE ttIniFile.
                ASSIGN
                    ttIniFile.iPos = iNextIni
                    iNextIni = iNextIni + 1
                    ttIniFile.cVarName = ENTRY(1,cIniLine,"=")
                    ttIniFile.cVarValue = ENTRY(2,cIniLine,"="). */
            END.
        END.
        ELSE DO:
            FIND ttIniFile WHERE 
                ttIniFile.cVarName = cIniLine
                NO-ERROR.
            IF AVAIL ttIniFile THEN ASSIGN
                ttIniFile.cRaw = cIniLine.
            ELSE DO:
                /* message cIniLine view-as alert-box.
                CREATE ttIniFile.
                ASSIGN
                    ttIniFile.iPos = iNextIni
                    iNextIni = iNextIni + 1
                    ttIniFile.cVarName = ENTRY(1,cIniLine,"=")
                    ttIniFile.cVarValue = ENTRY(2,cIniLine,"="). */
            END.
        END.            
    END.
    INPUT CLOSE.

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
        FOR EACH ttIniFile:
            CASE ttIniFile.cVarName:
                WHEN "siteName" THEN ASSIGN fiSiteName:{&SV} = ttIniFile.cVarValue.
                WHEN "hostname" THEN ASSIGN fiHostName:{&SV} = ttIniFile.cVarValue.
                WHEN "drive" THEN ASSIGN fiDrive:{&SV} = ttIniFile.cVarValue.
                WHEN "topDir" THEN ASSIGN fiTopDir:{&SV} = ttIniFile.cVarValue.
                WHEN "mapDir" THEN ASSIGN fiMapDir:{&SV} = ttIniFile.cVarValue.
                WHEN "currVer" THEN ASSIGN fiCurrVer:{&SV} = ttIniFile.cVarValue.
                WHEN "verDate" THEN ASSIGN fiVerDate:{&SV} = ttIniFile.cVarValue.
                WHEN "DLCDir" THEN ASSIGN fiDLCDir:{&SV} = ttIniFile.cVarValue.
                WHEN "adminDir" THEN ASSIGN fiAdminDir:{&SV} = ttIniFile.cVarValue.
                WHEN "backupDir" THEN ASSIGN fiBackupDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbDir" THEN ASSIGN fiDbdir:{&SV} = ttIniFile.cVarValue.
                WHEN "deskDir" THEN ASSIGN fiDeskDir:{&SV} = ttIniFile.cVarValue.
                WHEN "docDir" THEN ASSIGN fiDocDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envDir" THEN ASSIGN fiEnvDir:{&SV} = ttIniFile.cVarValue.
                WHEN "installDir" THEN ASSIGN fiInstallDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updateDir" THEN ASSIGN fiUpdatesDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbAdmin" THEN ASSIGN fiDbAdmin:{&SV} = ttIniFile.cVarValue.
                WHEN "envAdmin" THEN ASSIGN fiEnvAdmin:{&SV} = ttIniFile.cVarValue.
                WHEN "dbBackup" THEN ASSIGN fiDbBackup:{&SV} = ttIniFile.cVarValue.
                WHEN "pgmBackup" THEN ASSIGN fiPgmBackup:{&SV} = ttIniFile.cVarValue.
                WHEN "resBackup" THEN ASSIGN fiResBackup:{&SV} = ttIniFile.cVarValue.
                WHEN "dbDataDir" THEN ASSIGN fiDbDataDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbProdDir" THEN ASSIGN fiDbProdDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbShipDir" THEN ASSIGN fiDbShipDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbStructDir" THEN ASSIGN fiDbStructDir:{&SV} = ttIniFile.cVarValue.
                WHEN "dbTestDir" THEN ASSIGN fiDbTestDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envProdDir" THEN ASSIGN fiEnvProdDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envTestDir" THEN ASSIGN fiEnvTestDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envAddonDir" THEN ASSIGN fiEnvAddonDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envCustFiles" THEN ASSIGN fiEnvCustFiles:{&SV} = ttIniFile.cVarValue.
                WHEN "envCustomerDir" THEN ASSIGN fiEnvCustomerDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envOverride" THEN ASSIGN fiEnvOverrideDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envPoDir" THEN ASSIGN fiEnvPoDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envProgramsDir" THEN ASSIGN fiEnvProgramsDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envResourceDir" THEN ASSIGN fiEnvResourceDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envScheduleDir" THEN ASSIGN fiEnvScheduleDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envTemplateDir" THEN ASSIGN fiEnvTemplateDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envUserMenuDir" THEN ASSIGN fiEnvUsermenuDir:{&SV} = ttIniFile.cVarValue.
                WHEN "envUsersDir" THEN ASSIGN fiEnvUsersDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updAdminDir" THEN ASSIGN fiUpdAdminDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updCompressDir" THEN ASSIGN fiupdCompressDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updDataDir" THEN ASSIGN fiupdDataDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updDataUpdateDir" THEN ASSIGN fiUpdDataUpdateDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updDeskDir" THEN ASSIGN fiUpdDeskDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updMenuDir" THEN ASSIGN fiUpdMenuDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updProgramDir" THEN ASSIGN fiUpdProgramDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updRelNotesDir" THEN ASSIGN fiUpdRelNotesDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updSqlDir" THEN ASSIGN fiUpdSqlDir:{&SV} = ttIniFile.cVarValue.
                WHEN "updStructureDir" THEN ASSIGN fiUpdStructureDir:{&SV} = ttIniFile.cVarValue.
                WHEN "prodDbName" THEN ASSIGN fiDbName-1:{&SV} = ttIniFile.cVarValue.
                WHEN "prodDbPort" THEN ASSIGN fiDbPort-1:{&SV} = ttIniFile.cVarValue.
                /* WHEN "prodDbStFile" THEN ASSIGN fiDbStFile-1:{&SV} = ttIniFile.cVarValue. */
                WHEN "shipDbName" THEN ASSIGN fiDbName-3:{&SV} = ttIniFile.cVarValue.
                WHEN "shipDbPort" THEN ASSIGN fiDbPort-3:{&SV} = ttIniFile.cVarValue.
                /* WHEN "shipbStFile" THEN ASSIGN fi:{&SV} = ttIniFile.cVarValue. */
                WHEN "testDbName" THEN ASSIGN fiDbName-2:{&SV} = ttIniFile.cVarValue.
                WHEN "testDbPort" THEN ASSIGN fiDbPort-2:{&SV} = ttIniFile.cVarValue.
                /* WHEN "testDbStFile" THEN ASSIGN fi:{&SV} = ttIniFile.cVarValue. */
                /* WHEN "modeList" THEN ASSIGN fi:{&SV} = ttIniFile.cVarValue. */
                /* WHEN "pgmList" THEN ASSIGN fi:{&SV} = ttIniFile.cVarValue. */
                /* WHEN "envList" THEN ASSIGN fi:{&SV} = ttIniFile.cVarValue. */
                WHEN "dbDirList" THEN DO: 
                    ASSIGN
                        fiDbDir-1:{&SV} = ENTRY(1,ttIniFile.cVarValue)
                        fiDbDir-2:{&SV} = IF NUM-ENTRIES(ttIniFile.cVarValue) GT 1 THEN ENTRY(2,ttIniFile.cVarValue) ELSE ""
                        fiDbDir-3:{&SV} = IF NUM-ENTRIES(ttIniFile.cVarValue) GT 2 THEN ENTRY(3,ttIniFile.cVarValue) ELSE ""
                        .
                END.
                WHEN "dbList" THEN DO:
                    ASSIGN
                        fiDbName-1:{&SV} = ENTRY(1,ttIniFile.cVarValue)
                        fiDbName-2:{&SV} = IF NUM-ENTRIES(ttIniFile.cVarValue) GT 1 THEN ENTRY(2,ttIniFile.cVarValue) ELSE ""
                        fiDbName-3:{&SV} = IF NUM-ENTRIES(ttIniFile.cVarValue) GT 2 THEN ENTRY(3,ttIniFile.cVarValue) ELSE ""
                        .
                END.
                WHEN "dbPortList" THEN DO:
                    ASSIGN
                        fiDbPort-1:{&SV} = ENTRY(1,ttIniFile.cVarValue)
                        fiDbPort-2:{&SV} = IF NUM-ENTRIES(ttIniFile.cVarValue) GT 1 THEN ENTRY(2,ttIniFile.cVarValue) ELSE ""
                        fiDbPort-3:{&SV} = IF NUM-ENTRIES(ttIniFile.cVarValue) GT 2 THEN ENTRY(3,ttIniFile.cVarValue) ELSE ""
                        .
                END.
                WHEN "adminPort" THEN ASSIGN fiAdminPort:{&SV} = ttIniFile.cVarValue.
                WHEN "dfFileName" THEN ASSIGN fiDfFileName:{&SV} = ttIniFile.cVarValue.
                WHEN "deltaFileName" THEN ASSIGN fiDeltaFileName:{&SV} = ttIniFile.cVarValue.
                WHEN "lockoutTries" THEN ASSIGN fiLockoutTries:{&SV} = ttIniFile.cVarValue.
                /* WHEN "makeBackup" THEN ASSIGN fi:{&SV} = ttIniFile.cVarValue. */
            END CASE.
        END.
    END.
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

    ASSIGN
        cLogFile = cUpdatesDir + "\" + "Patch" + fiNewVer:{&SV} + "\installLog.txt"
        iMsgCtr = iMsgCtr + 1
        cMsgStr[iMsgCtr] = ipcStatus + "...".
        
    DO:
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
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField2}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.

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
    ASSIGN
        cTestDir = fiMapDir:{&SV} + "\" + {&cField1}:{&SV} + "\" + {&cField2}:{&SV} + "\"
        FILE-INFO:FILE-NAME = cTestDir
        lStructOK = FILE-INFO:FULL-PATHNAME NE ?
        lAllOK = IF NOT lStructOK THEN FALSE ELSE lAllOK
        cBadDirList = IF NOT lAllOK THEN (cBadDirList + cTestDir + ",") ELSE cBadDirList
        {&cField2}:BGCOLOR = IF (NOT lStructOK OR INDEX(cTestDir,"\\") <> 0) THEN 14 ELSE ?.

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

    ASSIGN
        tbComp-1:CHECKED = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

