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

DEF STREAM s1.
DEF STREAM sInstr.
DEF STREAM outFile.
DEF STREAM logFile.
DEF STREAM outStream.
DEF STREAM logStream.
DEF STREAM iniStream.

DEF TEMP-TABLE ttIniFile
    FIELD iPos AS INT
    FIELD cRaw AS CHAR
    FIELD cVarName AS CHAR
    FIELD cVarValue AS CHAR
    INDEX idxPos IS PRIMARY UNIQUE iPos.
    
DEF TEMP-TABLE ttDatabases
    FIELD cName AS CHAR
    FIELD cDir AS CHAR
    FIELD cPort AS CHAR
    FIELD cVer AS CHAR
    FIELD cAudName AS CHAR
    FIELD cAudPort AS CHAR.

DEFINE VARIABLE cDLC    AS CHARACTER NO-UNDO.
DEF    VAR      cDevCfg AS CHAR      NO-UNDO.
DEF    VAR      cRunCfg AS CHAR      NO-UNDO.
DEF    VAR      cCfgCfg AS CHAR      NO-UNDO.
DEF    VAR      cState  AS CHAR      NO-UNDO.
DEF VAR deMinLevel AS DECI NO-UNDO INITIAL 16.7.
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
DEF VAR iUserLevel AS INT NO-UNDO.
DEF VAR iNumUsers AS INT NO-UNDO.
DEF VAR iListEntry AS INT NO-UNDO.
DEF VAR iIndex AS INT NO-UNDO.
DEF VAR cIniLine AS CHAR NO-UNDO.
DEF VAR cIniLoc AS CHAR NO-UNDO.
DEF VAR cUsrLoc AS CHAR NO-UNDO.
DEF VAR cUsrLine AS CHAR NO-UNDO.
DEF VAR lConnectAudit AS LOG NO-UNDO.
DEF VAR lFoundIni AS LOG NO-UNDO.
DEF VAR lFoundUsr AS LOG NO-UNDO.
DEF VAR lCorrupt AS LOG NO-UNDO.
DEF VAR lSuccess AS LOG NO-UNDO.
DEF VAR lSysError AS LOG NO-UNDO.
DEF VAR lMakeBackup AS LOG NO-UNDO.
DEF VAR lValidDB AS LOG NO-UNDO.
DEF VAR lHeader AS LOG NO-UNDO.
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
DEF VAR cDLList AS CHAR NO-UNDO.
DEF VAR cPatchList AS CHAR NO-UNDO.
DEF VAR cFtpInstrFile AS CHAR NO-UNDO.
DEF VAR lOKtoProceed AS LOG  NO-UNDO.
DEF VAR cCmdLine     AS CHAR NO-UNDO.
DEF VAR cIpAddress AS CHAR NO-UNDO.
DEF VAR cFtpUser AS CHAR NO-UNDO.
DEF VAR cFtpPassword AS CHAR NO-UNDO.
DEF VAR cInstallerFile AS CHAR NO-UNDO.
DEF VAR cFtpOutputFile AS CHAR NO-UNDO.
DEF VAR cFtpErrFile AS CHAR NO-UNDO.
DEF VAR c7ZOutputFile AS CHAR NO-UNDO.
DEF VAR c7ZErrFile AS CHAR NO-UNDO.
DEF VAR iCurrDbVer AS INT NO-UNDO.
DEF VAR iPatchDbVer AS INT NO-UNDO.
DEF VAR iCurrEnvVer AS INT NO-UNDO.
DEF VAR iPatchEnvVer AS INT NO-UNDO.
DEF VAR cLogFile AS CHAR NO-UNDO.
DEF VAR cOutFile AS CHAR NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 RECT-4 fiUserID bCancel ~
fiPassword bGetFiles slEnvList eStatus 
&Scoped-Define DISPLAYED-OBJECTS fiUserID fiPassword slEnvList ~
fiFromVersion fiToVersion eStatus tbClearLog fiLogFile 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fIntVer C-Win 
FUNCTION fIntVer RETURNS INTEGER
  ( INPUT cVerString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bCancel AUTO-END-KEY 
     LABEL "Exit" 
     SIZE 11 BY 1.91.

DEFINE BUTTON bGetFiles 
     LABEL "Download" 
     SIZE 40 BY 1.91.

DEFINE BUTTON bUpdate 
     LABEL "Start Update" 
     SIZE 40 BY 1.91.

DEFINE VARIABLE eStatus AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 75 BY 9.52 NO-UNDO.

DEFINE VARIABLE fiFromVersion AS CHARACTER FORMAT "X(256)":U INITIAL "99.99.99.99" 
     LABEL "From Version" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fiLogFile AS CHARACTER FORMAT "X(256)":U INITIAL "Log of actions will be stored in N:~\Admin~\EnvAdmin~\UpdateLog.txt" 
      VIEW-AS TEXT 
     SIZE 65 BY .62 NO-UNDO.

DEFINE VARIABLE fiPassword AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiToVersion AS CHARACTER FORMAT "X(256)":U INITIAL "99.99.99.99" 
     LABEL "To Version" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fiUserID AS CHARACTER FORMAT "X(256)":U 
     LABEL "User ID" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 3.33.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 3.33.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 2.86.

DEFINE VARIABLE slEnvList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 30 BY 1.67 NO-UNDO.

DEFINE VARIABLE tbClearLog AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiUserID AT ROW 2.19 COL 19 COLON-ALIGNED WIDGET-ID 18
     bCancel AT ROW 2.19 COL 63 WIDGET-ID 16 NO-TAB-STOP 
     fiPassword AT ROW 3.38 COL 19 COLON-ALIGNED WIDGET-ID 20 PASSWORD-FIELD 
     bGetFiles AT ROW 6.24 COL 21 WIDGET-ID 32
     slEnvList AT ROW 10.29 COL 21 NO-LABEL WIDGET-ID 58
     bUpdate AT ROW 13.14 COL 6 WIDGET-ID 14
     fiFromVersion AT ROW 13.14 COL 63 COLON-ALIGNED WIDGET-ID 38
     fiToVersion AT ROW 14.33 COL 63 COLON-ALIGNED
     eStatus AT ROW 16.71 COL 3 NO-LABEL WIDGET-ID 52
     tbClearLog AT ROW 26.48 COL 74 WIDGET-ID 60
     fiLogFile AT ROW 26.71 COL 5 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     " Step 2 - Download and uncompress the latest ASI upgrade files" VIEW-AS TEXT
          SIZE 62 BY .62 AT ROW 5.29 COL 3 WIDGET-ID 30
     "Status:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 15.76 COL 4 WIDGET-ID 54
     " Step 1 - Enter a valid user id and password" VIEW-AS TEXT
          SIZE 43 BY .62 AT ROW 1.24 COL 3 WIDGET-ID 22
     " Step 3 - Choose the environment to upgrade" VIEW-AS TEXT
          SIZE 44 BY .62 AT ROW 9.33 COL 3 WIDGET-ID 24
     RECT-2 AT ROW 1.48 COL 2 WIDGET-ID 44
     RECT-3 AT ROW 5.52 COL 2 WIDGET-ID 46
     RECT-4 AT ROW 9.57 COL 2 WIDGET-ID 48
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.8 BY 26.62 WIDGET-ID 100.


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
         TITLE              = "ASIupdate 160800-01"
         HEIGHT             = 26.62
         WIDTH              = 79.8
         MAX-HEIGHT         = 26.67
         MAX-WIDTH          = 81
         VIRTUAL-HEIGHT     = 26.67
         VIRTUAL-WIDTH      = 81
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
/* SETTINGS FOR BUTTON bUpdate IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFromVersion IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiLogFile IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiToVersion IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbClearLog IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* ASIupdate 160800-01 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
  QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* ASIupdate 160800-01 */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON ALT-C OF FRAME DEFAULT-FRAME
ANYWHERE
DO:
    ASSIGN
        tbClearLog:CHECKED IN FRAME {&FRAME-NAME} = NOT tbClearLog:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bCancel C-Win
ON CHOOSE OF bCancel IN FRAME DEFAULT-FRAME /* Exit */
OR CHOOSE of bGetFiles
OR CHOOSE of bUpdate
DO:
    
    CASE SELF:NAME:
        WHEN "bCancel" THEN DO:
            RUN ipStatus("User chose Exit button").
            IF cState EQ "Dev" THEN 
            DO:
                OS-RENAME VALUE (cCfgCfg) VALUE(cDLC + "\progress.dev").
                OS-RENAME VALUE (cRunCfg) VALUE(cDLC + "\progress.cfg").
                RUN ipStatus("Resetting Progress mode").
            END.
            APPLY 'close' TO THIS-PROCEDURE.
            QUIT.
        END.
        WHEN "bGetFiles" THEN DO:
            RUN ipStatus("User chose Download button").
            RUN ipGetPatchList (1).
            RUN ipBuildVerification (1).
            RUN ipDownload.
            RUN ipExpand.
            RUN ipGetPatchList (2).
            RUN ipBuildVerification (2).
            ASSIGN
                slEnvList:SENSITIVE = TRUE
                bGetFiles:SENSITIVE = TRUE
                bGetFiles:LABEL = "Download"
                bUpdate:SENSITIVE = TRUE.
            APPLY 'entry' to slEnvList.
            RETURN NO-APPLY.
        END.
        WHEN "bUpdate" THEN DO:
            RUN ipStatus("User chose Start Update button").
            
            RUN ipValidateChoices (OUTPUT lOKtoProceed).
            IF NOT lOKtoProceed THEN DO:
                RUN ipStatus("User made invalid choices for application").
                RETURN.
            END.
    
            RUN ipStatus(" ").
            RUN ipStatus("UPGRADING ENVIRONMENT " + slEnvList:{&SV}).
            RUN ipStatus("  from version " + fiFromVersion:{&SV} + " to version: " + fiToVersion:{&SV}).
            RUN ipStatus(" ").
    
            RUN ipCopyDeploy.
            RUN ipProcess.
            RUN ipStatus("Upgrade Complete.").
            RUN ipStatus(" ").
            
            RUN ipStatus("Sending report to ASI").
            RUN ipBuildVerification (3).
            RUN ipSendVerification.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPassword
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPassword C-Win
ON LEAVE OF fiPassword IN FRAME DEFAULT-FRAME /* Password */
DO:
    IF SELF:{&SV} EQ "" THEN DO:
        RUN ipStatus("  Entered blank Password").
        RUN ipStatus("  Advised that this is not allowed in upgrade process").
        MESSAGE
            "This function does not allow blank passwords." SKIP
            "If your user id has a blank password, you must" SKIP
            "use a userid with higher-level privileges."
            VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' to fiUserID.
        RETURN NO-APPLY.
    END.
    
    IF (fiUserID:{&SV} EQ "asi" AND NOT SELF:{&SV} EQ "Package99")
    OR (fiUserID:{&SV} EQ "admin" AND NOT SELF:{&SV} EQ "installme") THEN DO:
        MESSAGE 
            "The UserID and Password you entered do not match." SKIP 
            "Please re-enter or cancel."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            SELF:{&SV} = "".
        APPLY 'entry' TO fiUserID.
        RETURN NO-APPLY.
    END. 
        
    ASSIGN
        bGetFiles:SENSITIVE = TRUE
        slEnvList:SENSITIVE = TRUE
        bUpdate:SENSITIVE = TRUE
        tbClearLog:SENSITIVE = fiUserID:{&SV} EQ "asi".
    
    APPLY 'entry' TO bGetFiles.
    RETURN NO-APPLY.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUserID C-Win
ON ENTRY OF fiUserID IN FRAME DEFAULT-FRAME /* User ID */
OR ENTRY OF fiPassword
OR ENTRY OF bGetFiles
OR ENTRY OF slEnvList
OR ENTRY OF bUpdate
DO:
    CASE SELF:NAME:
        WHEN "fiUserID" THEN ASSIGN
            eStatus:{&SV}       = eStatus:{&SV} + "Enter a valid user id for your database..." + CHR(10)
            eStatus:CURSOR-LINE = eStatus:NUM-LINES.
        WHEN "fiPassword" THEN ASSIGN
            eStatus:{&SV}       = eStatus:{&SV} + "Enter a valid password for this user..." + CHR(10)
                eStatus:CURSOR-LINE = eStatus:NUM-LINES.
        WHEN "bGetFiles" THEN ASSIGN
                eStatus:{&SV}       = eStatus:{&SV} + "Choose to download new patch files..." + CHR(10)
                eStatus:CURSOR-LINE = eStatus:NUM-LINES.
        WHEN "slEnvList" THEN ASSIGN
                eStatus:{&SV}       = eStatus:{&SV} + "Select the environment to update..." + CHR(10)
                eStatus:CURSOR-LINE = eStatus:NUM-LINES.
        WHEN "bUpdate" THEN ASSIGN
                eStatus:{&SV}       = eStatus:{&SV} + "Choose to start the update process..." + CHR(10)
                eStatus:CURSOR-LINE = eStatus:NUM-LINES.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUserID C-Win
ON LEAVE OF fiUserID IN FRAME DEFAULT-FRAME /* User ID */
DO:
    IF SELF:{&SV} = "" THEN 
    DO:
        RUN ipStatus("  Blank UserID - ENTRY to Exit button").
        APPLY 'entry' TO bCancel.
        RETURN NO-APPLY.
    END.

    IF SELF:{&SV} NE "asi"
    AND SELF:{&SV} NE "admin" THEN DO:
        MESSAGE 
            "This is not a valid user id for this function." SKIP 
            "Please re-enter or Exit."
            VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
    END.
    
    RUN ipStatus("  Entered UserID " + SELF:{&SV}).
    
    APPLY 'entry' to fiPassword.
    RETURN NO-APPLY.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slEnvList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slEnvList C-Win
ON VALUE-CHANGED OF slEnvList IN FRAME DEFAULT-FRAME
DO:
    CASE SELF:NAME:
        WHEN "slEnvList" THEN DO:
            ASSIGN
                iIndex = LOOKUP(SELF:{&SV},SELF:LIST-ITEMS)
                fiFromVersion:{&SV} = ENTRY(iIndex,cEnvVerList)
                iCurrEnvVer = fIntVer(fiFromVersion:{&SV})
                iCurrDbVer = fIntVer(ENTRY(iIndex,cDBVerList))
                .
        END.
    END CASE.
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
    
    DEF VAR deEnvVer AS DECIMAL NO-UNDO.
    DEF VAR lNeedDBWork AS LOG NO-UNDO.
    DEF VAR lGoodNos AS CHAR NO-UNDO.
    DEF VAR lGoodList AS CHAR NO-UNDO.

    /* There should not be any connected DBs at this point.  If there are, disconnect */
    DO ictr = 1 TO NUM-DBS:
        DISCONNECT VALUE (LDBNAME(ictr)).
    END. 
    
    RUN ipCreateTTiniFile.
    RUN ipFindIniFile.
    IF cIniLoc NE "" THEN 
        RUN ipReadIniFile.
    RUN ipExpandVarNames.
    
    ASSIGN
        lMakeBackup = TRUE 
        slEnvList:LIST-ITEMS = cEnvList
        slEnvList:SCREEN-VALUE = ENTRY(1,cEnvList)
        iIndex              = LOOKUP(slEnvList:{&SV},slEnvList:LIST-ITEMS)
        fiFromVersion:{&SV} = ENTRY(iIndex,cEnvVerList)
        iCurrEnvVer         = fIntVer(fiFromVersion:{&SV})
        iCurrDbVer          = fIntVer(ENTRY(iIndex,cDBVerList))
        .

    RUN ipGetPatchList (0).

    ASSIGN
        bGetFiles:SENSITIVE = FALSE
        slEnvList:SENSITIVE = FALSE
        bUpdate:SENSITIVE = FALSE
        cIpAddress     = "34.203.15.64"
        cFtpUser       = "ftptest"
        cFtpPassword   = "TestFTP1!"
        cInstallerFile = "*.7z"
        cFtpOutputFile = cEnvAdmin + "\ftpOutput.txt"
        cFtpErrFile    = cEnvAdmin + "\ftpErrs.txt"
        c7ZOutputFile  = cEnvAdmin + "\7zOutput.txt"
        c7ZErrFile     = cEnvAdmin + "\7zErrs.txt"
        cFtpInstrFile  = cEnvAdmin + "\ftpInstr.txt"
        cOutFile = cEnvAdmin + "\" + cSiteName + "-" +
                   STRING(YEAR(TODAY),"9999") +
                   STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + ".txt"
        .    
    
    /* Look in Progress dir to find out which mode (RUN/DEV) we're in */
    GET-KEY-VALUE SECTION 'Startup' KEY 'DLC' VALUE cDLC.
    ASSIGN 
        cDevCfg = cDLC + "\progress.dev"
        cRunCfg = cDLC + "\progress.run"
        cCfgCfg = cDLC + "\progress.cfg".
        
    /* Make sure progress.cfg is where we expect it; otherwise disable this feature */    
    IF SEARCH (cCfgCfg) EQ ? THEN MESSAGE 
            "Unable to locate a valid progress.cfg."
            VIEW-AS ALERT-BOX ERROR.
    
    /* Make sure progress.dev has been installed */    
    IF SEARCH (cDevCfg) EQ ? 
    AND SEARCH (cRunCfg) EQ ? THEN DO:
        OS-COPY VALUE(cUpdStructureDir + "\STFiles\progress.dev") VALUE(cDLC).
    END. 
    
    /* Now figure out if we're in dev mode or run mode */
    IF SEARCH (cDevCfg) NE ? THEN ASSIGN
        cState              = "Run".       
    ELSE IF SEARCH (cRunCfg) NE ? THEN ASSIGN
        cState              = "Dev".
                       
    IF SEARCH(cFtpOutputFile) NE ? THEN
      OS-DELETE VALUE(SEARCH(cFtpOutputFile)).
    IF SEARCH(cFtpErrFile) NE ? THEN
        OS-DELETE VALUE(SEARCH(cFtpErrFile)).
    IF SEARCH(c7zOutputFile) NE ? THEN
        OS-DELETE VALUE(SEARCH(c7zOutputFile)).
    IF SEARCH(c7zErrFile) NE ? THEN
      OS-DELETE VALUE(SEARCH(c7zErrFile)).
    
    IF SEARCH(cFtpInstrFile) NE ? THEN
      OS-DELETE VALUE(SEARCH(cFtpInstrFile)).
      
    RUN ipStatus("Initialize").
    
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
  DISPLAY fiUserID fiPassword slEnvList fiFromVersion fiToVersion eStatus 
          tbClearLog fiLogFile 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-2 RECT-3 RECT-4 fiUserID bCancel fiPassword bGetFiles slEnvList 
         eStatus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipBuildVerification C-Win 
PROCEDURE ipBuildVerification :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipiPhase AS INT NO-UNDO.
    DEF VAR cLine AS CHAR NO-UNDO.
    
    IF NOT lHeader THEN DO:
        OUTPUT STREAM outFile TO VALUE(cOutFile).
        OUTPUT STREAM outFile CLOSE.
        ASSIGN
            lHeader = TRUE.
    END.
    
    CASE ipiPhase:
        WHEN 1 THEN DO:
            RUN ipStatus("  Building FTP Verification file").
            OUTPUT STREAM outFile TO VALUE(cOutFile).
            PUT STREAM outFile UNFORMATTED "Download started " + STRING(TODAY) + " at " + STRING(TIME,"HH:MM:SS") + CHR(10).
            OUTPUT STREAM outFile CLOSE.
        END.
        WHEN 2 THEN DO:
            OUTPUT STREAM outFile TO VALUE(cOutFile) APPEND.
            PUT STREAM outFile UNFORMATTED "Downloads: " + cDLList + CHR(10).
            OUTPUT STREAM outFile CLOSE.
        END.
        WHEN 3 THEN DO:
            OUTPUT STREAM outFile TO VALUE(cOutFile) APPEND.
            INPUT STREAM logFile FROM VALUE(cLogFile).
            REPEAT:
                IMPORT STREAM logFile UNFORMATTED cLine.
                PUT STREAM outFile UNFORMATTED "  " + cLine + CHR(10).
            END.
            INPUT STREAM logFile CLOSE.
            OUTPUT STREAM outFile CLOSE.
        END.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCopyDeploy C-Win 
PROCEDURE ipCopyDeploy :
/*------------------------------------------------------------------------------
  Purpose:     Copy Patch Deployment Directory Entries to Base Structure
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN
        cCmdLine = "XCOPY " + cUpdatesDir + "\" + cPatchList + "\Deployment\Admin\*.* " + cMapDir + "\Admin\" + " /C /E /Q /Y".
    OS-COMMAND SILENT VALUE(cCmdLine).
    ASSIGN
        cCmdLine = "XCOPY " + cUpdatesDir + "\" + cPatchList + "\Deployment\Databases " + cMapDir + "\Databases"+ " /C /E /Q /Y".
    OS-COMMAND SILENT VALUE(cCmdLine).
    ASSIGN
        cCmdLine = "XCOPY " + cUpdatesDir + "\" + cPatchList + "\Deployment\Desktop " + cMapDir + "\Desktop" + " /C /E /Q /Y".
    OS-COMMAND SILENT VALUE(cCmdLine).

END PROCEDURE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDownload C-Win 
PROCEDURE ipDownload :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR l7ZExists AS LOGICAL NO-UNDO.
    
    RUN ipStatus("  Building FTP Instruction file").
    RUN ipStatus("    File: " + cFtpInstrFile).
    RUN ipStatus("    FTP Addr: " + cIpAddress).
    OUTPUT STREAM sInstr TO VALUE(cFtpInstrFile).
    PUT STREAM sInstr UNFORMATTED "OPEN " + cIpAddress SKIP.
    PUT STREAM sInstr UNFORMATTED "PROMPT " SKIP.
    PUT STREAM sInstr UNFORMATTED "USER " + cFtpUser + " " + cFtpPassword SKIP.
    PUT STREAM sInstr UNFORMATTED "LCD " + cUpdatesDir SKIP.
    PUT STREAM sInstr UNFORMATTED "MGET PATCH" + "*.7z" SKIP.
    PUT STREAM sInstr UNFORMATTED "BYE".
    OUTPUT STREAM sInstr CLOSE.
    IF SEARCH(cFtpInstrFile) EQ ? THEN DO:
        RUN ipStatus("  FTP Instr File build failed. Aborting...").
        APPLY 'choose' to bCancel IN FRAME {&FRAME-NAME}.
    END.

    ASSIGN
        bGetFiles:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
        bGetFiles:LABEL = "Downloading...".
        
    RUN ipStatus("  Starting FTP session").
    OS-COMMAND SILENT VALUE("FTP -n -s:" + cFtpInstrFile + " > " + cFtpOutputFile + " 2> " + cFtpErrFile).
    RUN ipStatus("  Checking for downloaded files").
    RUN ipGetDlList.
    IF cDLList EQ "" THEN DO:
        MESSAGE
            "The download process was unable to retrieve the patch files" SKIP
            "from the Advantzware server.  This can happen for several" SKIP
            "reasons, but is usually a problem with a firewall or anti-" SKIP
            "virus program.  Please contact Advantzware support for an" SKIP
            "alternate method of retrieving the Patch files."
            VIEW-AS ALERT-BOX INFO.
        RUN ipStatus("    No files were downloaded. Continuing...").
    END.
    ELSE DO iCtr = 1 to NUM-ENTRIES(cDLList):
        RUN ipStatus("    Downloaded file: " + ENTRY(iCtr,cDLList)).
    END.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipExpand C-Win 
PROCEDURE ipExpand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cCmdLine1 AS CHAR NO-UNDO.
    DEF VAR cFileName AS CHAR NO-UNDO.
    DEF VAR cDirName AS CHAR NO-UNDO.
    DEF VAR cCmdLine AS CHAR NO-UNDO.
    
    /* Make sure that 7z executables are in place to expand */
    OS-COPY VALUE(cUpdatesDir + "\" + "Patch" + cPatchNo + "\Deployment\Admin\EnvAdmin\7z.*")
            VALUE(cEnvAdmin).
    
    ASSIGN
        bGetFiles:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
        bGetFiles:LABEL = "Extracting...".
    RUN ipStatus("  Expanding downloaded files").
    DO iCtr = 1 TO NUM-ENTRIES(cDLList):
        RUN ipStatus("    File name: " + ENTRY(iCtr,cDLList)).
        ASSIGN
            cFileName = ENTRY(iCtr,cDLList)
            cDirName = REPLACE(cFileName,".7z","")
            cCmdLine1 = cEnvAdmin + "\7z.exe x " + 
                        cFileName + " -y -o" + 
                        cDirName.
        IF SEARCH(cFileName) NE ? THEN
            OS-COMMAND SILENT VALUE(cCmdLine1).

        IF SEARCH(cDirName + "\StartUpdate.bat") EQ ? THEN DO:
            RUN ipStatus("      Expansion failed. Aborting...").
            APPLY 'choose' to bCancel IN FRAME {&FRAME-NAME}.
        END.
        ELSE DO:
            RUN ipStatus("    Expansion succeeded").
        END.
    END.
    RUN ipStatus("  File expansion complete").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipGetDlList C-Win 
PROCEDURE ipGetDlList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cTestName AS CHAR.
    DEF VAR cLongName AS CHAR.
    DEF VAR cAttribs AS CHAR.
    
    INPUT FROM OS-DIR (cUpdatesDir).
    REPEAT:
        IMPORT 
            cTestName
            cLongName
            cAttribs.
        IF INDEX(cAttribs,"F") NE 0
        AND cTestName BEGINS "PATCH" THEN ASSIGN
            cDLList = cDLList + cLongName + ",".
    END.
    ASSIGN
        cDLList = TRIM(cDLList,",").
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipGetPatchList C-Win 
PROCEDURE ipGetPatchList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipiCallNo AS INT NO-UNDO.
    
    DEF VAR cTestName AS CHAR.
    DEF VAR cLongName AS CHAR.
    DEF VAR cAttribs AS CHAR.
    
    ASSIGN
        cPatchList = "".
    INPUT FROM OS-DIR (cUpdatesDir).
    REPEAT:
        IMPORT 
            cTestName
            cLongName
            cAttribs.

        IF INDEX(cAttribs,"F") <> 0             /* Patch .7z file */ 
        AND cTestName BEGINS "PATCH"
        AND INDEX(cTestName,".7z") <> 0 THEN
            OS-DELETE VALUE(cLongName).
        ELSE IF ipiCallNo EQ 1 
        AND INDEX(cAttribs,"D") NE 0 
        AND cTestName BEGINS "PATCH" THEN DO:   /* Remove all old directories */
            RUN ipStatus("  Removing patch " + cTestName).
            OS-DELETE VALUE(cLongName) RECURSIVE.
        END.
        ELSE IF INDEX(cAttribs,"D") NE 0        /* Should ONLY be new patch */ 
        AND cTestName BEGINS "PATCH" THEN ASSIGN
            cPatchList = cPatchList + cTestName + ",".
    END.

    ASSIGN
        cPatchList = ENTRY(1,cPatchList,",")
        iPatchEnvVer = fIntVer(REPLACE(cPatchList,"PATCH",""))
        fiToVersion:{&SV} = REPLACE(cPatchList,"PATCH","")
        iPatchDbVer  = iPatchEnvVer - (iPatchEnvVer MODULO 10000)
        .
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipProcess C-Win 
PROCEDURE ipProcess :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cConnect AS CHAR NO-UNDO.
    DEF VAR cAudDB AS CHAR NO-UNDO.
    DEF VAR cPort AS CHAR NO-UNDO.
    DEF VAR iListItem AS INT NO-UNDO.
    DEF VAR cEnvVer AS CHAR NO-UNDO.
    DEF VAR iEnv AS INT NO-UNDO.
    DEF VAR cFileToRun AS CHAR NO-UNDO.
        
    ASSIGN 
        lSuccess = TRUE.
        
    IF fiUserID:{&SV} EQ "asi" 
    AND fiPassword:{&SV} EQ "Package99" THEN ASSIGN
        iUserLevel = 10.
    ELSE IF fiUserID:{&SV} EQ "admin" 
    AND fiPassword:{&SV} EQ "installme" THEN ASSIGN
        iUserLevel = 6.
    ELSE DO:
        MESSAGE 
            "Unable to start this process with the credentials supplied."
            VIEW-AS ALERT-BOX.
        RUN ipStatus("Invalid credentials.  Abort.").
        QUIT.
    END.

    IF iCurrDbVer LT iPatchDbVer
    OR iCurrEnvVer = 16070000 THEN DO:
                
        RUN ipStatus("Database requires upgrade...").
        
        /* Needs .cfg update.  Execute, then force user to restart pgm */
        IF cState EQ "Run" THEN 
        DO:
            RUN ipStatus("  Switching progress.cfg").
            OS-RENAME VALUE (cCfgCfg) VALUE(cDLC + "\progress.run").
            OS-RENAME VALUE (cDevCfg) VALUE(cDLC + "\progress.cfg").
            MESSAGE
                "Modified configuration to support DB changes." SKIP  
                "You need to restart the update program now."
                VIEW-AS ALERT-BOX INFO.
            QUIT.
        END.
        
        ASSIGN
            iEnv = LOOKUP (slEnvList:{&SV},slEnvList:list-items)
            c-Win:visible = FALSE 
            lSuccess = FALSE 
            .

        RUN ipStatus("Initiating asiUpdateDB.w").
        
        IF SEARCH("asiUpdateDB.r") NE ? 
        OR SEARCH("asiUpdateDB.w") NE ? THEN DO:
            ASSIGN 
                cFileToRun = "asiUpdateDB.w".
        END.
        ELSE ASSIGN 
            cFileToRun = "N:\Repository\PatchTemplate\Deployment\Admin\EnvAdmin\asiUpdateDB.w".
        
        RUN  VALUE (cFileToRun) (ENTRY(iEnv,cDBList),
                                ENTRY(iEnv,cDBPortList),
                                ENTRY(iEnv,cDbDirList),
                                fiFromVersion:{&SV},
                                fiToVersion:{&SV},
                                iUserLevel,
                                OUTPUT lSuccess).
        RUN ipStatus("Return from asiUpdateDB.w").

        IF cState EQ "Dev" THEN 
        DO:
            RUN ipStatus("  Restoring original progress.cfg").
            OS-RENAME VALUE (cCfgCfg) VALUE(cDLC + "\progress.dev").
            OS-RENAME VALUE (cRunCfg) VALUE(cDLC + "\progress.cfg").
        END.

        IF lSuccess THEN DO:
            /* asiUpdateDB could change audit variables in ini file; must reread */
            RUN ipReadIniFile.
            RUN ipExpandVarNames.
            EMPTY TEMP-TABLE ttDatabases.
            DO iCtr = 1 to NUM-ENTRIES(cDbList):
                CREATE ttDatabases.
                ASSIGN
                    ttDatabases.cName = ENTRY(iCtr,cDBList)
                    ttDatabases.cDir = ENTRY(iCtr,cDbDirList)
                    ttDatabases.cPort = ENTRY(iCtr,cDBPortList)
                    ttDatabases.cVer = ENTRY(iCtr,cDBVerList)
                    ttDatabases.cAudName = ENTRY(iCtr,cAudDbList)
                    ttDatabases.cAudPort = ENTRY(iCtr,cAudPortList).
            END.
            ASSIGN 
                lMakeBackup = FALSE.
            
        END. 
        ELSE DO:
            MESSAGE 
                "The system upgrade was NOT successful.  Please" SKIP  
                "contact Advantzware technical support for assistance."
                VIEW-AS ALERT-BOX ERROR.
            QUIT.
        END.
    END.
  
    ASSIGN
        iEnv = LOOKUP (slEnvList:{&SV},slEnvList:list-items)
        .

    DO iCtr = 1 to NUM-ENTRIES(cDbList):
        CREATE ttDatabases.
        ASSIGN
            ttDatabases.cName = ENTRY(iCtr,cDBList)
            ttDatabases.cDir = ENTRY(iCtr,cDbDirList)
            ttDatabases.cPort = ENTRY(iCtr,cDBPortList)
            ttDatabases.cVer = ENTRY(iCtr,cDBVerList)
            ttDatabases.cAudName = ENTRY(iCtr,cAudDbList)
            ttDatabases.cAudPort = ENTRY(iCtr,cAudPortList).
    END.
        
    FIND FIRST ttDatabases WHERE
        ttDatabases.cName EQ ENTRY(iEnv,cDBList) AND
        ttDatabases.cPort EQ ENTRY(iEnv,cDBPortList)
        NO-ERROR.
    IF AVAIL ttDatabases THEN ASSIGN
        cAudDb = ttDatabases.cAudName
        cPort = ttDatabases.cAudPort.

    RUN ipStatus("Connecting databases").
    RUN ipStatus("  Connecting ASI DB with statement...").
    ASSIGN
        cConnect = "-db " + ENTRY(iEnv,cDBList) + 
                   " -H " + chostName +
                   " -S " + ENTRY(iEnv,cDBPortList) +
                   " -N tcp -ld ASI".
    RUN ipStatus("    " + cConnect).
    CONNECT VALUE(cConnect) NO-ERROR.
    IF NOT CONNECTED ("asi") THEN DO:
        RUN ipStatus("  DB connection failed.  Cancelling.").
        DO iCtr = 1 TO ERROR-STATUS:NUM-MESSAGES:
            RUN ipStatus ("    " + ERROR-STATUS:GET-MESSAGE(iCtr)).
        END.
        MESSAGE 
            "Unable to connect to the asi database.  Cannot continue." SKIP 
            "Please contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    IF cAudName NE "" THEN DO:
        RUN ipStatus("  Connecting Audit DB with statement...").
        ASSIGN
            cConnect = "-db " + cAudName + 
                       " -H " + chostName +
                       " -S " + cPort +
                       " -N tcp -ld Audit".
        RUN ipStatus("    " + cConnect).
        CONNECT VALUE(cConnect) NO-ERROR.
    END.
    IF NOT CONNECTED ("audit") THEN DO:
        RUN ipStatus("  DB connection failed.  Cancelling.").
        DO iCtr = 1 TO ERROR-STATUS:NUM-MESSAGES:
            RUN ipStatus ("    " + ERROR-STATUS:GET-MESSAGE(iCtr)).
        END.
        MESSAGE 
            "Unable to connect to the audit database.  Cannot continue." SKIP 
            "Please contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    IF SEARCH("asiUpdateENV.r") NE ? 
        OR SEARCH("asiUpdateENV.w") NE ? THEN ASSIGN 
            cFileToRun = "asiUpdateENV.w".
    ELSE ASSIGN 
            cFileToRun = "N:\Repository\PatchTemplate\Deployment\Admin\EnvAdmin\asiUpdateENV.w".

    RUN ipStatus("Initiating asiUpdateENV.w").
    RUN VALUE(cFileToRun) (ttDatabases.cName,
                          ttDatabases.cPort,
                          ttDatabases.cDir,
                          ttDatabases.cVer,
                          slEnvList:{&SV},
                          fiFromVersion:{&SV},
                          fiToVersion:{&SV},
                          iUserLevel,
                          lMakeBackup, /* Need backup? */
                          OUTPUT lSuccess).
    RUN ipStatus("Return from asiUpdateENV.w").

    ASSIGN
        c-Win:visible = TRUE.
   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSendVerification C-Win 
PROCEDURE ipSendVerification :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cFTPxmit AS CHAR NO-UNDO.
    
    ASSIGN
        cFTPxmit = cEnvAdmin + "\FTPout.txt".
        
    RUN ipStatus("  Building FTP Transmit file").
    RUN ipStatus("    File: " + cFTPxmit).
    RUN ipStatus("    FTP Addr: " + cIpAddress).
    
    OUTPUT STREAM sInstr TO VALUE(cFTPxmit).
    PUT STREAM sInstr UNFORMATTED "OPEN " + cIpAddress SKIP.
    PUT STREAM sInstr UNFORMATTED "PROMPT " SKIP.
    PUT STREAM sInstr UNFORMATTED "USER " + cFtpUser + " " + cFtpPassword SKIP.
    PUT STREAM sInstr UNFORMATTED "CD Results" SKIP.
    PUT STREAM sInstr UNFORMATTED "PUT " + cOutFile SKIP.
    PUT STREAM sInstr UNFORMATTED "BYE".
    OUTPUT STREAM sInstr CLOSE.
    
    IF SEARCH(cFtpxmit) EQ ? THEN DO:
        RUN ipStatus("  FTP Transmit File build failed. Aborting...").
        APPLY 'choose' to bCancel IN FRAME {&FRAME-NAME}.
    END.

    RUN ipStatus("  Starting 2d FTP session").
    OS-COMMAND SILENT VALUE("FTP -n -s:" + cFTPxmit + " >> " + cFtpOutputFile + " 2>> " + cFtpErrFile).

    /* File cleanup */
    RUN ipStatus("  Cleaning work files").
    OS-DELETE VALUE(cFTPInstrFile).
    OS-DELETE VALUE(cFTPOutputFile).
    OS-DELETE VALUE(cFTPErrFile).
    OS-DELETE VALUE(c7zOutputFile).
    OS-DELETE VALUE(c7zErrFile).
    OS-DELETE VALUE(cEnvAdmin + "\" + cOutFile).
    OS-DELETE VALUE(cFTPxmit).
    OS-DELETE VALUE(cEnvAdmin + "\cOutputFile").
   
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
                
    ASSIGN
        cLogFile = cEnvAdmin + "\UpdateLog.txt".

    IF ipcStatus = "Initialize" THEN DO:
        IF tbClearLog:CHECKED IN FRAME {&FRAME-NAME} THEN    
            OUTPUT STREAM logStream TO VALUE(cLogFile) .
        ELSE OUTPUT STREAM logStream TO VALUE(cLogFile) APPEND.

        PUT STREAM logStream UNFORMATTED "---------------------------------------------------------------" + CHR(10).
        PUT STREAM logStream UNFORMATTED "Customer Site: " + cSiteName + CHR(10).
        PUT STREAM logStream UNFORMATTED "Host Server: " + cHostName + CHR(10).
        PUT STREAM logStream UNFORMATTED "Environments: " + cEnvList + CHR(10).
        PUT STREAM logStream UNFORMATTED "Env. Versions: " + cEnvVerList + CHR(10).
        PUT STREAM logStream UNFORMATTED "Action Date: " + STRING(TODAY,"99/99/99") + CHR(10).
        PUT STREAM logStream UNFORMATTED "Action Time: " + STRING(TIME,"HH:MM:SS") + CHR(10).
        PUT STREAM logStream UNFORMATTED "---------------------------------------------------------------" + CHR(10).
        PUT STREAM logStream UNFORMATTED CHR(10).
        PUT STREAM logStream
            STRING(TODAY,"99/99/99") AT 1
            STRING(TIME,"HH:MM:SS") AT 12
            "Initializing log" FORMAT "x(160)" AT 25
            SKIP.
        OUTPUT STREAM logStream CLOSE.
        RETURN.
    END.
    ELSE DO:
        ASSIGN
            eStatus:{&SV} = eStatus:{&SV} + ipcStatus + CHR(10)
            eStatus:CURSOR-LINE = eStatus:NUM-LINES.

        IF INDEX(ipcStatus,"duplicate") EQ 0 THEN DO:
            ASSIGN
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
    END.
        
    PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipValidateChoices C-Win 
PROCEDURE ipValidateChoices :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER lOK AS LOG NO-UNDO.
    
    IF iCurrEnvVer GT iPatchEnvVer THEN DO:
        MESSAGE
            "You have chosen to apply a patch OLDER than your current version.  " +
            "This is not allowed in this version of the automated update.  " +
            "Please contact Advantzware Support for more assistance."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN
            lOK = NO.
        RETURN.
    END.
    ELSE IF iCurrEnvVer EQ iPatchEnvVer THEN DO:
        MESSAGE
            "You have chosen to apply a patch that is EQUAL to your current " +
            "version. This is allowed in this version of the automated update, " +
            "but should only be performed under certain circumstances.  Are you sure?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSure AS LOG.
        ASSIGN
            lOK = lSure.
        RETURN.
    END.
    ELSE ASSIGN
        lOK = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipValidUser C-Win 
PROCEDURE ipValidUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER oplValidUser AS LOG.
    DEF VAR cInpString AS CHAR.
    
    ASSIGN
        oplValidUser = FALSE.
        cUsrLoc = REPLACE(cIniLoc,".ini",".usr").
    
    INPUT FROM VALUE(cUsrLoc).
    REPEAT:
        IMPORT UNFORMATTED cInpString.
        IF ENTRY(2,cInpString,"|") NE "*" THEN DO:
            RUN ipStatus("  User name NOT validated. Usr file problem.").
            MESSAGE
                "Can't validate user with this version" SKIP
                "of the system. Continuing..."
                VIEW-AS ALERT-BOX.
            ASSIGN
                oplValidUser = TRUE.
            RETURN.
        END.
        IF ENTRY(1,cInpString,"|") EQ fiUserID:{&SV} THEN DO:
            RUN ipStatus("  User name validated.").
            ASSIGN
                oplValidUser = TRUE.
            RETURN.
        END.
    END.
    IF NOT oplValidUser THEN 
        RUN ipStatus("  User name failed validation.").
    
    RETURN.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fIntVer C-Win 
FUNCTION fIntVer RETURNS INTEGER
  ( INPUT cVerString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Converts a version string like "16.4.8" or "16.7.12.2" to an integer
    Notes:  In the cases above, these would be 16040800 and 16071202
            Useful for version comparisons
------------------------------------------------------------------------------*/

    DEF VAR cStrVal AS CHAR EXTENT 4 NO-UNDO.
    DEF VAR iIntVal AS INT EXTENT 4 NO-UNDO.
    DEF VAR iIntVer AS INT NO-UNDO.
    ASSIGN
        cStrVal[1] = ENTRY(1,cVerString,".")
        cStrVal[2] = ENTRY(2,cVerString,".")
        cStrVal[3] = IF NUM-ENTRIES(cVerString,".") GT 2 THEN ENTRY(3,cVerString,".") ELSE "0"
        cStrVal[4] = IF NUM-ENTRIES(cVerString,".") GT 3 THEN ENTRY(4,cVerString,".") ELSE "0"
        iIntVal[1] = INT(cStrVal[1])
        iIntVal[2] = INT(cStrVal[2])
        iIntVal[3] = INT(cStrVal[3])
        iIntVal[4] = INT(cStrVal[4])
        iIntVer = (iIntVal[1] * 1000000) + (iIntVal[2] * 10000) + (iIntVal[3] * 100) + iIntVal[4]
        NO-ERROR.
    
    RETURN iIntVer.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

