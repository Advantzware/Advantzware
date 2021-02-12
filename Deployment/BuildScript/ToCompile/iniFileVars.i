/*------------------------------------------------------------------------
    File        : iniFileVars.i
    Syntax      : {iniFileVars.i}
    Description : Common variables and procedures for advantzware.ini file usage
                  Used by asiLogin.w, asiUpdateXXX.w
    Author(s)   : MYT
    Created     : Sun Mar 31 19:26:48 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/* Ensure that these lists always match, 'c' is always the prefix */
DEFINE NEW GLOBAL SHARED VARIABLE cIniLoc AS CHAR NO-UNDO.
DEF VAR cIniVarList AS CHAR NO-UNDO.
DEF VAR cIniLine AS CHAR NO-UNDO.
DEF VAR iLine AS INT NO-UNDO.
DEF VAR lValueChanged AS LOG NO-UNDO.
DEF VAR wDbList AS CHAR NO-UNDO.
DEF VAR wDbVerList AS CHAR NO-UNDO.
DEF VAR wAudDbList AS CHAR NO-UNDO.
DEF VAR wAudPortList AS CHAR NO-UNDO.
DEF VAR wAudDirList AS CHAR NO-UNDO.
DEF VAR wAudVerList AS CHAR NO-UNDO.

ASSIGN cIniVarList =  
    "# Setup Variables,siteName,hostname,drive,dbDrive,topDir,mapDir,DLCDir,currVer,verDate,connectAudit,makeBackup,lockoutTries," +
    "# ASI Login Elements,envList,envVerList,modeList,pgmList,dbList,dbVerList,dbDirList,dbPortList,audDbList,audVerList,audDirList,audPortList," +
    "# API Elements,apiIPAddress,apiPort,adminPort,nameServerName,nameServerPort,appServerName,appServerPort," +
    "# Directory structure Variables,adminDir,backupDir,dbDir,deskDir,docDir,envDir,installDir,updatesDir," +
    "# Admin subdirs,dbAdmin,envAdmin," +
    "# Backup subdirs,dbBackup,pgmBackup,resBackup," +
    "# Database subdirs,dbAuditDir,dbDataDir,dbProdDir,dbShipDir,dbStructDir,dbTestDir," +
    "# Documentation subdirs,docMiscDocuments,docReleaseNotes,docUserManual," +
    "# Environment subdirs,envProdDir,envTestDir," +
    "# Environment inner structure,envAddonDir,envCustFiles,envCustomerDir,envOverrideDir,envPoDir,envProgramsDir,envResourceDir,envScheduleDir,envTemplateDir,envUserMenuDir,envUsersDir," + 
    "# Install subdirs,instAOA,instBackup,instDBMS,instEsko,instFileUtils,instLocalPrint,instRemAccess"
    .

/* # Setup Variables */
DEF VAR cSitename AS CHAR INITIAL "ASI" NO-UNDO.
DEF VAR cHostname AS CHAR INITIAL "HOSTNAME" NO-UNDO.
DEF VAR cDrive AS CHAR INITIAL "C:" NO-UNDO.
DEF VAR cDbDrive AS CHAR INITIAL "C:" NO-UNDO.
DEF VAR cTopDir AS CHAR INITIAL "asigui" NO-UNDO.
DEF VAR cMapDir AS CHAR INITIAL "N:" NO-UNDO.
DEF VAR cDLCDir AS CHAR INITIAL "C:\Progress\OE116" NO-UNDO.
DEF VAR cConnectAudit AS CHAR INITIAL "YES" NO-UNDO.
DEF VAR cLockoutTries AS CHAR INITIAL "4" NO-UNDO.
/* #ASI Login Items */
DEF VAR cModeList AS CHAR INITIAL "Advantzware,API Monitor,AutoLogout,CaseLabel,ClockIn,cXML Monitor,Esko Monitor,FG XML Monitor,Loadtags,Rel XML Monitor,RFID Monitor,RM ASN Tag,RMLoadtag,Schedule Board,ScheduleMonitor,Sharpshooter,Shop Floor,TaskMonitor,Touchscreen" NO-UNDO.
DEF VAR cPgmList AS CHAR INITIAL "system/mainmenu.w,api/Monitor.w,usercontrol/monitor.w,oerep/r-casetg.w,touch/clockio.p,cXML\monitor.r,jobxml\monitor.w,fgXml\monitor.w,sharpshooter/w-createloadtag.w,relxml\monitor.w,rfid\monitor.w,tagmon/monitor.w,rmrep/rmloadtg.w,schedule/sbPro.p,custom/asiSchW.w,sshoot/sshoot.w,sharpshooter/ssMenu.r,AOA/Tasker.w,touch/touchscr.w" NO-UNDO.
DEF VAR cEnvList AS CHAR INITIAL "Prod,Test" NO-UNDO.
DEF VAR cEnvVerList AS CHAR INITIAL "00.00.00,00.00.00" NO-UNDO.
DEF VAR cDbList AS CHAR INITIAL "asiProd,asiTest" NO-UNDO.
DEF VAR cDbVerList AS CHAR INITIAL "00.00.00,00.00" NO-UNDO.
DEF VAR cDbDirList AS CHAR INITIAL "Prod,Test" NO-UNDO.
DEF VAR cDbPortList AS CHAR INITIAL "2826,2827" NO-UNDO.
DEF VAR cAudDbList AS CHAR INITIAL "audProd,audTest" NO-UNDO.
DEF VAR cAudVerList AS CHAR INITIAL "00.00.00,00.00.00" NO-UNDO.
DEF VAR cAudDirList AS CHAR INITIAL "Audit,Audit" NO-UNDO.
DEF VAR cAudPortList AS CHAR INITIAL "2836,2837" NO-UNDO.
/* API Elements */
DEF VAR cAPIIPAddress AS CHAR INITIAL "0.0.0.0" NO-UNDO.
DEF VAR cAPIPort AS CHAR INITIAL "8443" NO-UNDO.
DEF VAR cAdminPort AS CHAR INITIAL "20931" NO-UNDO.
DEF VAR cNameServerName AS CHAR INITIAL "NS1" NO-UNDO.
DEF VAR cNameServerPort AS CHAR INITIAL "5162" NO-UNDO.
DEF VAR cAppServerName AS CHAR INITIAL "Advantzware_API" NO-UNDO.
DEF VAR cAppServerPort AS CHAR INITIAL "3092" NO-UNDO.
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
DEF VAR cEnvUsersDir AS CHAR INITIAL "Users" NO-UNDO.
/* # Install subdirs */
DEF VAR cInstAOA AS CHAR INITIAL "AOAInstall" NO-UNDO.
DEF VAR cInstBackup AS CHAR INITIAL "BackupInstall" NO-UNDO.
DEF VAR cInstDBMS AS CHAR INITIAL "DBMSInstall" NO-UNDO.
DEF VAR cInstEsko AS CHAR INITIAL "EskoInstall" NO-UNDO.
DEF VAR cInstFileUtils AS CHAR INITIAL "FileUtilities" NO-UNDO.
DEF VAR cInstLocalPrint AS CHAR INITIAL "LocalPrintInstall" NO-UNDO.
DEF VAR cInstRemAccess AS CHAR INITIAL "RemoteAccessInstall" NO-UNDO.
/* End of advantzware.ini variables */

DEF VAR cOutAdminPort AS CHAR NO-UNDO.
DEF VAR cOutAppServerName AS CHAR INITIAL "Advantzware_API" NO-UNDO.
DEF VAR cOutAppServerPort AS CHAR NO-UNDO.
DEF VAR cOutNameServerName AS CHAR NO-UNDO.
DEF VAR cOutNameServerPort AS CHAR NO-UNDO.
DEF VAR cDefaultModeList AS CHAR INITIAL "Advantzware,API Monitor,AutoLogout,CaseLabel,ClockIn,cXML Monitor,Esko Monitor,FG XML Monitor,Loadtags,Rel XML Monitor,RFID Monitor,RM ASN Tag,RMLoadtag,Schedule Board,ScheduleMonitor,Sharpshooter,Shop Floor,TaskMonitor,Touchscreen" NO-UNDO.
DEF VAR cDefaultPgmList AS CHAR INITIAL "system/mainmenu.w,api/Monitor.w,usercontrol/monitor.w,oerep/r-casetg.w,touch/clockio.p,cXML\monitor.r,jobxml\monitor.w,fgXml\monitor.w,sharpshooter/w-createloadtag.w,relxml\monitor.w,rfid\monitor.w,tagmon/monitor.w,rmrep/rmloadtg.w,schedule/sbPro.p,custom/asiSchW.w,sshoot/sshoot.w,sharpshooter/ssMenu.r,AOA/Tasker.w,touch/touchscr.w" NO-UNDO.

/* END advantzware.ini Variables */

DEF TEMP-TABLE ttIniFile
    FIELD iPos AS INT
    FIELD cRaw AS CHAR
    FIELD cVarName AS CHAR
    FIELD cVarValue AS CHAR
    INDEX idxPos IS PRIMARY UNIQUE iPos.

/* Create the temp-table and load var names */
EMPTY TEMP-TABLE ttIniFile.
DO iLine = 1 TO NUM-ENTRIES(cIniVarList):
    CREATE ttIniFile.
    ASSIGN
        ttIniFile.iPos = iLine
        ttIniFile.cVarName = ENTRY(iLine,cIniVarList).
END.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE ipFindIniFile:
/*------------------------------------------------------------------------------
 Purpose:   Locate the advantzware.ini file somewhere in the file structure
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileTest AS CHAR.
    DEF OUTPUT PARAMETER opcFileLoc AS CHAR.

    DEF VAR cLetters AS CHAR NO-UNDO INITIAL "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
    DEF VAR cEnt AS CHAR NO-UNDO.
    DEF VAR cTest AS CHAR NO-UNDO.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR cExt AS CHAR NO-UNDO.
    
    ASSIGN
        cTest = ipcFileTest
        cExt = SUBSTRING(cTest,LENGTH(cTest) - 2,3).

    /* Start guessing where the file might be */
    SEARCHBLOCK:
    DO:
        IF SEARCH(cTest) <> ? THEN DO:
            ASSIGN
                opcFileLoc = SEARCH(cTest).
            LEAVE SEARCHBLOCK.
        END.
        ELSE ASSIGN
            cTest = "..\advantzware." + cExt.
        IF SEARCH(cTest) <> ? THEN DO:
            ASSIGN
                opcFileLoc = SEARCH(cTest).
            LEAVE SEARCHBLOCK.
        END.
    
        ASSIGN
            cTest = "N:\Admin\advantzware" + cExt.
        IF SEARCH(cTest) <> ? THEN DO:
            ASSIGN
                opcFileLoc = SEARCH(cTest).
            LEAVE SEARCHBLOCK.
        END.

        DO iCtr = 1 TO LENGTH(cLetters):
            ASSIGN 
                cEnt = SUBSTRING(cLetters,iCtr,1)
                cTest = cEnt + ":\Admin\advantzware." + cExt.
            IF SEARCH(cTest) <> ? THEN DO:
                ASSIGN
                    opcFileLoc = SEARCH(cTest).
                LEAVE SEARCHBLOCK.
            END.
        END.
        
        DO iCtr = 1 TO LENGTH(cLetters):
            ASSIGN 
                cEnt = SUBSTRING(cLetters,iCtr,1)
                cTest = cEnt + ":\asigui\Admin\advantzware" + cExt.
            IF SEARCH(cTest) <> ? THEN DO:
                ASSIGN
                    opcFileLoc = SEARCH(cTest).
                LEAVE SEARCHBLOCK.
            END.
        END.

        DO iCtr = 1 TO LENGTH(cLetters):
            ASSIGN 
                cEnt = SUBSTRING(cLetters,iCtr,1)
                cTest = cEnt + ":\asi\Admin\advantzware." + cExt.
            IF SEARCH(cTest) <> ? THEN DO:
                ASSIGN
                    opcFileLoc = SEARCH(cTest).
                LEAVE SEARCHBLOCK.
            END.
        END.

        ASSIGN
            opcFileLoc = "".
    END.


END PROCEDURE.

PROCEDURE ipGetPropertyValues:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER opcOutAdminPort AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER opcOutAppServerName AS CHAR INITIAL "Advantzware_API" NO-UNDO.
    DEF OUTPUT PARAMETER opcOutAppServerPort AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER opcOutNameServerName AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER opcOutNameServerPort AS CHAR NO-UNDO.
    
    DEF VAR cTestLine AS CHAR NO-UNDO.
    DEF VAR lInSection AS LOG NO-UNDO.
    DEF VAR ctDLC AS CHAR NO-UNDO.
    
    /* Use this if cDLC shared var is not yet set */
    DEF BUFFER bttIniFile FOR ttIniFile.
    FIND FIRST bttIniFile WHERE 
        bttIniFile.cVarName EQ "DLCdir"
        NO-ERROR.
    IF AVAIL bttIniFile THEN ASSIGN  
        ctDLC = bttIniFile.cVarValue.
    ELSE ASSIGN 
        ctDLC = "c:\Progress\OE116".
        
    /* Get the AdminService Port number */
    INPUT FROM VALUE(ctDLC + "\Properties\AdminServerPlugins.properties").
    ASSIGN 
        lInSection = FALSE.
    testLoop1:
    REPEAT:
        IMPORT cTestLine.
        IF cTestLine BEGINS "[PluginPolicy.Progress.AdminServer]" THEN ASSIGN 
            lInSection = TRUE.
        IF lInSection 
        AND TRIM(cTestLine) BEGINS "Port=" THEN DO:
            ASSIGN 
                opcOutAdminPort = ENTRY(2,trim(cTestLine),"=").
            LEAVE testLoop1.
        END.
    END. 
    INPUT CLOSE.

    /* Get the AppServer Port number and the controlling NameServer Name */
    INPUT FROM VALUE(ctDLC + "\Properties\ubroker.properties").
    ASSIGN 
        lInSection = FALSE.
    testLoop2:
    REPEAT:
        IMPORT cTestLine.
        IF cTestLine BEGINS "[UBroker.AS." + opcOutAppServerName THEN ASSIGN 
            lInSection = TRUE.
        IF lInSection 
        AND TRIM(cTestLine) BEGINS "portNumber=" THEN ASSIGN 
            opcOutAppServerPort = ENTRY(2,trim(cTestLine),"=").
        IF lInSection 
        AND TRIM(cTestLine) BEGINS "controllingNameServer=" THEN ASSIGN 
            opcOutNameServerName = ENTRY(2,trim(cTestLine),"=").
        IF opcOutAppServerPort NE ""
        AND opcOutNameServerName NE "" THEN 
            LEAVE testLoop2.
    END. 
    INPUT CLOSE.
    IF opcOutNameServerName EQ "" THEN ASSIGN 
        opcOutNameServerName = "NS1".
    IF opcOutAppServerPort EQ "" THEN ASSIGN 
        opcOutAppServerPort = "3092".

    /* Get the appropriate NameServer Port number */
    INPUT FROM VALUE(ctDLC + "\Properties\ubroker.properties").
    ASSIGN 
        lInSection = FALSE.
    testLoop3:
    REPEAT:
        IMPORT cTestLine.
        IF cTestLine BEGINS "[NameServer." + opcOutNameServerName THEN ASSIGN 
            lInSection = TRUE.
        IF lInSection 
        AND TRIM(cTestLine) BEGINS "portNumber=" THEN ASSIGN 
            opcOutNameServerPort = ENTRY(2,trim(cTestLine),"=").
        IF lInSection 
        AND TRIM(cTestLine) BEGINS "[" THEN
            LEAVE testLoop3. 
    END.
    IF opcOutNameServerPort EQ "" THEN ASSIGN 
        opcOutNameServerPort = "5162".
    INPUT CLOSE.

END PROCEDURE.

PROCEDURE ipReadIniFile:
/*------------------------------------------------------------------------------
 Purpose:   Read the advantzware.ini file and populate values
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR cAPIList AS CHAR INITIAL "adminPort,nameServerName,nameServerPort,appServerName,appServerPort" NO-UNDO.
    DEF VAR iCtr AS INT NO-UNDO.
    
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
            /* Setup Variables */
            WHEN "siteName" THEN ASSIGN         cSiteName           = ttIniFile.cVarValue.
            WHEN "hostname" THEN ASSIGN         cHostname           = ttIniFile.cVarValue.
            WHEN "drive" THEN ASSIGN            cDrive              = ttIniFile.cVarValue.
            WHEN "dbDrive" THEN ASSIGN          cDbDrive            = ttIniFile.cVarValue.
            WHEN "topDir" THEN ASSIGN           cTopDir             = ttIniFile.cVarValue.
            WHEN "mapDir" THEN ASSIGN           cMapDir             = ttIniFile.cVarValue.
            WHEN "DLCDir" THEN ASSIGN           cDLCDir             = ttIniFile.cVarValue.
            WHEN "connectAudit" THEN ASSIGN     cConnectAudit       = ttIniFile.cVarValue.
            WHEN "lockoutTries" THEN ASSIGN     cLockoutTries       = ttIniFile.cVarValue.
            /* Directory structure variables */
            WHEN "adminDir" THEN ASSIGN         cAdminDir           = ttIniFile.cVarValue.
            WHEN "backupDir" THEN ASSIGN        cBackupDir          = ttIniFile.cVarValue.
            WHEN "dbDir" THEN ASSIGN            cDbDir              = ttIniFile.cVarValue.
            WHEN "deskDir" THEN ASSIGN          cDeskDir            = ttIniFile.cVarValue.
            WHEN "docDir" THEN ASSIGN           cDocDir             = ttIniFile.cVarValue.
            WHEN "envDir" THEN ASSIGN           cEnvDir             = ttIniFile.cVarValue.
            WHEN "installDir" THEN ASSIGN       cInstallDir         = ttIniFile.cVarValue.
            WHEN "updatesDir" THEN ASSIGN       cUpdatesDir         = ttIniFile.cVarValue.
            /* Admin subdirs */
            WHEN "dbAdmin" THEN ASSIGN          cDbAdmin            = ttIniFile.cVarValue.
            WHEN "envAdmin" THEN ASSIGN         cEnvAdmin           = ttIniFile.cVarValue.
            /* Backup subdirs */
            WHEN "dbBackup" THEN ASSIGN         cDbBackup           = ttIniFile.cVarValue.
            WHEN "pgmBackup" THEN ASSIGN        cPgmBackup          = ttIniFile.cVarValue.
            WHEN "resBackup" THEN ASSIGN        cResBackup          = ttIniFile.cVarValue.
            /* Database subdirs */
            WHEN "dbAuditDir" THEN ASSIGN       cDbAuditDir         = ttIniFile.cVarValue.
            WHEN "dbDataDir" THEN ASSIGN        cDbDataDir          = ttIniFile.cVarValue.
            WHEN "dbProdDir" THEN ASSIGN        cDbProdDir          = ttIniFile.cVarValue.
            WHEN "dbShipDir" THEN ASSIGN        cDbShipDir          = ttIniFile.cVarValue.
            WHEN "dbStructDir" THEN ASSIGN      cDbStructDir        = ttIniFile.cVarValue.
            WHEN "dbTestDir" THEN ASSIGN        cDbTestDir          = ttIniFile.cVarValue.
            /* Documentation subdirs */
            WHEN "docMiscDocuments" THEN ASSIGN cDocMiscDocuments   = ttIniFile.cVarValue.
            WHEN "docReleaseNotes" THEN ASSIGN  cDocReleaseNotes    = ttIniFile.cVarValue.
            WHEN "docUserManual" THEN ASSIGN    cDocUserManual      = ttIniFile.cVarValue.
            /* Basic environment subdirs */
            WHEN "envProdDir" THEN ASSIGN       cEnvProdDir         = ttIniFile.cVarValue.
            WHEN "envTestDir" THEN ASSIGN       cEnvTestDir         = ttIniFile.cVarValue.
            /* Per-environment subdirs */
            WHEN "envAddonDir" THEN ASSIGN      cEnvAddonDir        = ttIniFile.cVarValue.
            WHEN "envCustFiles" THEN ASSIGN     cEnvCustFiles       = ttIniFile.cVarValue.
            WHEN "envCustomerDir" THEN ASSIGN   cEnvCustomerDir     = ttIniFile.cVarValue.
            WHEN "envOverrideDir" THEN ASSIGN   cEnvOverrideDir     = ttIniFile.cVarValue.
            WHEN "envPoDir" THEN ASSIGN         cEnvPoDir           = ttIniFile.cVarValue.
            WHEN "envProgramsDir" THEN ASSIGN   cEnvProgramsDir     = ttIniFile.cVarValue.
            WHEN "envResourceDir" THEN ASSIGN   cEnvResourceDir     = ttIniFile.cVarValue.
            WHEN "envScheduleDir" THEN ASSIGN   cEnvScheduleDir     = ttIniFile.cVarValue.
            WHEN "envUsersDir" THEN ASSIGN      cEnvUsersDir        = ttIniFile.cVarValue.
            /* Install subdirs */
            WHEN "instAOA" THEN ASSIGN          cInstAOA            = ttIniFile.cVarValue.
            WHEN "instBackup" THEN ASSIGN       cInstBackup         = ttIniFile.cVarValue.
            WHEN "instDBMS" THEN ASSIGN         cInstDBMS           = ttIniFile.cVarValue.
            WHEN "instEsko" THEN ASSIGN         cInstEsko           = ttIniFile.cVarValue.
            WHEN "instFileUtils" THEN ASSIGN    cInstFileUtils      = ttIniFile.cVarValue.
            WHEN "instLocalPrint" THEN ASSIGN   cInstLocalPrint     = ttIniFile.cVarValue.
            WHEN "instRemAccess" THEN ASSIGN    cInstRemAccess      = ttIniFile.cVarValue.
            /* ASI Login elements */
            WHEN "envList" THEN ASSIGN          cEnvList            = ttIniFile.cVarValue.
            WHEN "envVerList" THEN ASSIGN       cEnvVerList         = ttIniFile.cVarValue.
            WHEN "modeList" THEN ASSIGN         cModeList           = ttIniFile.cVarValue.
            WHEN "pgmList" THEN ASSIGN          cPgmList            = ttIniFile.cVarValue.
            WHEN "dbList" THEN ASSIGN           cDbList             = ttIniFile.cVarValue.
            WHEN "dbVerList" THEN ASSIGN        cDbVerList          = ttIniFile.cVarValue.
            WHEN "dbDirList" THEN ASSIGN        cDbDirList          = ttIniFile.cVarValue.
            WHEN "dbPortList" THEN ASSIGN       cDbPortList         = ttIniFile.cVarValue.
            WHEN "audDbList" THEN ASSIGN        cAudDbList          = ttIniFile.cVarValue.
            WHEN "audVerList" THEN ASSIGN       cAudVerList         = ttIniFile.cVarValue.
            WHEN "audDirList" THEN ASSIGN       cAudDirList         = ttIniFile.cVarValue.
            WHEN "audPortList" THEN ASSIGN      cAudPortList        = ttIniFile.cVarValue.
            /* API Elements */
            WHEN "apiIPAddress" THEN ASSIGN     cAPIIpAddress       = ttIniFile.cVarValue.
            WHEN "apiPort" THEN ASSIGN          cAPIPort            = ttIniFile.cVarValue.
            WHEN "adminPort" THEN ASSIGN        cAdminPort          = ttIniFile.cVarValue.
            WHEN "nameServerName" THEN ASSIGN   cNameServerName     = ttIniFile.cVarValue.
            WHEN "nameServerPort" THEN ASSIGN   cNameServerPort     = ttIniFile.cVarValue.
            WHEN "appServerName" THEN ASSIGN    cAppServerName      = ttIniFile.cVarValue.
            WHEN "appServerPort" THEN ASSIGN    cAppServerPort      = ttIniFile.cVarValue.
        END CASE.
    END.
    
    /* Are we on the server? If we are, validate the API elements */
    IF SEARCH(cDlcDir + "\bin\prowin.exe") NE ? THEN DO:  
        /* Reset API values from DLC properties pages */
        RUN ipGetPropertyValues (
            OUTPUT cOutAdminPort,
            OUTPUT cOutAppServerName,
            OUTPUT cOutAppServerPort,
            OUTPUT cOutNameServerName,
            OUTPUT cOutNameServerPort
            ).
        DO iCtr = 1 TO NUM-ENTRIES(cAPIList):
            FIND ttIniFile WHERE 
                ttIniFile.cVarName EQ ENTRY(iCtr,cAPIList)
                NO-ERROR.
            CASE ttIniFile.cVarName:
                WHEN "adminPort" THEN DO:
                    IF ttIniFile.cVarValue NE cOutAdminPort THEN ASSIGN 
                        ttIniFile.cVarValue = cOutAdminPort 
                        lValueChanged = TRUE.
                END.
                WHEN "nameServerName" THEN DO: 
                    IF ttIniFile.cVarValue NE cOutNameServerName THEN ASSIGN 
                        ttIniFile.cVarValue = cOutNameServerName 
                        lValueChanged = TRUE.
                END.
                WHEN "nameServerPort" THEN DO: 
                    IF ttIniFile.cVarValue NE cOutNameServerPort THEN ASSIGN 
                        ttIniFile.cVarValue = cOutNameServerPort
                        lValueChanged = TRUE.
                END.
                WHEN "appServerName" THEN DO: 
                    IF ttIniFile.cVarValue NE cOutAppServerName THEN ASSIGN 
                        ttIniFile.cVarValue = cOutAppServerName
                        lValueChanged = TRUE.
                END.
                WHEN "appServerPort" THEN DO: 
                    IF ttIniFile.cVarValue NE cOutAppServerPort THEN ASSIGN 
                        ttIniFile.cVarValue = cOutAppServerPort
                        lValueChanged = TRUE.
                END.
            END.
        END.
    END.             

    /* At v21.00.02, made changes to advantzware.ini structure.  If current file is at previous level, write it back out */
    /* This group is new or renamed lines, in particular, line group names (#) */
    FOR EACH ttIniFile WHERE 
        ttIniFile.cRaw EQ "":
        ASSIGN 
            ttIniFile.cRaw = ttIniFile.cVarName
            lValueChanged = TRUE.
    END.
    FIND FIRST ttIniFile WHERE 
        ttIniFile.cVarName EQ "modeList"
        NO-ERROR.
    IF AVAIL ttIniFile 
    AND ttIniFile.cVarValue NE cDefaultModeList THEN ASSIGN 
        ttIniFile.cVarValue = cDefaultModeList
        lValueChanged = TRUE.
    FIND FIRST ttIniFile WHERE 
        ttIniFile.cVarName EQ "pgmList"
        NO-ERROR.
    IF AVAIL ttIniFile 
    AND ttIniFile.cVarValue NE cDefaultPgmList THEN ASSIGN 
        ttIniFile.cVarValue = cDefaultPgmList
        lValueChanged = TRUE.
    
    /* Finally, if we made any changes to the ttIniFile, write it back out */
    IF lValueChanged THEN 
        RUN ipWriteIniFile.

END PROCEDURE.

PROCEDURE ipWriteIniFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cThisElement AS CHAR NO-UNDO.

    IF LOOKUP("ipStatus",THIS-PROCEDURE:INTERNAL-ENTRIES) NE 0 THEN
        RUN ipStatus ("Writing advantzware.ini file..."). 

    /* This can happen during an update when the environment version changes */
    IF wDbVerList NE "" THEN DO:
        FIND ttIniFile WHERE ttIniFile.cVarName = "dbVerList" NO-ERROR.
        ASSIGN 
            ttIniFile.cVarValue = wDbVerList.
    END.    
    IF wAudVerList NE "" THEN DO:
        FIND ttIniFile WHERE ttIniFile.cVarName = "audVerList" NO-ERROR.
        ASSIGN 
            ttIniFile.cVarValue = wAudVerList.
    END. 
    
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

