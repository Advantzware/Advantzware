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
    "# Filestructure Variables,adminDir,backupDir,dbDir,deskDir,docDir,envDir,installDir,updatesDir," +
    "# Admin subdirs,dbAdmin,envAdmin," +
    "# Backup subdirs,dbBackup,pgmBackup,resBackup," +
    "# Database subdirs,dbAuditDir,dbDataDir,dbProdDir,dbShipDir,dbStructDir,dbTestDir," +
    "# Documentation subdirs,docMiscDocuments,docReleaseNotes,docUserManual," +
    "# Environment subdirs,envProdDir,envTestDir," +
    "# Environment inner structure,envAddonDir,envCustFiles,envCustomerDir,envOverrideDir,envPoDir,envProgramsDir,envResourceDir,envScheduleDir,envTemplateDir,envUserMenuDir,envUsersDir," + 
    "# Install subdirs,instAOA,instBackup,instDBMS,instEsko,instFileUtils,instLocalPrint,instRemAccess," +
    "# Updates subdirs,updAdminDir,updCompressDir,updDataDir,updDataUpdateDir,updDeskDir,updMenuDir,updProgramDir,updRelNotesDir,updSqlDir,updStructureDir," +
    "# ASI Login Items,modeList,pgmList," +
    "# Environment List,envList,envVerList," +
    "# Database List,dbList,dbVerList,dbDirList,dbPortList," +
    "# Audit DB List,audDbList,audVerList,audDirList,audPortList," +
    "# Basic DB Elements,audDbName,audDbPort,audDbStFile,prodDbName,prodDbPort,prodDbStFile,shipDbName,shipDbPort,shipDbStFile,testDbName,testDbPort,testDbStFile," +
    "# API Elements,apiIPAddress,apiPort,adminPort,nameServerName,nameServerPort,appServerName,appServerPort," +
    "# Misc Elements,dfFileName,deltaFileName".

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
DEF VAR cPgmList AS CHAR INITIAL "system/mainmenu.w,system/addmain.w,oerep/r-casetg.w,custom/asiSchW.w,_edit.p,jobxml\monitor.w,fgXml\monitor.w,oerep/r-loadtg.w,proshut.bat,relxml\monitor.w,rfid\monitor.w,rmrep/rmloadtg.w,sshoot/sshoot.w,touch/touchscr.w" NO-UNDO.
/* # Environment List Items */
DEF VAR cEnvList AS CHAR INITIAL "Prod" NO-UNDO.
DEF VAR cEnvVerList AS CHAR INITIAL "16.00.00" NO-UNDO.
/* # Database List Items */
DEF VAR cDbList AS CHAR INITIAL "asiProd" NO-UNDO.
DEF VAR cDbVerList AS CHAR INITIAL "16.00.00" NO-UNDO.
DEF VAR cDbDirList AS CHAR INITIAL "Prod" NO-UNDO.
DEF VAR cDbPortList AS CHAR INITIAL "2826" NO-UNDO.
/* AuditDB List Items */
DEF VAR cAudDbList AS CHAR INITIAL "audProd" NO-UNDO.
DEF VAR cAudVerList AS CHAR INITIAL "16.00.00" NO-UNDO.
DEF VAR cAudDirList AS CHAR INITIAL "Audit" NO-UNDO.
DEF VAR cAudPortList AS CHAR INITIAL "2836" NO-UNDO.
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
/* API Elements */
DEF VAR cAPIIPAddress AS CHAR INITIAL "0.0.0.0" NO-UNDO.
DEF VAR cAPIPort AS CHAR INITIAL "8443" NO-UNDO.
DEF VAR cAdminPort AS CHAR INITIAL "20931" NO-UNDO.
DEF VAR cNameServerName AS CHAR INITIAL "NS1" NO-UNDO.
DEF VAR cNameServerPort AS CHAR INITIAL "5162" NO-UNDO.
DEF VAR cAppServerName AS CHAR INITIAL "Advantzware_API" NO-UNDO.
DEF VAR cAppServerPort AS CHAR INITIAL "3092" NO-UNDO.
/* # Misc Elements */
DEF VAR cDfFileName AS CHAR INITIAL "asi167.df" NO-UNDO.
DEF VAR cDeltaFileName AS CHAR INITIAL "asi166167.df" NO-UNDO.

DEF VAR cOutAdminPort AS CHAR NO-UNDO.
DEF VAR cOutAppServerName AS CHAR INITIAL "Advantzware_API" NO-UNDO.
DEF VAR cOutAppServerPort AS CHAR NO-UNDO.
DEF VAR cOutNameServerName AS CHAR NO-UNDO.
DEF VAR cOutNameServerPort AS CHAR NO-UNDO.

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
    
    /* This lets the advantzware.ini file "heal" itself when changes made to program but not local */
    FOR EACH ttIniFile WHERE
        NOT ttIniFile.cVarName BEGINS "#" AND
        NOT ttIniFile.cVarName EQ "" AND
        NOT ttIniFile.cVarName EQ "audVerList" AND 
        ttIniFile.cVarValue = "":
    
        /* We'll test for these after we set the local variables, and check for running on server */
        IF CAN-DO(cAPIList,ttIniFile.cVarName) THEN NEXT.
        
        IF ttIniFile.cVarValue EQ "" THEN DO:
            ASSIGN 
                lValueChanged = TRUE.
            CASE ttIniFile.cVarName:
                WHEN "siteName" THEN ASSIGN          ttIniFile.cVarValue = cSiteName.
                WHEN "hostname" THEN ASSIGN          ttIniFile.cVarValue = cHostname.
                WHEN "drive" THEN ASSIGN             ttIniFile.cVarValue = cDrive.
                WHEN "dbDrive" THEN ASSIGN           ttIniFile.cVarValue = cDbDrive.
                WHEN "topDir" THEN ASSIGN            ttIniFile.cVarValue = cTopDir.
                WHEN "mapDir" THEN ASSIGN            ttIniFile.cVarValue = cMapDir.
                WHEN "DLCDir" THEN ASSIGN            ttIniFile.cVarValue = cDLCDir.
                WHEN "currVer" THEN ASSIGN           ttIniFile.cVarValue = cCurrVer.
                WHEN "verDate" THEN ASSIGN           ttIniFile.cVarValue = cVerDate.
                WHEN "connectAudit" THEN ASSIGN      ttIniFile.cVarValue = cConnectAudit.
                WHEN "makeBackup" THEN ASSIGN        ttIniFile.cVarValue = cMakeBackup.
                WHEN "lockoutTries" THEN ASSIGN      ttIniFile.cVarValue = cLockoutTries.
                WHEN "adminDir" THEN ASSIGN          ttIniFile.cVarValue = cAdminDir.
                WHEN "backupDir" THEN ASSIGN         ttIniFile.cVarValue = cBackupDir.
                WHEN "dbDir" THEN ASSIGN             ttIniFile.cVarValue = cDbDir.
                WHEN "deskDir" THEN ASSIGN           ttIniFile.cVarValue = cDeskDir.
                WHEN "docDir" THEN ASSIGN            ttIniFile.cVarValue = cDocDir.
                WHEN "envDir" THEN ASSIGN            ttIniFile.cVarValue = cEnvDir.
                WHEN "installDir" THEN ASSIGN        ttIniFile.cVarValue = cInstallDir.
                WHEN "updatesDir" THEN ASSIGN        ttIniFile.cVarValue = cUpdatesDir.
                WHEN "dbAdmin" THEN ASSIGN           ttIniFile.cVarValue = cDbAdmin.
                WHEN "envAdmin" THEN ASSIGN          ttIniFile.cVarValue = cEnvAdmin.
                WHEN "dbBackup" THEN ASSIGN          ttIniFile.cVarValue = cDbBackup.
                WHEN "pgmBackup" THEN ASSIGN         ttIniFile.cVarValue = cPgmBackup.
                WHEN "resBackup" THEN ASSIGN         ttIniFile.cVarValue = cResBackup.
                WHEN "dbAuditDir" THEN ASSIGN        ttIniFile.cVarValue = cDbAuditDir.
                WHEN "dbDataDir" THEN ASSIGN         ttIniFile.cVarValue = cDbDataDir.
                WHEN "dbProdDir" THEN ASSIGN         ttIniFile.cVarValue = cDbProdDir.
                WHEN "dbShipDir" THEN ASSIGN         ttIniFile.cVarValue = cDbShipDir.
                WHEN "dbStructDir" THEN ASSIGN       ttIniFile.cVarValue = cDbStructDir.
                WHEN "dbTestDir" THEN ASSIGN         ttIniFile.cVarValue = cDbTestDir.
                WHEN "docMiscDocuments" THEN ASSIGN  ttIniFile.cVarValue = cDocMiscDocuments.
                WHEN "docReleaseNotes" THEN ASSIGN   ttIniFile.cVarValue = cDocReleaseNotes.
                WHEN "docUserManual" THEN ASSIGN     ttIniFile.cVarValue = cDocUserManual.
                WHEN "envProdDir" THEN ASSIGN        ttIniFile.cVarValue = cEnvProdDir.
                WHEN "envTestDir" THEN ASSIGN        ttIniFile.cVarValue = cEnvTestDir.
                WHEN "envAddonDir" THEN ASSIGN       ttIniFile.cVarValue = cEnvAddonDir.
                WHEN "envCustFiles" THEN ASSIGN      ttIniFile.cVarValue = cEnvCustFiles.
                WHEN "envCustomerDir" THEN ASSIGN    ttIniFile.cVarValue = cEnvCustomerDir.
                WHEN "envOverrideDir" THEN ASSIGN    ttIniFile.cVarValue = cEnvOverrideDir.
                WHEN "envPoDir" THEN ASSIGN          ttIniFile.cVarValue = cEnvPoDir.
                WHEN "envProgramsDir" THEN ASSIGN    ttIniFile.cVarValue = cEnvProgramsDir.
                WHEN "envResourceDir" THEN ASSIGN    ttIniFile.cVarValue = cEnvResourceDir.
                WHEN "envScheduleDir" THEN ASSIGN    ttIniFile.cVarValue = cEnvScheduleDir.
                WHEN "envTemplateDir" THEN ASSIGN    ttIniFile.cVarValue = cEnvTemplateDir.
                WHEN "envUserMenuDir" THEN ASSIGN    ttIniFile.cVarValue = cEnvUserMenuDir.
                WHEN "envUsersDir" THEN ASSIGN       ttIniFile.cVarValue = cEnvUsersDir.
                WHEN "instAOA" THEN ASSIGN           ttIniFile.cVarValue = cInstAOA.
                WHEN "instBackup" THEN ASSIGN        ttIniFile.cVarValue = cInstBackup.
                WHEN "instDBMS" THEN ASSIGN          ttIniFile.cVarValue = cInstDBMS.
                WHEN "instEsko" THEN ASSIGN          ttIniFile.cVarValue = cInstEsko.
                WHEN "instFileUtils" THEN ASSIGN     ttIniFile.cVarValue = cInstFileUtils.
                WHEN "instLocalPrint" THEN ASSIGN    ttIniFile.cVarValue = cInstLocalPrint.
                WHEN "instRemAccess" THEN ASSIGN     ttIniFile.cVarValue = cInstRemAccess.
                WHEN "updAdminDir" THEN ASSIGN       ttIniFile.cVarValue = cUpdAdminDir.
                WHEN "updCompressDir" THEN ASSIGN    ttIniFile.cVarValue = cUpdCompressDir.
                WHEN "updDataDir" THEN ASSIGN        ttIniFile.cVarValue = cUpdDataDir.
                WHEN "updDataUpdateDir" THEN ASSIGN  ttIniFile.cVarValue = cUpdDataUpdateDir.
                WHEN "updDeskDir" THEN ASSIGN        ttIniFile.cVarValue = cUpdDeskDir.
                WHEN "updMenuDir" THEN ASSIGN        ttIniFile.cVarValue = cUpdMenuDir.
                WHEN "updProgramDir" THEN ASSIGN     ttIniFile.cVarValue = cUpdProgramDir.
                WHEN "updRelNotesDir" THEN ASSIGN    ttIniFile.cVarValue = cUpdRelNotesDir.
                WHEN "updSqlDir" THEN ASSIGN         ttIniFile.cVarValue = cUpdSqlDir.
                WHEN "updStructureDir" THEN ASSIGN   ttIniFile.cVarValue = cUpdStructureDir.
                WHEN "modeList" THEN ASSIGN          ttIniFile.cVarValue = cModeList.
                WHEN "envList" THEN ASSIGN           ttIniFile.cVarValue = cEnvList.
                WHEN "dbList" THEN ASSIGN            ttIniFile.cVarValue = cDbList.
                WHEN "pgmList" THEN ASSIGN           ttIniFile.cVarValue = cPgmList.
                WHEN "dbDirList" THEN ASSIGN         ttIniFile.cVarValue = cDbDirList.
                WHEN "dbPortList" THEN ASSIGN        ttIniFile.cVarValue = cDbPortList.
                WHEN "audDirList" THEN ASSIGN        ttIniFile.cVarValue = cAudDirList.
                WHEN "audDbList" THEN ASSIGN         ttIniFile.cVarValue = cAudDbList.
                WHEN "audPortList" THEN ASSIGN       ttIniFile.cVarValue = cAudPortList.
                WHEN "envVerList" THEN ASSIGN        ttIniFile.cVarValue = cEnvVerList.
                WHEN "dbVerList" THEN ASSIGN         ttIniFile.cVarValue = cDbVerList.
                WHEN "audVerList" THEN ASSIGN        ttIniFile.cVarValue = cAudVerList.
                WHEN "prodDbName" THEN ASSIGN        ttIniFile.cVarValue = cProdDbName.
                WHEN "prodDbPort" THEN ASSIGN        ttIniFile.cVarValue = cProdDbPort.
                WHEN "prodDbStFile" THEN ASSIGN      ttIniFile.cVarValue = cProdDbStFile.
                WHEN "shipDbName" THEN ASSIGN        ttIniFile.cVarValue = cShipDbName.
                WHEN "shipDbPort" THEN ASSIGN        ttIniFile.cVarValue = cShipDbPort.
                WHEN "shipDbStFile" THEN ASSIGN      ttIniFile.cVarValue = cShipDbStFile.
                WHEN "testDbName" THEN ASSIGN        ttIniFile.cVarValue = cTestDbName.
                WHEN "testDbPort" THEN ASSIGN        ttIniFile.cVarValue = cTestDbPort.
                WHEN "testDbStFile" THEN ASSIGN      ttIniFile.cVarValue = cTestDbStFile.
                WHEN "dfFileName" THEN ASSIGN        ttIniFile.cVarValue = cDfFileName.
                WHEN "deltaFileName" THEN ASSIGN     ttIniFile.cVarValue = cDeltaFileName.
                WHEN "apiIPAddress" THEN ASSIGN      ttIniFile.cVarValue = cAPIIpAddress.
                WHEN "apiPort" THEN ASSIGN           ttIniFile.cVarValue = cAPIPort.
                WHEN "adminPort" THEN ASSIGN         ttIniFile.cVarValue = cAdminPort.
                WHEN "nameServerName" THEN ASSIGN    ttIniFile.cVarValue = cNameServerName.
                WHEN "nameServerPort" THEN ASSIGN    ttIniFile.cVarValue = cNameServerPort.
                WHEN "appServerName" THEN ASSIGN     ttIniFile.cVarValue = cAppServerName.
                WHEN "appServerPort" THEN ASSIGN     ttIniFile.cVarValue = cAppServerPort.
            END CASE.                                                      
        END.
    END. 
    
    FOR EACH ttIniFile:
        CASE ttIniFile.cVarName:
            WHEN "siteName" THEN ASSIGN         cSiteName           = ttIniFile.cVarValue.
            WHEN "hostname" THEN ASSIGN         cHostname           = ttIniFile.cVarValue.
            WHEN "drive" THEN ASSIGN            cDrive              = ttIniFile.cVarValue.
            WHEN "dbDrive" THEN ASSIGN          cDbDrive            = ttIniFile.cVarValue.
            WHEN "topDir" THEN ASSIGN           cTopDir             = ttIniFile.cVarValue.
            WHEN "mapDir" THEN ASSIGN           cMapDir             = ttIniFile.cVarValue.
            WHEN "DLCDir" THEN ASSIGN           cDLCDir             = ttIniFile.cVarValue.
            WHEN "currVer" THEN ASSIGN          cCurrVer            = ttIniFile.cVarValue.
            WHEN "verDate" THEN ASSIGN          cVerDate            = ttIniFile.cVarValue.
            WHEN "connectAudit" THEN ASSIGN     cConnectAudit       = ttIniFile.cVarValue.
            WHEN "makeBackup" THEN ASSIGN       cMakeBackup         = ttIniFile.cVarValue.
            WHEN "lockoutTries" THEN ASSIGN     cLockoutTries       = ttIniFile.cVarValue.
            WHEN "adminDir" THEN ASSIGN         cAdminDir           = ttIniFile.cVarValue.
            WHEN "backupDir" THEN ASSIGN        cBackupDir          = ttIniFile.cVarValue.
            WHEN "dbDir" THEN ASSIGN            cDbDir              = ttIniFile.cVarValue.
            WHEN "deskDir" THEN ASSIGN          cDeskDir            = ttIniFile.cVarValue.
            WHEN "docDir" THEN ASSIGN           cDocDir             = ttIniFile.cVarValue.
            WHEN "envDir" THEN ASSIGN           cEnvDir             = ttIniFile.cVarValue.
            WHEN "installDir" THEN ASSIGN       cInstallDir         = ttIniFile.cVarValue.
            WHEN "updatesDir" THEN ASSIGN       cUpdatesDir         = ttIniFile.cVarValue.
            WHEN "dbAdmin" THEN ASSIGN          cDbAdmin            = ttIniFile.cVarValue.
            WHEN "envAdmin" THEN ASSIGN         cEnvAdmin           = ttIniFile.cVarValue.
            WHEN "dbBackup" THEN ASSIGN         cDbBackup           = ttIniFile.cVarValue.
            WHEN "pgmBackup" THEN ASSIGN        cPgmBackup          = ttIniFile.cVarValue.
            WHEN "resBackup" THEN ASSIGN        cResBackup          = ttIniFile.cVarValue.
            WHEN "dbAuditDir" THEN ASSIGN       cDbAuditDir         = ttIniFile.cVarValue.
            WHEN "dbDataDir" THEN ASSIGN        cDbDataDir          = ttIniFile.cVarValue.
            WHEN "dbProdDir" THEN ASSIGN        cDbProdDir          = ttIniFile.cVarValue.
            WHEN "dbShipDir" THEN ASSIGN        cDbShipDir          = ttIniFile.cVarValue.
            WHEN "dbStructDir" THEN ASSIGN      cDbStructDir        = ttIniFile.cVarValue.
            WHEN "dbTestDir" THEN ASSIGN        cDbTestDir          = ttIniFile.cVarValue.
            WHEN "docMiscDocuments" THEN ASSIGN cDocMiscDocuments   = ttIniFile.cVarValue.
            WHEN "docReleaseNotes" THEN ASSIGN  cDocReleaseNotes    = ttIniFile.cVarValue.
            WHEN "docUserManual" THEN ASSIGN    cDocUserManual      = ttIniFile.cVarValue.
            WHEN "envProdDir" THEN ASSIGN       cEnvProdDir         = ttIniFile.cVarValue.
            WHEN "envTestDir" THEN ASSIGN       cEnvTestDir         = ttIniFile.cVarValue.
            WHEN "envAddonDir" THEN ASSIGN      cEnvAddonDir        = ttIniFile.cVarValue.
            WHEN "envCustFiles" THEN ASSIGN     cEnvCustFiles       = ttIniFile.cVarValue.
            WHEN "envCustomerDir" THEN ASSIGN   cEnvCustomerDir     = ttIniFile.cVarValue.
            WHEN "envOverrideDir" THEN ASSIGN   cEnvOverrideDir     = ttIniFile.cVarValue.
            WHEN "envPoDir" THEN ASSIGN         cEnvPoDir           = ttIniFile.cVarValue.
            WHEN "envProgramsDir" THEN ASSIGN   cEnvProgramsDir     = ttIniFile.cVarValue.
            WHEN "envResourceDir" THEN ASSIGN   cEnvResourceDir     = ttIniFile.cVarValue.
            WHEN "envScheduleDir" THEN ASSIGN   cEnvScheduleDir     = ttIniFile.cVarValue.
            WHEN "envTemplateDir" THEN ASSIGN   cEnvTemplateDir     = ttIniFile.cVarValue.
            WHEN "envUserMenuDir" THEN ASSIGN   cEnvUserMenuDir     = ttIniFile.cVarValue.
            WHEN "envUsersDir" THEN ASSIGN      cEnvUsersDir        = ttIniFile.cVarValue.
            WHEN "instAOA" THEN ASSIGN          cInstAOA            = ttIniFile.cVarValue.
            WHEN "instBackup" THEN ASSIGN       cInstBackup         = ttIniFile.cVarValue.
            WHEN "instDBMS" THEN ASSIGN         cInstDBMS           = ttIniFile.cVarValue.
            WHEN "instEsko" THEN ASSIGN         cInstEsko           = ttIniFile.cVarValue.
            WHEN "instFileUtils" THEN ASSIGN    cInstFileUtils      = ttIniFile.cVarValue.
            WHEN "instLocalPrint" THEN ASSIGN   cInstLocalPrint     = ttIniFile.cVarValue.
            WHEN "instRemAccess" THEN ASSIGN    cInstRemAccess      = ttIniFile.cVarValue.
            WHEN "updAdminDir" THEN ASSIGN      cUpdAdminDir        = ttIniFile.cVarValue.
            WHEN "updCompressDir" THEN ASSIGN   cUpdCompressDir     = ttIniFile.cVarValue.
            WHEN "updDataDir" THEN ASSIGN       cUpdDataDir         = ttIniFile.cVarValue.
            WHEN "updDataUpdateDir" THEN ASSIGN cUpdDataUpdateDir   = ttIniFile.cVarValue.
            WHEN "updDeskDir" THEN ASSIGN       cUpdDeskDir         = ttIniFile.cVarValue.
            WHEN "updMenuDir" THEN ASSIGN       cUpdMenuDir         = ttIniFile.cVarValue.
            WHEN "updProgramDir" THEN ASSIGN    cUpdProgramDir      = ttIniFile.cVarValue.
            WHEN "updRelNotesDir" THEN ASSIGN   cUpdRelNotesDir     = ttIniFile.cVarValue.
            WHEN "updSqlDir" THEN ASSIGN        cUpdSqlDir          = ttIniFile.cVarValue.
            WHEN "updStructureDir" THEN ASSIGN  cUpdStructureDir    = ttIniFile.cVarValue.
            WHEN "modeList" THEN ASSIGN         cModeList           = ttIniFile.cVarValue.
            WHEN "envList" THEN ASSIGN          cEnvList            = ttIniFile.cVarValue.
            WHEN "dbList" THEN ASSIGN           cDbList             = ttIniFile.cVarValue.
            WHEN "pgmList" THEN ASSIGN          cPgmList            = ttIniFile.cVarValue.
            WHEN "dbDirList" THEN ASSIGN        cDbDirList          = ttIniFile.cVarValue.
            WHEN "dbPortList" THEN ASSIGN       cDbPortList         = ttIniFile.cVarValue.
            WHEN "audDirList" THEN ASSIGN       cAudDirList         = ttIniFile.cVarValue.
            WHEN "audDbList" THEN ASSIGN        cAudDbList          = ttIniFile.cVarValue.
            WHEN "audPortList" THEN ASSIGN      cAudPortList        = ttIniFile.cVarValue.
            WHEN "envVerList" THEN ASSIGN       cEnvVerList         = ttIniFile.cVarValue.
            WHEN "dbVerList" THEN ASSIGN        cDbVerList          = ttIniFile.cVarValue.
            WHEN "audVerList" THEN ASSIGN       cAudVerList         = ttIniFile.cVarValue.
            WHEN "prodDbName" THEN ASSIGN       cProdDbName         = ttIniFile.cVarValue.
            WHEN "prodDbPort" THEN ASSIGN       cProdDbPort         = ttIniFile.cVarValue.
            WHEN "prodDbStFile" THEN ASSIGN     cProdDbStFile       = ttIniFile.cVarValue.
            WHEN "shipDbName" THEN ASSIGN       cShipDbName         = ttIniFile.cVarValue.
            WHEN "shipDbPort" THEN ASSIGN       cShipDbPort         = ttIniFile.cVarValue.
            WHEN "shipDbStFile" THEN ASSIGN     cShipDbStFile       = ttIniFile.cVarValue.
            WHEN "testDbName" THEN ASSIGN       cTestDbName         = ttIniFile.cVarValue.
            WHEN "testDbPort" THEN ASSIGN       cTestDbPort         = ttIniFile.cVarValue.
            WHEN "testDbStFile" THEN ASSIGN     cTestDbStFile       = ttIniFile.cVarValue.
            WHEN "dfFileName" THEN ASSIGN       cDfFileName         = ttIniFile.cVarValue.
            WHEN "deltaFileName" THEN ASSIGN    cDeltaFileName      = ttIniFile.cVarValue.
            WHEN "apiIPAddress" THEN ASSIGN     cAPIIpAddress       = ttIniFile.cVarValue.
            WHEN "apiPort" THEN ASSIGN          cAPIPort            = ttIniFile.cVarValue.
            WHEN "adminPort" THEN ASSIGN        cAdminPort          = ttIniFile.cVarValue.
            WHEN "nameServerName" THEN ASSIGN   cNameServerName     = ttIniFile.cVarValue.
            WHEN "nameServerPort" THEN ASSIGN   cNameServerPort     = ttIniFile.cVarValue.
            WHEN "appServerName" THEN ASSIGN    cAppServerName      = ttIniFile.cVarValue.
            WHEN "appServerPort" THEN ASSIGN    cAppServerPort      = ttIniFile.cVarValue.
        END CASE.
    END.
    
    /* Are we on the server? If not, this isn't helpful */
    ASSIGN 
        FILE-INFO:FILE-NAME = cDbDrive + "\" + cTopDir + "\" + cDbDir + "\Structure\STFiles\progress.dev".
    IF FILE-INFO:FULL-PATHNAME NE ? THEN DO:  
        /* Reset API values from DLC properties pages */
        RUN ipGetPropertyValues (OUTPUT cOutAdminPort,
            OUTPUT cOutAppServerName,
            OUTPUT cOutAppServerPort,
            OUTPUT cOutNameServerName,
            OUTPUT cOutNameServerPort).
        DO iCtr = 1 TO NUM-ENTRIES(cAPIList):
            FIND ttIniFile WHERE 
                ttIniFile.cVarName EQ ENTRY(iCtr,cAPIList)
                NO-ERROR.
            CASE ttIniFile.cVarName:
                WHEN "adminPort" THEN 
                    ASSIGN 
                        ttIniFile.cVarValue = IF ttIniFile.cVarValue EQ "" THEN cOutAdminPort ELSE ttIniFile.cVarValue.
                WHEN "nameServerName" THEN 
                    ASSIGN 
                        ttIniFile.cVarValue = IF ttIniFile.cVarValue EQ "" THEN cOutNameServerName ELSE ttIniFile.cVarValue.
                WHEN "nameServerPort" THEN 
                    ASSIGN 
                        ttIniFile.cVarValue = IF ttIniFile.cVarValue EQ "" THEN cOutNameServerPort ELSE ttIniFile.cVarValue.
                WHEN "appServerName" THEN 
                    ASSIGN 
                        ttIniFile.cVarValue = IF ttIniFile.cVarValue EQ "" THEN cOutAppServerName ELSE ttIniFile.cVarValue.
                WHEN "appServerPort" THEN 
                    ASSIGN 
                        ttIniFile.cVarValue = IF ttIniFile.cVarValue EQ "" THEN cOutAppServerPort ELSE ttIniFile.cVarValue.
            END.
        END.
    END.             

    /* Handle initialization of newly added variables here */
    FIND ttIniFile WHERE 
        ttIniFile.cVarName = "audVerList"
        NO-ERROR.
    IF AVAILABLE ttIniFile 
    AND ttIniFile.cVarValue EQ "" THEN ASSIGN 
        ttIniFile.cVarValue = cDbVerList
        cAudVerList = cDbVerList.
        
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

    IF wDbVerList NE "" THEN DO:
        FIND ttIniFile WHERE ttIniFile.cVarName = "dbVerList" NO-ERROR.
        ASSIGN 
            ttIniFile.cVarValue = wDbVerList.
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

