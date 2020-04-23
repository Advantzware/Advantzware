@ECHO OFF
REM This program performs an unattended backup of all databases
ECHO Beginning unattended backup...

REM Check advantzware.ini for variable defaults
CALL ..\getSysVars.bat
REM Do this AFTER you get the system variables
SETLOCAL ENABLEDELAYEDEXPANSION

FOR /f "skip=1" %%x IN ('wmic os get localdatetime') DO IF NOT DEFINED MyDate SET MyDate=%%x
SET logFile=!Drive!\!topDir!\!adminDir!\!dbAdmin!\backup.log

IF "%drive%" == "" (
    ECHO advantzware.ini file not found...Exiting >> %logFile%
    GOTO :EXIT
    )

IF EXIST %logFile% (
    ECHO Beginning unattended backup at %MyDate% >> %logFile%
    )
IF NOT EXIST %logFile% (
    ECHO Beginning unattended backup at %MyDate% > %logFile%
    )

:BACKUP
SET backdate=%MyDate:~3,1%%MyDate:~4,2%%MyDate:~6,2%
SET DLC=%DLCDir%
ECHO Beginning PROD database backup >> %logFile%

!Drive!

CD \!topDir!\!DbDir!\!dbProdDir!
ECHO   Backing up !prodDbName! to !prodDbName!.bak >> %logFile%
CALL !DLC!\bin\probkup online !prodDbName! !Drive!\!topDir!\!backupDir!\!dbBackup!\!prodDbName!.bak > NUL
IF EXIST !Drive!\!topDir!\!backupDir!\!dbBackup!\!prodDbName!.bak (
    ECHO     !prodDbName! backup complete >> !logFile!
    )

CD \!topDir!\!DbDir!\!dbAuditDir!
ECHO   Backing up !audDbName! to !audDbName!.bak >> %logFile%
CALL !DLC!\bin\probkup online !audDbName! !Drive!\!topDir!\!backupDir!\!dbBackup!\!audDbName!.bak > NUL
IF EXIST !Drive!\!topDir!\!backupDir!\!dbBackup!\!audDbName!.bak (
    ECHO     !audDbName! backup complete >> !logFile!
    )

FOR /f "skip=1" %%x IN ('wmic os get localdatetime') DO IF NOT DEFINED MyDate2 SET MyDate2=%%x
ECHO Completed unattended backup at %MyDate2% >> !logFile!
ECHO ------------------------------------------------------ >> !logFile!
GOTO :EXIT

:EXIT
