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
:SETWHICH
SET whichDB=all

:BACKUP
SET backdate=%MyDate:~3,1%%MyDate:~4,2%%MyDate:~6,2%
SET DLC=%DLC11%
!Drive!
CD \!topDir!\!DbDir!\!dbProdDir!
ECHO Beginning PROD database backup >> %logFile%
CALL !DLC11!\bin\probkup online !prodDbName! !Drive!\!topDir!\!backupDir!\!dbBackup!\!prodDbName!.bak > NUL
IF EXIST !Drive!\!topDir!\!backupDir!\!dbBackup!\!prodDbName!.bak (
    ECHO PROD database backed up successfully
    ECHO PROD database backed up successfully >> !logFile!
    )
COPY !prodDbStFile! !Drive!\!topDir!\!backupDir!\!dbBackup!\!prodDbName!.st > NUL

CD \!topDir!\!DbDir!\!dbTestDir!
ECHO Beginning TEST database backup >> !logFile!
CALL !DLC11!\bin\probkup online !testDbName! !Drive!\!topDir!\!backupDir!\!dbBackup!\!testDbName!.bak > NUL
IF EXIST !Drive!\!topDir!\!backupDir!\!dbBackup!\!testDbName!.bak (
    ECHO TEST database backed up successfully
    ECHO TEST database backed up successfully >> !logFile!
    )
COPY !testDbStFile! !Drive!\!topDir!\!backupDir!\!dbBackup!\!testDbName!.st > NUL

CD \!topDir!\!DbDir!\!dbShipDir!
ECHO Beginning SHIP database backup >> !logFile!
CALL !DLC11!\bin\probkup online !shipDbName! !Drive!\!topDir!\!backupDir!\!dbBackup!\!shipDbName!.bak > NUL
IF EXIST !Drive!\!topDir!\!backupDir!\!dbBackup!\!shipDbName!.bak (
    ECHO SHIP database backed up successfully
    ECHO SHIP database backed up successfully >> !logFile!
    )
COPY !shipDbStFile! !Drive!\!topDir!\!backupDir!\!dbBackup!\!shipDbName!.st > NUL

ECHO Completed unattended backup >> !logFile!
ECHO ------------------------------------------------------ >> !logFile!
GOTO :EXIT

:EXIT
