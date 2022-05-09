@ECHO OFF
REM This program performs an unattended backup of all databases
:: ECHO Beginning unattended backup...

REM Check advantzware.ini for variable defaults
CALL ..\getSysVars.bat
REM Do this AFTER you get the system variables
SETLOCAL ENABLEDELAYEDEXPANSION

FOR /f "skip=1" %%x IN ('wmic os get localdatetime') DO IF NOT DEFINED MyDate SET MyDate=%%x
SET logFile=!Drive!\!topDir!\Admin\DbAdmin\backup.log

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

!DbDrive!

CD \!topDir!\Databases\Prod
ECHO   Backing up asiProd to asiProd.bak >> %logFile%
CALL !DLC!\bin\probkup online asiProd !MapDir!\Backups\Databases\asiProd.bak >> %logFile%
IF EXIST !Drive!\!topDir!\Backups\Databases\asiProd.bak (
    ECHO     asiProd backup complete >> !logFile!
    )

CD \!topDir!\Databases\Audit
ECHO   Backing up audProd to audProd.bak >> %logFile%
CALL !DLC!\bin\probkup online audProd !MapDir!\Backups\Databases\audProd.bak >> %logFile%
IF EXIST !Drive!\!topDir!\Backups\Databases\audProd.bak (
    ECHO     audProd backup complete >> !logFile!
    )

FOR /f "skip=1" %%x IN ('wmic os get localdatetime') DO IF NOT DEFINED MyDate2 SET MyDate2=%%x
ECHO Completed unattended backup at %MyDate2% >> !logFile!
ECHO ------------------------------------------------------ >> !logFile!
GOTO :EXIT

:EXIT
