@ECHO OFF
REM This program allows a user to restore a database from a list 
REM of dated backup files

REM Check advantzware.ini for variable defaults
CALL ..\getSysVars.bat
IF "%drive%" == "" (
    ECHO advantzware.ini file not found...Exiting
    GOTO :EXIT
    )

REM Do this AFTER you get the system variables
SETLOCAL ENABLEDELAYEDEXPANSION

:SETWHICH
CLS
ECHO This program allows you to restore a database from a list
ECHO of dated backup files.
ECHO Enter one of 'prod' or 'test'
SET /P whichDB="Which database do you wish to back up: "
ECHO .
IF /I NOT !whichDB! == prod (
    IF /I NOT !whichDB! == test (
        ECHO You must enter one of 'prod' or 'test'
        GOTO :SETWHICH
        )
    )
IF /I !whichDB! == prod (
    ECHO/
    ECHO *** THIS COMPLETELY REPLACES THE CURRENT LIVE DB WITH A BACKUP ***
    SET /P imSure="Are you SURE you want to replace the PROD database? (yes/no)
    IF /I NOT !imSure! == yes (
        ECHO Aborting restore...
        GOTO :END
        )
    ) 
:ENDSETWHICH

:CHOOSEBACKUP

ECHO/
ECHO The following backups were found to restore from:

SET index=1
FOR /F "usebackq tokens=2 delims=." %%d IN (`dir /b "!Drive!\!TopDir!\!BackupDir!\!DbBackup!\asi!whichDB!.*.bak"`) DO (
  SET backup!index!=%%d
  ECHO !index! - %%d
  SET /A index=!index!+1
)
SETLOCAL DISABLEDELAYEDEXPANSION
SET /P selection="Select a backup date by number: "
SET backup%selection% >nul 2>&1
IF ERRORLEVEL 1 (
  echo/
  ECHO Invalid number selected, try again.
  echo/
  goto :choosebackup
)
CALL :RESOLVE %%backup%selection%%%
ECHO Selected backup: %backup_date%
echo/
GOTO :verify
:RESOLVE
SET backup_date=%1
GOTO :EOF

:verify
set missing=0
set asi_filename=D:\ASIDbBackup\asi.%backup_date%.bak
if not exist "%asi_filename%" set /a missing=%missing%+1

if %missing%==0 goto :restore
echo Some backup files do not exist for the selected backup!
echo Please try a different backup.
echo/
goto :choosebackup


:END
pause