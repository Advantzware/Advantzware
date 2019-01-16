@ECHO OFF
REM This program allows a user to start all viable databases 

REM Check advantzware.ini for variable defaults
CALL ..\getSysVars.bat
IF "%drive%" == "" (
    ECHO advantzware.ini file not found...Exiting
    GOTO :EXIT
    )

REM Do this AFTER you get the system variables
SETLOCAL ENABLEDELAYEDEXPANSION

ECHO Stopping PROD database
CALL !DLC11!\bin\dbman -host %hostname% -port %adminport% -database !prodDBName! -stop > NUL
ECHO Stopping TEST database
CALL !DLC11!\bin\dbman -host %hostname% -port %adminport% -database !testDBName! -stop > NUL

:EXIT