@ECHO OFF
:: This code "assumes" it's own location is in /programs/api
:: If moved, may have to change the ini file lookup

:: Read advantzware.ini and set variable values
SET iniFile=..\..\..\..\Admin\advantzware.ini
FOR /F "tokens=1,2 delims==" %%G IN (%iniFile%) DO SET %%G=%%H
IF "%drive%" == "" (
    ECHO ...advantzware.ini file not found...Exiting
    GOTO :EXIT
    )

SETLOCAL ENABLEDELAYEDEXPANSION

:: Check to ensure we're running from the ASI server
:: Finding a guaranteed file in the filesystem database directory is as good a method as any
IF NOT EXIST !dbDrive!\!topDir!\!dbDir!\Structure\STFiles\progress.dev (
    ECHO MSGBOX "You MUST run this program on the ASI server." > %temp%\TEMPmessage.vbs
    CALL %temp%\TEMPmessage.vbs
    DEL %temp%\TEMPmessage.vbs /f /q
    GOTO :QUIT
    ) 

ECHO ...Stopping Advantzware_API appserver
!drive!
CD !DLCDir!\bin
CALL asbman -name !AppServerName! -host !hostname! -port !adminPort! -stop

:QUIT
EXIT