@ECHO ON
SET iniFile=..\Admin\advantzware.ini
SET tgtDir=..\Admin\EnvAdmin

:: Read advantzware.ini and set variable values
FOR /F "tokens=1,2 delims==" %%G IN (%iniFile%) DO SET %%G=%%H
SETLOCAL ENABLEDELAYEDEXPANSION

:: Check to ensure we're NOT in the upgrade dir
IF EXIST "..\..\DoNotDelete.txt" (
    ECHO MSGBOX "DON'T RUN THIS FROM AWS /UPGRADES!!!" > %temp%\TEMPmessage.vbs
    CALL %temp%\TEMPmessage.vbs
    DEL %temp%\TEMPmessage.vbs /f /q
    GOTO :QUIT
    )

:: Check to ensure we're on the ASI server
IF /I NOT !Hostname! == %computername% (
    ECHO MSGBOX "You MUST run this program on the ASI server." > %temp%\TEMPmessage.vbs
    CALL %temp%\TEMPmessage.vbs
    DEL %temp%\TEMPmessage.vbs /f /q
    GOTO :QUIT
    )

GOTO :Continue
:: Check to ensure the user has Admin rights
NET SESSION >nul 2>&1
IF NOT %errorlevel%==0 (
    ECHO MSGBOX "You MUST run this program as an Administrator." > %temp%\TEMPmessage.vbs
    CALL %temp%\TEMPmessage.vbs
    DEL %temp%\TEMPmessage.vbs /f /q
    GOTO :QUIT
    )

:Continue

:: Stop node listeners
taskkill /im node.exe /F > NUL

:: Move to Admin/EnvAdmin dir
CD ..\Admin\Envadmin

:: Remove deprecated files from Admin/EnvAdmin
DEL /Q asiLogin.* > NUL
DEL /Q asiUpdate.* > NUL
DEL /Q asiLogin*.* > NUL
DEL /Q *.bak > NUL
DEL /Q *.e > NUL
DEL /Q *.out > NUL
DEL /Q *.p > NUL
DEL /Q *-*.txt > NUL
DEL /Q *2.txt > NUL
DEL /Q *3.txt > NUL
DEL /Q 7z.* > NUL
DEL /Q convusr.* > NUL
CLS

:: Build some directories that may or may not already exist
CD ..\..
MKDIR Documentation > NUL
CD Documentation
MKDIR DBDict > NUL
CD ..
MKDIR Install > NUL
CD Install
MKDIR ReportWriter > NUL
CD ..
MKDIR Backups > NUL
CD Backups
MKDIR PatchFiles
CD ..
CD Updates

:: Copy files/dirs from Patch to "regular" directories
XCOPY /Y .\*.zip ..\Backups\PatchFiles > NUL
XCOPY /S /Y .\Admin\*.* ..\Admin > NUL
XCOPY /S /Y .\Desktop\*.* ..\Desktop > NUL
XCOPY /S /Y .\Documentation\*.* ..\Documentation > NUL
XCOPY /S /Y .\Install\ReportWriter\*.* ..\Install\ReportWriter > NUL
XCOPY /S /Y .\Structure\*.* ..\Databases\Structure > NUL
IF NOT EXIST ..\Admin\EnvAdmin\updateHist.txt (
    COPY /Y .\UpdateHist.txt ..\Admin\EnvAdmin\UpdateHist.txt > NUL
)

:: Switch progress.cfg with .dev version
IF EXIST "!DLCDir!\progress.run" (
    COPY /Y !DLCDir!\progress.run .\progress.cfg > NUL
    COPY /Y .\Structure\STFiles\progress.dev !DLCDir!\progress.cfg > NUL
) ELSE (
    COPY /Y !DLCDir!\progress.cfg .\progress.cfg > NUL
    COPY /Y .\Structure\STFiles\progress.dev !DLCDir!\progress.cfg > NUL
)
CACLS !DLCDir!\progress.cfg /e /p Everyone:f

:: Now move into envadmin and run the update program(s)
CD ..\Admin\Envadmin

CALL !DLCDir!\bin\prowin.exe -basekey INI -ininame dbms.ini -pf advantzware.pf -p asiUpdate.w  

:: Switch progress.cfg back to .run version

CD ..\..\Updates

COPY /Y !DLCDir!\progress.cfg !DLCDir!\progress.dev > NUL
COPY /Y .\progress.cfg !DLCDir!\progress.cfg > NUL
CACLS !DLCDir!\progress.cfg /e /p Everyone:f

:: Concatenate the extended and current update logs
COPY /Y ..\Admin\EnvAdmin\UpdateLog.txt+..\Admin\EnvAdmin\UpdateHist.txt ..\Admin\EnvAdmin\UpdateHist1.txt > NUL
DEL /Q ..\Admin\EnvAdmin\UpdateHist.txt > NUL
COPY /Y ..\Admin\EnvAdmin\UpdateHist1.txt ..\Admin\EnvAdmin\UpdateHist.txt > NUL
DEL /Q ..\Admin\EnvAdmin\UpdateHist1.txt > NUL
CLS

:: Delete any unused/deprecated programs from dir structure
cd ..\Desktop

IF EXIST "ASI Update" (
    DEL /S /Q "ASI Update"* > NUL
)

:QUIT
CD ..\Admin\EnvAdmin
EmptyFolder.bat

EXIT

