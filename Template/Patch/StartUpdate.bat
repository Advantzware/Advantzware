@ECHO OFF
CONSOLESTATE /HIDE
SET iniFile=..\Admin\advantzware.ini
SET tgtDir=..\Admin\EnvAdmin

:: Read advantzware.ini and set variable values
FOR /F "tokens=1,2 delims==" %%G IN (%iniFile%) DO SET %%G=%%H
SETLOCAL ENABLEDELAYEDEXPANSION

:: Move to Admin/EnvAdmin dir
CD ..\Admin\Envadmin

:: Remove deprecated files from Admin/EnvAdmin
DEL /Q asiLogin.* > NUL
DEL /Q asiUpdate.* > NUL
DEL /Q asiLogin*.* > NUL
DEL /Q asiUpdate*.* > NUL
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
DEL /Q prerun*.* > NUL

:: Move to Updates dir
CD ..\..\Updates

:: Copy files/dirs from Patch to "regular" directories
XCOPY /S /Y .\Admin\*.* ..\Admin > NUL
XCOPY /S /Y .\DBDict\*.* ..\Documentation\DBDict > NUL
XCOPY /S /Y .\ReleaseNotes\*.* ..\Documentation\ReleaseNotes > NUL
XCOPY /S /Y .\Structure\*.* ..\Databases\Structure > NUL

:: Switch progress.cfg with .dev version
IF EXIST "!DLCDir!\progress.run" (
    XCOPY /Y !DLCDir!\progress.run .\progress.cfg > NUL
    XCOPY /Y .\Structure\STFiles\progress.dev !DLCDir!\progress.cfg > NUL
) ELSE (
    XCOPY /Y !DLCDir!\progress.cfg .\progress.cfg > NUL
    XCOPY /Y .\Structure\STFiles\progress.dev !DLCDir!\progress.cfg > NUL
)
    
:: Now move into envadmin and run the update program(s)
CD ..\Admin\Envadmin
CALL !DLCDir!\bin\prowin.exe -basekey INI -ininame dbms.ini -pf advantzware.pf -p asiUpdate.w  > NUL

:: Switch progress.cfg back to .run version
CD ..\..\Updates
XCOPY /Y !DLCDir!\progress.cfg !DLCDir!\progress.dev > NUL
XCOPY /Y .\progress.cfg !DLCDir!\progress.cfg > NUL

:: Concatenate the extended and current update logs
XCOPY /Y ..\Admin\EnvAdmin\UpdateHist.txt+..\Admin\EnvAdmin\UpdateLog.txt ..\Admin\EnvAdmin\UpdateHist1.txt > NUL
DEL /Q ..\Admin\EnvAdmin\UpdateHist.txt > NUL
DEL /Q ..\Admin\EnvAdmin\UpdateLog.txt > NUL
XCOPY /Y ..\Admin\EnvAdmin\UpdateHist1.txt ..\Admin\EnvAdmin\UpdateHist.txt > NUL
DEL /Q ..\Admin\EnvAdmin\UpdateHist1.txt > NUL

:: Delete any unused/deprecated programs from dir structure
cd ..\Desktop
DEL /S /Q "ASI Update"* > NUL

:: Remove all programs from /Updates dir
cd ..\Updates
RD /S /Q . > NUL
DEL /S /Q *.* > NUL

