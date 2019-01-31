@ECHO OFF
SET iniFile=..\Admin\advantzware.ini
SET tgtDir=..\Admin\EnvAdmin

:: Read advantzware.ini and set variable values
FOR /F "tokens=1,2 delims==" %%G IN (%iniFile%) DO SET %%G=%%H
SETLOCAL ENABLEDELAYEDEXPANSION

:: Copy files/dirs from Patch to "regular" directories
XCOPY /S /Y .\Admin\*.* ..\Admin > NUL
XCOPY /S /Y .\ReleaseNotes\*.* ..\Documentation\ReleaseNotes > NUL
XCOPY /S /Y .\Structure\*.* ..\Databases\Structure > NUL

:: Switch progress.cfg with .dev version
COPY !DLCDir!\progress.cfg .\progress.cfg > NUL
COPY !DLCDir!\progress.run .\progress.run > NUL
COPY .\Structure\STFiles\progress.dev !DLCDir!\progress.cfg > NUL

:: Now move into envadmin and run the update program(s)
cd ..\Admin\Envadmin
CALL !DLCDir!\bin\prowin.exe -basekey INI -ininame dbms.ini -pf advantzware.pf -p asiUpdate.w  > NUL

:: Switch progress.cfg back to .run version
cd ..\..\Updates
XCOPY /Y .\progress.cfg !DLCDir!\progress.cfg > NUL
XCOPY /Y .\progress.run !DLCDir!\progress.cfg > NUL

:: Copy the extended update log to EnvAdmin and concatenate with current log
COPY /Y .\UpdateLog.txt ..\Admin\EnvAdmin\UpdateLog2.txt > NUL
COPY /Y ..\Admin\EnvAdmin\UpdateLog2.txt+..\Admin\EnvAdmin\UpdateLog.txt ..\Admin\EnvAdmin\UpdateLog3.txt > NUL

:: Remove all programs from /Updates dir
RD /S /Q . > NUL
DEL /S /Q *.* > NUL

:: Move the (new) extended update log back to /Updates
MOVE /Y ..\Admin\EnvAdmin\UpdateLog3.txt .\UpdateLog.txt > NUL