@ECHO OFF
SET iniFile=..\Admin\advantzware.ini
SET tgtDir=..\Admin\EnvAdmin

:: Read advantzware.ini and set variable values
FOR /F "tokens=1,2 delims==" %%G IN (%iniFile%) DO SET %%G=%%H
SETLOCAL ENABLEDELAYEDEXPANSION

:: Copy files from Patch\Deployment to "regular" directories
XCOPY /S /Y .\Deployment\Admin\*.* ..\Admin > NUL
XCOPY /S /Y .\Deployment\Desktop\*.* ..\Desktop > NUL
XCOPY /S /Y .\Deployment\Databases\*.* ..\Databases > NUL

:: Switch progress.cfg with .dev version
COPY !DLCDir!\progress.cfg .\progress.cfg > NUL
COPY !DLCDir!\progress.run .\progress.run > NUL
COPY .\StructureUpdate\STFiles\progress.dev !DLCDir!\progress.cfg > NUL

:: Now move into envadmin to run the update programs
cd ..\admin\envadmin
CALL !DLCDir!\bin\prowin.exe -basekey INI -ininame dbms.ini -pf advantzware.pf -p asiUpdate.w  > NUL

:: Switch progress.cfg back to .run version
cd ..\..\updates
XCOPY /Y .\progress.cfg !DLCDir!\progress.cfg > NUL
XCOPY /Y .\progress.run !DLCDir!\progress.cfg > NUL

:: Remove all programs
RD /S /Q . > NUL
DEL /S /Q *.* > NUL
COPY ..\Admin\EnvAdmin\UpdateLog.txt .\UpdateLog.txt > NUL
