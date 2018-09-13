@ECHO OFF
REM This program allows a user to create a backup of a single database, 
REM or of all databases known to the admin server
SET iniFile=..\..\Admin\advantzware.ini
SET tgtDir=..\..\Admin\EnvAdmin
FOR /F "tokens=1,2 delims==" %%G IN (%iniFile%) DO SET %%G=%%H
SETLOCAL ENABLEDELAYEDEXPANSION
XCOPY /S /Y .\Deployment\Admin\*.* ..\..\Admin > NUL
XCOPY /S /Y .\Deployment\Desktop\*.* ..\..\Desktop > NUL
XCOPY /S /Y .\Deployment\Databases\*.* ..\..\Databases > NUL
cd ..\..\admin\envadmin
START !DLCDir!\bin\prowin.exe -basekey INI -ininame dbms.ini -pf advantzware.pf -p asiUpdate.w
