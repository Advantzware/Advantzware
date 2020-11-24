@ECHO OFF

IF EXIST c:\asigui\build\buildON.txt (
    ECHO A build is already running
    pause
    GOTO :EXIT
)
TASKKILL /F /IM ASIbranchTest.exe > NUL

:INITIALIZE
SET DLC=C:\Progress\OE116_64
SET DLCBIN=%DLC%\bin
SET buildDir=\asigui\build
SET clogfile=c:\asigui\build\mergelog.txt
SET cViewFile=c:\asigui\build\mergecomp.txt
SET ghToken=5c5b4ee9facd6495549ae1a5e6c4184ec492807c
SET gitCredentials=-u https://%ghToken%@github.com/mark-advantzware/advantzware.git
:: This file will serve as flag to let the later test process know that the build did not complete successfully
ECHO Build started %date% %time% > c:\asigui\build\buildON.txt
:: Copy existing merge log so we can put current on top
MOVE /Y c:\asigui\build\mergelog.txt c:\asigui\build\mergetemp.txt > NUL
ECHO %date% %time% Merge/Compile Started > %cViewFile%
ECHO ---------------------------------------------------- >> %cViewFile%
ECHO %date% Merge/Compile >> %clogfile%
ECHO ---------------------------------------------------- >> %clogFile%
:: Update auditTbl/auditFld records
C:
CD %buildDir%
CALL %DLCBIN%\prowin.exe -basekey "INI" -ininame versionBuild.ini -pf versionBuildNODB.pf -p updateAuditTbl1.p

:GITHUB
ECHO %time% Pulling latest merges from GitHub
ECHO %time% Pulling latest merges from GitHub >> %clogfile%
ECHO .
c:
cd /asigui/repositories/advantzware
git checkout -q develop >> %clogfile%
git pull -q origin develop >> %clogfile%
:: git push origin develop >> %clogfile%
:: Stash any local changes to files
git stash >> %clogfile%
:: Fetch all from origin
git fetch --all >> %clogfile%
:: Perform hard reset (removing any unpushed files) to develop head
git reset --hard origin/develop >> %clogfile%
ECHO %time% Git Pull Complete >> %cViewFile%
ECHO %time% Git Pull Complete
ECHO .

:FILECOPY
ECHO %time% Copying program files to /Devel environment
ECHO %time% Copying program files to /Devel environment >> %clogfile%
ECHO %time%   Deleting and recreating /Devel/Source directory
ECHO %time%   Deleting and recreating /Devel/Source directory >> %clogfile%
RMDIR /Q /S c:\asigui\environments\devel\source >> %clogfile%
MKDIR c:\asigui\environments\devel\Source >> %clogfile%
ECHO %time%   Copying source files from repository to /Devel/Source
ECHO %time%   Copying source files from repository to /Devel/Source >> %clogfile%
XCOPY /E /q /r /y /C c:\asigui\repositories\advantzware\source c:\asigui\environments\devel\source >> %clogfile%
ECHO .
ECHO %time% Copying resource files to /Devel environment
ECHO %time% Copying resource files to /Devel environment >> %clogfile%
::For now, skip anything having to do with resource dirs
GOTO :EndResource
ECHO %time%   Copying resource files from repository to /Devel/Resources2
ECHO %time%   Copying resource files from repository to /Devel/Resources2 >> %clogfile%
MKDIR c:\asigui\environments\devel\Resources2 >> %clogfile%
XCOPY /E /q /r /y /C c:\asigui\repositories\advantzware\resources c:\asigui\environments\devel\Resources2 >> %clogfile%
ECHO %time%   Deleting /Devel/Resources directory
ECHO %time%   Deleting /Devel/Resources directory >> %clogfile%
RMDIR /Q /S c:\asigui\environments\devel\resources >> %clogfile%
ECHO %time%   Moving Resources2 directory to Resource
ECHO %time%   Moving Resources2 directory to Resource >> %clogfile%
MOVE /Y c:\asigui\environments\devel\Resources2 c:\asigui\environments\devel\Resources >> %clogfile%
:EndResource
ECHO %time%   Copying resource files from repository to /Devel/Resources
ECHO %time%   Copying resource files from repository to /Devel/Resources >> %clogfile%
XCOPY /E /q /r /y /C c:\asigui\repositories\advantzware\resources c:\asigui\environments\devel\Resources >> %clogfile%
ECHO %time%   Removing /Devel/Override
ECHO %time%   Removing /Devel/Override >> %clogfile%
RMDIR /Q /S c:\asigui\environments\devel\override >> %clogfile%
MKDIR c:\asigui\environments\devel\override >> %clogfile%
ECHO %time% Source/Resource copy complete >> %cViewFile%
ECHO %time% Source/Resource copy complete
ECHO %time% Source/Resource copy complete >> %clogfile%
ECHO .

:COMPILE
ECHO %time% Begin compiling program files
ECHO %time% Begin compiling program files >> %clogfile%
for /F "tokens=1-3 delims=:." %%a in ("%time%") do (
   set timeHour=%%a
   set timeMinute=%%b
   set timeSeconds=%%c
)
SET /A newTime=timeHour*60 + timeMinute + 20
SET /A timeHour=newTime/60, timeMinute=newTime%%60
IF %timeHour% gtr 23 SET timeHour=0
IF %timeHour% lss 10 SET timeHour=0%timeHour%
IF %timeMinute% lss 10 SET timeMinute=0%timeMinute%
ECHO Should complete approximately %timeHour%:%timeMinute%
ECHO Should complete approximately %timeHour%:%timeMinute% >> %clogfile%
c:
cd /asigui/build/fsinstaller
CALL C:\Progress\OE116_64\bin\prowin.exe -basekey "INI" -ininame progress11.ini -zn -pf client2.pf -p c:\asigui\build\fsinstaller\fsInstallerAuto.w -param "C:\asigui\build\fsInstaller\AWDevel.cfg" >> %clogfile%
ECHO %time% Merge/Compile Complete
ECHO %time% Merge/Compile Complete >> %clogfile%
ECHO ---------------------------------------------------- >> %clogFile%

:CLEANUP
ECHO %time% Compile Complete >> %cViewFile%
:: This removes the "flag" file on successful completion
DEL /Q c:\asigui\build\buildON.txt > NUL

ECHO . >> %clogFile%
ECHO ---------------------------------------------------- >> %cViewFile%
ECHO %date% %time% Build Complete >> %cViewFile%

:DBBACKUP
ECHO %time% Begin devel database backup
ECHO %time% Begin devel database backup >> %clogfile%
c:
cd /asigui/databases/test
CALL C:\Progress\OE116_64\bin\probkup.bat online TESTDEVELd c:\asigui\backups\databases\TESTDEVELd.bak >> %clogfile%
CALL C:\Progress\OE116_64\bin\probkup online TESTDEVELa c:\asigui\backups\databases\TESTDEVELa.bak >> %clogfile%
: Zip up the backup files for transfer
DEL C:\asigui\backups\databases\databases.7z > NUL
C:\asigui\admin\envadmin\7z.exe a c:\asigui\backups\databases\databases.7z c:\asigui\backups\databases\TESTDEVEL?.bak > NUL
ECHO %time% Ending devel database backup
ECHO %time% Ending devel database backup >> %clogfile%
ECHO .
ECHO %time% DB Backup Complete >> %cViewFile%

:DEVELCOPY
:: Copy the zip file to developers' machines
ECHO %time%   Copying zipped backup to developers machines
ECHO %time%   Copying zipped backup to developers machines >> %clogfile%
COPY /Y /B c:\asigui\backups\databases\databases.7z K:\backups\databases\databases.7z >> %clogfile%
COPY /Y /B c:\asigui\backups\databases\databases.7z R:\backups\databases\databases.7z >> %clogfile%
COPY /Y /B c:\asigui\backups\databases\databases.7z S:\backups\databases\databases.7z >> %clogfile%
COPY /Y /B c:\asigui\backups\databases\databases.7z X:\backups\databases\databases.7z >> %clogfile%
COPY /Y /B c:\asigui\backups\databases\databases.7z Y:\backups\databases\databases.7z >> %clogfile%
COPY /Y /B c:\asigui\backups\databases\databases.7z Z:\backups\databases\databases.7z >> %clogfile%
COPY /Y /B c:\asigui\backups\databases\*.df K:\backups\databases >> %clogfile%
COPY /Y /B c:\asigui\backups\databases\*.df R:\backups\databases >> %clogfile%
COPY /Y /B c:\asigui\backups\databases\*.df S:\backups\databases >> %clogfile%
COPY /Y /B c:\asigui\backups\databases\*.df X:\backups\databases >> %clogfile%
COPY /Y /B c:\asigui\backups\databases\*.df Y:\backups\databases >> %clogfile%
COPY /Y /B c:\asigui\backups\databases\*.df Z:\backups\databases >> %clogfile%
:: Resequence logs so we can put current on top
COPY c:\asigui\build\mergelog.txt + c:\asigui\build\mergetemp.txt c:\asigui\build\Combined.txt > NUL
DEL /Q c:\asigui\build\mergelog.txt > NUL
DEL /Q c:\asigui\build\mergetemp.txt > NUL
MOVE /Y c:\asigui\build\Combined.txt c:\asigui\build\mergelog.txt > NUL

:END
::exit
