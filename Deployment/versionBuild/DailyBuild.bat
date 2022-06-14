@ECHO OFF

IF EXIST c:\asigui\build\buildON.txt (
    ECHO A build is already running
    pause
    GOTO :EXIT
)

:INITIALIZE
:: Set basic variables from THIS SERVER
SET DLC=C:\Progress\OE116_64
SET DLCBIN=%DLC%\bin
SET buildDir=\asigui\build
SET clogfile=c:\asigui\build\mergelog.txt
SET cViewFile=c:\asigui\build\mergecomp.txt

:: Establish GIT credentials
SET ghToken=5c5b4ee9facd6495549ae1a5e6c4184ec492807c
SET gitCredentials=-u https://%ghToken%@github.com/mark-advantzware/advantzware.git

:: This file will serve as flag to let the later test process know that the build did not complete successfully
ECHO Build started %date% %time% > c:\asigui\build\buildON.txt

:: Copy existing merge log so we can put current on top
MOVE /Y c:\asigui\build\mergelog.txt c:\asigui\build\mergetemp.txt > NUL

:: Write the build header to the build log
ECHO %date% %time% Merge/Compile Started > %cViewFile%
ECHO ---------------------------------------------------- >> %cViewFile%
ECHO %date% Merge/Compile >> %clogfile%
ECHO ---------------------------------------------------- >> %clogFile%

:: Update auditTbl/auditFld records
ECHO %time% Validating/adding auditTbl/Fld records
ECHO %time% Validating/adding auditTbl/Fld records >> %clogfile%
ECHO %time% Updating SQL permissions
ECHO %time% Updating SQL permissions >> %clogfile%
ECHO .
C:
CD %buildDir%
CALL %DLCBIN%\prowin.exe -basekey "INI" -ininame versionBuild.ini -pf versionBuildNODB.pf -p updateAuditTblConnect.p

:GITHUB
ECHO %time% Pulling latest merges from GitHub
ECHO %time% Pulling latest merges from GitHub >> %clogfile%
ECHO .
C:
CD /asigui/repositories/advantzware
:: NOTE: GIT commands must be in lowercase
git checkout -q develop >> %clogfile%
git pull -q origin develop >> %clogfile%
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

:: Create a time stamp and predict compile completion
FOR /F "tokens=1-3 delims=:." %%a IN ("%time%") DO (
   SET timeHour=%%a
   SET timeMinute=%%b
   SET timeSeconds=%%c
)
SET /A newTime=timeHour*60 + timeMinute + 20
SET /A timeHour=newTime/60, timeMinute=newTime%%60
IF %timeHour% gtr 23 SET timeHour=0
IF %timeHour% lss 10 SET timeHour=0%timeHour%
IF %timeMinute% lss 10 SET timeMinute=0%timeMinute%
ECHO Should complete approximately %timeHour%:%timeMinute%
ECHO Should complete approximately %timeHour%:%timeMinute% >> %clogfile%

C:
CD /asigui/build/fsinstaller

:: Run the compiler program
CALL C:\Progress\OE116_64\bin\prowin.exe -basekey "INI" -ininame progress.ini -zn -pf client2.pf -p c:\asigui\build\fsinstaller\fsInstallerAuto.w -param "C:\asigui\build\fsInstaller\AWDevel.cfg" >> %clogfile%

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
ECHO .

:PLBUILD
:: This creates current .pl files for inclusion in version build
:: Note: these are NOT copied to DEVEL, as would be blocked by any users currently logged in
ECHO Creating program library (.pl) files...
ECHO Creating program library (.pl) files... >> %clogfile%
C:
CD /asigui/build
:: Empty the existing PLBuild directory and recreate subdirs
DEL /s /q PLBuild\*.* >NUL
RMDIR /s /q PLBuild\Graphics >NUL
RMDIR /s /q PLBuild\adm >NUL
RMDIR /s /q PLBuild\adm2 >NUL
MKDIR PLBuild\Graphics
MKDIR PLBuild\adm
MKDIR PLBuild\adm2

:: Copy graphics and objects from current code
XCOPY c:\asigui\environments\Devel\Resources\Graphics\*.* .\PLBuild\Graphics\ /q /s >NUL
XCOPY c:\asigui\environments\Devel\Programs\adm\*.* .\PLBuild\adm\ /q /s >NUL
XCOPY c:\asigui\environments\Devel\Programs\adm2\*.* .\PLBuild\adm2\ /q /s >NUL

CD /asigui/Build/PLBuild
%DLCBIN%/prolib asiGraphics.pl -create
%DLCBIN%/prolib asiObjects.pl -create

%DLCBIN%/prolib asiGraphics.pl -add Graphics/*.* >NUL
%DLCBIN%/prolib asiGraphics.pl -add Graphics/16x16/*.* >NUL
%DLCBIN%/prolib asiGraphics.pl -add Graphics/24x24/*.* >NUL
%DLCBIN%/prolib asiGraphics.pl -add Graphics/32x32/*.* >NUL
%DLCBIN%/prolib asiGraphics.pl -add Graphics/48x48/*.* >NUL

%DLCBIN%/prolib asiObjects.pl -add adm/objects/*.* >NUL
%DLCBIN%/prolib asiObjects.pl -add adm2/*.* >NUL
%DLCBIN%/prolib asiObjects.pl -add adm2/custom/*.* >NUL
%DLCBIN%/prolib asiObjects.pl -add adm2/image/*.* >NUL
%DLCBIN%/prolib asiObjects.pl -add adm2/support/*.* >NUL

ECHO Finished creating program library (.pl) files...
ECHO Finished creating program library (.pl) files... >> %clogfile%
ECHO .

:DBBACKUP
ECHO %time% Begin devel database backup
ECHO %time% Begin devel database backup >> %clogfile%
C:
CD /asigui/databases/test
CALL C:\Progress\OE116_64\bin\probkup.bat online TESTDEVELd c:\asigui\backups\databases\TESTDEVELd.bak >> %clogfile%
CALL C:\Progress\OE116_64\bin\probkup.bat online TESTDEVELa c:\asigui\backups\databases\TESTDEVELa.bak >> %clogfile%
: Zip up the backup files for transfer
DEL C:\asigui\backups\databases\databases.7z > NUL
C:\asigui\admin\envadmin\7z.exe a c:\asigui\backups\databases\databases.7z c:\asigui\backups\databases\TESTDEVEL?.bak > NUL
ECHO %time% Ending devel database backup
ECHO %time% Ending devel database backup >> %clogfile%
ECHO .
ECHO %time% DB Backup Complete >> %cViewFile%

:DEVELCOPY
:: Copy the zip file to the help server so JAX-DEV can pull it down
ECHO %time%   Copying zipped backup to helpsvr
ECHO %time%   Copying zipped backup to helpsvr >> %clogfile%
COPY /Y /B c:\asigui\backups\databases\databases.7z H:\PubUpdates\CurrentPatch\databases.zip >> %clogfile%

:: Resequence logs so we can put current on top
COPY c:\asigui\build\mergelog.txt + c:\asigui\build\mergetemp.txt c:\asigui\build\Combined.txt > NUL
DEL /Q c:\asigui\build\mergelog.txt > NUL
DEL /Q c:\asigui\build\mergetemp.txt > NUL
MOVE /Y c:\asigui\build\Combined.txt c:\asigui\build\mergelog.txt > NUL

:END
exit
