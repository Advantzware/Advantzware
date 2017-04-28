
REM ================================================================================ 
REM ================ InstallPatch.bat                           ====================
REM ===================Version 1.0 04/05/2016                     ==================


REM ================================================================================ 
REM ================ For Future Installs                        ====================
REM ===================DEFINE MSI,MST,UPN,log file & folder here ==================
REM SET MSINAME=SETUP.MSI
REM SET MSTNAME=SETUP.MSI
REM sET UPN=AppID-Vendor-AppName-Version-ReleseVersion
REM SET LOGSFOLDER="C:\ApplicationLogs\%UPN%_Install.log"
REM IF NOT EXIST "C:\ApplicationLogs" MD "C:\ApplicationLogs"
REM ================================================================================
REM ===================Check if the Product exists already========================== 
REM SET PRODUCTKEY=HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Uninstall
REM REG QUERY %PRODUCTKEY%\{4ECF4BDC-8387-329A-ABE9-CF5798F84BB2} 
REM IF NOT %ERRORLEVEL% EQU 0 (GOTO :INSTALL) ELSE GOTO :ENDHERE  


REM Prevent each command from being echoed to the screen - very annoying otherwise
@echo off

REM ================================================================================ 
REM ================ Extract value of current version from      ====================






REM FOR /F "eol=; tokens=1,2* delims=, " %%i in (asiversion.txt) do @echo %%i
REM set InstallVersion=%i%
REM echo %InstallVersion% 
REM set InstallVersion=16.0.1


REM ================================================================================ 
REM ================ Get Information needed from user           ====================
CLS
REM ECHO Install Version is %InstallVersion%
REM 	
SET /P InstallVersion="Enter the version number for this patch: "


SET /P askUpgrade="Do you wish to continue to install the patch for version  %InstallVersion%?"
IF NOT %askUpgrade% == Y DO GOTO :exit

SET /P sitename="Enter the site name (no spaces): "
SET /P dbDir="Enter the location of the databases: "
SET /P progressDir="Enter the location of where progress is installed (e.g. c:\progress\openedge): "
SET /P backupDir="Enter the location of the backup folder: "
SET /P rootfolder="Enter the location where the rcode is (e.g. N:\  : "
SET /P rcodefolder="Enter the name of the rcode is (e.g. rcode) : "


set AsiPublicFtpSite=ftp.advantzware.com
set AsiPublicFtpPwd=asi215 Yorkie12\301oxford
set InstallLog=.\%InstallVersion%Install.log




ECHO [InstallVersion] > advantzware.ini
ECHO %InstallVersion% >> advantzware.ini

ECHO [SiteName] >> advantzware.ini
ECHO %sitename% >> advantzware.ini

ECHO [dbDir] >> advantzware.ini
ECHO %dbDir% >> advantzware.ini

ECHO [progressDir] >> advantzware.ini
ECHO %progressDir% >> advantzware.ini
 
ECHO [backupDir] >> advantzware.ini
ECHO %backupDir% >> advantzware.ini

ECHO [rootFolder] >> advantzware.ini
ECHO %rootfolder% >> advantzware.ini
 
ECHO [rcodeFolder] >> advantzware.ini
ECHO %rcodefolder% >> advantzware.ini




mkdir %backupDir%

xcopy /s /E /I /Y %rootfolder%\%rcodefolder% %backupDir%
CLS
ECHO.
ECHO.
ECHO.
ECHO.

ECHO Insert this into rcode\progress.ini N:\programs,N:\programs\addon,N:\Resources,
ECHO Then Press spacebar to continue
PAUSE

ECHO Make sure the asi.pf and nosweat.pf files have -db as the first line, then press spacebar
ECHO Then Press spacebar to continue
PAUSE

SET /P askReady="Are you ready to start install?"
IF NOT %askReady% == Y DO GOTO :exit


REM ================================================================================ 
REM ================ Extract utility folder & .r's for use in this script ==========



REM ================================================================================ 
REM Clear the DOS Box and display detailed message for End User
REM Explain what is being installed and why
REM Give expected duration of install
REM Include any instructions that the end user needs to know about.
CLS
echo.
echo.
echo.
echo.
echo.
echo.
echo Installing Required components .....
echo You can continue working during the installation (it takes about ....).
echo.
echo.
echo Do NOT close this box until prompted, otherwise the installation will run again the next time that you login to the network.
echo.
echo.
echo.
echo If you have any questions, please contact Wade Kaldawi
echo.
echo.



REM ================================================================================ 
REM ================ Extract Info from lnk file                           ==========

REM     Run .r to parse the html of lnk information and create advantzware.ini
REM     Construct advantzware.ini: startin, dlc, executable, ini file, databases+info, database folder, backup folder, tmp folder (individual or shared), windows server version, expected max users, asiftp site, asiftp password, 
set OldRcode=
set InstallDir=
set InstallDrive=
set DbDrive=
set DbFolder=
set advIni=
set DbBackupFolder=

REM ================================================================================ 
REM ================ Drop a file in the root directory so that the batch file ======
REM ================ does not run repeatedly at login                         ======
REM ================ Include install results in dropped file                  ======

echo %COMPUTERNAME% >>%InstallLog%
date /T >>%InstallLog%
time /T >>%InstallLog%



REM ================================================================================ 
REM ================ Collect Windows Server Info and Ftp it ======
ECHO Capturing system information to systemSummary.txt
msinfo32 /report .\systemSummary.txt /categories +systemsummary

REM Export current database extents, % full
prostrct list %dbDir%\asi
prostrct list %dbDir%\nosweat
prostrct list %dbDir%\asihelp
prostrct list %dbDir%\addons\nosweat
prostrct list %dbDir%\addons\jobs
prostrct list %dbDir%\addons\emptrack
prostrct list %dbDir%\addons\rfq

copy %dbDir%\asi.st .
copy %dbDir%\nosweat.st .
copy %dbDir%\asihelp.st .
copy %dbDir%\addons\nosweat.st .\nosweat-add.st
copy %dbDir%\addons\emptrack.st .
copy %dbDir%\addons\jobs.st .
copy %dbDir%\addons\rfq.st .

REM 	nosweat.p modified to add to propath based on advanztware.ini
REM 	extract dlc value from advantzware.ini
REM 	extract location of test databases and code from advantzware.ini

REM 	Run utility to convert recid to rec_key
REM     Run utility to extract info about extents

REM run current application, run utility to export current propath, start-in folder
@echo off 
REM This line originally specified a drive letter
set DUMP_INC_DFFILE=.\wrk\delta.df 
set DUMP_INC_CODEPAGE=1252 
set DUMP_INC_INDEXMODE=active 
set DUMP_INC_RENAMEFILE=.\wrk\renamefile.rf 
set DUMP_INC_DEBUG=0 
set PROPATH=.
REM %DLC%/bin/_progres -b -db master -1 -db slave -1 -p prodict/dump_inc.p > D:\wrk\dump_inc.log 

echo End Of Dump Delta Definition File 
echo Start Ftp
echo open ftp.advantzware.com > ftpcmd.txt
echo user asi215 Yorkie12\301oxford >> ftpcmd.txt
echo put systemSummary.txt >> ftpcmd.txt
echo mkdir %sitename% >> ftpcmd.txt
echo cd %sitename% >> ftpcmd.txt
echo mput *.html >> ftpcmd.txt
echo put wrk\renamefile.rf >> ftpcmd.txt
echo put wrk\delta.df >> ftpcmd.txt
echo quit >> ftpcmd.txt


REM user ... ...
REM mkdir %sitename%
REM cd %sitename%
REM mput *.html
REM put wrk\renamefile.rf
REM put wrk\delta.df
REM quit
ftp -n -i -s:ftpcmd.txt


CLS
ECHO.
ECHO.
ECHO.
ECHO.
ECHO "resources.zip is file" .\resources.zip
EcHO "programs.zip is file: " .\programs.zip

ECHO "Validating prior install..."
REM 	Check for existence of databases, rcode folder, DLC folder

REM set rootfolder=...

mkdir %rootfolder%\resources
mkdir %rootfolder%\programs
mkdir %rootfolder%\asiutil

REM uncompress rcode to appropriate place
cd %rootfolder%
powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::ExtractToDirectory('U:\PATCH16.0.1\programs.zip', 'Programs'); }"
powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::ExtractToDirectory('U:\PATCH16.0.1\resources.zip', 'Resources'); }"
powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::ExtractToDirectory('U:\PATCH16.0.11\LnkParser.zip', '.\asiutil\LnkParser'); }"
powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::ExtractToDirectory('U:\PATCH16.0.1\GnuWin32.zip', '.\asiutil\GnuWin32'); }"
ECHO Completed extracting zip files, press spacebar to continue
pause
copy /Y .\*.d rcode
copy /Y .\*.lst rcode

REM run test of database connectivity based on extract of values from conmgr.properties
REM dbman -db asi -S ... -H ...

REM validate database schema identical to model schema for each database
REM Update VST's	




CLS
ECHO.
ECHO.
ECHO.
ECHO.
ECHO Notify users to log off of the system and press space bar
PAUSE
echo Removing old rcode




REM verify backups are valid by restoring to test database
REM Valicate install via DIR command and calculation of a CRC
REM Future: clean up log folders, lg files, 
REM Delete old version (2 versions back) xx.xx.xx?
REM Email install log?
REM Cleaning update temporary files (CRC, ZIP files)


REM Inform User that install is complete - Give follow-up instructions if needed
cls
echo The installation is now complete. You may close this window at any time.
echo.
echo.
echo.
echo press spacebar to continue
pause

REM clean up and exit
goto exit

:ERROR
CLS
ECHO An error occurred, aborting install...
PAUSE 
exit

:done
REM nothing to do
:exit

REM MSIEXEC.EXE /I "%~dp0SETUP.MSI" /TRANSFORMS="%~dp0SETUP.MST" /QB! /L*V "%LOGSFOLDER%"
REM set MSIERROR=%errorlevel%
REM if %MSIERROR%==0 GOTO :ENDHERE
REM if %MSIERROR%==1641 GOTO :ENDHERE
REM if %MSIERROR%==3010 GOTO :ENDHERE
REM GOTO :ERROR
REM ================================================================================ 
REM ================ Installation successful. Write to Event Log====================
REM :ENDHERE
REM EVENTCREATE /l Application /so %UPN%-Install-SUCCESS /t SUCCESS /id 1000 /d "Application installed REM successfully."
REM Exit 0
REM ================================================================================

REM ================ Installation failed. Write to Event Log========================
REM :ERROR
REM EVENTCREATE /l Application /so %UPN%-Install-FAILED--(ERROR=%MSIERROR%) /t ERROR /id 999 /d REM "Application installation failed."
REM Exit %MSIERROR%
REM ================================================================================


