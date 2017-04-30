
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
set InstallVersion=16.0.0
set AsiPublicFtpSite=
set AsiPublicFtpPwd=
set InstallLog=.\%InstallVersion%Install.log

REM ================================================================================ 
REM ================ Get Information needed from user           ====================
CLS
ECHO Install Version is %InstallVersion%
REM 	
SET /P InstallVersion="Enter the version number for this patch: "
SET /P askUpgrade="Do you wish to continue to create patch for version  %InstallVersion%?"
IF NOT %askUpgrade% == Y DO GOTO :exit



SET /P askReady="Are you ready?"

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
echo If you have any questions, please contact ...
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
msinfo32 /report c:\tmp\systemSummary.txt /categories +systemsummary



REM 	nosweat.p modified to add to propath based on advanztware.ini
REM 	extract dlc value from advantzware.ini
REM 	extract location of test databases and code from advantzware.ini

REM 	Run utility to convert recid to rec_key
REM     Run utility to extract info about extents

REM run current application, run utility to export current propath, start-in folder

ECHO "resources.zip is file" .\resources.zip
EcHO "programs.zip is file: " .\programs.zip

ECHO "Validating prior install..."
REM 	Check for existence of databases, rcode folder, DLC folder

set rootfolder=...
mkdir %rootfolder%\resources
mkdir %rootfolder%\programs
mkdir %rootfolder%\asiutil

REM uncompress rcode to appropriate place
cd %rootfolder%
powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::ExtractToDirectory('programs.zip', 'Programs%InstallVersion%'); }"
powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::ExtractToDirectory('programs.zip', 'Resources%InstallVersion%'); }"


REM run test of database connectivity based on extract of values from conmgr.properties
REM dbman -db asi -S ... -H ...

REM validate database schema identical to model schema for each database
REM Update VST's	

@echo off 
set DUMP_INC_DFFILE=D:\wrk\delta.df 
set DUMP_INC_CODEPAGE=1252 
set DUMP_INC_INDEXMODE=active 
set DUMP_INC_RENAMEFILE=D:\wrk\renamefile.rf 
set DUMP_INC_DEBUG=0 

%DLC%/bin/_progres -b -db master -1 -db slave -1 -p prodict/dump_inc.p > D:\wrk\dump_inc.log 

echo End Of Dump Delta Definition File 
REM Export current database extents, % full
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

:ERROR
CLS
ECHO An error occurred, aborting install...
PAUSE 
exit

REM clean up and exit
goto exit
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


