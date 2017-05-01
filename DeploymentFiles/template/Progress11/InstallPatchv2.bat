
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


SET /P askUpgrade="Do you wish to continue to install the patch for Version  %InstallVersion%?"
IF NOT %askUpgrade% == Y GOTO :exit

SET sitename=%1
SET dbDir=%~f2
SET progressDir=%~f3
SET backupDir=%~f4
SET rootfolder=%5
SET rcodefolder=%6
SET isUpgrade=%7
SET deltaDb=%8
SET asiPF=%9
shift
SET nosweatPF=%9

echo "sitename" %sitename%
echo "dbdir " %dbdir%
echo "progressDir " %progressDir%
echo "Backup " %backupDir%
echo "root folder " %rootfolder%
echo "rcodefolder " %rcodefolder%
echo "Is Upgrade? " 
echo "is upgrade " %isupgrade% 
echo "delta d00f " %deltaDb% 
echo "asi pf " %asiPF% 
echo "nosweat pf" %nosweatPF%

pause


IF NOT EXIST %dbDir% (
  ECHO Location of databases not found, please start script again
  ECHO Press spacebar to continue
  goto END:
)

%progressDir%/bin/_progres -b -p getDbConnection.p -param %asiPF% 
%progressDir%/bin/_progres -b -p getDbConnection.p -param %nosweatPF% 



echo Check the pf files
pause 




set AsiPublicFtpSite=ftp.advantzware.com
set AsiPublicFtpPwd=asi215 Yorkie12\301oxford
set InstallLog=.\%InstallVersion%Install.log

IF %isUpgrade% == yes GOTO :STARTUNCOMPRESS


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

ECHO "Site: " %sitename%
ECHO "DB Folder: " %dbDir%
ECHO "Progress Folder: " %progressDir%
ECHO "Backup Folder: " %backupDir%
ECHO "Root Folder (Contains rcode folder): " %rootfolder%
ECHO "Rcode Folder name: " %rcodefolder%


SET /P makeBackup="Do you wish to backup the existing rcode?"

IF NOT %makeBackup% == Y GOTO :ENDBACKUP


mkdir %backupDir%

xcopy /s /E /I /Y %rootfolder%\%rcodefolder% %backupDir%

:ENDBACKUP



REM CLS
ECHO.
ECHO.
ECHO.
ECHO.

ECHO Please confirm that backup of the rcode folder completed successfully and press spacebar
pause



ECHO Insert this into rcode\progress.ini: 
ECHO.
ECHO      N:\programs,N:\programs\addon,N:\Resources,
ECHO      (substitute actual drive letter.  Insert right after the ., )
ECHO.
ECHO Then Press spacebar to continue
ECHO.
PAUSE

ECHO Make sure rcode\nosweat.pf has '-p nosweat.p' and not '-p start.p', then press spacebar
ECHO Then Press spacebar to continue
PAUSE

SET /P askReady="Are you ready to start install?"
IF NOT %askReady% == Y GOTO :exit


REM ================================================================================ 
REM ================ Extract utility folder & .r's for use in this script ==========



REM ================================================================================ 
REM Clear the DOS Box and display detailed message for End User
REM Explain what is being installed and why
REM Give expected duration of install
REM Include any instructions that the end user needs to know about.
rem CLS
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
echo If you have any questions, please contact Advantzware software support
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


SET /P askAnalyze="Capture system information?"
IF NOT %askAnalyze% == Y GOTO :EXPST

ECHO Capturing system information to systemSummary.txt
msinfo32 /report .\systemSummary.txt /categories +systemsummary

rem goto :COPYST

:EXPST
echo Start Export DB Extent info

REM Export current database extents, % full
%progressDir%\prostrct list %dbDir%\asi
%progressDir%\prostrct list %dbDir%\nosweat
%progressDir%\prostrct list %dbDir%\asihelp
%progressDir%\prostrct list %dbDir%\addons\nosweat
%progressDir%\prostrct list %dbDir%\addons\jobs
%progressDir%\prostrct list %dbDir%\addons\emptrack
%progressDir%\prostrct list %dbDir%\addons\rfq


:COPYST
echo Start Copy of .st files
copy %dbDir%\asi.st .
copy %dbDir%\nosweat.st .
copy %dbDir%\asihelp.st .
copy %dbDir%\addons\nosweat.st .\nosweat-add.st
copy %dbDir%\addons\emptrack.st .
copy %dbDir%\addons\jobs.st .
copy %dbDir%\addons\rfq.st .


ECHO Exporting .st files for reference:
dir *.st

REM 	nosweat.p modified to add to propath based on advanztware.ini
REM 	extract dlc value from advantzware.ini
REM 	extract location of test databases and code from advantzware.ini

REM 	Run utility to convert recid to rec_key, needs to be in rcode for asi.pf
pause 



echo Start running utility:
%progressDir%\bin\prowin32.exe -b -pf %rootfolder%\%rcodefolder%\nosweat-dev.pf -p .\fixreffe.r -param %rootfolder%\%rcodefolder%
pause



ECHO Verify that a .d file was created in the current folder


REM     Run utility to extract info about extents

REM run current application, run utility to export current propath, start-in folder
@echo off 
REM This line originally specified a drive letter
set DUMP_INC_DFFILE=.\wrk\delta.df 
set DUMP_INC_CODEPAGE=1252 
set DUMP_INC_INDEXMODE=active 
set DUMP_INC_RENAMEFILE=.\wrk\renamefile.rf 
set DUMP_INC_DEBUG=0 
set PROPATH=.\prodict


echo Start delta.df creation:
REM %DLC%/bin/_progres -b -db master -1 -db slave -1 -p prodict/dump_inc.p > D:\wrk\dump_inc.log 
C:\Progress\OpenEdge/bin/_progres -b -db emptrack -1 -db %dbDir%\addon\emptrack -ld slave -p prodict/dump_inc.p  1>dump_inc.log
copy wrk\delta.df emptrack-delta.df
C:\Progress\OpenEdge/bin/_progres -b -db jobs -1 -db %dbDir%\addon\jobs -ld slave -p prodict/dump_inc.p  1>dump_inc.log
copy wrk\delta.df jobs-delta.df
C:\Progress\OpenEdge/bin/_progres -b -db nosweat -1 -db %dbDir%\nosweat -ld slave -p prodict/dump_inc.p  1>dump_inc.log
copy wrk\delta.df nosweat-delta.df
C:\Progress\OpenEdge/bin/_progres -b -db nosweatadd -1 -db %dbDir%\addon\nosweat -ld slave -p prodict/dump_inc.p  1>dump_inc.log
copy wrk\delta.df nosweatadd-delta.df
pause



echo End Of Dump Delta Definition File 
echo Start Ftp
echo open ftp.advantzware.com > ftpcmd.txt
echo user asi215 Yorkie12\301oxford >> ftpcmd.txt
echo put systemSummary.txt >> ftpcmd.txt
echo mkdir %sitename% >> ftpcmd.txt
echo cd %sitename% >> ftpcmd.txt
echo mput *.html >> ftpcmd.txt
echo put wrk\renamefile.rf >> ftpcmd.txt
echo mput *.df >> ftpcmd.txt
echo mput *.st >> ftpcmd.txt
echo quit >> ftpcmd.txt


REM user ... ...
REM mkdir %sitename%
REM cd %sitename%
REM mput *.html
REM mput *.ini
REM mput *.st
REM put wrk\renamefile.rf
REM put wrk\delta.df
REM quit
ftp -n -i -s:ftpcmd.txt


rem CLS
ECHO.
ECHO.
ECHO.
ECHO.
ECHO "resources.zip is file" .\resources.zip
EcHO "programs.zip is file: " .\programs.zip

ECHO "Validating prior install..."
REM 	Check for existence of databases, rcode folder, DLC folder

REM set rootfolder=...
IF NOT EXIST %rootfolder%\resources mkdir %rootfolder%\resources
IF NOT EXIST %rootfolder%\programs mkdir %rootfolder%\programs
IF NOT EXIST %rootfolder%\asiutil mkdir %rootfolder%\asiutil

:STARTUNCOMPRESS
echo Start of uncompress
SET /P askUncompress="Are you ready to Uncompress Zipped Files?"
IF NOT %askUncompress% == Y GOTO :COPYNEW
ECHo Installing new files, ask users to log off now!
pause
If defined %deltaDb% (
   echo Apply the delta.df manually
   pause 
   
   if %deltaDB%==ASI (
       REM Apply delta.df to ASI database
	   rem %progressDir%/bin/_progres -b -p load.p -zn -pf %asipf%
      )
	  
   if %deltaDB%==Nosweat (
       REM Apply delta.df to NOsweat database
	rem    %progressDir%/bin/_progres -b -p load.p -zn -pf %nosweatpf%
      )	  
   echo Check the databaseName.e file for errors applying the delta.df
   pause 
	  
   )
REM uncompress rcode to appropriate place
.\7-zip\7z.exe x programs.7z -o%rootfolder% -r -Y
pause
.\7-zip\7z.exe x resources.7z -o%rootfolder% -r -Y


echo End uncompress
pause
ECHO Completed extracting zip files , press spacebar to continue
pause




:COPYNEW

REM IF %isUpgrade% == yes GOTO :SCHEDULEBOARD


copy /Y .\*.d %rootfolder%\%rcodefolder%
copy /Y .\*.lst %rootfolder%\%rcodefolder%
copy /Y .\menu.* %rootfolder%\%rcodefolder%
copy /Y .\advantzware.ini %rootfolder%\%rcodefolder%

REM Also copy in Addon menu and .d files
copy /Y .\addon\*.d %rootfolder%\%rcodefolder%\addon
copy /Y .\addon\*.lst %rootfolder%\%rcodefolder%\addon
copy /Y .\addon\menu.* %rootfolder%\%rcodefolder%\addon


IF NOT EXIST %rootfolder%\programs\nosweat\mfvalad.r copy /Y hotfixes\nosweat\mfvalad.r %rootfolder%\programs\nosweat

IF  EXIST %rootfolder%\%rcodefolder%\images\logo1.bmp ren %rootfolder%\%rcodefolder%\images\logo1.bmp %rootfolder%\%rcodefolder%\images\logo1.bak

copy .\nosweat\persist.r %rootfolder%\%rcodefolder%\nosweat
copy .\addon\system\addmain.r %rootfolder%\%rcodefolder%\addon\system
copy .\addon\nosweat\persist.r %rootfolder%\%rcodefolder%\addon\nosweat
copy .\addon\nosweat.r %rootfolder%\%rcodefolder%\addon
copy .\addon\start.r %rootfolder%\%rcodefolder%\addon
copy .\addon\start10.r %rootfolder%\%rcodefolder%\addon
copy .\addon\start1010.r %rootfolder%\%rcodefolder%\addon
copy .\addon\addon2.r %rootfolder%\%rcodefolder%\addon
copy .\addon\addon3.r %rootfolder%\%rcodefolder%\addon
copy .\addon\clockio.r %rootfolder%\%rcodefolder%\addon
copy .\addon\start.r %rootfolder%\%rcodefolder%\addon
copy .\addon\sharpsh.r %rootfolder%\%rcodefolder%\addon
copy .\addon\sharpsh2.r %rootfolder%\%rcodefolder%\addon

REM Bounce Appserver for AOA
call aoainstall.bat %progressDir% %rootfolder%\resources

REM run test of database connectivity based on extract of values from conmgr.properties
REM dbman -db asi -S ... -H ...

REM validate database schema identical to model schema for each database
REM Update VST's	

echo.
echo.
echo.
echo Please check each icon to verify that it works including addons, sharpshooter and touchscreen icons
echo If it does not work, try replacing '-p start.p' or '-p start1010.p', etc, with '-p nosweat.p'
echo.


rem CLS
ECHO.
ECHO.
ECHO.
ECHO.
ECHO Notify users to log off of the system and press space bar
PAUSE



ECHO Removing old rcode from %rootfolder%\%rcodefolder%
PAUSE
del /s /q %rootfolder%\%rcodefolder%\*.r
REM COPY IN ANY HOTFIXES
xcopy /Y /S  .\hotfixes %rootfolder%\%rcodefolder%

:SCHEDULEBOARD
REM 9/28/2016 Scheduleboard should run without this restore
REM ECHO Restoring schedule folder
REM xcopy /s /i %backupDir%\schedule %rootfolder%\%rcodefolder%\schedule


REM verify backups are valid by restoring to test database
REM Valicate install via DIR command and calculation of a CRC
REM Future: clean up log folders, lg files, 
REM Delete old version (2 versions back) xx.xx.xx?
REM Email install log?
REM Cleaning update temporary files (CRC, ZIP files)


REM Inform User that install is complete - Give follow-up instructions if needed
rem cls
echo The installation is now complete. You may close this window at any time.
echo.
echo.
echo.
echo press spacebar to continue
pause

REM clean up and exit
goto exit

:ERROR
rem CLS
ECHO An error occurred, aborting install...
PAUSE 
exit

:done
REM nothing to do
:exit

:END


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


