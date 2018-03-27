REM ================================================================================ 
REM InstallProgress.bat
REM Version 1.0 06/01/2017 MYT
REM ================================================================================ 
REM This batch file will allow a user to choose from a menu of options, allowing:
REM 1. Installation of a new Progress version (default is 11.6)
REM 2. Uninstall of an older Progress version (default is 10.2B)
REM 3. Both of the above
REM
REM Information needed for the install is:
REM 1. Location of the new Progress setup folder/files
REM 2. Target location for the installation (first time only)
REM 3. Location of the Progress license file document (first time only)
REM (If a license document is not available, user will be prompted for serial
REM  number and access codes during interactive install)
REM
REM

@ECHO OFF
:START
CLS
ECHO *********************************************************************
ECHO Please choose from one of the following options:
ECHO 1. Install new DBMS version
ECHO 2. Uninstall old DBMS version
ECHO 3. Perform both above actions
ECHO *********************************************************************
SET /P doAction="Enter the number of your choice here (1,2,3): "

IF "%doAction%" == "1" (
    GOTO :GETINFO1
    )
IF "%doAction%" == "2" (
    GOTO :GETINFO2
    )
IF "%doAction%" == "3" (
    GOTO :GETINFO1
    )
ECHO You must enter one of the valid choices
PAUSE
GOTO :START

:GETINFO1
CLS
REM 32- or 64-bit install?
SET bit64=N
IF /I "%Processor_Architecture%"=="AMD64" SET bit64=Y
IF /I "%PROCESSOR_ARCHITEW6432%"=="AMD64" SET bit64=Y
IF %bit64% == N (
    ECHO Detected that this is a 32-bit machine.
    ECHO Can only install the 32-bit version of Progress here.
    )
IF %bit64% == Y (
    ECHO Detected that this is a 64-bit machine.
    ECHO Can install either the 32-bit or 64-bit version of the DBMS.
    ECHO The default is to install the 64-bit version.
    SET /P bit64="Enter 'N' (without quotes) to install the 32-bit version instead: "
    )

IF %bit64% == Y (
    ECHO Enter the location of your 64-bit installation files.
    )
IF %bit64% == N (
    ECHO Enter the location of your 32-bit installation files.
    )
ECHO -Do NOT include a slash (/,\) at the end of this entry-
SET /P fileLoc=":"
IF NOT EXIST "%fileLoc%\setup.exe" (
    ECHO Cannot locate a setup.exe file in this location.
    ECHO Please try again
    PAUSE
    GOTO :GETINFO1
    )

IF NOT EXIST "%fileLoc%\response.ini" (
    ECHO This is the first time using this install directory.
    ECHO You will need to run the installation interactively
    ECHO for the first install.  For later installs, the process
    ECHO may be run "silently" with no user action required.
    PAUSE
    "%fileLoc%\setup.exe"
    CLS
    ECHO You should now manually copy the latest 'response.ini" file from
    ECHO the new directory [install subdirectory] to a file named
    ECHO "response.ini" in the %fileLoc%\ directory. 
    ECHO NOTE: The "correct" response file may have a different extension.
    ECHO Use the "largest" file in this group.
    PAUSE
    IF "%doAction%" == "1" (
        GOTO :END
        )
    )    

CLS
ECHO About to start installation of your Advantzware DBMS
ECHO This process can take 15 - 20 minutes depending on the components installed
ECHO Please be patient.  You will see additional messages when the process
ECHO completes, whether successfully or not
ECHO .
ECHO .
   
START "" /b "%fileLoc%\setup.exe" -psc_s -psc_f1="%fileLoc%\response.ini" -psc_f2="c:\temp\install.log"
GOTO LOOP:

:LOOP
ping localhost -n 11 > nul
FOR /f "eol=[ tokens=* usebackq" %%a in ("c:\temp\install.log") do (
    SET %%a
    FOR /f "delims== tokens=1,2" %%b in ("%%a") do (
        IF "%%b" == "Progress" SET pctComp=%%c
        )
    )
IF "%pctComp%" == "99 " (
    GOTO :LOOP2
    )
CLS
ECHO About to start installation of your Advantzware DBMS
ECHO This process can take 15 - 20 minutes depending on the components installed
ECHO Please be patient.  You will see additional messages when the process
ECHO completes, whether successfully or not
ECHO .
ECHO Percent complete = %pctComp%%%
GOTO :LOOP

:LOOP2
ping localhost -n 11 > nul
FOR /f "eol=[ tokens=* usebackq" %%d in ("c:\temp\install.log") do (
    SET %%d
    FOR /f "delims== tokens=1,2" %%e in ("%%d") do (
        IF "%%e" == "ResultDescription" SET pctComp2=100
        )
    )
IF "%pctComp2%" == "100" (
    GOTO :NEXT
    )
CLS
ECHO Now tailoring files for your installation.  This should only take a few minutes.
ECHO .
ECHO .
GOTO :LOOP2

:NEXT
IF "%doAction%" == "1" (
    GOTO :END
    )
    
:GETINFO2
ECHO About to start removing old Progress version
ECHO .
IF bit64 == N (
    ECHO Cannot automatically remove a 32-bit version of the DBMS from this machine.
    ECHO You should use Control Panel | Programs to remove manually.
    PAUSE
    GOTO :END
    )

SET /P doRemove="Are you sure? (Y/N): "
IF "%doRemove%" == "y" (
    SET doRemove=Y
    )
IF NOT "%doRemove%" == "Y" (
    GOTO :END
    )
    
ECHO .
ECHO .
ECHO About to start REMOVAL of your old Advantzware DBMS
ECHO This process can take 15 - 20 minutes depending on the components installed
ECHO Please be patient.  You will see additional messages when the process
ECHO completes, whether successfully or not
ECHO .
ECHO .

IF EXIST "C:\Program Files (x86)\InstallShield Installation Information\{69E3F70C-32B2-47A8-A70D-653D16ECEFC2}\setup.exe" (
    "C:\Program Files (x86)\InstallShield Installation Information\{69E3F70C-32B2-47A8-A70D-653D16ECEFC2}\setup.exe" -psc_s -runfromtemp -l0x0009 -removeonly
    )
IF EXIST "C:\Program Files\InstallShield Installation Information\{69E3F70C-32B2-47A8-A70D-653D16ECEFC2}\setup.exe" (    
    "C:\Program Files\InstallShield Installation Information\{69E3F70C-32B2-47A8-A70D-653D16ECEFC2}\setup.exe" -psc_s -runfromtemp -l0x0009 -removeonly
    )
CLS
ECHO Old DBMS version should now be removed
GOTO :END    

:END
DEL c:\temp\install.log > NUL
ECHO Your DBMS installation should now be complete.
ECHO Please verify the installation before continuing.
PAUSE
