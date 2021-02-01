::@echo off

SET iniFile=..\Admin\advantzware.ini
SET tgtDir=..\Admin\EnvAdmin
SET SQLFile=..\Admin\SQLparms.txt
if not exist %SQLFile% goto err_end2

:: Read advantzware.ini and set variable values
FOR /F "tokens=1,2 delims==" %%G IN (%iniFile%) DO SET %%G=%%H
:: Read SQLFi8le and set SQL variable values
FOR /F "tokens=1,2 delims==" %%G IN (%SQLFile%) DO SET %%G=%%H

set DLC=%DLCDIR%
set WRKDIR=%DLCDIR%WRK
set OEM=%DLCDIR%oemgmt
set OEMWRKDIR=%DLCDIR%wrk_oemgmt
set OPENSSL_CONF=%DLC%\keys\policy\pscpki.cnf
set BPSERVER_BIN=%DLC%\oebpm\server\bin
if x%1 == xpsc goto setit

if not exist %DLC%\bin\proenv.bat goto err_end
if not "%OS%"=="Windows_NT" set ENVSIZE=/E:5120
if exist %COMSPEC% goto csok
echo.
echo COMSPEC is not defined correctly, the current definition is:
echo COMSPEC=%COMSPEC%
echo.
pause
exit 

:csok
::%COMSPEC% %ENVSIZE% /K %DLC%\bin\proenv.bat psc
::if not x%1 == xpsc goto tell

:setit
   set PATH=%DLC%\BIN;%BPSERVER_BIN%;%DLC%\PERL\BIN;%PATH%
   set LIB=%DLC%\LIB;%LIB%
::   if x%1 == xpsc prompt proenv$g
   cls
   goto tell

:err_end
   echo.
   echo DLC variable is not correct - Please check setting in (%0)
   echo.
   exit

:err_end2
   exit

:tell
cls

SETLOCAL
REM Progress SQL Explorer Startup script

REM Directory where the Product is installed
if "%DLC%"==""         set DLC=%DLCDIR%
if exist "%DLC%\version" goto BIN
   echo.
   echo DLC environment variable not set correctly - Please set DLC variable
   echo.
   pause
   goto END

:BIN
set SQLEXPGUICLASS=com.progress.vj.sql.explorer.SQLExplorerGUI
set SQLEXPCLASS=com.progress.sql.explorer.SQLExplorer
set TOOLSPROP=%DLC%\properties\JavaTools.properties
set TOOLSGRP=SQLExplorerCLI
set TOOLSGUIGRP=SQLExplorer

:JSTART
set JVMSTRT="%DLC%\bin\jvmStart.exe"
if exist %JVMSTRT% goto SETJVM
   cls
   echo Progress SQL Explorer %0 Messages:
   echo.
   echo Java Starter could not be found.
   echo.
   echo Progress DLC environment variable may not be set correctly.
   echo Set DLC variable to Progress installation directory.
   echo.
   echo Progress DLC setting: %DLC%
   echo Java Starter not found: %JVMSTRT%
   echo.
   pause
   goto END


:SETJVM
REM if Java environment variables set, use them
REM otherwise use the registry
if "%JREHOME%"=="" goto USEREG

:USEENV
set JVM=%JREHOME%\bin\java
set WRK=%WRKDIR%
goto SETPARAMS


:USEREG
set JVM=@{JAVA\JREHOME}\bin\@{JAVA\JVMEXE}
set WRK=@{WorkPath}
goto SETPARAMS

:SETPARAMS
   set PARMS=-char -infile .\Structure\allFiles.SQL -outfile .\Structure\report.txt -db %dbName% -S %dbPort% -H localhost -user %cusername% -password %cpassword%
   set PARMS=%PARMS:@=~@%

:START
::   type "%DLC%"\version
CALL %JVMSTRT% -a "%TOOLSPROP%"::%TOOLSGRP% -o NUL -s -m silent "%JVM%" -DInstall.Dir="%DLC%" %PROSQLJVMARGS% %SQLEXPCLASS% %PARMS% > NUL

:END
::EXIT