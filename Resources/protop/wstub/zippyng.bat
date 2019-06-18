@echo off

call #ENVPT3#\bin\protopenv.bat


set "MNAME=util/zippyng.p"
set "USAGE=Usage: %0 friendlyName shm^|lh^|cs [host] [port]"
cd /d %PROTOP%

REM Delay variable expansion to runtime
Setlocal EnableDelayedExpansion

set FRNAME=%1
set MONTYPE=%2
set HOST=%3
set PORT=%4

if not defined FRNAME (
  echo %USAGE%
  exit /b
)

if not defined MONTYPE (
  echo %USAGE%
  exit /b
)
if not %MONTYPE% == shm  if not %MONTYPE% == lh if not %MONTYPE% == cs (

  echo Montype must be one of shm, lh or cs
  echo %USAGE%
  exit /b
)

REM For localhost (LH), only the port needs to be specified, not the host
if not defined PORT if defined HOST (
  set PORT=%HOST%
  set "HOST="
)

REM Get DB Path
for /f "tokens=* USEBACKQ" %%A IN (`%PROTOP%\bin\getdbpath !FRNAME!`) do (
  set DBPATH=%%A
)
REM echo DBPath=!DBPATH!

if not defined DBPATH if not %MONTYPE% == cs (
  echo DBPATH not found for !FRNAME!
  echo %USAGE%
  exit /b
)

for /f "tokens=* USEBACKQ" %%A IN (`basename !DBPATH!`) do (
  set DBNAME=%%A
)
REM echo DBNAME=!DBNAME! DBPath=!DBPATH!



set FRNAME=%FRNAME%.%MONTYPE%

if %MONTYPE%==lh if not defined PORT (
  echo Please specify a port to connect to
  echo %USAGE%
  exit /b
) else set "XTRA=-S !PORT! -H localhost"


if %MONTYPE%==cs if not defined HOST (
  echo Please specify a host and port to connect to
  echo %USAGE%
  exit /b
) else set "XTRA=-S !PORT! -H !HOST!"

REM ok, we are ready to launch!
REM

REM _progres -b -p util/zippyng.p friendlyName -param friendlyName.cs

REM FRNAME = FRNAME.MONTYPE (see above)

rem	if exist %PTTMP%\zippyng.!FRNAME!.flg (
rem	  echo %PTTMP%\zippyng.!FRNAME!.flg exists. Cowardly refusing to start a redundant session.
rem	  exit /b
rem	)

if not exist !DBPATH!.db if not !MONTYPE!==cs (
  echo Database !DBPATH! does not exist.
  exit /b
)

REM The next two are the same, but since there is no OR syntax I separated them
REM Really there is no zippy with montype=shm so I can probably remove both ifs and simply run _progres ...

if !MONTYPE!==lh (
   start /b _progres -b -p %MNAME% -db !DBNAME! !XTRA! -param !FRNAME! -pf etc/protop.pf -T %PTTMP% >> %PTTMP%\zippyng.!FRNAME!.err 2>&1 
  exit /b
)

if !MONTYPE!==shm (
   start /b _progres -b -p %MNAME% -db !DBPATH! -param !FRNAME! -pf etc/protop.pf -T %PTTMP% >> %PTTMP%\zippyng.!FRNAME!.err 2>&1
  exit /b
)

if !MONTYPE!==cs (

 REM Note: FRNAME includes the ".cs", you will need to replace SERVER: and target /logdir with appropriate values
 REM
 REM  set this in bin\zipenv.bat -- it is documented here for reference
 REM
 REM example: "set ZIPSYNC=copy /y %LOGDIR%\zippyng.!FRNAME!.log \\ServerName\d$\protop\log\zippyng.!FRNAME!.log >> %PTTMP%\zipsync.copy.err 2>&1"
 REM Note the quotes on the outside of the set. That is so the >zipsync.err 2>&1 are part of the ZIPSYNC command

  if exist %PROTOP%\bin\zipenv.bat call %PROTOP%\bin\zipenv
  REM echo ZIPSYNC COMMAND = !ZIPSYNC!
	 
  start /b _progres -b -p %MNAME% -db !DBNAME! !XTRA! -param !FRNAME! -pf etc/protop.pf -T %PTTMP% %ZIPPYMm%  >> %PTTMP%\zippyng.!FRNAME!.err 2>&1
  exit /b
)

`
