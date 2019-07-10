@ECHO OFF
REM ---------------------------------------------------------
REM   Name:      logrotate.bat
REM   Author:    Paul Koufalis
REM   Written:   2017-01-12
REM   Purpose:   Log rotater and archiver for Progress Openedge log files.
REM              Optionally filters out strings when archiving
REM
REM     Usage:   logrotate.bat <full path> <friendlyName>
REM
REM   bin/localenv.bat variables:
REM
REM      LOGHIST=<# weeks of full logs to keep>
REM      FLOGHIST=<# weeks of filtered logs to keep>
REM      LGARCDIR=<Log archive dir>
REM      LOGFILTER=<File containing list of strings to filter out, one msg per line>
REM                ex.: (8873)
REM                     (452)
REM                     (453)
REM 
REM ---------------------------------------------------------

REM Set EKKO=echo for debug
REM set EKKO=

 
REM Change false to true for verbose debugging displays
SET DEBUG=false

REM Delay variable expansion to runtime
Setlocal EnableDelayedExpansion

REM Set environment variables 
CALL #ENVPT3#\bin\protopenv.bat

set RLOG=%LOGDIR%\logrotate.log
echo ################### STARTING %DATE% %TIME% ################### >> "%RLOG%"
  
cd "%PROTOP%"

REM Sanity check command line parameters
 
IF "%~1" == "" (
   CALL :SUB_USAGE
   EXIT /b 1
) else SET IP1=%~1

SET FILTER=false   
if defined LOGFILTER (
   SET "FILTER=true"
)

set DB=%~1
set FRNAME=%~2

if not exist !DB!.db (
   echo Database !DB! does not exist
   exit /b
)

"%PROTOP%\ubin\basename.exe" !DB! > "%TMPDIR%\tmpbasename" 2>nul
set /p DBNAME=<"%TMPDIR%\tmpbasename"
del "%TMPDIR%\tmpbasename"

"%PROTOP%\ubin\dirname.exe" !DB! > "%TMPDIR%\tmpdirname" 2>nul
set /p DBDIR=<"%TMPDIR%\tmpdirname"
del "%TMPDIR%\tmpdirname"

IF "%DEBUG%"=="true" (
   echo DB=!DB!
   echo FRNAME=!FRNAME!
   echo DBNAME=!DBNAME!
   echo DBDIR=!DBDIR!
   set/p x=Press enter to continue
)   
    
REM Sanity check localenv.bat variables

IF NOT DEFINED LGARCDIR (
  echo LGARCDIR not set. Default to DB dir !DBDIR!
  set LGARCDIR=!DBDIR!
)

if not defined LOGHIST (
  echo Log history weeks not set. Default to 52 weeks.  >> "%RLOG%"
  set LOGHIST=52
)

set /A isInt = %LOGHIST% 2>NUL
if not %isInt% == %LOGHIST% (
  echo LOGHIST=%LOGHIST% must be an integer
  exit /b 1
)

if not defined FLOGHIST (
  echo Filtered log history weeks not set. Default to 52 weeks.. >> "%RLOG%"
  set FLOGHIST=52
)

set /A isInt = %FLOGHIST% 2>NUL
if not %isInt% == %FLOGHIST% (
  echo FLOGHIST=%FLOGHIST% must be an integer
  exit /b 1
)

IF "%DEBUG%"=="true" (
   echo FILT = %FILT%
   echo LOGHIST = %LOGHIST%
   echo FLOGHIST = %FLOGHIST%
   echo LOGFILTER=%LOGFILTER%
   echo LGARCDIR = %LGARCDIR%
   set/p x=Press enter to continue
)

REM   Set up all the date and time fields using localised formats
REM   Specific path below as it was using system date command and hanging
"%PROTOP%\ubin\date.exe" +%%W>"%TMPDIR%\MYWK"
SET /p WK=<"%TMPDIR%\MYWK"
DEL "%TMPDIR%\MYWK"

IF "%DEBUG%"=="true" (
   echo Week Number is %WK%
   set /p x=Press enter to continue
)

IF "%DEBUG%"=="true" (
   echo OUTSIDE
   echo FRNAME = !FRNAME!
   echo DB=!DB!
   echo DBNAME=!DBNAME!
   set /p x=Press enter to continue
)

REM Purge LGARCDIR
 
IF "%DEBUG%"=="true" (
   echo LOGHIST = %LOGHIST%
   echo FLOGHIST = %FLOGHIST%
   echo RLOG = %RLOG%
   set /p x=Press enter to continue
)
 
set /a LOGDAYS="%LOGHIST% * 7"
set /a FLOGDAYS="%FLOGHIST% * 7"
IF NOT "%LOGDAYS%"=="0" (
   echo Cleaning up any archived log files older than %LOGHIST% weeks >>"%RLOG%" 2>&1
   forfiles /p "%LGARCDIR%" /m !DBNAME!.lg.* /c "cmd /c %EKKO% del @path" /d -%LOGDAYS% 2>NUL
)
IF NOT "%FLOGDAYS%"=="0" (
   echo Cleaning up any filtered archived log files older than %FLOGHIST% weeks >>"%RLOG%" 2>&1
   forfiles /p "%LGARCDIR%" /m !DBNAME!.lgf.* /c "cmd /c %EKKO% del @path" /d -%FLOGDAYS% 2>NUL
)
 
REM Check if DB is online
%EKKO% call proutil "%DB%" -C holder >NUL 2>&1
IF ERRORLEVEL 16 ( 
   set ONLINE=-online
) else (
    if errorlevel 0 (
	   set ONLINE=
    ) else (
       echo DB not available for prolog %ERRORLEVEL%
       exit /b
	  )
)

IF "%DEBUG%"=="true" (
   echo ONLINE=%ONLINE%
)   


REM Prolog db.lg
echo %DATE% %TIME% Rolling log for %FRNAME% >>"%RLOG%" 2>&1

IF "%DEBUG%"=="true" (
   echo DB = %DB%
   echo DBNAME = %DBNAME%
   set /p x=Press enter to continue
)
  
%EKKO% copy "%DB%.lg" "%LGARCDIR%\%FRNAME%.lg.%WK%" >> "%RLOG%" 2>&1
IF NOT ERRORLEVEL 0 (
   echo Copy of database log %DBNAME%.lg failed - exiting
   exit /b
)

%EKKO% call prolog "%DB%" %ONLINE% >> "%RLOG%" 2>&1

REM IF NOT ERRORLEVEL 0 (
REM   %EKKO% CALL "%PROTOP%\bin\sendalert" %FRNAME% -m prolog -type alarm -msg "prolog failed during log roll"
REM    exit /b
REM ) else (
REM    %EKKO% CALL "%PROTOP%\bin\sendalert" %FRNAME% -m prolog -type info -msg "prolog completed successfully"
REM )
  
IF "%FILTER%"=="true" (
   echo Filtering %LGARCDIR%\%FRNAME%.lg.%WK% to %LGARCDIR%\%FRNAME%.lgf.%WK% >> "%RLOG%"
   %EKKO% grep -v -f "%LOGFILTER%" "%LGARCDIR%\%FRNAME%.lg.%WK%"      > "%LGARCDIR%\%FRNAME%.lgf.%WK%"
)

EXIT /B
 
:SUB_USAGE
   ECHO.
   ECHO "Usage: logrotate.bat <full DB path> <friendlyName>"
   ECHO.
   EXIT /B
 

