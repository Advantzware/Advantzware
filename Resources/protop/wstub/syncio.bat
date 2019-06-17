@echo off

call #ENVPT3#\bin\protopenv.bat

if NOT "%1" == "" (
	set SYNCIODIR=%1
)

cd /d %SYNCIODIR% 2>NUL

FOR /F "tokens=* USEBACKQ" %%F IN (`type %DLC%\version ^| gawk "{ print $3 }" ^| gawk -F. "{print $1}"`) do (SET PROVERSION=%%F)

if %PROVERSION% == 11 (
	set ZEXTENDSYNCIO=-zextendSyncIO
) else (
	unset ZEXTENDSYNCIO
)

if exist sports.db (
	echo y | call prodel sports > NUL 2>&1
)

call prodb sports sports > NUL 2>&1
call proutil sports -C truncate bi -bi 16384 > NUL 2>&1

rem	Progress version 10 does not have (nor does it need) -zextendSyncIO
rem
rem	ptime proutil sports -C  bigrow 2

ptime proutil sports -C bigrow 2 %ZEXTENDSYNCIO% | tail -1 | gawk "{print $3}" > syncio.tmp
set /p SYNCIO=<syncio.tmp

%PROTOP%\ubin\date "+%%Y/%%m/%%d %%T " > myts.tmp
set /p MYTS=<myts.tmp

echo %MYTS% %SYNCIO% %SYNCIODIR% | tee -a %LOGDIR%\syncio.log

echo.
echo 9 seconds =  10MB/sec -- anything longer and your disk susbsystem is junk
echo 5 seconds =  20MB/sec -- not horrible
echo 3 seconds =  30MB/sec -- good
echo 1 second  = 100MB/sec -- excellent!
echo.

echo y | call prodel sports > NUL 2>&1
del /q sports.st 2>NUL

cd /d %PROTOP%
