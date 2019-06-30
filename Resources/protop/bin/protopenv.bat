@echo off

if "%DLC%"=="" set DLC=C:\Progress\OE116_64

set PROTOP=C:\Users\Administrator.WIN-SIDPMPLNDIB\Downloads\protop
set LOGDIR=C:\Users\Administrator.WIN-SIDPMPLNDIB\Downloads\protop\log
set TMPDIR=C:\Users\Administrator.WIN-SIDPMPLNDIB\Downloads\protop\tmp
set RPTDIR=C:\Users\Administrator.WIN-SIDPMPLNDIB\Downloads\protop\rpt
set PROXY=

rem	these are now set in bin\portalenv.bat
rem

rem	set PTHOST=demo.wss.com
rem	set PTUPD=demo.wss.com

set PROPATH=%PROTOP%
set PT3=%PROTOP%
set DBA=%PROTOP%

rem	for compatibility with the UNIX protopenv
rem

set PTTMP=%TMPDIR%

rem     set the screen to support protop with more than 48 lines
rem     is ok, recommended even - smaller is not
rem
rem     picking a decent font will also help, the default fonts
rem     are awful but lucida 12 and consola 16 are pretty good
rem
rem	(you may also want to adjust protop.ini)
rem
rem     the following is usually appropriate:

set ROWS=48
set COLS=160

rem     while simple, this approach results in PATH constantly growing
rem

rem set PATH=%DLC%\bin;%PROTOP%\bin;%PROTOP%\ubin;%PATH%

rem     more complicated but it preserves whatever the local PATH is ...
rem

rem call "%PROTOP%"\bin\addpath.bat "%DLC%\bin"
rem call "%PROTOP%"\bin\addpath.bat "%PROTOP%\bin"
rem call "%PROTOP%"\bin\addpath.bat "%PROTOP%\ubin"

rem     the local PATH is usually full of junk so the
rem     best option is to just go with the minimum:
rem

set PATH=%DLC%\bin;%PROTOP%\bin;%PROTOP%\ubin;C:\Windows\system32;C:\Windows;C:\Windows\System32\Wbem

if exist %PROTOP%\bin\portalenv.bat (
	call %PROTOP%\bin\portalenv.bat
)

if exist %PROTOP%\bin\dbaenv.bat (
	call %PROTOP%\bin\dbaenv.bat
)

if exist %PROTOP%\bin\localenv.bat (
	call %PROTOP%\bin\localenv.bat
)

mode con cols=%COLS% lines=%ROWS%
