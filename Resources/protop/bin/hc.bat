@echo off

call C:\Users\Administrator.WIN-SIDPMPLNDIB\Downloads\protop\bin\protopenv.bat

cd /d %PROTOP%

set SENDTYPE=JSON

set XDB=%1
set P1=%1

if exist %PROTOP%\etc\dblist.cfg (

	echo. > %TEMP%\pt3dblist.tmp
	grep "^%1|" %PROTOP%\etc\dblist.cfg | gawk -F "|" "{print $2}" > %TEMP%\pt3dblist.tmp
	set /p XDB=<%TEMP%\pt3dblist.tmp

)

if exist %XDB%.db (
	del /q %LOGDIR%\hc.%P1%.log 2>NUL
	%DLC%\bin\_progres.exe -b -p util\healthcheck.p -pf %PROTOP%\etc\protop.pf -T %TMPDIR% -basekey "INI" -ininame etc\protop.ini %XDB% -param "%P1%" >> %TMPDIR%\healthcheck.%P1%.e2 2>&1
)
