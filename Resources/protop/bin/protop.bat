@echo off

call C:\Users\Administrator.WIN-SIDPMPLNDIB\Downloads\protop\bin\protopenv.bat

cd /d %PROTOP%

rem	set the screen to support protop bigger than 160x48 is ok
rem	  - smaller is not
rem
rem	picking a decent font will also help, the default fonts
rem	are awful but lucida 12 and consola 16 are pretty good
rem

mode con cols=%COLS% lines=%ROWS%

rem	chain togther .pf files if they exist
rem
rem	      etc/protop.pf
rem	      etc/custId.pf
rem	      etc/serverName.pf
rem	      etc/type.pf
rem	      etc/friendlyName.pf
rem	

set PFLIST=

if exist %PROTOP%\etc\protop.pf (
	set PFLIST=%PFLIST% -pf etc\protop.pf
)

set /p CUSTID=<etc\custid.cfg

if exist %PROTOP%\etc\%CUSTID%.pf (
	set PFLIST=%PFLIST% -pf etc\%CUSTID%.pf
)

if exist %PROTOP%\etc\%HOSTNAME%.pf (
	set PFLIST=%PFLIST% -pf etc\%HOSTNAME%.pf
)

set XDB=%1
set FNAME=-param %1

rem	echo %XDB%
rem	echo %FNAME%
rem	sleep 5

if exist %PROTOP%\etc\dblist.cfg (

	echo. > %TEMP%\pt3dblist.tmp
	grep "^%1|" %PROTOP%\etc\dblist.cfg | gawk -F "|" "{print $2}" > %TEMP%\pt3dblist.tmp
	set /p XDB=<%TEMP%\pt3dblist.tmp

	echo. > %TEMP%\pt3dblist.tmp
	grep "^%1|" %PROTOP%\etc\dblist.cfg | gawk -F "|" "{print $6}" > %TEMP%\pt3dblist.tmp
	set /p XTYPE=<%TEMP%\pt3dblist.tmp

	if exist %PROTOP%\etc\%XTYPE%.pf (
		set PFLIST=%PFLIST% -pf etc\%XTYPE%.pf
	)

)

if exist %PROTOP%\etc\%1.pf (
	set PFLIST=%PFLIST% -pf etc\%1.pf
)

if %XDB% == -1 (
	%DLC%\bin\_progres.exe -p protop.p %FNAME% %PFLIST% -T %TMPDIR% -basekey "INI" -ininame etc\protop.ini %XDB% %2 %3 %4 %5 %6 %7 %8 %9
) else if exist %XDB%.db (
	%DLC%\bin\_progres.exe -p protop.p %FNAME% %PFLIST% -T %TMPDIR% -basekey "INI" -ininame etc\protop.ini %XDB% %2 %3 %4 %5 %6 %7 %8 %9
) else (
	echo You must provide a friendly name that is defined in etc\dblist.cfg
	sleep 10
)
