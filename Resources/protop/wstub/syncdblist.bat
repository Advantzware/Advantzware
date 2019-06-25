@echo off

call #ENVPT3#\bin\protopenv.bat

cd /d %PROTOP%

%DLC%\bin\_progres.exe -b -p util\syncdblist.p -pf %PROTOP%\etc\protop.pf -T %TMPDIR% -basekey "INI" -ininame etc\protop.ini >> %LOGDIR%\syncdblist.log 2>&1


