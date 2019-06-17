@echo off

call C:\Users\Administrator.WIN-SIDPMPLNDIB\Downloads\protop\bin\protopenv.bat

cd /d %PROTOP%

%DLC%\bin\_progres.exe -b -p util\syncdblist.p -pf %PROTOP%\etc\protop.pf -T %TMPDIR% -basekey "INI" -ininame etc\protop.ini >> %LOGDIR%\syncdblist.log 2>&1


