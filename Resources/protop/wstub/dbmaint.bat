@echo off

call #ENVPT3#\bin\protopenv.bat

cd /d %PROTOP%

rem	"%DLC%"\bin\_progres.exe -p util\dbmaint.p -pf "%PROTOP%"\etc\protop.pf -T "%TMPDIR%" -basekey "INI" -ininame etc\protop.ini

"%DLC%"\bin\_progres.exe -p util\dbmaint.p -pf "%PROTOP%"\etc\protop.pf -T "%TMPDIR%" -basekey "INI" -ininame etc\dbmaint.ini

