@echo off

call #ENVPT3#\bin\protopenv.bat

cd /d %PROTOP%

if exist %PROTOP%\bin\sshinit.bat (
	call %PROTOP%\bin\sshinit.bat > %TMPDIR%\dbmonitor.sshinit.log 2>&1
)

%DLC%\bin\_progres -1 -b -p .\util\dbmonitor.p -T %TMPDIR% -t >> %TMPDIR%\dbmonitor.e2 2>&1 &
