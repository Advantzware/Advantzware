@echo off

call C:\Users\Administrator.WIN-SIDPMPLNDIB\Downloads\protop\bin\protopenv.bat

cd /d %PROTOP%

%DLC%\bin\_progres %1 -b -p .\util\newdb.p -param "%2" >> %LOGDIR%\newdb.log 2>&1 &
