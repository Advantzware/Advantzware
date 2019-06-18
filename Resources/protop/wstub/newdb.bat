@echo off

call #ENVPT3#\bin\protopenv.bat

cd /d %PROTOP%

%DLC%\bin\_progres %1 -b -p .\util\newdb.p -param "%2" >> %LOGDIR%\newdb.log 2>&1 &
