@echo off

call #ENVPT3#\bin\protopenv.bat

cd /d %PROTOP%

del /q %TMPDIR%\*.flg 2>NUL
sleep 5

rem	del /q %TMPDIR%\* 2>NUL
rem	del /q %LOGDIR%\* 2>NUL
