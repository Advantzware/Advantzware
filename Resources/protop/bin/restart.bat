@echo off

call C:\Users\Administrator.WIN-SIDPMPLNDIB\Downloads\protop\bin\protopenv.bat

cd /d %PROTOP%

del /q %TMPDIR%\*.flg 2>NUL
sleep 5

rem	del /q %TMPDIR%\* 2>NUL
rem	del /q %LOGDIR%\* 2>NUL
