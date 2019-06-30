@echo off

call C:\Users\Administrator.WIN-SIDPMPLNDIB\Downloads\protop\bin\protopenv.bat

cd /d %PROTOP%

call %DLC%\bin\promon %1 < %PROTOP%\etc\promon.pica 2> NUL | grep " : "
