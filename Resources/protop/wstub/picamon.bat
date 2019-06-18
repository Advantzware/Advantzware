@echo off

call #ENVPT3#\bin\protopenv.bat

cd /d %PROTOP%

call %DLC%\bin\promon %1 < %PROTOP%\etc\promon.pica 2> NUL | grep " : "
