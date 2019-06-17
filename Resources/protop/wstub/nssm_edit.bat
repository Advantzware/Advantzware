@echo off

call #ENVPT3#\bin\protopenv.bat

cd /d %PROTOP%

nssm64 edit "Protop3 DB Monitor"
