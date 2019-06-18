@echo off

call C:\Users\Administrator.WIN-SIDPMPLNDIB\Downloads\protop\bin\protopenv.bat

cd /d %PROTOP%

set FRNAME=%1

if not defined FRNAME (
  exit /b
)

grep %FRNAME% %PROTOP%\etc\dblist.cfg | gawk -F"|" "{ print $2 }"
