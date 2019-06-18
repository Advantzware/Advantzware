@echo off

call #ENVPT3#\bin\protopenv.bat

cd /d %PROTOP%


set MyCommand=%1

if not defined MyCommand (
  echo Nothing to do
  
)
	  
REM  Do not redirect output as this will preclude the ability to run more than one
REM  foreachdb.bat at the same time
_progres -1 -b -p ./util/foreachdb.p -param "%MyCommand%" 


