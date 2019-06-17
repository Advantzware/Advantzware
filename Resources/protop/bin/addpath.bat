@echo off

echo %PATH% | findstr /i %1 > NUL

rem     BAT programming 101 -- or yet another reason why I despise Windows...
rem     testing error level is testing for X or higher success = 0,
rem	failure = 1 thus you CANNOT simply test for success instead you
rem     have to test for failure or not failure

if errorlevel 1 set PATH=%1;%PATH%

