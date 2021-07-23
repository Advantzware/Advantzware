@ECHO OFF
REM This a template for a batch script that allows automated log in into advantzware envronments

REM Check advantzware.ini for variable defaults
CALL ..\getSysVars.bat
REM Do this AFTER you get the system variables
SETLOCAL ENABLEDELAYEDEXPANSION

SET DLC=%DLCDir%
!Drive!
CD \!topDir!\!adminDir!\!envAdmin!
START !DLC!\bin\prowin -basekey INI -ininame dbms.ini -pf advantzware.pf -p asiLogin.w -param "<userid>,<password>,<environment>,<mode>,<database>"
GOTO :EXIT

:EXIT