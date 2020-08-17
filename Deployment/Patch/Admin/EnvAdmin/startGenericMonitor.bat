@ECHO OFF
:: NONE OF THE ITEMS BELOW SHOULD BE EDITED
:: THIS FILE SHOULD ALWAYS BE RUN FROM A 'CALLING' BATCH FILE

:: This section tests to see if the monitor is already running, prevents second instance if true
QPROCESS ASI%MonitorName%.exe > NUL
IF %ERRORLEVEL% == 0 (
    ECHO %MonitorName% is already running.
    PAUSE
    GOTO :EXIT
    )
:: Check advantzware.ini for variable defaults
CALL ..\getSysVars.bat
SET DLC=%DLCDir%
:: Do this AFTER you get the system variables
SETLOCAL ENABLEDELAYEDEXPANSION
!Drive!
:: Ensure that prowin.exe is copied to an executable name specific to this monitor
COPY /Y /B !DLC!\bin\prowin.exe !DLC!\bin\ASI!MonitorName!.exe > NUL
CD \!topDir!\!adminDir!\!envAdmin!
:: Finally, run asiLogin with parameters to start the monitor in batch mode
START !DLC!\bin\ASI!MonitorName! -basekey INI -ininame dbms.ini -pf advantzware.pf -p asiLogin.w -param "monitor,monitor,!environment!,!Mode!,!dbName!"
:EXIT
EXIT
