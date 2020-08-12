@ECHO OFF
:: This a template for a batch script that allows automated log in into advantzware environments
:: This version of the template allows one-click running of ASI monitor programs
:: This program CAN NOT be scheduled to run on Computer Startup in Task Scheduler

:: UNCOMMENT the appropriate line for this monitor
::SET MonitorName=cXMLMonitor&SET Mode=cXML Monitor
::SET MonitorName=eskoMonitor&SET Mode=Esko Monitor
::SET MonitorName=fgXmlMonitor&SET Mode=FG XML Monitor
::SET MonitorName=relXmlMonitor&SET Mode=Rel XML Monitor
SET MonitorName=rfidMonitor&SET Mode=RFID Monitor
::SET MonitorName=rmAsnTagMonitor&SET Mode=RM ASN Tag
::SET MonitorName=scheduleMonitor&SET Mode=ScheduleMonitor
::SET MonitorName=taskMonitor&SET Mode=TaskMonitor
::SET MonitorName=userMonitor&SET Mode=AutoLogout

:: These assignments should be common to all customer distributions (Prod and asiProd)
SET environment=devel
SET dbName=TESTDEVELd

:: NONE OF THE ITEMS BELOW SHOULD BE EDITED
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
