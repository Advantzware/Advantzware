@ECHO OFF
:: This a template for a batch script that allows automated log in into advantzware environments
:: This version of the template allows one-click running of ASI monitor programs
:: This program CAN NOT be scheduled to run on Computer Startup in Task Scheduler

:: UNCOMMENT the appropriate line for this monitor
::SET MonitorName=cXMLMonitor&SET Mode=cXML Monitor
::SET MonitorName=eskoMonitor&SET Mode=Esko Monitor
::SET MonitorName=fgXmlMonitor&SET Mode=FG XML Monitor
::SET MonitorName=relXmlMonitor&SET Mode=Rel XML Monitor
::SET MonitorName=rfidMonitor&SET Mode=RFID Monitor
SET MonitorName=rmAsnTagMonitor&SET Mode=RM ASN Tag
::SET MonitorName=scheduleMonitor&SET Mode=ScheduleMonitor
::SET MonitorName=taskMonitor&SET Mode=TaskMonitor
::SET MonitorName=userMonitor&SET Mode=AutoLogout

:: UNCOMMENT to select the appropriate Environment and DB name
SET environment=Prod&SET dbName=asiProd
::SET environment=Test&SET dbName=asiTest
::SET environment=devel&SET dbName=TESTDEVELd

CD /D %~dp0
CALL startGenericMonitor.bat
EXIT

