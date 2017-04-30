set DLC=%1
set PROEXE=%DLC%\bin\prowin32.exe
echo "checking status"
call %DLC%\bin\asbman.bat -name asAOA -query > aoaStatus.txt
echo "status was checked"
find /i "status" aoaStatus.txt |find /i "active"
echo %ERRORLEVEL%
echo sent errorlevel
if ERRORLEVEL 1 goto :END

set resourcesFolder=%2
set AOAFolder=c:\ProgramData\DataPA
set AOAReportService=DataPA Enterprise Report Service
set AOAEnterpriseService=DataPA Enterprise Service

echo %AOAReportService%
sc stop "%AOAReportService%"

echo %AOAEnterpriseService%
sc stop "%AOAEnterpriseService%"

REM Pause to Allow service to stop
echo Pause for stop of service
PING 1.1.1.1 -n 1 -w 15000 >NUL

xcopy %resourcesFolder%\ProgramData\DataPA\*.* %AOAFolder% /s /Y
ECHO Stopping asAOA
call %DLC%\bin\asbman.bat -name asAOA -stop
echo Pausing for stop of asAOA
PING 1.1.1.1 -n 1 -w 15000 >NUL

ECHO Starting asAOA
call %DLC%\bin\asbman.bat -name asAOA -start

ECHO Starting Report Services

sc start "%AOAReportService%"
sc start "%AOAEnterpriseService%"

:END
