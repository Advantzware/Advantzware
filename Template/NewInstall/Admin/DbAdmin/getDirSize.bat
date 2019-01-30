@echo off
c:
cd asigui\databases\Ptree
For /F "tokens=*" %%a IN ('"dir /s /-c | find "bytes" | find /v "free""') do @Set summaryout=%%a
For /f "tokens=1,2 delims=)" %%a in ("%summaryout%") do @set filesout=%%a&set sizeout=%%b
Set sizeout=%sizeout:bytes=%
Set sizeout=%sizeout: =%
Echo %sizeout%
pause
