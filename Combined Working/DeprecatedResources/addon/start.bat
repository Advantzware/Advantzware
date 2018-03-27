@echo off
set DLC=C:\Progress\OpenEdge
set WRKDIR=C:\OpenEdge\WRK
set OEM=C:\Progress\oemgmt
set OEMWRKDIR=C:\OpenEdge\wrk_oemgmt
set PATH=%DLC%\BIN;%DLC%\PERL\BIN;%PATH%
set LIB=%DLC%\LIB;%LIB%
p:
cd \asi10test\rco1010\addon
start "" "C:\PROGRESS\OpenEdge\bin\prowin32.exe" -pf .\nosweat.pf
