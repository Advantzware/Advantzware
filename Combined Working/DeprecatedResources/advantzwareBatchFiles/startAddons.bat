set DLC=C:\Progress\OpenEdge
set WRKDIR=C:\OpenEdge\WRK
set OEM=C:\Progress\oemgmt
set OEMWRKDIR=C:\OpenEdge\wrk_oemgmt

REM change directory to the 'start in' folder
cd 

REM This corresponds to the Target field in the normal ICON
start /D N:\rcode\addon C:\Progress\OpenEdge\bin\prowin32.exe -pf n:\rcode\addon\nosweat.pf -basekey INI -ininame n:\rcode\progress -p n:\rcode\addon\start.p

exit
exit
