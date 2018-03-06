@echo off
if "%DLC%"=="" set DLC=C:\Progress\OE116

call %DLC%\bin\asbman -name asAOA -port 20952 -stop >> C:\Progress\OE116WRK\asAOA.bat.log

call %DLC%\bin\asbman -name asAOA -port 20952 -start >> C:\Progress\OE116WRK\asAOA.bat.log
