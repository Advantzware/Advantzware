@echo off

set PTHOST=demo.wss.com
set PTUPD=demo.wss.com

rem
rem	This is a stub -- it is appended to bin\portalenv.bat to form the complete pt3inst.bat script
rem

@echo off

if not defined DLC (
	rem	echo X=MsgBox("You must install ProTop from a PROENV prompt, not from Windows Explorer",0+16,"Improper ProTop Install")>bin\pt3insterr.vbs

	if exist bin\pt3insterr.vbs (
		wscript bin\pt3insterr.vbs
	) else (
		wscript pt3insterr.vbs
	)
	exit
)

net session >nul 2>&1
if not %errorLevel%==0 (

        echo Current permissions are inadequate.  You must run pt3inst as administrator.

) else (

	mode con cols=160 lines=48

	if not "%1"=="" (
		set PTHOST=%1
	)

	if exist .\lib\install.p (
		ubin\curl -L %PTHOST%:/proxy.test
		set PROPATH=.
		%DLC%\bin\_progres.exe -1 -p .\lib\install.p -debugalert -errorstack -clientlog pt3inst.err
		copy /y bin\protopenv.bat %WRKDIR% > NUL 2>&1
	) else (
		echo "You must be in the ProTop 3 install directory to run pt3inst.bat"
	)

)


