@echo off 

rem	for mysterious reasons, known only to Windows, the "proenv" icon sometimes needs to be created
rem	this may not be the prettiest code ever but it gets the job done
rem

call C:\Users\Administrator.WIN-SIDPMPLNDIB\Downloads\protop\bin\protopenv.bat

cd /d %PROTOP%

echo Set oWS = WScript.CreateObject("WScript.Shell")         >  %TMPDIR%\CreateShortcut.vbs
echo sLinkFile = "%HOMEDRIVE%%HOMEPATH%\Desktop\proenv.lnk"  >> %TMPDIR%\CreateShortcut.vbs
echo Set oLink = oWS.CreateShortcut(sLinkFile)               >> %TMPDIR%\CreateShortcut.vbs

echo   oLink.TargetPath = "%DLC%\bin\proenv.bat"             >> %TMPDIR%\CreateShortcut.vbs
echo   oLink.WorkingDirectory = "%WRKDIR%"                   >> %TMPDIR%\CreateShortcut.vbs
echo   oLink.Description = "proenv"                          >> %TMPDIR%\CreateShortcut.vbs
echo   oLink.IconLocation = "%DLC%\gui\adeicon\progress.ico" >> %TMPDIR%\CreateShortcut.vbs

rem echo   oLink.Arguments =  "%1"                           >> %TMPDIR%\CreateShortcut.vbs

echo oLink.Save                                              >> %TMPDIR%\CreateShortcut.vbs

cscript %TMPDIR%\CreateShortcut.vbs

del %TMPDIR%\CreateShortcut.vbs
