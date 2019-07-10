@echo off

call C:\Users\Administrator.WIN-SIDPMPLNDIB\Downloads\protop\bin\protopenv.bat

cd /d %PROTOP%

echo Set oWS = WScript.CreateObject("WScript.Shell")        >  %TMPDIR%\CreateShortcut.vbs
echo sLinkFile = "%HOMEDRIVE%%HOMEPATH%\Desktop\pt3_%1.lnk" >> %TMPDIR%\CreateShortcut.vbs
echo Set oLink = oWS.CreateShortcut(sLinkFile)              >> %TMPDIR%\CreateShortcut.vbs

echo   oLink.TargetPath = "%PROTOP%\bin\protop.bat"         >> %TMPDIR%\CreateShortcut.vbs
echo   oLink.Arguments =  "%1"                              >> %TMPDIR%\CreateShortcut.vbs
echo   oLink.Description = "ProTop3 %1"                     >> %TMPDIR%\CreateShortcut.vbs
echo   oLink.IconLocation = "%PROTOP%\etc\protop.ico"       >> %TMPDIR%\CreateShortcut.vbs
echo   oLink.WorkingDirectory = "%PROTOP%"                  >> %TMPDIR%\CreateShortcut.vbs

echo oLink.Save                                             >> %TMPDIR%\CreateShortcut.vbs

cscript %TMPDIR%\CreateShortcut.vbs

del %TMPDIR%\CreateShortcut.vbs
