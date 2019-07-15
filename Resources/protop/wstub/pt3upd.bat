@echo off

call #ENVPT3#\bin\protopenv.bat

cd /d %PROTOP%

set PTVER=%1

if "%PTVER%"=="" (
	set PTVER=pt
)

set /p DEPLOY=<%PROTOP%\etc\deploy.cfg

set FNAME=%PTVER%-%DEPLOY%.zip

if not "%PROXY%"=="" (
	set PROXYOPT=-x %PROXY%
)

if exist %TMPDIR%\%FNAME% (
	del /q %TMPDIR%\%FNAME% 2>NUL
)

if exist %PROTOP%\update.bat (
	del /q update.bat 2>NUL
)

del /q ptdefs\*.xsd 2>NUL

echo curl %PROXYOPT% -L -o %TMPDIR%\%FNAME% http://%PTUPD%/pt3upd/%FNAME%
curl %PROXYOPT% -L -o %TMPDIR%\%FNAME% http://%PTUPD%/pt3upd/%FNAME%
sleep 5

if exist %TMPDIR%\%FNAME% (

	del /q estub\* 2>NUL
	del /q wstub\* 2>NUL

	unzip -o %TMPDIR%\%FNAME%

	if exist %PROTOP%\update.bat (
		call %PROTOP%\update.bat
		del /q update.bat 2>NUL
	)

	%DLC%\bin\_progres -1 -b -p .\lib\install.p

	del /q %TMPDIR%\*.flg 2>NUL
	sleep 5

	del /q %TMPDIR%\* 2>NUL
	del /q %LOGDIR%\* 2>NUL

) else (

	echo Download failed.

)
