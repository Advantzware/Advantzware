@echo off

call #ENVPT3#\bin\protopenv.bat

cd /d %PROTOP%

set LOGDIR=%PROTOP%\log

nssm64 install "ProTop3 DB Monitor" %ProTop%\bin\dbmonitor.bat

nssm64 set "ProTop3 DB Monitor" Description "DB monitor for OpenEdge."
nssm64 set "ProTop3 DB Monitor" AppDirectory %ProTop%
nssm64 set "ProTop3 DB Monitor" AppRestartDelay 60000
nssm64 set "ProTop3 DB Monitor" AppStdout %LOGDIR%\dbmonitor.log
nssm64 set "ProTop3 DB Monitor" AppStderr %TMPDIR%\dbmonitor.nssm.err
nssm64 set "ProTop3 DB Monitor" AppEnvironmentExtra DLC=%DLC%
nssm64 set "ProTop3 DB Monitor" Start SERVICE_DELAYED_AUTO_START

nssm64 start "ProTop3 DB Monitor"

nssm64 status "ProTop3 DB Monitor"

rem	nssm64 edit "ProTop3 DB Monitor"
