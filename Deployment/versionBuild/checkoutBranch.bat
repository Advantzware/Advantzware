@ECHO OFF
SET ghToken=8a8ee226d2420dab562e5482a466d1649303f00c
SET gitCredentials=-u https://%ghToken%@github.com/mark-advantzware/advantzware.git
c:
cd /asigui/repositories/advantzware > NUL
git checkout %1 > NUL
::git pull origin %1
::pause
exit
