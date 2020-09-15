@ECHO OFF
SET ghToken=5c5b4ee9facd6495549ae1a5e6c4184ec492807c
SET gitCredentials=-u https://%ghToken%@github.com/mark-advantzware/advantzware.git
c:
cd /asigui/repositories/advantzware
git branch -a > c:\tmp\branchlist.txt
exit
