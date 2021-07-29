@ECHO OFF
c:
cd /Users/%username%/advantzware > NUL
git fetch --prune
git branch -a > c:\tmp\branchlist.txt
exit
