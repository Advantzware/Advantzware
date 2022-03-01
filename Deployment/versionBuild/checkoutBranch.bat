@ECHO OFF
C:
CD /Users/%username%/advantzware > NUL

git clean -fdx
git fetch origin  -prune
git checkout develop
git pull
git checkout %1

EXIT
