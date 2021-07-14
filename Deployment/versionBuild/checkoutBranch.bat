@ECHO OFF
C:
CD /Users/%username%/advantzware > NUL
git checkout %1 > NUL
EXIT
