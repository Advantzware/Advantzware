@ECHO OFF
C:
CD \asigui\environments
MKDIR 16.11.01

XCOPY /S /Y /E .\Devel .\16.11.01 > NUL
Exit
