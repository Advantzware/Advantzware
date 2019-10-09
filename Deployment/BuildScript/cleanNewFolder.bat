@ECHO OFF
C:
CD \asigui\environments

CD 16.11.01
REN Legacy Source > NUL


RMDIR /S /Q programs
RMDIR /S /Q source
RMDIR /S /Q resources
RMDIR /S /Q override
MKDIR programs
MKDIR source
MKDIR resources
MKDIR override
pause
Exit
