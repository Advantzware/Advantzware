@ECHO OFF
C:
CD \asigui\environments


XCOPY /S /Y /E C:\asigui\repository\legacy\*.* C:\asigui\environments\16.11.01\source > NUL
Exit
