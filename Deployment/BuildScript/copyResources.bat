@ECHO OFF
C:
CD \asigui\environments


XCOPY /S /Y /E C:\asigui\repository\resources\*.* C:\asigui\environments\16.11.01\resources > NUL
Exit
