rem @echo off
REM  KIWI  FTP connect comand  batch mode.
REM ================================================
ftp -n -s:"c:\tmp\kiwicmd.bat" 10.0.0.4
move C:\TMP\HOSTKIWI c:\tmp\hostkiwi.sav
