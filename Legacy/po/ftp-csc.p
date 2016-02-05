/* po/ftp-csc.p FTP exported file to ftp server */

DEF INPUT PARAM ip-exp-file AS cha NO-UNDO.

OS-COPY value(ip-exp-file) VALUE("c:\tmp\ftpexppo.txt").
OS-COMMAND VALUE("@echo off").
OS-COMMAND value("ftp -v -i -s:.\po\ftpcmd.txt 216.127.228.178").
