::@ECHO OFF
:: This code "assumes" it's own location is in /programs/api
:: If moved, may have to change the ini file lookup

:: Read advantzware.ini and set variable values
SET iniFile=..\..\..\..\Admin\advantzware.ini
FOR /F "tokens=1,2 delims==" %%G IN (%iniFile%) DO SET %%G=%%H
IF "%drive%" == "" (
    ECHO ...advantzware.ini file not found...Exiting
    GOTO :EXIT
    )

SETLOCAL ENABLEDELAYEDEXPANSION

:: Check to ensure we're running from the ASI server
:: Finding a guaranteed file in the filesystem database directory is as good a method as any
IF NOT EXIST !dbDrive!\!topDir!\!dbDir!\Structure\STFiles\progress.dev (
    ECHO MSGBOX "You MUST run this program on the ASI server." > %temp%\TEMPmessage.vbs
    CALL %temp%\TEMPmessage.vbs
    DEL %temp%\TEMPmessage.vbs /f /q
    GOTO :QUIT
    ) 

SET DLC=c:\Progress\OE116_64
SET API_JAVA_PROGRAMS_DIR=java
SET API_JAVA_LOGS_DIR=java\logs
SET API_JAVA_PROCESSOR=RequestDispatcher
SET API_APPSERVER_URL=AppServer://localhost:5163/Advantzware_API
SET API_APPSERVER_REQUEST_ROUTER=api/inbound/APIRequestRouterAS.p
SET API_IP_ADDRESS=172.31.22.4
SET API_PORT=8443

ECHO Running node listeners...Ctrl-Break to exit
CD ..\..\CustFiles\node
START /B node InboundAPIStart.js >> node.server.log 2>> node.server.error.log
pause

:QUIT
EXIT