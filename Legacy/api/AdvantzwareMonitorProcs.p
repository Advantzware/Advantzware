/*------------------------------------------------------------------------
    File        : api/AdvantzwareMonitorProcs.p
    Purpose     : To monitor Advantzware Resources

    Syntax      :

    Description : To monitor Advantzware Resources

    Author(s)   : Vishnu Vellanki
    Created     : Tue Aug 30 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE INPUT PARAMETER ipcDLC AS CHARACTER NO-UNDO.
    
DEFINE VARIABLE cLine         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE cPathDataFile AS CHARACTER NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

/* This is to get path */
RUN GetBarDirFilePath IN hdOutputProcs ("001","temp", OUTPUT cPathDataFile).
 
/* Get NodeServer status */ 
PROCEDURE getNodeStatus:
    DEFINE INPUT  PARAMETER ipcNodePort AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcStatus   AS CHARACTER NO-UNDO.
        
    DEFINE VARIABLE cCommand AS CHARACTER NO-UNDO. 
    
    cCommand = "POWERSHELL GET-PROCESS -ID (GET-NETTCPCONNECTION -LOCALPORT " + ipcNodePort + ").OWNINGPROCESS >" + cPathDataFile. 
    OS-COMMAND SILENT VALUE(cCommand).
    IF SEARCH(cPathDataFile) = ? THEN RETURN.
    
    opcStatus ="Stopped". 
    
    INPUT FROM VALUE(cPathDataFile).
    REPEAT:
        IMPORT UNFORMATTED cLine.
        IF cLine MATCHES "*node*" THEN
           opcStatus ="Running".
    END.
    OS-DELETE VALUE(cPathDataFile).
END PROCEDURE.

/* Monitors Advantzware Resources */
PROCEDURE pMonitor:
    FOR EACH serverResource 
        WHERE serverResource.isActive NO-LOCK:
        
        RUN updateStatus(
            INPUT ROWID(serverResource)
            ).
    END.
END PROCEDURE.

/* Returns AppServer Server */
PROCEDURE getASBrokerStatus:
    DEFINE INPUT  PARAMETER ipcBrokerName     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNameServer     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNameServerPort AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBrokerStatus   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFullFilePath AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCounter      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ibrokerLine   AS INTEGER   NO-UNDO. 

    cFullFilePath = ipcDLC + "\" + "properties\ubroker.properties".

    INPUT FROM VALUE(cFullFilePath).
    BROKER-LOOP:
    REPEAT:
        iCounter = iCounter + 1.
        IMPORT UNFORMATTED cLine.
        
        IF cLine MATCHES "[UBroker.AS." + ipcBrokerName + "]*" THEN 
            ibrokerLine  = iCounter + 1.

        IF ibrokerLine NE 0 AND 
            iCounter > ibrokerLine AND 
            cLine MATCHES "*controllingNameServer*" THEN DO:
            
            opcNameServer = ENTRY(2,cLine,"=").
            LEAVE BROKER-LOOP.  
        END.
    END.
    
    INPUT CLOSE.
    
    RUN getNameServerPort   (
        INPUT  opcNameServer, 
        OUTPUT opcNameServerPort
        ). 
               
               
    OS-COMMAND SILENT VALUE(ipcDLC + "\bin\asbman.bat") -NAME VALUE(ipcBrokerName) -QUERY > VALUE(cPathDataFile).
    IF SEARCH(cPathDataFile) = ? THEN RETURN.
    INPUT FROM VALUE(cPathDataFile).
    REPEAT:
       IMPORT UNFORMATTED cLine.
               
       IF cLine MATCHES "Broker:*not running (8313)*"  OR 
          cLine MATCHES "Unable to find*" THEN 
           opcBrokerStatus = "Stopped".
               
       IF cLine BEGINS "Broker Status" THEN
           opcBrokerStatus = "Running".
 
    END.
     
    INPUT CLOSE.
    OS-DELETE VALUE(cPathDataFile).
END PROCEDURE.


/* Returns NameServer status */
PROCEDURE getNameServerStatus:
    DEFINE INPUT  PARAMETER ipcNameServerName    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNameServerStatus  AS CHARACTER NO-UNDO.
  
  
    OS-COMMAND SILENT VALUE(ipcDLC + "\bin\nsman.bat") -NAME VALUE(ipcNameServerName) -QUERY > VALUE(cPathDataFile).
    IF SEARCH(cPathDataFile) = ? THEN RETURN.

    INPUT FROM VALUE(cPathDataFile).
    REPEAT:
        IMPORT UNFORMATTED cLine.

        IF cLine MATCHES "NameServer: * not running (8313)" THEN 
            opcNameServerStatus = "Stopped".
           
        IF cLine MATCHES "NameServer * running on Host*" OR 
           cLine MATCHES "Unable to find*" THEN
             opcNameServerStatus = "Running".
    END.
    INPUT CLOSE.
    OS-DELETE VALUE(cPathDataFile).
END PROCEDURE.

/* Returns NameServer port */
PROCEDURE getNameServerPort:
    DEFINE INPUT  PARAMETER ipcNameServerName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNameServerPort AS CHARACTER NO-UNDO.
  
    OS-COMMAND SILENT VALUE(ipcDLC + "\bin\nsconfig.bat") -NAME VALUE(ipcNameServerName) > VALUE(cPathDataFile).
    IF SEARCH(cPathDataFile) = ? THEN RETURN.
    INPUT FROM VALUE(cPathDataFile).
    PORT-CHECK:
    REPEAT:
        IMPORT UNFORMATTED cLine.

        IF cLine MATCHES "*portNumber*" THEN DO:
            opcNameServerPort = TRIM(ENTRY(2,cLine,":")).
            LEAVE PORT-CHECK.
        END.
    END.
    INPUT CLOSE.
    OS-DELETE VALUE(cPathDataFile).
END PROCEDURE.

/* Returns AdminServer status */
PROCEDURE getAdminServerStatus:
    DEFINE INPUT  PARAMETER ipcPort               AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAdminServerStatus  AS CHARACTER NO-UNDO.
 
    OS-COMMAND SILENT VALUE(ipcDLC + "\bin\proadsv.bat") -QUERY -port VALUE(ipcPort) > VALUE(cPathDataFile).
    IF SEARCH(cPathDataFile) = ? THEN RETURN.
    
    INPUT FROM VALUE(cPathDataFile).
    REPEAT:
        IMPORT UNFORMATTED cLine.
        opcAdminServerStatus  = IF cLine MATCHES "AdminServer is alive*" THEN "Running" ELSE "Stopped".
    END.
    INPUT CLOSE.
    OS-DELETE VALUE(cPathDataFile).
END PROCEDURE.


/* send email notification */
PROCEDURE sendEmail:
    DEFINE PARAMETER BUFFER bufServerResource FOR serverResource.
    DEFINE OUTPUT PARAMETER oplSent           AS LOGICAL NO-UNDO.
    IF AVAILABLE bufServerResource THEN DO:
        MESSAGE "DEBUG - NOW EMAIL WILL BE SENT TO: " bufServerResource.emailRecipients  VIEW-AS ALERT-BOX.
        oplSent = TRUE.
    END.
END PROCEDURE.


/* update status */
PROCEDURE updateStatus:
    DEFINE INPUT PARAMETER iprRowId AS ROWID NO-UNDO.

    DEFINE VARIABLE cStatus               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBrokerNameServer     AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cBrokerNameServerPort AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cNodePath             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE emailNotificationSent AS LOGICAL   NO-UNDO.

    /* not adding no-wait as the status must be updated */
    FIND serverResource 
        WHERE ROWID(serverResource) EQ iprRowId EXCLUSIVE-LOCK. 
    IF AVAILABLE serverResource THEN DO:
        CASE serverResource.resourceType:
            WHEN "Node" THEN
                RUN getNodeStatus(
                    INPUT serverResource.port,
                    OUTPUT cStatus
                    ).      
                    
            WHEN "AdminServer" THEN
                RUN getAdminServerStatus(
                    INPUT  serverResource.port,
                    OUTPUT cStatus
                    ).            
                                           
            WHEN "NameServer" THEN
                RUN getNameServerStatus (
                    INPUT  serverResource.Name,
                    OUTPUT cStatus
                    ).
                    
            WHEN "AppServer" THEN DO:
                RUN getASBrokerStatus(
                    INPUT  serverResource.Name,
                    OUTPUT cBrokerNameServer,
                    OUTPUT cBrokerNameServerPort,
                    OUTPUT cStatus
                    ).
               
                /* we need to update the statusRemarks for AppServer because AppServer listens on controllingNameServer's port */
                serverResource.statusRemarks  = IF serverResource.resourceType EQ "AppServer" THEN 
                                                   "Listening on NameServer [" + cBrokerNameServer + ":" + cBrokerNameServerPort + "]"
                                                ELSE 
                                                   "".
            END.
        END CASE.
            
        ASSIGN serverResource.resourceStatus = cStatus
               serverResource.notified       = (IF cStatus = "Running" THEN NO ELSE serverResource.notified)
               serverResource.statusDateTime = NOW.

        IF serverResource.resourceStatus = "Stopped" AND 
           NOT serverResource.notified THEN DO:
            RUN sendEmail(
                BUFFER serverResource,
                OUTPUT emailNotificationSent
                ).

            serverResource.notified = emailNotificationSent.
        END.
                    
    END.
RELEASE serverResource.
END PROCEDURE.





