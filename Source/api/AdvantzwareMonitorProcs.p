/*------------------------------------------------------------------------
    File        : api/AdvantzwareMonitorProcs.p
    Purpose     : To monitor Advantzware Resources

    Syntax      :

    Description : To monitor Advantzware Resources

    Author(s)   : Vishnu Vellanki
    Created     : Tue Aug 30 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE VARIABLE cDLC             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAdminServerPort AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAppServerName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAppServerPort   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNameServerName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNameServerPort  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAPIIPAddress    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAPIPort         AS CHARACTER NO-UNDO.

DEFINE VARIABLE cResourceStatusRunning AS CHARACTER NO-UNDO INITIAL "Running".
DEFINE VARIABLE cResourceStatusStopped AS CHARACTER NO-UNDO INITIAL "Stopped".

DEFINE VARIABLE cResourceTypeNode        AS CHARACTER NO-UNDO INITIAL "Node".
DEFINE VARIABLE cResourceTypeAppServer   AS CHARACTER NO-UNDO INITIAL "AppServer".
DEFINE VARIABLE cResourceTypeNameServer  AS CHARACTER NO-UNDO INITIAL "NameServer".
DEFINE VARIABLE cResourceTypeAdminServer AS CHARACTER NO-UNDO INITIAL "AdminServer".
DEFINE VARIABLE cResourceTypeASI         AS CHARACTER NO-UNDO INITIAL "ASI".

DEFINE VARIABLE lSuccess      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cLine         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE cPathDataFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdSession     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdOSProcs     AS HANDLE    NO-UNDO.

RUN system/OSProcs.p     PERSISTENT SET hdOSProcs.
RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
RUN system/Session.p     PERSISTENT SET hdSession.

/* This is to get temporary path */
RUN GetBarDirFilePath IN hdOutputProcs (
    INPUT cCompany,
    INPUT "temp", 
    OUTPUT cPathDataFile
    ).

PROCEDURE AdvantzwareMonitor_Initialize:
    /*------------------------------------------------------------------------------
     Purpose: Initialize the required configuration variables
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcDLC             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAdminServerPort AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAppServerName   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAppServerPort   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcNameServerName  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcNameServerPort  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAPIIPAddress    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAPIPort         AS CHARACTER NO-UNDO.
    
    ASSIGN
        cDLC             = ipcDLC            
        cCompany         = ipcCompany        
        cAdminServerPort = ipcAdminServerPort
        cAppServerName   = ipcAppServerName  
        cAppServerPort   = ipcAppServerPort  
        cNameServerName  = ipcNameServerName 
        cNameServerPort  = ipcNameServerPort 
        cAPIIPAddress    = ipcAPIIPAddress 
        cAPIPort         = ipcAPIPort
        .
END PROCEDURE.

PROCEDURE AdvantzwareMonitor_UpdateResourceStatusTime:
/*------------------------------------------------------------------------------
     Purpose: Procedure to update status date time with current date and time
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcASIServerName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-serverResource FOR serverResource.
    
    FIND FIRST bf-serverResource EXCLUSIVE-LOCK
         WHERE bf-serverResource.resourceType EQ cResourceTypeASI
           AND bf-serverResource.name         EQ ipcASIServerName
         NO-ERROR NO-WAIT.
    IF LOCKED bf-serverResource THEN
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "serverResource record locked"
            .
    ELSE IF NOT AVAILABLE bf-serverResource THEN
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Unable to find serverResource record"
            .
    ELSE IF AVAILABLE bf-serverResource THEN
        ASSIGN
            bf-serverResource.statusDateTime = NOW
            bf-serverResource.statusRemarks  = "Last update time: " + STRING(bf-serverResource.statusDateTime)
            oplSuccess                       = TRUE
            opcMessage                       = "Success"
            .
    
    RELEASE bf-serverResource.
END PROCEDURE.

/* Gets NodeServer status */ 
PROCEDURE pGetNodeStatus PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch the Node server status
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcNodePort AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcStatus   AS CHARACTER NO-UNDO.
        
    DEFINE VARIABLE cCommand   AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE lcResponse AS LONGCHAR  NO-UNDO.
    
    opcStatus = cResourceStatusStopped. 

/*    cCommand = "POWERSHELL GET-PROCESS -ID (GET-NETTCPCONNECTION -LOCALPORT " + ipcNodePort + ").OWNINGPROCESS".*/
/*    cCommand = "netstat -an -p tcp".*/
    
    IF SEARCH("curl.exe") EQ ? THEN DO:
        MESSAGE "curl not found! Unable to find node status"
            VIEW-AS ALERT-BOX ERROR. 
        RETURN.
    END.

    /* curl parameter connect-timout is used to set the timeout to establish a 
       connection with url. If establishing a connection with url exceeds the
       set time, curl should terminate the process and exit. In this case the
       timeout is set to 0.1 seconds or 100 milliseconds */
    /* api/getnodestatus is the dummy api, that is going to return the status as 
       "Running" if node is running. This response will return from node itself,
       so no AppServer brokers are blocked in the process */    
    cCommand = SEARCH("curl.exe") + ' --connect-timeout 0.1 --insecure' + ' '
              + '-H "Content-Type: application/json"' + ' '
              + 'http://' + cAPIIPAddress
              + (IF cAPIPort NE '' THEN ":" + cAPIPort ELSE "")
              + '/api/getnodestatus'.

    RUN OS_RunCommand IN hdOSProcs (
        INPUT  cCommand,             /* Command string to run */
        INPUT  cPathDataFile,        /* File name to write the command output */
        INPUT  TRUE,                 /* Run with SILENT option */
        INPUT  FALSE,                /* Run with NO-WAIT option */
        OUTPUT lSuccess,
        OUTPUT cMessage
        ) NO-ERROR.
    IF ERROR-STATUS:ERROR OR NOT lSuccess THEN
        RETURN.

    IF SEARCH(cPathDataFile) = ? THEN
        RETURN.

    COPY-LOB FILE cPathDataFile TO lcResponse.

    IF lcResponse MATCHES "*200*" AND lcResponse MATCHES "*Running*" THEN
       opcStatus = cResourceStatusRunning.

    OS-DELETE VALUE(cPathDataFile).
END PROCEDURE.

/* Monitors Advantzware Resources */
PROCEDURE AdvantzwareMonitor_UpdateResourceStatus:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch and update the resource status
     Notes:
    ------------------------------------------------------------------------------*/ 
    FOR EACH serverResource 
        WHERE serverResource.resourceType EQ cResourceTypeNode AND 
        serverResource.isActive NO-LOCK:
        RUN pUpdateStatus(
            INPUT ROWID(serverResource)
            ).
    END.
    PAUSE 2.
    FOR EACH serverResource 
        WHERE serverResource.resourceType EQ cResourceTypeAppServer AND 
        serverResource.isActive NO-LOCK:
        RUN pUpdateStatus(
            INPUT ROWID(serverResource)
            ).
    END.
    PAUSE 2.
    FOR EACH serverResource 
        WHERE serverResource.resourceType EQ cResourceTypeNameServer AND 
        serverResource.isActive NO-LOCK:
        RUN pUpdateStatus(
            INPUT ROWID(serverResource)
            ).
    END.
    PAUSE 2.
    FOR EACH serverResource 
        WHERE serverResource.resourceType EQ cResourceTypeAdminServer AND 
        serverResource.isActive NO-LOCK:
        RUN pUpdateStatus(
            INPUT ROWID(serverResource)
            ).
    END.
    PAUSE 2.
    FOR EACH serverResource 
        WHERE serverResource.resourceType EQ cResourceTypeASI AND 
        serverResource.isActive NO-LOCK:
        RUN pUpdateStatus(
            INPUT ROWID(serverResource)
            ).
    END.
END PROCEDURE.

/* Gets AppServer Status */
PROCEDURE pGetASBrokerStatus PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch the AppServer status
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcBrokerName    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBrokerStatus  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFullFilePath    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCounter         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ibrokerLine      AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE cCommand         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdServer         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
    
    opcBrokerStatus = cResourceStatusStopped.
    
    cCommand = cDLC + "\bin\asbman.bat -NAME " + ipcBrokerName + " -PORT " 
             + STRING(cAdminServerPort) + " -QUERY".
    
    RUN OS_RunCommand IN hdOSProcs (
        INPUT  cCommand,             /* Command string to run */
        INPUT  cPathDataFile,        /* File name to write the command output */
        INPUT  TRUE,                 /* Run with SILENT option */
        INPUT  FALSE,                /* Run with NO-WAIT option */
        OUTPUT lSuccess,
        OUTPUT cMessage
        ) NO-ERROR.
    IF ERROR-STATUS:ERROR OR NOT lSuccess THEN
        RETURN.
        
    IF SEARCH(cPathDataFile) = ? THEN 
        RETURN.

    INPUT FROM VALUE(cPathDataFile).
    REPEAT:
       IMPORT UNFORMATTED cLine.
               
       IF cLine MATCHES "Broker:*not running (8313)*"  OR 
          cLine MATCHES "Unable to find*" THEN 
           opcBrokerStatus = cResourceStatusStopped.
               
       IF cLine BEGINS "Broker Status" THEN
           opcBrokerStatus = cResourceStatusRunning.
       
    END.     
    INPUT CLOSE.

    IF opcBrokerStatus EQ cResourceStatusRunning THEN DO:
        CREATE SERVER hdServer.

        IF hdServer:CONNECT("-URL AppServerDC://localhost:" + cAppServerPort + "/" + ipcBrokerName) THEN DO:        
            RUN api\ASStatus.p  ON hdServer (
                OUTPUT lSuccess, 
                OUTPUT opcMessage
                ) NO-ERROR.
                            
            hdServer:DISCONNECT().
        END.
        
        IF NOT lSuccess THEN
            opcBrokerStatus = cResourceStatusStopped.
    END.
    
    DELETE OBJECT hdServer.
    
    OS-DELETE VALUE(cPathDataFile).
END PROCEDURE.

/* Gets NameServer Status */
PROCEDURE pGetNameServerStatus PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch the NameServer status
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cNameServerName    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNameServerStatus  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cCommand AS CHARACTER NO-UNDO.
    
    opcNameServerStatus = cResourceStatusStopped.

    cCommand = cDLC + "\bin\nsman.bat -NAME " + cNameServerName + " -PORT " 
             + STRING(cAdminServerPort) + " -QUERY".
    
    RUN OS_RunCommand IN hdOSProcs (
        INPUT  cCommand,             /* Command string to run */
        INPUT  cPathDataFile,        /* File name to write the command output */
        INPUT  TRUE,                 /* Run with SILENT option */
        INPUT  FALSE,                /* Run with NO-WAIT option */
        OUTPUT lSuccess,
        OUTPUT cMessage
        ) NO-ERROR.
    IF ERROR-STATUS:ERROR OR NOT lSuccess THEN
        RETURN.

    IF SEARCH(cPathDataFile) = ? THEN
        RETURN.

    INPUT FROM VALUE(cPathDataFile).
    REPEAT:
        IMPORT UNFORMATTED cLine.

        IF cLine MATCHES "NameServer: * not running (8313)" THEN 
            opcNameServerStatus = cResourceStatusStopped.
           
        IF cLine MATCHES "NameServer * running on Host*" OR 
           cLine MATCHES "Unable to find*" THEN
             opcNameServerStatus = cResourceStatusRunning.
    END.
    INPUT CLOSE.
    OS-DELETE VALUE(cPathDataFile). 
END PROCEDURE.

/* Gets AdminServer Status */
PROCEDURE pGetAdminServerStatus PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch the AdminServer status
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcPort               AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAdminServerStatus  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cCommand AS CHARACTER NO-UNDO.

    opcAdminServerStatus  = cResourceStatusStopped.

    cCommand = cDLC + "\bin\proadsv.bat -QUERY -port "
             + STRING(ipcPort).

    RUN OS_RunCommand IN hdOSProcs (
        INPUT  cCommand,             /* Command string to run */
        INPUT  cPathDataFile,        /* File name to write the command output */
        INPUT  TRUE,                 /* Run with SILENT option */
        INPUT  FALSE,                /* Run with NO-WAIT option */
        OUTPUT lSuccess,
        OUTPUT cMessage
        ) NO-ERROR.
    IF ERROR-STATUS:ERROR OR NOT lSuccess THEN
        RETURN.
    
    IF SEARCH(cPathDataFile) = ? THEN
        RETURN.
    
    INPUT FROM VALUE(cPathDataFile).
    REPEAT:
        IMPORT UNFORMATTED cLine.
        opcAdminServerStatus  = IF cLine MATCHES "AdminServer is alive*" THEN 
                                    cResourceStatusRunning 
                                ELSE 
                                    cResourceStatusStopped.
    END.
    INPUT CLOSE.
    OS-DELETE VALUE(cPathDataFile).
END PROCEDURE.

/* Gets Monitor Status */
PROCEDURE pGetASIServerStatus PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch the ASI Monitor resource status
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipdtLastStatusTime     AS DATETIME  NO-UNDO.
    DEFINE INPUT  PARAMETER opiRefreshFrequency    AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMonitorServerStatus AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iToleranceTimeInSeconds AS INTEGER NO-UNDO INITIAL 5.
    
    opcMonitorServerStatus = cResourceStatusStopped.

    IF ipdtLastStatusTime EQ ? THEN        
        RETURN.
    
    /* Verify if last status time on the resource, exceeds the max refresh frequency
       plus tolerence time from current time */
    IF INTERVAL(NOW, ipdtLastStatusTime, "seconds") GE opiRefreshFrequency + iToleranceTimeInSeconds THEN
        RETURN.

    opcMonitorServerStatus  = cResourceStatusRunning.
END PROCEDURE.

/* Sends Email notification when any resource is stopped */
PROCEDURE pSendEmail PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to send email notification
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER bufServerResource FOR serverResource.
    DEFINE OUTPUT PARAMETER oplSent           AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cSubject  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBody     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.
    
    IF AVAILABLE bufServerResource THEN DO:
	
        RUN pGetEmailConfigBody (
            INPUT  bufServerResource.configID,
            OUTPUT cBody
            ).
                
        RUN pGetEmailConfigSubject (
            INPUT  bufServerResource.configID,
            OUTPUT cSubject
            ).
        
        ASSIGN
            cSubject = REPLACE(cSubject,"<$serverResourcePort$>",bufServerResource.port)
            cSubject = REPLACE(cSubject,"<$serverResourceName$>",bufServerResource.name)
            .

        RUN spSendEmail IN hdSession (
            INPUT bufServerResource.configID,  /* emailConfig.ConfigID */
            INPUT "",                          /* Override for Email RecipientsinTo */
            INPUT "",                          /* Override for Email RecipientsinReplyTo */
            INPUT "",                          /* Override for Email RecipientsinCC */
            INPUT "",                          /* Override for Email RecipientsinBCC */
            INPUT cSubject,                    /* Override for Email Subject */
            INPUT cBody,                       /* Override for Email Body */
            INPUT "",                          /* Email Attachment */
            OUTPUT lSuccess,                   /* Email success or not */
            OUTPUT cMessage                    /* Reason for failure in case email is not sent */
            ).
        oplSent = lSuccess.
    END.
END PROCEDURE.

/* Updates resouce status */
PROCEDURE pUpdateStatus PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to update the serverResource table with resource's status
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowId AS ROWID NO-UNDO.

    DEFINE VARIABLE cStatus                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBrokerNameServer      AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cBrokerNameServerPort  AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE lEmailNotificationSent AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER serverResource FOR serverResource.
    
    /* Not adding no-wait as the status must be updated */
    FIND serverResource 
        WHERE ROWID(serverResource) EQ iprRowId EXCLUSIVE-LOCK. 
    
	IF AVAILABLE serverResource THEN DO:
        
		CASE serverResource.resourceType:            
            WHEN cResourceTypeNode THEN
                RUN pGetNodeStatus(
                    INPUT serverResource.port,
                    OUTPUT cStatus
                    ).                    
            WHEN cResourceTypeAdminServer THEN DO:                
                IF serverResource.port NE cAdminServerPort THEN
                    serverResource.port = cAdminServerPort.
                    
                RUN pGetAdminServerStatus(
                    INPUT  serverResource.port,
                    OUTPUT cStatus
                    ).     
            END.                              
            WHEN cResourceTypeNameServer THEN DO:               
                IF serverResource.name NE cNameServerName THEN
                    serverResource.name = cNameServerName.
                IF serverResource.port NE cNameServerPort THEN
                    serverResource.port = cNameServerPort.
                
                RUN pGetNameServerStatus (
                    INPUT  serverResource.Name,
                    OUTPUT cStatus
                    ).
            END.     
            WHEN cResourceTypeAppServer THEN DO:            
                IF serverResource.name NE cAppServerName THEN
                    serverResource.name = cAppServerName.
                IF serverResource.port NE cAppServerPort THEN
                    serverResource.port = cAppServerPort.
                    
                RUN pGetASBrokerStatus(
                    INPUT  serverResource.Name,
                    OUTPUT cStatus,
                    OUTPUT serverResource.statusRemarks
                    ).
            END.
            WHEN cResourceTypeASI THEN DO:                
                RUN pGetASIServerStatus (
                    INPUT  serverResource.statusDateTime,
                    INPUT  serverResource.refreshFrequency,
                    OUTPUT cStatus
                    ).
            END.
        END CASE.
            
        ASSIGN 
            serverResource.resourceStatus = cStatus
            serverResource.notified       = (IF cStatus EQ cResourceStatusRunning THEN 
                                                 NO 
                                             ELSE 
                                                 serverResource.notified)
            serverResource.statusDateTime = (IF serverResource.resourceType EQ cResourceTypeASI THEN
                                                 serverResource.statusDateTime
                                             ELSE
                                                 NOW)
            .

        IF serverResource.resourceStatus = cResourceStatusStopped AND 
           NOT serverResource.notified THEN DO:
            
            RUN pSendEmail(
                BUFFER serverResource,
                OUTPUT lEmailNotificationSent
                ).

            serverResource.notified = lEmailNotificationSent.
        END.
                    
    END.
RELEASE serverResource.
END PROCEDURE.

/* gets Emailconfig Body */
PROCEDURE pGetEmailConfigBody PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch email body for a given configID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcConfigID AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBody     AS CHARACTER NO-UNDO.
    
	FIND FIRST emailConfig NO-LOCK
         WHERE emailConfig.configID EQ ipcConfigID
           AND isActive
         NO-ERROR.
    IF AVAILABLE emailConfig THEN
        opcBody = emailConfig.body
        .
        
END PROCEDURE.

/* gets Emailconfig Subject */
PROCEDURE pGetEmailConfigSubject PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch email subject for a given configID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcConfigID AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSubject  AS CHARACTER NO-UNDO.
    
    FIND FIRST emailConfig NO-LOCK
         WHERE emailConfig.configID EQ ipcConfigID
           AND isActive
         NO-ERROR.
    IF AVAILABLE emailConfig THEN
        opcSubject = emailConfig.subject
        .
       
END PROCEDURE.

