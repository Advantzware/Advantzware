/*------------------------------------------------------------------------
    File        : api/AdvantzwareMonitorProcs.p
    Purpose     : To monitor Advantzware Resources

    Syntax      :

    Description : To monitor Advantzware Resources

    Author(s)   : Vishnu Vellanki
    Created     : Tue Aug 30 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE INPUT PARAMETER ipcDLC             AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcAdminServerPort AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcAppServerName   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcAppServerPort   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcNameServerName  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcNameServerPort  AS CHARACTER NO-UNDO.
   
DEFINE VARIABLE cLine         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE cPathDataFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdSession     AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
RUN system/Session.p     PERSISTENT SET hdSession.
  
/* This is to get path */
RUN GetBarDirFilePath IN hdOutputProcs (
    INPUT ipcCompany,
    INPUT "temp", 
    OUTPUT cPathDataFile
    ).
 
/* Gets NodeServer status */ 
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

/* Gets AppServer Status */
PROCEDURE getASBrokerStatus:
    DEFINE INPUT  PARAMETER ipcBrokerName    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBrokerStatus  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFullFilePath    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCounter         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ibrokerLine      AS INTEGER   NO-UNDO. 
       
    OS-COMMAND SILENT VALUE(ipcDLC + "\bin\asbman.bat") -NAME VALUE(ipcBrokerName) -PORT VALUE(ipcAdminServerPort) -QUERY > VALUE(cPathDataFile).
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


/* Gets NameServer Status */
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

/* Gets AdminServer Status */
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


/* Sends Email notification when any resource is stopped */
PROCEDURE sendEmail:
    DEFINE PARAMETER BUFFER bufServerResource FOR serverResource.
    DEFINE OUTPUT PARAMETER oplSent           AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cSubject  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBody     AS CHARACTER NO-UNDO.
    
    IF AVAILABLE bufServerResource THEN DO:
	
        RUN getEmailConfigBody (
            INPUT  bufServerResource.configID,
            OUTPUT cBody
            ).
                
        RUN getEmailConfigSubject (
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
            INPUT ""                           /* Email Attachment */
            ).
        oplSent = TRUE.
    END.
END PROCEDURE.

/* Updates resouce status */
PROCEDURE updateStatus:
    DEFINE INPUT PARAMETER iprRowId AS ROWID NO-UNDO.

    DEFINE VARIABLE cStatus                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBrokerNameServer      AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cBrokerNameServerPort  AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE lEmailNotificationSent AS LOGICAL   NO-UNDO.

    /* Not adding no-wait as the status must be updated */
    FIND serverResource 
        WHERE ROWID(serverResource) EQ iprRowId EXCLUSIVE-LOCK. 
    
	IF AVAILABLE serverResource THEN DO:
        
		CASE serverResource.resourceType:
            
    	         WHEN "Node" THEN
                    RUN getNodeStatus(
                        INPUT serverResource.port,
                        OUTPUT cStatus
                        ).      
                        
                WHEN "AdminServer" THEN DO:
                    
                    IF serverResource.port NE ipcAdminServerPort THEN
                        serverResource.port = ipcAdminServerPort.
                        
                    RUN getAdminServerStatus(
                        INPUT  serverResource.port,
                        OUTPUT cStatus
                        ).  
       
                END.                              
                WHEN "NameServer" THEN DO:
                    
                    IF serverResource.name NE ipcNameServerName THEN
                        serverResource.name = ipcNameServerName.
                    IF serverResource.port NE ipcNameServerport THEN
                        serverResource.port = ipcNameServerPort.
                    
                    RUN getNameServerStatus (
                        INPUT  serverResource.Name,
                        OUTPUT cStatus
                        ).
                END.     
                WHEN "AppServer" THEN DO:
                
                    IF serverResource.name NE ipcAppServerName THEN
                        serverResource.name = ipcAppServerName.
                    IF serverResource.port NE ipcAppServerport THEN
                        serverResource.port = ipcAppServerPort.
                        
                    RUN getASBrokerStatus(
                        INPUT  serverResource.Name,
                        OUTPUT cStatus
                        ).
                END.
        END CASE.
            
        ASSIGN serverResource.resourceStatus = cStatus
               serverResource.notified       = (IF cStatus EQ "Running" THEN NO ELSE serverResource.notified)
               serverResource.statusDateTime = NOW.

        IF serverResource.resourceStatus = "Stopped" AND 
           NOT serverResource.notified THEN DO:
            
            RUN sendEmail(
                BUFFER serverResource,
                OUTPUT lEmailNotificationSent
                ).

            serverResource.notified = lEmailNotificationSent.
        END.
                    
    END.
RELEASE serverResource.
END PROCEDURE.

/* gets Emailconfig Body */
PROCEDURE getEmailConfigBody:
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
PROCEDURE getEmailConfigSubject:
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

