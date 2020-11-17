/* monitor.w */

{custom/monitor.w "API" "InboundAPIMonitor"}

&SCOPED-DEFINE monitorActivity

PROCEDURE postMonitor:
/*------------------------------------------------------------------------------
  Purpose:    creates monitor, process and response directories and import 
              monitored  files
  Parameters: <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cMonitorFile    AS CHARACTER FORMAT 'X(50)' NO-UNDO.
    DEFINE VARIABLE cAttrList       AS CHARACTER FORMAT 'X(4)'  NO-UNDO.
    DEFINE VARIABLE cCSVFile        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRenamedCSVFile AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-APIInboundEvent FOR APIInboundEvent.
    DEFINE BUFFER bf-APIInbound      FOR APIInbound.
    
    FOR EACH bf-APIInboundEvent NO-LOCK
        WHERE bf-APIInboundEvent.delayedProcessingStatus EQ "queued"
           BY bf-APIInboundEvent.apiInboundEventID:
        FIND FIRST bf-APIInbound NO-LOCK
             WHERE bf-APIInbound.apiRoute    EQ bf-APIInboundEvent.apiRoute
               AND bf-APIInbound.inActive    EQ FALSE
               AND bf-APIInbound.canBeQueued EQ TRUE
             NO-ERROR.
        IF NOT AVAILABLE bf-APIInbound THEN DO:
            &IF DEFINED(monitorActivity) NE 0 &THEN
                RUN monitorActivity (
                    INPUT 'Inbound configuration for ' + bf-APIInboundEvent.apiRoute + ' is not available or inactive or not enabled for queueing',
                    INPUT TRUE,
                    INPUT ''
                    ).
            &ENDIF            
            NEXT.
        END.
        
        RUN Inbound_ReTrigger IN hdInboundProcs (
            INPUT  bf-APIInboundEvent.apiInboundEventID,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).
        &IF DEFINED(monitorActivity) NE 0 &THEN
            RUN monitorActivity (
                INPUT 'Inbound Event: ' + STRING(bf-APIInboundEvent.apiInboundEventID) + ", Status: " + STRING(lSuccess, "Success/Failed") + ", Message: " + cMessage,
                INPUT TRUE,
                INPUT ''
                ).
        &ENDIF        
    END.   

    RUN FileSys_ValidateDirectory (
        INPUT  monitorImportDir,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).
    IF NOT lSuccess THEN DO:
        &IF DEFINED(monitorActivity) NE 0 &THEN
            RUN monitorActivity (
                INPUT 'Directory ' + monitorImportDir + ' is not valid',
                INPUT TRUE,
                INPUT ''
                ).
        &ENDIF
        LEAVE. 
    END.     

    INPUT FROM OS-DIR(monitorImportDir) NO-ECHO.
    REPEAT:
        SET cMonitorFile ^ cAttrList.
    
        /* skips directories and files which are not csv */
        IF cAttrList NE 'F' OR cMonitorFile BEGINS '.' OR INDEX(cMonitorFile,'.csv') EQ 0 THEN
            NEXT.
        
        cCSVFile = monitorImportDir + "\" + cMonitorFile.

        &IF DEFINED(monitorActivity) NE 0 &THEN
            RUN monitorActivity (
                INPUT 'Processing file ' + cCSVFile,
                INPUT TRUE,
                INPUT ''
                ).
        &ENDIF

        RUN LoadRequestsFomCSV IN hdInboundProcs (
            INPUT  cCSVFile,
            OUTPUT TABLE ttInboundRequest
            ) NO-ERROR.
    
        RUN ProcessRequests IN hdInboundProcs (
            INPUT-OUTPUT TABLE ttInboundRequest,
            INPUT cUser,
            INPUT cPassword
            ).
        
        FOR EACH ttInboundRequest:
            &IF DEFINED(monitorActivity) NE 0 &THEN
                RUN monitorActivity (
                    INPUT 'Inbound Event: ' + STRING(ttInboundRequest.eventID) + ", Status: " + STRING(ttInboundRequest.success, "Success/Failed") + ", Message: " + ttInboundRequest.exception,
                    INPUT TRUE,
                    INPUT ''
                    ).
            &ENDIF        
        END.
        
        RUN FileSys_CreateDirectory (
            INPUT  monitorImportDir + "\" + "processed",
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).

        IF lSuccess THEN DO:
            RUN FileSys_GetUniqueFileName (
                INPUT  monitorImportDir + "\" + "processed" + "\" + cMonitorFile,
                INPUT  TRUE, /* Auto increment file count */
                OUTPUT cRenamedCSVFile,
                OUTPUT lSuccess,
                OUTPUT cMessage
                ).

            OS-RENAME VALUE(cCSVFile) VALUE(cRenamedCSVFile).

            &IF DEFINED(monitorActivity) NE 0 &THEN
                RUN monitorActivity (
                    INPUT 'File moved from ' + cCSVFile + ' to ' + cRenamedCSVFile,
                    INPUT TRUE,
                    INPUT ''
                    ).
            &ENDIF            
        END.
    END.
    INPUT CLOSE. 
    
END PROCEDURE.

PROCEDURE pGetUserDetails:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcUser     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPassword AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    FIND FIRST ASI.users NO-LOCK
         WHERE ASI.users.user_id EQ USERID("ASI")
         NO-ERROR.
    IF NOT AVAILABLE ASI.users THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "Authentication Failed"
            .
        
        RETURN.
    END.
    
    /* Checks _user table */
    FIND FIRST ASI._user NO-LOCK
         WHERE ASI._user._Userid EQ ASI.users.user_id NO-ERROR.
    IF NOT AVAILABLE ASI._user THEN DO:
        ASSIGN 
            oplSuccess = NO
            opcMessage = "Authentication Failed"
            .
               
        RETURN.
    END.
    
    ASSIGN
        opcUser     = _user._Userid
        opcPassword = _user._password
        oplSuccess  = TRUE
        opcMessage  = "Success"
        .
    
END PROCEDURE.