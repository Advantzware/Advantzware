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
    DEFINE VARIABLE cMonitorFile   AS CHARACTER FORMAT 'X(50)' NO-UNDO.
    DEFINE VARIABLE cAttrList      AS CHARACTER FORMAT 'X(4)'  NO-UNDO.
    DEFINE VARIABLE cFile          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRenamedFile   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcResponseData AS LONGCHAR  NO-UNDO.
    
    DEFINE BUFFER bf-APIInboundEvent FOR APIInboundEvent.
    DEFINE BUFFER bf-APIInbound      FOR APIInbound.
    
    SESSION:SET-WAIT-STATE ("GENERAL").
    
    FOR EACH bf-APIInboundEvent NO-LOCK
        WHERE bf-APIInboundEvent.delayedProcessingStatus EQ "queued"
           BY bf-APIInboundEvent.apiInboundEventID:

        RUN Inbound_GetAPIRouteStatus IN hdInboundProcs (
            INPUT  cocode,
            INPUT  bf-APIInboundEvent.apiRoute,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).            
        IF NOT lSuccess THEN DO:
            &IF DEFINED(monitorActivity) NE 0 &THEN
                RUN monitorActivity (
                    INPUT cMessage,
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
    
        /* skips directories and files which are not csv and xml */
        IF cAttrList NE 'F' OR cMonitorFile BEGINS '.' OR (INDEX(cMonitorFile,'.csv') EQ 0 AND INDEX(cMonitorFile,'.xml') EQ 0) THEN
            NEXT.
        
        cFile = monitorImportDir + "\" + cMonitorFile.
        
        &IF DEFINED(monitorActivity) NE 0 &THEN
            RUN monitorActivity (
                INPUT 'Processing file ' + cFile,
                INPUT TRUE,
                INPUT ''
                ).
        &ENDIF

        IF INDEX(cMonitorFile,'.xml') GT 0 THEN DO:
            COPY-LOB FILE cFile TO lcResponseData NO-ERROR.
            IF lcResponseData NE "" THEN
                /* Currently cXMLOrder api is hardcoded. Need an identifier either by folder name or file name to get eh api route dynamically */
                RUN Inbound_CreateAndProcessRequestForAPIRoute IN hdInboundProcs (
                    INPUT  cocode,
                    INPUT  "/api/cXMLOrder",
                    INPUT  lcResponseData,
                    OUTPUT lSuccess,
                    OUTPUT cMessage
                    ).  
            ELSE 
                cMessage = "Not able to extract contents of " + cFile.                  
            
            &IF DEFINED(monitorActivity) NE 0 &THEN
                RUN monitorActivity (
                    INPUT cMessage,
                    INPUT TRUE,
                    INPUT ''
                    ).
            &ENDIF
                  
        END.
        ELSE IF INDEX(cMonitorFile,'.csv') GT 0 THEN DO:
            RUN LoadRequestsFromCSV IN hdInboundProcs (
                INPUT  cFile,
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
                OUTPUT cRenamedFile,
                OUTPUT lSuccess,
                OUTPUT cMessage
                ).

            OS-RENAME VALUE(cFile) VALUE(cRenamedFile).

            &IF DEFINED(monitorActivity) NE 0 &THEN
                RUN monitorActivity (
                    INPUT 'File moved from ' + cFile + ' to ' + cRenamedFile,
                    INPUT TRUE,
                    INPUT ''
                    ).
            &ENDIF            
        END.
    END.
    INPUT CLOSE. 
    
    SESSION:SET-WAIT-STATE ("").
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