PROCEDURE ShareEstimate:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdOutboundProcs AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lIsAPIActive    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSuccess        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
        
    DEFINE BUFFER bf-est              FOR est.
    DEFINE BUFFER bf-apiOutboundEvent FOR apiOutboundEvent.
    
    IF NOT AVAILABLE eb THEN DO:
        MESSAGE "Estimate does not exist"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    FIND FIRST bf-est NO-LOCK
         WHERE bf-est.company EQ eb.company
           AND bf-est.est-no  EQ eb.est-no
         NO-ERROR.
    IF NOT AVAILABLE bf-est THEN DO:
        MESSAGE "Estimate does not exist"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
        
    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.
    
    RUN Outbound_IsApiScopeActive IN hdOutboundProcs (eb.company, "", "SendEstimate", "", "", "ShareEstimate", OUTPUT lIsAPIActive).
    
    IF NOT lIsAPIActive THEN DO:
        MESSAGE "Cannot run 'SendEstimate' API. Please verify if 'SendEstimate' API and 'ShareEstimate' trigger is active" 
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    SESSION:SET-WAIT-STATE ("GENERAL").
        
    RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
        INPUT  eb.company,                             /* Company Code (Mandatory) */
        INPUT  "",                                     /* Location Code (Mandatory) */
        INPUT  "SendEstimate",                         /* API ID (Mandatory) */
        INPUT  "",                                     /* Scope ID */
        INPUT  "",                                     /* Scope Type */
        INPUT  "ShareEstimate",                        /* Trigger ID (Mandatory) */
        INPUT  "est",                                  /* Comma separated list of table names for which data being sent (Mandatory) */
        INPUT  STRING(ROWID(bf-est)),                  /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
        INPUT  eb.est-no,                              /* Primary ID for which API is called for (Mandatory) */   
        INPUT  "Share Estimate from E-C",              /* Event's description (Optional) */
        OUTPUT lSuccess,                               /* Success/Failure flag */
        OUTPUT cMessage                                /* Status message */
        ).

    SESSION:SET-WAIT-STATE ("").

    RUN Outbound_GetEvents IN hdOutboundProcs (OUTPUT TABLE ttAPIOutboundEvent).
    
    FIND FIRST ttAPIOutboundEvent NO-LOCK NO-ERROR.
    IF AVAILABLE ttAPIOutboundEvent THEN DO:
        FIND FIRST bf-apiOutboundEvent NO-LOCK
             WHERE bf-apiOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.APIOutboundEventID
             NO-ERROR.
    END.    
    
    MESSAGE "Share " + STRING(AVAILABLE bf-apiOutboundEvent, "Complete/Failed") + "." "Check API Outbound Event (N-A-3) for status"
        VIEW-AS ALERT-BOX INFORMATION.
    
    FINALLY:
        IF VALID-HANDLE (hdOutboundProcs) THEN
            DELETE PROCEDURE hdOutboundProcs.
    END FINALLY.
END PROCEDURE.    