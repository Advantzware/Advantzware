/* Local delete for po-ord */
/* If the po-ord record is deleted (buffer po-ord should not be available) then make the API call,
   else reset the context for further outbound requests. The preparation of request data should be made
   in methods\viewers\delete\po-ord.i   */
/* Note - The variable hdOutboundProcs is defined in include file methods\viewers\delete\po-ord.i */
IF VALID-HANDLE(hdOutboundProcs) AND NOT adm-new-record THEN DO:    
    IF NOT AVAILABLE po-ord THEN DO:
        RUN Outbound_Execute IN hdOutboundProcs (
            OUTPUT lSuccess,
            OUTPUT cMessage
            ) NO-ERROR.
    END.
    /* Reset the context in OutbounddProcs once processing of outbound request is completed */
    RUN Outbound_ResetContext IN hdOutboundProcs.
END.