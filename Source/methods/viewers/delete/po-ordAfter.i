/* Local delete for po-ord */
/* If the po-ord record is deleted (buffer po-ord should not be available) then make the API call,
   else reset the context for further outbound requests */
IF NOT AVAILABLE po-ord THEN DO:
    RUN Outbound_Execute IN hdOutboundProcs (
        OUTPUT lSuccess,
        OUTPUT cMessage
        ) NO-ERROR.
END.
/* Reset the context in OutbounddProcs once processing of outbound request is completed */
RUN Outbound_ResetContext IN hdOutboundProcs.
