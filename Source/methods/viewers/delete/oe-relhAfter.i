/* Local delete for oe-relh */
/* If the oe-relh record is deleted (buffer oe-relh should not be available) then make the API call,
   else reset the context for further outbound requests. The preparation of request data should be made
   in methods\viewers\delete\oe-relh.i   */
/* Note - The variable hdOutboundProcs is defined in include file methods\viewers\delete\oe-relh.i */
IF NOT AVAILABLE oe-relh THEN DO:
    RUN Outbound_Execute IN hdOutboundProcs (
        OUTPUT lSuccess,
        OUTPUT cMessage
        ) NO-ERROR.
END.
/* Reset the context in OutbounddProcs once processing of outbound request is completed */
RUN Outbound_ResetContext IN hdOutboundProcs.
