/*util/updovun.p Update oe-ordl.over-pct and under-pct from oe-ord*/
DISABLE TRIGGERS FOR LOAD OF oe-ordl.
SESSION:SET-WAIT-STATE("general").
DEF VAR i AS INT NO-UNDO.

MESSAGE "Are you ready to update order line's over/under% from order header?" VIEW-AS ALERT-BOX
    QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.

IF ll-ans THEN
FOR EACH oe-ordl:
    FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
    IF AVAIL oe-ord AND (oe-ordl.over-pct = 0 OR oe-ordl.under-pct = 0) THEN
       ASSIGN oe-ordl.under-pct = oe-ord.under-pct
              oe-ordl.over-pct = oe-ord.over-pct.
END.

SESSION:SET-WAIT-STATE("").
IF ll-ans THEN MESSAGE "Completed. " VIEW-AS ALERT-BOX.

