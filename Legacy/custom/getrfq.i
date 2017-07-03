/* getrfq.i */

FIND FIRST asi.module NO-LOCK WHERE module.module EQ 'rfq' NO-ERROR.
IF AVAILABLE module AND module.is-used THEN DO:
  IF module.expire-date EQ ? OR module.expire-date GE TODAY THEN DO:
    IF NOT CONNECTED('rfq') AND SEARCH('addon\rfq.pf') NE ? THEN
    CONNECT -pf VALUE(SEARCH('addon\rfq.pf')) NO-ERROR.
    IF CONNECTED('rfq') THEN DO:
      RUN custom/getrfq.p (quotehd.company,quotehd.loc,quotehd.est-no,
                           OUTPUT quotehd.rfq).
      /* DISCONNECT rfq. */
    END. /* connected rfq db */
  END. /* expire-date */
END. /* avail module */
