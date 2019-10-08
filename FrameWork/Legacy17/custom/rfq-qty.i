/* rfq-qty.i */

FIND FIRST asi.module NO-LOCK WHERE module.module EQ 'rfq' NO-ERROR.
IF AVAILABLE module AND module.is-used THEN DO:
  IF module.expire-date EQ ? OR module.expire-date GE TODAY THEN DO:
    IF NOT CONNECTED('rfq') AND SEARCH('addon\rfq.pf') NE ? THEN
    CONNECT -pf VALUE(SEARCH('addon\rfq.pf')) NO-ERROR.
    IF CONNECTED('rfq') THEN DO:
      &IF DEFINED(getrfq) NE 0 &THEN
      IF quotehd.rfq EQ '' THEN
      RUN custom/getrfq.p (quotehd.company,quotehd.loc,quotehd.est-no,
                           OUTPUT quotehd.rfq).
      &ENDIF
      RUN custom/rfq-qty.p (quotehd.company,quotehd.loc,quotehd.est-no,
                            quotehd.rfq,quoteitm.part-no,quoteqty.qty,
                            quoteqty.price,quoteqty.uom,TODAY,quoteqty.rels).
      /* DISCONNECT rfq. */
    END. /* if connected */
  END. /* expire-date */
END. /* avail module */
