/* vend.i */

  WHEN "{&FIRST-EXTERNAL-TABLE}.buyer" THEN
  DO:
    {custom/getcmpny.i}
    FIND buyer
        WHERE buyer.company = gcompany
          AND buyer.buyer = {&FIRST-EXTERNAL-TABLE}.buyer:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    buyer_buyer-n = IF NOT AVAILABLE buyer THEN ""
                ELSE buyer.buyer-n.
    DISPLAY buyer_buyer-n.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.carrier" THEN
  DO:
    {custom/getcmpny.i}
    FIND carrier
        WHERE carrier.company = gcompany
          AND carrier.carrier = {&FIRST-EXTERNAL-TABLE}.carrier:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    carrier_dscr = IF NOT AVAILABLE carrier THEN ""
                   ELSE carrier.dscr.
    DISPLAY carrier_dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.terms" THEN
  DO:
    {custom/getcmpny.i}
    FIND terms
        WHERE terms.company = gcompany
          AND terms.t-code = {&FIRST-EXTERNAL-TABLE}.terms:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    terms_dscr = IF NOT AVAILABLE terms THEN ""
                 ELSE terms.dscr.
    DISPLAY terms_dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.type" THEN
  DO:
    {custom/getcmpny.i}
    FIND ventype
        WHERE ventype.company = gcompany
          AND ventype.type = {&FIRST-EXTERNAL-TABLE}.type:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    ventype_dscr = IF NOT AVAILABLE ventype THEN ""
                   ELSE ventype.dscr.
    DISPLAY ventype_dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.curr-code" THEN
  DO:
    {custom/getcmpny.i}
    FIND currency WHERE currency.company = gcompany
          AND currency.c-code = {&FIRST-EXTERNAL-TABLE}.curr-code:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    curr_dscr = IF NOT AVAILABLE currency THEN ""
                   ELSE currency.c-desc.
    DISPLAY curr_dscr.
  END.
