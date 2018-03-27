/* shipto.i */
/*
  WHEN "{&FIRST-EXTERNAL-TABLE}.carrier" THEN
  DO:
    {custom/getcmpny.i}
    FIND carrier
        WHERE carrier.company = gcompany
          AND carrier.loc = {&FIRST-EXTERNAL-TABLE}.loc:SCREEN-VALUE
          AND carrier.carrier = {&FIRST-EXTERNAL-TABLE}.carrier:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    carrier_dscr = IF NOT AVAILABLE carrier THEN ""
                   ELSE carrier.dscr.
    DISPLAY carrier_dscr. 
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.dest-code" THEN
  DO:
    {custom/getcmpny.i}
    FIND carr-mtx
        WHERE carr-mtx.company = gcompany
          AND carr-mtx.loc = {&FIRST-EXTERNAL-TABLE}.loc:SCREEN-VALUE
          AND carr-mtx.carrier = {&FIRST-EXTERNAL-TABLE}.carrier:SCREEN-VALUE
          AND carr-mtx.del-zone = {&FIRST-EXTERNAL-TABLE}.dest-code:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    carr-mtx_del-dscr = IF NOT AVAILABLE carr-mtx THEN ""
                        ELSE carr-mtx.del-dscr. 
    DISPLAY carr-mtx_del-dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.loc" THEN
  DO:
    {custom/getcmpny.i}
    FIND loc
        WHERE loc.company = gcompany
          AND loc.loc = {&FIRST-EXTERNAL-TABLE}.loc:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    loc_dscr = IF NOT AVAILABLE loc THEN ""
               ELSE loc.dscr.
    DISPLAY loc_dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.tax-code" THEN
  DO:
    FIND first stax
        WHERE stax.tax-group = {&FIRST-EXTERNAL-TABLE}.tax-code:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    stax_tax-dscr = IF NOT AVAILABLE stax THEN ""
                    ELSE stax.tax-dscr[1].
    DISPLAY stax_tax-dscr.
  END.
*/
