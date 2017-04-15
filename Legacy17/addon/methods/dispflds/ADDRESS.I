/* address.i */

WHEN "{&FIRST-EXTERNAL-TABLE}.zipcode" THEN
DO:
  FIND zipcode
      WHERE zipcode.zipcode = {&FIRST-EXTERNAL-TABLE}.zipcode:SCREEN-VALUE
        AND zipcode.pref_type = {&FIRST-EXTERNAL-TABLE}.pref_type:SCREEN-VALUE
        AND zipcode.pref# = INTEGER({&FIRST-EXTERNAL-TABLE}.pref#:SCREEN-VALUE)
      NO-LOCK NO-ERROR.
  ASSIGN
    zipcode_city = IF NOT AVAILABLE zipcode THEN ""
                   ELSE zipcode.city
    zipcode_country = IF NOT AVAILABLE zipcode THEN ""
                      ELSE zipcode.country
    zipcode_state = IF NOT AVAILABLE zipcode THEN ""
                    ELSE zipcode.state.
  DISPLAY zipcode_city zipcode_country zipcode_state.
END.
