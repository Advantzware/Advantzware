/* carrier.i */

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
