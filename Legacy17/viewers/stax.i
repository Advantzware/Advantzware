
IF stax.tax-group:SCREEN-VALUE NE stax.tax-code1[{1}]:SCREEN-VALUE THEN DO:
  FIND b-stax
      WHERE b-stax.company   EQ cocode
        AND b-stax.tax-group BEGINS stax.tax-code1[{1}]:SCREEN-VALUE
        AND b-stax.tax-group EQ b-stax.tax-code1[1]
        AND ROWID(b-stax)    NE ROWID(stax)
      NO-LOCK NO-ERROR.
  IF AVAIL b-stax THEN
    ASSIGN
     stax.tax-code1[{1}]:SCREEN-VALUE = CAPS(b-stax.tax-code1[1])
     stax.tax-dscr1[{1}]:SCREEN-VALUE = b-stax.tax-dscr1[1]
     stax.tax-rate1[{1}]:SCREEN-VALUE = STRING(b-stax.tax-rate1[1])
     stax.tax-frt1[{1}]:SCREEN-VALUE  = STRING(b-stax.tax-frt1[1])
     stax.tax-acc1[{1}]:SCREEN-VALUE  = b-stax.tax-acc1[1].
END.
