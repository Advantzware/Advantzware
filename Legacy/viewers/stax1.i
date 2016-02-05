
stax.tax-code1[{1}]:SCREEN-VALUE = CAPS(stax.tax-code1[{1}]:SCREEN-VALUE).

IF stax.tax-code1[{1}]:SCREEN-VALUE NE ""                          AND
   stax.tax-code1[{1}]:SCREEN-VALUE NE stax.tax-group:SCREEN-VALUE AND
   NOT CAN-FIND(FIRST b-stax
                  WHERE b-stax.company   EQ cocode
                    AND b-stax.tax-group EQ stax.tax-code1[{1}]:SCREEN-VALUE
                    AND ROWID(b-stax)    NE ROWID(stax))          THEN DO:
  MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
  APPLY "entry" TO stax.tax-code1[{1}].
  RETURN ERROR.
END.
