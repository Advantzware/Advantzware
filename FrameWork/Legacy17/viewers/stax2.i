
DEF VAR rowid-val AS ROWID NO-UNDO.

RUN windows/l-stax-g.w (gcompany, FOCUS:SCREEN-VALUE, OUTPUT rowid-val).
FIND b-stax WHERE ROWID(b-stax) EQ rowid-val NO-LOCK NO-ERROR.
IF AVAIL b-stax AND b-stax.tax-group NE FOCUS:SCREEN-VALUE THEN DO:
  FOCUS:SCREEN-VALUE = b-stax.tax-group.
  APPLY "value-changed" TO FOCUS.
END.

RETURN NO-APPLY.
