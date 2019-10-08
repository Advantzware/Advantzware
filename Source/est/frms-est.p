
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


FIND est WHERE ROWID(est) EQ ip-rowid NO-ERROR.
IF AVAIL est THEN DO:
  est.form-qty = 0.
  FOR EACH ef OF est NO-LOCK BREAK BY ef.eqty:
    est.form-qty = est.form-qty + 1.
    IF LAST-OF(ef.eqty) THEN LEAVE.
  END.
END.
