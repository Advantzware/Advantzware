
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  
FIND ef WHERE ROWID(ef) EQ ip-rowid NO-ERROR.
IF AVAIL ef THEN DO:
  ef.blank-qty = 0.
  FOR EACH eb OF ef NO-LOCK:
    ef.blank-qty = ef.blank-qty + 1.
  END.
END.
