
DEF INPUT PARAM ip-rowid AS   ROWID        NO-UNDO.
DEF INPUT PARAM ip-q-no  LIKE quotehd.q-no NO-UNDO.


FOR EACH oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-LOCK:
  FIND FIRST reftable
      WHERE reftable.reftable EQ "oe-ordl.q-no"
        AND reftable.company  EQ oe-ordl.company
        AND reftable.loc      EQ STRING(oe-ordl.ord-no,"9999999999")
        AND reftable.code     EQ oe-ordl.i-no
        AND reftable.code2    EQ STRING(oe-ordl.line,"9999999999")
      NO-ERROR.
  IF NOT AVAIL reftable THEN DO:
    CREATE reftable.
    ASSIGN
     reftable.reftable = "oe-ordl.q-no"
     reftable.company  = oe-ordl.company
     reftable.loc      = STRING(oe-ordl.ord-no,"9999999999")
     reftable.code     = oe-ordl.i-no
     reftable.code2    = STRING(oe-ordl.line,"9999999999").
  END.
  reftable.val[1] = ip-q-no.
END.
