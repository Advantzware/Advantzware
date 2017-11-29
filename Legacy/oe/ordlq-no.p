
DEF INPUT PARAM ip-rowid AS   ROWID        NO-UNDO.
DEF INPUT PARAM ip-q-no  LIKE quotehd.q-no NO-UNDO.


FOR EACH oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-LOCK:
  
  ASSIGN oe-ordl.q-no = ip-q-no.
END.
