DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
      
DEF VAR li AS INT NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
DEF VAR lv-freight LIKE oe-ord.t-freight NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF oe-ordl.

FIND oe-ord WHERE ROWID(oe-ord) EQ ip-rowid NO-LOCK NO-ERROR.

FOR EACH oe-ordl OF oe-ord NO-LOCK:
  RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT ld).
  ASSIGN
   li         = li + 1
   lv-freight = lv-freight + ld.
END.

ld = oe-ord.t-freight / li.

IF lv-freight NE oe-ord.t-freight THEN
FOR EACH oe-ordl OF oe-ord:
  RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT oe-ordl.t-freight).
  IF lv-freight EQ 0 THEN oe-ordl.t-freight = ld.

  ELSE
    oe-ordl.t-freight = oe-ord.t-freight *
                        (oe-ordl.t-freight / lv-freight).
END.
