
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF VAR li AS INT NO-UNDO.
DEF VAR li1 AS INT NO-UNDO.
DEF VAR li2 AS INT NO-UNDO.


FIND est WHERE ROWID(est) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL est THEN
DO li = 1 TO 2:
  ASSIGN
   li1 = IF li EQ 1 THEN 0 ELSE 500
   li2 = li1.

  FOR EACH est-op
      WHERE est-op.company EQ est.company
        AND est-op.est-no  EQ est.est-no
        AND est-op.line    GE li1
        AND est-op.line    LT li1 + 500,
      FIRST eb OF est NO-LOCK
      BY est-op.qty BY est-op.s-num BY est-op.b-num BY est-op.d-seq BY est-op.op-pass:
      
    ASSIGN
     li2         = li2 + 1
     est-op.line = li2.

    IF est.est-type EQ 1 OR est.est-type EQ 5 OR est.est-type EQ 6 THEN DO:
      IF est-op.qty EQ 0 THEN est-op.qty = eb.eqty.
    END.

    ELSE
    IF est-op.qty EQ eb.eqty THEN est-op.qty = 0.

    ELSE
    IF est-op.qty NE 0 THEN DELETE est-op.
  END.
END.
