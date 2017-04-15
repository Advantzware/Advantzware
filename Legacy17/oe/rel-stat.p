
DEF INPUT PARAM  ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-stat  AS CHAR NO-UNDO.


FIND oe-rel WHERE ROWID(oe-rel) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-rel THEN
FIND FIRST oe-ord NO-LOCK
    WHERE oe-ord.company EQ oe-rel.company
      AND oe-ord.ord-no  EQ oe-rel.ord-no
    NO-ERROR.

IF AVAIL oe-ord THEN DO:
  {oe/rel-stat.i op-stat}
END.

ELSE op-stat = "S".
