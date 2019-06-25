
DEF INPUT PARAM  ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-stat  AS CHAR NO-UNDO.

FIND oe-rel WHERE ROWID(oe-rel) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-rel THEN
FIND FIRST oe-ord OF oe-rel NO-LOCK NO-ERROR.

IF AVAIL oe-ord THEN DO:
  {oe/rel-stat.i op-stat}
END.

ELSE op-stat = "S".
