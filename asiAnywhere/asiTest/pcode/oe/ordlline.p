
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF VAR li AS INT NO-UNDO.


FIND oe-ord WHERE ROWID(oe-ord) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-ord THEN
FOR EACH oe-ordl
    WHERE oe-ordl.company EQ oe-ord.company
      AND oe-ordl.ord-no  EQ oe-ord.ord-no
      AND oe-ordl.line    GT 0
    BY oe-ordl.line:

  ASSIGN
   li           = li + 1
   oe-ordl.line = li.
END.
