
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF BUFFER b-oe-ordl FOR oe-ordl.

DEF VAR li AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR l-orig-line AS INT NO-UNDO.

FIND oe-ord WHERE ROWID(oe-ord) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-ord THEN
FOR EACH oe-ordl
    WHERE oe-ordl.company EQ oe-ord.company
      AND oe-ordl.ord-no  EQ oe-ord.ord-no
      AND oe-ordl.line    GT 0
    BY oe-ordl.line:

  ASSIGN
   l-orig-line  = oe-ordl.LINE 
   li           = li + 1
   oe-ordl.line = li.
  IF li NE l-orig-line THEN DO:

    ll = NOT CAN-FIND(FIRST b-oe-ordl
                      WHERE b-oe-ordl.company EQ oe-ordl.company
                        AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
                        AND b-oe-ordl.i-no    EQ oe-ordl.i-no
                        AND ROWID(b-oe-ordl)  NE ROWID(oe-ordl)).

    RUN oe/lineUpdate.p (INPUT   ROWID(oe-ordl),
                         INPUT   oe-ordl.i-no,
                         INPUT   l-orig-line,
                         INPUT   ll).
  END.
END.
