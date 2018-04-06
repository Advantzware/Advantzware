DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-comp AS LOG INIT NO NO-UNDO.


FIND ar-invl WHERE ROWID(ar-invl) EQ ip-rowid NO-LOCK NO-ERROR.

op-comp = AVAIL ar-invl AND ar-invl.ord-no NE 0 AND NOT ar-invl.misc AND
          NOT CAN-FIND(FIRST oe-ordl
                       WHERE oe-ordl.company EQ ar-invl.company
                         AND oe-ordl.ord-no  EQ ar-invl.ord-no
                         AND oe-ordl.i-no    EQ ar-invl.i-no
                         AND oe-ordl.is-a-component EQ NO).
