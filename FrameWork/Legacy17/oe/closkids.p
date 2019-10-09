
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF BUFFER b-oe-ordl FOR oe-ordl.


FIND oe-ord WHERE ROWID(oe-ord) EQ ip-rowid NO-ERROR.

DISABLE TRIGGERS FOR LOAD OF oe-ord.
DISABLE TRIGGERS FOR LOAD OF oe-ordl.
DISABLE TRIGGERS FOR LOAD OF oe-rell.
DISABLE TRIGGERS FOR LOAD OF oe-boll.

IF AVAIL oe-ord THEN DO:
  oe-ord.opened = LOOKUP(oe-ord.stat,"C,D,Z") EQ 0.
      
  FOR EACH b-oe-ordl NO-LOCK
      WHERE b-oe-ordl.company EQ oe-ord.company
        AND b-oe-ordl.ord-no  EQ oe-ord.ord-no:

    FIND oe-ordl WHERE ROWID(oe-ordl) EQ ROWID(b-oe-ordl)
        EXCLUSIVE NO-ERROR NO-WAIT.
    
    IF NOT AVAIL oe-ordl THEN NEXT.

    oe-ordl.opened = oe-ord.opened.

    FOR EACH oe-rell
        WHERE oe-rell.company EQ oe-ordl.company
          AND oe-rell.ord-no  EQ oe-ordl.ord-no
          AND oe-rell.i-no    EQ oe-ordl.i-no
          AND oe-rell.line    EQ oe-ordl.line
        USE-INDEX ord-no:

      oe-rell.opened = oe-ordl.opened.
    END.

    FOR EACH oe-boll
        WHERE oe-boll.company EQ oe-ordl.company
          AND oe-boll.ord-no  EQ oe-ordl.ord-no
          AND oe-boll.i-no    EQ oe-ordl.i-no
          AND oe-boll.line    EQ oe-ordl.line
        USE-INDEX ord-no:

      oe-boll.opened = oe-ordl.opened.
    END.
    RUN oe/ordlsqty.p (ROWID(oe-ordl),
                   OUTPUT oe-ordl.inv-qty,
                   OUTPUT oe-ordl.ship-qty).
  END.
END.
