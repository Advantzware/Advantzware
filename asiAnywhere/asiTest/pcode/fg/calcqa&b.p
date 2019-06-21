
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-q-alloc LIKE itemfg.q-alloc NO-UNDO.
DEF OUTPUT PARAM op-q-back LIKE itemfg.q-back NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEF VAR ld-qty LIKE oe-ordl.t-ship-qty NO-UNDO.


FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid NO-LOCK NO-ERROR.


IF AVAIL itemfg THEN DO:

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ itemfg.company NO-LOCK NO-ERROR.

cocode = itemfg.company.

/*** itemfg.q-alloc & itemfg.q-back from customer orders ***/
IF AVAIL oe-ctrl THEN
FOR EACH oe-ordl
    WHERE oe-ordl.company EQ itemfg.company
      AND oe-ordl.opened  EQ YES 
      AND oe-ordl.i-no    EQ itemfg.i-no
      AND oe-ordl.stat    NE "C"
      AND CAN-FIND(FIRST oe-ord OF oe-ordl WHERE oe-ord.type NE "T")
      AND oe-ordl.is-a-component EQ NO
    USE-INDEX item NO-LOCK: 

  ld-qty = 0.

  IF oe-ctrl.u-inv THEN
  FOR EACH oe-boll
    WHERE oe-boll.company EQ oe-ordl.company
      AND oe-boll.ord-no  EQ oe-ordl.ord-no
      AND oe-boll.i-no    EQ oe-ordl.i-no
      AND oe-boll.line    EQ oe-ordl.line
      AND oe-boll.s-code  NE "T"
      AND CAN-FIND(FIRST oe-bolh
                   WHERE oe-bolh.b-no   EQ oe-boll.b-no
                     AND oe-bolh.posted EQ YES
                   USE-INDEX b-no)
    USE-INDEX ord-no NO-LOCK:  
      
    ld-qty = ld-qty + oe-boll.qty.
  END.
        
  ELSE ld-qty = oe-ordl.ship-qty.

  op-q-alloc = op-q-alloc + (oe-ordl.qty - MIN(ld-qty,oe-ordl.qty)).

  FOR EACH oe-rell
      WHERE oe-rell.company  EQ itemfg.company
        AND oe-rell.ord-no   EQ oe-ordl.ord-no
        AND oe-rell.i-no     EQ oe-ordl.i-no
        AND oe-rell.line     EQ oe-ordl.line
        AND oe-rell.b-ord-no NE 0
        AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
      USE-INDEX ord-no NO-LOCK:

    FOR EACH oe-boll
        WHERE oe-boll.company  EQ oe-rell.company
          AND oe-boll.ord-no   EQ oe-rell.ord-no
          AND oe-boll.line     EQ oe-rell.line
          AND oe-boll.i-no     EQ oe-rell.i-no
          AND oe-boll.r-no     EQ oe-rell.r-no
          AND oe-boll.rel-no   EQ oe-rell.rel-no
          AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
          AND oe-boll.po-no    EQ oe-rell.po-no
          AND CAN-FIND(FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no)
        USE-INDEX ord-no NO-LOCK:
      LEAVE.
    END.

    IF NOT AVAIL oe-boll THEN op-q-back = op-q-back + oe-rell.qty.
  END.
END.

END.
