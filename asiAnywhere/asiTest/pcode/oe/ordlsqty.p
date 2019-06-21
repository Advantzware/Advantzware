
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-inv-qty LIKE oe-ordl.inv-qty NO-UNDO.
DEF OUTPUT PARAM op-ship-qty LIKE oe-ordl.ship-qty NO-UNDO.

DEF BUFFER b-oe-ordl FOR oe-ordl.


FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-ordl THEN
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

 /* From oe-bolp3.i */
 IF oe-boll.s-code NE "S" AND NOT oe-ordl.is-a-component THEN
   op-inv-qty = op-inv-qty + oe-boll.qty.

 IF (oe-boll.s-code NE "I"                                            OR
    CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})) THEN
   op-ship-qty = op-ship-qty + oe-boll.qty.
END.
