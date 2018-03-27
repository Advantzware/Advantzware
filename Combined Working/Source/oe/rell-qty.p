/* -------------------------------------------------- oe/rell-qty.p 01/05 JLF */
/* Reset Release Qty on Order Line                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT  PARAM ip-rowid AS   ROWID.
DEF OUTPUT PARAM op-qty   LIKE oe-ordl.t-rel-qty NO-UNDO.

{sys/inc/var.i NEW SHARED}

op-qty = 0.

FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-ordl THEN DO:
  cocode = oe-ordl.company.

  FOR EACH oe-rell
      WHERE oe-rell.company EQ cocode
        AND oe-rell.ord-no  EQ oe-ordl.ord-no
        AND oe-rell.i-no    EQ oe-ordl.i-no
        AND oe-rell.line    EQ oe-ordl.line
        AND oe-rell.s-code  NE "I"
        AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
        AND NOT CAN-FIND(FIRST oe-boll
                         WHERE oe-boll.company  EQ oe-rell.company
                           AND oe-boll.r-no     EQ oe-rell.r-no
                           AND oe-boll.ord-no   EQ oe-rell.ord-no
                           AND oe-boll.i-no     EQ oe-rell.i-no
                           AND oe-boll.line     EQ oe-rell.line
                           AND oe-boll.rel-no   EQ oe-rell.rel-no
                           AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
                           AND oe-boll.po-no    EQ oe-rell.po-no
                         USE-INDEX ord-no)
      USE-INDEX ord-no NO-LOCK:
   
    op-qty = op-qty + oe-rell.qty.
  END.
END.
