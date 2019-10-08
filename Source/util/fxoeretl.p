
SESSION:SET-WAIT-STATE ("general").

FOR EACH oe-retl
    WHERE CAN-FIND(FIRST oe-reth
                   WHERE oe-reth.company EQ oe-retl.company
                     AND oe-reth.r-no    EQ oe-retl.r-no
                     AND oe-reth.posted  EQ YES)
    NO-LOCK,
    FIRST oe-ordl
    WHERE oe-ordl.company EQ oe-retl.company
      AND oe-ordl.ord-no  EQ oe-retl.ord-no
      AND oe-ordl.i-no    EQ oe-retl.i-no
    EXCLUSIVE
    BREAK BY oe-retl.company
          BY oe-retl.ord-no
          BY oe-retl.i-no
    TRANSACTION:

  IF FIRST-OF(oe-retl.i-no) THEN DO:
    oe-ordl.ship-qty = 0.

    FOR EACH oe-boll
        WHERE oe-boll.company EQ oe-ordl.company
          AND oe-boll.ord-no  EQ oe-ordl.ord-no
          AND oe-boll.i-no    EQ oe-ordl.i-no
          AND oe-boll.line    EQ oe-ordl.line
          AND oe-boll.s-code  NE "I"
          AND CAN-FIND(FIRST oe-bolh OF oe-boll WHERE oe-bolh.posted EQ YES)
        NO-LOCK:
      oe-ordl.ship-qty = oe-ordl.ship-qty + oe-boll.qty.
    END.
  END.

  oe-ordl.ship-qty = oe-ordl.ship-qty -
                     MIN(oe-ordl.ship-qty,oe-retl.qty-return-inv).
END.

SESSION:SET-WAIT-STATE ("").
