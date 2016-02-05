
v-loc = shipto.loc.

RELEASE oe-relh.
RELEASE oe-rell.
RELEASE oe-bolh.
RELEASE oe-boll.
RELEASE ar-inv.
RELEASE ar-invl.
      
IF oe-rel.link-no NE 0 THEN DO:
  FIND FIRST oe-rell
      WHERE oe-rell.company EQ oe-rel.company
        AND oe-rell.ord-no  EQ oe-rel.ord-no
        AND oe-rell.r-no    EQ oe-rel.link-no
        AND oe-rell.i-no    EQ oe-rel.i-no
        AND oe-rell.line    EQ oe-rel.line
      NO-LOCK NO-ERROR.
      
  IF AVAIL oe-rell THEN
  FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.

  IF AVAIL oe-relh THEN
  FOR EACH oe-boll
      WHERE oe-boll.company  EQ oe-rell.company
        AND oe-boll.ord-no   EQ oe-rell.ord-no
        AND oe-boll.line     EQ oe-rell.line
        AND oe-boll.i-no     EQ oe-rell.i-no
        AND oe-boll.r-no     EQ oe-rell.r-no
        AND oe-boll.rel-no   EQ oe-rell.rel-no
        AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
      NO-LOCK,
      FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no NO-LOCK:
    LEAVE.
  END.

  IF AVAIL oe-bolh AND oe-bolh.posted THEN DO:
    v-loc = oe-boll.loc.

    FOR EACH ar-invl
        WHERE ar-invl.company EQ oe-boll.company
          AND ar-invl.b-no    EQ oe-boll.b-no
          AND ar-invl.ord-no  EQ oe-boll.ord-no
          AND ar-invl.i-no    EQ oe-boll.i-no
          AND ar-invl.po-no   EQ oe-boll.po-no
        NO-LOCK,
        FIRST ar-inv
        WHERE ar-inv.x-no   EQ ar-invl.x-no
          AND ar-inv.posted EQ YES
        NO-LOCK:
      LEAVE.
    END.
  END.
END.
