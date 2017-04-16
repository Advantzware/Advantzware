  
RELEASE oe-rell.
RELEASE oe-relh.
RELEASE oe-boll.
RELEASE oe-bolh.
RELEASE ar-invl.

IF oe-rel.link-no EQ 0 THEN DO:   /* actual release is not posted yet*/
  FOR EACH oe-rell
      WHERE oe-rell.company  EQ oe-rel.company
        AND oe-rell.ord-no   EQ oe-rel.ord-no
        AND oe-rell.i-no     EQ oe-rel.i-no
        AND oe-rell.line     EQ oe-rel.line
        AND oe-rell.rel-no   EQ oe-rel.rel-no
        AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
        AND oe-rell.po-no    EQ oe-rel.po-no
      USE-INDEX ord-no NO-LOCK,
      FIRST oe-relh
      WHERE oe-relh.r-no    EQ oe-rell.r-no
        AND oe-relh.posted  EQ NO
        AND oe-relh.deleted EQ NO
      NO-LOCK:
    LEAVE.
  END.
END.

ELSE DO: 
  FIND FIRST oe-rell
      WHERE oe-rell.company  EQ oe-rel.company
        AND oe-rell.r-no     EQ oe-rel.link-no
        AND oe-rell.ord-no   EQ oe-rel.ord-no
        AND oe-rell.rel-no   EQ oe-rel.rel-no
        AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
        AND oe-rell.i-no     EQ oe-rel.i-no
        AND oe-rell.line     EQ oe-rel.line
        AND oe-rell.po-no    EQ oe-rel.po-no
        AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
      USE-INDEX r-no NO-LOCK NO-ERROR.
      
  IF AVAIL oe-rell THEN
  FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
END.
