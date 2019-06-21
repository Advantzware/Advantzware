DEFINE SHARED VAR lv-stat AS CHARACTER NO-UNDO.  
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

  lv-stat = IF AVAIL oe-rell THEN
          IF oe-rell.b-ord-no EQ 0      THEN "A" ELSE "B"
        ELSE
        IF {2}oe-ord.last-date LT TODAY THEN "I" ELSE
        IF oe-rel.rel-date LT TODAY     THEN "L" ELSE "S".
END.

ELSE DO:
  lv-stat = "P".
  
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

  IF AVAIL oe-relh THEN
  FOR EACH oe-boll
      WHERE oe-boll.company  EQ oe-rell.company
        AND oe-boll.ord-no   EQ oe-rell.ord-no
        AND oe-boll.line     EQ oe-rell.line
        AND oe-boll.i-no     EQ oe-rell.i-no
        AND oe-boll.r-no     EQ oe-rell.r-no
        AND oe-boll.rel-no   EQ oe-rell.rel-no
        AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
        AND oe-boll.po-no    EQ oe-rell.po-no
      USE-INDEX ord-no NO-LOCK,
      FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no NO-LOCK:
    LEAVE.
  END.
  
  IF AVAIL oe-boll AND oe-bolh.posted THEN DO:
    lv-stat = IF oe-boll.s-code EQ "T" THEN "C" ELSE "Z".

    FOR EACH ar-invl
        WHERE ar-invl.company EQ oe-boll.company
          AND ar-invl.b-no    EQ oe-boll.b-no
          AND ar-invl.ord-no  EQ oe-boll.ord-no
          AND ar-invl.i-no    EQ oe-boll.i-no
          AND ar-invl.po-no   EQ oe-boll.po-no
          AND CAN-FIND(FIRST ar-inv WHERE ar-inv.x-no   EQ ar-invl.x-no
                                      AND ar-inv.posted EQ YES)
        NO-LOCK:
      LEAVE.
    END.
  END.

  IF AVAIL ar-invl THEN lv-stat = "C".
END.
MESSAGE "lv-stat"   lv-stat.
