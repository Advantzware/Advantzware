
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF VAR v-rel-stat AS CHAR NO-UNDO.
DEF TEMP-TABLE w-rel NO-UNDO LIKE oe-rel
      INDEX seq-no r-no po-no.

&SCOPED-DEFINE using-fields USING company ord-no rel-no b-ord-no i-no line po-no r-no

DISABLE TRIGGERS FOR LOAD OF oe-rel.

FIND ar-invl NO-LOCK
    WHERE ROWID(ar-invl) EQ ip-rowid
      AND CAN-FIND(FIRST ar-inv
                   WHERE ar-inv.x-no   EQ ar-invl.x-no
                     AND ar-inv.posted EQ YES)
    NO-ERROR.

IF AVAIL ar-invl THEN
FOR EACH oe-boll NO-LOCK
    WHERE oe-boll.company EQ ar-invl.company
      AND oe-boll.b-no    EQ ar-invl.b-no
      AND oe-boll.ord-no  EQ ar-invl.ord-no
      AND oe-boll.i-no    EQ ar-invl.i-no
      AND oe-boll.po-no   EQ ar-invl.po-no:

  CREATE w-rel.
  BUFFER-COPY oe-boll {&using-fields} TO w-rel NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DELETE w-rel.
END.

ELSE
FIND FIRST oe-boll NO-LOCK WHERE ROWID(oe-boll) EQ ip-rowid NO-ERROR.

IF AVAIL oe-boll THEN DO:
  CREATE w-rel.
  BUFFER-COPY oe-boll {&using-fields} TO w-rel NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DELETE w-rel.
END.

ELSE
FIND FIRST oe-rell NO-LOCK WHERE ROWID(oe-rell) EQ ip-rowid NO-ERROR.

IF AVAIL oe-rell THEN DO:
  CREATE w-rel.
  BUFFER-COPY oe-rell {&using-fields} TO w-rel NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DELETE w-rel.
END.

FOR EACH w-rel
    BREAK BY w-rel.company
          BY w-rel.ord-no
          BY w-rel.rel-no
          BY w-rel.b-ord-no
          BY w-rel.i-no
          BY w-rel.line
          BY w-rel.po-no:

  /* IF LAST-OF(w-rel.po-no) THEN - task 02231208 */
  FOR EACH oe-rel
      WHERE oe-rel.company  EQ w-rel.company
        AND oe-rel.ord-no   EQ w-rel.ord-no
        AND oe-rel.i-no     EQ w-rel.i-no
        AND oe-rel.line     EQ w-rel.line
        AND oe-rel.rel-no   EQ w-rel.rel-no
        AND oe-rel.b-ord-no EQ w-rel.b-ord-no
        AND oe-rel.po-no    EQ w-rel.po-no:

    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-rel-stat).
    IF oe-rel.stat NE v-rel-stat THEN
      oe-rel.stat = v-rel-stat.
  END.
END.

