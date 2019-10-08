
DEF BUFFER b-oe-boll FOR oe-boll.
DEF BUFFER b-oe-bolh FOR oe-bolh.
DEF BUFFER b-oe-rell FOR oe-rell.
DEF BUFFER b-oe-rel  FOR oe-rel.

DEF VAR lv-ord-no LIKE oe-rell.ord-no NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF oe-ordl.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ oe-boll.company NO-LOCK.

FIND sys-ctrl
    WHERE sys-ctrl.company eq oe-boll.company
      AND sys-ctrl.name    eq "ADDRELSE"
    NO-LOCK NO-ERROR.
v-do-bol = AVAIL sys-ctrl AND sys-ctrl.log-fld AND oe-ctrl.ship-from.

FIND FIRST b-oe-boll
    WHERE b-oe-boll.company  EQ oe-boll.company
      AND b-oe-boll.ord-no   EQ oe-boll.ord-no
      AND b-oe-boll.line     EQ oe-boll.line
      AND b-oe-boll.i-no     EQ oe-boll.i-no
      AND b-oe-boll.r-no     EQ oe-boll.r-no
      AND b-oe-boll.rel-no   EQ oe-boll.rel-no
      AND b-oe-boll.b-ord-no EQ oe-boll.b-ord-no
      AND b-oe-boll.po-no    EQ oe-boll.po-no
      AND ROWID(b-oe-boll)   NE ROWID(oe-boll)
      AND CAN-FIND(FIRST b-oe-bolh WHERE b-oe-bolh.b-no   EQ b-oe-boll.b-no
                                     AND b-oe-bolh.posted EQ NO)
    USE-INDEX ord-no NO-LOCK NO-ERROR.

IF NOT AVAIL b-oe-boll THEN DO:
  FOR EACH oe-rell
      WHERE oe-rell.company  EQ oe-boll.company
        AND oe-rell.ord-no   EQ oe-boll.ord-no
        AND oe-rell.line     EQ oe-boll.line
        AND oe-rell.i-no     EQ oe-boll.i-no
        AND oe-rell.r-no     EQ oe-boll.r-no
        AND oe-rell.rel-no   EQ oe-boll.rel-no
        AND oe-rell.b-ord-no EQ oe-boll.b-ord-no
        AND oe-rell.po-no    EQ oe-boll.po-no
        AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
      USE-INDEX ord-no NO-LOCK:

    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ oe-boll.company
          AND oe-ordl.ord-no  EQ oe-rell.ord-no
          AND oe-ordl.i-no    EQ oe-rell.i-no
          AND oe-ordl.line    EQ oe-rell.line
        USE-INDEX ord-no NO-ERROR.

    IF AVAIL oe-ordl THEN DO:
      IF oe-rell.s-code NE "I" THEN
        oe-ordl.t-rel-qty = oe-ordl.t-rel-qty - oe-rell.qty.
      IF oe-ordl.t-rel-qty LT 0 THEN oe-ordl.t-rel-qty = 0.
    END.

    lv-ord-no = oe-rell.ord-no.

    FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
  
    IF v-do-bol THEN DO:
      FIND b-oe-rell WHERE ROWID(b-oe-rell) EQ ROWID(oe-rell) NO-ERROR.
      IF AVAIL b-oe-rell THEN DELETE b-oe-rell.

      IF AVAIL oe-relh THEN DO:
        FIND FIRST b-oe-rell
            WHERE b-oe-rell.company EQ oe-relh.company
              AND b-oe-rell.r-no    EQ oe-relh.r-no
            USE-INDEX r-no NO-LOCK NO-ERROR.
        IF NOT AVAIL b-oe-rell THEN DO:
          IF AVAIL oe-relh THEN DO:
            FIND CURRENT oe-relh NO-ERROR.
            oe-relh.posted = NO.
            DELETE oe-relh.
            FIND CURRENT oe-relh NO-LOCK NO-ERROR.
          END.
        END.
      END.
    END.

    ELSE DO:
      FIND b-oe-rell WHERE ROWID(b-oe-rell) EQ ROWID(oe-rell) NO-ERROR.
      IF AVAIL b-oe-rell THEN b-oe-rell.posted = NO.

      IF AVAIL oe-relh THEN DO:
        FIND FIRST b-oe-rell
            WHERE b-oe-rell.company EQ oe-relh.company
              AND b-oe-rell.r-no    EQ oe-relh.r-no
              AND b-oe-rell.posted  EQ YES
            USE-INDEX r-no NO-LOCK NO-ERROR.
        IF NOT AVAIL b-oe-rell THEN DO:
          FIND CURRENT oe-relh NO-ERROR.
          oe-relh.posted = NO.
          FIND CURRENT oe-relh NO-LOCK NO-ERROR.
        END.
      END.
    END.
     
    FOR EACH oe-rel
        WHERE oe-rel.company EQ oe-boll.company
          AND oe-rel.ord-no  EQ lv-ord-no
          AND oe-rel.link-no NE 0
        USE-INDEX ord-item NO-LOCK:

      FOR EACH b-oe-rell
          WHERE b-oe-rell.company  EQ oe-rel.company
            AND b-oe-rell.r-no     EQ oe-rel.link-no
            AND b-oe-rell.ord-no   EQ oe-rel.ord-no
            AND b-oe-rell.i-no     EQ oe-rel.i-no
            AND b-oe-rell.line     EQ oe-rel.line
            AND b-oe-rell.rel-no   EQ oe-rel.rel-no
            AND b-oe-rell.b-ord-no EQ oe-rel.b-ord-no
            AND b-oe-rell.po-no    EQ oe-rel.po-no
            AND b-oe-rell.posted   EQ YES
          USE-INDEX r-no
          BREAK BY b-oe-rell.r-no:
        b-oe-rell.link-no = oe-rel.r-no.
        IF LAST(b-oe-rell.r-no) THEN LEAVE.
      END.

      

      /*IF NOT AVAIL b-oe-rell THEN DO:*/
        FIND b-oe-rel WHERE ROWID(b-oe-rel) EQ ROWID(oe-rel) NO-ERROR.

        IF AVAIL b-oe-rel THEN b-oe-rel.link-no = 0.
      /*END.*/
    END.

    RUN oe/rel-stat-upd.p (ROWID(oe-rell)).
  END. /* each oe-rell */
END.

