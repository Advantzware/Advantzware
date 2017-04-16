DEF BUFFER tmp-oe-boll FOR oe-boll.
DEF VAR v-sum-qty LIKE oe-boll.qty NO-UNDO.


FIND FIRST oe-ordl NO-LOCK
    WHERE oe-ordl.company EQ oe-boll.company
      AND oe-ordl.ord-no  EQ oe-boll.ord-no
      AND oe-ordl.i-no    EQ oe-boll.i-no
    NO-ERROR.

IF AVAIL oe-ordl THEN DO:
  v-sum-qty = 0.
  FOR EACH tmp-oe-boll FIELDS(qty) NO-LOCK
      WHERE tmp-oe-boll.company EQ oe-ordl.company
		AND tmp-oe-boll.ord-no  EQ oe-ordl.ord-no
		AND tmp-oe-boll.i-no    EQ oe-ordl.i-no 
        AND tmp-oe-boll.line    EQ oe-ordl.line
        AND (tmp-oe-boll.rel-no LT oe-boll.rel-no      OR
             (tmp-oe-boll.rel-no EQ oe-boll.rel-no AND
              tmp-oe-boll.b-ord-no LE oe-boll.b-ord-no))
		AND ROWID(tmp-oe-boll)  NE ROWID(oe-boll)
      USE-INDEX ord-no:
    v-sum-qty = v-sum-qty + tmp-oe-boll.qty.
  END.

  oe-boll.p-c = oe-boll.qty + v-sum-qty GE
                (oe-ordl.qty * (1 - (oe-ordl.under-pct / 100))).
END.

/* To catch multiple tags selected, need to examine all other lines */
RUN oe/oe-bolpc.p (INPUT ROWID(oe-boll), "{1}").
