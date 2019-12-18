DEF BUFFER tmp-oe-boll FOR oe-boll.
DEF VAR v-sum-qty LIKE oe-boll.qty NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cBOLPartialFlag AS CHARACTER NO-UNDO .
DEFINE VARIABLE iRelQty AS INTEGER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BOLPartialFlag", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    cBOLPartialFlag = cRtnChar NO-ERROR.

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

  IF  cBOLPartialFlag EQ "Release Quantity" THEN do:
      iRelQty = 0.
      FOR EACH oe-rell FIELDS(qty) NO-LOCK
          WHERE oe-rell.company EQ cocode 
          AND oe-rell.r-no    eq oe-boll.r-no :
          iRelQty = iRelQty + oe-rell.qty .
      END.

      oe-boll.p-c = oe-boll.qty + v-sum-qty GE iRelQty .
  END.
  ELSE DO:
      oe-boll.p-c = oe-boll.qty + v-sum-qty GE
                    (oe-ordl.qty * (1 - (oe-ordl.under-pct / 100))).
  END. /* cBOLPartialFlag EQ "Order Quantity"*/
END.

/* To catch multiple tags selected, need to examine all other lines */
RUN oe/oe-bolpc.p (INPUT ROWID(oe-boll), "{1}").
