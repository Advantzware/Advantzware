DEF INPUT PARAM ipUpdOrd AS LOG NO-UNDO.
DEF PARAM BUFFER io-cust FOR cust.

DEF OUTPUT PARAM op-dec AS DEC NO-UNDO.

DEF BUFFER b-oe-ord FOR oe-ord.

DEF VAR v-tot AS DEC EXTENT 10 NO-UNDO.
DEF VAR v-tax-rate AS DEC NO-UNDO.
DEF VAR v-frt-tax-rate AS DEC NO-UNDO.


IF AVAIL io-cust THEN DO:
  FOR EACH oe-ord
      WHERE oe-ord.company EQ io-cust.company
        AND oe-ord.opened  EQ YES
        AND oe-ord.cust-no eq io-cust.cust-no
      USE-INDEX opened NO-LOCK:

    IF ipUpdOrd THEN DO:
   
        FIND FIRST b-oe-ord WHERE RECID(b-oe-ord) EQ RECID(oe-ord)
            EXCLUSIVE-LOCK no-error NO-WAIT.
              
        IF AVAIL b-oe-ord THEN DO:   /******* CALCULATE TOTALS FOR ORDER *******/
          RUN oe/calcordt.p (ROWID(b-oe-ord)).
          FIND CURRENT b-oe-ord NO-LOCK.
            
          v-tot[4] = b-oe-ord.t-revenue + b-oe-ord.tax.
        END.
    
        ELSE v-tot[4] = oe-ord.t-revenue + oe-ord.tax.
    END.
    ELSE v-tot[4] = oe-ord.t-revenue + oe-ord.tax.

    RUN ar/cctaxrt.p (oe-ord.company, oe-ord.tax-gr,
                      OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

    FOR EACH oe-ordm NO-LOCK
        WHERE oe-ordm.company EQ oe-ord.company
          AND oe-ordm.ord-no  EQ oe-ord.ord-no
          AND oe-ordm.bill    EQ "I":

      v-tot[4] = v-tot[4] - oe-ordm.amt.

      IF oe-ordm.tax AND v-tax-rate GT 0 THEN
        v-tot[4] = v-tot[4] - ROUND((oe-ordm.amt * v-tax-rate) / 100,2).
    END.

    v-tot[1] = 0.

    FOR EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ oe-ord.company
          AND oe-ordl.ord-no  EQ oe-ord.ord-no:

      v-tot[5] = 0.

      IF oe-ordl.stat EQ "C" THEN v-tot[5] = oe-ordl.qty.

      ELSE
      FOR EACH ar-invl NO-LOCK
          WHERE ar-invl.company EQ oe-ordl.company
            AND ar-invl.ord-no  EQ oe-ordl.ord-no
            AND ar-invl.i-no    EQ oe-ordl.i-no
          USE-INDEX ord-no:
        v-tot[5] = v-tot[5] + ar-invl.inv-qty.
      END.

      v-tot[6] = IF v-tot[5] LT oe-ordl.qty THEN v-tot[5] / oe-ordl.qty ELSE 1.

      IF v-tot[6] GT 0 THEN DO:
        ASSIGN
         v-tot[7] = oe-ordl.t-price * v-tot[6]
         v-tot[1] = v-tot[1] + v-tot[7].

        IF oe-ordl.tax AND v-tax-rate GT 0 THEN
          v-tot[1] = v-tot[1] + ROUND(v-tot[7] * v-tax-rate / 100,2).

        IF oe-ord.f-bill THEN
          v-tot[1] = v-tot[1] + (oe-ordl.t-freight * v-tot[6]).
      END.
    END.
      
    IF v-tot[1] GT v-tot[4] THEN v-tot[1] = v-tot[4].
     
    ASSIGN
     v-tot[2] = v-tot[2] + v-tot[1]
     v-tot[3] = v-tot[3] + v-tot[4].
  END.

  FOR EACH inv-head
      WHERE inv-head.company EQ io-cust.company
        AND inv-head.cust-no EQ io-cust.cust-no
        AND inv-head.bol-no  EQ 0
        AND inv-head.terms   NE "CASH"
      NO-LOCK:
    ACCUMULATE inv-head.t-inv-rev (TOTAL).
  END.

  op-dec = v-tot[3] - v-tot[2] + (ACCUM TOTAL inv-head.t-inv-rev).

  IF op-dec LT 0 THEN op-dec = 0.
END.
