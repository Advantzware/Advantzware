
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-close AS LOG   NO-UNDO.

DEF VAR v-factor       AS   INT NO-UNDO.
DEF VAR v-qty          LIKE oe-ordl.qty NO-UNDO.
DEF VAR v-tax-rate     AS   DEC FORMAT ">,>>9.99<<<" NO-UNDO.
DEF VAR v-frt-tax-rate LIKE v-tax-rate NO-UNDO.
DEF VAR v-dcr-val      LIKE oe-ordl.cost INIT 0 NO-UNDO.
DEF VAR v-uom-rate     AS   INT NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF itemfg.

v-factor = IF ip-close THEN -1 ELSE 1.

FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-ERROR.

IF AVAIL oe-ordl THEN
FIND FIRST oe-ord NO-LOCK
    WHERE oe-ord.company EQ oe-ordl.company
      AND oe-ord.ord-no  EQ oe-ordl.ord-no
    NO-ERROR.

IF AVAIL oe-ord THEN
FIND FIRST cust
    WHERE cust.company eq oe-ord.company
      AND cust.cust-no eq oe-ord.cust-no
    NO-ERROR.

IF AVAIL cust THEN DO:
  RUN ar/cctaxrt.p (INPUT oe-ord.company, oe-ord.tax-gr,
                    OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

  v-qty = oe-ordl.qty - oe-ordl.ship-qty.
  IF v-qty LT 0 THEN v-qty = 0.

  FIND FIRST itemfg
      WHERE itemfg.company eq oe-ordl.company
        AND itemfg.i-no    eq oe-ordl.i-no
      NO-ERROR.
  IF AVAIL itemfg THEN DO:
    IF oe-ord.type NE "T" THEN DO:


      RUN fg/chkfgloc.p (INPUT oe-ordl.i-no, INPUT oe-ord.loc).

      FIND FIRST itemfg-loc 
            WHERE itemfg-loc.company EQ oe-ordl.company
              AND itemfg-loc.i-no    EQ oe-ordl.i-no
              AND itemfg-loc.loc     EQ oe-ord.loc
            EXCLUSIVE-LOCK NO-ERROR.

     
      /* Original Code */
      itemfg.q-alloc = itemfg.q-alloc + (v-qty * v-factor).
      IF itemfg.q-alloc LT 0 THEN itemfg.q-alloc = 0.

      itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.
      IF itemfg.q-avail LT 0 THEN itemfg.q-avail = 0.       

      /* New Code */
      IF AVAIL itemfg-loc THEN DO:

          itemfg-loc.q-alloc = itemfg-loc.q-alloc + (v-qty * v-factor).
          IF itemfg-loc.q-alloc LT 0 THEN
              itemfg-loc.q-alloc = 0.

          itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
          IF itemfg-loc.q-avail LT 0 THEN 
              itemfg-loc.q-avail = 0.     

      END.

    END. /* Ord Type ne 'T' */

    ASSIGN
     itemfg.q-ptd     = itemfg.q-ptd     + (v-qty * v-factor)
     itemfg.q-ord-ytd = itemfg.q-ord-ytd + (v-qty * v-factor).

    IF AVAIL itemfg-loc THEN
    ASSIGN
     itemfg-loc.q-ptd     = itemfg-loc.q-ptd     + (v-qty * v-factor)
     itemfg-loc.q-ord-ytd = itemfg-loc.q-ord-ytd + (v-qty * v-factor).
    FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.

  END.

  IF v-qty GT 0 THEN DO:
    ASSIGN
     v-uom-rate = IF oe-ordl.pr-uom EQ "M"  THEN 1000 ELSE
                  IF oe-ordl.pr-uom EQ "C"  THEN 100  ELSE
                  IF AVAIL itemfg           AND
                    oe-ordl.pr-uom  EQ "CS" THEN itemfg.case-count ELSE 1

     v-dcr-val  = (v-qty / v-uom-rate) * oe-ordl.price
     v-dcr-val  = v-dcr-val - (v-dcr-val * oe-ordl.disc / 100).

    IF oe-ordl.tax THEN
      v-dcr-val = v-dcr-val + (v-dcr-val * v-tax-rate / 100).

    IF AVAIL cust THEN cust.ord-bal = cust.ord-bal + (v-dcr-val * v-factor).
  END.

  oe-ordl.stat = STRING(ip-close,"C/").
END.

