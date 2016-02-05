/* rm/polclose.p */
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-qty LIKE item.q-ono NO-UNDO.
DEF INPUT PARAM ip-uom LIKE item.cons-uom no-undo.

DEF VAR ld AS DEC NO-UNDO.
DEF VAR v-overrun-qty LIKE po-ordl.ord-qty NO-UNDO.
DEF VAR v-underrun-qty LIKE po-ordl.ord-qty NO-UNDO.


FIND po-ordl WHERE ROWID(po-ordl) EQ ip-rowid NO-ERROR.

IF AVAIL po-ordl THEN DO:
  ASSIGN
   v-overrun-qty  = po-ordl.cons-qty *
                    (1 + (po-ordl.over-pct / 100))
   v-underrun-qty = po-ordl.cons-qty *
                    (1 - (po-ordl.under-pct / 100)).

  FIND FIRST item
      WHERE item.company EQ po-ordl.company
        AND item.i-no    EQ po-ordl.i-no
      NO-ERROR.

  /* If orderline status is not "C" */
  IF AVAIL item AND po-ordl.stat NE "C" THEN DO:
    ld = ip-qty.

    IF ip-uom NE item.cons-uom THEN
      RUN sys/ref/convquom.p (ip-uom, item.cons-uom,
                            item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                            ld, OUTPUT ld).

    IF item.cons-uom EQ "EA" THEN ld = ROUND(ld,0).

    /* Update item qty on order */
    ASSIGN item.q-ono = (item.q-ono - ld).

    ASSIGN ld = po-ordl.t-rec-qty - po-ordl.cons-qty.


    IF ld GT 0 THEN DO:
      IF po-ordl.cons-uom NE item.cons-uom THEN
        RUN sys/ref/convquom.p (po-ordl.cons-uom, item.cons-uom,
                              item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                              ld, OUTPUT ld).

      IF item.cons-uom EQ "EA" THEN ld = ROUND(ld,0).

      /* update item qty on order. */
      item.q-ono = item.q-ono + ld.
    END.

    /* update item qty on order. */
    if item.q-ono lt 0 then item.q-ono = 0.
  END. /* If orderline status is not "C" */


  /* If received qty < underrun qty, then close the PO line. */
  IF po-ordl.t-rec-qty LT v-underrun-qty THEN DO:
    IF po-ordl.stat EQ "C" THEN RUN po/closepol.p (ROWID(po-ordl), 1).

    ASSIGN po-ordl.stat = "P".
  END.

  ELSE IF po-ordl.stat NE "C" THEN
      RUN po/closepol.p (ROWID(po-ordl), -1).
END.
