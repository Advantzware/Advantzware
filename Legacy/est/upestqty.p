
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF BUFFER b-est-qty FOR est-qty.

DEF TEMP-TABLE tt-qty FIELD tt-qty AS INT INDEX tt-qty tt-qty.

DEF VAR li AS INT NO-UNDO.


FIND est WHERE ROWID(est) EQ ip-rowid EXCLUSIVE NO-ERROR.

IF AVAIL est AND est.est-type GE 5 AND est.est-type LE 6 THEN
FIND FIRST eb
    WHERE eb.company EQ est.company
      AND eb.est-no  EQ est.est-no
      AND eb.form-no NE 0
    NO-LOCK NO-ERROR.

IF AVAIL eb THEN
FIND FIRST est-qty
    WHERE est-qty.company EQ eb.company
      AND est-qty.est-no  EQ eb.est-no
      AND est-qty.eqty    EQ eb.eqty
    NO-ERROR.

IF AVAIL est-qty THEN DO:
  /*FOR EACH b-est-qty
      WHERE b-est-qty.company EQ est-qty.company
        AND b-est-qty.est-no  EQ est-qty.est-no
        AND ROWID(b-est-qty)  NE ROWID(est-qty)
      NO-LOCK:
    CREATE tt-qty.
    tt-qty = b-est-qty.eqty.
  END.

  FOR EACH probe
      WHERE probe.company EQ est-qty.company
        AND probe.est-no  EQ est-qty.est-no
      NO-LOCK:
    CREATE tt-qty.
    tt-qty = probe.est-qty.
  END.

  ASSIGN
   est-qty.qty    = 0
   est-qty.qty[1] = eb.eqty
   li             = 1.

  FOR EACH tt-qty
      WHERE tt-qty NE eb.eqty
        AND tt-qty GT 0
      BREAK BY tt-qty:
    IF FIRST-OF(tt-qty) THEN DO:
      li = li + 1.
      IF li LE 20 THEN est-qty.qty[li] = tt-qty.
    END.
  END.*/

  ASSIGN
   est.est-qty[1] = est-qty.qty[1]
   est.est-qty[2] = est-qty.qty[2]
   est.est-qty[3] = est-qty.qty[3]
   est.est-qty[4] = est-qty.qty[4].
END.

