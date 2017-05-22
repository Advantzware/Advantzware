
DEF VAR ld AS DEC DECIMALS 10 NO-UNDO.
DEF VAR lv-stat LIKE po-ord.stat NO-UNDO.
DEF VAR v-underrun-qty LIKE po-ordl.cons-qty NO-UNDO.
DEF VAR v-reduce-qty LIKE po-ordl.cons-qty NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEF BUFFER b-po-ordl FOR po-ordl.


SESSION:SET-WAIT-STATE ("general").

FOR EACH po-ordl WHERE po-ordl.stat NE "C" USE-INDEX stat:
  cocode = po-ordl.company.

  IF po-ordl.item-type THEN DO:
    FIND FIRST item
        WHERE item.company EQ po-ordl.company
          AND item.i-no    EQ po-ordl.i-no
        USE-INDEX i-no NO-ERROR.

    IF AVAIL item THEN DO:
     v-underrun-qty = po-ordl.cons-qty * (1 - (po-ordl.under-pct / 100)).

      IF po-ordl.t-rec-qty GE v-underrun-qty THEN DO:
        po-ordl.stat = "C".

        RUN rm/rm-reset.p (RECID(item)).
      END.
    END.
  END.

  ELSE DO:
    FIND FIRST itemfg
        WHERE itemfg.company EQ po-ordl.company
          AND itemfg.i-no    EQ po-ordl.i-no
        NO-ERROR.
    IF AVAIL itemfg THEN DO:
      v-reduce-qty = 0.

      IF po-ordl.cons-uom EQ "EA" THEN v-reduce-qty = po-ordl.cons-qty.

      ELSE RUN sys/ref/convquom.p (INPUT po-ordl.cons-uom,
                                   INPUT "EA", INPUT 0,
                                   INPUT po-ordl.s-len,
                                   INPUT po-ordl.s-wid,
                                   INPUT 0,
                                   INPUT po-ordl.cons-qty,
                                   OUTPUT v-reduce-qty).

      v-underrun-qty = v-reduce-qty * (1 - (po-ordl.under-pct / 100)).

      IF po-ordl.t-rec-qty GE v-underrun-qty THEN DO:
        po-ordl.stat = "C".

        RUN fg/fg-reset.p (RECID(itemfg)).
      END.
    END.
  END.
END.

SESSION:SET-WAIT-STATE ("").
