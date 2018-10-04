
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-cost AS DEC DECIMALS 10 NO-UNDO.

{sys/inc/var.i SHARED}

DEF VAR v-cost LIKE fg-rctd.std-cost NO-UNDO.

{fg/fullset.i NEW}


FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL itemfg THEN DO:
  RUN fg/fullset.p (ROWID(itemfg)).

  FOR EACH tt-fg-set,
      
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ tt-fg-set.part-no
      NO-LOCK:

    v-cost = itemfg.last-cost.
    IF itemfg.prod-uom NE "M" THEN
      RUN sys/ref/convcuom.p(itemfg.prod-uom, "M",
                             0, 0, 0, 0, v-cost, OUTPUT v-cost).

    op-cost = op-cost + (v-cost * tt-fg-set.part-qty-dec).
  END.
END.
