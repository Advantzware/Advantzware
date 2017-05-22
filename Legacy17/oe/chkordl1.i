   
    FIND FIRST b-ord NO-LOCK
        WHERE b-ord.company    EQ b-ordl.company
          AND b-ord.ord-no     EQ b-ordl.ord-no
          AND (b-ord.ord-date  LT oe-ord.ord-date OR
               (b-ord.ord-date EQ oe-ord.ord-date AND
                b-ord.ord-no   LT oe-ord.ord-no))
          AND b-ordl.ship-qty  LT b-ordl.qty * (1 - (b-ord.under-pct / 100))
        NO-ERROR.

    IF AVAIL b-ord THEN DO:
      lv-bin-qty = 0.

      /* If using loadtags, check bins created from tags for line item */
      FOR EACH loadtag
          WHERE loadtag.company   EQ b-ordl.company
            AND loadtag.item-type EQ NO
            AND loadtag.i-no      EQ b-ordl.i-no
            AND loadtag.ord-no    EQ b-ordl.ord-no
          NO-LOCK,

          EACH fg-bin
          WHERE fg-bin.company EQ loadtag.company
            AND fg-bin.i-no    EQ loadtag.i-no
            AND fg-bin.tag     EQ loadtag.tag-no
          NO-LOCK

          BREAK BY fg-bin.tag:

        IF FIRST(fg-bin.tag) THEN lv-bin-qty = 0.

        lv-bin-qty = lv-bin-qty + fg-bin.qty.
      END.

      IF lv-bin-qty GT 0 THEN DO:
        CREATE w-ord.
        w-ord-no = b-ordl.ord-no.
      END.
    END.
