
DEF PARAM BUFFER io-po-ordl FOR po-ordl.

DEF VAR ld-qty AS DEC NO-UNDO.


IF AVAIL io-po-ordl THEN DO:
  io-po-ordl.t-rec-qty = 0.

  FOR EACH rm-rcpth
      WHERE rm-rcpth.company   EQ io-po-ordl.company
        AND rm-rcpth.i-no      EQ io-po-ordl.i-no
        AND rm-rcpth.po-no     EQ STRING(io-po-ordl.po-no)
        AND rm-rcpth.job-no    EQ io-po-ordl.job-no
        AND rm-rcpth.job-no2   EQ io-po-ordl.job-no2
        AND rm-rcpth.rita-code EQ "R"
      NO-LOCK,
      FIRST item NO-LOCK
      WHERE item.company EQ rm-rcpth.company
        AND item.i-no    EQ rm-rcpth.i-no:

    FOR EACH rm-rdtlh
        WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
        NO-LOCK
        BREAK BY rm-rdtlh.s-num DESC:

      IF rm-rdtlh.s-num EQ io-po-ordl.s-num OR
         io-po-ordl.s-num EQ 0 AND LAST-OF(rm-rdtlh.s-num) THEN DO:

        ld-qty = rm-rdtlh.qty.
 
        IF rm-rcpth.pur-uom NE io-po-ordl.cons-uom THEN
          RUN sys/ref/convquom (rm-rcpth.pur-uom, io-po-ordl.cons-uom,
                                item.basis-w, io-po-ordl.s-len, io-po-ordl.s-wid, item.s-dep,
                                ld-qty, OUTPUT ld-qty).

        IF io-po-ordl.cons-uom EQ "EA" THEN DO:
          {sys/inc/roundup.i ld-qty}
        END.

        io-po-ordl.t-rec-qty = io-po-ordl.t-rec-qty + ld-qty.
      END.
    END.
  END.
END.
