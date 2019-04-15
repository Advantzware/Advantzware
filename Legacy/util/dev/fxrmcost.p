
DEF VAR v-r-qty AS   DEC                    NO-UNDO.
DEF VAR v-i-qty AS   DEC                    NO-UNDO.
DEF VAR v-t-qty AS   DEC                    NO-UNDO.
DEF VAR ld-cst  AS   DEC                    NO-UNDO.
DEF VAR lv-uom  AS   CHAR                   NO-UNDO.

SESSION:SET-WAIT-STATE ("").

FOR EACH item WHERE item.i-code EQ "R" NO-LOCK:

  FOR EACH rm-rcpth
      WHERE rm-rcpth.company   EQ item.company
        AND rm-rcpth.i-no      EQ item.i-no
        AND rm-rcpth.rita-code EQ "R"
      NO-LOCK,

      EACH rm-rdtlh
      WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
        AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
      NO-LOCK,

      FIRST rm-bin
      WHERE rm-bin.company EQ rm-rcpth.company
        AND rm-bin.i-no    EQ rm-rcpth.i-no
        AND rm-bin.loc     EQ rm-rdtlh.loc
        AND rm-bin.loc-bin EQ rm-rdtlh.loc-bin
        AND rm-bin.tag     EQ rm-rdtlh.tag

      BREAK BY rm-rdtlh.loc
            BY rm-rdtlh.loc-bin
            BY rm-rdtlh.tag:

    IF FIRST-OF(rm-rdtlh.tag) THEN rm-bin.cost = 0.

    ASSIGN
     lv-uom = rm-rcpth.pur-uom
     ld-cst = rm-rdtlh.cost.

    IF lv-uom EQ "" THEN lv-uom = item.cons-uom.

    IF item.cons-uom NE lv-uom THEN
      RUN custom/convcuom.p (rm-rcpth.company, lv-uom, item.cons-uom,
                             item.basis-w, item.s-len, item.s-wid, item.s-dep,
                             ld-cst, OUTPUT ld-cst).

    {rm/rm-post.i "rm-bin.qty" "rm-bin.cost" "rm-rdtlh.qty" "ld-cst"}
  END.
END.
