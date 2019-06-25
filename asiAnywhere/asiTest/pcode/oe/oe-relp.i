/* --------------------------------------------------- oe/oe-relp.i 01/98 JLF */
/* ORDER ENTRY MODULE - O/E RELEASE EDIT /                                    */
/* -------------------------------------------------------------------------- */

  post-blok:
  do transaction on error undo, next headblok:
    v-first-release = yes.

    FIND upd-oe-relh WHERE ROWID(upd-oe-relh) EQ ROWID(oe-relh) EXCLUSIVE NO-WAIT NO-ERROR.
    IF NOT AVAIL upd-oe-relh THEN UNDO post-blok, NEXT headblok.

    for each oe-rell
        where oe-rell.company eq cocode
          and oe-rell.r-no    eq oe-relh.r-no
        use-index r-no:

      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq oe-rell.i-no
          use-index i-no no-error.

      if not avail itemfg then undo post-blok, next headblok.

      find first oe-ord
          where oe-ord.company eq cocode
            and oe-ord.ord-no  eq oe-rell.ord-no  /* used be oe-relh.ord-no */
          no-lock no-error.

      if (not avail oe-ord)                                           or
         (avail oe-ord and oe-ord.inv-no ne 0 and oe-ord.stat eq "X") then
        undo post-blok, next headblok.

      else do:
        assign
         oe-rell.printed = yes
         oe-rell.posted  = yes.

        if (oe-rell.s-code eq "S" or oe-rell.s-code eq "B") and
           not oe-relh.deleted                              then
          itemfg.q-rel = itemfg.q-rel + oe-rell.qty.

        RELEASE itemfg.

        if avail oe-ord then
        find first oe-ordl
            where oe-ordl.company eq cocode
              and oe-ordl.ord-no  eq oe-ord.ord-no
              and oe-ordl.i-no    eq oe-rell.i-no
              and oe-ordl.line    eq oe-rell.line
            use-index ord-no no-error.

        else undo post-blok, next headblok.

        if avail oe-ordl and not oe-relh.deleted and oe-rell.s-code ne "I" then
          oe-ordl.t-rel-qty = oe-ordl.t-rel-qty + oe-rell.qty.

        RELEASE oe-ordl.

        RUN oe/upschrel.p (ROWID(oe-rell)).
      end. /* else do: */
    end. /* each oe-rell */

    upd-oe-relh.posted = YES.
  end. /* post-blok */

/* end--------------------------------- copyright 1998 advanced software inc. */
