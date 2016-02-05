/* --------------------------------------------- oe/rep/bolroyal.i  08/97 FWK */
/* PRINT Royal BOL                                                            */
/* -------------------------------------------------------------------------- */

    page {1}.

    v-lines = 0.

    for each report where report.term-id eq v-term-id,

        first oe-boll where recid(oe-boll) eq report.rec-id

        by report.key-01
        by report.key-02:

      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq oe-boll.ord-no
            and oe-ordl.i-no    eq oe-boll.i-no
            and oe-ordl.line    eq oe-boll.line
          no-lock no-error.

      if avail oe-ordl then
      find oe-ord
          where oe-ord.company eq oe-ordl.company
            and oe-ord.ord-no  eq oe-ordl.ord-no
          no-lock no-error.
      release oe-rel.
      find first oe-rell
          where oe-rell.company eq oe-boll.company
            and oe-rell.r-no    eq oe-boll.r-no
            AND oe-rell.ord-no  EQ oe-boll.ord-no
            and oe-rell.i-no    eq oe-boll.i-no
            and oe-rell.line    eq oe-boll.line
           no-lock no-error.

      if avail oe-rell then do:
        find first oe-relh of oe-rell no-lock.
        find first oe-rel
            where oe-rel.company eq oe-relh.company
              and oe-rel.ord-no  eq oe-rell.ord-no
              and oe-rel.line    eq oe-rell.line
              and oe-rel.link-no eq oe-rell.r-no
              and oe-rel.ship-no eq oe-relh.ship-no
              and oe-rel.i-no    eq oe-rell.i-no
            no-lock no-error.

        if not avail oe-rel then
          find first oe-rel
              where oe-rel.company  eq oe-relh.company
                and oe-rel.ord-no   eq oe-rell.ord-no
                and oe-rel.line     eq oe-rell.line
                and oe-rel.rel-date eq oe-relh.rel-date
                and oe-rel.ship-no  eq oe-relh.ship-no
                and oe-rel.i-no     eq oe-rell.i-no
              no-lock no-error.
      end.

      v-to-ship = if oe-ordl.pr-uom eq "CS" then
        oe-boll.cases + (if oe-boll.partial gt 0 then 1 else 0)
      else
        oe-boll.qty.

      if avail oe-ordl then
      find first fg-bin
          where fg-bin.company eq cocode
            and fg-bin.loc     eq oe-boll.loc
            and fg-bin.loc-bin eq oe-boll.loc-bin
            and fg-bin.i-no    eq oe-boll.i-no
            and fg-bin.tag     eq oe-boll.tag
            and fg-bin.job-no  eq oe-ordl.job-no
            and fg-bin.job-no2 eq oe-ordl.job-no2
          use-index co-ino no-lock no-error.
      else
      find first fg-bin
          where fg-bin.company eq cocode
            and fg-bin.tag     eq oe-boll.tag
            and fg-bin.loc     eq oe-boll.loc
            and fg-bin.loc-bin eq oe-boll.loc-bin
            and fg-bin.i-no    eq oe-boll.i-no
          use-index tag no-lock no-error.

      if avail fg-bin then
        assign
         v-pallets = truncate((v-to-ship / fg-bin.unit-count),0)
         v-partial = v-to-ship - (v-pallets * fg-bin.unit-count).

      v-shiplines = 2.

      if oe-ordl.part-dscr1 ne "" then     v-shiplines = v-shiplines + 1.

      if v-print-pal and avail fg-bin then v-shiplines = v-shiplines + 1.

      if v-lines + v-shiplines gt 36 then do:
        page {1}.
        v-lines = 0.
      end.
        
      display {1} oe-boll.loc-bin
              oe-boll.i-no
              oe-ordl.i-name
              v-to-ship
          with frame ln-s.
      down {1} with frame ln-s.

      if oe-ordl.part-dscr1 ne "" then
        put {1} oe-ordl.part-dscr1 format "x(30)" at 26 skip.

      if v-print-pal and avail fg-bin then do:
        put {1} v-pallets format ">9" at 25
            " SKID @"
            fg-bin.cases-unit
            " CTN AND"
            v-partial format ">>9"
            " CTN" skip.
      end.

      put {1} skip(1).

      v-lines = v-lines + v-shiplines.

      if "{1}" eq "" then delete report.
    end. /* for each oe-boll */

    v-shiplines = 0.
    do i = 1 to 4:
      if oe-bolh.ship-i[i] ne "" then v-shiplines = v-shiplines + 1.
    end.

    if v-lines + v-shiplines gt 36 then do:
      page {1}.
      v-lines = 0.
    end.

    do i = 1 to 4:
      if oe-bolh.ship-i[i] ne "" then put {1} oe-bolh.ship-i[i] at 11 skip.
    end.

    assign
     v-lines         = v-lines + v-shiplines
     oe-bolh.printed = yes.

/* end ---------------------------------- copr. 1992  Advanced Software, Inc. */
