/* ---------------------------------------------- oe/rep/relhop.i 05/00 DJK   */
/* print oe Release/Picking tickets when RELPRINT = "HOP"                     */
/* -------------------------------------------------------------------------- */

      for each oe-rell
          where oe-rell.company eq oe-relh.company
            and oe-rell.r-no    eq oe-relh.r-no
          USE-INDEX r-no:

        find first oe-rel
            where oe-rel.company  eq oe-rell.company
              and oe-rel.ord-no   eq oe-rell.ord-no
              and oe-rel.line     eq oe-rell.line
              and oe-rel.link-no  eq oe-rell.r-no
              and oe-rel.ship-no  eq oe-relh.ship-no
              and oe-rel.i-no     eq oe-rell.i-no
            no-lock no-error.

        if not avail oe-rel then do:
          find first oe-rel
              where oe-rel.company  eq oe-rell.company
                and oe-rel.ord-no   eq oe-rell.ord-no
                and oe-rel.line     eq oe-rell.line
                and oe-rel.rel-date eq oe-relh.rel-date
                and oe-rel.ship-no  eq oe-relh.ship-no
                and oe-rel.i-no     eq oe-rell.i-no
              no-lock no-error.
       end.
       
        correct-po = if avail oe-ord then oe-ord.po-no else "".

        find first oe-ordl
            where oe-ordl.company eq cocode
              and oe-ordl.ord-no  eq oe-rell.ord-no
              and oe-ordl.i-no    eq oe-rell.i-no
              and oe-ordl.line    eq oe-rell.line
            no-lock no-error.

        find itemfg of oe-rell no-lock no-error.
        
        locbin = "".

        if oe-rell.loc-bin > "" then
          locbin[1] = oe-rell.loc-bin.
        else
        do:
          xx = 0.
          if avail itemfg then
          for each fg-bin
              where fg-bin.company eq oe-rell.company
                and fg-bin.i-no eq itemfg.i-no
                and fg-bin.qty > 0
              no-lock break by fg-bin.loc-bin:
            if first-of(fg-bin.loc-bin) and xx lt 10 then
            assign
              xx         = xx + 1
              locbin[xx] = fg-bin.loc-bin.
          end.
        end.

        if oe-rell.po-no ne "" then put {1} oe-rell.po-no skip.

        display {1}
              oe-rell.i-no
              itemfg.part-no     when avail itemfg
              oe-ordl.i-name     when avail oe-ordl
              locbin[1]
              oe-ordl.part-dscr1 when avail itemfg
              itemfg.q-onh
              oe-rell.qty
              locbin[2] when xx ge 2
              with frame relprint.
        down with frame relprint.

        if avail oe-rel then do:
          if oe-rel.ship-i[1] ne "" then
            put {1} oe-rel.ship-i[1] format "x(60)" at 2 skip.
          if oe-rel.ship-i[2] ne "" then
            put {1} oe-rel.ship-i[2] format "x(60)" at 2 skip.
          if oe-rel.ship-i[3] ne "" then
            put {1} oe-rel.ship-i[3] format "x(60)" at 2 skip.
          if oe-rel.ship-i[4] ne "" then
            put {1} oe-rel.ship-i[4] format "x(60)" at 2 skip.
        end.

        oe-rell.printed = true.
      end.

/* END ----------------------------------- copr. 1998 Advanced Software, Inc. */

