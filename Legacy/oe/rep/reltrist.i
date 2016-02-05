/* ---------------------------------------------- oe/rep/reltrist.i 04/99 RLL */
/* print oe Release/Picking tickets when RELPRINT = "TriState"                */
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
        and oe-rel.po-no    eq oe-relh.po-no
        and oe-rel.i-no     eq oe-rell.i-no
      no-lock no-error.

  if not avail oe-rel then
  find first oe-rel
      where oe-rel.company  eq oe-rell.company
        and oe-rel.ord-no   eq oe-rell.ord-no
	    and oe-rel.line     eq oe-rell.line
	    and oe-rel.rel-date eq oe-relh.rel-date
	    and oe-rel.ship-no  eq oe-relh.ship-no
	    and oe-rel.po-no    eq oe-relh.po-no
	    and oe-rel.i-no     eq oe-rell.i-no
      no-lock no-error.

  correct-po = if avail oe-ord then oe-ord.po-no else "".

  find first oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq oe-rell.ord-no
        and oe-ordl.i-no    eq oe-rell.i-no
        and oe-ordl.line    eq oe-rell.line
      no-lock no-error.

  v-to-ship =
  if AVAIL oe-ordl and oe-ordl.pr-uom = "CS" then
    oe-rell.cases + if oe-rell.partial > 0 then 1 else 0
  else
    oe-rell.qty.

  find itemfg of oe-rell no-lock no-error.

  locbin[1] = if oe-rell.loc-bin > "" then oe-rell.loc-bin else "".

  display {1}
          locbin[1]
          oe-rell.i-no
          itemfg.i-name     when avail itemfg
          v-to-ship
      with frame relprint.
  down with frame relprint.

  if avail oe-rel THEN do:
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
