/* ---------------------------------------------- oe/rep/boltrist.i 01/99 JLF */
/* Print Tri-State BOL                                                        */
/* -------------------------------------------------------------------------- */

for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:

  find first oe-ordl
      where oe-ordl.company eq cocode
	    and oe-ordl.ord-no  eq oe-boll.ord-no
	    and oe-ordl.i-no    eq oe-boll.i-no
	    and oe-ordl.line    eq oe-boll.line
      no-lock no-error.

  find first oe-ord
      where oe-ord.company eq cocode
	    and oe-ord.ord-no  eq oe-boll.ord-no
      no-lock no-error.

  find first itemfg
      where itemfg.company eq oe-boll.company
	    and itemfg.i-no    eq oe-boll.i-no
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

  if oe-ordl.pr-uom eq "CS" then
    v-to-ship = oe-boll.cases + (if oe-boll.partial gt 0 then 1 else 0).
  else
    v-to-ship = oe-boll.qty.

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

  display {1} oe-boll.loc-bin
/*
	  oe-boll.i-no
*/
	  oe-ordl.part-no
	  itemfg.i-name
/*
	  v-to-ship
*/
	  oe-ordl.qty
       with frame bol-mid.
  down {1} with frame bol-mid.

/*
  if avail itemfg and itemfg.part-dscr1 ne "" then
    put {1} itemfg.part-dscr1 format "x(30)" at 26 skip.
*/
  if avail oe-ordl and oe-ordl.part-dscr1 ne "" then
    put {1} oe-ordl.part-dscr1 format "x(30)" at 26 skip.
  if avail oe-ordl and oe-ordl.part-dscr2 ne "" then
    put {1} oe-ordl.part-dscr2 format "x(30)" at 26 skip.

  v-tot-sqft = itemfg.t-sqft * oe-boll.qty.

  if oe-boll.partial gt 0 then
  do:
    put {1} "SQFT:" at 26 v-tot-sqft skip.
    put {1} oe-boll.cases format ">>9" to 47 "x" to 49 
  	    oe-boll.qty-case format ">>>>9" to 56
  	    "1 x" to 62 oe-boll.partial format ">>>>9" to 68 
	    "=" to 70 v-to-ship format ">>>,>>9" to 78 skip.
  end.
  else
  do:
    put {1} "SQFT:" at 26 v-tot-sqft skip.
    put {1} oe-boll.cases format ">>9" to 47 "x" to 49 
  	    oe-boll.qty-case format ">>>>9" to 56
	    "=" to 70 v-to-ship format ">>>,>>9" to 78 skip.
  end.

  if v-print-pal and avail fg-bin then
    put {1} v-pallets format ">9" at 25
	    " SKID @"
	    fg-bin.cases-unit
	    " CTN AND"
	    v-partial format ">>9"
	    " CTN" skip.

  put {1} skip(1).

  oe-boll.printed = yes.
end. /* for each oe-boll */

do i = 1 to 4:
  if oe-bolh.ship-i[i] ne "" then put {1} oe-bolh.ship-i[i] at 11 skip.
end.

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
