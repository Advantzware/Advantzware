/*
release oe-rel.
find first oe-rell
    where oe-rell.company eq cocode
      and oe-rell.r-no    eq oe-boll.r-no
      and oe-rell.i-no    eq oe-boll.i-no
      and oe-rell.line    eq oe-boll.line
    no-lock no-error.

if avail oe-rell then do:
  find first xoe-relh of oe-rell no-lock.
  find first oe-rel
      where oe-rel.company eq cocode
        and oe-rel.ord-no  eq xoe-relh.ord-no
        and oe-rel.line    eq oe-rell.line
        and oe-rel.link-no eq oe-rell.r-no
        and oe-rel.i-no    eq oe-rell.i-no
      no-lock no-error.

  if not avail oe-rel then
  find first oe-rel
      where oe-rel.company  eq cocode
        and oe-rel.ord-no   eq xoe-relh.ord-no
        and oe-rel.rel-no   eq xoe-relh.rel-no
        and oe-rel.b-ord-no eq xoe-relh.b-ord-no
        and oe-rel.i-no     eq oe-rell.i-no
        and oe-rel.line     eq oe-rell.line
      no-lock no-error.
end.

v-po-no = if avail oe-rel and oe-rel.po-no ne "" then oe-rel.po-no
          else oe-bolh.po-no.
*/

v-po-no = oe-boll.po-no.

