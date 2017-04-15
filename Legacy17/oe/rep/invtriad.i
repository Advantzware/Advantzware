/* ---------------------------------------------- oe/rep/invtriad.i 12/98 JLF */
/* PRINT Triad Invoice                                                        */
/* -------------------------------------------------------------------------- */

page {1}.

assign
 j             = int("{1}" eq "") + 1
 v-tot-disc[j] = 0
 v-tot-sf[j]   = 0
 v-tot-wt[j]   = 0.

for each inv-line
    where inv-line.r-no eq inv-head.r-no
    no-lock,

    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq inv-line.i-no
      and itemfg.procat  ne "FS"
    no-lock

    break by inv-line.i-no
          by inv-line.line:

  if first(inv-line.i-no) then do:
    hide {1} frame inv-bot2.
    view {1} frame inv-bot1.
  end.

  v-lines = 2.
  if inv-line.i-name     ne "" then v-lines = v-lines + 1.
  if inv-line.part-dscr1 ne "" then v-lines = v-lines + 1.
  if inv-line.part-dscr2 ne "" then v-lines = v-lines + 1.
  if inv-line.i-no       ne "" then v-lines = v-lines + 1.

  if line-counter {2} - 1 + v-lines gt page-size {2} + 1 then page {1}.

  if last(inv-line.i-no) then do:
    hide {1} frame inv-bot1.
    view {1} frame inv-bot2.
  end.

  find first oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq inv-line.ord-no
        and oe-ordl.i-no    eq inv-line.i-no
        and oe-ordl.line    eq inv-line.line
      no-lock no-error.

  assign
   v-part-comp   = if not avail oe-ordl                                  or
                    oe-ordl.ship-qty + inv-line.ship-qty ge oe-ordl.qty then "C"
                   else "P"
   v-inv-price   = inv-line.t-price / inv-line.inv-qty
   v-inv-price   = v-inv-price / (1 - (inv-line.disc / 100))
   v-tot-disc[j] = v-tot-disc[j] +
                   ((v-inv-price * inv-line.inv-qty) - inv-line.t-price).

  if v-inv-price eq ? then v-inv-price = 0.

  display {1}
          oe-ordl.qty               when avail oe-ordl
          inv-line.ship-qty         when not avail oe-ordl  @ oe-ordl.qty
          trim(string(oe-ordl.ord-no,">>>>>9"))
                                    when avail oe-ordl      @ inv-line.i-no
          ""                        when not avail oe-ordl  @ inv-line.i-no
          inv-line.po-no
          inv-line.ship-qty
          v-part-comp
          inv-line.price                                    @ v-inv-price
          v-inv-price * inv-line.inv-qty                    @ inv-line.t-price

      with frame inv-mid1.
  down {1} with frame inv-mid1.

  do i = 1 to 5:
    v-part-dscr = if i eq 1 then itemfg.i-name
                  else
                  if i eq 2 then itemfg.part-dscr1
                  else
                  if i eq 3 then itemfg.part-dscr2
                  else
                  if i eq 4 then itemfg.part-no
                  else           itemfg.i-no.

    if v-part-dscr ne "" then put {1} v-part-dscr at 8.
  end.

  put {1} skip(1).

  assign
   v-tot-sf[j] = v-tot-sf[j] + (inv-line.ship-qty * itemfg.t-sqft)
   v-tot-wt[j] = v-tot-wt[j] + (inv-line.ship-qty / 100 * itemfg.weight-100).
end. /* for each inv-line */

for each inv-misc
    where inv-misc.company eq cocode
      and inv-misc.r-no    eq inv-head.r-no
      and inv-misc.bill    eq "Y"
    no-lock

    break by inv-misc.charge:

  if first(inv-misc.charge) then do:
    hide {1} frame inv-bot2.
    view {1} frame inv-bot1.
  end.

  v-lines = 2.

  if line-counter {2} - 1 + v-lines gt page-size {2} + 1 then page {1}.

  if last(inv-misc.charge) then do:
    hide {1} frame inv-bot1.
    view {1} frame inv-bot2.
  end.

  display {1}
          inv-misc.charge
          inv-misc.dscr
          inv-misc.amt

      with frame inv-mid2.
  down {1} with frame inv-mid2.
end. /* each inv-misc */

for each inv-line
    where inv-line.r-no eq inv-head.r-no
    no-lock,

    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq inv-line.i-no
      and itemfg.procat  eq "FS"
    no-lock

    break by inv-line.i-no:
    
  if first(inv-line.i-no) then do:
    hide {1} frame inv-bot2.
    view {1} frame inv-bot1.
  end.

  v-lines = 2.

  if line-counter {2} - 1 + v-lines gt page-size {2} + 1 then page {1}.

  if last(inv-line.i-no) then do:
    hide {1} frame inv-bot1.
    view {1} frame inv-bot2.
  end.
    
  display {1}
          itemfg.i-no      @ inv-misc.charge
          inv-line.t-price @ inv-misc.amt

      with frame inv-mid2.
  down {1} with frame inv-mid2.
end.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */
