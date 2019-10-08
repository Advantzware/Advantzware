
ASSIGN
 lv-uom = rm-rcpth.pur-uom
 ld-qty = rm-rdtlh.qty
 ld-cst = rm-rdtlh.cost.

IF lv-uom EQ "" THEN lv-uom = item.cons-uom.

IF item.cons-uom NE lv-uom THEN DO:
  RUN custom/convquom.p (rm-rcpth.company, lv-uom, item.cons-uom,
                         item.basis-w, (if item.r-wid eq 0 then item.s-len else 12),
                         (if item.r-wid eq 0 then item.s-wid else item.r-wid), item.s-dep,
                         ld-qty, OUTPUT ld-qty).

  RUN custom/convcuom.p (rm-rcpth.company, lv-uom, item.cons-uom,
                         item.basis-w, (if item.r-wid eq 0 then item.s-len else 12),
                         (if item.r-wid eq 0 then item.s-wid else item.r-wid), item.s-dep,
                         ld-cst, OUTPUT ld-cst).
END.

if ld-cst ne ?                              and
   (rm-rcpth.rita-code eq "R" or
    (can-do("A,T",rm-rcpth.rita-code) and
     ld-cst ne 0))                          then do:

  IF rm-rcpth.rita-code EQ "A" THEN {1}rm-bin.cost = ld-cst.

  {rm/rm-post.i {1}rm-bin.qty {1}rm-bin.cost ld-qty ld-cst}

  IF "{1}" EQ "" THEN item.last-cost = ld-cst.
end. /* R */

if index("RATC",rm-rcpth.rita-code) ne 0 then
  {1}rm-bin.qty = ld-qty +
                  if rm-rcpth.rita-code eq "C" then 0 else {1}rm-bin.qty.

else
  {1}rm-bin.qty = {1}rm-bin.qty - ld-qty.
  
/*if rm-rcpth.rita-code eq "T"                and
   (rm-rdtlh.loc     ne rm-rdtlh.loc2   or
    rm-rdtlh.loc-bin ne rm-rdtlh.locbin or
    rm-rdtlh.tag     ne rm-rdtlh.tag2)      then do:
  find first {1}rm-bin
      where {1}rm-bin.company eq item.company
        and {1}rm-bin.i-no    eq rm-rcpth.i-no
        and {1}rm-bin.loc     eq rm-rdtlh.loc2
        and {1}rm-bin.loc-bin eq rm-rdtlh.loc-bin2
        and {1}rm-bin.tag     eq rm-rdtlh.tag2
      use-index loc-bin no-error.
  if not avail {1}rm-bin then do:
    create {1}rm-bin.
    assign
     {1}rm-bin.company    = item.company
     {1}rm-bin.i-no       = rm-rcpth.i-no
     {1}rm-bin.loc        = rm-rdtlh.loc2
     {1}rm-bin.loc-bin    = rm-rdtlh.loc-bin2
     {1}rm-bin.tag        = rm-rdtlh.tag2.
  end.

  if ld-cst ne ? then do:
    {rm/rm-post.i {1}rm-bin ld-qty ld-cst}
  end.
  
  {1}rm-bin.qty = {1}rm-bin.qty + ld-qty.
end.*/
