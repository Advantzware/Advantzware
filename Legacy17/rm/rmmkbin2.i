
for each {2}rm-bin
    where {2}rm-bin.company        eq item.company
      and {2}rm-bin.i-no           eq item.i-no
    use-index i-no:
  assign
   {2}rm-bin.qty  = 0
   {2}rm-bin.cost = 0.
end.

IF {1} EQ TODAY THEN
for each rm-bin
    where rm-bin.company      eq item.company
      and rm-bin.i-no         eq item.i-no
    no-lock use-index i-no
     :
    
  find first {2}rm-bin
      where {2}rm-bin.company eq item.company
        and {2}rm-bin.i-no    eq rm-bin.i-no
        and {2}rm-bin.loc     eq rm-bin.loc
        and {2}rm-bin.loc-bin eq rm-bin.loc-bin
        and {2}rm-bin.tag     eq rm-bin.tag
      use-index loc-bin no-error.
  if not avail {2}rm-bin then do:

    create {2}rm-bin.
    assign
     {2}rm-bin.company    = item.company
     {2}rm-bin.loc        = rm-bin.loc
     {2}rm-bin.loc-bin    = rm-bin.loc-bin
     {2}rm-bin.tag        = rm-bin.tag
     {2}rm-bin.i-no       = rm-bin.i-no
     {2}rm-bin.po-no      = int(rm-bin.po-no).
  end.

  /*{rm/rm-mkbin.i {2}}*/

  ASSIGN
      /*lv-uom = rm-rcpth.pur-uom*/
      ld-qty = rm-bin.qty
      ld-cst = rm-bin.cost.

  IF lv-uom EQ "" THEN lv-uom = item.cons-uom.
  
  IF item.cons-uom NE lv-uom THEN DO:
      RUN custom/convquom.p (rm-bin.company, lv-uom, item.cons-uom,
                         item.basis-w, (if item.r-wid eq 0 then item.s-len else 12),
                         (if item.r-wid eq 0 then item.s-wid else item.r-wid), item.s-dep,
                         ld-qty, OUTPUT ld-qty).

      RUN custom/convcuom.p (rm-bin.company, lv-uom, item.cons-uom,
                         item.basis-w, (if item.r-wid eq 0 then item.s-len else 12),
                         (if item.r-wid eq 0 then item.s-wid else item.r-wid), item.s-dep,
                         ld-cst, OUTPUT ld-cst).
  END.

  /*if ld-cst ne ?                              and
  (rm-rcpth.rita-code eq "R" or
  (can-do("A,T",rm-rcpth.rita-code) and
  ld-cst ne 0))                          then do:*/
  
  /*IF rm-rcpth.rita-code EQ "A" THEN {2}rm-bin.cost = ld-cst.*/
  
  {rm/rm-post.i {2}rm-bin.qty {2}rm-bin.cost ld-qty ld-cst}
      
      IF "{2}" EQ "" THEN item.last-cost = ld-cst.
  /*end. /* R */  */

      /*if index("RATC",rm-rcpth.rita-code) ne 0 then*/
      {2}rm-bin.qty = ld-qty +
          /*if rm-rcpth.rita-code eq "C" then 0 else*/ {2}rm-bin.qty.

      /*else
      {1}rm-bin.qty = {1}rm-bin.qty - ld-qty.*/
  
end. /* each rm-rcpth */
ELSE
for each rm-rcpth
    where rm-rcpth.company      eq item.company
      and rm-rcpth.i-no         eq item.i-no
      AND rm-rcpth.trans-date  LE {1}
    no-lock use-index i-no,

    each rm-rdtlh
    where rm-rdtlh.r-no         eq rm-rcpth.r-no
      and rm-rdtlh.rita-code    eq rm-rcpth.rita-code
    USE-INDEX rm-rdtl no-lock
    
    BY rm-rcpth.trans-date
    /*BY rm-rcpth.rec_key
    BY rm-rdtlh.rec_key*/
    BY rm-rcpth.r-no:
    
  find first {2}rm-bin
      where {2}rm-bin.company eq item.company
        and {2}rm-bin.i-no    eq rm-rcpth.i-no
        and {2}rm-bin.loc     eq rm-rdtlh.loc
        and {2}rm-bin.loc-bin eq rm-rdtlh.loc-bin
        and {2}rm-bin.tag     eq rm-rdtlh.tag
      use-index loc-bin no-error.
  if not avail {2}rm-bin then do:
    create {2}rm-bin.
    assign
     {2}rm-bin.company    = item.company
     {2}rm-bin.loc        = rm-rdtlh.loc
     {2}rm-bin.loc-bin    = rm-rdtlh.loc-bin
     {2}rm-bin.tag        = rm-rdtlh.tag
     {2}rm-bin.i-no       = rm-rcpth.i-no
     {2}rm-bin.po-no      = int(rm-rcpth.po-no)
        .
  end.

  {rm/rm-mkbin.i {2}}
end. /* each rm-rcpth */


