
for each {2}rm-bin
    where {2}rm-bin.company        eq item.company
      and {2}rm-bin.i-no           eq item.i-no
    use-index i-no:
  assign
   {2}rm-bin.qty  = 0
   {2}rm-bin.cost = 0.
end.

IF {1} EQ TODAY THEN
for each rm-rcpth
    where rm-rcpth.company      eq item.company
      and rm-rcpth.i-no         eq item.i-no
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
     {2}rm-bin.po-no      = int(rm-rcpth.po-no).
  end.

  {rm/rm-mkbin.i {2}}
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
