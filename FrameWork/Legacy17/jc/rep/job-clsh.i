DEF VAR ll-act-rate AS LOG NO-UNDO.
DEF VAR ld-tot-rate AS DEC NO-UNDO.
    
for FIRST itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq job-hdr.i-no
    no-lock:
    
  find first work-item WHERE
       work-item.cust-no eq job-hdr.cust-no AND
       work-item.i-no    eq job-hdr.i-no
       no-error.
  if not avail work-item then do:
    create work-item.
    assign
     work-item.i-no    = job-hdr.i-no
     work-item.cust-no = job-hdr.cust-no.
  END.

  ASSIGN
   work-item.qty-ord = work-item.qty-ord + job-hdr.qty
   v-t-qty-ord       = 0
   v-act-price       = 0.

  IF itemfg.isaset AND itemfg.alloc THEN
     FOR EACH fg-act FIELDS(qty)
         WHERE fg-act.company eq job-hdr.company
           AND fg-act.job-no  eq job-hdr.job-no
           AND fg-act.job-no2 eq job-hdr.job-no2
           AND fg-act.i-no    eq job-hdr.i-no
         NO-LOCK:
         work-item.qty-prod = work-item.qty-prod + fg-act.qty.
     END.

  FOR EACH fg-rcpth FIELDS(r-no rita-code company)
      WHERE fg-rcpth.company   EQ job-hdr.company
        AND fg-rcpth.i-no      EQ job-hdr.i-no
        AND fg-rcpth.job-no    EQ job-hdr.job-no
        AND fg-rcpth.job-no2   EQ job-hdr.job-no2
        AND fg-rcpth.rita-code EQ "R"
        NO-LOCK,

      EACH fg-rdtlh FIELDS(qty)
      WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
      NO-LOCK
        
      BREAK BY fg-rcpth.company:

    IF FIRST(fg-rcpth.company) THEN work-item.qty-prod = 0.

    work-item.qty-prod = work-item.qty-prod + fg-rdtlh.qty.
  END.

  for each oe-ordl FIELDS(qty t-price)
      where oe-ordl.company eq cocode
        and oe-ordl.job-no  eq job-hdr.job-no
        and oe-ordl.job-no2 eq job-hdr.job-no2
        and oe-ordl.ord-no  eq job-hdr.ord-no
        and oe-ordl.i-no    eq job-hdr.i-no
      use-index job no-lock:
      
    assign
     v-t-qty-ord = v-t-qty-ord + oe-ordl.qty
     v-act-price = v-act-price + oe-ordl.t-price.
  end.
  
  if v-t-qty-ord eq 0 then v-t-qty-ord = work-item.qty-ord.
  
  if v-act-price eq 0 then do:
     if itemfg.sell-uom eq "EA" then
        v-act-price = itemfg.sell-price.
     ELSE IF itemfg.sell-uom EQ "CS" THEN
        v-act-price = itemfg.sell-price / itemfg.case-count.
     ELSE
        run sys/ref/convcuom.p(itemfg.sell-uom, "EA", 0, 0, 0, 0,
                               itemfg.sell-price, output v-act-price).
                              
     v-act-price = v-act-price * v-t-qty-ord.                          
  end.

  assign
   work-item.qty-ord = v-t-qty-ord.
  IF work-item.qty-ord GT 0 AND work-item.qty-prod GT 0 THEN
      v-act-price       = v-act-price / work-item.qty-ord * work-item.qty-prod.
  v-std-price       = v-std-price + v-act-price.

end.

v-act-price = v-std-price.
