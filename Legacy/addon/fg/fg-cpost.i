    
for each {1}fg-rcpts
    where {1}fg-rcpts.company   eq cocode
      and {1}fg-rcpts.rita-code eq "C",

    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq {1}fg-rcpts.i-no

    break by {1}fg-rcpts.i-no:

  for each {1}fg-rdtl where {1}fg-rdtl.r-no eq {1}fg-rcpts.r-no
      
      break by {1}fg-rdtl.loc
            by {1}fg-rdtl.loc-bin
            by {1}fg-rdtl.tag     desc:
            
    release b-fg-bin.        
            
    /* Find Bin for that specific Job# Sequence & if avail
       then use Standard Cost from that Bin. */
    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq {1}fg-rcpts.i-no
          and fg-bin.loc     eq {1}fg-rdtl.loc
          and fg-bin.loc-bin eq {1}fg-rdtl.loc-bin
          and fg-bin.tag     eq {1}fg-rdtl.tag
          and fg-bin.job-no  eq {1}fg-rcpts.job-no
          and fg-bin.job-no2 eq {1}fg-rcpts.job-no2
        use-index co-ino no-error.

    if not avail fg-bin and {1}fg-rdtl.t-qty ne 0 then do:
      if fg-rdtl.tag ne "" then      /* Check for Transfer/Count function */
      find first b-fg-bin
          where b-fg-bin.company eq cocode
            and b-fg-bin.i-no    eq {1}fg-rcpts.i-no
            and b-fg-bin.loc     eq {1}fg-rdtl.loc
            and b-fg-bin.loc-bin eq {1}fg-rdtl.loc-bin
            and b-fg-bin.tag     eq ""
            and b-fg-bin.job-no  eq {1}fg-rcpts.job-no
            and b-fg-bin.job-no2 eq {1}fg-rcpts.job-no2
          use-index co-ino no-error.
            
      create fg-bin.
      assign
       fg-bin.company = {1}fg-rdtl.company
       fg-bin.job-no  = {1}fg-rcpts.job-no
       fg-bin.job-no2 = {1}fg-rcpts.job-no2
       fg-bin.loc     = {1}fg-rdtl.loc
       fg-bin.loc-bin = {1}fg-rdtl.loc-bin
       fg-bin.tag     = {1}fg-rdtl.tag
       fg-bin.i-no    = {1}fg-rcpts.i-no
       fg-bin.qty     = {1}fg-rdtl.t-qty.
       
      if avail b-fg-bin then                      /* Transfer/Count */
        assign
         fg-bin.case-count   = b-fg-bin.case-count
         fg-bin.cases-unit   = b-fg-bin.cases-unit
         fg-bin.unit-count   = b-fg-bin.unit-count
         fg-bin.units-pallet = b-fg-bin.units-pallet
         fg-bin.std-mat-cost = b-fg-bin.std-mat-cost
         fg-bin.std-lab-cost = b-fg-bin.std-lab-cost
         fg-bin.std-fix-cost = b-fg-bin.std-fix-cost
         fg-bin.std-var-cost = b-fg-bin.std-var-cost
         b-fg-bin.qty        = b-fg-bin.qty - {1}fg-rdtl.t-qty.
      
      else do:
        find first job-hdr
            where job-hdr.company      eq cocode
              and job-hdr.i-no         eq fg-rcpts.i-no
              and trim(job-hdr.job-no) eq trim(fg-rcpts.job-no)
              and job-hdr.job-no2      eq fg-rcpts.job-no2
            use-index i-no no-lock no-error.
                   
        if avail job-hdr then
          assign
           fg-bin.std-mat-cost = job-hdr.std-mat-cost
           fg-bin.std-lab-cost = job-hdr.std-lab-cost
           fg-bin.std-fix-cost = job-hdr.std-fix-cost
           fg-bin.std-var-cost = job-hdr.std-var-cost.
      
        else
          assign
           fg-bin.std-mat-cost = itemfg.std-mat-cost
           fg-bin.std-lab-cost = itemfg.std-lab-cost
           fg-bin.std-fix-cost = itemfg.std-fix-cost
           fg-bin.std-var-cost = itemfg.std-var-cost.
      end.
        
      fg-bin.std-tot-cost = fg-bin.std-mat-cost + fg-bin.std-lab-cost +
                            fg-bin.std-fix-cost + fg-bin.std-var-cost. 
    end.

    if avail fg-bin then do:
      assign
       v-q-adj-ytd       = v-q-adj-ytd + ({1}fg-rdtl.t-qty - fg-bin.qty)
       fg-bin.qty        = {1}fg-rdtl.t-qty
       fg-bin.last-count = {1}fg-rdtl.t-qty
       fg-bin.last-date  = {1}fg-rcpts.trans-date.
       
      /* Update bin with any transactions after this cycle count */
      for each fg-rcpth
          where fg-rcpth.company    eq cocode
            and fg-rcpth.i-no       eq fg-bin.i-no
            and fg-rcpth.trans-date ge {1}fg-rcpts.trans-date
            and fg-rcpth.job-no     eq fg-bin.job-no
            and fg-rcpth.job-no2    eq fg-bin.job-no2  
          no-lock use-index tran,

          each fg-rdtlh
          where fg-rdtlh.r-no      eq fg-rcpth.r-no
            and fg-rdtlh.rita-code eq fg-rcpth.rita-code
            and fg-rdtlh.loc       eq fg-bin.loc
            and fg-rdtlh.loc-bin   eq fg-bin.loc-bin
            and fg-rdtlh.tag       eq fg-bin.tag  
          no-lock

          by fg-rcpth.trans-date
          BY fg-rdtlh.trans-time
          by fg-rcpth.r-no:
            
        if fg-rcpth.trans-date eq {1}fg-rcpts.trans-date and
           fg-rcpth.r-no       lt {1}fg-rcpts.r-no       then next.
               
        {fg/fg-mkbin.i}   
      end.
    end.  
        
    create fg-rdtlh.
    {1}fg-rdtl.rita-code = {1}fg-rcpts.rita-code.
    {fg/fg-rdtl.i fg-rdtlh {1}fg-rdtl}
    
    if last-of({1}fg-rdtl.loc-bin) and avail b-fg-bin then do:
                                                    /* Transfer/Count */
      assign
       {1}fg-rdtl.tag      = ""
       {1}fg-rdtl.ext-cost = b-fg-bin.qty *
                             ({1}fg-rdtl.ext-cost / {1}fg-rdtl.t-qty)
       {1}fg-rdtl.t-qty    = b-fg-bin.qty.

      create fg-rdtlh.
      {1}fg-rdtl.rita-code = {1}fg-rcpts.rita-code.
      {fg/fg-rdtl.i fg-rdtlh {1}fg-rdtl}
    end.
      
    delete {1}fg-rdtl.
  end. /* each {1}fg-rdtl */

  create fg-rcpth.
  {fg/fg-rcpts.i fg-rcpth {1}fg-rcpts}   /* Create History Record */

  /* update cost for non-job order lines for item */
  if last-of({1}fg-rcpts.i-no) then do:
    assign
     v-q-adj-ytd = 0
     v-qty-onh   = 0.

    for each fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq itemfg.i-no 
        no-lock:
            
      v-qty-onh = v-qty-onh + fg-bin.qty.
    end. /* each fg-bin */

    assign
     itemfg.q-onh     = v-qty-onh
     itemfg.q-avail   = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
     itemfg.q-adj-ytd = itemfg.q-adj-ytd + v-q-adj-ytd. 

    v-qty-onh   = 0.
    for each fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq itemfg-loc.i-no 
          AND fg-bin.loc     EQ {1}.fg-rcpts.loc
        no-lock:            
      v-qty-onh = v-qty-onh + fg-bin.qty.
    end. /* each fg-bin */
    RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {1}.fg-rcpts.loc).

    FIND FIRST itemfg-loc 
        WHERE itemfg-loc.company EQ itemfg.company
          AND itemfg-loc.i-no    EQ itemfg.i-no
          AND itemfg-loc.loc     EQ {1}.fg-rcpts.loc
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL itemfg-loc THEN
    assign
     itemfg-loc.q-onh     = v-qty-onh
     itemfg-loc.q-avail   = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc
     itemfg-loc.q-adj-ytd = itemfg-loc.q-adj-ytd + v-q-adj-ytd.

    run fg/updfgcs1.p (recid(itemfg), NO).

    find b-itemfg where recid(b-itemfg) eq recid(itemfg).

    for each oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.i-no    eq {1}fg-rcpts.i-no
          and oe-ordl.job-no  eq ""
          and oe-ordl.cost    eq 0
        use-index item,
        
        first oe-ord
        where oe-ord.company           eq cocode
          and oe-ord.ord-no            eq oe-ordl.ord-no
          and index("CDZ",oe-ord.stat) eq 0
        no-lock:
        
      run sys/ref/convcuom.p(b-itemfg.prod-uom, "M", 0, 0, 0, 0,
                             b-itemfg.total-std-cost, output oe-ordl.cost).
    end.
  end.

  delete {1}fg-rcpts.
end. /* each {1}fg-rcpts */

