    
for each {1}fg-rctd
    where {1}fg-rctd.company   eq cocode
      and {1}fg-rctd.rita-code eq "C"
      AND ("{1}" EQ "w-" OR
           (fg-rctd.created-by GE begin_userid  AND
            fg-rctd.created-by LE end_userid))
   break by {1}fg-rctd.i-no 
          by {1}fg-rctd.loc
          by {1}fg-rctd.loc-bin
          by {1}fg-rctd.tag     desc:
                      
    FIND FIRST itemfg WHERE
         itemfg.company eq cocode AND
         itemfg.i-no    eq {1}fg-rctd.i-no
         NO-LOCK NO-ERROR.
        
    IF NOT AVAIL itemfg THEN NEXT.


    release b2-fg-bin.        
            
    /* Find Bin for that specific Job# Sequence & if avail
       then use Standard Cost from that Bin. */
    find first b-fg-bin
        where b-fg-bin.company   eq {1}fg-rctd.company
          and b-fg-bin.i-no      eq {1}fg-rctd.i-no
          and b-fg-bin.loc       eq {1}fg-rctd.loc
          and b-fg-bin.loc-bin   eq {1}fg-rctd.loc-bin
          and b-fg-bin.tag       eq {1}fg-rctd.tag
          and b-fg-bin.job-no    eq {1}fg-rctd.job-no
          and b-fg-bin.job-no2   eq {1}fg-rctd.job-no2
          and b-fg-bin.cust-no   eq {1}fg-rctd.cust-no
        use-index co-ino no-error.

    if not avail b-fg-bin and {1}fg-rctd.t-qty ne 0 then do:
      if {1}fg-rctd.tag ne "" then      /* Check for Transfer/Count function */
      find first b2-fg-bin
          where b2-fg-bin.company         eq {1}fg-rctd.company
            and b2-fg-bin.i-no            eq {1}fg-rctd.i-no
            and b2-fg-bin.loc             eq {1}fg-rctd.loc
            and b2-fg-bin.loc-bin         eq {1}fg-rctd.loc-bin
            and b2-fg-bin.tag             eq ""
            and b2-fg-bin.job-no          eq {1}fg-rctd.job-no
            and b2-fg-bin.job-no2         eq {1}fg-rctd.job-no2
            and b2-fg-bin.cust-no         eq {1}fg-rctd.cust-no
          use-index co-ino no-error.
            
      create b-fg-bin.
      assign
       b-fg-bin.company = {1}fg-rctd.company
       b-fg-bin.job-no  = {1}fg-rctd.job-no
       b-fg-bin.job-no2 = {1}fg-rctd.job-no2
       b-fg-bin.loc     = {1}fg-rctd.loc
       b-fg-bin.loc-bin = {1}fg-rctd.loc-bin
       b-fg-bin.tag     = {1}fg-rctd.tag
       b-fg-bin.cust-no = {1}fg-rctd.cust-no
       b-fg-bin.i-no    = {1}fg-rctd.i-no
       b-fg-bin.qty     = {1}fg-rctd.t-qty.

      if avail b2-fg-bin then                      /* Transfer/Count */
        assign
         b-fg-bin.case-count   = b2-fg-bin.case-count
         b-fg-bin.cases-unit   = b2-fg-bin.cases-unit
         b-fg-bin.unit-count   = b2-fg-bin.unit-count
         b-fg-bin.units-pallet = b2-fg-bin.units-pallet
         b-fg-bin.std-mat-cost = b2-fg-bin.std-mat-cost
         b-fg-bin.std-lab-cost = b2-fg-bin.std-lab-cost
         b-fg-bin.std-fix-cost = b2-fg-bin.std-fix-cost
         b-fg-bin.std-var-cost = b2-fg-bin.std-var-cost
         b2-fg-bin.qty        = b2-fg-bin.qty - {1}fg-rctd.t-qty.
      
      else do:
        find first job-hdr
            where job-hdr.company      eq cocode
              and job-hdr.i-no         eq {1}fg-rctd.i-no
              and trim(job-hdr.job-no) eq trim({1}fg-rctd.job-no)
              and job-hdr.job-no2      eq {1}fg-rctd.job-no2
            use-index i-no no-lock no-error.
                   
        if avail job-hdr then
          assign
           b-fg-bin.std-mat-cost = job-hdr.std-mat-cost
           b-fg-bin.std-lab-cost = job-hdr.std-lab-cost
           b-fg-bin.std-fix-cost = job-hdr.std-fix-cost
           b-fg-bin.std-var-cost = job-hdr.std-var-cost.
      
        else
          assign
           b-fg-bin.std-mat-cost = itemfg.std-mat-cost
           b-fg-bin.std-lab-cost = itemfg.std-lab-cost
           b-fg-bin.std-fix-cost = itemfg.std-fix-cost
           b-fg-bin.std-var-cost = itemfg.std-var-cost.
      end.
        
      b-fg-bin.std-tot-cost = b-fg-bin.std-mat-cost + b-fg-bin.std-lab-cost +
                            b-fg-bin.std-fix-cost + b-fg-bin.std-var-cost. 
    end.

    if avail b-fg-bin then do:
      assign
       v-q-adj-ytd            = v-q-adj-ytd + ({1}fg-rctd.t-qty - b-fg-bin.qty)
       b-fg-bin.cases         = {1}fg-rctd.cases
       b-fg-bin.case-count    = {1}fg-rctd.qty-case
       b-fg-bin.units-pallet  = {1}fg-rctd.units-pallet
       b-fg-bin.cases-unit    = {1}fg-rctd.cases-unit
       b-fg-bin.partial-count = {1}fg-rctd.partial
       b-fg-bin.qty           = {1}fg-rctd.t-qty
       b-fg-bin.last-count    = {1}fg-rctd.t-qty
       b-fg-bin.last-date     = {1}fg-rctd.rct-date.
       
      /* Update bin with any transactions after this cycle count */
      for each fg-rcpth no-lock 
          where fg-rcpth.company    eq cocode
            and fg-rcpth.i-no       eq b-fg-bin.i-no
            and fg-rcpth.trans-date ge {1}fg-rctd.rct-date
            and fg-rcpth.job-no     eq b-fg-bin.job-no
            and fg-rcpth.job-no2    eq b-fg-bin.job-no2  
          use-index tran,
          each fg-rdtlh NO-LOCK
          where fg-rdtlh.r-no      eq fg-rcpth.r-no
            and fg-rdtlh.rita-code eq fg-rcpth.rita-code
            and fg-rdtlh.loc       eq b-fg-bin.loc
            and fg-rdtlh.loc-bin   eq b-fg-bin.loc-bin
            and fg-rdtlh.tag       eq b-fg-bin.tag
            and fg-rdtlh.cust-no   eq b-fg-bin.cust-no
          by fg-rcpth.trans-date
          BY fg-rdtlh.trans-time
          by fg-rcpth.r-no:
            
        if fg-rcpth.trans-date eq {1}fg-rctd.rct-date and
           fg-rcpth.r-no       lt {1}fg-rctd.r-no       then next.
               
        {fg/fg-mkbin.i b- b-}   
      end.
    end.  
        
    create fg-rdtlh.
    {fg/fg-rdtl.i fg-rdtlh {1}fg-rctd}
    
    if last-of({1}fg-rctd.loc-bin) and avail b2-fg-bin then do:
                                                    /* Transfer/Count */
      assign
       {1}fg-rctd.tag      = ""
       {1}fg-rctd.ext-cost = b2-fg-bin.qty *
                             ({1}fg-rctd.ext-cost / {1}fg-rctd.t-qty)
       {1}fg-rctd.t-qty    = b2-fg-bin.qty.

      create fg-rdtlh.      
      {fg/fg-rdtl.i fg-rdtlh {1}fg-rctd}
    end.

  create fg-rcpth.
  {fg/fg-rcpts.i fg-rcpth {1}fg-rctd}   /* Create History Record */

  /* update cost for non-job order lines for item */
  if last-of({1}fg-rctd.i-no) then do:

    assign
     v-q-adj-ytd = 0
     v-qty-onh   = 0.

    for each b-fg-bin FIELDS(qty)
        where b-fg-bin.company eq cocode
          and b-fg-bin.i-no    eq itemfg.i-no 
          AND b-fg-bin.loc     EQ {1}fg-rctd.loc
        no-lock:
            
      v-qty-onh = v-qty-onh + b-fg-bin.qty.
    end. /* each b-fg-bin */
    run fg/chkfgloc.p (input itemfg.i-no, input {1}fg-rctd.loc).
    REPEAT:

       FIND itemfg-loc 
           WHERE itemfg-loc.company EQ itemfg.company
             AND itemfg-loc.i-no EQ itemfg.i-no
             AND itemfg-loc.loc  EQ {1}fg-rctd.loc
           EXCLUSIVE-LOCK NO-ERROR.

       IF AVAIL itemfg-loc THEN
       DO:
          assign
           itemfg-loc.q-onh     = v-qty-onh
           itemfg-loc.q-avail   = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc
           itemfg-loc.q-adj-ytd = itemfg-loc.q-adj-ytd + v-q-adj-ytd.
         
          FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
          LEAVE.
       END.
    END.

    assign
     v-q-adj-ytd = 0
     v-qty-onh   = 0.

    for each b-fg-bin FIELDS(qty)
        where b-fg-bin.company eq cocode
          and b-fg-bin.i-no    eq itemfg.i-no 
        no-lock:
            
      v-qty-onh = v-qty-onh + b-fg-bin.qty.
    end. /* each b-fg-bin */

    REPEAT:

       FIND CURRENT itemfg EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      
       IF AVAIL itemfg THEN
       DO:
          assign
           itemfg.q-onh     = v-qty-onh
           itemfg.q-avail   = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
           itemfg.q-adj-ytd = itemfg.q-adj-ytd + v-q-adj-ytd.
         
          FIND CURRENT itemfg NO-LOCK NO-ERROR.
          LEAVE.
       END.
    END.

    run fg/updfgcs1.p (recid(itemfg), NO).

    find b-itemfg where recid(b-itemfg) eq recid(itemfg) NO-LOCK.

    for each oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.i-no    eq {1}fg-rctd.i-no
          and oe-ordl.opened  eq yes
          and oe-ordl.job-no  eq ""
          and oe-ordl.cost    eq 0
        use-index item:
        
      run sys/ref/convcuom.p(b-itemfg.prod-uom, "M", 0, 0, 0, 0,
                             b-itemfg.total-std-cost, output oe-ordl.cost).
    end.
  end. /* last-of i-no*/

  IF "{1}" EQ "" AND {1}fg-rctd.t-qty NE 0 THEN DO:
    FIND FIRST loadtag
        WHERE loadtag.company   EQ {1}fg-rctd.company
          AND loadtag.item-type EQ NO
          AND loadtag.tag-no    EQ {1}fg-rctd.tag
        NO-ERROR.

    IF AVAIL loadtag THEN 
      ASSIGN
       loadtag.loc     = {1}fg-rctd.loc
       loadtag.loc-bin = {1}fg-rctd.loc-bin.
  END.

  delete {1}fg-rctd.
end. /* each {1}fg-rctd */

