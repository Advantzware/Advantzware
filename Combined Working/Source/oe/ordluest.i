/* -------------------------------------------------- oe/ordluest.i 04/00 JLF */
/* Order Entry - update for adding from estimating                            */
/* -------------------------------------------------------------------------- */

    find first oe-ctrl where oe-ctrl.company eq cocode no-wait no-error.
    if avail oe-ctrl then v-fr-tax = oe-ctrl.f-tax.
    run ar/cctaxrt.p (input cocode, xoe-ord.tax-gr,
                      output v-tax-rate, output v-frt-tax-rate).
                      
    find first cust
        where cust.company eq cocode
          and cust.cust-no eq xoe-ord.cust-no
        no-error.
        
    if avail cust then
      if v-fr-tax then
        cust.ord-bal = (cust.ord-bal - {1}.t-freight -
                        round(({1}.t-freight * v-frt-tax-rate) / 100,2)).
      else
        cust.ord-bal = cust.ord-bal - {1}.t-freight.
        
    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq eb.stock-no
        no-lock no-error.
      
    if avail itemfg then {1}.part-dscr2 = itemfg.part-dscr2.
        
    assign
     {1}.i-no       = eb.stock-no
     {1}.i-name     = eb.part-dscr1
     {1}.part-no    = eb.part-no
     {1}.part-dscr1 = eb.part-dscr2
     {1}.qty        = v-qty
     {1}.est-type   = xest.est-type
     {1}.form-no    = eb.form-no
     {1}.blank-no   = eb.blank-no.

    RUN oe/oe-cnt.p(RECID({1}), OUTPUT {1}.cas-cnt, OUTPUT {1}.cases-unit).
     
    find first blk where blk.id eq eb.part-no no-lock no-error.
    if avail blk then do:
      assign
       xoe-ord.t-weight  = xoe-ord.t-weight  - {1}.t-weight
       xoe-ord.t-freight = xoe-ord.t-freight - {1}.t-freight
       {1}.t-weight      = blk.fg-wt
       {1}.t-freight     = blk.fg-wt$ * (blk.fg-wt / 100)
       {1}.cost          = if v-full-cost then blk.cost else blk.fact 
       {1}.cost          = {1}.cost - (((blk.fg-wt / 100) * blk.fg-wt$) *
                                        (blk.qyld / xest.est-qty[1])).
                                        
      if xest.est-type eq 4 or xest.est-type eq 8 then
        {1}.cost = (if v-full-cost then blk.cost else blk.fact) / 
                   (blk.qyld / 1000).

      assign
       xoe-ord.t-weight  = xoe-ord.t-weight  + {1}.t-weight
       xoe-ord.t-freight = xoe-ord.t-freight + {1}.t-freight.
       
      if {1}.cas-cnt eq 0 and eb.cas-wt ne 0 then
        {1}.cas-cnt = trunc({1}.t-weight / eb.cas-wt,0) +
                      int({1}.t-weight modulo eb.cas-wt gt 0).
    end.   

    if avail xest and v-quo-price and not avail w-oe-ordl then
      run oe/look/getqpric.p (recid(xest), {1}.part-no, "", {1}.qty,
                              input-output {1}.price,
                              input-output {1}.pr-uom,
                              OUTPUT lv-q-no).  
    
    if avail cust then
      if v-fr-tax then
        cust.ord-bal = (cust.ord-bal + {1}.t-freight +
                        round(({1}.t-freight * v-frt-tax-rate) / 100,2)).
      else
        cust.ord-bal = cust.ord-bal + {1}.t-freight.

