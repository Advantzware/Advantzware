/* --------------------------------------------------- ap/ap-aged.i 08/98 JLF */
/* Vendor Aging Report Program - A/P Module                                   */
/* -------------------------------------------------------------------------- */

for each ap-inv
    where ap-inv.company   eq cocode
      and ap-inv.vend-no   eq vend.vend-no
      and ap-inv.posted    eq yes
      and (ap-inv.inv-date le as_of_date or not v-idate)
    use-index ap-inv no-lock,
    
    first ap-ledger
    where ap-ledger.company  eq cocode
      and ap-ledger.vend-no  eq ap-inv.vend-no
      and ap-ledger.ref-date eq ap-inv.inv-date
      and ap-ledger.refnum   eq ("INV# " + ap-inv.inv-no)
      and (ap-ledger.tr-date le as_of_date or v-idate)
    use-index ap-ledger no-lock
    
    break BY ap-inv.curr-code[1]
          by ap-inv.vend-no
          by (if v-idate then ap-inv.inv-date else ap-ledger.tr-date)
          by ap-inv.inv-no:
  
  if FIRST(ap-inv.vend-no) then do:
    assign
     first-time = yes
     cust-t[1]  = 0
     cust-t[2]  = 0
     cust-t[3]  = 0
     cust-t[4]  = 0
     cust-t[5]  = 0
     
     m3 = vend.area-code + vend.phone
     m2 = vend.cont
     ni = 0.
  end.

  assign
   v-amt  = 0
   v-date = if v-idate then ap-inv.inv-date else ap-ledger.tr-date.
   
  for each ap-payl
      where ap-payl.inv-no   eq ap-inv.inv-no
        and ap-payl.vend-no  eq ap-inv.vend-no
        and ap-payl.posted   eq yes
        and ap-payl.due-date eq ap-inv.due-date
      use-index inv-no no-lock:

    release xap-ledger.
    find first ap-pay
        where ap-pay.company eq cocode
          and ap-pay.c-no eq ap-payl.c-no
        use-index c-no no-lock no-error.
    if avail ap-pay then
    /* use ap-ledger for posting date not cr/db or check date */
    find first xap-ledger
        where xap-ledger.company  eq cocode
          and xap-ledger.vend-no  eq ap-payl.vend-no
          and xap-ledger.ref-date eq ap-pay.check-date
          and xap-ledger.refnum   eq ("MEMO#" + ap-payl.inv-no)
        use-index ap-ledger no-lock no-error.
    if not avail xap-ledger and ap-payl.line lt 50 then
    find first xap-ledger
        where xap-ledger.company  eq cocode
          and xap-ledger.vend-no  eq ap-payl.vend-no
          and xap-ledger.ref-date eq ap-pay.check-date
          and xap-ledger.refnum   eq ("AC" + string(ap-payl.check-no,"999999"))
        use-index ap-ledger no-lock no-error.
    if not avail xap-ledger and ap-payl.line gt 50 then
    find first xap-ledger
        where xap-ledger.company eq cocode
          and xap-ledger.vend-no eq ap-pay.vend-no
          and xap-ledger.refnum  eq
                          ("VOIDED CHECK" + string(ap-pay.check-no,"zzzzzzz9"))
        use-index ap-ledger no-lock no-error.

    if avail ap-pay and ap-pay.check-date le as_of_date then do:
      if ap-payl.amt-paid ne 0 then v-amt = v-amt - ap-payl.amt-paid.
      if ap-payl.amt-disc ne 0 then do:
        if not ap-payl.memo then v-amt = v-amt - ap-payl.amt-disc.
        if ap-payl.memo then v-amt = v-amt + ap-payl.amt-disc.
      end.
    end.

    release ap-pay.
    release xap-ledger.
  end. /* for each ap-payl */

  assign
   d     = as_of_date - v-date
   v-amt = v-amt + ap-inv.net + ap-inv.freight
   c1    = c1 + v-amt
   ni    = ni + 1.

  /* if days old less then 0 make equal to 0 */
  if d lt 0 then assign d = 0.
  
  if v-amt ne 0 then do:
    if first-time then do:
      display vend.vend-no vend.name
          with no-labels no-box no-attr-space frame vendor1{1}1.
      display m3 at 10 with frame vendor2{1} no-labels no-box stream-io width 132.
      first-time = no.
    end.

    form header vend.vend-no vend.name
    
        with frame v-top-{1} page-top no-box no-labels no-attr-space stream-io.
    view frame v-top-{1}.
    
    assign
     ag        = 0
     i         = trunc(d / 30,0) + int(d modulo 30 gt 0)
     i         = if i gt 5 then 5 else if i lt 1 then 1 else i
     ag[i]     = v-amt
     cust-t[i] = cust-t[i] + ag[i].
    
    if v-dtl then
      display ap-inv.inv-no     at 3        format "x(12)"
              space(2)
              v-date
              space(3)
              v-amt
              d                             format "-999"
              ag[1]             to 67       when ag[1] ne 0
              ag[2]             to 83       when ag[2] ne 0
              ag[3]             to 99       when ag[3] ne 0
              ag[4]             to 115      when ag[4] ne 0 
              ag[5]             to 131      when ag[5] ne 0
              
          with frame detail{1} no-labels no-box stream-io width 132.
  end.  /* if v-amt ne 0  */

  if LAST(ap-inv.vend-no) and c1 ne 0 then do:
    if ni gt 1 then m3 = "".
    if ni eq 1 then m3 = m2.
    
    display space (10) "VENDOR TOTALS" c1 to 44
            cust-t[1] to 67
            cust-t[2] to 83
            cust-t[3] to 99
            cust-t[4] to 115
            cust-t[5] to 131
            skip(1)
            
        with frame vendor3{1} no-labels no-box no-attr-space stream-io width 132.
        
    do i = 1 to 5:
      grand-t[i] = grand-t[i] + cust-t[i].
      cust-t[i] = 0.
    end.
    c1 = 0.
  end.  /* last-of loop */

  /*m3 = "".*/
  t1 = t1 + v-amt.
  /*if ni eq 1 then m3 = m2.*/
end.  /* for each ap-inv */

/* end ---------------------------------- copr. 1992  advanced software, inc. */
