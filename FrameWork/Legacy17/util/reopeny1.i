
find first b-period
    where b-period.company eq cocode
      and b-period.yr      eq period.yr - {1}
    no-lock no-error.
start-date = IF AVAIL b-period THEN b-period.pst ELSE ?.
  
IF "{1}" EQ "1" THEN
  find last b-period
      where b-period.company eq cocode
        and b-period.yr      eq period.yr - 1
      no-lock no-error.
ELSE
  find last b-period
      where b-period.company eq cocode
        and b-period.pstat   eq yes
      no-lock no-error.
end-date = IF AVAIL b-period THEN b-period.pend ELSE ?.

assign
 cust.{2}-msf   = 0
 cust.{3}-sales = 0
 cust.cost[{4}] = 0
 cust.comm[{4}] = 0.
  
IF start-date NE ? AND end-date NE ? THEN
for each ar-ledger
    where ar-ledger.company eq cocode
      and ar-ledger.cust-no eq cust.cust-no
      and ar-ledger.tr-date ge start-date
      and ar-ledger.tr-date le end-date
      and ar-ledger.ref-num begins "INV#"
    no-lock,

    first ar-inv
    where ar-inv.company eq cocode
      and ar-inv.posted  eq yes
      and ar-inv.cust-no eq cust.cust-no
      and ar-inv.inv-no  eq int(substr(ar-ledger.ref-num,6,
                                       length(ar-ledger.ref-num)))
    use-index posted no-lock:

  for each ar-invl
      where ar-invl.company eq cocode
        and ar-invl.cust-no eq ar-inv.cust-no
        and ar-invl.inv-no  eq ar-inv.inv-no
      use-index inv-no no-lock:
        
    if ar-invl.amt-msf ne 0 then
      cust.{2}-msf = cust.{2}-msf + ar-invl.amt-msf.
         
    else do:
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq ar-invl.i-no
          use-index i-no no-lock no-error.
            
      if avail itemfg then
        cust.{2}-msf = cust.{2}-msf + (ar-invl.inv-qty * itemfg.t-sqft / 1000).
    end.
  end.
  
  assign
   cust.{3}-sales = cust.{3}-sales +
                    if ((if ar-inv.gross gt ar-inv.net then ar-inv.gross else
                         ar-inv.net) - ar-inv.tax-amt) eq ? then 0 
                    else
                    ((if ar-inv.gross gt ar-inv.net then ar-inv.gross else
                      ar-inv.net) - ar-inv.tax-amt)
   cust.cost[{4}] = cust.cost[{4}] + ar-inv.t-cost
   cust.comm[{4}] = cust.comm[{4}] + ar-inv.t-comm.
end. /* for each ar-ledger INV */

IF start-date NE ? AND end-date NE ? THEN
for each ar-ledger
    where ar-ledger.company eq cocode
      and ar-ledger.cust-no eq cust.cust-no
      and ar-ledger.tr-date ge start-date
      and ar-ledger.tr-date le end-date
      and ar-ledger.ref-num begins "Memo#"
    no-lock,

    first ar-cash
    where ar-cash.company  eq cocode
      and ar-cash.cust-no  eq cust.cust-no
      and ar-cash.posted   eq yes
      and ar-cash.check-no eq int(substr(ar-ledger.ref-num,6,8))
    use-index posted no-lock,

    each ar-cashl
    where ar-cashl.company eq cocode
      and ar-cashl.c-no    eq ar-cash.c-no
    use-index c-no no-lock:
                              
  cust.{3}-sales = cust.{3}-sales + (ar-cashl.amt-paid - ar-cashl.amt-disc).
end. /* for each ar-ledger MEMO */

IF "{1}" EQ "0" THEN RUN ar/ptdsales.p (ROWID(cust)).
