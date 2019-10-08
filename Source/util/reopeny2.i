
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
 vend.{2} = 0
 vend.{3} = 0. 

IF "{1}" EQ "0" THEN
DO li = 1 TO 12:
  vend.purch[li] = 0.
END.

IF start-date NE ? AND end-date NE ? THEN
for each ap-ledger
    where ap-ledger.company eq cocode
      and ap-ledger.vend-no eq vend.vend-no
      and ap-ledger.tr-date ge start-date
      and ap-ledger.tr-date le end-date
      and ap-ledger.refnum  begins "INV#"
    no-lock,

    FIRST b-period
    WHERE b-period.company EQ ap-ledger.company
      AND b-period.pst     LE ap-ledger.tr-date
      AND b-period.pend    GE ap-ledger.tr-date
    NO-LOCK,

    first ap-inv
    where ap-inv.company eq cocode
      and ap-inv.vend-no eq vend.vend-no
      and ap-inv.posted  eq yes
      and ap-inv.inv-no  eq substr(ap-ledger.refnum,6,
                                   length(trim(ap-ledger.refnum)))
    use-index ap-inv no-lock:

  for each ap-invl
      where ap-invl.company eq cocode
        and ap-invl.inv-no  eq ap-inv.inv-no
        and ap-invl.i-no    eq ap-inv.i-no
      use-index i-no no-lock:
    
    if ap-invl.amt-msf ne 0 then
      vend.{2} = vend.{2} + ap-invl.amt-msf.
    
    else do:
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no eq string(ap-invl.i-no)
          use-index i-no no-lock no-error.
      if avail itemfg then
        vend.{2} = vend.{2} + (ap-invl.qty * itemfg.t-sqft / 1000).
    end.
  end.

  IF "{1}" EQ "0" THEN
    vend.purch[b-period.pnum] = vend.purch[b-period.pnum] + ap-ledger.amt.

  vend.{3} = vend.{3} + ap-ledger.amt.
end. /* for each ap-ledger INV */

IF start-date NE ? AND end-date NE ? THEN
for each ap-ledger
    where ap-ledger.company eq cocode
      and ap-ledger.vend-no eq vend.vend-no
      and ap-ledger.tr-date ge start-date
      and ap-ledger.tr-date le end-date
      and (ap-ledger.refnum begins "Memo#" /*or
           ap-ledger.refnum begins "Chk#" */)
    no-lock,

    FIRST b-period
    WHERE b-period.company EQ ap-ledger.company
      AND b-period.pst     LE ap-ledger.tr-date
      AND b-period.pend    GE ap-ledger.tr-date
    NO-LOCK:

  IF "{1}" EQ "0" THEN
    vend.purch[b-period.pnum] = vend.purch[b-period.pnum] - ap-ledger.amt.

  vend.{3} = vend.{3} - ap-ledger.amt.
end. /* for each ap-ledger MEMO */
