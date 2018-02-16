/* ------------------------------------------------ce/tan/mach-sq2.p 10/94 gb */
/* create machine routing sequence    part-2                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def buffer xeb2 for eb.
def buffer xop  for est-op.
DEF BUFFER op-lock FOR reftable.

def shared var xcal as de NO-UNDO.
def shared var maxco as int  no-undo.
def shared var v-2 as log init no NO-UNDO.
def new shared var r-spo   as de no-undo.
def new shared var spo     as de no-undo.
def new shared var cumul   as de no-undo.
def new shared var cumul2  as de no-undo.
def new shared var prev-op as ch no-undo.
def new shared var save_id as RECID  NO-UNDO.
def new shared var v-n-out as int init 1 no-undo.
DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR CALL_id AS RECID NO-UNDO.

DEF VAR v-num-up AS INT NO-UNDO.

{ce/mach-lst.i}

{est/d-machex.i NEW}


run ce/mach-chk.p (no).

if not avail xef then find first xef where xef.company = xest.company 
                                       AND xef.est-no eq xest.est-no no-error.
if not avail xeb then find first xeb where xeb.company = xest.company 
                                       AND xeb.est-no eq xest.est-no no-error.

save_id = recid(xef).

j = 1.
for each m-lst by m-lst.seq by m-lst.b-no by m-lst.pass-no:
  find first mach
      {sys/look/machW.i}
        and mach.m-code eq m-lst.m-code
      no-lock no-error.
  create est-op.
  assign
   est-op.e-num      = xest.e-num
   est-op.company = xest.company 
   est-op.est-no     = xest.est-no
   est-op.line       = j
   est-op.dept       = m-lst.dept
   est-op.d-seq      = mach.d-seq
   est-op.s-num      = if m-lst.f-no ne ? then m-lst.f-no else xef.form-no
   est-op.b-num      = m-lst.b-no
   est-op.op-sb      = yes
   est-op.op-pass    = m-lst.pass-no
   est-op.m-code     = mach.m-code
   est-op.m-dscr     = mach.m-dscr
   est-op.n-out      = minimum(m-lst.n-out,mach.num-wid)
   est-op.op-spoil   = mach.run-spoil
   est-op.op-crew[1] = mach.mr-crusiz
   est-op.op-crew[2] = mach.run-crusiz
   est-op.op-rate[1] = mach.mr-trate
   est-op.op-rate[2] = mach.run-trate.
  if mach.p-type eq "B" then est-op.op-sb = no.
  j = j + 1.
end.

qty = xest.est-qty[1].
/* Sort list again ... */
i = 0.
for each est-op where est-op.company = xest.company 
                  AND est-op.est-no eq xest.est-no:
  i = i + 1.
  est-op.line = i.
end.
    

  FIND CURRENT xest EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN
         xest.recalc    = YES
         xest.recalc-mr = YES.
  FIND CURRENT xest NO-LOCK. 


cumul = xest.est-qty[1] / xeb.num-up.
for each est-op where est-op.company = xest.company 
                  AND est-op.est-no eq xest.est-no,
    first mach
    {sys/look/machW.i}
      and mach.m-code eq est-op.m-code
    no-lock
    by est-op.line desc:

  x = if est-op.b-num eq 0 then 1 else est-op.b-num.

  find first xeb2
      where xeb2.company = xest.company 
        AND xeb2.est-no    eq xest.est-no
        and xeb2.blank-no eq x
      no-lock no-error.
  maxco = xeb2.i-col + (xeb2.i-coat * (if mach.coater eq yes then 1 else 0)).

  if est-op.dept eq "RC" then
    assign
     cumul   = cumul / xef.n-out
     v-n-out = xef.n-out.

  if prev-op eq "PR" and est-op.dept ne "PR" then do:
    cumul = 0.
    for each xop where xop.e-num eq xest.e-num and xop.dept  eq "PR":
      cumul = cumul + xop.num-sh.
    end.
  end.

  if prev-op eq "GL" and est-op.dept ne "GL" then do:
    cumul = 0.
    for each xop where xop.e-num eq xest.e-num and xop.dept  eq "GL":
      cumul = cumul + xop.num-sh.
    end.
  end.

  else
  if ( prev-op ne "PR" and est-op.dept eq "PR" )
   or ( prev-op ne "GL" and est-op.dept eq "GL" )
  then
  cumul2 = cumul.

  if est-op.b-num gt 0
    then
  qty = (cumul2 * (xeb2.bl-qty / xest.est-qty[1])) * xeb.num-up.
  else
  qty = cumul * xeb.num-up.
  est-op.num-sh = qty / (xeb.num-up * v-n-out).

/* JLF added 01/26/96 */
  est-op.op-spoil = 0.
/* JLF added 01/26/96 */

  {ce/kspoil.i &fil=est-op &fld=op-spoil}

/* JLF added 01/23/96 */
  est-op.op-spoil = est-op.op-spoil + mach.run-spoil.
/* JLF added 01/23/96 */

  if (est-op.dept eq "PR"
   or est-op.dept eq "GL")
  and not xef.op-lock then
  do:

    FIND FIRST {est/plfoun-w.i reftable xeb2} NO-LOCK NO-ERROR.

    est-op.op-waste = mach.mr-waste +
                      if est-op.b-num eq 1 then
                        (mach.col-wastesh * (xeb2.i-col + xeb2.i-coat))
                      else
                      IF AVAIL reftable THEN
                        (mach.col-wastesh * (reftable.val[01] + reftable.val[02]))
                      ELSE 0.

    if xeb2.blank-no gt 1 THEN do:   
      IF AVAIL reftable AND (reftable.val[01] ne 0 or reftable.val[02] ne 0) then
        est-op.op-mr = (mach.tan-mrp * reftable.val[01]) +
             (if est-op.dept eq "PR" then (mach.tan-mrf * reftable.val[02]) else 0).
    end.
    spo = spo + est-op.op-waste.
  end.

  if est-op.dept eq "PR"
   or est-op.dept eq "GL"
  then
  do:
    xxx = cumul2 * (xeb2.bl-qty / xest.est-qty[1]).
    {sys/inc/roundup.i xxx}
    zzz = xxx / (1 -  (est-op.op-spoil / 100)).
    {sys/inc/roundup.i zzz}
    est-op.num-sh = zzz.
    r-spo = r-spo + (zzz - xxx).
  end.
  else
  do:
    xxx = cumul.
    cumul = cumul / (1 - (est-op.op-spoil / 100)).
    {sys/inc/roundup.i xxx}
    {sys/inc/roundup.i cumul}
    est-op.num-sh = (cumul / v-n-out).
    r-spo = r-spo + (cumul - xxx).
  end.

  if est-op.dept eq "PR" then
    est-op.op-waste = (mach.col-wastesh * maxco) + mach.mr-waste.
  else
    est-op.op-waste = mach.mr-waste.

  if est-op.op-sb eq no then
  spo = spo + (est-op.op-waste / xeb.num-up).
  else
  spo = spo + est-op.op-waste.
  if est-op.op-sb eq no
    then
  est-op.num-sh = est-op.num-sh + (est-op.op-waste / xeb.num-up).
  else
  est-op.num-sh = est-op.num-sh + est-op.op-waste.
  {sys/inc/roundup.i r-spo}
  {sys/inc/roundup.i spo}
  {sys/inc/roundup.i cumul}
  {sys/inc/roundup.i cumul2}
  {sys/inc/roundup.i est-op.num-sh}
  prev-op = est-op.dept.

  /* flip dimensions if corr. xgrain */
  if est-op.dept eq "LM" and
    ((xef.n-out-l ne 0 and
    (xef.lam-dscr eq  "R" or (xef.lam-dscr ne "R" and xef.xgrain eq "S")))
    or
    (xef.n-out-l eq 0 and (xef.lam-dscr ne "R" and xef.xgrain ne "S"))
    )
    then
  do:
    find first xef where xef.e-num eq xest.e-num no-error.
    zzz = xef.gsh-wid.
    xef.gsh-wid = xef.gsh-len.
    xef.gsh-len = zzz.
    zzz = xef.lsh-wid.
    xef.lsh-wid = xef.lsh-len.
    xef.lsh-len = zzz.
    zzz = xef.nsh-wid.
    xef.nsh-wid = xef.nsh-len.
    xef.nsh-len = zzz.
    zzz = xef.trim-w.
    xef.trim-w = xef.trim-l.
    xef.trim-l = zzz.
  end.
  find xef where recid(xef) eq save_id no-error.
  /* use ce directory ... */
  {ce/kmr-run.i &fil=est-op &fld=op-mr &fil2=est-op &fld2=op-speed}
  /* reset dimensions if corr.xgrain */
  if est-op.dept eq "LM" and
    ((xef.n-out-l ne 0 and
    (xef.lam-dscr eq  "R" or (xef.lam-dscr ne "R" and xef.xgrain eq "S")))
    or
    (xef.n-out-l eq 0 and (xef.lam-dscr ne "R" and xef.xgrain ne "S"))
    )
    then
  do:
    zzz = xef.gsh-wid.
    xef.gsh-wid = xef.gsh-len.
    xef.gsh-len = zzz.
    zzz = xef.lsh-wid.
    xef.lsh-wid = xef.lsh-len.
    xef.lsh-len = zzz.
    zzz = xef.nsh-wid.
    xef.nsh-wid = xef.nsh-len.
    xef.nsh-len = zzz.
    zzz = xef.trim-w.
    xef.trim-w = xef.trim-l.
    xef.trim-l = zzz.
  end.
  if ( est-op.dept eq "PR" or est-op.dept eq "GL" )
  and xeb2.blank-no gt 1 then
  do: /* yield-qty eq plate qty, cust-% eq fntn qty */
    if reftable.val[01] ne 0 or reftable.val[02] ne 0 then
    est-op.op-mr = (mach.tan-mrp * reftable.val[01]) +
             (if est-op.dept eq "PR" then (mach.tan-mrf * reftable.val[02]) else 0).
  end.
  else
  do:
    if mach.washup gt 0 and mach.col-pass eq "P" then
    est-op.op-mr = est-op.op-mr + mach.washup.
    else
    if mach.washup gt 0 and mach.col-pass eq "C" then
    est-op.op-mr = est-op.op-mr + (mach.washup * maxco).
  end.
end.
find first est-op where  est-op.company = xest.company 
                  AND est-op.est-no eq xest.est-no and
  est-op.line lt 500 no-lock no-error.
if est-op.dept ne "PR"
and est-op.dept ne "GL"
then
cumul = est-op.num-sh.
else if est-op.dept eq "PR" then
do:
  cumul = 0.
  for each xop where  est-op.company = xest.company 
                  AND est-op.est-no eq xest.est-no and
      xop.line  lt 500         and
      xop.dept  eq "PR":
    cumul = cumul + xop.num-sh.
  end.
end.

else if est-op.dept eq "GL" then
do:
  cumul = 0.
  for each xop where  est-op.company = xest.company 
                  AND est-op.est-no eq xest.est-no and
      xop.line  lt 500         and
      xop.dept  eq "GL":
    cumul = cumul + xop.num-sh.
  end.
end.

qty = xest.est-qty[1].
find first xeb2 where  est-op.company = xest.company 
                  AND est-op.est-no eq xest.est-no no-lock no-error.
save_id = recid(xef).
xef.gsh-qty = cumul.
find xef where recid(xef) eq save_id no-lock no-error.
DELETE op-lock.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
