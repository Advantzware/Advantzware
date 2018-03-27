/* --------------------------------------------------- ce/mach-sq2.p 10/94 gb */
/* create machine routing sequence    part-2                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{ce/print4.i "new shared" "new shared"}

DEF SHARED VAR qty AS INT NO-UNDO.
def shared var maxco as int no-undo.
def var cumul as dec no-undo.
def new shared var v-n-out as int init 1 no-undo.
def var v-spo as dec.
def var v-num-up like xeb.num-up.
def var v-est-sh like est-op.num-sh.
def var v-high-sh like est-op.num-sh.
DEF VAR ip-rowid AS ROWID NO-UNDO.

DEF NEW SHARED VAR CALL_id AS RECID NO-UNDO.
DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.


{ce/mach-lst.i}

{ce/mach-ink.i}

{est/d-machex.i NEW}


run ce/mach-chk.p (no).

j = 1.
for each m-lst by m-lst.f-no by m-lst.seq by m-lst.b-no by m-lst.pass-no:
   find first mach {sys/look/machW.i} and
              mach.m-code  = m-lst.m-code no-lock no-error.
   create est-op.
   assign
      est-op.company    = xest.company
      est-op.e-num      = xest.e-num
      est-op.est-no     = xest.est-no
      est-op.line       = j
      est-op.dept       = m-lst.dept
      est-op.d-seq      = mach.d-seq
      est-op.s-num      = m-lst.f-no
      est-op.b-num      = m-lst.b-no
      est-op.op-pass    = m-lst.pass-no
      est-op.m-code     = mach.m-code
      est-op.n-out      = minimum(m-lst.n-out,mach.num-wid)
      est-op.op-spoil   = mach.run-spoil
      est-op.op-crew[1] = mach.mr-crusiz
      est-op.op-crew[2] = mach.run-crusiz
      est-op.op-rate[1] = mach.mr-trate
      est-op.op-rate[2] = mach.run-trate.

   if mach.p-type ne "B" then est-op.op-sb = true.
   else est-op.op-sb = no.

   if m-lst.dscr ne "" then est-op.m-dscr = m-lst.dscr.
   else est-op.m-dscr = mach.m-dscr.
end.

i = 0.
for each est-op
    where est-op.company eq xest.company
      and est-op.est-no  eq xest.est-no:
   ASSIGN
   i = i + 1
   est-op.line = i.
end.

assign
 v-num-up = xeb.num-up / (if xest.form-qty eq 1 then 2 else 1)
 save-qty = qty
 qty      = xest.est-qty[1]
 maxco    = 0.

for each ef
    where ef.company eq xest.company
      and ef.est-no  eq xest.est-no
    no-lock
    BREAK BY ef.form-no:

  find xef where recid(xef) eq recid(ef).

  assign
   xef.op-lock        = no
   v-est-sh           = qty / v-num-up /
                        (if xef.n-out   eq 0 then 1 else xef.n-out) /
                        (if xef.n-out-l eq 0 then 1 else xef.n-out-l)
   r-spo[xef.form-no] = 0.

  find xef where recid(xef) eq recid(ef) no-lock.

    {est/op-lock.i xest}
  ASSIGN
    op-lock.val[1] = 1
    op-lock.val[2] = 1.

  RELEASE op-lock.
  run ce/com/localk.p (0, ?).
  
end. /* for each ef */

for each est-op
    where est-op.company eq xest.company
      and est-op.est-no  eq xest.est-no
    break by est-op.s-num:
  if first-of(est-op.s-num) then do:
     find first ef
         where ef.company eq est-op.company
           and ef.est-no  eq est-op.est-no
           and ef.form-no eq est-op.s-num
         no-error.
     if avail ef then assign ef.op-lock = true
                                 ef.gsh-qty = est-op.num-sh.
  end.
end.
find xef where recid(xef) = call_id no-lock no-error.
qty = xest.est-qty[1].

/* end ---------------------------------- copr. 1993  advanced software, inc. */
