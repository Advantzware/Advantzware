/* --------------------------------------------------- ce/mach-sq2.p 10/94 gb */
/* create machine routing sequence    part-2                                  */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-form-no LIKE ef.form-no NO-UNDO.

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
DEF BUFFER bf-ef FOR ef.
DEF BUFFER bf-eb FOR eb.
DEF NEW SHARED VAR CALL_id AS RECID NO-UNDO.
DEF NEW SHARED VAR qty AS INT NO-UNDO.

{ce/print4.i "new shared" "new shared"}

def shared var maxco as int no-undo.

{ce/mach-lst.i}

{est/d-machex.i NEW}


run ce/mach-chk.p (no).

for each est-op
    where est-op.company eq xest.company
      and est-op.est-no  eq xest.est-no
      and (est-op.s-num  eq ip-form-no or ip-form-no eq 0 or est-op.line ge 500)
      and est-op.qty     eq qty
      and (est-op.auto or est-op.line ge 500):
   delete est-op.
end.

for each xef where xef.company = xest.company
               AND xef.est-no = xest.est-no
               AND (xef.form-no = ip-form-no OR ip-form-no = 0):
   if t-blksht[xef.form-no] = 0
   then for each bf-eb where bf-eb.company = xef.company AND
                             bf-eb.est-no   = xef.est-no  and
                             bf-eb.form-no = xef.form-no :
      t-blksht[xef.form-no] = t-blksht[xef.form-no] + bf-eb.num-up.
   end.
end.

find first xef where xef.company = xest.company
               AND xef.est-no = xest.est-no
               AND (xef.form-no = ip-form-no OR ip-form-no = 0)
     no-lock no-error.
j = 1.
for each m-lst by m-lst.f-no by m-lst.b-no by m-lst.seq by m-lst.pass-no:
   find first mach {sys/look/machW.i} and
              mach.m-code  = m-lst.m-code no-lock no-error.

   create est-op.
   assign
      est-op.e-num      = xest.e-num
      est-op.company = xest.company
      est-op.est-no     = xest.est-no
      est-op.line       = j
      est-op.dept       = m-lst.dept
      est-op.d-seq      = mach.d-seq
      est-op.s-num      = m-lst.f-no
      est-op.b-num      = m-lst.b-no
      est-op.op-pass    = m-lst.pass-no
      est-op.m-code     = mach.m-code
      est-op.m-dscr     = mach.m-dscr
      est-op.n-out      = minimum(m-lst.n-out,mach.num-wid)
      est-op.op-spoil   = mach.run-spoil
      est-op.op-crew[1] = mach.mr-crusiz
      est-op.op-crew[2] = mach.run-crusiz
      est-op.op-rate[1] = mach.mr-trate
      est-op.op-rate[2] = mach.run-trate.

   if mach.p-type ne "B" then est-op.op-sb = true.
   else est-op.op-sb = no.
end.

i = 0.
for each est-op where est-op.company = xest.company
                  AND est-op.est-no = xest.est-no :
   i = i + 1.
   est-op.line = i.
end.

call_id = recid(xef).

for each xef where xef.company = xest.company
               AND xef.est-no = xest.est-no
               AND (xef.form-no = ip-form-no OR ip-form-no = 0):
   xef.op-lock = no.
   {est/op-lock.i xest}
   ASSIGN
    op-lock.val[1] = 1
    op-lock.val[2] = 1.
   run ce/com/localk.p (0, ?).
end.

find xef where recid(xef) = call_id no-lock no-error.

qty = xest.est-qty[1].

/* end ---------------------------------- copr. 1992  advanced software, inc. */
