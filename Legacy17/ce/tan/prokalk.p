/* ----------------------------------------------- ce/tan/prokalk.p 03/98 JLF */
/* recalculate values of sequenced machines.                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def buffer xeb2 for eb.

def buffer xop for est-op.
DEF BUFFER bf-eb FOR eb.
{ce/print4.i shared shared}

def new shared var maxco as int no-undo.
def var mess as ch format "x(80)" extent 2 NO-UNDO.
def new shared var v-n-out as int init 1 no-undo.
def var v-spo as dec NO-UNDO.
def var cumul as dec NO-UNDO.
def var v-hold as int NO-UNDO.
def var v-num-up like xeb.num-up NO-UNDO.
def var v-dept like est-op.dept NO-UNDO.
def var v-est-qty like est.est-qty extent 1 NO-UNDO.
DEF VAR FIL_id AS RECID NO-UNDO.
DEF VAR ip-rowid AS ROWID NO-UNDO.

DEF NEW SHARED VAR CALL_id AS RECID NO-UNDO.

DEF SHARED VAR qty AS INT NO-UNDO.

def workfile w-qty
  field b-num  like est-op.b-num
  field num-sh as dec.

{ce/mach-ink.i new}

run ce/mach-ink.p.

assign
 spo      = 0
 r-spo[1] = 0
 v-num-up = xeb.num-up
 v-n-out  = (if xef.n-out   eq 0 then 1 else xef.n-out) *
            (if xef.n-out-l eq 0 then 1 else xef.n-out-l).

v-est-qty[1] = 0.
for each bf-eb
    where bf-eb.company = xest.company
      AND bf-eb.est-no     eq xest.est-no
      and bf-eb.form-no    eq xef.form-no
    no-lock:

  create w-qty.
  assign
   w-qty.b-num  = bf-eb.blank-no
   w-qty.num-sh = bf-eb.bl-qty / v-num-up / v-n-out.

   v-est-qty[1] = v-est-qty[1] + bf-eb.bl-qty.

  {sys/inc/roundup.i w-qty.num-sh}.
end.

assign
 save-qty = qty
 qty      = v-est-qty[1].

for each est-op
    where est-op.company = xest.company
      AND est-op.est-no eq xest.est-no
      and est-op.s-num eq xef.form-no
      and est-op.line  ge 500
    by line descending:

  cumul = 0.

  for each w-qty
      where (w-qty.b-num eq est-op.b-num and est-op.b-num ne 0)
         or est-op.b-num eq 0:

    cumul = cumul + w-qty.num-sh.
  end.
  
  find first xeb2
      where xeb2.company = est-op.company
        AND xeb2.est-no   eq est-op.est-no
        and xeb2.form-no  eq est-op.s-num
        and xeb2.blank-no eq est-op.b-num
      no-lock no-error.

  ip-rowid = ROWID(est-op).

  {ce/prokalk.i xeb2}
  
  v-hold = 0.

  for each w-qty
      where (w-qty.b-num eq est-op.b-num and est-op.b-num ne 0)
         or est-op.b-num eq 0:

    v-hold = v-hold + w-qty.num-sh.
  end.

  for each w-qty
      where (w-qty.b-num eq est-op.b-num and est-op.b-num ne 0)
         or est-op.b-num eq 0:

    w-qty.num-sh = cumul * w-qty.num-sh / v-hold.
    {sys/inc/roundup.i w-qty.num-sh}.
  end.
end.

qty = save-qty.

{sys/inc/roundup.i r-spo[1]}
fil_id = recid(xef).
find xef where recid(xef) eq fil_id no-error.
xef.gsh-qty = 0.
for each w-qty:
  xef.gsh-qty = xef.gsh-qty + w-qty.num-sh.
end.
release est-op.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
