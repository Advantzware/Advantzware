/* ------------------------------------------=-------- ce/mach-rek.p 10/94 gb */
/* recalculate values of sequenced machines.                                  */
/* -------------------------------------------------------------------------- */
{sys/inc/var.i shared}
define shared buffer xest for est.
define shared buffer xef  for ef.
define shared buffer xeb  for eb.

define buffer xest-op for est-op.
DEF BUFFER op-lock FOR reftable.

def shared var xcal as de.
def var cumul as dec.
def new shared var maxco as int no-undo.
def new shared var v-2 as logical init false.
def shared var save_id as recid.
def shared var chosen as logical format "y/n".
def var mess as ch format "x(80)" extent 2.
def var save-qty as int.
define shared var head as ch format "x(78)" extent 20.
def new shared var v-n-out as int no-undo.
def var v-spo as dec.
def var r-spo as dec extent 99.
def var spo as de.
def var v-num-up like xeb.num-up.
def var v-est-sh like est-op.num-sh.
def var v-high-sh like est-op.num-sh.
DEF VAR ip-rowid AS ROWID NO-UNDO.

DEF SHARED VAR CALL_id AS RECID NO-UNDO.
DEF VAR fil_id AS RECID NO-UNDO.
DEF SHARED VAR qty AS INT NO-UNDO.


{ce/mach-ink.i new}

run ce/mach-ink.p.

find est-op where recid(est-op) = fil_id no-error.

find first xef
    where xef.company eq est-op.company
      and xef.est-no  eq est-op.est-no
      and xef.form-no eq est-op.s-num
    no-lock.

find first xeb
    where xeb.company eq est-op.company
      and xeb.est-no  eq est-op.est-no
      and xeb.form-no eq est-op.s-num
    no-lock.

assign
 save-qty           = qty
 v-num-up           = xeb.num-up / (if xest.form-qty eq 1 then 2 else 1)
 v-n-out            = (if xef.n-out   eq 0 then 1 else xef.n-out) *
                      (if xef.n-out-l eq 0 then 1 else xef.n-out-l)
 v-est-sh           = qty / v-num-up / v-n-out
 r-spo[xef.form-no] = 0.

{sys/inc/roundup.i v-est-sh}

for each est-op
    where est-op.company eq xef.company
      and est-op.est-no  eq xef.est-no
      and est-op.s-num   eq xef.form-no
      and est-op.line    lt 500
    break by est-op.b-num desc by est-op.d-seq desc by line desc:

  if first-of(est-op.b-num) then do:
    if first(est-op.b-num) then v-high-sh = v-est-sh.
    cumul = if est-op.b-num eq 0 then v-high-sh else v-est-sh.
  end.

  ip-rowid = ROWID(est-op).

  {ce/prokalk.i xeb}

  if est-op.num-sh gt v-high-sh and
     est-op.b-num  ne 0         then v-high-sh = est-op.num-sh.
end.

find est-op where recid(est-op) = fil_id no-lock no-error.
qty = save-qty.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
