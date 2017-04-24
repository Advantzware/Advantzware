/* -------------------------------------------------- ce/kxy-form.p 10/98 JLF */
/* Create MR hours with a formula                                             */
/* -------------------------------------------------------------------------- */

def input        parameter v-recid as   recid.
def input-output parameter v-hours like est-op.op-mr.

{sys/inc/var.i shared}
def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.
DEF SHARED VAR qty AS INT NO-UNDO.
DEF BUFFER eb FOR eb.

def shared var maxco    as int no-undo.
def shared var sh-wid   as de.
def shared var sh-len   as de.
def shared var xcal     as de.
def shared var v-n-out  as int no-undo.

def var v-up     as   int.
def var v-die-in like xef.die-in.
def var v-field  like mstd.mr-x.


{ce/msfcalc.i}

find first mstd where recid(mstd) eq v-recid no-lock no-error.
find first mach of mstd no-lock.

if mach.p-type eq "B" then v-up = xeb.num-up.
else
run sys/inc/numup.p (xef.company,xef.est-no, xef.form-no, output v-up).

v-field = if mstd.mr-x eq 99 then mstd.mr-y else mstd.mr-x.

if v-field eq 19 then do:
  v-die-in = 0.

  if xest.est-type eq 4 or xef.die-in eq 0 then
  for each eb FIELDS(die-in) where
      eb.company = xef.company AND
      eb.est-no   eq xef.est-no AND
	  eb.form-no eq xef.form-no   no-lock:
      v-die-in = v-die-in + eb.die-in.
  end.

  else
    v-die-in = xef.die-in.

  v-hours = round(v-die-in * v-hours / 3600,2).
end.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
