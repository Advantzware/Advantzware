/* --------------------------------------------------- ce/mach-sq2.p 10/94 gb */
/* create machine routing sequence    part-2                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

DEF BUFFER op-lock FOR reftable.

def shared var maxco as int no-undo.
def shared var v-2 as logical init false.
def shared var xcal as de NO-UNDO.

def var cumul as dec NO-UNDO.
def var save-qty as int NO-UNDO.
def new shared var v-n-out as int init 1 no-undo.
def var v-spo as dec NO-UNDO.
def var r-spo as dec extent 1 NO-UNDO.
def var spo as de NO-UNDO.
def var v-num-up like xeb.num-up NO-UNDO.
DEF VAR ip-rowid AS ROWID NO-UNDO.

DEF SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR CALL_id AS RECID NO-UNDO.
DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
{ce/mach-lst.i}

{ce/mach-ink.i}

{est/d-machex.i NEW}


run ce/mach-chk.p (no).

j = 1.
for each m-lst,
    first mach
    {sys/look/machW.i}
      and mach.m-code eq m-lst.m-code
    no-lock
    by m-lst.seq:

  create est-op.
  assign
   est-op.company    = xest.company   
   est-op.e-num      = xest.e-num
   est-op.est-no     = xest.est-no
   est-op.qty        = qty
   est-op.line       = j
   est-op.s-num      = xef.form-no
   est-op.b-num      = IF mach.p-type EQ "B" THEN xeb.blank-no ELSE 0
   est-op.op-pass    = m-lst.pass-no
   est-op.op-sb      = yes
   est-op.m-code     = mach.m-code
   est-op.m-dscr     = mach.m-dscr
   est-op.n-out      = minimum(m-lst.n-out,mach.num-wid)
   est-op.dept       = m-lst.dept
   est-op.d-seq      = mach.d-seq
   est-op.op-spoil   = mach.run-spoil
   est-op.op-crew[1] = mach.mr-crusiz
   est-op.op-crew[2] = mach.run-crusiz
   est-op.op-rate[1] = mach.mr-trate
   est-op.op-rate[2] = mach.run-trate.

  if mach.p-type eq "B" then est-op.op-sb = no.
  j = j + 1.
end.

i = 0.
for each est-op
    where est-op.company = xest.company
       AND est-op.est-no eq xest.est-no
      and est-op.qty   eq qty:
  i = i + 1.
  est-op.line = i.
end.

ASSIGN
 save-qty = qty
 v-n-out = (IF xef.n-out   EQ 0 THEN 1 ELSE xef.n-out) *
           (IF xef.n-out-l EQ 0 THEN 1 ELSE xef.n-out-l)
 cumul   = qty / xeb.num-up / v-n-out.

FOR EACH est-op
    WHERE est-op.company = xest.company
      AND est-op.est-no eq xest.est-no
      AND est-op.qty   eq save-qty
    BY est-op.line desc:
  ASSIGN
   v-num-up = xeb.num-up
   ip-rowid = ROWID(est-op).

  {ce/prokalk.i xeb}
END.

qty = save-qty.

for each ef
    where ef.company eq xest.company
      and ef.est-no  eq xest.est-no:
  ef.op-lock = yes.
end.
find first xef
    where xef.company eq xest.company
      and xef.est-no  eq xest.est-no
    no-error.
call_id = recid(xef).

for each est-op
    where est-op.company eq xest.company
      and est-op.est-no  eq xest.est-no
      and est-op.qty     eq qty
    break by est-op.s-num:

  if first-of(est-op.s-num) then do:
     find first ef
         where ef.company eq est-op.company
           and ef.est-no  eq est-op.est-no
           and ef.form-no eq est-op.s-num
         no-error.
     if avail ef then
       assign
        ef.op-lock = yes
        ef.gsh-qty = est-op.num-sh.
  end.
end.

find xef where recid(xef) eq call_id no-lock no-error.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
