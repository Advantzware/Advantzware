/* --------------------------------------=-------- ce/box/mach-re2.p 9/92 cd  */
/* recalculate values of sequenced machines.                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
define shared buffer xest for est.
define shared buffer xef  for ef.
define shared buffer xeb  for eb.

def shared var sh-wid   as de.
def shared var sh-len   as de.
def shared var xcal as de.
def new shared var maxco as int.
def new shared var v-2 as logical init false.
def shared var save_id as recid.
def shared var chosen as logical format "y/n".
def var mess as ch format "x(80)" extent 2.

DEF VAR fil_id AS RECID NO-UNDO.
DEF SHARED VAR qty AS INT NO-UNDO.


find est-op where recid(est-op) = fil_id no-error.

blok:
do on error undo:
   find first mach {sys/look/machW.i}  and
		    mach.m-code  = est-op.m-code
		    no-lock no-error.
   if not available mach then do:
      delete est-op.
      leave.
   end.
   /*
   assign
      est-op.m-dscr     = mach.m-dscr        user might want own dscr here
      est-op.op-spoil   = mach.run-spoil.

      est-op.s-num      = xef.form-no
      est-op.b-num      = xeb.blank-no.
   if mach.p-type =  "B" then est-op.s-num = 0.
   */
   if mach.p-type = "B" then est-op.op-sb = false.

   maxco = 0.
   if est-op.dept = "PR" then do:
      do i = 1 to 20:
	 if xeb.i-ps2[i] ne est-op.op-pass then next.
	 find first item {sys/look/itemW.i} and item.i-no = xeb.i-code2[i]
	 no-lock no-error.
	 if (mach.coater = no and item.mat-type ne "I") or
	    index("IV",item.mat-type) = 0 then next.
	 maxco = maxco + 1.
      end.  /* maxco now = # colors/coating this machine/this pass */
   end.
   if not xef.op-lock then do:
      /* use ce directory ... */
      /* {ce/kmr-run.i &fil=est-op &fld=op-mr &fil2=est-op &fld2=op-speed} */

      if mach.col-wastesh ne 0 then est-op.op-waste = mach.col-wastesh * maxco.
      else est-op.op-waste = mach.mr-waste.
      if mach.washup > 0 and mach.col-pass = "P" then
	 est-op.op-mr = est-op.op-mr + mach.washup.
      else if mach.washup > 0 and mach.col-pass = "C" then
	  est-op.op-mr = est-op.op-mr + (mach.washup * maxco).
   end.
   assign
      est-op.op-rate[1] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[1]) +
			   mach.mr-varoh + mach.mr-fixoh.
      est-op.op-rate[2] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[2]) +
			   mach.run-varoh + mach.run-fixoh.
end.

find est-op where recid(est-op) = fil_id no-lock no-error.

def var ttt as de.
def var spo as de.
def var cumul as de.

ttt = xest.est-qty[1] / xeb.num-up.
cumul = ttt.
for each est-op
    where est-op.company eq xest.company
      and est-op.est-no  eq xest.est-no
    by est-op.line descending:
   cumul = cumul / (1 - (est-op.op-spoil / 100)).
   if est-op.op-sb = true then spo = spo + est-op.op-waste.
   else
   if est-op.op-sb = no then spo = spo + (est-op.op-waste / xeb.num-up).
   est-op.num-sh = cumul.
end.

save_id = fil_id.
fil_id = recid(xef).
find xef where recid(xef) = fil_id no-error.
xef.gsh-qty = integer(cumul + spo) .
find xef where recid(xef) = fil_id no-lock no-error.
fil_id = save_id.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
