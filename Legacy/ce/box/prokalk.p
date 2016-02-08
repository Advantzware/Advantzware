/* ------------------------------------------------ ce/box/prokalk.p 1/92 cd  */
/* recalculate values of sequenced machines.                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

define shared buffer xest for est.
define shared buffer xef  for ef.
define shared buffer xeb  for eb.

define new shared buffer xop for est-op.

DEF BUFFER op-lock FOR reftable.

def shared var sh-wid as de.
def shared var sh-len as de.
def shared var xcal as de.
def new shared var maxco as int no-undo.
def new shared var v-2 as logical init false.
def shared var r-spo as de.
def shared var i-qty as de extent 10.
def shared var save_id as recid.
def shared var chosen as logical format "y/n".
def var mess as ch format "x(80)" extent 2.
def var save-qty like qty.
DEF VAR v-num-up AS INT NO-UNDO.

def new shared var v-n-out as int init 1 no-undo.
DEF SHARED VAR qty AS INT NO-UNDO.
DEF VAR fil_id AS RECID NO-UNDO.
DEF NEW SHARED VAR CALL_id AS RECID NO-UNDO.


save-qty = qty.

{sys/form/s-top.f}
    
{est/op-lock.i xest}

ASSIGN
 op-lock.val[1] = 1
 op-lock.val[2] = 1.

blok:
for each xop
    where xop.company eq xest.company
      and xop.est-no  eq xest.company
      and xop.line    gt 500:
   find first mach {sys/look/machW.i}  and mach.m-code = xop.m-code
   no-lock no-error.
   if not available mach then leave.

   qty = save-qty.
   if mach.p-type = "B" then do:
      xop.op-sb = false.
      /* qty = qty * xeb.num-up. */
   end.

   maxco = 0.
   if xop.dept = "PR" then
   do:
      do i = 1 to 20:
	 if xeb.i-ps2[i] ne xop.op-pass then next.
	 find first item {sys/look/itemW.i} and item.i-no = xeb.i-code2[i]
	 no-lock no-error.
	 if not available item or (mach.coater = no and item.mat-type ne "I") or
	    index("IV",item.mat-type) = 0 then next.
	 maxco = maxco + 1.
      end.  /* maxco now = # colors/coating this machine/this pass */
   end.
   if not xef.op-lock then do:

/* JLF added 01/26/96 */
      xop.op-spoil = 0.
/* JLF added 01/26/96 */

      {ce/kspoil.i &fil=xop &fld=op-spoil}

/* JLF added 01/23/96 */
      xop.op-spoil = xop.op-spoil + mach.run-spoil.
/* JLF added 01/23/96 */

      /* flip dimensions if corr. xgrain */
      find first xef of xef no-error.
      if xop.dept = "LM" and
	 ((xef.n-out-l ne 0 and
	   (xef.lam-dscr =  "R" or (xef.lam-dscr ne "R" and xef.xgrain = "S")))
	  or
	  (xef.n-out-l = 0 and (xef.lam-dscr ne "R" and xef.xgrain ne "S"))
	 ) THEN
     ASSIGN
	 zzz = sh-wid
     sh-wid = sh-len
     sh-len = zzz
	 zzz = xef.lsh-wid
     xef.lsh-wid = xef.lsh-len
     xef.lsh-len = zzz
	 zzz = xef.gsh-wid
     xef.gsh-wid = xef.gsh-len
     xef.gsh-len = zzz
	 zzz = xef.nsh-wid
     xef.nsh-wid = xef.nsh-len
     xef.nsh-len = zzz.
      
      /* use ce directory ... */
      {ce/kmr-run.i &fil=xop &fld=op-mr &fil2=xop &fld2=op-speed}
      /* reset dimensions if corr.xgrain */
      if xop.dept = "LM" and
	 ((xef.n-out-l ne 0 and
	   (xef.lam-dscr =  "R" or (xef.lam-dscr ne "R" and xef.xgrain = "S")))
	  or
	  (xef.n-out-l = 0 and (xef.lam-dscr ne "R" and xef.xgrain ne "S"))
	 ) then
     ASSIGN
	 zzz = sh-wid
     sh-wid = sh-len
     sh-len = zzz
	 zzz = xef.lsh-wid
     xef.lsh-wid = xef.lsh-len
     xef.lsh-len = zzz
	 zzz = xef.gsh-wid
     xef.gsh-wid = xef.gsh-len
     xef.gsh-len = zzz
	 zzz = xef.nsh-wid
     xef.nsh-wid = xef.nsh-len
     xef.nsh-len = zzz.
      
      find first xef of xef no-lock no-error.

      xop.op-waste = mach.mr-waste + (mach.col-wastesh * maxco).

      if mach.washup > 0 and mach.col-pass = "P" then
	 xop.op-mr = xop.op-mr + mach.washup.
      else if mach.washup > 0 and mach.col-pass = "C" then
	  xop.op-mr = xop.op-mr + (mach.washup * maxco).
   assign
      xop.op-rate[1] = (mach.lab-rate[mach.lab-drate] * xop.op-crew[1]) +
			mach.mr-varoh + mach.mr-fixoh
      xop.op-rate[2] = (mach.lab-rate[mach.lab-drate] * xop.op-crew[2]) +
			mach.run-varoh + mach.run-fixoh.
   end.
end.

qty = save-qty.
def var ttt as de.
def var spo as de.
def var cumul as de.

ASSIGN
ttt = qty / (xeb.num-up / 2)
cumul = ttt.

for each xop
    where xop.company eq xest.company
      and xop.est-no  eq xest.company
      and xop.line    gt 500
    by xop.line descending:

   if xop.op-sb = no then
      ASSIGN
      r-spo = r-spo + ((cumul * xeb.num-up) / (1 - (xop.op-spoil / 100))
		       - (cumul * xeb.num-up)) / xeb.num-up
      cumul = ((cumul * xeb.num-up) / (1 - (xop.op-spoil / 100))) / xeb.num-up
      cumul = cumul + (xop.op-waste / xeb.num-up)
      spo = spo + (xop.op-waste / xeb.num-up).
   
   else
      ASSIGN
      r-spo = r-spo + (cumul / (1 - (xop.op-spoil / 100)) - cumul)
      cumul = cumul / (1 - (xop.op-spoil / 100))
      cumul = cumul + xop.op-waste
      spo = spo + xop.op-waste.
   
   {sys/inc/roundup.i cumul}
   {sys/inc/roundup.i r-spo}
   {sys/inc/roundup.i spo}
   xop.num-sh = cumul.
end.

fil_id = recid(xef).
find xef where recid(xef) = fil_id no-error.
find first est-op
    where est-op.company eq xef.company
      and est-op.est-no  eq xef.est-no
      and est-op.s-num   eq xef.form-no
      and est-op.line    gt 500
    no-error.
if available est-op then xef.gsh-qty = est-op.num-sh.
release est-op.
find xef where recid(xef) = fil_id no-error.
qty = save-qty.
DELETE op-lock.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
