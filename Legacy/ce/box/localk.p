/* ------------------------------------------------- ce/box/localk.p 10/94 gb */
/* recalculate values of sequenced machines.                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

define shared buffer xest for est.
define shared buffer xef  for ef.
define shared buffer xeb  for eb.
define new shared buffer xop for est-op.

DEF BUFFER op-lock FOR reftable.

def        var t-blksht as int extent 99. /* total # blanks per sheets  */
def        var t-blkqty as int extent 99. /* total # blanks per sheets  */
def        var t-shtfrm as de extent 99.
def        var r-spo    as de extent 99.
def        var sav-spo  as de extent 99.
def        var cumul    as de.
def        var spo      as de.
def        var eb_id as recid.
def shared var maxco as int no-undo.
def new shared var v-2 as logical init false.
def        var save-qty as de.
def        var xcal     as de.
def        var tt-blk as de.
DEF VAR v-num-up AS INT NO-UNDO.

def new shared var v-n-out as int init 1 no-undo.

DEF SHARED VAR CALL_id AS RECID NO-UNDO.
DEF VAR fil_id AS RECID NO-UNDO.
DEF SHARED VAR qty AS INT NO-UNDO.


assign save-qty = qty
       call_id  = recid(xeb)
       zzz      = 0
       maxco    = xef.f-col.
    
{est/op-lock.i xest}

ASSIGN
 op-lock.val[1] = 1
 op-lock.val[2] = 1.

   for each xeb
       where xeb.company eq xest.company
         and xeb.est-no  eq xest.est-no
         and xeb.form-no eq xef.form-no:
      /* find sheet qty needed for this form (without spoil)*/
      if (xeb.yld-qty / xeb.num-up) > zzz then
      assign zzz = (xeb.yld-qty / xeb.num-up).
      assign t-shtfrm[xeb.form-no] = zzz
			   call_id = recid(xeb).
      {sys/inc/roundup.i t-shtfrm[xeb.form-no]}
   end.

   find first xeb where recid(xeb) = call_id no-lock no-error.
   /* process 'blank' lines of this sheet */
   for each xop
       where xop.company eq xest.company
         and xop.est-no  eq xest.est-no
		 and xop.s-num   eq xef.form-no
         and xop.b-num   ne 0
   break by xop.b-num by xop.line descending:
      if first-of(xop.b-num) then do:
	 find first eb
         where eb.company  eq xest.company
           and eb.est-no   eq xest.est-no
		   and eb.form-no  eq xop.s-num
           and eb.blank-no eq xop.b-num
         no-lock no-error.
	 assign eb_id      = recid(eb)
		cumul      = t-shtfrm[xef.form-no].
      end.
      if xop.num-sh = 0 then xop.num-sh = t-shtfrm[xef.form-no].
      if not available eb then find eb where recid(eb) = eb_id no-lock no-error.
      find first mach {sys/ref/machW.i} and mach.m-code = xop.m-code
      no-lock no-error.
      qty = eb.yld-qty.

/* JLF added 01/26/96 */
      xop.op-spoil = 0.
/* JLF added 01/26/96 */

      {ce/kspoil.i &fil=xop &fld=op-spoil &fra=1}

/* JLF added 01/23/96 */
      xop.op-spoil = xop.op-spoil + mach.run-spoil.
/* JLF added 01/23/96 */

      if xop.op-spoil ne 0
      then sav-spo[eb.blank-no] = sav-spo[eb.blank-no] +
			   ((cumul / (1 - (xop.op-spoil / 100))) - cumul).

      ASSIGN
      cumul = cumul / (1 - (xop.op-spoil / 100))
      spo = xop.op-waste / eb.num-up. /* op-w=in blks, make in sht */
      {sys/inc/roundup.i cumul}
      {sys/inc/roundup.i spo}

      assign
      xop.num-sh = cumul + spo
      cumul = xop.num-sh.
   end.
   do i = 1 to 10:
      if sav-spo[i] > r-spo[xef.form-no]
      then assign r-spo[xef.form-no] = sav-spo[i].
   end.
   sav-spo = 0. /* zero array */
   for each xop
       where xop.company eq xest.company
         and xop.est-no  eq xest.est-no
		 and xop.s-num   eq xef.form-no
         and xop.b-num   eq 0
       break by xop.s-num by xop.line descending:
      find first mach {sys/ref/machW.i} and mach.m-code = xop.m-code
      no-lock no-error.
      maxco = xef.f-col.
      if mach.coater = true then maxco = maxco + xef.f-coat.
      if first-of(xop.s-num) then do:
	 cumul = 0. spo   = 0.
	 for each est-op
         where est-op.company eq xest.company
           and est-op.est-no  eq xest.est-no
		   and est-op.s-num   eq xef.form-no
		   and est-op.b-num   ne 0:
	    if est-op.num-sh   > cumul then cumul = est-op.num-sh.
        find first eb
            where eb.company  eq xest.company
              and eb.est-no   eq xest.est-no
		      and eb.form-no  eq est-op.s-num
              and eb.blank-no eq est-op.b-num
            no-lock no-error.
	    if (est-op.op-waste / eb.num-up) > sav-spo[xef.form-no]
	    then sav-spo[xef.form-no] = (est-op.op-waste / eb.num-up).
	    /* last sheet fed op must supply enough for the blank op that
	    needs the most */
	 end.
      end.

      ASSIGN
      xop.num-sh = cumul
/* JLF added 01/26/96 */
      xop.op-spoil = 0.
/* JLF added 01/26/96 */

      {ce/kspoil.i &fil=xop &fld=op-spoil &fra=2}

/* JLF added 01/23/96 */
      xop.op-spoil = xop.op-spoil + mach.run-spoil.
/* JLF added 01/23/96 */

      if xop.op-spoil ne 0
      then assign
	 r-spo[xef.form-no] = r-spo[xef.form-no] +
			    (cumul / (1 - (xop.op-spoil / 100)) - cumul)
	 cumul = cumul / (1 - (xop.op-spoil / 100)).
      sav-spo[xef.form-no] = sav-spo[xef.form-no] + xop.op-waste.
      {sys/inc/roundup.i sav-spo[xef.form-no]}
      spo = xop.op-waste.
      {sys/inc/roundup.i cumul}
      {sys/inc/roundup.i spo}
      ASSIGN
      xop.num-sh = cumul + spo
      cumul = xop.num-sh.
   end.
   {sys/inc/roundup.i r-spo[xef.form-no]}

if not xef.op-lock then
for each xop
    where xop.company eq xest.company
      and xop.est-no  eq xest.est-no
      and xop.s-num   eq xef.form-no
    break by xop.b-num by xop.line descending:
   find first mach {sys/look/machW.i}  and mach.m-code = xop.m-code
   no-lock no-error.
   if not available mach then leave.
   z = 0.
   if first-of(xop.b-num) then do:
     if xop.b-num > 0 then do:
       find first eb
           where eb.company  eq xest.company
             and eb.est-no   eq xest.est-no
             and eb.form-no  eq xop.s-num
             and eb.blank-no eq xop.b-num
           no-lock no-error.
	   z = eb.num-up.
     end.
     else
     for each eb
         where eb.company  eq xest.company
           and eb.est-no   eq xest.est-no
           and eb.form-no  eq xop.s-num:
	   z = z + eb.num-up.
     end.
   end.
   if not available eb then
      find first eb
          where eb.company  eq xest.company
            and eb.est-no   eq xest.est-no
            and eb.form-no  eq xop.s-num
          no-lock no-error.
   qty = xop.num-sh * z.
   if mach.p-type = "B" then xop.op-sb = false.
   if xop.dept = "RC" then
   do:
      assign cumul = cumul / xef.n-out.
      {sys/inc/roundup.i cumul}
      assign spo   = spo / xef.n-out.
      {sys/inc/roundup.i spo}
      assign v-n-out = xef.n-out.
   end.
   if xop.dept = "PR" then do:
      find first ef
           where ef.company  eq xest.company
             and ef.est-no   eq xest.est-no
             and ef.form-no  eq xop.s-num
		   no-lock no-error.
      maxco = ef.f-col.
      if mach.coater = true then maxco = maxco + ef.f-coat.
      /*
      do i = 1 to 10:
	 if eb.i-ps2[i] ne xop.op-pass then next.
	 find first item {sys/look/itemW.i} and item.i-no = eb.i-code2[i]
	 no-lock no-error.
	 if not available item or (mach.coater = no and item.mat-type ne "I") or
	    index("IV",item.mat-type) = 0 then next.
	 maxco = maxco + 1.
      end.  /* maxco now = # colors/coating this machine/this pass */
      */
   end.


   /* flip dimensions if corr. xgrain */
   call_id = recid(xef).
   find first xef where recid(xef) = call_id no-error.
   if xop.dept = "LM" and
      ((xef.n-out-l ne 0 and
	(xef.lam-dscr =  "R" or (xef.lam-dscr ne "R" and xef.xgrain = "S")))
       or
       (xef.n-out-l = 0 and (xef.lam-dscr ne "R" and xef.xgrain ne "S"))
      )
   THEN

      ASSIGN
      zzz = xef.gsh-wid
      xef.gsh-wid = xef.gsh-len
      xef.gsh-len = zzz
      zzz = xef.lsh-wid
      xef.lsh-wid = xef.lsh-len
      xef.lsh-len = zzz
      zzz = xef.nsh-wid
      xef.nsh-wid = xef.nsh-len
      xef.nsh-len = zzz
      zzz = xef.trim-w
      xef.trim-w = xef.trim-l
      xef.trim-l = zzz.
   
   /* use ce directory ... */
   {ce/kmr-run.i &fil=xop &fld=op-mr &fil2=xop &fld2=op-speed}
   /* reset dimensions if corr.xgrain */
   if xop.dept = "LM" and
      ((xef.n-out-l ne 0 and
	(xef.lam-dscr =  "R" or (xef.lam-dscr ne "R" and xef.xgrain = "S")))
       or
       (xef.n-out-l = 0 and (xef.lam-dscr ne "R" and xef.xgrain ne "S"))
      )
   THEN
   ASSIGN
      zzz = xef.gsh-wid
      xef.gsh-wid = xef.gsh-len
      xef.gsh-len = zzz
      zzz = xef.lsh-wid
      xef.lsh-wid = xef.lsh-len
      xef.lsh-len = zzz
      zzz = xef.nsh-wid
      xef.nsh-wid = xef.nsh-len
      xef.nsh-len = zzz
      zzz = xef.trim-w
      xef.trim-w = xef.trim-l
      xef.trim-l = zzz.

   find first xef where recid(xef) = call_id no-lock no-error.

/* if mach.col-wastesh ne 0 then xop.op-waste = mach.col-wastesh * maxco.
   else xop.op-waste = mach.mr-waste.
*/
   xop.op-waste = (mach.col-wastesh * maxco) + mach.mr-waste.

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

fil_id = recid(xef).
find xef where recid(xef) = fil_id no-error.
ASSIGN
xef.gsh-qty = cumul
qty = save-qty.
DELETE op-lock.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
