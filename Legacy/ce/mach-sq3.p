/* --------------------------------------------------- ce/mach-sq3.p 10/94 gb */
/* create machine routing sequence    part-3                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def shared var maxco as INT no-undo.
def shared var v-2 as logical init FALSE NO-UNDO.
def shared var xcal as de NO-UNDO.
def shared var sh-wid as de NO-UNDO.
def shared var sh-len as de NO-UNDO.

def var cumul as int NO-UNDO.
def var save-qty as int NO-UNDO.
DEF SHARED VAR qty AS INT NO-UNDO.

{ce/mach-lst.i}

assign save-qty = qty
       cumul    = qty / xeb.num-up.
for each est-op where est-op.company = xest.company and
                      est-op.est-no  = xest.est-no by line descending:
   find first mach {sys/look/machW.i} and
	      mach.m-code  = est-op.m-code no-lock no-error.
   if est-op.dept = "PR" then do:
      maxco = 0.
      do i = 1 to 20:
	 if xeb.i-ps2[i] ne est-op.op-pass then next.
	 find first item {sys/look/itemW.i} and item.i-no = xeb.i-code2[i]
	 no-lock no-error.
	 if (mach.coater = no and item.mat-type ne "I") or
	    index("IV",item.mat-type) = 0 then next.
	 maxco = maxco + 1.
      end.  /* maxco now = # colors/coating this machine/this pass */
   end.
   if est-op.dept = "PR" and col-wastesh ne 0
   then est-op.op-waste = mach.col-wastesh * maxco.
   else est-op.op-waste = mach.mr-waste.

   est-op.num-sh = cumul / (1 - (est-op.op-spoil / 100)).
   {sys/inc/roundup.i est-op.num-sh}
   if est-op.op-sb
   then est-op.num-sh = est-op.num-sh + est-op.op-waste.
   else est-op.num-sh = est-op.num-sh + (est-op.op-waste / xeb.num-up).
   {sys/inc/roundup.i est-op.num-sh}

   ASSIGN
   cumul = est-op.num-sh
   qty = est-op.num-sh * xeb.num-up.

   /* flip dimensions if corr. xgrain */
   if est-op.dept = "LM" and
      ((xef.n-out-l ne 0 and
	(xef.lam-dscr =  "R" or (xef.lam-dscr ne "R" and xef.xgrain = "S")))
       or
       (xef.n-out-l = 0 and (xef.lam-dscr ne "R" and xef.xgrain ne "S"))
      )
   then do:
      find first xef where xef.company = xest.company and
                           xef.est-no = xest.est-no no-error.
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
   end.
   /* {ce/kmr-run.i &fil=est-op &fld=op-mr &fil2=est-op &fld2=op-speed} */
   /* reset dimensions if corr.xgrain */
   if est-op.dept = "LM" and
      ((xef.n-out-l ne 0 and
	(xef.lam-dscr =  "R" or (xef.lam-dscr ne "R" and xef.xgrain = "S")))
       or
       (xef.n-out-l = 0 and (xef.lam-dscr ne "R" and xef.xgrain ne "S"))
      )
   then do:
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
      find first xef where xef.company = xest.company and
                           xef.est-no = xest.est-no no-lock no-error.
   end.
   if mach.washup > 0 and mach.col-pass = "P" then
      est-op.op-mr = est-op.op-mr + mach.washup.
   else if mach.washup > 0 and mach.col-pass = "C" then
       est-op.op-mr = est-op.op-mr + (mach.washup * maxco).
end.
qty = save-qty.
find ef where recid(ef) = recid(xef) no-error.
ef.op-lock = true.
find xef where recid(xef) = recid(ef) no-lock no-error.
release ef.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
