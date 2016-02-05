/* --------------------------------------------------- ce/mach-sq2.p 4/92 cd  */
/* create machine routing sequence    part-2                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def new shared var sh-wid  as de NO-UNDO.
def new shared var sh-len  as de NO-UNDO.
def shared var maxco as int no-undo.
def shared var v-2 as logical init FALSE NO-UNDO.
def shared var xcal    as de NO-UNDO.
DEF SHARED VAR CALL_id AS RECID NO-UNDO.

{ce/mach-lst.i}


j = 1.
for each m-lst by m-lst.f-no by m-lst.seq by m-lst.b-no by m-lst.pass-no:
   find first mach {sys/look/mach.w} and
	      mach.m-code  = m-lst.m-code no-lock no-error.
   create est-op.
   assign
      est-op.e-num      = xest.e-num
      est-op.est-no     = xest.est-no
      est-op.line       = j
      est-op.dept       = m-lst.dept
      est-op.d-seq      = mach.d-seq
      est-op.s-num      = m-lst.f-no
      est-op.b-num      = m-lst.b-no
      est-op.op-pass    = m-lst.pass-no
      est-op.m-code     = mach.m-code
      est-op.op-spoil   = mach.run-spoil
      est-op.op-crew[1] = mach.mr-crusiz
      est-op.op-crew[2] = mach.run-crusiz
      est-op.op-rate[1] = mach.mr-trate
      est-op.op-rate[2] = mach.run-trate.

   if mach.p-type ne "B" then est-op.op-sb = true.
   else est-op.op-sb = no.

   if m-lst.dscr ne "" then est-op.m-dscr = m-lst.dscr.
   else est-op.m-dscr = mach.m-dscr.

   find first xef
       where xef.company eq est-op.company
         and xef.est-no  eq est-op.est-no
         and xef.form-no eq est-op.s-num
       no-error.
   find first xeb
       where xeb.company eq est-op.company
         and xeb.est-no  eq est-op.est-no
         and xeb.form-no eq est-op.s-num
       no-error.
   if est-op.b-num > 0 then
   find first xeb
       where xeb.company  eq est-op.company
         and xeb.est-no   eq est-op.est-no
         and xeb.form-no  eq est-op.s-num
         and xeb.blank-no eq est-op.b-num
       no-error.
   if est-op.dept = "PR" then
   do:
      maxco = 0.
      /* find blank with most colors */
      for each eb of xef:
	 do i = 1 to 20:
	    if eb.i-ps2[i] ne est-op.op-pass then next.
	    find first item {sys/look/item.w} and item.i-no = eb.i-code2[i]
	    no-lock no-error.
	    if not available item or
	       (mach.coater = no and item.mat-type ne "I") or
	       index("IV",item.mat-type) = 0 then next.
	    maxco = maxco + 1.
	 end.
	 if maxco > mach.max-color then maxco = mach.max-color.
      end.  /* maxco now = # colors/coating this machine/this pass */
   end.
   qty = xeb.yld-qty.

   /* flip dimensions if corr. xgrain */
   if est-op.dept = "LM" and
      ((xef.n-out-l ne 0 and
	(xef.lam-dscr =  "R" or (xef.lam-dscr ne "R" and xef.xgrain = "S")))
       or
       (xef.n-out-l = 0 and (xef.lam-dscr ne "R" and xef.xgrain ne "S"))
      )
   then 
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
   
   {ce/box/kmr-run.i &fil=est-op &fld=op-mr &fil2=est-op &fld2=op-speed}
   /* reset dimensions if corr.xgrain */
   if est-op.dept = "LM" and
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
   
   if est-op.dept = "PR" and est-op.op-pass = 1 then
   do:
      find first xef
          where xef.company eq est-op.company
            and xef.est-no  eq est-op.est-no
            and xef.form-no eq est-op.s-num
          no-lock no-error.
      if mach.washup > 0 and mach.col-pass = "P" then
	 est-op.op-mr = est-op.op-mr + (mach.washup * xef.f-pass).
      else if mach.washup > 0 and mach.col-pass = "C" then
	 est-op.op-mr = est-op.op-mr + (mach.washup * xef.f-col).
      if mach.col-wastesh ne 0
	 then est-op.op-waste = mach.col-wastesh * xef.f-col.
   end.
   if est-op.dept ne "PR" then est-op.op-waste = mach.mr-waste.
   j = j + 1.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
