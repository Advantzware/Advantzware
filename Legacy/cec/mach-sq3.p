/* -------------------------------------------------- cec/mach-sq3.p 4/92 cd  */
/* create machine routing sequence    part-3                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

DEF BUFFER op-lock FOR reftable.

def shared var maxco as INT NO-UNDO.
def shared var v-2 as logical init FALSE NO-UNDO.
def shared var xcal as de no-undo.
def shared var sh-wid as de no-undo.
def shared var sh-len as de no-undo.

def var qty as int no-undo.
def var call_id as recid no-undo.
def var cumul as INT NO-UNDO.
def var save-qty as INT NO-UNDO.
def var vn-out as INT NO-UNDO.
DEF VAR v-dep LIKE eb.dep NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.

DEF BUFFER b-eb FOR eb.

{ce/mach-lst.i}
    
{est/op-lock.i xest}

ASSIGN
 op-lock.val[1] = 1
 op-lock.val[2] = 1.

assign save-qty = qty
       cumul    = qty / xeb.num-up.
for each est-op where est-op.e-num = xest.e-num by line descending:
   find first mach {sys/look/machW.i} and
              mach.m-code  = est-op.m-code no-lock no-error.
   if est-op.dept = "PR" then do:
      maxco = 0.
      do i = 1 to 10:
         if xeb.i-ps[i] ne est-op.op-pass then next.
         find first item {sys/look/itemW.i} and item.i-no = xeb.i-code[i]
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
      find first xef where xef.e-num = xest.e-num no-error.
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
   end.

   ASSIGN
      v-count = 0
      v-dep = xeb.dep.

   IF xest.est-type EQ 6 THEN
   FOR EACH b-eb FIELDS(style) WHERE
       b-eb.company EQ xef.company AND
       b-eb.est-no  EQ xef.est-no AND
       b-eb.form-no NE 0
       NO-LOCK:

       v-count = v-count + 1.
       IF v-count GE 3 THEN
          LEAVE.

       IF NOT CAN-FIND(FIRST style WHERE
          style.company EQ xef.company AND
          style.style   EQ b-eb.style AND
          LOOKUP(style.TYPE,'P,R') > 0) THEN
          DO:
             v-count = 0.
             LEAVE.
          END.
   END.

   IF v-count EQ 2 THEN
      v-dep = xeb.wid.

   {cec/kmr-run.i &fil=est-op &fld=op-mr &fil2=est-op &fld2=op-speed
                 &out=vn-out}
   /* reset dimensions if corr.xgrain */
   if est-op.dept = "LM" and
      ((xef.n-out-l ne 0 and
        (xef.lam-dscr =  "R" or (xef.lam-dscr ne "R" and xef.xgrain = "S")))
       or
       (xef.n-out-l = 0 and (xef.lam-dscr ne "R" and xef.xgrain ne "S"))
      )
   then do:
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
      find first xef where
           xef.company = xest.company AND
           xef.e-num = xest.e-num
           no-lock no-error.
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
DELETE op-lock.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
