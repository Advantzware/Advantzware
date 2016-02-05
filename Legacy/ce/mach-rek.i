
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def buffer xest-op for est-op.

{ce/print4.i "new shared" "new shared"}

def new shared var maxco as int no-undo.
def new shared var v-n-out as int init 1 no-undo.

DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR CALL_id AS RECID NO-UNDO.
DEF SHARED VAR fil_id AS RECID NO-UNDO.
    
def var save_id as recid.
def var mess as ch format "x(80)" extent 2.
def var cumul as dec.
def var v-rc-line as int init 999 no-undo.
def var v-outw like xef.n-out.
def var v-outl like xef.n-out-l.
def var v-outf as dec.
def var vn-out as dec.
def var v-on-f as dec.
def var v-on-l as dec.
def var sh-tmp like sh-len.
def var v-widp as log.
def var v-spo as DEC EXTENT 2.
def var v-yld as dec.
def var v-pass-colors as int.
DEF VAR v-hold AS INT NO-UNDO.
DEF VAR v-num-up AS INT NO-UNDO.
DEF VAR v-est-qty AS DEC NO-UNDO.
DEF VAR v-eb-qty AS DEC NO-UNDO.
def var v-cumul like cumul.
def var v-dec as dec.
def var v-sav as dec extent 2.
def var v-blk as dec.
DEF VAR ll-no-more-blank-fed AS LOG NO-UNDO.

DEF WORKFILE w-qty
  FIELD b-num  LIKE est-op.b-num
  FIELD num-up LIKE eb.num-up
  FIELD num-bl AS DEC
  FIELD sav-bl AS DEC
  FIELD spo-bl AS DEC.

{ce/mach-ink.i new}


find est-op where recid(est-op) eq fil_id no-error.

IF xest.est-type NE 1 THEN RUN ce/com/localk.p (0, ip-rowid).

ELSE DO:
run ce/mach-ink.p.
save-qty = qty.
v-op-qty = est-op.qty.

save_id = recid(xef).
find xef where recid(xef) = save_id no-error.

ASSIGN
 v-outw   = xef.n-out
 v-outl   = xef.n-out-l
 vn-out   = v-outw * v-outl.

find first xef
    WHERE xef.company EQ est-op.company
      AND xef.est-no  eq est-op.est-no
      and xef.form-no eq est-op.s-num
    no-lock.

run sys/inc/numup.p (xef.company,xef.est-no, xef.form-no, output v-num-up).

find first xeb
    WHERE xeb.company EQ est-op.company
      AND xeb.est-no  eq est-op.est-no
      and xeb.form-no eq est-op.s-num
    no-lock.
    
ASSIGN
 v-n-out  = (if xef.n-out   eq 0 then 1 else xef.n-out) *
            (if xef.n-out-l eq 0 then 1 else xef.n-out-l)
 cumul    = v-op-qty / v-num-up / v-n-out.

v-est-qty = 0.
FOR EACH eb
    WHERE eb.company EQ xest.company
      AND eb.est-no  EQ xest.est-no
      AND eb.form-no EQ xef.form-no
    NO-LOCK:
    
  IF xest.est-type EQ 1 THEN v-eb-qty = v-op-qty.

  ELSE
    ASSIGN
     v-yld    = IF eb.cust-% lt 0 then -1 / eb.cust-% else eb.cust-%
     v-eb-qty = xest.est-qty[1] * v-yld.

  {sys/inc/roundup.i v-eb-qty}

  CREATE w-qty.
  ASSIGN
   w-qty.b-num  = eb.blank-no
   w-qty.num-up = eb.num-up
   w-qty.num-bl = v-eb-qty.

  IF v-eb-qty / eb.num-up GT v-est-qty THEN
    v-est-qty = v-eb-qty / eb.num-up.
END.

ASSIGN
 sh-wid = xef.gsh-wid / v-outw
 sh-len = xef.gsh-len / v-outl
 sh-tmp = sh-len
 v-on-l = vn-out
 v-on-f = vn-out
 v-outf = 0
 v-widp = v-outl lt 2.

 r-spo[xef.form-no] = 0.

for each est-op
    WHERE xef.company  EQ est-op.company
      AND xef.est-no   EQ est-op.est-no
      and est-op.qty   eq v-op-qty
      and est-op.s-num eq xef.form-no
      and est-op.line  lt 500
    break by est-op.d-seq desc by line desc:

  FIND FIRST xeb
      WHERE xeb.company   EQ est-op.company
        AND xeb.est-no    EQ est-op.est-no
        AND xeb.form-no   EQ est-op.s-num
        AND (xeb.blank-no EQ est-op.b-num OR est-op.b-num EQ 0)
      NO-LOCK.

  IF est-op.b-num EQ 0 AND NOT ll-no-more-blank-fed THEN DO:
    FOR EACH w-qty BREAK BY w-qty.num-bl / w-qty.num-up DESC:
      IF FIRST(w-qty.num-bl / w-qty.num-up) THEN
        ASSIGN
         w-qty.b-num  = 0
         w-qty.num-bl = w-qty.num-bl / w-qty.num-up * v-num-up.

      ELSE DELETE w-qty.
    END.

    ll-no-more-blank-fed = YES.
  END.

  FIND FIRST w-qty WHERE w-qty.b-num EQ est-op.b-num.
  ASSIGN
   v-blk    = w-qty.num-bl
   v-sav[2] = w-qty.sav-bl
   spo      = w-qty.spo-bl.

  IF FIRST(est-op.d-seq) THEN cumul = v-blk / (v-num-up * vn-out).

  {ce/box/prokalk.i}
      
  ASSIGN
   w-qty.num-bl = v-blk
   w-qty.sav-bl = v-sav[2]
   w-qty.spo-bl = spo.

  {sys/inc/roundup.i w-qty.num-bl}

  IF est-op.b-num NE 0 THEN spo = 0.
end.

END. /* else */

find est-op where recid(est-op) = fil_id no-lock no-error.
find xef where recid(xef) = save_id no-lock no-error.
qty = save-qty.
