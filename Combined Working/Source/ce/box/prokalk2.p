/* ----------------------------------------------- ce/box/prokalk2.p 10/94 gb */
/*                                                                            */
/* recalculate values of sequenced machines.                                  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared var qty as int NO-UNDO.

def new shared var call_id as recid no-undo.
def var fil_id as recid no-undo.
def var col-error as cha no-undo.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def new shared buffer xop for est-op.

{ce/print4.i shared shared}

def new shared var sav-spo  as de extent 10.
def new shared var cumul    as de.
def new shared var eb_id as recid.
def new shared var maxco as int no-undo.
def new shared var v-n-out as int init 1 no-undo.

DEFINE BUFFER b-eb3 FOR eb.

def var v-outw like xef.n-out NO-UNDO.
def var v-outl like xef.n-out-l NO-UNDO.
def var v-outf as DEC NO-UNDO.
def var vn-out as DEC NO-UNDO.
def var v-on-f as DEC NO-UNDO.
def var v-on-l as DEC NO-UNDO.
def var sh-tmp like sh-len NO-UNDO.
def var v-widp as LOG NO-UNDO.
def var v-spo as dec extent 2 NO-UNDO.
def var v-yld as DEC NO-UNDO.
def var v-pass-colors as INT NO-UNDO.
DEF VAR v-hold AS INT NO-UNDO.
DEF VAR v-num-up AS INT NO-UNDO.
DEF VAR v-est-qty AS DEC NO-UNDO.
DEF VAR v-eb-qty AS DEC NO-UNDO.
def var v-cumul like cumul NO-UNDO.
def var v-dec as DEC NO-UNDO.
def var v-sav as dec extent 2 NO-UNDO.
def var v-blk as DEC NO-UNDO.
DEF VAR ll-no-more-blank-fed AS LOG NO-UNDO.
DEF VAR ip-rowid AS ROWID NO-UNDO.
DEF VAR v-num-inks AS INT NO-UNDO.
DEF VAR v-index AS INT NO-UNDO.
DEF VAR v-program-6 AS CHAR NO-UNDO.

DEF TEMP-TABLE w-qty NO-UNDO
  FIELD b-num  LIKE est-op.b-num
  FIELD num-up LIKE eb.num-up
  FIELD num-bl AS DEC
  FIELD sav-bl AS DEC
  FIELD spo-bl AS DEC.

{ce/mach-ink.i NEW}


RUN ce/mach-ink.p.

ASSIGN
 spo      = 0
 save-qty = qty
 v-outw   = xef.n-out
 v-outl   = xef.n-out-l
 vn-out   = v-outw * v-outl.

RUN sys/inc/numup.p (xef.company, xef.est-no, xef.form-no, OUTPUT v-num-up).

v-est-qty = 0.
FOR EACH eb
    WHERE eb.company EQ xest.company
      AND eb.est-no  EQ xest.est-no
      AND eb.form-no EQ xef.form-no
    NO-LOCK:
    
  ASSIGN
   v-yld    = IF eb.cust-% lt 0 then -1 / eb.cust-% else eb.cust-%
   v-eb-qty = tt-blk * v-yld.

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
 v-widp = v-outl lt 2
 r-spo[xef.form-no] = 0.

for each est-op where est-op.company = xest.company and
                      est-op.est-no = xest.est-no and
                      est-op.s-num = xef.form-no and
                      est-op.line gt 500
    break by est-op.b-num desc by est-op.d-seq desc by line desc:

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

for each est-op
    where est-op.company = xest.company
      AND est-op.est-no eq xest.est-no
      and est-op.s-num eq xef.form-no
      and est-op.line  ge 500
    no-lock
    by est-op.line:
  cumul = est-op.num-sh.
  leave.
end.

fil_id = recid(xef).
find xef where recid(xef) eq fil_id no-error.
ASSIGN
xef.gsh-qty = cumul
qty = save-qty
r-spo[xef.form-no] = xef.gsh-qty - t-shtfrm[xef.form-no] - spo.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
