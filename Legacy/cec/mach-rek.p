/* -------------------------------------------------- cec/mach-rek.p 1/92 cd  */
/* recalculate values of sequenced machines.                                  */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def buffer xop for est-op.

{cec/print4.i "new shared" "new shared"}

def new shared var qty as INT NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.
def shared var fil_id as recid no-undo.
def new shared var call_id as recid no-undo.
def new shared var maxco as int no-undo.

def shared var chosen as log format "y/n" no-undo.
def shared var head as ch format "x(78)" extent 20 no-undo.

def var save_id as recid no-undo.
def var mess as ch format "x(80)" extent 2 no-undo.
def var cumul as dec no-undo.
def var vn-out like xef.n-out no-undo.
def var v-outw like xef.n-out no-undo.
def var v-outl like xef.n-out-l no-undo.
def var v-outf as dec no-undo.
def var v-on-f as dec no-undo.
def var v-on-l as dec no-undo.
def var sh-tmp like sh-len no-undo.
def var v-widp as log no-undo.
def var v-spo as dec extent 2.
def var v-line like est-op.line no-undo.
def var v-pass-colors as int no-undo.
DEF VAR v-num-up AS INT NO-UNDO.
def var v-cumul like cumul.
def var v-dec as dec.
def var v-sav as dec extent 2.
def var v-blk as dec.
DEF VAR ll-foam AS LOG NO-UNDO.
DEF VAR v-dep LIKE eb.dep NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.

DEF BUFFER b-eb FOR eb.

find est-op where recid(est-op) eq fil_id no-error.

IF xest.est-type EQ 8 THEN RUN cec/com/localk.p (0, ip-rowid, NO).

ELSE DO:

ASSIGN
   qty = xest.est-qty[1]
   save-qty = qty
   v-op-qty = est-op.qty.

IF xest.est-type EQ 6 THEN RUN cec/box/prokalk2.p (0, ip-rowid).

ELSE DO:
find first xef  where xef.company = est-op.company 
                  and xef.est-no  eq est-op.est-no
                  and xef.form-no eq est-op.s-num
                  no-lock.
vn-out = xef.n-out * xef.n-out-l * xef.n-out-d.

run sys/inc/numup.p (xef.company, xef.est-no, xef.form-no, output v-num-up).

find first xeb where  xeb.company = est-op.company 
                  and xeb.est-no  eq est-op.est-no
                  and xeb.form-no eq est-op.s-num
                  no-lock.
assign
 save-qty = qty
 cumul    = v-op-qty  / xeb.num-up
 v-outw   = xef.n-out
 v-outl   = xef.n-out-l
 vn-out   = v-outw * v-outl
 sh-wid   = xef.gsh-wid / v-outw
 sh-len   = xef.gsh-len / v-outl
 sh-tmp   = sh-len
 v-on-l   = vn-out
 v-on-f   = vn-out
 v-outf   = 0
 v-widp   = v-outl lt 2
 cumul    = cumul / vn-out.

RELEASE est-op.

for each est-op
    where est-op.company eq xest.company 
      and est-op.est-no  eq xest.est-no
      and est-op.qty     eq v-op-qty
      and est-op.s-num   eq xef.form-no
      and est-op.line    lt 500
    break by est-op.d-seq desc by line desc:
  {cec/prokalk.i}
end.

END. /* else est-type 6 */

END. /* else est-type 8 */

find est-op where recid(est-op) = fil_id no-error.
qty = save-qty.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
