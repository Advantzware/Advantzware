/* --------------------------------------------------- cec/prokalk.p 10/94 gb */
/* recalculate values of sequenced machines.                                  */
/* -------------------------------------------------------------------------- */

{cec/print4.i shared shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def buffer xop for est-op.

def new shared var x as int no-undo.
def new shared var y as int no-undo.
def new shared var call_id as recid no-undo.
def new shared var maxco as int no-undo.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def shared var qty as int NO-UNDO.
def shared var k as int no-undo.

def var i as int no-undo.
def var fil_id as recid no-undo.
def var j as int no-undo.
def var xxx as int no-undo.
def var zzz as int no-undo.
def var col-error as cha no-undo.
def var tmpstore as cha no-undo.
def var cumul as dec.
def var vn-out like xef.n-out.
def var v-outw like xef.n-out.
def var v-outl like xef.n-out-l.
def var v-outf as dec.
def var v-on-f as dec.
def var v-on-l as dec.
def var sh-tmp like sh-len.
def var v-widp as log.
def var v-spo as dec extent 2.
def var v-pass-colors as int.
DEF VAR v-num-up AS INT NO-UNDO.
def var v-cumul like cumul.
def var v-dec as dec.
def var v-sav as dec extent 2.
def var v-blk as dec.
DEF VAR ip-rowid AS ROWID NO-UNDO.
DEF VAR ll-foam AS LOG NO-UNDO.
DEF VAR hld-n-out AS INT NO-UNDO.
DEF VAR hld-shqty AS INT NO-UNDO.
DEF VAR v-dep LIKE eb.dep NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VARIABLE dOutDivisor AS DECIMAL NO-UNDO INIT 1. /*For cumulative Out Divisor 19774*/

DEF BUFFER b-eb FOR eb.

run sys/inc/numup.p (xef.company, xef.est-no, xef.form-no, output v-num-up).

ASSIGN
 v-outw   = xef.n-out
 v-outl   = xef.n-out-l
 vn-out   = v-outw * v-outl.

IF AVAIL xef THEN 
  RUN est/ef-#out.p (ROWID(xef), OUTPUT vn-out).

assign
 spo      = 0
 r-spo    = 0
 save-qty = qty
 cumul    = qty / v-num-up
 sh-wid   = xef.gsh-wid / v-outw
 sh-len   = xef.gsh-len / v-outl
 sh-tmp   = sh-len
 v-on-l   = vn-out
 v-on-f   = vn-out
 v-outf   = 0
 v-widp   = v-outl lt 2
 cumul    = cumul / vn-out.

RUN cec/isitfoam.p (ROWID(xef), OUTPUT ll-foam).

IF ll-foam THEN
FIND FIRST ef-nsh OF xef NO-LOCK NO-ERROR.

IF AVAIL ef-nsh THEN DO:
  RUN est/ef-#out.p (ROWID(xef), OUTPUT vn-out).

  RUN cec/foamblks.p (ROWID(xef), OUTPUT cumul).

  cumul = qty / cumul.
END.

{sys/inc/roundup.i cumul}

ASSIGN
 hld-n-out = vn-out
 hld-shqty = cumul.

for each est-op
    where est-op.company eq xest.company
      and est-op.est-no  eq xest.est-no
      and est-op.qty     eq v-op-qty 
      and est-op.line    ge 500
    break by est-op.d-seq desc by line desc:
   
   /*19774 - accumulate the Out Divisor for use in final waste calc*/
   IF est-op.n_out_div GT 1 THEN 
        dOutDivisor = dOutDivisor * est-op.n_out_div.
        
  {cec/prokalk.i}
end.
r-spo[1] = cumul - (spo + hld-shqty * dOutDivisor).

IF r-spo[1] LT 0 THEN r-spo[1] = 0.

spo = cumul - (hld-shqty * dOutDivisor + r-spo[1]).

IF spo LT 0 THEN spo = 0.

ASSIGN
 qty      = save-qty
 fil_id   = recid(xef).

find xef where recid(xef) = fil_id no-error.
xef.gsh-qty = cumul.
find xef where recid(xef) = fil_id no-lock no-error.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
