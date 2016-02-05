/* ---------------------------------------------------- cec/localk.p 10/94 gb */
/* recalculate values of sequenced machines.                                  */
/* -------------------------------------------------------------------------- */

def input parameter v-line as int.

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def buffer xop for est-op.
def buffer bf-eb for eb.
DEF BUFFER bf-nsh FOR ef-nsh.
DEF BUFFER op-lock FOR reftable.

def shared var qty    as INT NO-UNDO.
def shared var maxco  as int no-undo.
def shared var v-2    as logical init false.
def shared var xcal   as de no-undo.
def shared var sh-wid as de no-undo.
def shared var sh-len as de no-undo.
def shared var call_id as recid no-undo.

def new shared var v-n-out as int init 1 no-undo.

def var cumul as dec.
def var save-qty as int.
def var vn-out like xef.n-out-l init 1.
def var save_id as recid.
def var v-outw like xef.n-out.
def var v-outl like xef.n-out-l.
def var v-outf as dec.
def var v-on-f as dec.
def var v-on-l as dec.
def var sh-tmp like sh-len.
def var v-widp as log.
def var v-spo as dec extent 2.
def var spo as de.
def var v-yld as dec.
def var v-pass-colors as int.
def var fil_id as recid no-undo.
def var v-hold as int NO-UNDO.
def var v-num-up as int NO-UNDO.
def var v-est-qty like est.est-qty extent 1  NO-UNDO.
def var v-eb-qty like eb.yld-qty no-undo.
def var v-cumul like cumul.
def var v-dec as dec.
def var v-sav as dec extent 2.
def var v-blk as dec.
DEF VAR ip-rowid AS ROWID NO-UNDO.
DEF VAR ll-foam AS LOG NO-UNDO.
DEF VAR v-dep LIKE eb.dep NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.

DEF BUFFER b-eb FOR eb.

def TEMP-TABLE w-qty NO-UNDO
  field b-num  like est-op.b-num
  field num-sh as dec.

run sys/inc/numup.p (xef.company, xef.est-no, xef.form-no, output v-num-up).

RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

save-qty = qty.

RUN cec/isitfoam.p (ROWID(xef), OUTPUT ll-foam).

IF ll-foam THEN
FIND FIRST ef-nsh OF xef NO-LOCK NO-ERROR.

v-est-qty[1] = 0.
FOR EACH bf-eb
    WHERE bf-eb.company EQ xest.company
      AND bf-eb.est-no  EQ xest.est-no
      AND bf-eb.form-no EQ xef.form-no
    NO-LOCK:

  ASSIGN
   v-yld    = IF bf-eb.yld-qty LT 0 THEN -1 / bf-eb.yld-qty ELSE bf-eb.yld-qty.  
   v-eb-qty = qty * v-yld.

  CREATE w-qty.
  w-qty.b-num = bf-eb.blank-no.

  IF AVAIL ef-nsh THEN DO:
    FOR EACH bf-nsh OF xef NO-LOCK
        BREAK BY bf-nsh.sheet-no
              BY bf-nsh.pass-no DESC:
      IF FIRST-OF(bf-nsh.sheet-no) THEN cumul = 1.

      cumul = cumul * bf-nsh.n-out-l * bf-nsh.n-out-w * bf-nsh.n-out-d.

      IF LAST-OF(bf-nsh.sheet-no) THEN DO:
        cumul = cumul * v-num-up.

        IF bf-nsh.sheet-no NE 1 THEN
          ASSIGN
             cumul = cumul * bf-nsh.wid-out / xef.trim-w
             cumul = cumul * bf-nsh.len-out / xef.trim-l
             cumul = cumul * bf-nsh.dep-out / xef.trim-d.

        w-qty.num-sh = w-qty.num-sh + cumul.
      END.
    END.

    w-qty.num-sh = v-eb-qty / w-qty.num-sh.
  END.

  ELSE w-qty.num-sh = v-eb-qty / (v-num-up * v-n-out).

  {sys/inc/roundup.i w-qty.num-sh}

  v-eb-qty = w-qty.num-sh * v-num-up * v-n-out.
   
  IF v-eb-qty / bf-eb.num-up GT v-est-qty[1] THEN
    v-est-qty[1] = v-eb-qty / bf-eb.num-up.
END.

assign
 /*save-qty = qty*/
 qty      = v-est-qty[1]
 /*save-qty = qty*/
 v-outw   = xef.n-out
 v-outl   = xef.n-out-l
 vn-out   = v-outw * v-outl
 sh-wid   = xef.gsh-wid / v-outw
 sh-len   = xef.gsh-len / v-outl
 sh-tmp   = sh-len
 v-on-l   = vn-out
 v-on-f   = vn-out
 v-outf   = 0
 v-widp   = v-outl lt 2.

for each est-op
    where est-op.company = xest.company
      AND est-op.est-no eq xest.est-no
      and est-op.s-num eq xef.form-no
      and est-op.line  ge v-line
      and est-op.line  lt v-line + 500
    break by est-op.d-seq desc by est-op.line desc:

  find first xeb
      where xeb.company = est-op.company
        AND xeb.est-no    eq est-op.est-no
        and xeb.form-no   eq est-op.s-num
        and (xeb.blank-no eq est-op.b-num or est-op.b-num eq 0)
      no-lock.

  if first(est-op.d-seq) THEN
    v-yld = if xeb.yld-qty lt 0 then -1 / xeb.yld-qty else xeb.yld-qty.

  cumul = 0.

  for each w-qty
      where (w-qty.b-num eq est-op.b-num and est-op.b-num ne 0)
         or est-op.b-num eq 0:

    cumul = cumul + w-qty.num-sh.
  end.

  {cec/prokalk.i xeb}

  v-hold = 0.

  for each w-qty
      where (w-qty.b-num eq est-op.b-num and est-op.b-num ne 0)
         or est-op.b-num eq 0:

    v-hold = v-hold + w-qty.num-sh.
  end.

  for each w-qty
      where (w-qty.b-num eq est-op.b-num and est-op.b-num ne 0)
         or est-op.b-num eq 0:

    w-qty.num-sh = cumul * w-qty.num-sh / v-hold.
    {sys/inc/roundup.i w-qty.num-sh}.
  end.
end.

qty = save-qty.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
