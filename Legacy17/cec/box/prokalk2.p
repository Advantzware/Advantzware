/* ---------------------------------------------- cec/box/prokalk2.p 10/94 gb */
/*                                                                            */
/* recalculate values of sequenced machines.                                  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-line  AS INT   NO-UNDO.
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def shared var k as int no-undo.
def shared var qty as int NO-UNDO.

def var i as int no-undo.
def new shared var x as int no-undo.
def new shared var y as int no-undo.
def new shared var call_id as recid no-undo.
def var fil_id as recid no-undo.
def var j as int no-undo.
def var xxx as int no-undo.
def var zzz as int no-undo.
def var col-error as cha no-undo.
def var tmpstore as cha no-undo.

DEF BUFFER b-eb FOR eb.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def new shared buffer xop for est-op.

{cec/print4.i shared shared}

def new shared var sav-spo  as de extent 10.
def new shared var cumul    as de.
def new shared var eb_id as recid.
def new shared var maxco as int no-undo.
def new shared var v-n-out as int init 1 no-undo.

def var v-outw like xef.n-out.
def var v-outl like xef.n-out-l.
def var v-outf as dec.
def var vn-out as dec.
def var v-on-f as dec.
def var v-on-l as dec.
def var sh-tmp like sh-len.
def var v-widp as log.
def var v-spo as dec extent 2.
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
DEF VAR ll-foam AS LOG NO-UNDO.
DEF VAR v-dep LIKE eb.dep NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VARIABLE dOutDivisor AS DECIMAL NO-UNDO INIT 1. /*For cumulative Out Divisor 19774*/

DEF TEMP-TABLE w-qty NO-UNDO
  FIELD b-num   LIKE est-op.b-num
  FIELD num-up  LIKE eb.num-up
  FIELD num-bl  AS DEC
  FIELD sav-bl  AS DEC
  FIELD spo-bl  AS DEC.

DEF BUFFER b-w-qty FOR w-qty.


ASSIGN
 spo      = 0
 save-qty = qty
 cumul    = qty
 v-num-up = 1
 vn-out   = 1.

RUN assembly-mach.

ASSIGN
 spo    = 0
 v-blk  = 0
 v-sav  = 0
 v-outw = xef.n-out
 v-outl = xef.n-out-l
 vn-out = v-outw * v-outl.

RUN sys/inc/numup.p (xef.company, xef.est-no, xef.form-no, OUTPUT v-num-up).

RUN cec/isitfoam.p (ROWID(xef), OUTPUT ll-foam).

IF ll-foam THEN
FIND FIRST ef-nsh OF xef NO-LOCK NO-ERROR.

IF AVAIL ef-nsh THEN
RUN est/ef-#out.p (ROWID(xef), OUTPUT vn-out).

v-est-qty = 0.
FOR EACH eb NO-LOCK
    WHERE eb.company EQ xest.company
      AND eb.est-no  EQ xest.est-no
      AND eb.form-no EQ xef.form-no:

  ASSIGN
   v-yld    = IF eb.yld-qty LT 0 THEN -1 / eb.yld-qty ELSE eb.yld-qty
   v-eb-qty = qty * v-yld.

  i = 1.
  FOR EACH b-eb NO-LOCK
      WHERE b-eb.company EQ eb.company
        AND b-eb.est-no  EQ eb.est-no
        AND b-eb.form-no NE 0
        AND b-eb.form-no NE eb.form-no
        AND b-eb.part-no EQ eb.part-no:
    i = i + 1.
  END.
  v-eb-qty = v-eb-qty / i.

  {sys/inc/roundup.i v-eb-qty}

  CREATE w-qty.
  ASSIGN
   w-qty.b-num  = eb.blank-no
   w-qty.num-up = eb.num-up.

  IF AVAIL ef-nsh THEN DO:
    RUN cec/foamblks.p (ROWID(xef), OUTPUT v-num-up).

    ASSIGN
     w-qty.num-up          = v-num-up
     t-shtfrm[xef.form-no] = v-eb-qty / w-qty.num-up
     vn-out                = 1.

    {sys/inc/roundup.i t-shtfrm[xef.form-no]}
  END.

  w-qty.num-bl = v-eb-qty.

  IF v-eb-qty / w-qty.num-up GT v-est-qty THEN
    v-est-qty = v-eb-qty / w-qty.num-up.
END.

ASSIGN
 sh-wid = xef.gsh-wid / v-outw
 sh-len = xef.gsh-len / v-outl
 sh-tmp = sh-len
 v-on-l = vn-out
 v-on-f = vn-out
 v-outf = 0
 v-widp = v-outl lt 2

 r-spo[xef.form-no] = 0
 cumul              = 0
 v-cumul            = 0.

RUN non-assembly-mach.

FOR EACH est-op
    WHERE est-op.company EQ xest.company
      AND est-op.est-no  EQ xest.est-no
      AND est-op.qty     EQ v-op-qty
      AND est-op.s-num   EQ xef.form-no
      AND est-op.line    GE ip-line
      AND est-op.line    LT ip-line + 500
    NO-LOCK
    BY est-op.line:
  cumul = est-op.num-sh.
  LEAVE.
END.

fil_id = RECID(xef).
FIND xef WHERE RECID(xef) eq fil_id NO-ERROR.
xef.gsh-qty = cumul.
qty = save-qty.

      /*spo = 0.
      for each est-op where est-op.company = xest.company
                        aND est-op.est-no = xest.est-no
                        AND est-op.line > 500          and
                        est-op.s-num = xef.form-no and
                        est-op.b-num = 0 NO-LOCK:
         run sys/inc/numout.p (recid(est-op), output vn-out).
         spo = spo + (est-op.op-waste / vn-out).
      end.
      for each est-op where est-op.company = xest.company
                        aND est-op.est-no = xest.est-no
                        AND est-op.line > 500          and
                        est-op.s-num = xef.form-no and
                        est-op.b-num ne 0 no-lock:
         run sys/inc/numout.p (recid(est-op), output vn-out).
         spo = spo + (est-op.op-waste / (v-num-up * vn-out)).
      end.
      {sys/inc/roundup.i spo}*/

      r-spo[xef.form-no] = xef.gsh-qty - t-shtfrm[xef.form-no] * dOutDivisor - spo.
      dOutDivisor = 1.
/* end ---------------------------------- copr. 1992  advanced software, inc. */

RETURN.

PROCEDURE assembly-mach:

FOR EACH est-op
    WHERE est-op.company EQ xest.company
      AND est-op.est-no  EQ xest.est-no
      AND est-op.qty     EQ v-op-qty
      AND est-op.s-num   EQ xef.form-no
      AND est-op.line    GE ip-line
      AND est-op.line    LT ip-line + 500
      AND CAN-FIND(FIRST mach
                   {sys/look/machW.i}
                     AND mach.m-code EQ est-op.m-code
                     AND INDEX("AP",mach.p-type) GT 0)
    BREAK BY est-op.b-num DESC
          BY est-op.d-seq DESC
          BY est-op.line  DESC:

  FIND FIRST xeb
      WHERE xeb.company   EQ est-op.company
        AND xeb.est-no    EQ est-op.est-no
        AND xeb.form-no   EQ est-op.s-num
        AND (xeb.blank-no EQ est-op.b-num OR est-op.b-num EQ 0)
      NO-LOCK.
       
   /*19774 - accumulate the Out Divisor for use in final waste calc*/
   IF est-op.n_out_div GT 1 THEN 
        dOutDivisor = dOutDivisor * est-op.n_out_div.
        
  {cec/prokalk.i}
END.

END PROCEDURE.

PROCEDURE non-assembly-mach:

FOR EACH est-op
    WHERE est-op.company EQ xest.company
      AND est-op.est-no  EQ xest.est-no
      AND est-op.qty     EQ v-op-qty
      AND est-op.s-num   EQ xef.form-no
      AND est-op.line    GE ip-line
      AND est-op.line    LT ip-line + 500
      AND CAN-FIND(FIRST mach
                   {sys/look/machW.i}
                     AND mach.m-code EQ est-op.m-code
                     AND INDEX("AP",mach.p-type) LE 0)
    BREAK BY est-op.b-num DESC
          BY est-op.d-seq DESC
          BY est-op.line  DESC:

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
   
   /*19774 - accumulate the Out Divisor for use in final waste calc*/
   IF est-op.n_out_div GT 1 THEN 
        dOutDivisor = dOutDivisor * est-op.n_out_div.
        
  {cec/prokalk.i}

  ASSIGN
   w-qty.num-bl = v-blk
   w-qty.sav-bl = v-sav[2]
   w-qty.spo-bl = spo.

  {sys/inc/roundup.i w-qty.num-bl}

  IF est-op.b-num NE 0 THEN spo = 0.
END.

END PROCEDURE.

