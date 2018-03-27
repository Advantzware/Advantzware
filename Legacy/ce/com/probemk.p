/* ------------------------------------------------ ce/com/probemk.p 7/92 cd  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM v-rowid AS ROWID NO-UNDO.

{sys/inc/var.i shared}

def new shared buffer xop for est-op.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

DEF BUFFER bf-eb FOR eb.

{ce/print4.i shared shared}
{ce/print42.i shared}

DEF BUFFER probe-ref FOR reftable.

def shared var v-prep-mat like tprep-mat no-undo.  /* for probemk cost */
def shared var v-prep-lab like tprep-lab no-undo.
def shared var qty as INT NO-UNDO.

def var qm as dec.
def var vgsh-qty as dec NO-UNDO.
def var v-up as int NO-UNDO.
DEF VAR ld-yld AS DEC NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
def var v-comm like tt-tot NO-UNDO.
def var v-yld as dec NO-UNDO.
def var v-prf-s as dec NO-UNDO.
def var v-pct-s as dec NO-UNDO.
DEF VAR lv-sell-price LIKE probe.sell-price NO-UNDO.
def var v-avg-tan as log NO-UNDO.
DEF VAR ll-tandem AS LOG NO-UNDO.
DEF VAR ll-use-margin AS LOG NO-UNDO.


find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

{ce/msfcalc.i}
{cec/combasis.i}
{sys/inc/ceround.i}
{sys/inc/cerun.i F}

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "COMBCOST"
    no-lock no-error.
v-avg-tan = avail sys-ctrl and sys-ctrl.int-fld eq 2.

find probe where rowid(probe) eq v-rowid.
qm = probe.est-qty / 1000.

FIND FIRST xeb NO-LOCK
    WHERE xeb.company  EQ xest.company
      AND xeb.est-no   EQ xest.est-no
      AND xeb.form-no  NE 0
    NO-ERROR.

ASSIGN
 probe.comm      = IF AVAIL xeb THEN xeb.comm ELSE 0
 probe.freight   = 1          /* Holds Number of Releases */
 probe.mku_gsa-l = ce-ctrl.rm-rate
 probe.mku_gsa-m = ce-ctrl.fg-rate
 probe.mku_com   = ce-ctrl.comm-mrkup
 probe.mku_whs   = ce-ctrl.whse-mrkup
 probe.gsh-qty   = 0
 probe.tot-lbs   = 0.

IF cerunf EQ "Fibre" THEN
  RUN est/usemargin.p (ROWID(xest), OUTPUT ll-use-margin).

IF ll-use-margin THEN                 /* Get Margin% */
DO:
  RUN est/getsmanmtrx.p (ROWID(xest), "M",
                         INPUT-OUTPUT probe.comm,
                         INPUT-OUTPUT probe.market-price).
  v-com = probe.comm.
END.

FOR EACH car:
  probe.tot-lbs = probe.tot-lbs + car.qty.
END.

RUN ce/com/istandem.p (ROWID(xest), OUTPUT ll-tandem).

FOR EACH xef
    WHERE xef.company EQ xest.company
      AND xef.est-no  EQ xest.est-no:

   ASSIGN
     v-form-no = xef.form-no
     v-up = 0.

   FOR EACH xeb FIELDS(num-up)
       WHERE xeb.company EQ xest.company 
         AND xeb.est-no  EQ xef.est-no
         AND xeb.form-no EQ xef.form-no
       NO-LOCK:
      v-up = v-up + xeb.num-up.
   END.
   FIND FIRST est-op
       WHERE est-op.company EQ xest.company 
         AND est-op.est-no  EQ xest.est-no
         AND est-op.s-num   EQ xef.form-no
         AND est-op.line    GE 500
       NO-LOCK NO-ERROR.

   RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

   vgsh-qty = IF AVAIL est-op THEN est-op.num-sh
              ELSE IF xef.gsh-qty NE 0 THEN xef.gsh-qty
              ELSE
              qty / (v-up * v-n-out).
   {sys/inc/roundup.i vgsh-qty}
   ASSIGN 
    v-hopf        = IF cerunf EQ "HOP" THEN xef.n-out ELSE 1
    probe.gsh-qty = probe.gsh-qty + (vgsh-qty * v-hopf).
END.

ASSIGN
 probe.fact-cost  = fac-tot / qm
 probe.full-cost  = tt-tot / qm.
 
RUN ce/uprobeit.p (RECID(probe)).

ASSIGN
 probe.fact-cost  = 0
 probe.full-cost  = 0
 probe.sell-price = 0.

FOR EACH probeit
    WHERE probeit.company EQ probe.company
      AND probeit.est-no  EQ probe.est-no
      AND probeit.line    EQ probe.line
    NO-LOCK:

  ASSIGN
   v-yld            = IF xest.est-type LT 3 OR probeit.yrprice THEN probeit.yld-qty
                                                               ELSE probeit.bl-qty
   probe.fact-cost  = probe.fact-cost  +
                      (probeit.fact-cost  * v-yld / 1000)
   probe.full-cost  = probe.full-cost  +
                      (probeit.full-cost  * v-yld / 1000)
   probe.sell-price = probe.sell-price +
                      (probeit.sell-price * v-yld / 1000).
END.

ASSIGN
 probe.fact-cost  = probe.fact-cost / qm
 probe.full-cost  = probe.full-cost / qm
 probe.sell-price = probe.sell-price / qm
 probe.prof-on    = "Net"
 probe.bsf        = 0.

IF INDEX("BSG",ce-ctrl.sell-by) GT 0 THEN
  probe.prof-on = ENTRY(INDEX("BSG",ce-ctrl.sell-by),"Cust,MSF,Gross").

for each blk:
  find first eb where eb.company = xest.company and
                      eb.est-no    = xest.est-no and
                      eb.form-no  = blk.snum   and
                      eb.blank-no = blk.bnum no-lock no-error.
  ASSIGN
   ld-yld    = IF eb.est-type GE 3 THEN 1
               ELSE
               IF eb.quantityPerSet LT 0  THEN (-1 / eb.quantityPerSet)
                                   ELSE eb.quantityPerSet
   probe.bsf = probe.bsf +
               ((IF v-corr THEN (eb.t-sqin * .007) ELSE (eb.t-sqin / 144)) *
                ld-yld).
end.

IF v-round NE "PENNY" THEN DO:
  IF v-round EQ "DOLLAR" THEN DO:
    lv-sell-price = probe.sell-price.
    {sys/inc/roundup.i probe.sell-price}
    FOR EACH probeit
        WHERE probeit.company EQ probe.company
          AND probeit.est-no  EQ probe.est-no
          AND probeit.line    EQ probe.line:
       probeit.sell-price = ROUND(probeit.sell-price * (probe.sell-price / lv-sell-price),2).
    END.
  END.
END.

IF /*ll-tandem AND*/ v-avg-tan THEN
FOR EACH probeit
    WHERE probeit.company EQ probe.company
      AND probeit.est-no  EQ probe.est-no
      AND probeit.line    EQ probe.line:

  probeit.sell-price = probe.sell-price.
END.

assign
 probe.net-profit    = round((1 - (probe.full-cost / probe.sell-price)) * 100,2)
 probe.gross-profit  = round((1 - (probe.fact-cost / probe.sell-price)) * 100,2)
 probe.sell-price-wo = probe.sell-price.
    
IF NOT ll-use-margin THEN probe.market-price = probe.net-profit + probe.comm.
   
ASSIGN
 probe.mat-cost = (dm-tot[5] + mis-tot[1] + v-prep-mat) / (qty / 1000)
 probe.spare-dec-1   = dm-tot[5] / (qty / 1000)  /*24941 - Mat % Ex Mis and Prep*/
 probe.lab-cost = (opsplit$[1] + mis-tot[3] + v-prep-lab + ctrl2[2] + ctrl2[3]) /
                  (qty / 1000)
 probe.vo-cost = opsplit$[2] / (qty / 1000)
 probe.fo-cost = opsplit$[3] / (qty / 1000).

if opsys eq "unix" then
  unix silent copy value(outfile1) value(outfile3).
else
  dos silent copy value(outfile1) value(outfile3).

output to value(outfile3) append.

ASSIGN
 v-prf-s = probe.sell-price - probe.fact-cost
 v-pct-s = v-prf-s / probe.fact-cost * 100.

{ce/probepr2.i 48}

output close.
      
IF AVAIL probe-ref THEN
  probe.sell-price-wo = ((probe.sell-price * qm) -
                         (probe-ref.val[6] + probe-ref.val[7] +
                          probe-ref.val[8] + probe-ref.val[9])) / qm.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
