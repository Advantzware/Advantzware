/* ----------------------------------------------- cec/box/probemk.p 7/92 cd  */

DEF INPUT PARAM v-rowid AS ROWID NO-UNDO.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def shared var k as int no-undo.
def shared var qty as INT NO-UNDO .

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def new shared buffer xop for est-op.

{cec/print4.i shared shared}
{cec/print42.i shared}

DEF BUFFER probe-ref FOR reftable.

def shared var v-prep-mat like tprep-mat no-undo.  /* for probemk cost */
def shared var v-prep-lab like tprep-lab no-undo.

def var qm as de NO-UNDO.

def var v-comm   like tt-tot NO-UNDO.
def var v-price  like tt-tot NO-UNDO.
def var vgsh-qty as dec NO-UNDO.
def var v-up     as int NO-UNDO.
def var v-msf    as dec NO-UNDO.
def var blk-cost like blk.cost NO-UNDO.
def var blk-fact like blk.fact NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR j AS INT NO-UNDO.
DEF VAR ld-yld AS DEC NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
def var v-yld as dec.
DEF VAR lv-sell-price LIKE probe.sell-price NO-UNDO.
DEF VAR ll-use-margin AS LOG NO-UNDO.
DEF VAR v-max-sheets AS INT NO-UNDO.
DEF VAR v-sheets AS INT NO-UNDO.

DEF NEW SHARED TEMP-TABLE tt-rel NO-UNDO LIKE reftable.

{sys/inc/cerun.i C}

{cec/msfcalc.i}
{cec/combasis.i}
{sys/inc/ceround.i}

assign
 qty = tt-blk
 qm  = tt-blk / 1000.

FIND probe WHERE ROWID(probe) EQ v-rowid.

FIND FIRST xeb NO-LOCK
    WHERE xeb.company  EQ xest.company
      AND xeb.est-no   EQ xest.est-no
      AND xeb.form-no  NE 0
    NO-ERROR.

ASSIGN
 probe.comm      = IF AVAIL xeb THEN v-com ELSE 0
 probe.freight   = rels[vmcl]          /* Holds Number of Releases */
 probe.mku_gsa-l = ce-ctrl.rm-rate
 probe.mku_gsa-m = ce-ctrl.fg-rate
 probe.mku_com   = ce-ctrl.comm-mrkup
 probe.mku_whs   = ce-ctrl.whse-mrkup
 probe.est-qty   = qty
 probe.gsh-qty   = 0
 probe.tot-lbs   = 0.

IF cerunc EQ "Fibre" THEN
  RUN est/usemargin.p (ROWID(xest), OUTPUT ll-use-margin).

IF ll-use-margin THEN                 /* Get Margin% */
  RUN est/getsmanmtrx.p (ROWID(xest), "M",
                         INPUT-OUTPUT probe.comm,
                         INPUT-OUTPUT probe.market-price).

for each xef where xef.company = xest.company and
                   xef.est-no = xest.est-no:
   v-form-no = xef.form-no.

   v-up = 0.
   for each xeb fields(num-up) where
        xeb.company = xest.company AND
        xeb.est-no   eq xef.est-no AND
        xeb.form-no eq xef.form-no
       no-lock:
      v-up = v-up + xeb.num-up.
   end.
   find first est-op
       where est-op.company = xest.company 
         and est-op.est-no eq xest.est-no
         and est-op.s-num eq xef.form-no
         and est-op.line  ge 500
       no-lock no-error.

   RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

   vgsh-qty       = IF xef.gsh-qty NE 0 THEN xef.gsh-qty
                    else
                    if avail est-op then est-op.num-sh
                    else qty / (v-up * v-n-out).

   v-sheets = 0.
   IF xef.est-type NE 6 THEN
     vgsh-qty       = IF xef.gsh-qty NE 0 THEN xef.gsh-qty
                      else
                      if avail est-op then est-op.num-sh
                      else qty / (v-up * v-n-out).

   ELSE DO:
     v-max-sheets = 0.
     FOR EACH eb WHERE eb.company = xef.company
                   AND eb.est-no  = xef.est-no
                   AND eb.form-no EQ xef.form-no
                 NO-LOCK.
      v-sheets =  probe.est-qty * (IF eb.yld-qty GT 0 THEN eb.yld-qty ELSE 
        IF eb.yld-qty EQ 0 THEN 1 ELSE (-1 / eb.yld-qty)) / eb.num-up / v-n-out.
            
        IF v-sheets GT v-max-sheets THEN
             v-max-sheets = v-sheets.
     END.

     IF v-max-sheets GT vgsh-qty THEN
       vgsh-qty = v-max-sheets.
   END.

   IF vgsh-qty = 0 THEN
       vgsh-qty       = IF xef.gsh-qty NE 0 THEN xef.gsh-qty
                        else
                        if avail est-op then est-op.num-sh
                        else qty / (v-up * v-n-out).

   {sys/inc/roundup.i vgsh-qty}
 
   assign
    probe.gsh-qty = probe.gsh-qty + vgsh-qty
    probe.tot-lbs = probe.tot-lbs +
                    if v-corr then round(((xef.gsh-len * xef.gsh-wid) * .007)
                                                                  * vgsh-qty,0)
                              else round(((xef.gsh-len * xef.gsh-wid) / 144)
                                                                  * vgsh-qty,0).
end.

ASSIGN
 probe.fact-cost  = ord-cost / qm
 probe.full-cost  = tt-tot / qm.
 
RUN cec/uprobeit.p (RECID(probe)).

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
   v-yld            = IF xest.est-type LT 7 OR probeit.yrprice THEN probeit.yld-qty
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
   ld-yld    = IF eb.yld-qty LT 0 THEN (-1 / eb.yld-qty) ELSE eb.yld-qty
   probe.bsf = probe.bsf +
               ((if v-corr then (eb.t-sqin * .007) else (eb.t-sqin / 144)) *
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

IF xest.est-type EQ 6 AND probe.set-chg NE 0 AND vmclean2 THEN
   ASSIGN
      probe.net-profit    = ROUND(((1 - (probe.full-cost / probe.sell-price)) * 100) - probe.set-chg,2)
      probe.gross-profit  = ROUND(((1 - (probe.fact-cost / probe.sell-price)) * 100) - probe.set-chg,2).
ELSE
   ASSIGN
      probe.net-profit    = ROUND((1 - (probe.full-cost / probe.sell-price)) * 100,2)
      probe.gross-profit  = ROUND((1 - (probe.fact-cost / probe.sell-price)) * 100,2).

probe.sell-price-wo = probe.sell-price.

IF NOT ll-use-margin THEN probe.market-price = probe.net-profit.

FIND FIRST probe-ref NO-LOCK
    WHERE probe-ref.reftable EQ "probe-ref"
      AND probe-ref.company  EQ probe.company
      AND probe-ref.loc      EQ ""
      AND probe-ref.code     EQ probe.est-no
      AND probe-ref.code2    EQ STRING(probe.line,"9999999999")
    NO-ERROR.
IF AVAIL probe-ref THEN
  probe.sell-price-wo = ((probe.sell-price * qm) -
                         (probe-ref.val[6] + probe-ref.val[7] +
                          probe-ref.val[8] + probe-ref.val[9])) / qm.
   
assign
 probe.mat-cost = (dm-tot[5] + mis-tot[1] + v-prep-mat) / (qty / 1000)
 probe.spare-dec-1 = dm-tot[5] / (qty / 1000)
 probe.lab-cost = (opsplit$[1] + mis-tot[3] + v-prep-lab + ctrl2[2] + ctrl2[3]) /
                  (qty / 1000)
 probe.vo-cost = opsplit$[2] / (qty / 1000)
 probe.fo-cost = opsplit$[3] / (qty / 1000).
