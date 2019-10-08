/* ------------------------------------------------ ce/box/probemk.p 7/92 cd  */

DEF INPUT PARAM v-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEF SHARED BUFFER xest FOR est.
DEF SHARED BUFFER xef  FOR ef.
DEF SHARED BUFFER xeb  FOR eb.
DEF NEW SHARED BUFFER xop FOR est-op.

{ce/print4.i SHARED SHARED}
{ce/print42.i SHARED}

DEFINE SHARED VARIABLE qty AS INTEGER NO-UNDO .

DEF BUFFER probe-ref FOR reftable.

DEF SHARED VAR v-prep-mat LIKE tprep-mat NO-UNDO.  /* for probemk cost */
DEF SHARED VAR v-prep-lab LIKE tprep-lab NO-UNDO.

DEF VAR qm AS de NO-UNDO.

DEF VAR v-comm LIKE tt-tot NO-UNDO.
DEF VAR v-royl LIKE tt-tot NO-UNDO.
DEF VAR v-ware LIKE tt-tot NO-UNDO.
DEF VAR v-cust LIKE tt-tot NO-UNDO.
DEF VAR v-nman LIKE tt-tot NO-UNDO.
DEF VAR v-price  LIKE tt-tot NO-UNDO.
DEF VAR vgsh-qty AS DEC NO-UNDO.
DEF VAR v-up     AS INT NO-UNDO.
DEF VAR v-msf    AS DEC NO-UNDO.
DEF VAR blk-cost LIKE blk.cost NO-UNDO.
DEF VAR blk-fact LIKE blk.fact NO-UNDO.
DEF VAR ld-yld AS DEC NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
DEF VAR v-yld AS DEC.
DEF VAR lv-sell-price LIKE probe.sell-price NO-UNDO.
DEF VAR ll-use-margin AS LOG NO-UNDO.

DEF NEW SHARED TEMP-TABLE tt-rel NO-UNDO LIKE eb.


{sys/inc/cerun.i F}

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

{cec/msfcalc.i}
{sys/inc/ceround.i}

ASSIGN
 qty = tt-blk
 qm  = tt-blk / 1000.

FIND probe WHERE ROWID(probe) EQ v-rowid.

FIND FIRST xeb NO-LOCK
    WHERE xeb.company  EQ xest.company
      AND xeb.est-no   EQ xest.est-no
      AND xeb.form-no  NE 0
    NO-ERROR.

ASSIGN
 probe.comm      = IF AVAIL xeb THEN xeb.comm ELSE 0
 probe.mku_gsa-l = ce-ctrl.rm-rate
 probe.mku_gsa-m = ce-ctrl.fg-rate
 probe.mku_com   = ce-ctrl.comm-mrkup
 probe.mku_whs   = ce-ctrl.whse-mrkup
 probe.est-qty   = qty
 probe.gsh-qty   = 0
 probe.tot-lbs   = 0.

IF cerunf EQ "Fibre" THEN
  RUN est/usemargin.p (ROWID(xest), OUTPUT ll-use-margin).

IF ll-use-margin THEN                 /* Get Margin% */
  RUN est/getsmanmtrx.p (ROWID(xest), "M",
                         INPUT-OUTPUT probe.comm,
                         INPUT-OUTPUT probe.market-price).

FOR EACH car:
  probe.tot-lbs = probe.tot-lbs + car.qty.
END.

FOR EACH xef NO-LOCK
    WHERE xef.company EQ xest.company
      AND xef.est-no  EQ xest.est-no:
  ASSIGN
   v-hopf = IF cerunf EQ "HOP" THEN xef.n-out ELSE 1
   v-up   = 0.

  FOR EACH xeb
      WHERE xeb.company eq xef.company
        AND xeb.est-no  eq xef.est-no
        AND xeb.form-no eq xef.form-no
      NO-LOCK:
    v-up = v-up + xeb.num-up.
  END.
  IF xest.form-qty EQ 1 THEN v-up = v-up / 2.

  FIND FIRST est-op NO-LOCK
      WHERE est-op.company EQ xest.company
        AND est-op.est-no  EQ xest.est-no
        AND est-op.s-num   EQ xef.form-no
        AND est-op.line    GE 500
      NO-ERROR.

   vgsh-qty = IF AVAIL est-op THEN est-op.num-sh
              ELSE qty / (v-up *
                          (IF xef.n-out   NE 0 THEN xef.n-out   ELSE 1) *
                          (IF xef.n-out-l NE 0 THEN xef.n-out-l ELSE 1)).
   {sys/inc/roundup.i vgsh-qty}
   ASSIGN
    vgsh-qty      = vgsh-qty * v-hopf
    probe.gsh-qty = probe.gsh-qty + vgsh-qty.
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
   v-yld            = IF xest.est-type LT 4 OR probeit.yrprice THEN probeit.yld-qty
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

ASSIGN
 probe.net-profit    = ROUND((1 - (probe.full-cost / probe.sell-price)) * 100,2)
 probe.gross-profit  = ROUND((1 - (probe.fact-cost / probe.sell-price)) * 100,2)
 probe.sell-price-wo = probe.sell-price
 probe.spare-dec-1 = dm-tot[5] / qm
 .
    
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
