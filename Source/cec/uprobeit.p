/* ------------------------------------------------- cec/uprobeit.p 10/96 JLF */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def new shared var qty as int no-undo.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.

def input parameter v-recid as recid.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{cec/print4.i shared shared}
{cec/print42.i shared}

DEF BUFFER b-blk FOR blk.
DEF BUFFER b-probemk FOR reftable.
DEF BUFFER probe-ref FOR reftable.

DEF VAR qm AS DEC NO-UNDO.
DEF VAR v-comm LIKE tt-tot NO-UNDO.
DEF VAR v-royl LIKE tt-tot NO-UNDO.
DEF VAR v-ware LIKE tt-tot NO-UNDO.
DEF VAR v-cust LIKE tt-tot NO-UNDO.
DEF VAR v-nman LIKE tt-tot NO-UNDO.
def var v-price  like tt-tot NO-UNDO.
def var blk-cost like blk.cost.
def var blk-fact like blk.fact.
def var v-yld as dec.
def var v-qty as dec.
def var v-rel as dec.
DEF VAR lv-sell-by AS CHAR NO-UNDO.
DEF VAR lv-sell-by-ce-ctrl AS CHAR NO-UNDO.
DEF VAR ll-use-margin AS LOG NO-UNDO.
DEF VAR board-cst AS DEC NO-UNDO.
DEF VAR v-tmp-set-markup LIKE probe.set-chg NO-UNDO.
DEF VAR v-freight AS DEC NO-UNDO.
DEF VAR v-found AS LOG NO-UNDO.
DEF SHARED TEMP-TABLE tt-rel NO-UNDO LIKE reftable.
                              

if xest.est-type lt 6 then leave.

find probe where recid(probe) eq v-recid.

{sys/inc/cerun.i C}

{cec/combasis.i}

{cec/msfcalc.i}

{cec/rollfac.i}

ASSIGN
 qty = probe.est-qty
 qm  = qty / 1000 * v-sqft-fac
 
 blk-cost = 0
 blk-fact = 0
 i        = 0
    
 lv-sell-by = ce-ctrl.sell-by
 lv-sell-by-ce-ctrl = ce-ctrl.sell-by.

IF cerunc EQ "Fibre" THEN
  RUN est/usemargin.p (ROWID(xest), OUTPUT ll-use-margin).

find first probeit
    where probeit.company eq probe.company
      and probeit.est-no  eq probe.est-no
      and probeit.line    eq probe.line
    no-lock no-error.

if avail probeit then do:
  find first blk no-error.
  if not avail blk then
  for each eb
      where eb.company eq xest.company 
        and eb.est-no  eq xest.est-no
        and eb.form-no ne 0
      no-lock:
    create blk.
    assign
     blk.kli  = eb.cust-no 
     blk.id   = eb.part-no
     blk.snum = eb.form-no
     blk.bnum = eb.blank-no
     blk.qreq = IF eb.est-type GE 7 THEN probeit.bl-qty ELSE probeit.yld-qty
     blk.qyld = probeit.yld-qty
     blk.yr$  = eb.yrprice.
  end.

  assign
   v-tt-tot   = 0
   v-ord-cost = 0.
    
  for each probeit
      where probeit.company eq probe.company
        and probeit.est-no  eq probe.est-no
        and probeit.line    eq probe.line:

    assign
     v-comm = (probeit.sell-price - (if v-basis eq "G" then probeit.fact-cost else 0)) *
              (probe.comm / 100)
     probeit.full-cost = probeit.full-cost - v-comm.

    find first blk where blk.id eq probeit.part-no no-error.

    assign
     v-yld = IF xest.est-type LT 7 OR probeit.yrprice THEN probeit.yld-qty
                                                      ELSE probeit.bl-qty

     v-tt-tot[blk.snum]   = v-tt-tot[blk.snum] +
                            (probeit.full-cost * (v-yld / 1000))
     v-ord-cost[blk.snum] = v-ord-cost[blk.snum] +
                            (probeit.fact-cost * (v-yld / 1000))

     blk.cost = probeit.full-cost * (v-yld / 1000)
     blk.fact = probeit.fact-cost * (v-yld / 1000).
  end.
end.

for each blk:
  assign
   blk-cost = blk-cost + blk.cost
   blk-fact = blk-fact + blk.fact.
end.

IF lv-sell-by EQ "S" THEN DO:
  {cec/sqftmrkp.i probe.tot-lbs v-pct}
END.

for each blk BREAK BY blk.snum:
   IF vmclean2 AND FIRST-OF(blk.snum) THEN DO:
     ASSIGN
      blk-cost = 0
      blk-fact = 0.
  
     FOR EACH b-blk WHERE b-blk.snum EQ blk.snum:
       ASSIGN
        blk-cost = blk-cost + b-blk.cost
        blk-fact = blk-fact + b-blk.fact.
     END.
   END.
  
   find first ef
       where ef.company  eq xest.company
         and ef.est-no   eq xest.est-no
         and ef.form-no  eq blk.snum
       no-lock no-error.
  
   find first eb
       where eb.company  eq xest.company
         and eb.est-no   eq xest.est-no
         and eb.form-no  eq blk.snum
         and eb.blank-no eq blk.bnum
       no-lock no-error.
  
   v-yld = IF eb.est-type GE 7 THEN 1
           ELSE
           IF eb.yld-qty LT 0  THEN (-1 / eb.yld-qty)
                               ELSE eb.yld-qty.
  
   find first probeit
       where probeit.company eq probe.company
         and probeit.est-no  eq probe.est-no
         and probeit.line    eq probe.line
         and probeit.part-no eq blk.id
         NO-ERROR.
   if not avail probeit then do:
     create probeit.
     assign
      probeit.company   = xest.company
      probeit.est-no    = xest.est-no
      probeit.e-num     = xest.e-num
      probeit.line      = probe.line
      probeit.part-no   = blk.id
      probeit.cust-no   = eb.cust-no
      probeit.part-no   = eb.part-no
      probeit.yrprice   = eb.yrprice
      probeit.mku_gsa-l = IF eb.pur-man THEN rm-rate-f ELSE ce-ctrl.rm-rate
      probeit.mku_gsa-m = IF eb.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate
      probeit.mku_com   = probe.comm.
   END. /* new */
  
   IF eb.est-type GE 7 THEN
     ASSIGN
      probeit.bl-qty    = probeit.bl-qty + blk.qreq
      probeit.yld-qty   = probeit.yld-qty + blk.qyld
      probeit.fact-cost = probeit.fact-cost + blk.fact
      probeit.full-cost = probeit.full-cost + blk.cost.
   ELSE DO:
     ASSIGN
      probeit.bl-qty  = probeit.bl-qty + (qty * v-yld)
      probeit.yld-qty = probeit.yld-qty + (qty * v-yld)
      probeit.contrib-pct[1] = probeit.contrib-pct[1] + (qty * v-yld).
  
     IF vmclean2 THEN
        ASSIGN
           probeit.full-cost = v-tt-tot[blk.snum]   * (blk.cost / blk-cost)
           probeit.fact-cost = v-ord-cost[blk.snum] * (blk.fact / blk-fact).
     ELSE
       ASSIGN
        probeit.full-cost  = probe.full-cost * (blk.cost / blk-cost) * qm
        probeit.fact-cost  = probe.fact-cost * (blk.fact / blk-fact) * qm.
   END.
END.

FIND FIRST probe-ref
    WHERE probe-ref.reftable EQ "probe-ref"
      AND probe-ref.company  EQ probe.company
      AND probe-ref.loc      EQ ""
      AND probe-ref.code     EQ probe.est-no
      AND probe-ref.code2    EQ STRING(probe.line,"9999999999")
    NO-ERROR.
IF NOT AVAIL probe-ref THEN DO:
  CREATE probe-ref.
  ASSIGN
   probe-ref.reftable = "probe-ref"
   probe-ref.company  = probe.company
   probe-ref.loc      = ""
   probe-ref.code     = probe.est-no
   probe-ref.code2    = STRING(probe.line,"9999999999").
END.
ASSIGN
 probe-ref.val[6] = 0
 probe-ref.val[7] = 0
 probe-ref.val[8] = 0
 probe-ref.val[9] = 0.

FOR EACH probeit
    WHERE probeit.company eq probe.company
      AND probeit.est-no  eq probe.est-no
      AND probeit.line    eq probe.line:

   IF xest.est-type LT 7 OR probeit.yrprice THEN
      v-qty = IF probeit.contrib-pct[1] NE 0 THEN probeit.contrib-pct[1] ELSE probeit.yld-qty.
   ELSE
      v-qty = probeit.bl-qty.

   ASSIGN
   probeit.full-cost = probeit.full-cost / (v-qty / 1000)
   probeit.fact-cost = probeit.fact-cost / (v-qty / 1000)
   v-rel = 0.

  FOR EACH eb NO-LOCK
      WHERE eb.company EQ probeit.company
        AND eb.est-no  EQ probeit.est-no
        AND eb.part-no EQ probeit.part-no,
      FIRST tt-rel NO-LOCK
      WHERE tt-rel.reftable EQ "ce/com/selwhif1.w"
        AND tt-rel.company  EQ eb.company
        AND tt-rel.loc      EQ eb.est-no
        AND tt-rel.code     EQ STRING(eb.form-no,"9999999999")
        AND tt-rel.code2    EQ STRING(eb.blank-no,"9999999999"):
    v-rel = v-rel + tt-rel.val[1].
  END.

  FIND FIRST b-probemk
      WHERE b-probemk.reftable EQ "ce/com/probemk.p"
        AND b-probemk.company  EQ probeit.company
        AND b-probemk.loc      EQ probeit.est-no
        AND b-probemk.code     EQ STRING(probeit.line,"9999999999")
        AND b-probemk.code2    EQ probeit.part-no
      NO-ERROR.
  IF NOT AVAIL b-probemk THEN DO:
    CREATE b-probemk.
    ASSIGN
     b-probemk.reftable = "ce/com/probemk.p"
     b-probemk.company  = probeit.company
     b-probemk.loc      = probeit.est-no
     b-probemk.code     = STRING(probeit.line,"9999999999")
     b-probemk.code2    = probeit.part-no.
  END.
  b-probemk.val[1] = b-probemk.val[1] + (IF v-rel EQ 0 THEN 1 ELSE v-rel).

  IF b-probemk.val[1] GT probe.freight THEN probe.freight = b-probemk.val[1].

  RUN custom/markup.p (ROWID(eb),
                       INPUT-OUTPUT lv-sell-by,
                       INPUT-OUTPUT v-pct).

  IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN DO:
    board-cst = 0.
     
    FOR EACH blk WHERE blk.id EQ probeit.part-no,
        FIRST ef NO-LOCK
        WHERE ef.company EQ xest.company
          AND ef.est-no  EQ xest.est-no
          AND ef.form-no EQ blk.snum,
        EACH brd WHERE brd.form-no EQ ef.form-no:

      board-cst = board-cst + (brd.cost-m * blk.pct * (t-blkqty[ef.form-no] / 1000)).
    END.

    board-cst = board-cst / (v-qty / 1000).
  END.
  IF AVAIL probe THEN
    FIND FIRST est-summ
        WHERE est-summ.company EQ probe.company
          AND est-summ.est-no  EQ probe.est-no
        NO-LOCK NO-ERROR.

  v-freight = 0. v-found = FALSE.
  IF AVAIL est-summ THEN DO:
      FOR EACH est-summ
          WHERE est-summ.company EQ probe.company
            AND est-summ.est-no  EQ probe.est-no
           /* AND est-summ.e-num   EQ probe.line  */
            /*AND est-summ.part-no EQ probeit.part-no */
          USE-INDEX est-qty NO-LOCK.
        FIND FIRST eb NO-LOCK
            WHERE eb.company EQ probeit.company
              AND eb.est-no  EQ probeit.est-no
              AND eb.part-no EQ probeit.part-no
            NO-ERROR.
    
        IF AVAIL eb THEN DO:
          IF SUBSTR(est-summ.summ-tot,31) BEGINS "Freight" AND v-found  THEN DO:
            v-freight = est-summ.per-m.
            LEAVE.
          END.
    
          IF SUBSTR(est-summ.summ-tot,31) BEGINS "Total Fact" 
             AND ABS(est-summ.per-m - (eb.yld-qty * probeit.fact-cost)) < .02 THEN
            v-found = TRUE. 
        END.
    
      END.
  END.

  IF ll-use-margin THEN v-pct = probe.market-price.

  IF xest.est-type EQ 6 AND probe.set-chg NE 0 AND vmclean2 THEN
     v-tmp-set-markup = probe.set-chg.

  /*this commission logic also in oe/ordfrest.i and jc/jc-calc.p*/
  RUN custom/sellpric.p (lv-sell-by-ce-ctrl,
                         lv-sell-by,
                         v-basis,
                         (IF lv-sell-by-ce-ctrl NE "B" AND
                             lv-sell-by EQ "B" THEN board-cst
                          ELSE probeit.fact-cost),
                         (IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN 0
                          ELSE (IF lv-sell-by = "F" THEN v-freight ELSE (probeit.full-cost - probeit.fact-cost))),
                         (IF ll-use-margin OR
                             (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN 0
                          ELSE probe.comm),
                         v-pct + v-tmp-set-markup,
                         OUTPUT probeit.sell-price,
                         OUTPUT v-comm).

  IF ll-use-margin OR
     (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN
     v-comm = probeit.sell-price * probe.comm / 100.

  ASSIGN
   v-price            = v-price + (probeit.sell-price * (v-qty / 1000))
   v-nman             = v-comm
   probeit.full-cost  = probeit.full-cost + v-comm
   b-probemk.val[2]   = probe.comm
   b-probemk.val[6]   = v-comm * (v-qty / 1000)
   b-probemk.val[7]   = v-royl * (v-qty / 1000)
   b-probemk.val[8]   = v-ware * (v-qty / 1000)
   b-probemk.val[9]   = v-cust * (v-qty / 1000)
   probe-ref.val[6]   = probe-ref.val[6] + b-probemk.val[6]
   probe-ref.val[7]   = probe-ref.val[7] + b-probemk.val[7]
   probe-ref.val[8]   = probe-ref.val[8] + b-probemk.val[8]
   probe-ref.val[9]   = probe-ref.val[9] + b-probemk.val[9].
end.

ASSIGN
 probe-ref.val[2]  = probe-ref.val[6] / v-price * 10000000
 probe-ref.val[3]  = probe-ref.val[7] / v-price * 10000000
 probe-ref.val[4]  = probe-ref.val[8] / v-price * 10000000
 probe-ref.val[5]  = probe-ref.val[9] / v-price * 10000000
 probe-ref.val[10] = fac-tot2 - fac-tot.
