/* ------------------------------------------------- cec/uprobeit.p 10/96 JLF */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var qty as int no-undo.

def input parameter v-recid as recid.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{cec/print4.i shared shared}
{cec/print42.i shared}

DEF BUFFER b-probemk FOR reftable.

def var qm as de.
def var v-comm   like tt-tot NO-UNDO.
def var v-price  like tt-tot NO-UNDO.
def var blk-cost like blk.cost.
def var blk-fact like blk.fact.
def var v-yld as dec.
                              
DEF VAR v-tot-sell-price AS DEC NO-UNDO.
DEF VAR v-tot-full-cost AS DEC NO-UNDO.
DEF VAR v-sell-price AS DEC NO-UNDO.
DEF VAR lv-com AS DEC NO-UNDO.


find probe where recid(probe) eq v-recid.

FIND FIRST xest NO-LOCK
    WHERE xest.company EQ probe.company
      AND xest.est-no  EQ probe.est-no
    NO-ERROR.

if xest.est-type lt 2 then leave.

{ce/msfcalc.i}

{cec/combasis.i}

assign
 probe.sell-price = probe.sell-price - (probe.sell-price * v-match-up / 100)

 lv-com = v-com
                                 
 qty = probe.est-qty
 qm  = qty / 1000
 
 blk-cost = 0
 blk-fact = 0
 i        = 0.
  
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
     blk.qreq = probeit.yld-qty
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

    v-com = lv-com.

    FIND FIRST b-probemk NO-LOCK
        WHERE b-probemk.reftable EQ "ce/com/probemk.p"
          AND b-probemk.company  EQ probeit.company
          AND b-probemk.loc      EQ probeit.est-no
          AND b-probemk.code     EQ STRING(probeit.line,"9999999999")
          AND b-probemk.code2    EQ probeit.part-no
        NO-ERROR.
    IF AVAIL b-probemk THEN
      v-com = b-probemk.val[2] + b-probemk.val[3] +
              b-probemk.val[4] + b-probemk.val[5].

    ASSIGN
     v-comm = (probeit.sell-price - (if v-basis eq "G" then probeit.fact-cost else 0)) *
              (v-com / 100)
     probeit.full-cost = probeit.full-cost - v-comm.

    find first blk where blk.id eq probeit.part-no no-error.
    find first eb
        where eb.company = xest.company 
          and eb.est-no    eq xest.est-no
          and eb.form-no  eq blk.snum
          and eb.blank-no eq blk.bnum
        no-lock no-error.
    v-yld = if eb.cust-% lt 0 then -1 / eb.cust-% else eb.cust-%.

    if vmclean2 then
      assign
       v-tt-tot[blk.snum]   = probeit.full-cost * v-yld * qm
       v-ord-cost[blk.snum] = probeit.fact-cost * v-yld * qm.
    else
     assign
      blk.cost = probeit.full-cost * v-yld * qm
      blk.fact = probeit.fact-cost * v-yld * qm.
  end.
end.

for each blk:
  assign
   blk-cost = blk-cost + blk.cost
   blk-fact = blk-fact + blk.fact.
end.

ASSIGN
v-tot-sell-price = 0
v-tot-full-cost = 0.
for each blk:
  find first eb
      where eb.company  eq xest.company
        and eb.est-no   eq xest.est-no
        and eb.form-no  eq blk.snum
        and eb.blank-no eq blk.bnum
      no-lock no-error.
  v-yld = if eb.cust-% lt 0 then -1 / eb.cust-% else eb.cust-%.

  find first probeit
      where probeit.company eq probe.company
        and probeit.est-no  eq probe.est-no
        and probeit.line    eq probe.line
        and probeit.part-no eq blk.id
      no-error.
  if not avail probeit then do:
    create probeit.
    assign
     probeit.company = xest.company
     probeit.est-no  = xest.est-no
     probeit.e-num     = xest.e-num
     probeit.line      = probe.line
     probeit.part-no   = blk.id
     probeit.cust-no   = eb.cust-no
     probeit.part-no   = eb.part-no
     probeit.yrprice   = eb.yrprice
     probeit.mku_gsa-l = IF eb.pur-man THEN rm-rate-f ELSE ce-ctrl.rm-rate
     probeit.mku_gsa-m = IF eb.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate
     probeit.bl-qty    = qty
     probeit.yld-qty   = qty * v-yld.
  END.

    v-com = lv-com.

    FIND FIRST b-probemk NO-LOCK
        WHERE b-probemk.reftable EQ "ce/com/probemk.p"
          AND b-probemk.company  EQ probeit.company
          AND b-probemk.loc      EQ probeit.est-no
          AND b-probemk.code     EQ STRING(probeit.line,"9999999999")
          AND b-probemk.code2    EQ probeit.part-no
        NO-ERROR.
    IF AVAIL b-probemk THEN
       v-com = b-probemk.val[2] + b-probemk.val[3] +
               b-probemk.val[4] + b-probemk.val[5].

    v-comm = (probeit.sell-price - (if v-basis eq "G" then probeit.fact-cost else 0)) *
              (v-com / 100).

    if vmclean2 then
      assign
       probeit.full-cost = v-tt-tot[blk.snum]
       probeit.fact-cost = v-ord-cost[blk.snum].
       
    else
      assign
       probeit.full-cost  = probe.full-cost * (blk.cost / blk-cost) * qm
       probeit.fact-cost  = probe.fact-cost * (blk.fact / blk-fact) * qm.
       
    assign
     probeit.full-cost  = probeit.full-cost  / (probeit.yld-qty / 1000)
     probeit.fact-cost  = probeit.fact-cost  / (probeit.yld-qty / 1000).
   
/* 
  RUN custom/sellpric.p (ce-ctrl.sell-by,
                         v-basis,
                         probeit.fact-cost,
                         probeit.full-cost - probeit.fact-cost,
                         v-com,
                         probe.net-profit,
                         OUTPUT probeit.sell-price,
                         OUTPUT v-comm).
 */
  ASSIGN
  probeit.full-cost  = probeit.full-cost + v-comm
  v-tot-sell-price = v-tot-sell-price + probeit.sell-price
  v-tot-full-cost = v-tot-full-cost + probeit.full-cost.
 /* MESSAGE "ut2: " probeit.full-cost probeit.sell-price v-comm v-com
       VIEW-AS ALERT-BOX.
*/       
end.
/*
probe.sell-price = probe.sell-price / (1 - (v-match-up / 100)).
*/

ASSIGN
probe.sell-price = v-tot-sell-price /*+ (v-tot-sell-price * v-match-up / 100). */
probe.full-cost = v-tot-full-cost.

RUN ce/uprobit3.p (BUFFER probe).

