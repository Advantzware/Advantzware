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

def var qm as de.
def var v-comm   like tt-tot NO-UNDO.
def var v-price  like tt-tot NO-UNDO.
def var blk-cost like blk.cost.
def var blk-fact like blk.fact.
def var v-yld as dec.
                              
DEF VAR v-tot-sell-price AS DEC NO-UNDO.
DEF VAR v-tot-full-cost AS DEC NO-UNDO.
DEF VAR v-sell-price AS DEC NO-UNDO.

DEF TEMP-TABLE tt-temp NO-UNDO
FIELD b-num AS INT 
FIELD s-num AS INT
FIELD v-temp-tot AS DEC EXTENT 1000 
FIELD v-temp-ord AS DEC EXTENT 1000
INDEX b-num b-num s-num.


if xest.est-type lt 6 then leave.

find probe where recid(probe) eq v-recid.

{cec/combasis.i}

{cec/msfcalc.i}

{cec/rollfac.i}

assign
 probe.sell-price = probe.sell-price - (probe.sell-price * v-match-up / 100)
                                 
 qty = probe.est-qty
 qm  = qty / 1000 * v-sqft-fac
 
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
   
   
  FOR EACH tt-temp:
      tt-temp.v-temp-tot = 0 .
      tt-temp.v-temp-ord = 0.
  END.

  for each probeit
      where probeit.company eq probe.company
        and probeit.est-no  eq probe.est-no
        and probeit.line    eq probe.line:

    assign
     v-comm = (probeit.sell-price - (if v-basis eq "G" then probeit.fact-cost else 0)) *
              (v-com / 100)
     probeit.full-cost = probeit.full-cost - v-comm.

    find first blk where blk.id eq probeit.part-no no-error.

    FIND FIRST tt-temp WHERE tt-temp.b-num = blk.bnum 
                         AND tt-temp.s-num = blk.snum
        NO-ERROR.
    IF NOT AVAIL tt-temp  THEN DO:
        CREATE tt-temp.
        ASSIGN tt-temp.b-num = blk.bnum
               tt-temp.s-num = blk.snum.
    END. 

    find first eb
        where eb.company = xest.company 
          and eb.est-no    eq xest.est-no
          and eb.form-no  eq blk.snum
          and eb.blank-no eq blk.bnum
        no-lock no-error.
    v-yld = if eb.yld-qty lt 0 then -1 / eb.yld-qty else eb.yld-qty.
    
    if vmclean2 then
      assign
       v-tt-tot[blk.snum]   = probeit.full-cost * v-yld * qm
       v-ord-cost[blk.snum] = probeit.fact-cost * v-yld * qm
        tt-temp.v-temp-tot[1]   = probeit.full-cost * v-yld * qm
        tt-temp.v-temp-ord[1] = probeit.fact-cost * v-yld * qm
        .
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
  v-yld = if eb.yld-qty lt 0 then -1 / eb.yld-qty else eb.yld-qty.

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
     probeit.mku_com   = v-com
     probeit.bl-qty    = qty
     probeit.yld-qty   = qty * v-yld.
  END.
  v-comm = (probeit.sell-price - (if v-basis eq "G" then probeit.fact-cost else 0)) *
            (v-com / 100).
                                                                              
    FIND FIRST tt-temp WHERE tt-temp.b-num = blk.bnum 
                         AND tt-temp.s-num = blk.snum
        NO-ERROR.
    IF NOT AVAIL tt-temp  THEN DO:
        CREATE tt-temp.
        ASSIGN tt-temp.b-num = blk.bnum
               tt-temp.s-num = blk.snum.
    END. 

  if vmclean2 then
    assign
     probeit.full-cost = tt-temp.v-temp-tot[1]
     /* fact cost should not change when price changes - task 10111206 */
     /* probeit.fact-cost = v-ord-cost[blk.snum] */.
     
  else
    assign
     probeit.full-cost  = probe.full-cost * (blk.cost / blk-cost) * qm
     /*probeit.fact-cost  = probe.fact-cost * (blk.fact / blk-fact) * qm*/.
     
  assign
   probeit.full-cost  = probeit.full-cost  / (probeit.yld-qty / 1000)
   /* probeit.fact-cost  = probeit.fact-cost  / (probeit.yld-qty / 1000)*/
   probeit.full-cost  = probeit.full-cost + v-comm
   v-tot-sell-price = v-tot-sell-price + probeit.sell-price
   v-tot-full-cost = v-tot-full-cost + probeit.full-cost.
end.

ASSIGN
   probe.sell-price = v-tot-sell-price
   probe.full-cost = v-tot-full-cost.
