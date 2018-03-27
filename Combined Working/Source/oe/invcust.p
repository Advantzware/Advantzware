/* --------------------------------------------------- oe/invcust.p 10/99 JLF */
/* Invoicing  - Edit Register & Post Invoicing Transactions - 63K             */
/* -------------------------------------------------------------------------- */

def input parameter v-recid     as   recid.
def input parameter v-ord-no    like inv-line.ord-no.
def input parameter udate       like period.pst.
def input parameter uperiod     like period.pnum.


{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared var v-trnum as int.
def shared var v-reduce-ord-bal like cust.ord-bal no-undo.

find inv-head where recid(inv-head) eq v-recid no-lock.

find first cust
    {sys/ref/custW.i}
      and cust.cust-no eq inv-head.cust-no
    exclusive-lock.
    
find first period
    where period.company eq cocode
      and period.pst     le udate
      and period.pend    ge udate
      and period.pnum    eq uperiod
    no-lock no-error.
    
if avail period                     and
   inv-head.inv-date ge period.pst  and
   inv-head.inv-date le period.pend then
  assign
   cust.sales[uperiod] = cust.sales[uperiod] +
                         (inv-head.t-inv-rev - inv-head.t-inv-tax)
   cust.cost[1] = cust.cost[1] + inv-head.t-inv-cost      /* PTD */
   cust.comm[1] = cust.comm[1] + inv-head.t-comm.         /* PTD */
   
assign   
 cust.ytd-sales = cust.ytd-sales  +
                  (inv-head.t-inv-rev - inv-head.t-inv-tax)
 cust.cost[5] = cust.cost[5] + inv-head.t-inv-cost      /* YTD */
 cust.comm[5] = cust.comm[5] + inv-head.t-comm          /* YTD */
 cust.acc-bal = cust.acc-bal +
                if inv-head.terms eq "CASH" then 0 else inv-head.t-inv-rev.

if cust.acc-bal ge cust.hibal then
  assign
   cust.hibal      = cust.acc-bal
   cust.hibal-date = inv-head.inv-date.

if inv-head.terms eq "CASH" then
  assign
   cust.lpay      = inv-head.t-inv-rev
   cust.lpay-date = inv-head.inv-date.

else do:
  find ar-ledger
      where ar-ledger.company  eq inv-head.company
        and ar-ledger.cust-no  eq inv-head.cust-no
        and ar-ledger.ref-date eq inv-head.inv-date
        and ar-ledger.ref-num  eq "INV# " + string(inv-head.inv-no)
      no-error.
  if not avail ar-ledger then do:
    create ar-ledger.
    assign
     ar-ledger.company  = cocode
     ar-ledger.cust-no  = inv-head.cust-no
     ar-ledger.amt      = - inv-head.t-inv-rev
     ar-ledger.ref-num  = "INV# " + string(inv-head.inv-no)
     ar-ledger.ref-date = inv-head.inv-date
     ar-ledger.tr-num   = v-trnum
     ar-ledger.tr-date  = udate.
  end.

  else ar-ledger.amt  = ar-ledger.amt - inv-head.t-inv-rev.
end.

