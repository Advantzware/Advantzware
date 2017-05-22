/* --------------------------------------------------- ar/updcust.p 11/96 JLF */
/* -------------------------------------------------------------------------- */

def input parameter v-cust-no like cust.cust-no.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer b-cust for cust.

def new shared var v-tax-rate as dec.
def new shared var v-frt-tax-rate as dec.

def var v-fr-tax like oe-ctrl.f-tax init no.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
if avail oe-ctrl then v-fr-tax = oe-ctrl.f-tax.

for each cust
    where cust.company eq cocode
      and (v-cust-no eq "*" or cust.cust-no eq v-cust-no)
    no-lock
   
    transaction:

  status default "Please wait...  Updating Customer: " + trim(cust.cust-no).

  for each ar-inv
      where ar-inv.company eq cocode
        and ar-inv.cust-no eq cust.cust-no
        and ar-inv.posted  eq yes
      use-index posted no-lock:
    accumulate ar-inv.due (total).
  end.

  for each ar-cashl
      where ar-cashl.company    eq cocode
        and ar-cashl.posted     eq yes
        and ar-cashl.cust-no    eq cust.cust-no
        and ar-cashl.on-account eq yes
      use-index inv-no no-lock:
    accumulate ar-cashl.amt-paid (total).
  end.
  
  find b-cust where recid(b-cust) eq recid(cust)
      exclusive-lock no-error no-wait.
      
  IF AVAIL b-cust THEN DO:
    ASSIGN
     b-cust.on-account = (accum total ar-cashl.amt-paid)
     b-cust.acc-bal    = (accum total ar-inv.due) - cust.on-account.
  
    RUN ar/upcust1.p (ROWID(b-cust), OUTPUT b-cust.ord-bal).
  END.
end.

status default "".

/* end ---------------------------------- copr. 1996  Advanced Software, Inc. */
