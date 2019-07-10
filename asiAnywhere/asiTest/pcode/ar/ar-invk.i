 /* ar/ar-invk.i  */

 def var sub-tot as dec no-undo.
 def var tax as dec no-undo.
 def var ftax as dec no-undo.


 if not avail cust then
    find first cust where cust.company eq g_company
                      and cust.cust-no eq ar-inv.cust-no no-lock no-error.
 assign
   sub-tot     = {1}.gross + {1}.freight
   {1}.tax-amt = 0
   tax         = 0
   ftax        = 0.
   
  if {1}.tax-code ne "" and cust.sort eq "Y" then do:
    run ar/calctax.p ({1}.tax-code, no,
                      {1}.gross, output tax).
             
    run ar/calctax.p ({1}.tax-code, yes,
                      {1}.freight, output ftax).

    {1}.tax-amt = tax + ftax.
  end.

  assign
   {1}.net = sub-tot + {1}.tax-amt
   {1}.due = {1}.net - {1}.paid - {1}.disc-taken.
