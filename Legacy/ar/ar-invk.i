 /* ar/ar-invk.i  */

 def var sub-tot as dec no-undo.
 def var tax as dec no-undo.
 def var ftax as dec no-undo.
 DEFINE VARIABLE dLineTax AS DECIMAL NO-UNDO .
 DEFINE VARIABLE dLinefTax AS DECIMAL NO-UNDO .

 if not avail cust then
    find first cust where cust.company eq g_company
                      and cust.cust-no eq ar-inv.cust-no no-lock no-error.
 assign   
   {1}.tax-amt = 0
   tax         = 0
   ftax        = 0.

  if LOGICAL({1}.f-bill) EQ TRUE THEN
   sub-tot     = {1}.net + {1}.freight.
  ELSE   sub-tot     = {1}.net .
    
tax = 0.
ftax = 0.

FOR EACH ar-invl WHERE ar-invl.x-no = {1}.x-no NO-LOCK :
    
   if {1}.tax-code ne "" and ar-invl.tax eq yes Then DO:
     run ar/calctax2.p ({1}.tax-code,
                       no,
                       ar-invl.amt,
                       cust.company,
                       "", /* item */
                       OUTPUT dLineTax).
     tax = tax + dLineTax  .

     if {1}.f-bill Then do:
     run ar/calctax2.p ({1}.tax-code, 
                       yes,
                       {1}.freight, 
                       cust.company,
                       "", /* item */
                       OUTPUT dLinefTax).
       ftax = ftax + dLinefTax .
     END.
   END.
END.


   {1}.tax-amt = tax + ftax.   

  assign 
   {1}.gross = sub-tot + {1}.tax-amt  
   {1}.due = {1}.gross - {1}.paid - {1}.disc-taken.
