 /* ar/ar-invk.i  */

 def var tax as dec no-undo.
 def var ftax as dec no-undo.
 DEFINE VARIABLE dLineTax AS DECIMAL NO-UNDO .
 DEFINE VARIABLE dLinefTax AS DECIMAL NO-UNDO .
 DEFINE BUFFER bff-ar-invl FOR ar-invl .

 assign   
   {1}.net     = 0
   {1}.freight = 0
   {1}.tax-amt = 0
   tax         = 0
   ftax        = 0.


FOR EACH bff-ar-invl WHERE bff-ar-invl.x-no = {1}.x-no NO-LOCK :
   ASSIGN  
    {1}.net = {1}.net + bff-ar-invl.amt
    {1}.freight = {1}.freight + bff-ar-invl.t-freight
    . 
   if {1}.tax-code ne "" and bff-ar-invl.tax eq yes Then DO:
     run ar/calctax2.p ({1}.tax-code,
                       no,
                       bff-ar-invl.amt,
                       bff-ar-invl.company,
                       "", /* item */
                       OUTPUT dLineTax).
     tax = tax + dLineTax  .

     if {1}.f-bill Then do:
     run ar/calctax2.p ({1}.tax-code, 
                       yes,
                       bff-ar-invl.t-freight, 
                       bff-ar-invl.company,
                       "", /* item */
                       OUTPUT dLinefTax).
       ftax = ftax + dLinefTax .
     END.
   END.
END.

  ASSIGN  
   {1}.tax-amt = tax + ftax   
   {1}.net = {1}.net + (IF {1}.f-bill THEN {1}.freight ELSE 0)
   {1}.gross = {1}.net + {1}.tax-amt  
   {1}.due = {1}.gross - {1}.paid - {1}.disc-taken
   .
