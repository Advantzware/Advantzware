 /* ar/ar-invk.i  */

 DEFINE VARIABLE dLineTax AS DECIMAL NO-UNDO .
 DEFINE VARIABLE dLinefTax AS DECIMAL NO-UNDO .
 DEFINE BUFFER bff-ar-invl FOR ar-invl .

DEFINE VARIABLE dInvoiceTotal    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dInvoiceSubTotal AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dTotalTax        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cmessage         AS CHARACTER NO-UNDO.

 assign   
   {1}.net     = 0
   {1}.freight = 0
   {1}.tax-amt = 0
   .

FOR EACH bff-ar-invl WHERE bff-ar-invl.x-no = {1}.x-no NO-LOCK :
   ASSIGN  
    {1}.net = {1}.net + bff-ar-invl.amt
    {1}.freight = {1}.freight + bff-ar-invl.t-freight
    . 
END.

RUN Tax_CalculateForArInv  (
    INPUT  ROWID({1}),
    INPUT  locode,
    INPUT  "QUOTATION", /*  Message Type "INVOICE" or "QUOTATION" */
    INPUT  FALSE,       /* Post To journal */
    INPUT  "GetTaxAmount", /* Trigger ID */
    OUTPUT dTotalTax,
    OUTPUT dInvoiceTotal,
    OUTPUT dinvoiceSubTotal,
    OUTPUT lSuccess,
    OUTPUT cMessage
    ).

ASSIGN  
   {1}.tax-amt = dTotalTax   
   {1}.net     = {1}.net + (IF {1}.f-bill THEN {1}.freight ELSE 0)
   {1}.gross   = {1}.net + {1}.tax-amt  
   {1}.due     = {1}.gross - {1}.paid - {1}.disc-taken
   .
