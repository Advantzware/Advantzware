&Scoped-define TABLENAME InvoiceLineTax

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.invoiceLineTaxID = NEXT-VALUE(invoiceLineTaxID_seq).
