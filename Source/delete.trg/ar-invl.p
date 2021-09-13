&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME ar-invl

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH InvoiceLineTax WHERE InvoiceLineTax.invoiceLineRecKey EQ ar-invl.rec_key:
    DELETE InvoiceLineTax.
END.
