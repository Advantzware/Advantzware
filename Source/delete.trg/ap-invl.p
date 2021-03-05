&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME ap-invl

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEFINE VARIABLE hdAPInvoiceProcs AS HANDLE NO-UNDO.

RUN ap/APInvoiceProcs.p PERSISTENT SET hdAPInvoiceProcs.

RUN APInvoice_DeleteAPInvoiceLineLinks IN hdAPInvoiceProcs (
    INPUT {&TABLENAME}.rec_key
    ).

DELETE PROCEDURE hdAPInvoiceProcs.
            