&Scoped-define TABLENAME fg-rctd

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

ASSIGN fg-rctd.upd-date = TODAY
       fg-rctd.upd-time = TIME
       fg-rctd.created-by = USERID("NOSWEAT")
       fg-rctd.CreateInvoice = NO.
