&Scoped-define TABLENAME POReceiptLink

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.poReceiptLinkID = NEXT-VALUE(poReceiptLinkID_seq).
