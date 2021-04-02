&Scoped-define TABLENAME customerPart

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.customerPartID = NEXT-VALUE(customerPartID_seq).
