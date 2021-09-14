&Scoped-define TABLENAME orderType

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.orderTypeID = NEXT-VALUE(orderTypeID_seq).
