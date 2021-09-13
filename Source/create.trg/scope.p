&Scoped-define TABLENAME scope

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.scopeID = NEXT-VALUE(scopeID_seq).
