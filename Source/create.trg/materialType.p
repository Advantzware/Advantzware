&Scoped-define TABLENAME materialType

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

{&TABLENAME}.materialTypeID = NEXT-VALUE(materialTypeID_seq).
