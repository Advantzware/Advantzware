&Scoped-define TABLENAME estCostHeader

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{&TABLENAME}.estCostHeaderID = NEXT-VALUE(estCostHeaderID_seq,ASI).

{methods/triggers/create.i}
