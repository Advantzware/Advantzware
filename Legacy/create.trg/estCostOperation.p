&Scoped-define TABLENAME estCostOperation

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{&TABLENAME}.estCostOperationID = NEXT-VALUE(estCostOperationID_seq,ASI).

{methods/triggers/create.i}
