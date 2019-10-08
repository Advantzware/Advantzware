&Scoped-define TABLENAME estCostDetail

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{&TABLENAME}.estCostDetailID = NEXT-VALUE(estCostDetailID_seq,ASI).

{methods/triggers/create.i}

