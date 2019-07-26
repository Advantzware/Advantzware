&Scoped-define TABLENAME estCostMisc

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{&TABLENAME}.estCostMiscID = NEXT-VALUE(estCostMiscID_seq,ASI).

{methods/triggers/create.i}



