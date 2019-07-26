&Scoped-define TABLENAME estCostForm

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{&TABLENAME}.estCostFormID = NEXT-VALUE(estCostFormID_seq,ASI).

{methods/triggers/create.i}


