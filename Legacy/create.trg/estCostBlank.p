&Scoped-define TABLENAME estCostBlank

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{&TABLENAME}.estCostBlankID = NEXT-VALUE(estCostBlankID_seq,ASI).

{methods/triggers/create.i}

