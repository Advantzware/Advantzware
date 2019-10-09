&Scoped-define TABLENAME estCostItem

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{&TABLENAME}.estCostItemID = NEXT-VALUE(estCostItemID_seq,ASI).

{methods/triggers/create.i}

