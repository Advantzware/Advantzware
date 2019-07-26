&Scoped-define TABLENAME estCostMaterial

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{&TABLENAME}.estCostMaterialID = NEXT-VALUE(estCostMaterialID_seq,ASI).

{methods/triggers/create.i}

