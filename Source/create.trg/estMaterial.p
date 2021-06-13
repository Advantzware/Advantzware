&Scoped-define TABLENAME estMaterial

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

 ASSIGN 
    {&TABLENAME}.estMaterialID      = NEXT-VALUE(estMaterialID_seq) .
