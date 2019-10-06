&Scoped-define TABLENAME estPacking

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

 ASSIGN 
    {&TABLENAME}.estPackingID      = NEXT-VALUE(estPackingId_seq) .
