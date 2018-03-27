&Scoped-define TABLENAME e-itemfg-vend

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}


{&TABLENAME}.vend-no = FILL(" ",100) + STRING(USERID("nosweat"),"x(20)") + STRING(TIME,"9999999999").
