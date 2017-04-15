&Scoped-define TABLENAME e-item-vend

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}


{&TABLENAME}.vend-no = FILL(" ",100) + STRING(USERID("nosweat"),"x(20)") + STRING(TIME,"9999999999").
