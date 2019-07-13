&Scoped-define TABLENAME programMenu

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

ASSIGN 
    programMenu.menuID = NEXT-VALUE(programMenu_seq).
    