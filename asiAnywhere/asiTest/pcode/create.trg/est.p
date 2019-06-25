&Scoped-define TABLENAME est

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}


ASSIGN
 {&TABLENAME}.entered-date = TODAY
 {&TABLENAME}.entered-id   = USERID("nosweat").
