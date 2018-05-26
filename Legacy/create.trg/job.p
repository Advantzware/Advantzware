&Scoped-define TABLENAME job

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

/*{methods/triggers/create.i}  don't create rec_key here, get it from est*/
ASSIGN
    {&TABLENAME}.create-date = TODAY.
