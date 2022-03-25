&Scoped-define TABLENAME job

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

ASSIGN
    {&TABLENAME}.create-date = TODAY.
