&Scoped-define TABLENAME glhist

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

ASSIGN
    {&TABLENAME}.createdBy   = USERID(LDBNAME(1))
    {&TABLENAME}.createdDate = today .
