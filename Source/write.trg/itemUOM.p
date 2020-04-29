&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME itemUOM

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

ASSIGN
    {&TABLENAME}.updatedBy   = USERID("ASI")
    {&TABLENAME}.updatedDtTm = NOW
    .
