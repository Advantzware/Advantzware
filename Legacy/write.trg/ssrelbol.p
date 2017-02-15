&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ssrelbol

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

ASSIGN
   {&TABLENAME}.updated-date = TODAY
   {&TABLENAME}.updated-id = USERID("NOSWEAT")
   {&TABLENAME}.updated-time = TIME.

