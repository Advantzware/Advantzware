&Scoped-define ACTION UPDATE
&Scoped-define DBNAME LDBNAME('sb')
&Scoped-define TABLENAME sbNote

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}
