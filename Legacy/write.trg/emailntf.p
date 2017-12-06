&Scoped-define ACTION UPDATE
&Scoped-define DBNAME LDBNAME('tmp')
&Scoped-define TABLENAME emailntf

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}
