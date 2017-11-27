&Scoped-define ACTION UPDATE
&Scoped-define DBNAME LDBNAME('EMPTRACK')
&Scoped-define TABLENAME machemp

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}
