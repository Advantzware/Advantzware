&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('EMPTRACK')
&Scoped-define TABLENAME shift_break

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}
