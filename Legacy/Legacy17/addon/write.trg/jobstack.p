&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('JOBS')
&Scoped-define TABLENAME jobstack

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}
