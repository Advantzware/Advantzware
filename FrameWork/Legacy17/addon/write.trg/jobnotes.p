&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('JOBS')
&Scoped-define TABLENAME jobnotes

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}
