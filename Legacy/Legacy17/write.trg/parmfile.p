&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('NOSWEAT')
&Scoped-define TABLENAME parmfile

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}
