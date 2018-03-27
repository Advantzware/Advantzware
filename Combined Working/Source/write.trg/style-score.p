&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME style-score

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}
