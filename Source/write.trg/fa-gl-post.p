&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME fa-gl-post

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}
