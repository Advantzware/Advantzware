&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME job-code

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
