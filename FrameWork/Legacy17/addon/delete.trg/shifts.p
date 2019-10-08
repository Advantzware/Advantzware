&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('EMPTRACK')
&Scoped-define TABLENAME shifts

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
