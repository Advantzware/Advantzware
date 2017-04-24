&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('EMPTRACK')
&Scoped-define TABLENAME empmach

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
