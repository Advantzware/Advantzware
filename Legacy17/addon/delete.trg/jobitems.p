&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('JOBS')
&Scoped-define TABLENAME jobitems

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
