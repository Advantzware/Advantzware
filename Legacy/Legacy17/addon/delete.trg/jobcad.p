&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('JOBS')
&Scoped-define TABLENAME jobcad

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
