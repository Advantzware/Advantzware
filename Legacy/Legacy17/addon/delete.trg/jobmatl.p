&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('JOBS')
&Scoped-define TABLENAME jobmatl

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
