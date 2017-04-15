&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('EMPTRACK')
&Scoped-define TABLENAME machseq

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
