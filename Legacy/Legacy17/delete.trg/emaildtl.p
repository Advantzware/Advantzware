&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('NOSWEAT')
&Scoped-define TABLENAME emaildtl

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
