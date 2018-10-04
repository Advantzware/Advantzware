&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('NOSWEAT')
&Scoped-define TABLENAME usergrps

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
