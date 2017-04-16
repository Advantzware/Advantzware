&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('NOSWEAT')
&Scoped-define TABLENAME zipcode

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
