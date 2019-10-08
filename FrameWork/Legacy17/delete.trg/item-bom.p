&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME item-bom

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
