&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME vend-code-cust-xref

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
