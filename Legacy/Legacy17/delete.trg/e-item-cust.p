&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('asi')
&Scoped-define TABLENAME e-item-cust

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
