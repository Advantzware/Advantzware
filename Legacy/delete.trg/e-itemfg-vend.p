&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME e-itemfg-vend

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

DEF BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.
{methods/triggers/delete.i}

