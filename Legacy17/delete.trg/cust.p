&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME cust

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.


{methods/triggers/delete.i}

IF cust.cust-no NE "" THEN DO:
  {methods/delete.trg/{&TABLENAME}.i}
END.
