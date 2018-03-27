&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME quoteitm

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FIND FIRST quotehd OF {&TABLENAME} NO-LOCK NO-ERROR.

FOR EACH quoteqty OF {&TABLENAME}:
  {delete.trg/quoteqty.i}
  DELETE quoteqty.
END.

FOR EACH quotechg OF {&TABLENAME}:
  DELETE quotechg.                         
END.
