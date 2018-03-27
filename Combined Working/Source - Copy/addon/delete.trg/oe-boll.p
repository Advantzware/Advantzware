&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME oe-boll

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

/*for each oe-boll-qty of {&TABLENAME}:
  delete oe-boll-qty.
end.*/
