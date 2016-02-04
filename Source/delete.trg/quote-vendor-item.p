&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('NOSWEAT')
&Scoped-define TABLENAME quote-vendor-item

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FIND FIRST quote-vendor OF {&TABLENAME} NO-LOCK NO-ERROR.

FOR EACH quote-vendor-item OF {&TABLENAME}:
  DELETE quote-vendor-qty.
END.

