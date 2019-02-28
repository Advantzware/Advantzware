&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME ar-cashl

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}


FOR EACH reftable
    WHERE reftable.reftable EQ "ar-cashl.ar-cashl"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ ""
      AND reftable.code     EQ {&TABLENAME}.rec_key:
  DELETE reftable.
END.


