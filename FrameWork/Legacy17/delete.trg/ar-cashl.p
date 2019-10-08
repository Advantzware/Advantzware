&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ar-cashl

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}


FOR EACH reftable
    WHERE reftable.reftable EQ "ar-cashl.return"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ ""
      AND reftable.code     EQ STRING({&TABLENAME}.c-no,"9999999999")
      AND reftable.code2    EQ STRING({&TABLENAME}.line,"9999999999"):
  DELETE reftable.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "ar-cashl.ar-cashl"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ ""
      AND reftable.code     EQ {&TABLENAME}.rec_key:
  DELETE reftable.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "ar-cashl.inv-line"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ ""
      AND reftable.code     EQ STRING({&TABLENAME}.c-no,"9999999999") +
                               STRING({&TABLENAME}.line,"9999999999"):
  DELETE reftable.
END.
