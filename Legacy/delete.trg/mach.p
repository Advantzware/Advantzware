&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME mach

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

do: {est/del-mach.i } end.

FOR EACH reftable
    WHERE reftable.reftable EQ "MACH-CREW"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ {&TABLENAME}.loc
      AND reftable.code     EQ {&TABLENAME}.m-code:
  DELETE reftable.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "mach.plain-jobs"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ {&TABLENAME}.loc
      AND reftable.code     EQ {&TABLENAME}.m-code:
  DELETE reftable.
END.



FOR EACH mach-part WHERE
    mach-part.company EQ {&TABLENAME}.company AND
    mach-part.m-code EQ {&TABLENAME}.m-code
    EXCLUSIVE-LOCK:
    DELETE mach-part.
END.

