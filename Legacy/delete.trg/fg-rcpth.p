&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME fg-rcpth

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH fg-rdtlh
    WHERE fg-rdtlh.r-no      EQ {&TABLENAME}.r-no
      AND fg-rdtlh.rita-code EQ {&TABLENAME}.rita-code
    EXCLUSIVE-LOCK:
  DELETE fg-rdtlh.
END.

IF NOT CAN-FIND(FIRST fg-rctd WHERE fg-rctd.r-no EQ {&TABLENAME}.r-no) THEN
FOR EACH reftable
    WHERE reftable.reftable EQ "fg-rctd.user-id"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ STRING({&TABLENAME}.r-no,"9999999999"):
  DELETE reftable.
END.
