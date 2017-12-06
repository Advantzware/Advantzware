&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME fg-rcpth

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH fg-rdtlh
    WHERE fg-rdtlh.r-no      EQ {&TABLENAME}.r-no
      AND fg-rdtlh.rita-code EQ {&TABLENAME}.rita-code
    EXCLUSIVE-LOCK:
  DELETE fg-rdtlh.
END.

