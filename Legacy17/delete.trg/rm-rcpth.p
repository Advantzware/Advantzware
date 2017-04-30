&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME rm-rcpth

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH rm-rdtlh WHERE rm-rdtlh.r-no EQ {&TABLENAME}.r-no
    USE-INDEX rm-rdtl EXCLUSIVE-LOCK:
  DELETE rm-rdtlh.
END.
