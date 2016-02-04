&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME fg-rcpts

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH fg-rdtl WHERE fg-rdtl.r-no EQ {&TABLENAME}.r-no EXCLUSIVE-LOCK:
  DELETE fg-rdtl.
END.
