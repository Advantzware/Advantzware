&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME gl-rpt

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.


IF {&TABLENAME}.line EQ 0 THEN
FOR EACH b-{&TABLENAME}
    WHERE b-{&TABLENAME}.company EQ {&TABLENAME}.company
      AND b-{&TABLENAME}.rpt     EQ {&TABLENAME}.rpt
      AND b-{&TABLENAME}.line    NE 0:
  DELETE b-{&TABLENAME}.
END.

