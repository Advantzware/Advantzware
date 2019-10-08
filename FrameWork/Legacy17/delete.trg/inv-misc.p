&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME inv-misc

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

FIND FIRST inv-head OF {&TABLENAME} NO-LOCK NO-ERROR.
IF AVAIL inv-head THEN RUN oe/oeinvup2.p (ROWID(inv-head), INPUT NO).
    
IF TRIM({&TABLENAME}.rec_key) NE "" THEN
FOR EACH reftable
    WHERE reftable.rec_key  EQ {&TABLENAME}.rec_key
      AND reftable.company  EQ "{&TABLENAME}"
    USE-INDEX rec_key:

  DELETE reftable.
END.
