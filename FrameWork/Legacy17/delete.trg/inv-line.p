&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME inv-line

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}


ASSIGN
 cocode = g_company
 locode = g_loc.

FIND FIRST inv-head WHERE inv-head.r-no EQ {&TABLENAME}.r-no NO-LOCK NO-ERROR.
IF AVAIL inv-head THEN RUN oe/oeinvup2.p (ROWID(inv-head), INPUT NO).

FOR EACH reftable WHERE
    reftable.reftable EQ "inv-line.lot-no" AND
    reftable.rec_key EQ inv-line.rec_key
    USE-INDEX rec_key
    EXCLUSIVE-LOCK:

    DELETE reftable.
END.
