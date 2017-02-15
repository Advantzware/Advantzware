&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME oe-rel

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DISABLE TRIGGERS FOR LOAD OF oe-ord.

    
{sys/inc/var.i NEW SHARED}

cocode = {&TABLENAME}.company.

{sys/inc/oeuserid.i}

IF oeuserid-log THEN
FOR EACH oe-ord
    WHERE oe-ord.company EQ {&TABLENAME}.company
      AND oe-ord.ord-no  EQ {&TABLENAME}.ord-no:
  oe-ord.user-id = USERID("nosweat").
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "oe-rel.s-code"
      AND reftable.company  EQ STRING({&TABLENAME}.r-no,"9999999999")
    USE-INDEX reftable:
  DELETE reftable.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "oe-rel.lot-no"
      AND reftable.company  EQ STRING({&TABLENAME}.r-no,"9999999999")
    USE-INDEX reftable:
  DELETE reftable.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "oe-rel.sell-price"
      AND reftable.company  EQ STRING({&TABLENAME}.r-no,"9999999999")
    USE-INDEX reftable:
  DELETE reftable.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "oe-rel.job"
      AND reftable.code     EQ STRING({&TABLENAME}.r-no,"9999999999")
    USE-INDEX code:
  DELETE reftable.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
