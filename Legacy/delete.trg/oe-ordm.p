&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME oe-ordm

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DISABLE TRIGGERS FOR LOAD OF oe-ord.


{sys/inc/var.i NEW SHARED}

cocode = {&TABLENAME}.company.

{sys/inc/oeuserid.i}

FOR EACH reftable
    WHERE reftable.reftable EQ "oe/ordlmisc.p"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ STRING({&TABLENAME}.ord-no,"9999999999")
      AND reftable.code     EQ STRING({&TABLENAME}.line,"9999999999")
      AND reftable.code2    EQ {&TABLENAME}.charge:
  DELETE reftable.
END.

IF oeuserid-log THEN
FOR EACH oe-ord
    WHERE oe-ord.company EQ {&TABLENAME}.company
      AND oe-ord.ord-no  EQ {&TABLENAME}.ord-no:
  oe-ord.user-id = USERID("ASI").
END.
    
IF TRIM({&TABLENAME}.rec_key) NE "" THEN
FOR EACH reftable
    WHERE reftable.rec_key  EQ {&TABLENAME}.rec_key
      AND reftable.company  EQ "{&TABLENAME}"
    USE-INDEX rec_key:

  DELETE reftable.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
