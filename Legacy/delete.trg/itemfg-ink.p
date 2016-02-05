&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME itemfg-ink

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.

DEF BUFFER b-rt0 FOR reftable.
DEF BUFFER b-rt1 FOR reftable.


cocode = {&TABLENAME}.company.

IF cocode NE "" AND TRIM({&TABLENAME}.i-no) NE "" THEN DO:
  {sys/inc/fgcolors.i}

  IF fgcolors-log THEN
  FOR EACH eb
      WHERE eb.company  EQ {&TABLENAME}.company
        AND eb.stock-no EQ {&TABLENAME}.i-no
      NO-LOCK,
      FIRST est-op
      WHERE est-op.company EQ eb.company
        AND est-op.est-no  EQ eb.est-no
        AND est-op.s-num   EQ eb.form-no
        AND (est-op.b-num  EQ eb.blank-no OR eb.est-type NE 3) 
        AND est-op.op-pass EQ {&TABLENAME}.pass
        AND est-op.line    LE 500
        AND (est-op.dept   EQ "PR" OR est-op.dept EQ "CT")
      NO-LOCK,
      FIRST mach
      WHERE mach.company    EQ est-op.company
        AND mach.m-code     EQ est-op.m-code
      NO-LOCK:

    RUN fg/setcolor.p (ROWID(eb), mach.pr-type).
  END.
END.

IF TRIM({&TABLENAME}.rec_key) NE "" THEN
FOR EACH reftable
    WHERE reftable.rec_key  EQ {&TABLENAME}.rec_key
      AND reftable.reftable EQ "itemfg-ink.occurs"
    USE-INDEX rec_key:
  DELETE reftable.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
