&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME rm-rdtlh

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER rm-bin-age-date FOR reftable.


DISABLE TRIGGERS FOR LOAD OF rm-bin-age-date.

FOR EACH rm-rcpth WHERE rm-rcpth.r-no EQ {&TABLENAME}.r-no NO-LOCK:
  IF rm-rcpth.job-no EQ "" THEN {&TABLENAME}.s-num = 0.

  IF rm-rcpth.rita-code EQ "R" AND
     NOT CAN-FIND(FIRST rm-bin-age-date
                  WHERE rm-bin-age-date.reftable EQ "rm-bin.age-date"
                    AND rm-bin-age-date.company  EQ rm-rcpth.company
                    AND rm-bin-age-date.loc      EQ rm-rcpth.i-no
                    AND rm-bin-age-date.code     EQ STRING({&TABLENAME}.loc,"x(50)") +
                                                    STRING({&TABLENAME}.loc-bin,"x(50)")
                    AND rm-bin-age-date.code2    EQ {&TABLENAME}.tag) THEN DO:
    CREATE rm-bin-age-date.
    ASSIGN
     rm-bin-age-date.reftable = "rm-bin.age-date"
     rm-bin-age-date.company  = rm-rcpth.company
     rm-bin-age-date.loc      = rm-rcpth.i-no
     rm-bin-age-date.code     = STRING({&TABLENAME}.loc,"x(50)") +
                                STRING({&TABLENAME}.loc-bin,"x(50)")
     rm-bin-age-date.code2    = {&TABLENAME}.tag
     rm-bin-age-date.val[1]   = INT(rm-rcpth.trans-date).
  END.
  LEAVE.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
