&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME rm-rdtlh

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


DEF BUFFER b-rm-bin FOR rm-bin.

FOR EACH rm-rcpth WHERE rm-rcpth.r-no EQ {&TABLENAME}.r-no NO-LOCK:
  IF rm-rcpth.job-no EQ "" THEN {&TABLENAME}.s-num = 0.
  IF rm-rcpth.rita-code EQ "R" THEN DO:
    FIND FIRST b-rm-bin WHERE b-rm-bin.company = rm-rcpth.company 
                          AND b-rm-bin.loc     = STRING({&TABLENAME}.loc,"x(50)")
                          AND b-rm-bin.i-no    = rm-rcpth.i-no
                          AND b-rm-bin.loc-bin = STRING({&TABLENAME}.loc-bin,"x(50)")
                          AND b-rm-bin.tag     = {&TABLENAME}.tag   
                          EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE b-rm-bin THEN  
       ASSIGN  b-rm-bin.aging-date   = rm-rcpth.trans-date. 
    RELEASE b-rm-bin.
  END.
  LEAVE.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
