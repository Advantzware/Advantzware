&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME oe-ordm

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DISABLE TRIGGERS FOR LOAD OF oe-ord.


{sys/inc/var.i NEW SHARED}

cocode = {&TABLENAME}.company.

{sys/inc/oeuserid.i}

FOR EACH oe-ord
    WHERE oe-ord.company EQ {&TABLENAME}.company
      AND oe-ord.ord-no  EQ {&TABLENAME}.ord-no:

  IF oeuserid-log                        AND
     oe-ord.user-id NE USERID("nosweat") THEN
    oe-ord.user-id = USERID("nosweat").

  IF NOT CAN-FIND(FIRST oe-ordl 
                  WHERE oe-ordl.company EQ oe-ord.company
                    AND oe-ordl.ord-no  EQ oe-ord.ord-no) THEN DO:
    CREATE oe-ordl.
    ASSIGN
     oe-ordl.company = oe-ord.company
     oe-ordl.ord-no  = oe-ord.ord-no
     oe-ordl.line    = 0.
  END.

  FIND oe-ordl NO-LOCK
      WHERE oe-ordl.company EQ {&TABLENAME}.company
        AND oe-ordl.ord-no  EQ {&TABLENAME}.ord-no
        AND oe-ordl.i-no   EQ {&TABLENAME}.ord-i-no
      NO-ERROR.
  IF AVAIL oe-ordl THEN {&TABLENAME}.ord-line = oe-ordl.line.

  LEAVE.
END.

IF TRIM({&TABLENAME}.rec_key) NE "" THEN DO:
  {custom/coloraud.i}
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
