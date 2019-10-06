/* Be sure whatever mods you make take into account multi-invoice customers  */
&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME inv-head

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


DISABLE TRIGGERS FOR LOAD OF inv-line.

FOR EACH inv-line WHERE inv-line.r-no EQ {&TABLENAME}.r-no:
  inv-line.cust-no = {&TABLENAME}.cust-no.
END.

IF {&TABLENAME}.company NE "" AND {&TABLENAME}.inv-no NE 0 THEN
  RUN oe/checkinv#.p (BUFFER {&TABLENAME}).

RELEASE cust.
FIND FIRST cust NO-LOCK
    WHERE cust.company EQ {&TABLENAME}.company
      AND cust.cust-no EQ {&TABLENAME}.cust-no
    NO-ERROR.
IF AVAIL cust THEN {&TABLENAME}.curr-code[1] = cust.curr-code.

RUN oe/updmulti.p (BUFFER {&TABLENAME}, BUFFER old-{&TABLENAME}).

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
