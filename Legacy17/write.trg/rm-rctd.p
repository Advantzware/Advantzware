&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME rm-rctd

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.

DEF VAR li AS INT NO-UNDO.
DEF VAR li1 AS INT NO-UNDO.


IF {&TABLENAME}.company NE "" AND {&TABLENAME}.r-no NE 0 THEN DO:
ASSIGN
   {&TABLENAME}.user-id  = USERID("nosweat")
   {&TABLENAME}.upd-date = TODAY
   {&TABLENAME}.upd-time = TIME.
END.

{&TABLENAME}.po-no = TRIM(STRING(INT({&TABLENAME}.po-no),">>>>>>>>>>")).

IF {&TABLENAME}.job-no EQ "" THEN {&TABLENAME}.s-num = 0.

IF {&TABLENAME}.pur-uom EQ "EA" THEN DO:
  {sys/inc/roundup.i {&TABLENAME}.qty}
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
