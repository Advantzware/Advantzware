&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME rm-rcpth

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.

DEF VAR li AS INT NO-UNDO.
DEF VAR li1 AS INT NO-UNDO.


{&TABLENAME}.po-no = TRIM(STRING(INT({&TABLENAME}.po-no),">>>>>>>>>>")).

IF {&TABLENAME}.company NE "" AND {&TABLENAME}.r-no NE 0 THEN DO:

  IF TRIM({&TABLENAME}.user-id) EQ "" THEN
    ASSIGN
     {&TABLENAME}.user-id  = USERID("nosweat")
     {&TABLENAME}.upd-date = TODAY
     {&TABLENAME}.upd-time = TIME.

  RELEASE po-ord.
  IF TRIM({&TABLENAME}.po-no) NE "" THEN
  FIND FIRST po-ord NO-LOCK
      WHERE po-ord.company EQ {&TABLENAME}.company
        AND po-ord.po-no   EQ INT({&TABLENAME}.po-no)
      NO-ERROR.
  IF AVAIL po-ord THEN {&TABLENAME}.vend-no = po-ord.vend-no.
END.

IF {&TABLENAME}.job-no EQ "" THEN
FOR EACH rm-rdtlh WHERE rm-rdtlh.r-no EQ {&TABLENAME}.r-no:
  rm-rdtlh.s-num = 0.
END.

IF {&TABLENAME}.company NE "" AND INT({&TABLENAME}.po-no) NE 0 THEN
FOR EACH po-ord
    WHERE po-ord.company EQ {&TABLENAME}.company
      AND po-ord.po-no   EQ INT({&TABLENAME}.po-no)
    NO-LOCK:
  {&TABLENAME}.vend-no = po-ord.vend-no.
  LEAVE.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
