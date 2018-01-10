&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME fg-rcpth

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.

DEF VAR ll AS LOG NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR li1 AS INT NO-UNDO.


{&TABLENAME}.po-no = TRIM(STRING(INT({&TABLENAME}.po-no),">>>>>>>>>>")) NO-ERROR.

DISABLE TRIGGERS FOR LOAD OF b-{&TABLENAME}.

IF {&TABLENAME}.company NE "" AND {&TABLENAME}.r-no NE 0 THEN DO:
  FOR EACH b-{&TABLENAME}
      WHERE b-{&TABLENAME}.r-no   EQ {&TABLENAME}.r-no
        AND ROWID(b-{&TABLENAME}) NE ROWID({&TABLENAME}):
    BUFFER-COMPARE b-{&TABLENAME} EXCEPT rec_key TO {&TABLENAME} SAVE RESULT IN ll.
    IF ll THEN DELETE b-{&TABLENAME}.
  END.

  li1 = 0.
  DO WHILE CAN-FIND(FIRST b-{&TABLENAME}
                    WHERE b-{&TABLENAME}.r-no   EQ {&TABLENAME}.r-no
                      AND ROWID(b-{&TABLENAME}) NE ROWID({&TABLENAME})):
    ASSIGN
     li  = 0
     li1 = li1 + 1.

    IF li1 GE 1000 THEN LEAVE.

    FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAIL fg-rctd AND fg-rctd.r-no GT li THEN li = fg-rctd.r-no.

    FIND LAST b-{&TABLENAME} USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL b-{&TABLENAME} AND b-{&TABLENAME}.r-no GT li THEN li = b-{&TABLENAME}.r-no.

    {&TABLENAME}.r-no = li + 1.
  END.

  RELEASE po-ord.
  IF TRIM({&TABLENAME}.po-no) NE "" THEN
  FIND FIRST po-ord NO-LOCK
      WHERE po-ord.company EQ {&TABLENAME}.company
        AND po-ord.po-no   EQ INT({&TABLENAME}.po-no)
      NO-ERROR.
  IF AVAIL po-ord THEN {&TABLENAME}.vend-no = po-ord.vend-no.


     IF {&TABLENAME}.create-by = "" THEN DO:
         ASSIGN {&TABLENAME}.create-by = USERID("nosweat").
    END.
    ASSIGN
      {&TABLENAME}.update-by = USERID("nosweat")         
     {&TABLENAME}.upd-date = TODAY
     {&TABLENAME}.upd-time = TIME.
END.
{&TABLENAME}.update-by = USERID("nosweat").
/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
