&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME reftable

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF VAR lv-est-no LIKE est.est-no NO-UNDO.
DEF VAR li AS INT NO-UNDO.

IF {&TABLENAME}.reftable EQ "EST-COST" THEN DO:
  lv-est-no = "".
  DO li = 1 TO LENGTH({&TABLENAME}.code):
    IF SUBSTR(reftable.code,1,1) EQ "/" THEN LEAVE.
    lv-est-no = lv-est-no + SUBSTR({&TABLENAME}.code,1,1).
  END.
  lv-est-no = FILL(" ",8 - LENGTH(TRIM(lv-est-no))) + TRIM(lv-est-no).

  IF lv-est-no NE "" THEN
  FOR EACH est
      WHERE est.company EQ {&TABLENAME}.company
        AND est.est-no  EQ lv-est-no:
    RUN update-est.
    LEAVE.
  END.
END.
ELSE
IF {&TABLENAME}.reftable EQ "PLATE/FOUNTAIN"    OR
   {&TABLENAME}.reftable EQ "ce/v-est3.w Unit#"  OR 
   {&TABLENAME}.reftable EQ "cedepth" THEN
FOR EACH est
    WHERE est.company EQ {&TABLENAME}.company
      AND est.est-no  EQ {&TABLENAME}.loc:
  RUN update-est.
  LEAVE.
END.
ELSE
IF {&TABLENAME}.reftable EQ "est/getqty.w" THEN
FOR EACH est
    WHERE est.company EQ {&TABLENAME}.company
      AND est.est-no  EQ {&TABLENAME}.code:
  RUN update-est.
  LEAVE.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

RETURN.

/* Procedures */
PROCEDURE update-est.
  ASSIGN
   est.updated-date = TODAY
   est.updated-id   = USERID("nosweat").
END PROCEDURE.
