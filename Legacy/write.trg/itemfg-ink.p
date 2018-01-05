&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME itemfg-ink

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF eb.

cocode = {&TABLENAME}.company.

IF cocode NE ""                  AND 
   TRIM({&TABLENAME}.i-no) NE "" THEN DO:
  {sys/inc/fgcolors.i}

  IF fgcolors-log THEN
  FOR EACH eb
      WHERE eb.company  EQ {&TABLENAME}.company
        AND eb.stock-no EQ {&TABLENAME}.i-no,
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

    IF TRIM({&TABLENAME}.rm-i-no) NE ""                     AND
       TRIM(old-{&TABLENAME}.rm-i-no) NE ""                 AND
       (old-{&TABLENAME}.rm-i-no NE {&TABLENAME}.rm-i-no OR
        old-{&TABLENAME}.pass NE {&TABLENAME}.pass)         THEN

      IF eb.est-type LE 4 THEN
      DO li = 1 TO EXTENT(eb.i-code2):
        IF eb.i-code2[li] EQ old-{&TABLENAME}.rm-i-no AND
           eb.i-ps2[li]   EQ old-{&TABLENAME}.pass    THEN DO:
          ASSIGN
           eb.i-ps2[li]   = {&TABLENAME}.pass
           eb.i-code2[li] = {&TABLENAME}.rm-i-no
           eb.i-dscr2[li] = {&TABLENAME}.dscr
           eb.i-%2[li]    = {&TABLENAME}.cover%.
        END.
      END.
      ELSE
      DO li = 1 TO EXTENT(eb.i-code):
        IF eb.i-code[li] EQ old-{&TABLENAME}.rm-i-no AND
           eb.i-ps[li]   EQ old-{&TABLENAME}.pass    THEN
          ASSIGN
           eb.i-ps[li]   = {&TABLENAME}.pass
           eb.i-code[li] = {&TABLENAME}.rm-i-no
           eb.i-dscr[li] = {&TABLENAME}.dscr
           eb.i-%[li]    = {&TABLENAME}.cover%.
      END.

    RUN fg/setcolor.p (ROWID(eb), mach.pr-type).
  END.
END.


/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
