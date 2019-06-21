&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME oe-relh

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEF BUFFER b-oe-ordl FOR oe-ordl.

DEF VAR li AS INT NO-UNDO.


IF {&TABLENAME}.company NE "" AND {&TABLENAME}.r-no NE 0 AND NOT {&TABLENAME}.posted THEN
DO WHILE CAN-FIND(FIRST b-{&TABLENAME}
                  WHERE b-{&TABLENAME}.r-no   EQ {&TABLENAME}.r-no
                    AND ROWID(b-{&TABLENAME}) NE ROWID({&TABLENAME})):
  li = 0.

  FIND LAST b-{&TABLENAME} USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL b-{&TABLENAME} AND b-{&TABLENAME}.r-no GT li THEN li = b-{&TABLENAME}.r-no.

  {&TABLENAME}.r-no = li + 1.
END.

IF {&TABLENAME}.posted NE old-{&TABLENAME}.posted THEN
FOR EACH oe-rell
    WHERE oe-rell.company EQ {&TABLENAME}.company
      AND oe-rell.r-no    EQ {&TABLENAME}.r-no
    USE-INDEX r-no EXCLUSIVE:

  oe-rell.posted = {&TABLENAME}.posted.
      
  RUN oe/rel-stat-upd.p (ROWID(oe-boll)).

  IF NOT {&TABLENAME}.posted THEN
  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ oe-rell.company
        AND oe-ordl.ord-no  EQ oe-rell.ord-no
        AND oe-ordl.i-no    EQ oe-rell.i-no
        AND oe-ordl.line    EQ oe-rell.line
        AND oe-ordl.is-a-component EQ NO
        AND CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})
      USE-INDEX item-ord NO-LOCK:

    DELETE oe-rell.
    LEAVE.
  END.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

