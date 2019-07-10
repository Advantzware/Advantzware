&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME fg-rctd

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.


DISABLE TRIGGERS FOR LOAD OF b-{&TABLENAME}.

IF {&TABLENAME}.bol-no NE 0                             AND
   ({&TABLENAME}.loc     NE {&TABLENAME}.loc2     OR
    {&TABLENAME}.loc-bin NE {&TABLENAME}.loc-bin2 OR
    {&TABLENAME}.tag     NE {&TABLENAME}.tag2)          THEN DO:
  FIND FIRST oe-bolh NO-LOCK
      WHERE oe-bolh.company EQ {&TABLENAME}.company
        AND oe-bolh.bol-no  EQ {&TABLENAME}.bol-no
      NO-ERROR.

  IF AVAIL oe-bolh AND oe-bolh.b-no NE 0 AND
     CAN-FIND(FIRST fg-rcpth
              WHERE fg-rcpth.company EQ oe-bolh.company
                AND fg-rcpth.b-no    EQ oe-bolh.b-no
              USE-INDEX b-no)            THEN RETURN ERROR.
END.

FOR EACH fg-rcpts WHERE fg-rcpts.r-no EQ {&TABLENAME}.r-no:
  DELETE fg-rcpts.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "fg-rctd.user-id"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ STRING({&TABLENAME}.r-no,"9999999999"):
  DELETE reftable.
END.

IF {&TABLENAME}.rita-code EQ "T" AND AVAIL oe-bolh THEN DO:
  FIND CURRENT oe-bolh NO-ERROR.
  oe-bolh.posted = NO.
  FOR EACH b-{&TABLENAME}
      WHERE b-{&TABLENAME}.company EQ oe-bolh.company
        AND b-{&TABLENAME}.bol-no  EQ oe-bolh.bol-no
        AND ROWID(b-{&TABLENAME})  NE ROWID({&TABLENAME}):
    FOR EACH fg-rcpts WHERE fg-rcpts.r-no EQ b-{&TABLENAME}.r-no:
      DELETE fg-rcpts.
    END.
    DELETE b-{&TABLENAME}.
  END.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
