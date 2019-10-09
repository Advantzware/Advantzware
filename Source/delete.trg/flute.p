&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME flute

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
/* this include called delete.trg/stack-flute.p 
       and cause error and stop this deleting procedure
  {methods/delete.trg/{&TABLENAME}.i} */

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
    

DISABLE TRIGGERS FOR LOAD OF stack-flute.

IF NOT CAN-FIND(FIRST b-{&TABLENAME}
                WHERE b-{&TABLENAME}.company EQ {&TABLENAME}.company
                  AND b-{&TABLENAME}.code    EQ {&TABLENAME}.code
                  AND ROWID({&TABLENAME})    NE ROWID({&TABLENAME})) THEN DO:

  FOR EACH reftable
      WHERE reftable.reftable EQ "STYFLU"
        AND reftable.loc      EQ {&TABLENAME}.code:
    DELETE reftable.
  END.

  FOR EACH stack-flute
      WHERE stack-flute.company EQ {&TABLENAME}.company
        AND stack-flute.code    EQ {&TABLENAME}.code:
    DELETE stack-flute.
  END.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
