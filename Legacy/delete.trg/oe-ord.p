&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME oe-ord

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

DEFINE BUFFER bf-oe-ord FOR oe-ord .


/* not delete if estimate exists */
FIND est WHERE est.rec_key EQ oe-ord.rec_key NO-LOCK NO-ERROR.
IF NOT AVAIL est THEN DO:
  {methods/triggers/delete.i}
END.

IF {&TABLENAME}.ord-no NE 0 THEN
FOR EACH eb
    WHERE eb.company EQ {&TABLENAME}.company
      AND eb.ord-no  EQ {&TABLENAME}.ord-no
    USE-INDEX ord-no:
  eb.ord-no = 0. 
END.





/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
    
