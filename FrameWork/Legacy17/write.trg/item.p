&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME item

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF VAR ld AS DEC NO-UNDO.


IF {&TABLENAME}.i-code EQ "E" AND {&TABLENAME}.spare-char-2 NE "calc-est-qty" THEN 
    {&TABLENAME}.q-onh = 0.
ELSE
IF {&TABLENAME}.mat-type EQ "P"                 AND
   {&TABLENAME}.s-len NE old-{&TABLENAME}.s-len THEN
FOR EACH po-ordl
    WHERE po-ordl.company   EQ {&TABLENAME}.company
      AND po-ordl.i-no      EQ {&TABLENAME}.i-no
      AND po-ordl.opened    EQ YES
      AND po-ordl.item-type EQ YES
    USE-INDEX item:
    
  RUN po/rm-q-ono.p (BUFFER po-ordl, OUTPUT ld).

  ASSIGN
   {&TABLENAME}.q-ono = {&TABLENAME}.q-ono - ld
   po-ordl.s-len      = {&TABLENAME}.s-len.

  RUN po/poordltot.p (ROWID(po-ordl)).
    
  RUN po/rm-q-ono.p (BUFFER po-ordl, OUTPUT ld).

  {&TABLENAME}.q-ono = {&TABLENAME}.q-ono + ld.
END.
    
{&TABLENAME}.q-avail = {&TABLENAME}.q-onh + {&TABLENAME}.q-ono - {&TABLENAME}.q-comm.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
