&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME quoteqty

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


FIND FIRST quoteitm OF {&TABLENAME} NO-LOCK NO-ERROR.

IF AVAIL quoteitm THEN
FIND FIRST quotehd OF quoteitm NO-LOCK NO-ERROR.

IF AVAIL quotehd THEN
FIND FIRST eb
    WHERE eb.company EQ quotehd.company
      AND eb.est-no  EQ quotehd.est-no
      AND eb.part-no EQ quoteitm.part-no
    NO-LOCK NO-ERROR.

IF AVAIL eb AND eb.stock-no NE "" THEN
FIND FIRST itemfg
    WHERE itemfg.company EQ eb.company
      AND itemfg.i-no    EQ eb.stock-no
    NO-LOCK NO-ERROR.

IF {&TABLENAME}.qty NE 0                            AND
   {&TABLENAME}.uom NE ""                           AND
   AVAIL itemfg                                     AND
   ({&TABLENAME}.qty   NE old-{&TABLENAME}.qty   OR
    {&TABLENAME}.price NE old-{&TABLENAME}.price OR
    {&TABLENAME}.uom   NE old-{&TABLENAME}.uom)     THEN

  RUN fg/makenote.p (BUFFER oe-ordl,
                     BUFFER {&TABLENAME},
                     BUFFER ar-invl,
                     NO,
                     itemfg.rec_key).

{&TABLENAME}.rel = {&TABLENAME}.rels.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
