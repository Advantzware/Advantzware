&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME po-ord

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

{sys/inc/var.i NEW SHARED}

{custom/globdefs.i}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF BUFFER b-notes FOR notes.


DISABLE TRIGGERS FOR LOAD OF po-ordl.

{&TABLENAME}.opened = {&TABLENAME}.stat NE "C".

RUN po/po-total.p (RECID({&TABLENAME})).
      
FOR EACH po-ordl
    WHERE po-ordl.company EQ {&TABLENAME}.company
      AND po-ordl.po-no   EQ {&TABLENAME}.po-no:
    
  po-ordl.opened = {&TABLENAME}.opened.
END.
    
IF old-{&TABLENAME}.po-no NE 0  AND
   {&TABLENAME}.opened          AND
   {&TABLENAME}.stat NE "U"     AND
   {&TABLENAME}.stat NE "H"     AND
   {&TABLENAME}.printed         AND
   old-{&TABLENAME}.printed     THEN {&TABLENAME}.stat = "U".

FIND FIRST vend OF {&TABLENAME} NO-LOCK NO-ERROR.

IF {&TABLENAME}.vend-no NE old-{&TABLENAME}.vend-no AND
   {&TABLENAME}.vend-no NE ""                       THEN DO:

  FOR EACH notes WHERE notes.rec_key EQ {&TABLENAME}.rec_key:
    DELETE notes.
  END.

  IF AVAIL vend THEN
  FOR EACH b-notes
      WHERE b-notes.rec_key    EQ vend.rec_key
        AND b-notes.note_type  EQ "G"
        AND b-notes.note_group EQ "PO"
      NO-LOCK:

    CREATE notes.
    BUFFER-COPY b-notes EXCEPT rec_key note_date note_time user_id
                               viewed note_type note_code note_group
                        TO notes
    ASSIGN
     notes.rec_key   = {&TABLENAME}.rec_key
     notes.note_date = TODAY
     notes.note_time = TIME
     notes.user_id   = USERID("NOSWEAT").
  END.
END.

IF {&TABLENAME}.opened AND
   AVAIL vend          AND
   ({&TABLENAME}.t-cost NE old-{&TABLENAME}.t-cost  OR
    {&TABLENAME}.opened NE old-{&TABLENAME}.opened) THEN
  RUN ap/vendobal.p (ROWID(vend)).

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
