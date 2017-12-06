&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME oe-ordl

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

DEF BUFFER oe-ordl-q-no FOR reftable.
DEF BUFFER oe-ordl-whs-item FOR reftable.


DISABLE TRIGGERS FOR LOAD OF oe-ord.

{sys/inc/var.i NEW SHARED}

cocode = {&TABLENAME}.company.

{sys/inc/oeuserid.i}

/* not delete if estimate exists */
FIND est WHERE est.rec_key EQ oe-ordl.rec_key NO-LOCK NO-ERROR.
IF NOT AVAIL est THEN DO:
  {methods/triggers/delete.i}
END.

FIND FIRST itemfg
    WHERE itemfg.company EQ {&TABLENAME}.company
      AND itemfg.i-no    EQ {&TABLENAME}.i-no
    NO-LOCK NO-ERROR.      
IF AVAIL itemfg THEN RUN fg/makenote.p (BUFFER {&TABLENAME},
                                        BUFFER quoteqty,
                                        BUFFER ar-invl,
                                        YES,
                                        itemfg.rec_key).





{&TABLENAME}.line = 999999999.
FOR EACH oe-ord OF {&TABLENAME}:
  RUN oe/ordlline.p (ROWID(oe-ord)).

  IF oeuserid-log THEN oe-ord.user-id = USERID("nosweat").

  LEAVE.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.




