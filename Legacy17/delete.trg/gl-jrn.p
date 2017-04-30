&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME gl-jrn

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

IF gl-jrn.posted THEN DO:
  MESSAGE "Record already posted. No deletion allowed! " VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

FOR EACH gl-jrnl OF gl-jrn:
  DELETE gl-jrnl.
END.

/* REPEAT:                                                                                        */
/*    FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ gl-jrn.company EXCLUSIVE-LOCK NO-ERROR NO-WAIT. */
/*                                                                                                */
/*    IF AVAIL gl-ctrl THEN                                                                       */
/*    DO:                                                                                         */
/*       IF gl-jrn.journal EQ gl-ctrl.journal THEN gl-ctrl.journal = gl-ctrl.journal - 1.         */
/*       FIND CURRENT gl-ctrl NO-LOCK.                                                            */
/*       LEAVE.                                                                                   */
/*    END.                                                                                        */
/* END.                                                                                           */
FIND FIRST gl-ctrl
    WHERE gl-ctrl.company EQ gl-jrn.company
    EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL gl-ctrl THEN DO:
    IF gl-jrn.journal EQ gl-ctrl.journal THEN 
        gl-ctrl.journal = gl-ctrl.journal - 1.
    FIND CURRENT gl-ctrl NO-LOCK.
END.
