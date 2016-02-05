&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ar-inv

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

IF ar-inv.posted THEN DO:
   MESSAGE "Invoice already posted. No deletion allowed!" VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR.
END.
FOR EACH ar-invl WHERE ar-invl.x-no = ar-inv.x-no:
    DELETE ar-invl.
END.

FIND FIRST reftable WHERE
     reftable.reftable EQ "brokerbol" AND
     reftable.CODE EQ STRING(ar-inv.inv-no)
     NO-ERROR.

IF AVAIL reftable THEN
   DELETE reftable.

REPEAT:
   FIND FIRST ar-ctrl WHERE
        ar-ctrl.company EQ ar-inv.company
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

   IF AVAILABLE ar-ctrl THEN
   DO:
      IF ar-ctrl.last-inv = ar-inv.inv-no THEN
         ar-ctrl.last-inv = ar-ctrl.last-inv - 1.
      RELEASE ar-ctrl.
      LEAVE.
   END.
END.
