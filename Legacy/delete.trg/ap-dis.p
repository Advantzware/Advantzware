&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ap-dis

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

IF ap-dis.posted THEN DO:
  MESSAGE "Record already posted. No deletion allowed! " VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

FOR EACH ap-disl OF {&TABLENAME}:
  DELETE ap-disl.
END.

FIND FIRST bank OF ap-dis
    WHERE bank.last-chk EQ {&TABLENAME}.check-no
      AND NOT CAN-FIND(FIRST ap-pay
                       WHERE ap-pay.company   EQ bank.company
                         AND ap-pay.check-act EQ bank.actnum
                         AND ap-pay.check-no  EQ bank.last-chk)
    NO-ERROR.
IF AVAIL bank THEN bank.last-chk = bank.last-chk - 1.
