&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ar-mcash

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


IF {&TABLENAME}.company NE "" THEN DO:
  FIND FIRST currency
      WHERE currency.company EQ {&TABLENAME}.company
        AND currency.c-code = {&TABLENAME}.curr-code[1] NO-LOCK NO-ERROR.
  {&TABLENAME}.ex-rate = IF AVAIL currency THEN currency.ex-rate ELSE 1.

  IF {&TABLENAME}.check-date NE old-{&TABLENAME}.check-date THEN
  FOR EACH ar-ledger
      WHERE ar-ledger.company  EQ {&TABLENAME}.company
        AND ar-ledger.cust-no  EQ ""
        AND ar-ledger.ref-date EQ old-{&TABLENAME}.check-date
        AND ar-ledger.ref-num  EQ STRING({&TABLENAME}.m-no) + " " + {&TABLENAME}.payer:
    ar-ledger.ref-date = {&TABLENAME}.check-date.
  END.
END.
