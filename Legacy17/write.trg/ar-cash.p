&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ar-cash

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


IF {&TABLENAME}.c-no NE 0 THEN DO:
  FOR EACH ar-cashl WHERE ar-cashl.c-no EQ {&TABLENAME}.c-no:
    ar-cashl.cust-no = {&TABLENAME}.cust-no.
  END.

  IF {&TABLENAME}.check-date NE old-{&TABLENAME}.check-date THEN DO:
    IF {&TABLENAME}.memo THEN
    FOR EACH ar-ledger
        WHERE ar-ledger.company  EQ {&TABLENAME}.company
          AND ar-ledger.cust-no  EQ {&TABLENAME}.cust-no
          AND ar-ledger.ref-date EQ old-{&TABLENAME}.check-date
          AND ar-ledger.ref-num  EQ "Memo#" +
                                    STRING({&TABLENAME}.check-no,"99999999") +
                                    "A/R":
      ar-ledger.ref-date = {&TABLENAME}.check-date.
    END.

    ELSE DO:
      FOR EACH ar-ledger
          WHERE ar-ledger.company  EQ {&TABLENAME}.company
            AND ar-ledger.cust-no  EQ {&TABLENAME}.cust-no
            AND ar-ledger.ref-date EQ old-{&TABLENAME}.check-date
            AND ar-ledger.ref-num  EQ "CHK# " + STRING({&TABLENAME}.check-no,"9999999999"):
        ar-ledger.ref-date = {&TABLENAME}.check-date.
      END.

      FOR EACH ar-cashl
          WHERE ar-cashl.c-no     EQ {&TABLENAME}.c-no
            AND ar-cashl.amt-disc NE 0
          NO-LOCK:
        FOR EACH ar-ledger
            WHERE ar-ledger.company  EQ {&TABLENAME}.company
              AND ar-ledger.cust-no  EQ {&TABLENAME}.cust-no
              AND ar-ledger.ref-date EQ old-{&TABLENAME}.check-date
              AND ar-ledger.ref-num  EQ "DISC " +
                                        STRING({&TABLENAME}.check-no,"9999999999") +
                                        "-" + STRING(ar-cashl.line,"999"):
          ar-ledger.ref-date = {&TABLENAME}.check-date.
        END.
      END.
    END.
  END.
END.
