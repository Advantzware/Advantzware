&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ap-pay

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


IF {&TABLENAME}.check-date NE old-{&TABLENAME}.check-date THEN DO:
  FOR EACH ap-ledger
      WHERE ap-ledger.company  EQ {&TABLENAME}.company
        AND ap-ledger.vend-no  EQ {&TABLENAME}.vend-no
        AND ap-ledger.ref-date EQ old-{&TABLENAME}.check-date
        AND ap-ledger.refnum   EQ "CHK# " + STRING({&TABLENAME}.check-no) +
                                  " CD#" + {&TABLENAME}.bank-code:

    ap-ledger.ref-date = {&TABLENAME}.check-date.
  END.

  FOR EACH ap-ledger
    WHERE ap-ledger.company  EQ {&TABLENAME}.company
      AND ap-ledger.vend-no  EQ {&TABLENAME}.vend-no
      AND ap-ledger.ref-date EQ old-{&TABLENAME}.check-date
      AND ap-ledger.refnum   EQ "AC" + STRING({&TABLENAME}.check-no,"999999"):

    ap-ledger.ref-date = {&TABLENAME}.check-date.
  END.

  FOR EACH ap-payl
      WHERE ap-payl.c-no EQ {&TABLENAME}.c-no
      BREAK BY ap-payl.inv-no
            BY ap-payl.line:

    IF {&TABLENAME}.vend-no NE ""                           AND
       NOT CAN-FIND(FIRST ap-inv
                    WHERE ap-inv.company EQ {&TABLENAME}.company
                      AND ap-inv.vend-no EQ ap-payl.vend-no
                      AND ap-inv.inv-no  EQ ap-payl.inv-no) THEN
      ap-payl.vend-no = {&TABLENAME}.vend-no.

    IF LAST-OF(ap-payl.inv-no) THEN
    FOR EACH ap-ledger
        WHERE ap-ledger.company  EQ {&TABLENAME}.company
          AND ap-ledger.vend-no  EQ {&TABLENAME}.vend-no
          AND ap-ledger.ref-date EQ old-{&TABLENAME}.check-date
          AND ap-ledger.refnum   EQ "MEMO#" + ap-payl.inv-no:
      ap-ledger.ref-date = {&TABLENAME}.check-date.
    END.
  END.
END.
