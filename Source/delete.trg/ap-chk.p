&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME ap-chk

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH ap-sel
    WHERE ap-sel.company   EQ {&TABLENAME}.company
      AND ap-sel.vend-no   EQ {&TABLENAME}.vend-no
      AND ap-sel.man-check EQ {&TABLENAME}.man-check
      AND ap-sel.bank-code EQ {&TABLENAME}.bank-code
      AND ap-sel.check-no  EQ {&TABLENAME}.check-no
    EXCLUSIVE:
  DELETE ap-sel.
END.

FOR EACH bank
    WHERE bank.company   EQ {&TABLENAME}.company
      AND bank.bank-code EQ {&TABLENAME}.bank-code
      AND bank.last-chk  EQ {&TABLENAME}.check-no
      AND NOT CAN-FIND(FIRST ap-pay
                       WHERE ap-pay.company   EQ bank.company
                         AND ap-pay.check-act EQ bank.actnum
                         AND ap-pay.check-no  EQ bank.last-chk)
    EXCLUSIVE:
  bank.last-chk = bank.last-chk - 1.
END.
