
DEF VAR a LIKE ar-cashl.dscr NO-UNDO.


PAUSE 0 BEFORE-HIDE.

FOR EACH ar-cash WHERE posted EQ YES,
    EACH ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no
                    AND ar-cashl.memo EQ YES
                    AND ar-cashl.dscr EQ "debit":
  
  ar-cashl.dscr = IF ar-cashl.amt-disc - ar-cashl.amt-paid LT 0
                  THEN "Debit" ELSE "Credit".

  DISPLAY ar-cash.check-no FORMAT ">>>>>>>>>>"
      WITH TITLE "Fix Memo Description".
END.
