/* bank.i */

  WHEN "{&FIRST-EXTERNAL-TABLE}.actnum" THEN
  DO:
    {custom/getcmpny.i}
    FIND account
        WHERE account.company = gcompany
          AND account.actnum = {&FIRST-EXTERNAL-TABLE}.actnum:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    account_dscr = IF NOT AVAILABLE account THEN ""
                   ELSE account.dscr.
    DISPLAY account_dscr.
  END.
