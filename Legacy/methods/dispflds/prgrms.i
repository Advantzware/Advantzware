/* prgrms.i */

  WHEN "{&FIRST-EXTERNAL-TABLE}.itemParent" THEN DO:
      parentPrgTitle:SCREEN-VALUE = "Program Not Found".
      FIND FIRST bPrgrms NO-LOCK
           WHERE bPrgrms.prgmname EQ {&FIRST-EXTERNAL-TABLE}.itemParent:SCREEN-VALUE
           NO-ERROR.
      IF AVAILABLE bPrgrms THEN
      parentPrgTitle:SCREEN-VALUE = bPrgrms.prgtitle.
  END.
