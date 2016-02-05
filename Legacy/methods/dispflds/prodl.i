/* prodl.i */

  WHEN "{&FIRST-EXTERNAL-TABLE}.procat" THEN
  DO:
    {custom/getcmpny.i}
    FIND FIRST fgcat
        WHERE fgcat.company EQ gcompany
          AND fgcat.procat  EQ {&FIRST-EXTERNAL-TABLE}.procat:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    procat_dscr = IF AVAIL fgcat THEN fgcat.dscr ELSE "Not on file".
    DISPLAY procat_dscr.
  END.
