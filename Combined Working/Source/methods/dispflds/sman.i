/* sman.i */

  WHEN "{&FIRST-EXTERNAL-TABLE}.territory" THEN
  DO:
    {custom/getcmpny.i}
    FIND terr
        WHERE terr.company = gcompany
          AND terr.terr = {&FIRST-EXTERNAL-TABLE}.territory:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    terr_dscr = IF NOT AVAILABLE terr THEN ""
                ELSE terr.dscr.
    DISPLAY terr_dscr.
  END.
