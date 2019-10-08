/*  rc/getpswd.i - standard password verification include file.
{&field} = Field or Variable containing the correct password.
04.22.92 by CH:
1.  Added no-pause after hide-frame.

02.20.92 by CH:
1.  Initial write from ar/login.p for general ??/login.p use.
*/

DEF VAR tries   AS INTEGER NO-UNDO.

IF {&field} > '' THEN do with frame {&frame}:
_getpassword:
DO ON ERROR UNDO, RETRY:
  PROMPT-FOR
    pswd BLANK WHEN {&field} > '' WITH FRAME {&frame}
    EDITING:
    READKEY.
    APPLY LASTKEY.
    IF GO-PENDING THEN
    DO:
      IF INPUT pswd <> {&field} AND {&field} > '' THEN
      DO:
        BELL.
        MESSAGE COLOR VALUE(c_wrn) 'Enter password before continuing'.
        IF tries < 2 THEN
        DO:
          tries = tries + 1.
          PAUSE 1.
          UNDO, RETRY.
        END.
        ELSE
        DO:
          MESSAGE COLOR VALUE(c_err) 'Password violation!'.
          STOP.
        END.
      END.
    END.
  END.
  HIDE FRAME {&frame} NO-PAUSE.
END.    /* _getpassword do */
end.    /* if do */
