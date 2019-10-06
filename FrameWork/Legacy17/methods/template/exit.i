/* exit.i */

DEFINE VARIABLE close-ok AS LOGICAL NO-UNDO.

RUN Check-Exit IN Persistent-Handle (OUTPUT close-ok).

IF close-ok THEN
DO:
  {custom/exit.i}
  SYSTEM-HELP "system/nosweat.hlp" QUIT.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.
