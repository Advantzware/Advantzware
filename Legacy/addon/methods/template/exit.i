/* exit.i */

DEFINE VARIABLE close-ok AS LOGICAL NO-UNDO.

IF VALID-HANDLE(Persistent-Handle) THEN
RUN Check-Exit IN Persistent-Handle (OUTPUT close-ok).
ELSE
close-ok = YES.

IF close-ok THEN
DO:
  {custom/exit.i}
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.
