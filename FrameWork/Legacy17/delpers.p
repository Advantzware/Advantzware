/* delpers.p */

DEFINE VARIABLE phandle AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE shandle AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE i AS INTEGER FORMAT ">>9" NO-UNDO.
DEFINE VARIABLE delnames AS CHARACTER NO-UNDO.

phandle = SESSION:FIRST-PROCEDURE.
DO WHILE VALID-HANDLE(phandle):
  shandle = phandle:NEXT-SIBLING.
  delnames = delnames + phandle:FILE-NAME + ",".
  DELETE PROCEDURE phandle.
  phandle = shandle.
END.
DO i = 1 TO NUM-ENTRIES(delnames) - 1:
  DISPLAY i ENTRY(i,delnames) FORMAT "x(60)"
      WITH NO-LABELS DOWN.
  DOWN.
END.
