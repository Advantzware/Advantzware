/* delpers.p */

DEFINE VARIABLE pHandle     AS HANDLE    NO-UNDO.
DEFINE VARIABLE sHandle     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hCalledFrom AS HANDLE    NO-UNDO.
DEFINE VARIABLE idx         AS INTEGER   NO-UNDO FORMAT ">>9".
DEFINE VARIABLE delNames    AS CHARACTER NO-UNDO.

pHandle = SESSION:FIRST-PROCEDURE.
DO WHILE VALID-HANDLE(pHandle):
    ASSIGN
        sHandle     = pHandle:NEXT-SIBLING
        hCalledFrom = pHandle:INSTANTIATING-PROCEDURE
        delNames    = delNames
                    + pHandle:FILE-NAME
                    + (IF VALID-HANDLE(hCalledFrom) THEN " [" + hCalledFrom:NAME + "]" ELSE "")
                    + ","
        .
    DELETE PROCEDURE pHandle.
    pHandle = sHandle.
END.
delNames = TRIM(delNames,",").
DO idx = 1 TO NUM-ENTRIES(delNames) - 1:
  DISPLAY idx ENTRY(idx,delNames) FORMAT "x(60)"
      WITH NO-LABELS DOWN.
  DOWN.
END.
