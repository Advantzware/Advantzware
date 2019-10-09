/* logEntry.i */

PROCEDURE logEntry:
  DEFINE INPUT PARAMETER ipLogEntry AS CHARACTER NO-UNDO.

  OUTPUT TO 'itemImages/itemImages.log' APPEND.
  PUT UNFORMATTED STRING(TODAY,'99.99.9999') ' @ '
    STRING(TIME,'hh:mm:ss am') ': ' ipLogEntry SKIP.
  OUTPUT CLOSE.
END PROCEDURE.
