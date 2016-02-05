/* findfile.p */

DEFINE INPUT PARAMETER file-name AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER found-file AS LOGICAL NO-UNDO.

FIND dictdb._file WHERE dictdb._file._file-name = file-name NO-LOCK NO-ERROR.
found-file = AVAILABLE(dictdb._file).
