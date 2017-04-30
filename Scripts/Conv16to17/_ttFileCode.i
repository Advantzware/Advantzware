DEFINE TEMP-TABLE ttFileCode NO-UNDO
    FIELD ln AS INT
    FIELD lnText AS CHAR
        INDEX ttFileCode IS PRIMARY ln
    .

FUNCTION fAddComment RETURN CHARACTER (ipcText AS CHARACTER):
    RETURN ipcText + " /* added by script "
                   + PROGRAM-NAME(2) + " */".
END FUNCTION.
