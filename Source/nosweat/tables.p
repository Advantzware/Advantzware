/* tables.p - rstark - 12.22.2018 */

DEFINE OUTPUT PARAMETER opcTableName  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcTableLabel AS CHARACTER NO-UNDO.

FOR EACH dictdb._file
    WHERE dictdb._file._Tbl-type EQ "T"
    NO-LOCK
    :
    ASSIGN
        opcTableName  = opcTableName + dictdb._file._file-name + ","
        opcTableLabel = opcTableLabel
                      + (IF dictdb._file._file-label EQ ? THEN dictdb._file._file-name
                         ELSE dictdb._file._file-label)
                      + ","
                      .
END.
ASSIGN
    opcTableName  = TRIM(opcTableName,",")
    opcTableLabel = TRIM(opcTableLabel,",")
    .
SESSION:SET-WAIT-STATE("").
