/* tables.p - rstark - 12.22.2018 */

DEFINE OUTPUT PARAMETER opcTableName AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcTableDscr AS CHARACTER NO-UNDO.

FOR EACH dictdb._file
    WHERE dictdb._file._Tbl-type EQ "T"
    NO-LOCK
    :
    ASSIGN
        opcTableName = opcTableName + dictdb._file._file-name + ","
        opcTableDscr = opcTableDscr
                     + (IF dictdb._file._desc EQ "" THEN dictdb._file._file-name
                        ELSE REPLACE(dictdb._file._desc,",",""))
                     + ","
                     .
END.
ASSIGN
    opcTableName = TRIM(opcTableName,",")
    opcTableDscr = TRIM(opcTableDscr,",")
    .
SESSION:SET-WAIT-STATE("").
