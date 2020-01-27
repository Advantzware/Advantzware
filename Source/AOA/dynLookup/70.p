/* 70.p - rstark - 1.15.2020 */

&Scoped-define subjectID 70
&Scoped-define company style.company

DEFINE INPUT  PARAMETER ipcTableRowID   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcReturnFields AS CHARACTER NO-UNDO.

FIND FIRST style NO-LOCK
     WHERE ROWID(style) EQ TO-ROWID(ENTRY(2,ipcTableRowID))
     NO-ERROR.
IF NOT AVAILABLE style THEN RETURN.

{AOA/includes/dynLookup.i}

opcReturnFields = cReturnFields.
