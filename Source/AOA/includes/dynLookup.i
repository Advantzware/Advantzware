/* dynLookup.i - rstark - 1.15.2020 */

DEFINE VARIABLE cLookupField  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReturnFields AS CHARACTER NO-UNDO.
DEFINE VARIABLE rRecID        AS RECID     NO-UNDO.

RUN pDynPrgrmsPage ({&subjectID}, ipcTableRowID, "", 0, ?).

RUN system/openLookup.p (
    {&company},
    "",
    {&subjectID},
    USERID("ASI"),
    0,
    OUTPUT cReturnFields,
    OUTPUT cLookupField,
    OUTPUT rRecID
    ).
