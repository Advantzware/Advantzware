/* dynLookupSetParam.p - rstark - 1.31.2020 */

DEFINE INPUT  PARAMETER ipiSubjectID    AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER iprRowID        AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAMETER opcReturnFields AS CHARACTER NO-UNDO.

DEFINE VARIABLE cBuffer       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLookupField  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReturnFields AS CHARACTER NO-UNDO.
DEFINE VARIABLE hBuffer       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQuery        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hTable        AS HANDLE    NO-UNDO.
DEFINE VARIABLE rRecID        AS RECID     NO-UNDO.

FIND FIRST dynPrgrmsPage NO-LOCK
     WHERE dynPrgrmsPage.subjectID EQ ipiSubjectID
     NO-ERROR.
IF AVAILABLE dynPrgrmsPage THEN DO:
    CREATE QUERY hQuery.
    CREATE BUFFER hBuffer FOR TABLE dynPrgrmsPage.tableName.
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE(
        "FOR EACH " + dynPrgrmsPage.tableName + " NO-LOCK " +
        "WHERE ROWID(" + dynPrgrmsPage.tableName + ") = TO-ROWID(~"" +
        STRING(iprRowID) + "~")"
        ).
    hQuery:QUERY-OPEN().
    hTable = hQuery:GET-BUFFER-HANDLE(dynPrgrmsPage.tableName).
    hQuery:GET-FIRST().
    cCompany = hTable:BUFFER-FIELD("company"):BUFFER-VALUE() NO-ERROR.
    DELETE OBJECT hBuffer.
    DELETE OBJECT hQuery.
    
    RUN pDynPrgrmsPage (ipiSubjectID, dynPrgrmsPage.tableName + "," + STRING(iprRowID), "", 0, ?).
    RUN system/openLookup.p (
        cCompany,
        "",
        ipiSubjectID,
        USERID("ASI"),
        0,
        OUTPUT cReturnFields,
        OUTPUT cLookupField,
        OUTPUT rRecID
        ).
    opcReturnFields = cReturnFields.
END.
ELSE
MESSAGE
    "No Dynamic Subject Parameter Initialize Record Exists!!!" SKIP(1)
    "ND2 Dynamic Subjects [Parameter Sets] [Initialize]"
VIEW-AS ALERT-BOX ERROR.
