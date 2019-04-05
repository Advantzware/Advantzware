/* dynSubjectLookup.p - rstark - 4.4.2019 */

DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiSubjectID    AS INTEGER   NO-UNDO.

DEFINE OUTPUT PARAMETER opcReturnValues AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcLookupField  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oprRecID        AS RECID     NO-UNDO.

DEFINE VARIABLE cDisplayFields  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormats        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLabels         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRequiredFields AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReturnFields   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSearchFields   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSortFields     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSourceField    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSourceTable    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTitle          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWhereClause    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWidths         AS CHARACTER NO-UNDO.

FIND FIRST dynSubject NO-LOCK
     WHERE dynSubject.subjectID EQ ipiSubjectID
     NO-ERROR.
IF NOT AVAILABLE dynSubject THEN DO:
    MESSAGE 
        "Lookup Subject:" ipiSubjectID "Not Found."
    VIEW-AS ALERT-BOX ERROR.
    RETURN.
END. /* if not avail */
cTitle = dynSubject.subjectTitle.

FOR EACH dynSubjectWhere OF dynSubject NO-LOCK:
    cWhereClause = cWhereClause + dynSubjectWhere.whereElement + " ".
END. /* each dynsubjecthwere */
ASSIGN
    cWhereClause = REPLACE(cWhereClause,"WHERE ","")
    cWhereClause = REPLACE(cWhereClause,"[[company]]","~"" + ipcCompany + "~"")
    cWhereClause = TRIM(cWhereClause)
    .
FOR EACH dynSubjectColumn OF dynSubject NO-LOCK
    BY dynSubjectColumn.sortOrder
    :
    IF dynSubjectColumn.sortCol EQ 1 THEN
    ASSIGN
        cSourceTable = dynSubjectColumn.tableName
        cSourceField = dynSubjectColumn.fieldName
        .
    cRequiredFields = cRequiredFields + dynSubjectColumn.fieldName + ",".
    IF dynSubjectColumn.isActive THEN
    ASSIGN
        cDisplayFields = cDisplayFields + dynSubjectColumn.fieldName + ","
        cFormats       = cFormats + dynSubjectColumn.fieldFormat + ","
        cLabels        = cLabels + dynSubjectColumn.fieldLabel + ","
        cReturnFields  = cReturnFields + dynSubjectColumn.fieldName + ","
        cSearchFields  = cSearchFields + dynSubjectColumn.fieldName + ","
        cSortFields    = cSortFields + dynSubjectColumn.fieldName + ","
        cWidths        = cWidths + (IF dynSubjectColumn.dataType EQ "Date" THEN "20" ELSE "") + ","
        .
END. /* each dynsubjectcolumn */
ASSIGN
    cDisplayFields  = TRIM(cDisplayFields,",")
    cFormats        = TRIM(cFormats,",")
    cLabels         = TRIM(cLabels,",")
    cRequiredFields = TRIM(cRequiredFields,",")
    cReturnFields   = TRIM(cReturnFields,",")
    cSearchFields   = TRIM(cSearchFields,",")
    cSortFields     = TRIM(cSortFields,",")
    cWidths         = SUBSTRING(cWidths,1,LENGTH(cWidths) - 1)
    cDisplayFields  = REPLACE(cDisplayFields,cSourceTable + ".","")
    cRequiredFields = REPLACE(cRequiredFields,cSourceTable + ".","")
    cReturnFields   = REPLACE(cReturnFields,cSourceTable + ".","")
    cSearchFields   = REPLACE(cSearchFields,cSourceTable + ".","")
    cSortFields     = REPLACE(cSortFields,cSourceTable + ".","")
    cSourceField    = REPLACE(cSourceField,cSourceTable + ".","")
    .
RUN windows/l-lookup.w (
    cTitle,
    cSourceField,
    cSourceTable,
    cRequiredFields,
    cDisplayFields,
    cLabels,
    cFormats,
    cWidths,
    cSearchFields,
    cSortFields,
    cWhereClause,
    cReturnFields,
    OUTPUT opcReturnValues,
    OUTPUT opcLookupField,
    OUTPUT oprRecID
    ).
