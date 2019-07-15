/* dynLookup.p - rstark - 4.4.2019 */

DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiSubjectID    AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcUserID       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiParamValueID AS INTEGER   NO-UNDO.

DEFINE OUTPUT PARAMETER opcReturnValues AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcLookupField  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oprRecID        AS RECID     NO-UNDO.

/*DEFINE VARIABLE cCalcFieldValue AS CHARACTER NO-UNDO.*/
DEFINE VARIABLE cDate           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDisplayFields  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormats        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLabels         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cParam          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQueryStr       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRequiredFields AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReturnFields   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSearchFields   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSortFields     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSourceField    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSourceTable    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTableName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTitle          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWhereClause    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWidths         AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtDate          AS DATE      NO-UNDO.
DEFINE VARIABLE hDynCalcField   AS HANDLE    NO-UNDO.
DEFINE VARIABLE idx             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRecordLimit    AS INTEGER   NO-UNDO.

/*RUN AOA/spDynCalcField.p PERSISTENT SET hDynCalcField.*/

IF ipcUserID EQ "" THEN
ipcUserID = "_default".
FIND FIRST dynParamValue NO-LOCK
     WHERE dynParamValue.subjectID    EQ ipiSubjectID
       AND dynParamValue.user-id      EQ ipcUserID
       AND dynParamValue.paramValueID EQ ipiParamValueID
     NO-ERROR.
IF NOT AVAILABLE dynParamValue THEN DO:
    FIND FIRST dynSubject NO-LOCK
         WHERE dynSubject.subjectID EQ ipiSubjectID
         NO-ERROR.
    MESSAGE
        "Unable to Run Lookup." SKIP(1)
        "Dynamic Parameter Value Record Not Found." SKIP(1)
        "Subject ID:" ipiSubjectID
        (IF AVAILABLE dynSubject THEN "- " + dynSubject.subjectTitle ELSE "")
        SKIP
        "User ID:" ipcUserID SKIP
        "Parameter Value ID:" ipiParamValueID
    VIEW-AS ALERT-BOX ERROR.
    RETURN.
END. /* if not avail */
ASSIGN
    cTitle       = dynParamValue.paramTitle
    iRecordLimit = dynParamValue.recordLimit
    .
FOR EACH dynSubjectWhere NO-LOCK
    WHERE dynSubjectWhere.subjectID EQ ipiSubjectID
       BY dynSubjectWhere.sortOrder
    :
    cWhereClause = cWhereClause + dynSubjectWhere.whereElement + " ".
END. /* each dynsubjecthwere */
ASSIGN
    cWhereClause = REPLACE(cWhereClause,"WHERE ","")
    cWhereClause = TRIM(cWhereClause)
    .
/* replace [[parameter]] with parameter value */
{AOA/includes/cQueryStr.i cWhereClause}
cWhereClause = cQueryStr.

DO idx = 1 TO EXTENT(dynParamValue.colName):
    IF dynParamValue.colName[idx] EQ "" THEN LEAVE.
    IF dynParamValue.isCalcField[idx] THEN DO:
        /* dynamic lookups cannot handle calculated fields */
        /* future development logic to resolve added here  */
/*        RUN spDynCalcField IN hDynCalcField (*/
/*            ?,                               */
/*            dynParamValue.calcProc[idx],     */
/*            dynParamValue.calcParam[idx],    */
/*            dynParamValue.dataType[idx],     */
/*            dynParamValue.colFormat[idx],    */
/*            OUTPUT cCalcFieldValue           */
/*            ).                               */
        NEXT.
    END. /* if iscalcfield */
    ELSE
    ASSIGN
        cTableName = ENTRY(1,dynParamValue.colName[idx],".")
        cFieldName = ENTRY(2,dynParamValue.colName[idx],".")
        .
    IF idx EQ 1 THEN
    ASSIGN
        cSourceTable = cTableName
        cSourceField = cFieldName
        .
    ASSIGN
        cRequiredFields = cRequiredFields + cFieldName + ","
        cReturnFields   = cReturnFields   + cFieldName + ","
        .
    IF dynParamValue.isActive[idx] THEN
    ASSIGN
        cDisplayFields = cDisplayFields + cFieldName + ","
        cFormats       = cFormats       + REPLACE(dynParamValue.colFormat[idx],",","") + ","
        cLabels        = cLabels        + dynParamValue.colLabel[idx]  + ","
        cSearchFields  = cSearchFields  + cFieldName + ","
        cSortFields    = cSortFields    + cFieldName + ","
        cWidths        = cWidths        + (IF dynParamValue.dataType[idx] EQ "Date" THEN "20" ELSE "") + ","
        .
END. /* do idx */
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
    iRecordLimit,
    ipiSubjectID,
    ipcUserID,
    ipiParamValueID,
    OUTPUT opcReturnValues,
    OUTPUT opcLookupField,
    OUTPUT oprRecID
    ).
