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
DEFINE VARIABLE cWidths         AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtDate          AS DATE      NO-UNDO.
DEFINE VARIABLE hBusinessLogic  AS HANDLE    NO-UNDO.
DEFINE VARIABLE hDynCalcField   AS HANDLE    NO-UNDO.
DEFINE VARIABLE lRunLookup      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE idx             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRecordLimit    AS INTEGER   NO-UNDO.

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

cQueryStr = "FOR".
FOR EACH dynSubjectTable
    WHERE dynSubjectTable.subjectID EQ ipiSubjectID
       BY dynSubjectTable.sortOrder
    :
    cQueryStr = cQueryStr + " "
              + dynSubjectTable.tableFind + " "
              + dynSubjectTable.tableName + " "
              .
    FOR EACH dynSubjectWhere
        WHERE dynSubjectWhere.subjectID  EQ dynSubjectTable.subjectID
          AND dynSubjectWhere.whereTable EQ dynSubjectTable.tableName
           BY dynSubjectWhere.sortOrder
        :
        cQueryStr = cQueryStr + dynSubjectWhere.whereElement + " ".
    END. /* each ttSubjectWhere */
    cQueryStr = TRIM(cQueryStr) + ", ".
END. /* each ttSubjectTable */
cQueryStr = TRIM(cQueryStr,", ").
    
IF INDEX(cQueryStr,"[[") NE 0 THEN
RUN pInitParamValues (cQueryStr).

/* replace [[parameter]] with parameter value */
{AOA/includes/cQueryStr.i cQueryStr}

FOR EACH dynValueColumn NO-LOCK
    WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
      AND dynValueColumn.user-id      EQ dynParamValue.user-id
      AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
      AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
    BY dynValueColumn.sortOrder
    :

    cRequiredFields = cRequiredFields + dynValueColumn.colName + ",".
    
    IF NOT dynValueColumn.isCalcField AND LOOKUP(ENTRY(1, dynValueColumn.colName, "."), cSourceTable) EQ 0 THEN
        cSourceTable = cSourceTable + ENTRY(1, dynValueColumn.colName, ".") + ",".
        
    IF dynValueColumn.sortOrder EQ 1 THEN
        cSourceField = dynValueColumn.colName.
        
    IF dynValueColumn.isActive THEN DO:
        ASSIGN
            cDisplayFields = cDisplayFields + dynValueColumn.colName + ","
            cFormats       = cFormats       + REPLACE(dynValueColumn.colFormat,",","") + ","
            cLabels        = cLabels        + dynValueColumn.colLabel + ","
            cWidths        = cWidths        + (IF dynValueColumn.dataType EQ "Date" THEN "20" ELSE "") + ","
            .
        IF dynValueColumn.isReturnValue THEN
        cReturnFields = cReturnFields + dynValueColumn.colName + ",".
        IF dynValueColumn.isSearchable THEN
        cSearchFields = cSearchFields + dynValueColumn.colName + ",".
        IF dynValueColumn.isSortable THEN
        cSortFields   = cSortFields   + dynValueColumn.colName + ",".
    END. /* if isactive */
END. /* each dynvaluecolumn */

lRunLookup = YES.
IF cSourceTable BEGINS "tt" THEN DO:
    FIND FIRST dynSubject NO-LOCK
         WHERE dynSubject.subjectID     EQ ipiSubjectID
           AND dynSubject.businessLogic NE ""
         NO-ERROR.
    IF AVAILABLE dynSubject THEN DO:
        RUN VALUE(dynSubject.businessLogic) PERSISTENT SET hBusinessLogic.
        IF CAN-DO(hBusinessLogic:INTERNAL-ENTRIES,"pGetFileName") THEN DO:
            RUN pGetFileName IN hBusinessLogic (OUTPUT opcLookupField).
            FOCUS:SCREEN-VALUE = opcLookupField.
            lRunLookup = NO.
        END.
        DELETE PROCEDURE hBusinessLogic.
    END. /* if avail */
END. /* if begins */

IF lRunLookup THEN DO:
    ASSIGN
        cSourceTable    = TRIM(cSourceTable, ",")
        cDisplayFields  = TRIM(cDisplayFields,",")
        cFormats        = TRIM(cFormats,",")
        cLabels         = TRIM(cLabels,",")
        cRequiredFields = TRIM(cRequiredFields,",")
        cReturnFields   = TRIM(cReturnFields,",")
        cSearchFields   = TRIM(cSearchFields,",")
        cSortFields     = TRIM(cSortFields,",")
        cWidths         = SUBSTRING(cWidths,1,LENGTH(cWidths) - 1)
        .
    RUN windows/l-lookup-mult.w (
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
        cQueryStr,
        cReturnFields,
        iRecordLimit,
        ipiSubjectID,
        ipcUserID,
        ipiParamValueID,
        OUTPUT opcReturnValues,
        OUTPUT opcLookupField,
        OUTPUT oprRecID
        ).
END. /* if lRunLookup */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pInitParamValues:
    DEFINE INPUT PARAMETER ipcWhereClause AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cInitItems   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParam       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hDynInitProc AS HANDLE    NO-UNDO.

    DEFINE BUFFER bDynValueParam FOR dynValueParam.

    RUN AOA/spDynInitializeProc.p PERSISTENT SET hDynInitProc.

    FOR EACH dynValueParam NO-LOCK
        WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
          AND dynValueParam.user-id      EQ dynParamValue.user-id
          AND dynValueParam.prgmName     EQ dynParamValue.prgmName
          AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
        :
        cParam = "[[" + dynValueParam.paramName + "]]".
        IF INDEX(ipcWhereClause,cParam) EQ 0 THEN NEXT.
        FOR EACH dynValueParamSet NO-LOCK
            WHERE dynValueParamSet.subjectID    EQ dynValueParam.subjectID
              AND dynValueParamSet.user-id      EQ dynValueParam.user-id
              AND dynValueParamSet.prgmName     EQ dynValueParam.prgmName
              AND dynValueParamSet.paramValueID EQ dynValueParam.paramValueID,
            FIRST dynParamSet NO-LOCK
            WHERE dynParamSet.paramSetID EQ dynValueParamSet.paramSetID,
            FIRST dynParamSetDtl NO-LOCK
            WHERE dynParamSetDtl.paramSetID     EQ dynParamSet.paramSetID
              AND dynParamSetDtl.paramName      EQ dynValueParam.paramName
              AND dynParamSetDtl.initializeProc NE ""
            :
            RUN VALUE(dynParamSetDtl.initializeProc) IN hDynInitProc.
            cInitItems = RETURN-VALUE.
            DO TRANSACTION:
                FIND CURRENT dynValueParam EXCLUSIVE-LOCK.
                dynValueParam.paramValue = cInitItems.
                FIND CURRENT dynValueParam NO-LOCK.
            END. /* do trans */
        END. /* each dynvalueparam */
    END. /* each dynvalueparam */
    
    DELETE PROCEDURE hDynInitProc.

END PROCEDURE.
