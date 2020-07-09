/* dynSubjectCheck.p - rstark - 6.26.2020 */

DEFINE OUTPUT PARAMETER oplErrors AS LOGICAL NO-UNDO.

DEFINE VARIABLE cFieldName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTableName  AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttTable NO-UNDO
    FIELD subjectID LIKE dynSubject.subjectID
    FIELD tableName AS CHARACTER FORMAT "x(30)" LABEL "Table"
        INDEX ttTable IS PRIMARY subjectID tableName
    .
DEFINE TEMP-TABLE ttField NO-UNDO
    FIELD subjectID LIKE dynSubject.subjectID
    FIELD errorType AS CHARACTER FORMAT "X(6)"  LABEL "Type"
    FIELD tableName AS CHARACTER FORMAT "x(30)" LABEL "Table"
    FIELD fieldName AS CHARACTER FORMAT "x(30)" LABEL "Field"
        INDEX ttField IS PRIMARY subjectID tableName fieldName
    .
FOR EACH dynSubject NO-LOCK
    WHERE dynSubject.businessLogic EQ "",
    FIRST dynSubjectTable OF dynSubject NO-LOCK
    :
    FOR EACH dynSubjectColumn OF dynSubject NO-LOCK
        WHERE dynSubjectColumn.isCalc EQ NO
        :
        ASSIGN
            cTableName = ENTRY(1,dynSubjectColumn.fieldName,".")
            cFieldName = ENTRY(2,dynSubjectColumn.fieldName,".")
            .
        RUN pValidate (dynSubject.subjectID, "Column", cTableName, cFieldName).
    END. /* each dynsubjectcolumn */
    FOR EACH dynSubjectWhere OF dynSubject NO-LOCK
        WHERE INDEX(dynSubjectWhere.whereElement,".") NE 0
        :
        IF ENTRY(1,dynSubjectWhere.tableName," ") EQ "OF" THEN NEXT.
        ASSIGN
            cTableName = ENTRY(1,dynSubjectWhere.whereElement,".")
            cFieldName = ENTRY(2,dynSubjectWhere.whereElement,".")
            .
        RUN pValidate (dynSubject.subjectID, "Where", cTableName, cFieldName).
    END. /* each dynsubjectcolumn */
END. /* each dynsubject */

OUTPUT TO c:\tmp\DynSubjectErrors.txt.
IF CAN-FIND(FIRST ttTable) OR CAN-FIND(FIRST ttField) THEN DO:
    PUT UNFORMATTED
        "*** Dynamic Subject Missing Table and Field Report ***" SKIP
        "------------------------------------------------------" SKIP
        .
    FOR EACH ttTable
        BREAK BY ttTable.subjectID
        WITH STREAM-IO:
        IF FIRST-OF(ttTable.subjectID) THEN
        DISPLAY ttTable.subjectID.
        DISPLAY ttTable.tableName.
    END. /* each tttable */
    
    FOR EACH ttField
        BREAK BY ttField.subjectID
              BY ttField.errorType
              BY ttField.tableName
        WITH STREAM-IO WIDTH 100:
        IF FIRST-OF(ttField.subjectID) THEN
        DISPLAY ttField.subjectID.
        IF FIRST-OF(ttField.errorType) THEN
        DISPLAY ttField.errorType.
        IF FIRST-OF(ttField.tableName) THEN
        DISPLAY ttField.tableName.
        DISPLAY ttField.fieldName.
    END. /* each ttfield */
    PUT UNFORMATTED SKIP(1)
        "******************************************************" SKIP
        "******************************************************" SKIP
        "******************************************************" SKIP(1)
        .
    oplErrors = YES.
/*    OS-COMMAND NO-WAIT notepad.exe c:\tmp\DynSubjectErrors.txt.*/
END. /* if can-find */
OUTPUT CLOSE.

PROCEDURE pCreateField:
    DEFINE INPUT PARAMETER ipiSubjectID AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcErrorType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTableName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFieldName AS CHARACTER NO-UNDO.

    IF CAN-FIND(FIRST ttField
                WHERE ttField.tableName EQ ipcTableName
                  AND ttField.fieldName EQ ipcFieldName) THEN
    RETURN.
    CREATE ttField.
    ASSIGN
        ttField.subjectID = ipiSubjectID
        ttField.errorType = ipcErrorType
        ttField.tableName = ipcTableName
        ttField.fieldName = ipcFieldName
        .
END PROCEDURE.

PROCEDURE pCreateTable:
    DEFINE INPUT PARAMETER ipiSubjectID AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcTableName AS CHARACTER NO-UNDO.

    IF CAN-FIND(FIRST ttTable
                WHERE ttTable.tableName EQ ipcTableName) THEN
    RETURN.
    CREATE ttTable.
    ASSIGN
        ttTable.subjectID = ipiSubjectID
        ttTable.tableName = ipcTableName
        .
END PROCEDURE.

PROCEDURE pValidate:
    DEFINE INPUT PARAMETER ipiSubjectID AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcErrorType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTableName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFieldName AS CHARACTER NO-UNDO.

    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    idx = INDEX(ipcFieldName,"[").
    /* strip off array extent */
    IF idx NE 0 THEN
    ipcFieldName = SUBSTRING(ipcFieldName,1,idx - 1).
    FIND FIRST ASI._file NO-LOCK
         WHERE ASI._file._file-name EQ ipcTableName
         NO-ERROR.
    IF AVAILABLE ASI._file THEN DO:
        IF NOT CAN-FIND(FIRST ASI._field OF ASI._file
                        WHERE ASI._field._field-name EQ ipcFieldName) THEN
        RUN pCreateField (ipiSubjectID, ipcErrorType, ipcTableName, ipcFieldName).
    END. /* if avail */
    ELSE DO:
        FIND FIRST Audit._file
             WHERE Audit._file._file-name EQ ipcTableName
             NO-ERROR.
        IF AVAILABLE Audit._file THEN DO:
            IF NOT CAN-FIND(FIRST Audit._field OF Audit._file
                            WHERE Audit._field._field-name EQ cFieldName) THEN
            RUN pCreateField (ipiSubjectID, ipcErrorType, ipcTableName, ipcFieldName).
        END. /* if avail */
        ELSE
        RUN pCreateTable (ipiSubjectID, ipcTableName).
    END. /* else */
END PROCEDURE.
