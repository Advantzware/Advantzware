/* dynQuery.p - rstark - 2.5.2019 */

DEFINE INPUT  PARAMETER iprRowID     AS ROWID     NO-UNDO.
DEFINE INPUT  PARAMETER ipcQueryStr  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableName AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ophQuery     AS HANDLE    NO-UNDO.
DEFINE OUTPUT PARAMETER oplOK        AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcError     AS CHARACTER NO-UNDO.

DEFINE VARIABLE cDate         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cParam        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQueryStr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtDate        AS DATE      NO-UNDO.
DEFINE VARIABLE hBuffer       AS HANDLE    NO-UNDO EXTENT 200.
DEFINE VARIABLE hDynCalcField AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQuery        AS HANDLE    NO-UNDO.
DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
DEFINE VARIABLE lOK           AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE ttSortBy NO-UNDO
    FIELD ttOrder      AS INTEGER 
    FIELD ttSortBy     AS CHARACTER 
    FIELD ttDescending AS LOGICAL
        INDEX ttSortBy IS PRIMARY
            ttOrder
            .
RUN AOA/spDynCalcField.p PERSISTENT SET hDynCalcField.

FIND FIRST dynParamValue NO-LOCK WHERE ROWID(dynParamValue) EQ iprRowID NO-ERROR.
IF NOT AVAILABLE dynParamValue THEN RETURN.

/* replace [[parameter]] with parameter value */
{AOA/includes/cQueryStr.i ipcQueryStr}

EMPTY TEMP-TABLE ttSortBy.
DO idx = 1 TO EXTENT(dynParamValue.colName):
    IF dynParamValue.colName[idx] EQ "" THEN LEAVE.
    IF dynParamValue.sortCol[idx] EQ 0  THEN NEXT.
    CREATE ttSortBy.        
    ASSIGN 
        ttSortBy.ttOrder      = dynParamValue.sortCol[idx]
        ttSortBy.ttSortBy     = dynParamValue.colName[idx]
        ttSortBy.ttDescending = dynParamValue.sortDescending[idx]
        .
END. /* do idx */

FOR EACH ttSortBy BY ttSortBy.ttOrder:
    cQueryStr = cQueryStr + " BY " + ttSortBy.ttSortBy
              + IF ttSortBy.ttDescending THEN " DESCENDING" ELSE ""
              .
END. /* each ttSortBy */

RUN pGetWhereCalcFields (INPUT-OUTPUT cQueryStr).
CREATE QUERY hQuery.
DO idx = 1 TO NUM-ENTRIES(ipcTableName):
    CREATE BUFFER hBuffer[idx] FOR TABLE ENTRY(idx,ipcTableName).
    hQuery:ADD-BUFFER(hBuffer[idx]).
END. /* do idx */
oplOK = hQuery:QUERY-PREPARE(cQueryStr) NO-ERROR.
IF oplOK THEN
ophQuery = hQuery:HANDLE.
ELSE
DO idx = 1 TO ERROR-STATUS:NUM-MESSAGES:
    opcError = opcError + ERROR-STATUS:GET-MESSAGE(idx) + CHR(10).
END. /* do idx */

PROCEDURE pGetWhereCalcFields:
    DEFINE INPUT-OUTPUT PARAMETER iopcQueryStr AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cBufferValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCalcField   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQueryStr    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE edx          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE sdx          AS INTEGER   NO-UNDO.
    
    cQueryStr = iopcQueryStr.
    DO WHILE TRUE:
        sdx = INDEX(ipcQueryStr,"[|").
        IF sdx EQ 0 THEN LEAVE.
        ASSIGN
            edx = INDEX(ipcQueryStr,"|]")
            cCalcField = SUBSTR(ipcQueryStr,sdx,edx - sdx + 2)
            ipcQueryStr = SUBSTR(ipcQueryStr,edx + 2)
            .
        FIND FIRST dynSubjectWhere NO-LOCK
             WHERE dynSubjectWhere.subjectID    EQ dynParamValue.subjectID
               AND dynSubjectWhere.whereElement EQ cCalcField
             NO-ERROR.
        IF NOT AVAILABLE dynSubjectWhere THEN NEXT.
        cParamValue = FILL("|",NUM-ENTRIES(dynSubjectWhere.calcParam,"|") - 1).
        DO jdx = 1 TO NUM-ENTRIES(dynSubjectWhere.calcParam,"|"):
            ASSIGN
                cParamField = ENTRY(jdx,dynSubjectWhere.calcParam,"|")
                cParamField = REPLACE(cParamField,"[[","")
                cParamField = REPLACE(cParamField,"]]","")
                .
            DO idx = 1 TO EXTENT(dynParamValue.paramName):
                IF dynParamValue.paramName[idx] EQ "" THEN LEAVE.
                IF dynParamValue.paramName[idx] EQ cParamField THEN DO:
                    ENTRY(jdx,cParamValue,"|") = dynParamValue.paramValue[idx].
                    LEAVE.
                END. /* if cparamfield */
            END. /* do idx */
        END. /* do jdx */
        RUN spDynCalcField IN hDynCalcField (
            ?,
            dynSubjectWhere.calcProc,
            cParamValue,
            "",
            "",
            OUTPUT cBufferValue
            ).
        cQueryStr = REPLACE(cQueryStr,cCalcField,cBufferValue).
    END. /* do while */
    iopcQueryStr = cQueryStr.
END PROCEDURE.
