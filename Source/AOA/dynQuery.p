/* dynQuery.p - rstark - 2.5.2019 */

DEFINE INPUT  PARAMETER iprRowID       AS ROWID     NO-UNDO.
DEFINE INPUT  PARAMETER ipcQueryStr    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableName   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiRecordLimit AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER ophQuery       AS HANDLE    NO-UNDO.
DEFINE OUTPUT PARAMETER oplOK          AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcError       AS CHARACTER NO-UNDO.

DEFINE VARIABLE cCompany       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustListField AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDate          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustList   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cParam         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQueryStr      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustList AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtDate         AS DATE      NO-UNDO.
DEFINE VARIABLE hBuffer        AS HANDLE    NO-UNDO EXTENT 200.
DEFINE VARIABLE hDynCalcField  AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQuery         AS HANDLE    NO-UNDO.
DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
DEFINE VARIABLE lUseCustList   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lOK            AS LOGICAL   NO-UNDO.

{sys/ref/CustList.i NEW}
{AOA/BL/pBuildCustList.i}

RUN AOA/spDynCalcField.p PERSISTENT SET hDynCalcField.

FIND FIRST dynParamValue NO-LOCK
     WHERE ROWID(dynParamValue) EQ iprRowID
     NO-ERROR.
IF NOT AVAILABLE dynParamValue THEN RETURN.

IF dynParamValue.useCustList OR dynParamValue.CustListID NE "" THEN DO:
    FIND FIRST dynValueColumn NO-LOCK
         WHERE dynValueColumn.subjectID     EQ dynParamValue.subjectID
           AND dynValueColumn.user-id       EQ dynParamValue.user-id
           AND dynValueColumn.prgmName      EQ dynParamValue.prgmName
           AND dynValueColumn.paramValueID  EQ dynParamValue.paramValueID
           AND dynValueColumn.CustListField EQ YES
         NO-ERROR.
    IF AVAILABLE dynValueColumn THEN DO:    
        RUN spGetSessionParam ("Company", OUTPUT cCompany).
        RUN pBuildCustList (
            cCompany,
            dynParamValue.CustListID,
            OUTPUT cStartCustList,
            OUTPUT cEndCustList,
            OUTPUT lUseCustList
            ).
        IF lUseCustList THEN
        ASSIGN
            cCustListField = dynValueColumn.colName
            ipcTableName   = ipcTableName + ",ttCustList"
            .
    END. /* if avail */
END. /* if custlistid */

/* replace [[parameter]] with parameter value */
{AOA/includes/cQueryStr.i ipcQueryStr}

IF lUseCustList THEN
cQueryStr = cQueryStr
          + ", FIRST ttCustList WHERE ttCustList.cust-no EQ "
          + cCustListField
          + " AND ttCustList.log-fld EQ YES"
          .

/* append sort by option to query */
RUN AOA/dynSortBy.p (BUFFER dynParamValue, INPUT-OUTPUT cQueryStr).

RUN pGetWhereCalcFields (INPUT-OUTPUT cQueryStr).
IF ipiRecordLimit NE 0 THEN
cQueryStr = cQueryStr + " MAX-ROWS " + STRING(ipiRecordLimit).
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

DELETE PROCEDURE hDynCalcField.

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
            edx         = INDEX(ipcQueryStr,"|]")
            cCalcField  = SUBSTR(ipcQueryStr,sdx,edx - sdx + 2)
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
            FIND FIRST dynValueParam NO-LOCK
                 WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
                   AND dynValueParam.user-id      EQ dynParamValue.user-id
                   AND dynValueParam.prgmName     EQ dynParamValue.prgmName
                   AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
                   AND dynValueParam.paramName    EQ cParamField
                 NO-ERROR.
            IF NOT AVAILABLE dynValueParam THEN NEXT.
            ENTRY(jdx,cParamValue,"|") = dynValueParam.paramValue.
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
