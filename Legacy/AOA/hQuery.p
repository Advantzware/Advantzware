/* hQuery.p - rstark - 2.5.2019 */

DEFINE INPUT  PARAMETER iprRowID     AS ROWID     NO-UNDO.
DEFINE INPUT  PARAMETER ipcQueryStr  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableName AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ophQuery     AS HANDLE    NO-UNDO.
DEFINE OUTPUT PARAMETER oplOK        AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcError     AS CHARACTER NO-UNDO.

DEFINE VARIABLE cDate     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cParam    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQueryStr AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtDate    AS DATE      NO-UNDO.
DEFINE VARIABLE hBuffer   AS HANDLE    NO-UNDO EXTENT 1000.
DEFINE VARIABLE hQuery    AS HANDLE    NO-UNDO.
DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lOK       AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE ttSortBy NO-UNDO
    FIELD ttOrder  AS INTEGER 
    FIELD ttSortBy AS CHARACTER 
        INDEX ttSortBy IS PRIMARY
            ttOrder
            .
FIND FIRST dynParamValue NO-LOCK WHERE ROWID(dynParamValue) EQ iprRowID NO-ERROR.
IF NOT AVAILABLE dynParamValue THEN RETURN.

cQueryStr = ipcQueryStr.
IF INDEX(ipcQueryStr,"[[") NE 0 THEN
DO idx = 1 TO EXTENT(dynParamValue.paramName):
    IF dynParamValue.paramName[idx] EQ "" THEN LEAVE.
    cParam = "[[" + dynParamValue.paramName[idx] + "]]".
    IF INDEX(cQueryStr,cParam) NE 0 THEN
    CASE dynParamValue.paramDataType[idx]:
        WHEN "Character" THEN DO:
        cQueryStr = REPLACE(cQueryStr,cParam,"~"" + dynParamValue.paramValue[idx] + "~"").
        END.
        WHEN "Date" THEN DO:
            dtDate = DATE(dynParamValue.paramValue[idx]) NO-ERROR.
            ASSIGN
                cDate     = IF dtDate EQ ? THEN "~?" ELSE STRING(dtDate,dynParamValue.paramFormat[idx])
                cQueryStr = REPLACE(cQueryStr,cParam,cDate)
                .
        END. /* date */
        WHEN "DateTime" THEN DO:
            dtDate = DATE(dynParamValue.paramValue[idx]) NO-ERROR.
            ASSIGN
                cDate     = IF dtDate EQ ? THEN "~?" ELSE STRING(dtDate,dynParamValue.paramFormat[idx])
                cQueryStr = REPLACE(cQueryStr,cParam,cDate)
                cQueryStr = REPLACE(cQueryStr,cParam,dynParamValue.paramValue[idx])
                .
        END. /* date */
        WHEN "Decimal" OR WHEN "Integer" OR WHEN "Logical" THEN
        cQueryStr = REPLACE(cQueryStr,cParam,dynParamValue.paramValue[idx]).
    END CASE.
END. /* if [[ (parameter used) */

EMPTY TEMP-TABLE ttSortBy.
DO idx = 1 TO EXTENT(dynParamValue.colName):
    IF dynParamValue.colName[idx] EQ "" THEN LEAVE.
    IF dynParamValue.sortCol[idx] EQ 0  THEN NEXT.
    CREATE ttSortBy.        
    ASSIGN 
        ttSortBy.ttOrder  = dynParamValue.sortCol[idx]
        ttSortBy.ttSortBy = dynParamValue.colName[idx]
        .
END. /* do idx */

FOR EACH ttSortBy BY ttSortBy.ttOrder:
    cQueryStr = cQueryStr + " BY " + ttSortBy.ttSortBy.
END. /* each ttSortBy */

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
