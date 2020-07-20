/* jasperJSON.p - rstark - 2.5.2019 */

DEFINE INPUT  PARAMETER iprRowID       AS ROWID     NO-UNDO.
DEFINE INPUT  PARAMETER iphQuery       AS HANDLE    NO-UNDO.
DEFINE INPUT  PARAMETER ipcUserID      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcSubjectName AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTaskRecKey  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcJasperFile  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplOK          AS LOGICAL   NO-UNDO.

DEFINE VARIABLE cBufferValue  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFullName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJasonName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTableName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE hDynCalcField AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQueryBuf     AS HANDLE    NO-UNDO.

{AOA/includes/dynFuncs.i}

RUN AOA/spDynCalcField.p PERSISTENT SET hDynCalcField.
SESSION:ADD-SUPER-PROCEDURE (hDynCalcField).

FIND FIRST dynParamValue NO-LOCK
     WHERE ROWID(dynParamValue) EQ iprRowID
     NO-ERROR.
IF NOT AVAILABLE dynParamValue THEN RETURN.

iphQuery:QUERY-OPEN.
iphQuery:GET-FIRST().
OS-CREATE-DIR "users".
OS-CREATE-DIR "users\_default".
OS-CREATE-DIR VALUE("users\" + ipcUserID).
OS-CREATE-DIR VALUE("users\" + ipcUserID + "\Jasper").
ASSIGN
    cJasonName    = REPLACE(ipcSubjectName," ","") + "." + ipcTaskRecKey
    opcJasperFile = "users\" + ipcUserID + "\"
                  + cJasonName
                  + ".json"
                  .
OUTPUT TO VALUE(opcJasperFile).
PUT UNFORMATTED
    "~{" SKIP
    FILL(" ",2)
    "~"" REPLACE(ipcSubjectName," ","_") "~": ~{" SKIP
    FILL(" ",4)
    "~"" REPLACE(ipcSubjectName," ","") "~": [" SKIP
    .
IF NOT iphQuery:QUERY-OFF-END THEN
REPEAT:
    PUT UNFORMATTED
        FILL(" ",6) "~{" SKIP
        .
    FOR EACH dynValueColumn NO-LOCK
        WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
          AND dynValueColumn.user-id      EQ dynParamValue.user-id
          AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
          AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
           BY dynValueColumn.sortOrder
        :
        IF dynValueColumn.isCalcField THEN DO:
            cFullName = dynValueColumn.colName.
            IF dynValueColumn.calcProc NE "" THEN
            RUN spDynCalcField IN hDynCalcField (
                iphQuery:HANDLE,
                dynValueColumn.calcProc,
                dynValueColumn.calcParam,
                dynValueColumn.dataType,
                dynValueColumn.colFormat,
                OUTPUT cBufferValue
                ).
            ELSE
            IF dynValueColumn.calcFormula NE "" AND
               INDEX(dynValueColumn.calcFormula,"$") EQ 0 THEN
            RUN spDynCalcField IN hDynCalcField (
                iphQuery:HANDLE,
                "Calculator",
                dynValueColumn.calcFormula,
                dynValueColumn.dataType,
                dynValueColumn.colFormat,
                OUTPUT cBufferValue
                ).
            ELSE
            IF dynValueColumn.calcFormula NE "" THEN NEXT.
        END. /* if calc field */
        ELSE
        ASSIGN
            hQueryBuf    = iphQuery:GET-BUFFER-HANDLE(ENTRY(1,dynValueColumn.colName,"."))
            cFieldName   = ENTRY(2,dynValueColumn.colName,".")
            cBufferValue = fFormatValue(hQueryBuf, cFieldName)
            cBufferValue = DYNAMIC-FUNCTION("sfWebCharacters", cBufferValue, 8, "Web")
            cFullName    = REPLACE(dynValueColumn.colName,".","__")
            cFullName    = REPLACE(cFullName,"[","")
            cFullName    = REPLACE(cFullName,"]","")
            .
        IF dynValueColumn.sortOrder GT 1 THEN
        PUT UNFORMATTED "," SKIP.
        PUT UNFORMATTED
            FILL(" ",8)
            "~"" cFullName "~": ~""
            IF cBufferValue NE "" THEN cBufferValue ELSE " "
            "~""
            .
    END. /* each dynvaluecolumn */
    PUT UNFORMATTED SKIP FILL(" ",6) "}".
    iphQuery:GET-NEXT().
    IF iphQuery:QUERY-OFF-END THEN LEAVE.
    PUT UNFORMATTED "," SKIP.
END. /* repeat */
ELSE
PUT UNFORMATTED
    FILL(" ",8) "~{" SKIP
    FILL(" ",12)
    "~"NoDataMessage~": ~"No Data Exists for the Selected Parameter Value(s)~"" SKIP
    FILL(" ",8) "}" SKIP
    .
PUT UNFORMATTED
    SKIP
    FILL(" ",4) "]" SKIP
    FILL(" ",2) "}" SKIP
    "}" SKIP
    .
OUTPUT CLOSE.
oplOK = TRUE.
iphQuery:QUERY-CLOSE().

DELETE PROCEDURE hDynCalcField.
