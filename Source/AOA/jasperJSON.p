/* jasperJSON.p - rstark - 2.5.2019 */

DEFINE INPUT  PARAMETER iprRowID       AS ROWID     NO-UNDO.
DEFINE INPUT  PARAMETER iphQuery       AS HANDLE    NO-UNDO.
DEFINE INPUT  PARAMETER ipcUserID      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcSubjectName AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcJasperFile  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplOK          AS LOGICAL   NO-UNDO.

DEFINE VARIABLE cBufferValue  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFullName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTableName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE hDynCalcField AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQueryBuf     AS HANDLE    NO-UNDO.
DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.

{AOA/includes/dynFuncs.i}

RUN AOA/spDynCalcField.p PERSISTENT SET hDynCalcField.
SESSION:ADD-SUPER-PROCEDURE (hDynCalcField).

FIND FIRST dynParamValue NO-LOCK WHERE ROWID(dynParamValue) EQ iprRowID NO-ERROR.
IF NOT AVAILABLE dynParamValue THEN RETURN.

iphQuery:QUERY-OPEN.
iphQuery:GET-FIRST().
OS-CREATE-DIR "users".
OS-CREATE-DIR "users\_default".
OS-CREATE-DIR VALUE("users\" + ipcUserID).
OS-CREATE-DIR VALUE("users\" + ipcUserID + "\Jasper").
opcJasperFile = "users\" + ipcUserID + "\"
              + REPLACE(ipcSubjectName," ","")
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
    DO idx = 1 TO EXTENT(dynParamValue.colName):
        IF dynParamValue.colName[idx] EQ "" THEN LEAVE.
        IF dynParamValue.isActive[idx] EQ NO THEN NEXT.
        IF dynParamValue.isCalcField[idx] THEN DO:
            cFieldName = dynParamValue.colName[idx].
            RUN spDynCalcField IN hDynCalcField (
                iphQuery:HANDLE,
                dynParamValue.calcProc[idx],
                dynParamValue.calcParam[idx],
                dynParamValue.dataType[idx],
                dynParamValue.colFormat[idx],
                OUTPUT cBufferValue
                ).
        END. /* if calc field */
        ELSE
        ASSIGN
            hQueryBuf    = iphQuery:GET-BUFFER-HANDLE(ENTRY(1,dynParamValue.colName[idx],"."))
            cFullName    = ENTRY(2,dynParamValue.colName[idx],".")
            cFieldName   = IF INDEX(cFullName,"[") EQ 0 THEN cFullName
                           ELSE SUBSTRING(cFullName,1,INDEX(cFullName,"[") - 1)
            cBufferValue = fFormatValue(hQueryBuf, cFullName)
            cBufferValue = DYNAMIC-FUNCTION("sfWebCharacters", cBufferValue, 6, "Web")
            cFullName    = REPLACE(cFullName,"[","")
            cFullName    = REPLACE(cFullName,"]","")
            .
        IF idx GT 1 THEN
        PUT UNFORMATTED "," SKIP.
        PUT UNFORMATTED
            FILL(" ",8)
            "~"" cFullName "~": ~""
            IF cBufferValue NE "" THEN cBufferValue ELSE " "
            "~""
            .
    END. /* do idx */
    PUT UNFORMATTED SKIP FILL(" ",6) "}".
    iphQuery:GET-NEXT().
    IF iphQuery:QUERY-OFF-END THEN LEAVE.
    PUT UNFORMATTED "," SKIP.
END. /* repeat */
PUT UNFORMATTED
    SKIP
    FILL(" ",4) "]" SKIP
    FILL(" ",2) "}" SKIP
    "}" SKIP
    .
OUTPUT CLOSE.
oplOK = TRUE.
iphQuery:QUERY-CLOSE().
