/* jasperJSON.p - rstark - 2.5.2019 */

DEFINE INPUT  PARAMETER iprRowID       AS ROWID     NO-UNDO.
DEFINE INPUT  PARAMETER iphQuery       AS HANDLE    NO-UNDO.
DEFINE INPUT  PARAMETER ipcUserID      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcSubjectName AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTaskRecKey  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplProgressBar AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcJasperFile  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcRecipient   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplOK          AS LOGICAL   NO-UNDO.

DEFINE VARIABLE cBufferValue  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFullName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJasonName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJasperFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRecipient    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTableName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE hDynCalcField AS HANDLE    NO-UNDO.
DEFINE VARIABLE hMailProcs    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQueryBuf     AS HANDLE    NO-UNDO.
DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNumResults   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRecordCount  AS INTEGER   NO-UNDO.

DEFINE STREAM sJasperJSON.

{AOA/includes/dynFuncs.i}

RUN AOA/spDynCalcField.p PERSISTENT SET hDynCalcField.
SESSION:ADD-SUPER-PROCEDURE (hDynCalcField).
RUN system/MailProcs.p PERSISTENT SET hMailProcs.

FIND FIRST dynParamValue NO-LOCK
     WHERE ROWID(dynParamValue) EQ iprRowID
     NO-ERROR.
IF NOT AVAILABLE dynParamValue THEN RETURN.

RUN pClearTaskFiles. /* spJasper.p */

OS-CREATE-DIR "users".
OS-CREATE-DIR "users\_default".
OS-CREATE-DIR VALUE("users\" + ipcUserID).
OS-CREATE-DIR VALUE("users\" + ipcUserID + "\Jasper").
iphQuery:QUERY-OPEN.
{AOA/includes/iNumResults.i}
IF iNumResults GT 0 THEN DO:
    iphQuery:GET-FIRST().
    IF NOT iphQuery:QUERY-OFF-END THEN
    REPEAT:
        iRecordCount = iRecordCount + 1.
        IF dynParamValue.onePer OR iRecordCount EQ 1 THEN
        RUN pTaskFile (iRecordCount, OUTPUT cJasperFile).
        opcJasperFile = opcJasperFile + cJasperFile + ",".
        IF iplProgressBar THEN
        RUN spProgressBar (ipcSubjectName, iRecordCount, iNumResults).
        PUT STREAM sJasperJSON UNFORMATTED
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
                cBufferValue = fFormatValue(hQueryBuf, cFieldName, dynValueColumn.colFormat)
                cBufferValue = REPLACE(cBufferValue,"~"","''")
                cBufferValue = REPLACE(cBufferValue,"~\","~\~\")
                cBufferValue = REPLACE(cBufferValue,CHR(10),"~\n")
                cFullName    = REPLACE(dynValueColumn.colName,".","__")
                cFullName    = REPLACE(cFullName,"[","")
                cFullName    = REPLACE(cFullName,"]","")
                .
            IF dynParamValue.formType NE "" AND
               dynParamValue.onePer AND
               dynValueColumn.isFormField THEN DO:
                RUN pFormEmail (
                    dynParamValue.formType,
                    cBufferValue,
                    OUTPUT cRecipient
                    ).
                opcRecipient = opcRecipient + cRecipient + ",".
            END. /* if a form type subject */
            IF dynParamValue.outputFormat EQ "HTML" THEN
            cBufferValue = DYNAMIC-FUNCTION("sfWebCharacters", cBufferValue, 8, "Web").
            /* handle how jasper auto multiplies % formatted fields by 100 */
            IF INDEX(dynValueColumn.colFormat,"%") NE 0 THEN
            ASSIGN
                cBufferValue = REPLACE(cBufferValue,"%","")
                cBufferValue = STRING(DECIMAL(cBufferValue) / 100)
                cBufferValue = cBufferValue + "%"
                .
            IF dynValueColumn.sortOrder GT 1 THEN
            PUT STREAM sJasperJSON UNFORMATTED "," SKIP.
            PUT STREAM sJasperJSON UNFORMATTED
                FILL(" ",8)
                "~"" cFullName "~": ~""
                IF cBufferValue EQ ? AND dynValueColumn.dataType EQ "Character" THEN " "
                ELSE IF cBufferValue EQ ? AND dynValueColumn.dataType NE "Character" THEN "0"
                ELSE IF cBufferValue NE "" THEN cBufferValue ELSE " "
                "~""
                .
        END. /* each dynvaluecolumn */
        PUT STREAM sJasperJSON UNFORMATTED SKIP FILL(" ",6) "}".
        iphQuery:GET-NEXT().
        IF iphQuery:QUERY-OFF-END THEN LEAVE.
        IF dynParamValue.onePer AND iRecordCount GT 0 THEN DO:
            PUT STREAM sJasperJSON UNFORMATTED
                SKIP
                FILL(" ",4) "]"
                .
            RUN pSubDataSet ("Detail").
            RUN pSubDataSet ("Summary").
            PUT STREAM sJasperJSON UNFORMATTED
                SKIP
                FILL(" ",2) "}" SKIP
                "}" SKIP
                .
            OUTPUT STREAM sJasperJSON CLOSE.
        END.
        ELSE
        PUT STREAM sJasperJSON UNFORMATTED "," SKIP.
    END. /* repeat */
    PUT STREAM sJasperJSON UNFORMATTED
        SKIP
        FILL(" ",4) "]"
        .
    RUN pSubDataSet ("Detail").
    RUN pSubDataSet ("Summary").
END. /* inumresults gt 0 */
ELSE DO:
    RUN pTaskFile (0).
    PUT STREAM sJasperJSON UNFORMATTED
        FILL(" ",8) "~{" SKIP
        FILL(" ",12)
        "~"NoDataMessage~": ~"No Data Exists for the Selected Parameter Value(s)~"" SKIP
        FILL(" ",8) "}" SKIP
        FILL(" ",4) "]"
        .
END. /* else */
PUT STREAM sJasperJSON UNFORMATTED
    SKIP
    FILL(" ",2) "}" SKIP
    "}" SKIP
    .
OUTPUT STREAM sJasperJSON CLOSE.
ASSIGN
    opcJasperFile = TRIM(opcJasperFile,",")
    opcRecipient  = TRIM(opcRecipient,",")
    oplOK         = TRUE
    .
iphQuery:QUERY-CLOSE().
DELETE PROCEDURE hDynCalcField.
DELETE PROCEDURE hMailProcs.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pFormEmail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFormType    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBufferValue AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRecipient   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ipcCode  AS CHARACTER NO-UNDO.

    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    CASE ipcFormType:
        WHEN "Customer" THEN
        RUN pCustomer IN hMailProcs (
            ipcFormType,
            cCompany,
            ipcBufferValue,
            ipcCode,
            OUTPUT opcRecipient
            ).
        WHEN "Loc" THEN   
        RUN pLoc IN hMailProcs (
            ipcFormType,
            cCompany,
            ipcBufferValue,
            OUTPUT opcRecipient
            ).
        WHEN "SalesRep" THEN
        RUN pSalesRep IN hMailProcs (
            ipcFormType,
            cCompany,
            ipcBufferValue,
            ipcCode,
            OUTPUT opcRecipient
            ).
        WHEN "ShipTo" THEN
        RUN pShipTo IN hMailProcs (
            ipcFormType,
            cCompany,
            ipcBufferValue,
            ipcCode,
            OUTPUT opcRecipient
            ).
        WHEN "SoldTo" THEN  
        RUN pSoldTo IN hMailProcs (
            ipcFormType,
            cCompany,
            ipcBufferValue,
            ipcCode,
            OUTPUT opcRecipient
            ).
        WHEN "Vendor" THEN
        RUN pVendor IN hMailProcs (
            ipcFormType,
            cCompany,
            ipcBufferValue,
            ipcCode,
            OUTPUT opcRecipient
            ).
    END CASE.

END PROCEDURE.

PROCEDURE pSubDataSet:
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDataType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormat   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHandle   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTables   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWhere    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hQuery    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQueryBuf AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iField    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFieldIdx AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTables   AS INTEGER   NO-UNDO.

    RUN spGetSessionParam (ipcType + "Tables", OUTPUT cTables).
    iTables = INTEGER(cTables).
    IF iTables NE 0 THEN
    DO idx = 1 TO iTables:
        RUN spGetSessionParam (ipcType + "Handle" + STRING(idx), OUTPUT cHandle).
        hTable = WIDGET-HANDLE(cHandle).
        IF NOT VALID-HANDLE(hTable) THEN NEXT.
        PUT STREAM sJasperJSON UNFORMATTED
            "," SKIP
            FILL(" ",4)
            "~"" ipcType STRING(idx) "~": [" SKIP
            .
        /* scroll returned temp-table records */
        cWhere = "FOR EACH " + hTable:NAME.
        IF dynParamValue.onePer AND iRecordCount GT 0 THEN
        cWhere = cWhere + " WHERE " + hTable:NAME + ".recordID EQ "
               + STRING(iRecordCount)
               .
        CREATE QUERY hQuery.
        hTable = hTable:DEFAULT-BUFFER-HANDLE.
        hQuery:SET-BUFFERS(hTable:HANDLE).
        hQuery:QUERY-PREPARE(cWhere).
        hQuery:QUERY-OPEN.
        hQueryBuf = hQuery:GET-BUFFER-HANDLE(hTable:NAME).
        hQuery:GET-FIRST().
        IF NOT hQuery:QUERY-OFF-END THEN
        REPEAT:
            IF hQueryBuf:BUFFER-FIELD("RowType"):BUFFER-VALUE() NE "Data" THEN NEXT.
            iField = 0.
            DO iFieldIdx = 1 TO hTable:NUM-FIELDS:
                IF CAN-DO("rowType,parameters,recDataType",hTable:BUFFER-FIELD(iFieldIdx):NAME) THEN NEXT.
                IF hTable:BUFFER-FIELD(iFieldIdx):NAME BEGINS "xx" THEN NEXT.
                ASSIGN
                    iField       = iField + 1
                    cFieldName   = hTable:BUFFER-FIELD(iFieldIdx):NAME
                    cFormat      = hTable:BUFFER-FIELD(iFieldIdx):FORMAT
                    cBufferValue = hTable:BUFFER-FIELD(iFieldIdx):BUFFER-VALUE()
                    cBufferValue = REPLACE(cBufferValue,"~"","''")
                    cBufferValue = REPLACE(cBufferValue,"~\","~\~\")
                    cBufferValue = REPLACE(cBufferValue,CHR(10)," ")
                    cFullName    = hTable:NAME + "__" + cFieldName
                    cFullName    = REPLACE(cFullName,"[","")
                    cFullName    = REPLACE(cFullName,"]","")
                    .
                IF dynParamValue.outputFormat EQ "HTML" THEN
                cBufferValue = DYNAMIC-FUNCTION("sfWebCharacters", cBufferValue, 8, "Web").
                /* handle how jasper auto multiplies % formatted fields by 100 */
                IF INDEX(cFormat,"%") NE 0 THEN
                ASSIGN
                    cBufferValue = REPLACE(cBufferValue,"%","")
                    cBufferValue = STRING(DECIMAL(cBufferValue) / 100)
                    cBufferValue = cBufferValue + "%"
                    .
                IF iField EQ 1 THEN
                PUT STREAM sJasperJSON UNFORMATTED
                    FILL(" ",6) "~{" SKIP
                    .
                IF iField GT 1 THEN
                PUT STREAM sJasperJSON UNFORMATTED "," SKIP.
                PUT STREAM sJasperJSON UNFORMATTED
                    FILL(" ",8)
                    "~"" cFullName "~": ~""
                    IF cBufferValue EQ ? AND cDataType EQ "Character" THEN " "
                    ELSE IF cBufferValue EQ ? AND cDataType NE "Character" THEN "0"
                    ELSE IF cBufferValue NE "" THEN cBufferValue ELSE " "
                    "~""
                    .
            END. /* do iFieldIdx */
            PUT STREAM sJasperJSON UNFORMATTED SKIP FILL(" ",6) "}".
            hQuery:GET-NEXT().
            IF hQuery:QUERY-OFF-END THEN LEAVE.
            PUT STREAM sJasperJSON UNFORMATTED "," SKIP.
        END. /* repeat */
        PUT STREAM sJasperJSON UNFORMATTED
            SKIP
            FILL(" ",4) "]"
            .
        hQuery:QUERY-CLOSE().
        DELETE OBJECT hQuery.
        DELETE OBJECT hQueryBuf.
    END. /* do idx */

END PROCEDURE.

PROCEDURE pTaskFile:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiRecordID   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJasperFile AS CHARACTER NO-UNDO.

    ASSIGN
        cJasonName    = REPLACE(ipcSubjectName," ","") + "." + ipcTaskRecKey
                      + IF NOT dynParamValue.onePer THEN ""
                   ELSE IF ipiRecordID NE 0 THEN STRING(ipiRecordId)
                   ELSE ""
        opcJasperFile = "users\" + ipcUserID + "\"
                      + cJasonName
                      + ".json"
                      .
    RUN pCreateTaskFileRecord (opcJasperFile).
    OUTPUT STREAM sJasperJSON TO VALUE(opcJasperFile).
    PUT STREAM sJasperJSON UNFORMATTED
        "~{" SKIP
        FILL(" ",2)
        "~"" REPLACE(ipcSubjectName," ","_") "~": ~{" SKIP
        FILL(" ",4)
        "~"" REPLACE(ipcSubjectName," ","") "~": [" SKIP
        .


END PROCEDURE.
